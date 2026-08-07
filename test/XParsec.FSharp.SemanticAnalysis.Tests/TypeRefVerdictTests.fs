module XParsec.FSharp.SemanticAnalysis.Tests.TypeRefVerdictTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution — the one resolve-once layer — resolves every written external
// TYPE REFERENCE (opens-aware, at its syntactic arity) and records its `SymbolKey`
// in `Resolution.TypeRefVerdicts`, keyed by the `Type` node's `NodeKey`
// (`CstKeys.ofTypeRef`). `Translate.tryResolveExternalTypeStamped` reads that
// verdict and fetches the shape through the key-addressed store view instead of
// re-resolving the spelling. These tests assert the external verdict is present
// at representative type-annotation positions, that the reusable
// `CstWalk.iterType` recursion reaches references nested in type arguments,
// and — end to end — that the recorded key round-trips through the store view
// during inference. A project-local / unknown reference records a non-external
// verdict (Translate takes its local-registry / opaque paths).

/// A provider knowing a non-generic `Tests.Widget` and a generic `Tests.Box`1`,
/// both auto-opened via `AmbientOpenPrefixes` (as the real prelude opens the
/// package namespace). `ofNamedLeaf` derives the store view from the same by-name
/// table, so it resolves the SAME keys the resolver mints by construction — the
/// round-trip the stamp read relies on.
let private provider: IExternalSymbolProvider =
    let widget =
        ExternalTypeShape.Class(ExternalClassShape.basic (0, false, SymbolOrigin.Empty))

    let box =
        ExternalTypeShape.Class(ExternalClassShape.basic (1, false, SymbolOrigin.Empty))

    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookupType =
                fun n ->
                    match n with
                    | "Tests.Widget" -> ValueSome widget
                    | "Tests.Box`1" -> ValueSome box
                    | _ -> ValueNone
            AmbientOpenPrefixes = [ "Tests" ]
        }

let private analyse (input: string) = analyseNameRes provider input

let private returnTypeOf (b: Binding<SyntaxToken>) : Type<SyntaxToken> =
    match b.returnType with
    | ValueSome(ReturnType(typ = t)) -> t
    | ValueNone -> failwith "expected a return-type annotation"

/// The first `Pat.Typed` annotation type anywhere in a pattern.
let rec private typedAnnotationOf (p: Pat<SyntaxToken>) : Type<SyntaxToken> option =
    match p with
    | Pat.Typed(typ = t) -> Some t
    | Pat.EnclosedBlock(pat = inner)
    | Pat.Attributed(pat = inner)
    | Pat.As(pat = inner)
    | Pat.Optional(pat = inner) -> typedAnnotationOf inner
    | Pat.Tuple(patterns = ps) -> ps |> Seq.tryPick typedAnnotationOf
    | _ -> None

let private isExternalTypeRef (ctx: PassContext) (ty: Type<SyntaxToken>) : bool =
    match CstKeys.ofTypeRef ty with
    | ValueSome typeRef ->
        match ctx.Resolution.TypeRefVerdicts.TryGetValue typeRef.Site.Key with
        | ValueSome(TypeRefVerdict.ExternalType _) -> true
        | _ -> false
    | ValueNone -> false

/// A name nothing resolves is blamed by NAME, exactly ONCE: the annotation is the cause,
/// and everything downstream of it must recover in silence rather than spray secondary
/// errors through every expression that touched the binder.
///
/// Asserted on the VERDICT rather than on a rendered sentence: the diagnostic carries WHICH
/// type went undefined as data, so this pins the classification and the name it names, and
/// a reworded message cannot break it — nor can a differently-worded diagnostic sneak past
/// it by happening to contain the same substring. The severity it filters on is read off
/// the same kind, so both halves of the assertion are one fact.
let private expectSoleUndefinedType (name: string) (input: string) =
    let ctx, file = analyse input
    Unification.run ctx file

    let errors =
        ctx.Diagnostics |> Diagnostic.errors |> Seq.map (fun d -> d.Kind) |> List.ofSeq

    Expect.equal errors [ Kind.UndefinedType name ] (sprintf "one diagnostic, naming the type, for: %s" input)

// ---------------------------------------------------------------------------
// Locators for the expression-embedded type positions `CstWalk.iterExprEmbeddedTypes`
// enumerates. Each returns the `Type` nodes at ONE syntactic position, so a test can
// assert the found COUNT before asserting the stamp — a locator that silently found
// nothing (a parser shape drift) then fails instead of passing vacuously.
// ---------------------------------------------------------------------------

/// The `rhsType` of every `when ^T : Tycon` static-optimization constraint on the
/// first binding's body (`(e :?> 'T) when 'T: Widget = alt`).
let private staticOptRhsTypes (file: ImplementationFile<SyntaxToken>) : Type<SyntaxToken> list =
    match firstBindingExpr file with
    | Expr.LibraryOnlyStaticOptimization(clauses = clauses) ->
        [
            for clause in clauses do
                for c in clause.Constraints do
                    match c with
                    | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(rhsType = t) -> yield t
                    | StaticOptimizationConstraint.WhenTyparIsStruct _ -> ()
        ]
    | _ -> failtest "expected the binding body to parse as Expr.LibraryOnlyStaticOptimization"

/// A member signature's return type — the type an SRTP trait call names.
let private memberSigReturnType (ms: MemberSig<SyntaxToken>) : Type<SyntaxToken> =
    match ms with
    | MemberSig.MethodOrPropSig(sign = CurriedSig(returnType = ret))
    | MemberSig.PropSig(sign = CurriedSig(returnType = ret)) -> ret

/// The membersig return type of an SRTP trait call (`(^T: (static member M: unit -> Widget) x)`).
let private staticMemberInvocationReturnTypes (file: ImplementationFile<SyntaxToken>) : Type<SyntaxToken> list =
    match firstBindingExpr file with
    | Expr.StaticMemberInvocation(membersign = ms) -> [ memberSigReturnType ms ]
    | _ -> failtest "expected the binding body to parse as Expr.StaticMemberInvocation"

/// Every method/property RETURN-type annotation in an object-expression member block.
let private objectMemberReturnTypes (ObjectMembers(memberDefns = defns)) : Type<SyntaxToken> list =
    [
        for d in defns do
            match d with
            | MemberDefn.Member(defn = MethodOrPropDefn.Method(defn = b))
            | MemberDefn.Member(defn = MethodOrPropDefn.Property(defn = b)) ->
                match b.returnType with
                | ValueSome(ReturnType(typ = t)) -> yield t
                | ValueNone -> ()
            | _ -> ()
    ]

/// The object expression that is the first binding's body.
let private objectExprOf (file: ImplementationFile<SyntaxToken>) =
    match firstBindingExpr file with
    | Expr.Object(members = members; interfaceImpls = impls) -> members, impls
    | _ -> failtest "expected the binding body to parse as Expr.Object"

/// Member return annotations in the object expression's own `with` block.
let private objectExprMemberReturnTypes (file: ImplementationFile<SyntaxToken>) : Type<SyntaxToken> list =
    objectExprOf file |> fst |> objectMemberReturnTypes

/// Member return annotations inside the object expression's `interface … with` blocks.
let private objectExprInterfaceReturnTypes (file: ImplementationFile<SyntaxToken>) : Type<SyntaxToken> list =
    let _, impls = objectExprOf file

    [
        for InterfaceImpl.InterfaceImpl(objectMembers = objMembers) in impls do
            match objMembers with
            | ValueSome ms -> yield! objectMemberReturnTypes ms
            | ValueNone -> ()
    ]

/// The house discipline for a stamp position: locate the `Type` nodes at ONE
/// syntactic position, pin how many were found (so a locator that drifted to zero
/// nodes fails rather than passing vacuously), then assert each carries — or, for a
/// negative control, does NOT carry — an `ExternalType` verdict.
let private assertHeadsStamped
    (expected: bool)
    (count: int)
    (locate: ImplementationFile<SyntaxToken> -> Type<SyntaxToken> list)
    (input: string)
    =
    let ctx, file = analyse input
    let tys = locate file
    Expect.equal tys.Length count (sprintf "type-node count at the position under test in: %s" input)

    for t in tys do
        Expect.equal
            (isExternalTypeRef ctx t)
            expected
            (sprintf "expected stamped=%b at the position under test in: %s" expected input)

// Each source builder takes the type name to write at the position under test, so a
// positive case (`Widget`, provider-known) and its negative control (`Nope`,
// unknown) differ ONLY in that name — nothing else about the shape can drift
// between the two.

/// A static-optimization clause whose tycon-equality constraint names `typeName`.
let private staticOptSrc (typeName: string) =
    String.concat "\n" [ "let f (x: obj) : 'T ="; sprintf "    (x :?> 'T) when 'T: %s = x" typeName ]

/// An SRTP trait call whose member signature returns `typeName`.
let private staticMemberInvocationSrc (typeName: string) =
    sprintf "let inline f (x: ^T) = (^T: (static member Make: unit -> %s) x)" typeName

/// An object expression whose own `with`-block member returns `typeName`.
let private objExprSrc (typeName: string) =
    String.concat
        "\n"
        [
            "let o (x: Widget) ="
            "    { new Widget() with"
            sprintf "        member _.Get() : %s = x }" typeName
        ]

/// An object expression whose `interface … with` block's member returns `typeName`.
let private objExprInterfaceSrc (typeName: string) =
    String.concat
        "\n"
        [
            "let o (x: Widget) ="
            "    { new Widget() with"
            "        member _.Get() = x"
            "      interface Box<Widget> with"
            sprintf "        member _.Get2() : %s = x }" typeName
        ]

[<Tests>]
let tests =
    testList
        "TypeRefVerdict"
        [
            // A binding return-type annotation naming an external class: the name is
            // resolved once here, so translation reads the store view by key.
            test "return-type annotation type reference is external" {
                let ctx, file = analyse "let f (x: Widget) : Widget = x"
                let t = returnTypeOf (firstBinding file)
                Expect.isTrue (isExternalTypeRef ctx t) "Widget return-type type reference is external"
            }

            // A parameter annotation naming an external class is stamped too — pattern
            // annotations route through `stampPatCases`.
            test "parameter annotation type reference is external" {
                let ctx, file = analyse "let f (x: Widget) = x"

                let annot =
                    firstBinding file |> fun b -> b.argumentPats |> Seq.tryPick typedAnnotationOf

                match annot with
                | Some t -> Expect.isTrue (isExternalTypeRef ctx t) "Widget param type reference is external"
                | None -> failtest "expected a typed parameter annotation"
            }

            // The reusable `CstWalk.iterType` recursion reaches a nested generic
            // ARGUMENT position: `Box<Widget>` records BOTH the `Box` verdict and the inner
            // `Widget` one in a single walk.
            test "nested generic argument type reference is external (walker recursion)" {
                let ctx, file = analyse "let f (x: Widget) : Box<Widget> = box x"
                let boxTy = returnTypeOf (firstBinding file)
                Expect.isTrue (isExternalTypeRef ctx boxTy) "Box<Widget> type reference is external"

                match boxTy with
                | Type.GenericType(typeArgs = args) when args.Length = 1 ->
                    match args.[0] with
                    | TypeArg.Type widgetTy ->
                        Expect.isTrue (isExternalTypeRef ctx widgetTy) "nested Widget arg type reference is external"
                    | TypeArg.Measure _ -> failtest "expected a type argument"
                | other -> failtestf "expected Box<Widget> GenericType, got %A" other
            }

            // An unknown (not-provider-known) annotation records no EXTERNAL verdict — the
            // resolve-once layer records only what it resolved, never a stale key. The
            // ABSENCE of a stamp is what tells `translateType` the name is undefined, so this
            // is the read the undefined-type diagnostic below rests on.
            test "unknown annotation type reference is not external" {
                let ctx, file = analyse "let f (x: Nope) = x"

                let annot =
                    firstBinding file |> fun b -> b.argumentPats |> Seq.tryPick typedAnnotationOf

                match annot with
                | Some t -> Expect.isFalse (isExternalTypeRef ctx t) "unknown type reference is not external"
                | None -> failtest "expected a typed parameter annotation"
            }

            // End-to-end: the stamped key round-trips through the store view during
            // inference. This must assert the annotation's resulting IDENTITY, not merely
            // that it type-checks free of diagnostics: `Widget` is a name the provider
            // serves, so it is the ROUND-TRIP — stamp minted, store view served — that the
            // identity witnesses, and a diagnostics-only assertion would witness only that
            // the name was not diagnosed as undefined. Pinning `TyClass(externalTypeKey …)`
            // is what excludes it.
            test "external verdicts resolve through the store view during inference" {
                let ctx, file = analyse "let f (x: Widget) : Widget = x"
                Unification.run ctx file

                // pat `x` at offset 7: `let f (` is 7 chars.
                let patKey = NodeKey.ofSource 7 NodeKind.PatIdent

                // `SemType.TyClass`, not TestHelpers' string-keyed `TyClass` shim: the
                // whole point is to pin the KEY the resolver minted, not a name.
                let expected =
                    SemType.TyClass(SymbolKeyOps.externalTypeKeyOf SymbolOrigin.Empty "Tests.Widget" 0, EqArray.empty)

                Expect.equal
                    (typeOf ctx patKey)
                    expected
                    "the annotation froze to the external class the provider serves — NOT the opaque \
                     residue a missed stamp would mint"

                Expect.isFalse
                    (ctx.Diagnostics |> Seq.exists Diagnostic.isError)
                    "Widget -> Widget round-trips with no error"
            }

            // The negative control for the assertion above, and the rule itself: a name
            // NOTHING resolves — no scope of this file, no shape the provider serves — is not
            // a type, whatever it is spelled like. It is diagnosed where it is written, so
            // the positive test above is not vacuous: an annotation that type-checks in
            // silence is one the target really could name.
            test "a bare name nothing resolves is not a type — it is diagnosed" {
                expectSoleUndefinedType "Gadget" "let f (x: Gadget) = x"
            }

            // The DOTTED spelling is the one a free TyVar left decorative: a free variable
            // unifies with anything, so `Foo.Bar.Baz` typed as readily as `System.IO.TextWriter`
            // and the annotation asserted nothing about the value it named.
            test "a dotted name nothing resolves is not a type — it is diagnosed" {
                expectSoleUndefinedType "Foo.Bar.Baz" "let f (x: Foo.Bar.Baz) = x"
            }

            // An APPLIED name is resolved no differently: undefined is undefined whether or not
            // type arguments follow it (which it has no parameters to take). The argument names
            // a type this provider DOES serve, so the applied name alone is blamed — the args
            // resolve exactly as they would under a defined one.
            test "an applied name nothing resolves is not a type — it is diagnosed" {
                expectSoleUndefinedType "Gadget" "let f (x: Gadget<Widget>) = x"
            }

            // The three expression-embedded positions below sit BEHIND an expression
            // node, not behind a binding/pattern annotation, so they are reached only
            // by `CstWalk.iterExprEmbeddedTypes`. Each recorded no verdict before
            // that walk enumerated it — and a missing verdict is invisible on the read
            // side (`Translate.tryResolveExternalTypeStamped` has no by-name fallback),
            // so nothing else in the suite would catch a regression here.

            // A static-optimization clause's tycon-equality constraint names a type on
            // its RHS (`when ^T : System.DateTime`): `Expr.LibraryOnlyStaticOptimization`'s
            // `WhenTyparTyconEqualsTycon.rhsType`.
            test "static-optimization constraint rhs type type reference is external" {
                assertHeadsStamped true 1 staticOptRhsTypes (staticOptSrc "Widget")
            }

            test "unknown static-optimization constraint rhs type type reference is not external" {
                assertHeadsStamped false 1 staticOptRhsTypes (staticOptSrc "Nope")
            }

            // An SRTP trait call's member signature (`(^T: (static member Make: unit ->
            // Widget) x)`): `Expr.StaticMemberInvocation`'s membersig, walked through
            // `iterTypeMemberSig`.
            test "SRTP trait-call membersig return type reference is external" {
                assertHeadsStamped true 1 staticMemberInvocationReturnTypes (staticMemberInvocationSrc "Widget")
            }

            test "unknown SRTP trait-call membersig return type reference is not external" {
                assertHeadsStamped false 1 staticMemberInvocationReturnTypes (staticMemberInvocationSrc "Nope")
            }

            // An object-expression member's RETURN-type annotation, in the expression's
            // own `with` block. The base-call type was always stamped; the member's
            // signature types were not, until `memberDefnSigs`.
            test "object-expression member return-type type reference is external" {
                assertHeadsStamped true 1 objectExprMemberReturnTypes (objExprSrc "Widget")
            }

            test "unknown object-expression member return-type type reference is not external" {
                assertHeadsStamped false 1 objectExprMemberReturnTypes (objExprSrc "Nope")
            }

            // The same annotation inside an `interface … with` block of the object
            // expression — a SEPARATE `memberDefnSigs` call site in the walk, so it needs
            // its own coverage.
            test "object-expression interface-impl member return-type type reference is external" {
                assertHeadsStamped true 1 objectExprInterfaceReturnTypes (objExprInterfaceSrc "Widget")
            }

            test "unknown object-expression interface-impl member return-type type reference is not external" {
                assertHeadsStamped false 1 objectExprInterfaceReturnTypes (objExprInterfaceSrc "Nope")
            }
        ]
