module XParsec.FSharp.SemanticAnalysis.Tests.TypeRefVerdictTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Name resolution records a `TypeRefVerdict` for every written type reference, keyed by the
// `Type` node. These tests pin the verdict at representative annotation positions and, end
// to end, the round-trip of the recorded key through the store view during inference.

/// The store view serves the same table the resolver's scope reads.
let private provider: IExternalSymbolProvider =
    let widget =
        ExternalTypeShape.Class(ExternalClassShape.basic (TyparList.empty, ClassCommitment.Class, SymbolOrigin.Empty))

    let box =
        ExternalTypeShape.Class(
            ExternalClassShape.basic (TyparList.positional 1, ClassCommitment.Class, SymbolOrigin.Empty)
        )

    providerOfSurface (fun b ->
        PublishedSurfaceBuilder.addType b (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Widget" 0) widget
        PublishedSurfaceBuilder.addType b (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Box`1" 1) box
        b.ImplicitOpens <- [ SymbolKeyOps.assemblyAutoOpen "Tests" ]
    )

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

/// One error, citing the type: the annotation is the cause, so uses of the bound variable
/// must recover in silence rather than spray secondaries.
let private expectSoleUndefinedType (name: string) (input: string) =
    let ctx, file = analyse input
    Unification.run ctx file

    let errors =
        ctx.Diagnostics |> Diagnostic.errors |> Seq.map (fun d -> d.Kind) |> List.ofSeq

    Expect.equal errors [ Kind.UndefinedType name ] (sprintf "one diagnostic, naming the type, for: %s" input)

/// The `rhsType` of each constraint in `(x :?> 'T) when 'T: Widget = x`.
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

let private memberSigReturnType (ms: MemberSig<SyntaxToken>) : Type<SyntaxToken> =
    match ms with
    | MemberSig.MethodOrPropSig(sign = CurriedSig(returnType = ret))
    | MemberSig.PropSig(sign = CurriedSig(returnType = ret)) -> ret

/// The membersig return type of an SRTP trait call (`(^T: (static member M: unit -> Widget) x)`).
let private staticMemberInvocationReturnTypes (file: ImplementationFile<SyntaxToken>) : Type<SyntaxToken> list =
    match firstBindingExpr file with
    | Expr.StaticMemberInvocation(membersign = ms) -> [ memberSigReturnType ms ]
    | _ -> failtest "expected the binding body to parse as Expr.StaticMemberInvocation"

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

let private objectExprOf (file: ImplementationFile<SyntaxToken>) =
    match firstBindingExpr file with
    | Expr.Object(members = members; interfaceImpls = impls) -> members, impls
    | _ -> failtest "expected the binding body to parse as Expr.Object"

let private objectExprMemberReturnTypes (file: ImplementationFile<SyntaxToken>) : Type<SyntaxToken> list =
    objectExprOf file |> fst |> objectMemberReturnTypes

let private objectExprInterfaceReturnTypes (file: ImplementationFile<SyntaxToken>) : Type<SyntaxToken> list =
    let _, impls = objectExprOf file

    [
        for InterfaceImpl.InterfaceImpl(objectMembers = objMembers) in impls do
            match objMembers with
            | ValueSome ms -> yield! objectMemberReturnTypes ms
            | ValueNone -> ()
    ]

/// Pin how many nodes the locator found before asserting the verdict, so a locator that
/// drifted to zero nodes fails rather than passing vacuously.
let private assertTypeNamesStamped
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

// Each builder takes the name to write at the position under test, so the positive case
// (`Widget`, provider-known) and its control (`Nope`, unknown) differ only in that name.
let private staticOptSrc (typeName: string) =
    String.concat "\n" [ "let f (x: obj) : 'T ="; sprintf "    (x :?> 'T) when 'T: %s = x" typeName ]

let private staticMemberInvocationSrc (typeName: string) =
    sprintf "let inline f (x: ^T) = (^T: (static member Make: unit -> %s) x)" typeName

let private objExprSrc (typeName: string) =
    String.concat
        "\n"
        [
            "let o (x: Widget) ="
            "    { new Widget() with"
            sprintf "        member _.Get() : %s = x }" typeName
        ]

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
            test "return-type annotation type reference is external" {
                let ctx, file = analyse "let f (x: Widget) : Widget = x"
                let t = returnTypeOf (firstBinding file)
                Expect.isTrue (isExternalTypeRef ctx t) "Widget return-type type reference is external"
            }

            // A pattern annotation is a separate walk site from the return annotation above.
            test "parameter annotation type reference is external" {
                let ctx, file = analyse "let f (x: Widget) = x"

                let annot =
                    firstBinding file |> fun b -> b.argumentPats |> Seq.tryPick typedAnnotationOf

                match annot with
                | Some t -> Expect.isTrue (isExternalTypeRef ctx t) "Widget param type reference is external"
                | None -> failtest "expected a typed parameter annotation"
            }

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

            // Nothing is recorded for a name that did not resolve, and that ABSENCE is what
            // makes the name undefined — the read the diagnostic tests below rest on.
            test "unknown annotation type reference is not external" {
                let ctx, file = analyse "let f (x: Nope) = x"

                let annot =
                    firstBinding file |> fun b -> b.argumentPats |> Seq.tryPick typedAnnotationOf

                match annot with
                | Some t -> Expect.isFalse (isExternalTypeRef ctx t) "unknown type reference is not external"
                | None -> failtest "expected a typed parameter annotation"
            }

            // End to end: the minted key round-trips through the store view. Pinning the
            // resulting `TyClass` KEY is what excludes the opaque residue a missed stamp
            // would mint; a diagnostics-only assertion would not.
            test "external verdicts resolve through the store view during inference" {
                let ctx, file = analyse "let f (x: Widget) : Widget = x"
                Unification.run ctx file

                // pat `x` at offset 7: `let f (` is 7 chars.
                let patKey = NodeKey.ofSource 7 NodeKind.PatIdent

                // `SemType.TyClass`, not TestHelpers' string-keyed `TyClass` shim — the KEY
                // the resolver minted, not a name.
                let expected =
                    SemType.TyClass(SymbolKeyOps.qualifiedTypeKeyOf "Tests.Widget" 0, EqArray.empty)

                Expect.equal
                    (typeOf ctx patKey)
                    expected
                    "the annotation froze to the external class the provider serves — NOT the opaque \
                     residue a missed stamp would mint"

                Expect.isFalse
                    (ctx.Diagnostics |> Seq.exists Diagnostic.isError)
                    "Widget -> Widget round-trips with no error"
            }

            // Negative control for the test above: a name nothing resolves is diagnosed where
            // it is written, so an annotation that type-checks in silence really named a type.
            test "a bare name nothing resolves is not a type — it is diagnosed" {
                expectSoleUndefinedType "Gadget" "let f (x: Gadget) = x"
            }

            // The dots make no difference: the diagnostic is reported at the whole
            // `Foo.Bar.Baz`, by the name written.
            test "a dotted name nothing resolves is not a type — it is diagnosed" {
                expectSoleUndefinedType "Foo.Bar.Baz" "let f (x: Foo.Bar.Baz) = x"
            }

            // Type arguments do not change the verdict on the name they are applied to.
            // `Widget` is provider-known, so only `Gadget` is reported.
            test "an applied name nothing resolves is not a type — it is diagnosed" {
                expectSoleUndefinedType "Gadget" "let f (x: Gadget<Widget>) = x"
            }

            // The three positions below sit behind an EXPRESSION node rather than a binding
            // or pattern annotation, so only the expression-embedded type walk reaches them.
            test "static-optimization constraint rhs type type reference is external" {
                assertTypeNamesStamped true 1 staticOptRhsTypes (staticOptSrc "Widget")
            }

            test "unknown static-optimization constraint rhs type type reference is not external" {
                assertTypeNamesStamped false 1 staticOptRhsTypes (staticOptSrc "Nope")
            }

            test "SRTP trait-call membersig return type reference is external" {
                assertTypeNamesStamped true 1 staticMemberInvocationReturnTypes (staticMemberInvocationSrc "Widget")
            }

            test "unknown SRTP trait-call membersig return type reference is not external" {
                assertTypeNamesStamped false 1 staticMemberInvocationReturnTypes (staticMemberInvocationSrc "Nope")
            }

            test "object-expression member return-type type reference is external" {
                assertTypeNamesStamped true 1 objectExprMemberReturnTypes (objExprSrc "Widget")
            }

            test "unknown object-expression member return-type type reference is not external" {
                assertTypeNamesStamped false 1 objectExprMemberReturnTypes (objExprSrc "Nope")
            }

            // The same annotation in an `interface … with` block is a separate walk site.
            test "object-expression interface-impl member return-type type reference is external" {
                assertTypeNamesStamped true 1 objectExprInterfaceReturnTypes (objExprInterfaceSrc "Widget")
            }

            test "unknown object-expression interface-impl member return-type type reference is not external" {
                assertTypeNamesStamped false 1 objectExprInterfaceReturnTypes (objExprInterfaceSrc "Nope")
            }
        ]
