module XParsec.FSharp.SemanticAnalysis.Tests.ResolvedTypeHeadStampTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution — the one resolve-once layer — resolves every written external
// type-annotation HEAD (opens-aware, at its syntactic arity) and stamps its
// `SymbolKey` in `Resolution.ResolvedTypeHead`, keyed by the `Type` node's
// `NodeKey` (`CstKeys.ofTypeHead`). `Translate.tryResolveExternalType` reads that
// stamp and fetches the shape through the key-addressed store face instead of
// re-resolving the spelling. These tests assert the stamp is present at
// representative type-annotation positions and that the reusable `CstWalk.iterType`
// recursion reaches nested generic-argument heads. A project-local / unknown head
// is left unstamped (Translate takes its local-registry / opaque paths).

/// A provider knowing a non-generic `Tests.Widget` and a generic `Tests.Box`1`,
/// both auto-opened via `AmbientOpenPrefixes` (as the real prelude opens the
/// package namespace). The store face resolves the SAME keys the resolver mints, so
/// the round-trip the stamp read relies on is exercised.
let private provider: IExternalSymbolProvider =
    let widget =
        ExternalTypeShape.Class(ExternalClassShape.basic (0, false, SymbolOrigin.Empty))

    let box =
        ExternalTypeShape.Class(ExternalClassShape.basic (1, false, SymbolOrigin.Empty))

    { new IExternalSymbolProvider

      interface IExternalSymbolResolver with
          member _.TryLookup _ = ValueNone

          member _.TryLookupType(n: string) =
              match n with
              | "Tests.Widget" -> ValueSome widget
              | "Tests.Box`1" -> ValueSome box
              | _ -> ValueNone

          member _.TryLookupUnionCase _ = ValueNone
          member _.AmbientOpenPrefixes = [ "Tests" ]
      interface IExternalSymbolStore with
          member _.TryLookupType(key: SymbolKey) =
              match SymbolKeyOps.qualifiedName key with
              | "Tests.Widget" -> ValueSome widget
              | "Tests.Box`1" -> ValueSome box
              | _ -> ValueNone

          member _.TryLookupMember(_, _) = ValueNone
          member _.TryLookupMembers(_, _) = [||]
          member _.TryLookupIndexSignature _ = []
          member _.TryLookupInlineBody _ = ValueNone
          member _.IntrinsicReverseCanon = Map.empty
          member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
    }

let private analyse (input: string) : PassContext * ImplementationFile<SyntaxToken> =
    let lexed, file = parseFile input
    let ctx = PassContext(provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    ctx, file

let private firstBinding (file: ImplementationFile<SyntaxToken>) : Binding<SyntaxToken> =
    CstWalk.implFileElems file
    |> Seq.pick (fun m ->
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) when bindings.Length > 0 ->
            Some bindings.[0]
        | _ -> None
    )

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

let private isHeadStamped (ctx: PassContext) (ty: Type<SyntaxToken>) : bool =
    match CstKeys.ofTypeHead ty with
    | ValueSome head -> ctx.Resolution.ResolvedTypeHead.ContainsKey head.Key
    | ValueNone -> false

[<Tests>]
let tests =
    testList
        "ResolvedTypeHeadStamp"
        [
            // A binding return-type annotation naming an external class: the head is
            // resolved once here and stamped, so `translateType` reads the store face.
            test "return-type annotation head is stamped" {
                let ctx, file = analyse "let f (x: Widget) : Widget = x"
                let t = returnTypeOf (firstBinding file)
                Expect.isTrue (isHeadStamped ctx t) "Widget return-type head stamped"
            }

            // A parameter annotation naming an external class is stamped too — pattern
            // annotations route through `stampPatCases`.
            test "parameter annotation head is stamped" {
                let ctx, file = analyse "let f (x: Widget) = x"

                let annot =
                    firstBinding file |> fun b -> b.argumentPats |> Seq.tryPick typedAnnotationOf

                match annot with
                | Some t -> Expect.isTrue (isHeadStamped ctx t) "Widget param head stamped"
                | None -> failtest "expected a typed parameter annotation"
            }

            // The reusable `CstWalk.iterType` recursion reaches a nested generic
            // ARGUMENT head: `Box<Widget>` stamps BOTH the `Box` head and the inner
            // `Widget` head in one walk.
            test "nested generic argument head is stamped (walker recursion)" {
                let ctx, file = analyse "let f (x: Widget) : Box<Widget> = box x"
                let boxTy = returnTypeOf (firstBinding file)
                Expect.isTrue (isHeadStamped ctx boxTy) "Box<Widget> head stamped"

                match boxTy with
                | Type.GenericType(typeArgs = args) when args.Length = 1 ->
                    match args.[0] with
                    | TypeArg.Type widgetTy ->
                        Expect.isTrue (isHeadStamped ctx widgetTy) "nested Widget arg head stamped"
                    | TypeArg.Measure _ -> failtest "expected a type argument"
                | other -> failtestf "expected Box<Widget> GenericType, got %A" other
            }

            // An unknown (not-provider-known) annotation head is NOT stamped — the
            // resolve-once layer records only heads it resolved; `translateType` then
            // takes its opaque / `TyVar` path (or the resolver fallback for a synthesized
            // node), never a stale stamp.
            test "unknown annotation head is not stamped" {
                let ctx, file = analyse "let f (x: Nope) = x"

                let annot =
                    firstBinding file |> fun b -> b.argumentPats |> Seq.tryPick typedAnnotationOf

                match annot with
                | Some t -> Expect.isFalse (isHeadStamped ctx t) "unknown head is not stamped"
                | None -> failtest "expected a typed parameter annotation"
            }
        ]
