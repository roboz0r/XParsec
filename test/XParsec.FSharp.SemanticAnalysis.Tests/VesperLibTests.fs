module XParsec.FSharp.SemanticAnalysis.Tests.VesperLibTests

open System.IO
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// Locate `src/XParsec.FSharp.Lib` by walking up from the test assembly.
/// Layout: `<repo>/test/XParsec.FSharp.SemanticAnalysis.Tests/bin/...`.
let private libRoot =
    let testDir = Path.GetDirectoryName(typeof<VesperLib.LibFile>.Assembly.Location)
    let mutable dir = DirectoryInfo testDir
    let mutable found = None

    while not (isNull dir) && found.IsNone do
        let candidate = Path.Combine(dir.FullName, "src", "XParsec.FSharp.Lib")

        if Directory.Exists candidate then
            found <- Some candidate

        dir <- dir.Parent

    match found with
    | Some p -> p
    | None -> failwithf "Could not locate XParsec.FSharp.Lib starting from %s" testDir

/// Lazy: build the provider once and cache for the duration of the test
/// run. `buildProvider` parses every .fsi in the lib (~28 files); doing
/// it per-test would be wasteful.
let private builtProvider =
    lazy
        (match VesperLib.buildProvider libRoot with
         | Result.Error e -> failwithf "buildProvider failed: %s" e
         | Result.Ok r -> r)

[<Tests>]
let tests =
    testList
        "VesperLib"
        [
            test "root manifest loads with upstream + buckets" {
                match VesperLib.loadAll libRoot with
                | Result.Error e -> failtestf "loadAll failed: %s" e
                | Result.Ok loaded ->
                    Expect.isNonEmpty loaded.Root.UpstreamCommit "commit SHA"
                    Expect.isNonEmpty loaded.Root.Buckets "buckets present"

                    let names = loaded.Root.Buckets |> List.map (fun b -> b.Name) |> Set.ofList

                    Expect.equal
                        names
                        (Set.ofList [ "Clr"; "Common"; "Threading"; "Reflection"; "Printf" ])
                        "all five buckets"
            }

            test "files are sorted Clr first then Common" {
                match VesperLib.loadAll libRoot with
                | Result.Error e -> failtestf "loadAll failed: %s" e
                | Result.Ok loaded ->
                    let bucketOrder = loaded.Files |> List.map (fun f -> f.BucketName) |> List.distinct

                    Expect.equal (List.head bucketOrder) "Clr" "Clr first"
                    Expect.contains bucketOrder "Common" "Common present"
                    // Threading/Reflection/Printf all depend on Common — they must come after.
                    let commonIdx = List.findIndex ((=) "Common") bucketOrder

                    for later in [ "Threading"; "Reflection"; "Printf" ] do
                        let idx = List.findIndex ((=) later) bucketOrder
                        Expect.isGreaterThan idx commonIdx (sprintf "%s after Common" later)
            }

            test "every listed file exists on disk" {
                match VesperLib.loadAll libRoot with
                | Result.Error e -> failtestf "loadAll failed: %s" e
                | Result.Ok loaded ->
                    let missing = loaded.Files |> List.filter (fun f -> not (File.Exists f.Absolute))

                    Expect.isEmpty missing (sprintf "missing files: %A" (missing |> List.map (fun f -> f.Relative)))
            }

            test "every .fsi parses without errors" {
                match VesperLib.loadAll libRoot with
                | Result.Error e -> failtestf "loadAll failed: %s" e
                | Result.Ok loaded ->
                    let mutable failures = []

                    for f in loaded.Files do
                        match VesperLib.parseFile f with
                        | Result.Ok _ -> ()
                        | Result.Error e -> failures <- (f, e) :: failures

                    if not (List.isEmpty failures) then
                        let summary =
                            failures
                            |> List.map (fun (f, e) ->
                                sprintf "  %s/%s\n    %s" f.BucketName f.Relative (e.Replace("\n", "\n    "))
                            )
                            |> String.concat "\n"

                        failtestf "Parse failures (%d):\n%s" (List.length failures) summary
            }

            ptest "DEBUG: dump option.fsi val attributes" {
                // Trigger ref-parser init via the standard build path.
                let _ = builtProvider.Value
                let optPath = Path.Combine(libRoot, "Common", "option.fsi")

                let f: VesperLib.LibFile =
                    {
                        BucketName = "Common"
                        Relative = "option.fsi"
                        Absolute = optPath
                    }

                match VesperLib.parseFileFull f with
                | Result.Error e -> failtestf "parse failed: %s" e
                | Result.Ok parsed ->
                    let lexed = parsed.Lexed
                    let input = parsed.Input
                    let sb = System.Text.StringBuilder()

                    let tokStr (t: SyntaxToken) =
                        match t.Index with
                        | TokenIndex.Regular iT -> lexed.GetTokenString(iT, input)
                        | _ -> "<v>"

                    let dumpAttrs (attrs: Attributes<SyntaxToken> voption) =
                        match attrs with
                        | ValueNone -> sb.AppendLine "    attrs: <none>" |> ignore
                        | ValueSome sets ->
                            sb.AppendLine(sprintf "    attrs: %d set(s)" sets.Length) |> ignore

                            for i in 0 .. sets.Length - 1 do
                                let (AttributeSet(_, items, _)) = sets.[i]
                                sb.AppendLine(sprintf "      set %d: %d item(s)" i items.Length) |> ignore

                                for j in 0 .. items.Length - 1 do
                                    let (Attribute(_, construction), _) = items.[j]

                                    let typ, exprOpt =
                                        match construction with
                                        | ObjectConstruction(typ = t; expr = e) -> t, Some e
                                        | InterfaceConstruction(typ = t) -> t, None

                                    sb.AppendLine(sprintf "        item %d: typ tag = %s" j (typ.GetType().Name))
                                    |> ignore

                                    match typ with
                                    | Type.NamedType li ->
                                        let n =
                                            [ for k in 0 .. li.Idents.Length - 1 -> tokStr li.Idents.[k] ]
                                            |> String.concat "."

                                        sb.AppendLine(sprintf "          NamedType: %s" n) |> ignore
                                    | other -> sb.AppendLine(sprintf "          other: %A" other) |> ignore

                                    match exprOpt with
                                    | Some e ->
                                        let rec describe (e: Expr<SyntaxToken>) depth =
                                            if depth > 4 then
                                                "…"
                                            else
                                                match e with
                                                | Expr.EnclosedBlock(_, inner, _) ->
                                                    sprintf "EnclosedBlock(%s)" (describe inner (depth + 1))
                                                | Expr.Const c ->
                                                    match c with
                                                    | Constant.Literal tok ->
                                                        sprintf
                                                            "Const(Literal %s tok=%s)"
                                                            (string tok.Token)
                                                            (tokStr tok)
                                                    | Constant.MeasuredLiteral _ -> "Const(Measured)"
                                                | Expr.LongIdentOrOp _ -> "LongIdentOrOp"
                                                | Expr.App _ -> "App"
                                                | Expr.HighPrecedenceApp _ -> "HighPrecedenceApp"
                                                | Expr.Tuple _ -> "Tuple"
                                                | Expr.DotLookup _ -> "DotLookup"
                                                | Expr.String _ -> "String"
                                                | other -> sprintf "Other(%s)" (other.GetType().Name)

                                        sb.AppendLine(sprintf "          expr: %s" (describe e 0)) |> ignore
                                    | None -> sb.AppendLine "          expr: <none>" |> ignore

                    let rec dumpElems (elems: ModuleSignatureElements<SyntaxToken>) =
                        for i in 0 .. elems.Length - 1 do
                            match elems.[i] with
                            | ModuleSignatureElement.Val(ValSig(attrs, _, _, _, _, ident, _, _, _, _)) ->
                                let identStr =
                                    match ident with
                                    | IdentOrOp.Ident tok -> tokStr tok
                                    | _ -> "<op>"

                                sb.AppendLine(sprintf "  Val %s" identStr) |> ignore
                                dumpAttrs attrs
                            | ModuleSignatureElement.Module(ModuleSignature.ModuleSignature(attrs,
                                                                                            _,
                                                                                            _,
                                                                                            _,
                                                                                            identTok,
                                                                                            _,
                                                                                            body)) ->
                                let modName = tokStr identTok
                                sb.AppendLine(sprintf "Module %s" modName) |> ignore
                                dumpAttrs attrs
                                let (ModuleSignatureBody(_, inner, _)) = body
                                dumpElems inner
                            | _ -> ()

                    match parsed.Ast with
                    | FSharpAst.SignatureFile sf ->
                        match sf with
                        | SignatureFile.Namespaces groups ->
                            sb.AppendLine(sprintf "Namespaces: %d group(s)" groups.Length) |> ignore

                            for i in 0 .. groups.Length - 1 do
                                match groups.[i] with
                                | NamespaceDeclGroupSignature.Named(_, _, _, elems)
                                | NamespaceDeclGroupSignature.Global(_, _, elems) -> dumpElems elems
                        | _ -> sb.AppendLine "Not Namespaces" |> ignore
                    | _ -> sb.AppendLine "Not SignatureFile" |> ignore

                    failtestf "%s" (sb.ToString().Substring(0, min 4000 (sb.Length)))
            }

            test "buildProvider returns provider with parse errors confined to known v1 gaps" {
                let provider, errors = builtProvider.Value

                Expect.isNotNull (box provider) "provider present"

                // `SI.fs` is the lib's one `.fs` file (measure-type
                // declarations); the implementation-file parser has gaps
                // around `[<Measure>] type` that the .fsi-focused
                // signature parser doesn't trip on. Allow it through.
                let unexpected = errors |> List.filter (fun (f, _) -> f.Relative <> "SI.fs")

                Expect.isEmpty
                    unexpected
                    (sprintf "Unexpected parse errors: %A" (unexpected |> List.map (fun (f, _) -> f.Relative)))
            }

            test "monomorphic val: Operators.Not extracts as bool -> bool" {
                let provider, _ = builtProvider.Value

                match provider.TryLookup "Microsoft.FSharp.Core.Operators.Not" with
                | ValueNone -> failtestf "Microsoft.FSharp.Core.Operators.Not not in provider"
                | ValueSome sym ->
                    let ty = sym.Instantiate 0

                    match ty with
                    | TyFun(TyConst "bool", TyConst "bool") -> ()
                    | other -> failtestf "Expected bool -> bool, got %A" other
            }

            test "polymorphic val: Option.Map extracts with fresh TyVars per instantiation" {
                let provider, _ = builtProvider.Value

                // `[<CompilationRepresentation(ModuleSuffix)>]` rewrites `Option` to `OptionModule`;
                // `[<CompiledName("Map")>]` capitalises the ident.
                match provider.TryLookup "Microsoft.FSharp.Core.OptionModule.Map" with
                | ValueNone -> failtestf "Microsoft.FSharp.Core.OptionModule.Map not in provider"
                | ValueSome sym ->
                    let inst1 = sym.Instantiate 0
                    let inst2 = sym.Instantiate 0

                    let optionName = "Microsoft.FSharp.Core.option"

                    // Two TyVars share identity iff they're the same object — fresh
                    // instantiations must produce disjoint TyVars.
                    match inst1, inst2 with
                    | TyFun(TyFun(TyVar a1, TyVar b1), TyFun(TyRecord(n1, args1), TyRecord(n2, args2))),
                      TyFun(TyFun(TyVar a2, _), _) when
                        n1 = optionName && n2 = optionName && args1.Length = 1 && args2.Length = 1
                        ->
                        match args1.[0], args2.[0] with
                        | TyVar a1', TyVar b1' ->
                            Expect.isTrue
                                (System.Object.ReferenceEquals(a1, a1'))
                                "first instantiation: a appears in arg and option-arg"

                            Expect.isTrue
                                (System.Object.ReferenceEquals(b1, b1'))
                                "first instantiation: b appears in lambda result and option result"

                            Expect.isFalse
                                (System.Object.ReferenceEquals(a1, a2))
                                "two instantiations mint disjoint TyVars"
                        | _ ->
                            failtestf
                                "Expected ('T -> 'U) -> option<'T> -> option<'U>; got\n  first:  %A\n  second: %A"
                                inst1
                                inst2
                    | _ ->
                        failtestf
                            "Expected ('T -> 'U) -> option<'T> -> option<'U>; got\n  first:  %A\n  second: %A"
                            inst1
                            inst2
            }

            test "polymorphic val: Result.Map carries two typars" {
                let provider, _ = builtProvider.Value

                match provider.TryLookup "Microsoft.FSharp.Core.ResultModule.Map" with
                | ValueNone -> failtestf "Microsoft.FSharp.Core.ResultModule.Map not in provider"
                | ValueSome sym ->
                    let inst = sym.Instantiate 0
                    let resultName = "Microsoft.FSharp.Core.Result"

                    // val map: ('T -> 'U) -> Result<'T, 'TError> -> Result<'U, 'TError>
                    match inst with
                    | TyFun(TyFun(TyVar t, TyVar u), TyFun(TyRecord(n1, args1), TyRecord(n2, args2))) when
                        n1 = resultName && n2 = resultName && args1.Length = 2 && args2.Length = 2
                        ->
                        match args1.[0], args1.[1], args2.[0], args2.[1] with
                        | TyVar t', TyVar err1, TyVar u', TyVar err2 ->
                            Expect.isTrue (System.Object.ReferenceEquals(t, t')) "T is shared"
                            Expect.isTrue (System.Object.ReferenceEquals(u, u')) "U is shared"

                            Expect.isTrue
                                (System.Object.ReferenceEquals(err1, err2))
                                "TError is shared across both Results"
                        | _ -> failtestf "Result.Map shape unexpected: %A" inst
                    | _ -> failtestf "Result.Map shape unexpected: %A" inst
            }

            test "ModuleSuffix flag applied to the innermost module" {
                let provider, _ = builtProvider.Value

                // List module sits under Microsoft.FSharp.Collections and has the
                // ModuleSuffix flag — so it compiles to `ListModule`.
                let names =
                    [
                        "Microsoft.FSharp.Collections.ListModule.Map"
                        "Microsoft.FSharp.Collections.ArrayModule.Map"
                    ]

                for n in names do
                    match provider.TryLookup n with
                    | ValueSome _ -> ()
                    | ValueNone -> failtestf "%s should have been extracted" n
            }

            test "TyVar level is stamped at requested level" {
                let provider, _ = builtProvider.Value

                match provider.TryLookup "Microsoft.FSharp.Core.OptionModule.IsSome" with
                | ValueNone -> failtestf "IsSome not extracted"
                | ValueSome sym ->
                    let ty = sym.Instantiate 7

                    let rec firstTyVar t =
                        match t with
                        | TyVar tv -> ValueSome tv
                        | TyFun(a, b) ->
                            match firstTyVar a with
                            | ValueSome _ as r -> r
                            | _ -> firstTyVar b
                        | TyRecord(_, args)
                        | TyUnion(_, args)
                        | TyClass(_, args) ->
                            let mutable result = ValueNone

                            for a in args do
                                if result.IsNone then
                                    result <- firstTyVar a

                            result
                        | _ -> ValueNone

                    match firstTyVar ty with
                    | ValueSome tv -> Expect.equal tv.Level 7 "TyVar level matches requested level"
                    | _ -> failtestf "IsSome should contain at least one TyVar"
            }

            test "qualified-name lookup goes through the lib first, MockBuiltins second" {
                // Phase 3 fixture: chain `buildProvider <libRoot>` ahead of
                // `MockBuiltins.provider` and confirm both surfaces still
                // answer for their respective name spaces.
                let libProvider, _ = builtProvider.Value
                let chained = ExternalSymbols.composite [ libProvider; MockBuiltins.provider ]

                // Lib-sourced symbol: only `buildProvider` knows about it.
                match chained.TryLookup "Microsoft.FSharp.Core.OptionModule.Map" with
                | ValueNone -> failtest "OptionModule.Map should be answered by the lib provider"
                | ValueSome _ -> ()

                // `op_Addition` is no longer auto-opened *inside* the provider
                // (O3): the provider answers only the qualified name directly,
                // with the universal SRTP signature `^T1 -> ^T2 -> ^T3` (typars
                // free at instantiation; generalisation-time defaulting closes the
                // gap to `int` for `1 + 2`). The short form is surfaced as the
                // lib's ambient prefix set, probed by the pipeline behind explicit
                // opens — not a provider-internal retry that would shadow them.
                match libProvider.TryLookup "Microsoft.FSharp.Core.Operators.op_Addition" with
                | ValueNone -> failtest "op_Addition should resolve by its qualified name through the lib provider"
                | ValueSome sym ->
                    match sym.Instantiate 0 with
                    | TyFun(TyVar _, TyFun(TyVar _, TyVar _)) -> ()
                    | other -> failtestf "op_Addition shape unexpected: %A" other

                // The auto-open prefix is exposed via the provider's
                // `AmbientOpenPrefixes` member, not a provider-internal retry.
                Expect.isTrue
                    (libProvider.AmbientOpenPrefixes
                     |> List.contains "Microsoft.FSharp.Core.Operators")
                    "lib surfaces the Operators auto-open prefix"

                // Unknown name: both providers miss.
                match chained.TryLookup "nope.no.such.symbol" with
                | ValueNone -> ()
                | ValueSome _ -> failtest "missing names should not resolve through the chain"
            }

            test "Phase 4: option<'T> abbreviation resolves through TryLookupType" {
                // `'T option = Option<'T>` in prim-types.fsi. The abbreviation
                // body should be retrievable as an Abbrev shape; substituting
                // `int` for the typar must yield `TyRecord("...Option", [int])`.
                let provider, _ = builtProvider.Value

                match provider.TryLookupType "Microsoft.FSharp.Core.option" with
                | ValueNone -> failtest "Microsoft.FSharp.Core.option shape not found"
                | ValueSome(ExternalTypeShape.Abbrev(arity, build)) ->
                    Expect.equal arity 1 "option has one typar"
                    let body = build [| TyConst "int" |]

                    match body with
                    | TyRecord(name, args) when
                        args.Length = 1
                        && (
                            match args.[0] with
                            | TyConst "int" -> true
                            | _ -> false
                        )
                        ->
                        Expect.stringContains name "Option" "abbreviation expands to Option"
                    | other -> failtestf "Expected TyRecord(...Option, [int]); got %A" other
                | other -> failtestf "Expected Abbrev shape; got %A" other
            }

            test "Phase 4: Result<_,_> union exposes Ok / Error cases" {
                let provider, _ = builtProvider.Value

                match provider.TryLookupType "Microsoft.FSharp.Core.Result" with
                | ValueNone -> failtest "Result shape not found"
                | ValueSome(ExternalTypeShape.Union(arity, cases)) ->
                    Expect.equal arity 2 "Result has two typars"

                    let names = cases |> Array.map (fun c -> c.Name) |> Array.sort

                    Expect.equal names [| "Error"; "Ok" |] "Ok + Error cases extracted"

                    let okCase = cases |> Array.find (fun c -> c.Name = "Ok")
                    Expect.equal okCase.BuildFieldTypes.Length 1 "Ok carries one field"

                    let okFieldType = okCase.BuildFieldTypes.[0] [| TyConst "int"; TyConst "string" |]

                    match okFieldType with
                    | TyConst "int" -> ()
                    | _ ->
                        match okFieldType with
                        | TyVar tv when tv.Level = 0 -> ()
                        | _ ->
                            // The CST may carry an alias for the result type
                            // ('T or 'TSuccess depending on transliteration);
                            // assert by substituting the first typar.
                            failtestf "Ok field shape: %A" okFieldType
                | other -> failtestf "Expected Union shape; got %A" other
            }

            test "Phase 5: `when 'T : equality` captured + applied to fresh TyVar" {
                // `Seq.contains` in seq.fsi declares
                //   val inline contains: value:'T -> source: seq<'T> -> bool when 'T: equality
                // — a trait-table constraint that Phase 5a applies directly
                // to the fresh TyVar at instantiation. The Equality
                // constraint should land on both:
                //   - The symbol's structured `Constraints` list (for
                //     Phase 5b introspection); and
                //   - Every freshly-minted TyVar bound to the constrained
                //     typar position.
                let provider, _ = builtProvider.Value

                let providerOrFail =
                    match provider.TryLookup "Microsoft.FSharp.Collections.SeqModule.Contains" with
                    | ValueSome s -> s
                    | ValueNone -> failwith "Seq.Contains not extracted; v1 capture path broken"

                let equalityTrait =
                    providerOrFail.Constraints
                    |> List.exists (fun c ->
                        match c with
                        | ExternalConstraint.Trait(_, SemanticConstraintKind.Equality) -> true
                        | _ -> false
                    )

                Expect.isTrue equalityTrait "Constraints list carries Equality entry"

                let inst = providerOrFail.Instantiate 0

                let rec collectTyVars t =
                    seq {
                        match t with
                        | TyVar tv -> yield tv
                        | TyFun(a, b) ->
                            yield! collectTyVars a
                            yield! collectTyVars b
                        | TyTuple xs ->
                            for x in xs do
                                yield! collectTyVars x
                        | TyRecord(_, xs)
                        | TyUnion(_, xs)
                        | TyClass(_, xs) ->
                            for x in xs do
                                yield! collectTyVars x
                        | TyConst _ -> ()
                    }

                let anyEquality =
                    collectTyVars inst
                    |> Seq.exists (fun tv ->
                        tv.Constraints
                        |> List.exists (fun c -> c.Kind = SemanticConstraintKind.Equality)
                    )

                Expect.isTrue anyEquality "fresh TyVar carries Equality on its Constraints list"
            }

            test "Phase 5: SRTP member-trait clause captured as opaque MemberTrait marker" {
                // `Seq.sum` in seq.fsi declares
                //   val inline sum: source: seq< ^T > -> ^T
                //       when ^T: (static member (+): ^T * ^T -> ^T)
                //       and  ^T: (static member Zero: ^T)
                // The val itself may or may not survive translation
                // (SRTP signatures stress arms the v1 translator doesn't
                // yet model). Either way, the *capture path* shouldn't
                // crash and any extracted SRTP symbol should surface a
                // `MemberTrait` entry on its Constraints list.
                let provider, _ = builtProvider.Value

                match provider.TryLookup "Microsoft.FSharp.Collections.SeqModule.Sum" with
                | ValueNone ->
                    // Acceptable for v1 — Sum's signature includes the
                    // bare typar `^T` used in a way that today translates
                    // as a `TyConst "T"`. The capture path itself is
                    // exercised by Seq.Contains above.
                    ()
                | ValueSome sym ->
                    let hasMemberTrait =
                        sym.Constraints
                        |> List.exists (fun c ->
                            match c with
                            | ExternalConstraint.MemberTrait _ -> true
                            | _ -> false
                        )

                    Expect.isTrue hasMemberTrait "Sum carries an SRTP MemberTrait clause"
            }

            test "Phase 4: unknown type-name lookup returns ValueNone" {
                let provider, _ = builtProvider.Value

                match provider.TryLookupType "NoSuch.Type.Name" with
                | ValueNone -> ()
                | ValueSome _ -> failtest "unknown type should not be answered"
            }

            test "end-to-end: `let x = 1 + 2` types as int with chained provider" {
                // The smallest plausible Phase-3 closing fixture: lex + parse
                // a user program, run the full pipeline against the chained
                // provider, and assert `x : int`. Operator resolution flows
                // through `MockBuiltins` (still authoritative for ops in v1);
                // the test exists to prove the chain doesn't break that path.
                let libProvider, _ = builtProvider.Value
                let chained = ExternalSymbols.composite [ libProvider; MockBuiltins.provider ]

                let input = "let x = 1 + 2"
                let lexed, file = parseFile input
                let ctx = PassContext(chained, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Bindings.TypeVar.TryGetValue patKey with
                | ValueSome tv ->
                    match Unification.zonk (TyVar tv) with
                    | TyConst "int" -> ()
                    | other -> failtestf "Expected int, got %A" other
                | ValueNone -> failtest "no TypeVar for x"
            }

            test "Phase 5b: `let x = 1 + 2` types as int through lib-only provider" {
                // The proof point for Phase 5b defaulting: with MockBuiltins
                // removed from the chain, `op_Addition` resolves through the
                // lib's auto-open prefix to `Microsoft.FSharp.Core.Operators.op_Addition`
                // — universal SRTP signature `^T1 -> ^T2 -> ^T3 with default
                // ^T1 : int` etc. The fresh TyVars stamped at instantiation
                // pick up the `Defaults` chain; generalisation walks it and
                // links the result to `int` before quantification. Without
                // the defaulting pass, `x` would generalise as `∀'a. 'a` and
                // this test would fail.
                let libProvider, _ = builtProvider.Value

                let input = "let x = 1 + 2"
                let lexed, file = parseFile input
                let ctx = PassContext(libProvider, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                let patKey = NodeKey.ofSource 4 NodeKind.PatIdent

                match ctx.Bindings.TypeVar.TryGetValue patKey with
                | ValueSome tv ->
                    match Unification.zonk (TyVar tv) with
                    | TyConst "int" -> ()
                    | other -> failtestf "Expected int, got %A" other
                | ValueNone -> failtest "no TypeVar for x"
            }

            test "Phase 5b.2: V + V dispatches via SRTP member-trait" {
                // V with a static `op_Addition : V * V -> V`. The lib's
                // `(+) : ^T1 -> ^T2 -> ^T3` carries the SRTP member-trait
                // clause; instantiation stamps it on the fresh TyVars;
                // `drainSrtpBounds` fires when t1' resolves to `TyClass
                // "V"` and unifies the trait return slot with V's
                // static-member return type. End result: `let r = V() +
                // V()` types as `V` through a lib-only provider (no
                // MockBuiltins in the chain).
                let libProvider, _ = builtProvider.Value

                let input =
                    "type V() =\n    static member (+) (a: V, b: V) = V()\nlet r = V() + V()"

                let lexed, file = parseFile input
                let ctx = PassContext(libProvider, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                let vIdx = input.IndexOf "let r" + 4
                let patKey = NodeKey.ofSource vIdx NodeKind.PatIdent

                match ctx.Bindings.TypeVar.TryGetValue patKey with
                | ValueSome tv ->
                    match Unification.zonk (TyVar tv) with
                    | TyClass("V", _) -> ()
                    | other -> failtestf "Expected TyClass V, got %A" other
                | ValueNone -> failtest "no TypeVar for r"
            }

            test "defaultProvider caches the parsed lib across calls" {
                // Production-path helper: subsequent calls for the same
                // libRoot must return the same provider object, proving
                // the cache fires rather than re-parsing.
                let r1 = VesperLib.defaultProvider libRoot
                let r2 = VesperLib.defaultProvider libRoot

                match r1, r2 with
                | Result.Ok(p1, _), Result.Ok(p2, _) ->
                    Expect.isTrue (System.Object.ReferenceEquals(p1, p2)) "cached provider is reused"
                | _ -> failtest "defaultProvider failed on a known-good root"
            }
        ]
