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
                    | TyFun(TyConst("bool", _), TyConst("bool", _)) -> ()
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

                    // `option<'T>` is a transparent abbreviation for `Option<'T>`;
                    // so the head is the union name `Option`, not the abbreviation `option`. The
                    // FSharp.Core port declares `Option` with GADT-style cases
                    // (`| Some: Value:'T -> 'T option`); GADT-case extraction
                    // registers a genuine `Union` shape, so the kinded head bakes a proper `TyUnion`.
                    // Generic compiled names are arity-suffixed (`Option`1`), matching
                    // the emitted metadata name and the consumer's `keysFor` probe.
                    let optionName = "Microsoft.FSharp.Core.Option`1"

                    // Two TyVars share identity iff they're the same object — fresh
                    // instantiations must produce disjoint TyVars.
                    match inst1, inst2 with
                    | TyFun(TyFun(TyVar a1, TyVar b1), TyFun(TyUnion(n1, args1), TyUnion(n2, args2))),
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
                    // Arity-suffixed compiled name (`Result`2`).
                    let resultName = "Microsoft.FSharp.Core.Result`2"

                    // val map: ('T -> 'U) -> Result<'T, 'TError> -> Result<'U, 'TError>.
                    // `Result` is declared with ordinary cases, so dependency-aware
                    // extraction kinds the head as a proper `TyUnion`,
                    // not the old kind-agnostic `TyRecord` placeholder.
                    match inst with
                    | TyFun(TyFun(TyVar t, TyVar u), TyFun(TyUnion(n1, args1), TyUnion(n2, args2))) when
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

            test "cross-package nominal resolves through ambient shapes and bakes kind-correct" {
                // A dependency package contributes its type shapes through
                // `ExtractCtx.AmbientShapes`, keyed by qualified compiled name. A downstream
                // package's signature that references one of those types — by its
                // fully-qualified name and, separately, via an `open` — must (a)
                // RESOLVE (the reference is no longer silently skipped because the
                // name is absent from this package's own index) and (b) bake the
                // correct kind (`TyUnion` here, since the dependency contributes a
                // `Union` shape) at extraction time. The corpus's
                // real cross-package references are all abbreviations to unresolvable
                // BCL/GADT types, so this synthetic fixture exercises the path directly.
                let widgetShape = ExternalTypeShape.Union(1, [||], SymbolOrigin.Empty)

                let ambient name =
                    if name = "Dep.Widget" then
                        ValueSome widgetShape
                    else
                        ValueNone

                let input =
                    "namespace App\n\nopen Dep\n\nmodule M =\n    val qualified: Dep.Widget<int> -> int\n    val viaOpen: Widget<int> -> int\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                ctx.AmbientShapes <- ambient
                VesperLib.extractSymbols ctx parsed
                // Vals are stashed during extraction and built into `ctx.Symbols` by
                // the finalize pass once the registry is complete.
                VesperLib.finalizeDeferred ctx

                // Locate each val by its source name suffix so the assertion does
                // not hinge on the exact module-path compilation.
                let instOf (suffix: string) : SemType =
                    let mutable found = ValueNone

                    for kv in ctx.Symbols do
                        if found.IsNone && kv.Key.EndsWith("." + suffix) then
                            found <- ValueSome(kv.Value.Instantiate 0)

                    match found with
                    | ValueSome ty -> ty
                    | ValueNone ->
                        failtestf
                            "val '%s' was not extracted (cross-package reference skipped?). Symbols: %A"
                            suffix
                            (Seq.toList ctx.Symbols.Keys)

                let assertWidgetIntToInt (label: string) (ty: SemType) =
                    match ty with
                    | TyFun(TyUnion("Dep.Widget`1", args), TyConst("int", _)) when args.Length = 1 ->
                        match args.[0] with
                        | TyConst("int", _) -> ()
                        | other -> failtestf "%s: expected Dep.Widget<int>, got arg %A" label other
                    | other -> failtestf "%s: expected (Dep.Widget<int> -> int) with TyUnion head, got %A" label other

                assertWidgetIntToInt "fully-qualified reference" (instOf "qualified")
                assertWidgetIntToInt "reference via open" (instOf "viaOpen")
            }

            test "a signature naming an out-of-scope type bakes TyUnknown" {
                // No ambient shape and no local type declares `Missing.Thing`, so
                // `resolveTypeName` fails. Instead of a silent skip
                // (the val landing in `ctx.Skipped`), extraction retains the val and
                // bakes a `TyUnknown` leaf carrying the unresolved name — which a
                // consumer surfaces as a use-site diagnostic (see the unify arm in
                // `Passes/Unification/Engine.fs`).
                let input = "namespace App\n\nmodule M =\n    val broken: Missing.Thing -> int\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let mutable found = ValueNone

                for kv in ctx.Symbols do
                    if found.IsNone && kv.Key.EndsWith(".broken") then
                        found <- ValueSome(kv.Value.Instantiate 0)

                match found with
                | ValueNone ->
                    failtestf
                        "val 'broken' was skipped, not retained as TyUnknown. Symbols: %A"
                        (Seq.toList ctx.Symbols.Keys)
                | ValueSome ty ->
                    match ty with
                    | TyFun(TyUnknown name, TyConst("int", _)) ->
                        Expect.stringContains name "Thing" "TyUnknown carries the unresolved name"
                    | other -> failtestf "expected (TyUnknown -> int); got %A" other
            }

            test "Step C: module-function ValRepr / CompiledForm captured from the .fsi arity" {
                // The cross-assembly preserved-signatures path: a
                // `.fsi` `val`'s `CurriedSig`/`ArgsSpec` already encodes the source
                // arity the bare curried type erases, so `finalizeVal` records BOTH
                // the source `ValRepr` and the derived flat `CompiledForm` on the
                // symbol. The crux is `tupleGroup` vs `singleTuple`: identical bare
                // type `int * int -> int`, but the first is a tupled GROUP (flattens
                // to two CLR params) and the second a single tuple PARAM (stays one) —
                // a distinction only the recorded `ValRepr` carries.
                let input =
                    "module TestC\n"
                    + "val curried: int -> int -> int\n"
                    + "val tupleGroup: int * int -> int\n"
                    + "val singleTuple: (int * int) -> int\n"
                    + "val loneUnit: unit -> int\n"
                    + "val voidRet: int -> unit\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "testc.fsi"
                                Absolute = "testc.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let symOf (suffix: string) : ExternalSymbol =
                    let mutable found = ValueNone

                    for kv in ctx.Symbols do
                        if found.IsNone && kv.Key.EndsWith("." + suffix) then
                            found <- ValueSome kv.Value

                    match found with
                    | ValueSome s -> s
                    | ValueNone -> failtestf "val '%s' not extracted. Symbols: %A" suffix (Seq.toList ctx.Symbols.Keys)

                let intF = FTConst("int", EqArray.empty)
                let pairF = FTTuple(EqArray.ofList [ intF; intF ])

                // The compiled form is derived from the captured `ValRepr` on demand —
                // the same `TastLower.compiledOf` rule the codegen boundary applies, so
                // the test pins the single-sourced derivation, not a stored copy.
                let compiledOf (suffix: string) : Frozen.CompiledForm =
                    match (symOf suffix).ValRepr with
                    | ValueSome vr -> TastLower.compiledOf vr
                    | ValueNone -> failtestf "val '%s' carries no ValRepr" suffix

                // The flat compiled parameter TYPES the member-ref would encode.
                let compiledParamTys (suffix: string) : FrozenType list =
                    (compiledOf suffix).Params |> List.map (fun p -> p.Ty)

                let compiledReturn (suffix: string) : Frozen.CompiledReturn = (compiledOf suffix).Return

                // A terse rendering of the source group shape (the `ValRepr` arity).
                let groupTags (suffix: string) : string list =
                    match (symOf suffix).ValRepr with
                    | ValueSome vr ->
                        vr.Groups
                        |> List.map (fun g ->
                            match g with
                            | ArgGroupG.GUnit _ -> "unit"
                            | ArgGroupG.GSimple _ -> "simple"
                            | ArgGroupG.GTuple(TPatG.Tuple(items, _, _)) -> sprintf "tuple%d" items.Length
                            | ArgGroupG.GTuple _ -> "tuple?"
                        )
                    | ValueNone -> failtestf "val '%s' carries no ValRepr" suffix

                // Curried: two single-arg groups, two flat params, value return.
                Expect.equal (groupTags "curried") [ "simple"; "simple" ] "curried source arity"
                Expect.equal (compiledParamTys "curried") [ intF; intF ] "curried flat params"
                Expect.equal (compiledReturn "curried") (Frozen.CompiledReturn.RValue intF) "curried return"

                // Tupled group: one width-2 group flattens to TWO flat params.
                Expect.equal (groupTags "tupleGroup") [ "tuple2" ] "tupled-group source arity"
                Expect.equal (compiledParamTys "tupleGroup") [ intF; intF ] "tupled group flattens to 2 params"

                // Single tuple param: SAME bare type, but stays ONE param.
                Expect.equal (groupTags "singleTuple") [ "simple" ] "single-tuple-param source arity"
                Expect.equal (compiledParamTys "singleTuple") [ pairF ] "single tuple param stays one ValueTuple param"

                // Lone unit param erases to a parameterless method.
                Expect.equal (groupTags "loneUnit") [ "unit" ] "lone-unit source arity"
                Expect.equal (compiledParamTys "loneUnit") [] "lone unit param erased (parameterless)"
                Expect.equal (compiledReturn "loneUnit") (Frozen.CompiledReturn.RValue intF) "lone-unit return"

                // Unit return → RVoid.
                Expect.equal (compiledParamTys "voidRet") [ intF ] "void fn keeps its real param"
                Expect.equal (compiledReturn "voidRet") Frozen.CompiledReturn.RVoid "unit return → RVoid"
            }

            test "A body-less type registers an Opaque residue shape, not absence" {
                // An `enum` carries no front-end-modelled body shape
                // (enum/delegate kinds are deferred). The
                // deferral registers an explicit `Opaque` residue, so every
                // registered name carries a shape and `TryLookupType` returns
                // `ValueSome(Opaque)` rather than absence.
                let input =
                    "namespace App\n\nmodule M =\n    type Thing =\n        | Red = 0\n        | Green = 1\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed

                let thingShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "Thing" then
                            found <- ValueSome kv.Value

                    found

                match thingShape with
                | ValueSome(ExternalTypeShape.Opaque arity) -> Expect.equal arity 0 "Opaque carries the declared arity"
                | ValueSome other -> failtestf "expected an Opaque shape for the enum; got %A" other
                | ValueNone ->
                    failtestf
                        "enum registered no shape (name-without-shape gap). Shapes: %A"
                        (Seq.toList ctx.TypeShapes.Keys)
            }

            test "A `struct … end` value type extracts as a Class shape flagged IsValueType" {
                // A `type X = struct … end` value type in a `.fsi`
                // must surface its value-type-ness through the provider, or a consumer's
                // encoder emits `ELEMENT_TYPE_CLASS` for a referenced-package struct and
                // the loader faults "value type mismatch". Extraction registers a
                // `Class` shape (no front-end-modelled body) whose `Flags.IsValueType`
                // is `true` — the contract-layer twin of the metadata layer's
                // `Type.IsValueType` read.
                let input =
                    "namespace App\n\nmodule M =\n    type Point =\n        struct\n            val X: int\n            val Y: int\n        end\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed

                let pointShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "Point" then
                            found <- ValueSome kv.Value

                    found

                match pointShape with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    Expect.isTrue shape.Flags.IsValueType "the struct's Class shape is flagged IsValueType"
                    Expect.isFalse shape.IsInterface "a struct is not an interface"
                | ValueSome other -> failtestf "expected a Class shape for the struct; got %A" other
                | ValueNone -> failtestf "struct registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)
            }

            test "A GADT-cased union extracts as a genuine Union shape" {
                // The cons-list shape (operator cases with explicit return types):
                // `([])` and `(::)` are GADT-syntax. GADT-case extraction
                // registers a real `Union` — cases named by their canonical *ctor*
                // form (`Empty` / `Cons`, via the shared
                // `OperatorNames.unionCaseCtorName`), matching `FreezeExpr` and
                // codegen, fields drawn from the `(::)` signature's args, the
                // return type ignored. (`Thing<'T>` stands in for `'T list` to keep
                // the fixture self-contained — the self-referential field resolves
                // because the type's name is registered before its body is kinded.)
                let input =
                    "namespace App\n\nmodule M =\n    type Thing<'T> =\n        | ([]): Thing<'T>\n        | (::): Head: 'T * Tail: Thing<'T> -> Thing<'T>\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed

                let thingShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        // Generic compiled names are arity-suffixed (`Thing`1`).
                        if found.IsNone && kv.Key.EndsWith "Thing`1" then
                            found <- ValueSome kv.Value

                    found

                match thingShape with
                | ValueSome(ExternalTypeShape.Union(arity, cases, _)) ->
                    Expect.equal arity 1 "Union carries the declared arity"
                    Expect.equal cases.Length 2 "two cases extracted"
                    Expect.equal cases.[0].Name "Empty" "`([])` names the nullary case by its canonical ctor form"
                    Expect.equal cases.[0].FrozenFieldTypes.Length 0 "the nullary case has no fields"
                    Expect.equal cases.[1].Name "Cons" "`(::)` names the cons case by its canonical ctor form"
                    Expect.equal cases.[1].FrozenFieldTypes.Length 2 "cons has Head + Tail fields"
                    Expect.equal cases.[1].FieldNames [| ValueSome "Head"; ValueSome "Tail" |] "cons field names"
                | ValueSome other -> failtestf "expected a Union shape for the GADT-cased union; got %A" other
                | ValueNone -> failtestf "GADT union registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)
            }

            test "extraction records [<RequireQualifiedAccess>] unions and stamps their cases" {
                // opens-overhaul-plan Gap 1: the `[<RequireQualifiedAccess>]` attribute
                // on an extracted union is read into `ctx.RqaTypes` and rides through
                // the reverse case-name index as `ExternalUnionCase.IsRequireQualifiedAccess`,
                // so a consumer's bare reference to an RQA case can be rejected.
                let input =
                    "namespace App\n\nmodule M =\n    [<RequireQualifiedAccess>]\n    type Color =\n        | Red\n        | Green\n\n    type Hue =\n        | Blue\n        | Cyan\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed

                Expect.isTrue
                    (ctx.RqaTypes |> Seq.exists (fun n -> n.EndsWith "Color"))
                    "the RQA Color union is recorded in RqaTypes"

                Expect.isFalse
                    (ctx.RqaTypes |> Seq.exists (fun n -> n.EndsWith "Hue"))
                    "the ordinary Hue union is not recorded as RQA"

                // The flag rides through the reverse case-name index built by toProvider.
                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupUnionCase "Red" with
                | ValueSome uc -> Expect.isTrue uc.IsRequireQualifiedAccess "Red's union (Color) is RQA"
                | ValueNone -> failtest "Red case not found in the reverse index"

                match provider.TryLookupUnionCase "Blue" with
                | ValueSome uc -> Expect.isFalse uc.IsRequireQualifiedAccess "Blue's union (Hue) is not RQA"
                | ValueNone -> failtest "Blue case not found in the reverse index"
            }

            test "A reference to an Opaque-shaped type is refused at bake time" {
                // The consumer half of the residue: a signature naming a type whose
                // in-scope shape is `Opaque` (an enum / delegate / unmodelled body)
                // has no kind to bake. The val signature is translated in the finalize
                // pass, where `mkNominal`'s `Opaque` arm raises `BodylessExternalShape`;
                // the pass tolerates it as a per-val skip (the symbol is dropped and
                // recorded in `ctx.Skipped`) rather than minting a placeholder or
                // aborting the whole provider build. Seeded through the ambient so the
                // path is exercised directly.
                let ambient name =
                    if name = "Dep.Widget" then
                        ValueSome(ExternalTypeShape.Opaque 1)
                    else
                        ValueNone

                let input =
                    "namespace App\n\nopen Dep\n\nmodule M =\n    val qualified: Dep.Widget<int> -> int\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                ctx.AmbientShapes <- ambient

                // Extraction stashes the val; the finalize pass translates it, hits
                // the `Opaque` arm, and drops the val (recording the reason) rather
                // than minting a placeholder or aborting the build.
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let qualifiedRegistered =
                    ctx.Symbols.Keys |> Seq.exists (fun k -> k.EndsWith ".qualified")

                Expect.isFalse qualifiedRegistered "an Opaque-headed val is not registered as a symbol"

                let skipped = ctx.Skipped |> Seq.exists (fun (_, msg) -> msg.Contains "qualified")

                Expect.isTrue skipped "the dropped val is recorded in ctx.Skipped"
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

                match provider.TryLookupType "Microsoft.FSharp.Core.option`1" with
                | ValueNone -> failtest "Microsoft.FSharp.Core.option shape not found"
                | ValueSome(ExternalTypeShape.Abbrev(arity, frozen)) ->
                    Expect.equal arity 1 "option has one typar"
                    let body = instantiateDeclaring frozen [| TyConst("int", EqArray.empty) |]

                    match body with
                    | TyUnion(name, args) when
                        args.Length = 1
                        && (
                            match args.[0] with
                            | TyConst("int", _) -> true
                            | _ -> false
                        )
                        ->
                        Expect.stringContains name "Option" "abbreviation expands to Option"
                    | other -> failtestf "Expected TyUnion(...Option, [int]); got %A" other
                | other -> failtestf "Expected Abbrev shape; got %A" other
            }

            test "Phase 4: Result<_,_> union exposes Ok / Error cases" {
                let provider, _ = builtProvider.Value

                match provider.TryLookupType "Microsoft.FSharp.Core.Result`2" with
                | ValueNone -> failtest "Result shape not found"
                | ValueSome(ExternalTypeShape.Union(arity, cases, _)) ->
                    Expect.equal arity 2 "Result has two typars"

                    let names = cases |> Array.map (fun c -> c.Name) |> Array.sort

                    Expect.equal names [| "Error"; "Ok" |] "Ok + Error cases extracted"

                    let okCase = cases |> Array.find (fun c -> c.Name = "Ok")
                    Expect.equal okCase.FrozenFieldTypes.Length 1 "Ok carries one field"

                    let okFieldType =
                        instantiateDeclaring
                            okCase.FrozenFieldTypes.[0]
                            [| TyConst("int", EqArray.empty); TyConst("string", EqArray.empty) |]

                    match okFieldType with
                    | TyConst("int", _) -> ()
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

            test "finalize fills real templates on extracted shapes" {
                // Post the `toProvider` finalize pass,
                // every extracted shape's deferred `FrozenType` template is filled —
                // no `<deferred>` sentinel survives — and instantiates without
                // throwing. Genuinely body-less heads (`byref`) degrade to the
                // `<unfreezable>` sentinel and are skipped (there is no template to
                // realise). Guards the finalize pass against a regression that would
                // leave a sentinel on a shape codegen later reads.
                let provider, _ = builtProvider.Value

                let deferred = FTUnknown "<deferred>"
                let unfreezable = FTUnknown "<unfreezable external template>"

                /// Ground args for an arity-`n` declaring substitution.
                let argsFor (n: int) : SemType[] =
                    Array.init n (fun i -> TyConst(sprintf "g%d" i, EqArray.empty))

                /// Assert a finalized type-shape template is real (not the deferred
                /// sentinel) and instantiates without throwing.
                let checkTemplate (label: string) (arity: int) (frozen: FrozenType) =
                    Expect.notEqual frozen deferred (sprintf "%s: template finalized (not deferred)" label)

                    if frozen <> unfreezable then
                        instantiateDeclaring frozen (argsFor arity) |> ignore

                // A representative cross-section: the `option` abbrev, the `Result`
                // union's case fields, and a published member signature.
                match provider.TryLookupType "Microsoft.FSharp.Core.option`1" with
                | ValueSome(ExternalTypeShape.Abbrev(arity, frozen)) -> checkTemplate "option abbrev" arity frozen
                | _ -> failtest "option abbrev not found"

                match provider.TryLookupType "Microsoft.FSharp.Core.Result`2" with
                | ValueSome(ExternalTypeShape.Union(arity, cases, _)) ->
                    for c in cases do
                        c.FrozenFieldTypes
                        |> Array.iteri (fun i ft -> checkTemplate (sprintf "Result.%s field %d" c.Name i) arity ft)
                | _ -> failtest "Result union not found"

                // A member signature, if `Option.Map` is published with one.
                match provider.TryLookupMember("Microsoft.FSharp.Core.option`1", "Map") with
                | ValueSome m when m.Signature.Return <> unfreezable && m.Signature.Return <> deferred ->
                    ExternalSymbols.instantiateSignature m (argsFor m.Signature.DeclaringArity) 0
                    |> ignore
                | _ -> ()
            }

            test "objnull abbrev (`obj | null`) extracts to obj [Set G5 root 1]" {
                // `type objnull = obj | null` (prim-types-object.fsi) is a *nullable
                // reference type*; its abbrev RHS parses to `Type.UnionType(obj, |,
                // null)`. The contract extractor's `translateType` used to refuse every
                // `UnionType`, so the abbrev froze to the `<unfreezable external
                // template>` sentinel and every consumer reference thawed to a
                // `TyUnknown` that `unify` rejects (set.fs:906 `IComparable.CompareTo`'s
                // `that: objnull`). The fix collapses the `T | null` form to its
                // non-null part `T` — Vesper SemTypes carry no nullability axis. Gated
                // through the synthetic-`.fsi` extract+finalize path (sibling to
                // "finalize fills real templates", which guards the dual sentinel).
                let input =
                    "namespace App\n\nmodule M =\n    type objnull = obj | null\n    val f: objnull -> int\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed: %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "App"
                                Relative = "app.fsi"
                                Absolute = "app.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let unfreezable = FTUnknown "<unfreezable external template>"

                // The abbrev body, post-finalize, must be the non-null part `obj` —
                // not the sentinel the blanket UnionType-refusal used to leave.
                let objnullShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "objnull" then
                            found <- ValueSome kv.Value

                    found

                match objnullShape with
                | ValueSome(ExternalTypeShape.Abbrev(arity, frozen)) ->
                    Expect.equal arity 0 "objnull is nullary"
                    Expect.notEqual frozen unfreezable "objnull did not freeze to the <unfreezable> sentinel"

                    match frozen with
                    | FTConst("obj", _) -> ()
                    | other -> failtestf "expected objnull to freeze to FTConst(\"obj\"); got %A" other
                | ValueSome other -> failtestf "expected an Abbrev shape for objnull; got %A" other
                | ValueNone ->
                    failtestf "objnull abbrev registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)
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
                        | TyUnknown _ -> ()
                        // The nominal arms above go through `TestHelpers`' partial
                        // active patterns, so the match isn't provably exhaustive.
                        | _ -> ()
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
                    // as a `TyConst("T", _)`. The capture path itself is
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
                    | TyConst("int", _) -> ()
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
                    | TyConst("int", _) -> ()
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
