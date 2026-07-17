module XParsec.FSharp.SemanticAnalysis.Tests.SignatureExtractorTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "SignatureExtractor"
        [
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
                let widgetShape = ExternalTypeShape.Union(1, [||], [||], SymbolOrigin.Empty)

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
                            found <- ValueSome(ExternalSymbols.instantiateSymbol kv.Value 0)

                    match found with
                    | ValueSome ty -> ty
                    | ValueNone ->
                        failtestf
                            "val '%s' was not extracted (cross-package reference skipped?). Symbols: %A"
                            suffix
                            (Seq.toList ctx.Symbols.Keys)

                let assertWidgetIntToInt (label: string) (ty: SemType) =
                    match ty with
                    | TyFun(TyUnion("Dep.Widget`1", args), TyConst(k, _)) when
                        args.Length = 1 && SymbolKeyOps.simpleName k = DisplayName "int"
                        ->
                        match args.[0] with
                        | TyConst(k, _) when SymbolKeyOps.simpleName k = DisplayName "int" -> ()
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
                        found <- ValueSome(ExternalSymbols.instantiateSymbol kv.Value 0)

                match found with
                | ValueNone ->
                    failtestf
                        "val 'broken' was skipped, not retained as TyUnknown. Symbols: %A"
                        (Seq.toList ctx.Symbols.Keys)
                | ValueSome ty ->
                    match ty with
                    | TyFun(TyUnknown name, TyConst(k, _)) when SymbolKeyOps.simpleName k = DisplayName "int" ->
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

                let intF = FTConst(RuntimeNames.intKey, EqArray.empty)
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

            test "A type's `interface <ty>` impls publish into FrozenInterfaces (rung-4 M7)" {
                // The contract-layer twin of the metadata layer's `buildClassInterfaces`:
                // a type's directly-declared `interface IBox<'T>` must surface on the
                // extracted shape's `FrozenInterfaces` (args over the declaring typars),
                // so a consumer's interface-impl witness (`tryInterfaceWitness`' external
                // arm) can recover a phantom typar from a struct seq's `IStructSeq<'T,'E>`
                // impl — the `.fsi` half of the struct-seq external-head graduation. Filled
                // by the deferred finalize pass (the interface type may forward-reference a
                // sibling), so the previously-empty `basic` default is overwritten.
                let input =
                    "namespace App\n\nmodule M =\n    type IBox<'T> =\n        abstract member Get: unit -> 'T\n\n    [<Struct>]\n    type Holder<'T> =\n        new: value: 'T -> Holder<'T>\n        interface IBox<'T>\n"

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
                // `FrozenInterfaces` is deferred (the interface type may forward-reference a
                // sibling), so it is only filled once the finalize pass runs.
                VesperLib.finalizeDeferred ctx

                let holderShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "Holder`1" then
                            found <- ValueSome kv.Value

                    found

                match holderShape with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    match shape.FrozenInterfaces with
                    | [| (name, args) |] ->
                        Expect.isTrue (name.EndsWith "IBox`1") (sprintf "the IBox interface is published; got %s" name)
                        Expect.equal args.Length 1 "IBox<'T> carries one type arg"

                        match args.[0] with
                        | FTTypar(TyparAxis.Declaring, 0) -> ()
                        | other -> failtestf "the interface arg is the declaring typar 'T; got %A" other
                    | other -> failtestf "expected exactly one published interface (IBox); got %A" other
                | ValueSome other -> failtestf "expected a Class shape for the struct; got %A" other
                | ValueNone -> failtestf "Holder registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)
            }

            test "`extern with` publishes interfaces into FrozenInterfaces and members" {
                // A NON-intrinsic `extern` type whose trailing `with` body declares
                // interfaces + members publishes a capability surface: it registers
                // exactly as a bodied class would — `interface IBar<'T>` lands in
                // `FrozenInterfaces` (deferred → filled by `finalizeDeferred`) and
                // `member M` lands in `ctx.TypeMembers`. (The intrinsic-primitive
                // `extern with` form — `type string = extern with …` — is deferred
                // separately; this exercises only the class/interface branch.)
                let input =
                    "namespace App\n\nmodule M =\n    type IBar<'T> =\n        abstract member Get: unit -> 'T\n\n    type Foo<'T> = extern with\n        interface IBar<'T>\n        member M: unit -> 'T\n"

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

                let fooShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "Foo`1" then
                            found <- ValueSome kv.Value

                    found

                match fooShape with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    match shape.FrozenInterfaces with
                    | [| (name, args) |] ->
                        Expect.isTrue (name.EndsWith "IBar`1") (sprintf "the IBar interface is published; got %s" name)
                        Expect.equal args.Length 1 "IBar<'T> carries one type arg"

                        match args.[0] with
                        | FTTypar(TyparAxis.Declaring, 0) -> ()
                        | other -> failtestf "the interface arg is the declaring typar 'T; got %A" other
                    | other -> failtestf "expected exactly one published interface (IBar); got %A" other
                | ValueSome other -> failtestf "expected a Class shape for the extern type; got %A" other
                | ValueNone -> failtestf "Foo registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)

                // The `with member M` rides `ctx.TypeMembers` (a non-interface class's
                // members are served through `TryLookupMember`, not the shape).
                let fooMembers =
                    let mutable found = ValueNone

                    for kv in ctx.TypeMembers do
                        if found.IsNone && kv.Key.EndsWith "Foo`1" then
                            found <- ValueSome kv.Value

                    found

                match fooMembers with
                | ValueSome members ->
                    Expect.isTrue
                        (members |> Seq.exists (fun m -> m.Name = "M"))
                        (sprintf "member M is published; got %A" [ for m in members -> m.Name ])
                | ValueNone ->
                    failtestf "Foo registered no members. Member tables: %A" (Seq.toList ctx.TypeMembers.Keys)
            }

            test "A GADT-cased union extracts as a genuine Union shape" {
                // The cons-list shape (operator cases with explicit return types):
                // `([])` and `(::)` are GADT-syntax. GADT-case extraction
                // registers a real `Union` — cases named by their canonical *ctor*
                // form (`Empty` / `Cons`, via the shared
                // `OperatorNames.unionCaseCtorName`), matching `ElaborateExpr` and
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
                | ValueSome(ExternalTypeShape.Union(arity, cases, _, _)) ->
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
                // The `[<RequireQualifiedAccess>]` attribute
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

            test "objnull abbrev (`obj | null`) extracts to the union `FTOr [obj; null]`" {
                // `type objnull = obj | null` (prim-types-object.fsi) is a *nullable
                // reference type*; its abbrev RHS parses to `Type.UnionType(obj, |,
                // null)`. `objnull` is NOT a primitive — it is the ordinary `obj | null`
                // union, so its abbrev body freezes to the anonymous union `FTOr [obj;
                // null]` (the `null` member is the cross-backend `nullKey` intrinsic),
                // matching the front end's `Type.Null` mint so an extracted `objnull`
                // unifies with a written `T | null`. (Earlier this collapsed to the
                // non-null part `obj`, dropping the `null` member; the union is the
                // faithful representation.) Gated through the synthetic-`.fsi`
                // extract+finalize path (sibling to "finalize fills real templates",
                // which guards the dual sentinel).
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

                // The abbrev body, post-finalize, must be the union `FTOr [obj; null]` —
                // not the sentinel the blanket UnionType-refusal used to leave, nor the
                // collapse to bare `obj`.
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
                    | FTOr members ->
                        let names =
                            EqSet.toList members
                            |> List.map (fun ft ->
                                match ft with
                                | FTConst(k, _) ->
                                    let (DisplayName name) = SymbolKeyOps.simpleName k
                                    name
                                | other -> failtestf "expected FTConst members in objnull union; got %A" other
                            )
                            |> List.sort

                        Expect.equal names [ "null"; "obj" ] "objnull is the union of `obj` and `null`"
                    | other -> failtestf "expected objnull to freeze to FTOr [obj; null]; got %A" other
                | ValueSome other -> failtestf "expected an Abbrev shape for objnull; got %A" other
                | ValueNone ->
                    failtestf "objnull abbrev registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)
            }

            test "extern interface with abstract member extracts as an interface Class carrying its member surface" {
                // The BCL-free neutral capability surface `type disposable = extern interface
                // with abstract member Dispose : unit -> unit` extracts to a
                // `Class{IsInterface=true}` carrying `Dispose`, when NO `(# … #)` repr is
                // present (isIntrinsic=false → the `not isIntrinsic` bodied arm in
                // `VesperLib.extractTypeSig` runs `extractBodiedClassLike`). Interface-ness is
                // the EXPLICIT `interface` tag, not inferred from the all-abstract body. The
                // member-surface
                // half of a BCL-free capability is therefore free; the per-target IDENTITY is
                // supplied separately (a `.fs` `(# … #)` repr → an `IntrinsicInterface` on CLR, the
                // `capabilities-compat.js.fsi` shim on JS — NOT a plain `.fs` abbreviation,
                // which is harvested only for `(# … #)` while extraction runs only on `.fsi`).
                let input =
                    "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n"

                let lexed =
                    match Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ast =
                    let reader = Reader.ofLexed lexed input Set.empty

                    match FSharpAst.parseSignature reader with
                    | Result.Error e -> failtestf "parse failed (extern interface with did not parse): %A" e
                    | Result.Ok ast -> ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                BucketName = "Vesper"
                                Relative = "capabilities.fsi"
                                Absolute = "capabilities.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let key =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "disposable" then
                            found <- ValueSome kv.Key

                    match found with
                    | ValueSome k -> k
                    | ValueNone ->
                        failtestf "disposable registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)

                match ctx.TypeShapes.[key] with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue
                        shape.IsInterface
                        "extern interface with abstract member extracts as an INTERFACE Class"
                | other -> failtestf "expected a Class shape for disposable; got %A" other

                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "Dispose") with
                | ValueSome _ -> ()
                | ValueNone -> failtestf "Dispose member surface was dropped; members: (key=%s)" key
            }

            test
                "dual-faced capability interface: extern interface with abstract member + (# … #) repr → Class carrying platform face, NOT a reverse-canon entry" {
                // A capability anchor whose `.fsi` declares an interface member surface AND
                // whose `.fs` binds a platform type (`type disposable = (# "System.IDisposable"
                // #)`) must extract to ONE dual-faced shape: a `Class{IsInterface=true}` with
                // the member surface PLUS a `CapabilityFace` carrying `(canon, platform)`, so
                // it reconciles to its BCL spelling. UNLIKE `exn === System.Exception`, the
                // reconciliation rides the `CapabilityFace` / `CapabilityIdentity` — NOT the
                // reverse-canon map: a capability interface resolves to a `TyClass` constraint,
                // so a reverse entry would be dead weight (its only reader, `MetadataSymbols`,
                // canonicalizes to `TyConst` and guards interfaces out). This is the CLR
                // build's shape; on JS the `.fs` omits the repr, so `CapabilityFace` is
                // `ValueNone` and the canonical identity stands (see the compat-shim path).
                // (Synthetic: the `(# … #)` repr is seeded directly into the harvest dicts,
                // mirroring the CLR build where the base `.fs` repr seeds both
                // `IntrinsicBaseReprs` — the primitive marker that makes `isIntrinsic` true —
                // and `IntrinsicReprs`, the platform face.)
                let ctx = VesperLib.ExtractCtx.empty ()
                ctx.IntrinsicBaseReprs.["disposable"] <- "System.IDisposable"
                ctx.IntrinsicReprs.["disposable"] <- "System.IDisposable"

                let input =
                    "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n"

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
                                BucketName = "Vesper"
                                Relative = "capabilities.fsi"
                                Absolute = "capabilities.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let key =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "disposable" then
                            found <- ValueSome kv.Key

                    match found with
                    | ValueSome k -> k
                    | ValueNone ->
                        failtestf "disposable registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)

                match ctx.TypeShapes.[key] with
                | ExternalTypeShape.IntrinsicInterface iface ->
                    Expect.equal
                        (SymbolKey.Type iface.Canon)
                        (RuntimeNames.primitiveKey "disposable")
                        "IntrinsicInterface canon is the contract-sourced qualified identity (`namespace Vesper`)"

                    Expect.equal
                        iface.Platform
                        "System.IDisposable"
                        "IntrinsicInterface platform face is the `.fs` repr"
                | other -> failtestf "expected an IntrinsicInterface shape for disposable; got %A" other

                let provider = VesperLib.ExtractCtx.toProvider ctx

                // The member surface survived alongside the platform face.
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "Dispose") with
                | ValueSome _ -> ()
                | ValueNone -> failtest "Dispose member surface was dropped from the IntrinsicInterface"

                // A capability interface is deliberately ABSENT from the reverse-canon map —
                // reconciliation flows through the `CapabilityFace` above, not this map (the
                // entry would be dead weight, and keeping it would force an `IsInterface` guard
                // back into the reverse-map readers).
                match provider.IntrinsicReverseCanon.TryFind "System.IDisposable" with
                | None -> ()
                | Some canons -> failtestf "capability interface must NOT enter the reverse-canon map; found %A" canons
            }

            test "CONCRETE member surface on an intrinsic primitive is admitted as a member-bearing Class" {
                // A CONCRETE (non-interface) member surface over an intrinsic repr is a general
                // platform-binding capability: the `(# … #)`-bound member's body is served as a
                // member-keyed inline splice, so the surface registers as a plain member-bearing
                // `Class` (`IsInterface=false`, members via `TryLookupMember`), resolving to a
                // `TyClass`. UNLIKE a capability INTERFACE (all-abstract body), which republishes
                // to an `IntrinsicInterface`, a concrete-member surface is NOT an interface and
                // stays a `Class`.
                let ctx = VesperLib.ExtractCtx.empty ()
                // `isIntrinsic` is decided by the BASE repr marker (the primitive's `.fs`).
                ctx.IntrinsicBaseReprs.["widget"] <- "System.Widget"
                ctx.IntrinsicReprs.["widget"] <- "System.Widget"

                // A CONCRETE instance member (`member M`), NOT `abstract member`.
                let input =
                    "namespace Vesper\n\ntype widget = extern with\n    member M : unit -> unit\n"

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
                                BucketName = "Vesper"
                                Relative = "prim-types-widget.fsi"
                                Absolute = "prim-types-widget.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let key =
                    match ctx.TypeShapes.Keys |> Seq.tryFind (fun k -> k.EndsWith "widget") with
                    | Some k -> k
                    | None -> failtestf "widget registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)

                // Admitted as a member-bearing `Class` (NOT an interface), NOT rejected or kept a
                // bare `Intrinsic`. (A capability INTERFACE would instead republish to an
                // `IntrinsicInterface`; a concrete-member surface is not an interface.)
                match ctx.TypeShapes.[key] with
                | ExternalTypeShape.Class shape ->
                    Expect.isFalse shape.IsInterface "a concrete-member surface is not an interface"
                | other -> failtestf "expected a member-bearing Class for widget; got %A" other

                // No rejection diagnostic — the old inert-leaf guardrail is lifted.
                let rejected =
                    ctx.Diagnostics
                    |> Seq.exists (fun (_, msg) -> msg.Contains "concrete member surface on an intrinsic primitive")

                Expect.isFalse rejected "the lifted guardrail must NOT emit a rejection diagnostic"

                // The member surface survives on the Class, resolvable via TryLookupMember.
                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "M") with
                | ValueSome _ -> ()
                | ValueNone -> failtest "concrete member surface `M` was dropped from the member-bearing Class"
            }

            // The cross-face equality the contract's key mint exists to buy. `T` is declared
            // inside `module M` in `namespace Test.A`, so the identity a consumer's local
            // containment would mint for it is an `InModule` chain — and the contract's store
            // must answer THAT key, not a separately-spelled string. The store's index is the
            // key's own rendering, so the two agree by construction rather than by coincidence.
            test "a module-held contract type answers the KEY a module containment mints" {
                let input = "namespace Test.A\n\nmodule M =\n    type T = { X: int }\n"

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
                                BucketName = "Test"
                                Relative = "a.fsi"
                                Absolute = "a.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                // The key a CONSUMER's local containment mints for `T` — built from the
                // holder chain, never from a name.
                let key: TypeKey =
                    {
                        Holder = TypeHolder.InModule(SymbolKeyOps.moduleInNamespace "Test.A" "M")
                        Name = "T"
                        TyparArity = 0
                    }

                Expect.equal
                    (SymbolKeyOps.typeMetaName key)
                    "Test.A.M+T"
                    "the module's holder class encloses the type, as the CLR spells a nested type"

                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupType(SymbolKey.Type key) with
                | ValueSome(ExternalTypeShape.Record _) -> ()
                | ValueSome other -> failtestf "the key answered, but with the wrong shape: %A" other
                | ValueNone -> failtest "the contract store must be addressable by the module-held type's key"
            }

            // …and the SOURCE still names it with dots (`Test.A.M.T`, as `byref<'T,
            // ByRefKinds.In>` names `In`). The written spelling is not the metadata name, so
            // it reaches the identity through a redirect (`SymbolKeyOps.tryDottedModuleHeld`
            // over the DECLARED module containment) — never by being re-cut into a key of its
            // own, which would absorb the module into the namespace path and mint an unequal
            // identity.
            test "a module-held contract type still resolves by the name the source WRITES" {
                let input = "namespace Test.A\n\nmodule M =\n    type T = { X: int }\n"

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
                                BucketName = "Test"
                                Relative = "a.fsi"
                                Absolute = "a.fsi"
                            }
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }

                let ctx = VesperLib.ExtractCtx.empty ()
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                // The dotted source spelling is NOT an index entry of its own — the index is
                // the KEY's canonical rendering, so the redirect is the only route in.
                Expect.isTrue
                    (ctx.TypeKeys.ContainsKey "Test.A.M+T")
                    "the identity index is keyed by the canonical `+`-nested rendering"

                Expect.isFalse
                    (ctx.TypeKeys.ContainsKey "Test.A.M.T")
                    "the written dotted spelling is not separately registered"

                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupType "Test.A.M.T" with
                | ValueSome _ -> ()
                | ValueNone -> failtest "the written dotted spelling must still resolve"

                // …and it lands on the REGISTERED identity: the same `InModule` key the
                // module containment mints, not a key re-cut from the dotted string.
                let expected: TypeKey =
                    {
                        Holder = TypeHolder.InModule(SymbolKeyOps.moduleInNamespace "Test.A" "M")
                        Name = "T"
                        TyparArity = 0
                    }

                Expect.equal
                    (provider.TryResolveTypeName "Test.A.M.T")
                    (ValueSome(SymbolKey.Type expected))
                    "the written name resolves to the registered InModule identity"

                // A spelling that names nothing still resolves to nothing — the redirect is a
                // containment lookup, not a name-shaped guess.
                Expect.equal (provider.TryResolveTypeName "Test.A.M.Nope") ValueNone "an unknown member of M misses"
            }
        ]
