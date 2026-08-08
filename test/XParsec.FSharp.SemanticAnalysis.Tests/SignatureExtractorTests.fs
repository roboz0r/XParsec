module XParsec.FSharp.SemanticAnalysis.Tests.SignatureExtractorTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// Lex + parse an in-memory `.fsi` snippet into the `ParsedFile` the extractor consumes.
/// No FIELD of the `LibFile` is read — it is only the tag on a `ctx.Diagnostics` /
/// `ctx.Skipped` entry — so `relative` just names the snippet in a lex/parse failure.
let parseFsi (relative: string) (input: string) : VesperLibManifest.ParsedFile =
    let lexed =
        match Lexing.lexString input with
        | Result.Error e -> failtestf "lex failed in %s: %A" relative e
        | Result.Ok lexed -> lexed

    let ast =
        let reader = Reader.ofLexed lexed Set.empty

        match FSharpAst.parseSignature reader with
        | Result.Error e -> failtestf "parse failed in %s: %A" relative e
        | Result.Ok ast -> ast

    {
        File =
            {
                Path =
                    {
                        BucketName = "App"
                        Relative = relative
                    }
                Absolute = relative
            }
        Lexed = lexed
        Ast = ast
    }

/// extract + finalize over one in-memory `.fsi`. A fixture that must seed the ctx first
/// (`AmbientShapes`, intrinsic reprs) or pin the PRE-finalize state spells the steps out
/// instead of coming through here.
let extractFsi (relative: string) (input: string) : VesperLib.ExtractCtx =
    let ctx = VesperLib.ExtractCtx.empty "clr"
    VesperLib.extractSymbols ctx (parseFsi relative input)
    VesperLib.finalizeDeferred ctx
    ctx

[<Tests>]
let tests =
    testList
        "SignatureExtractor"
        [
            test "cross-package nominal resolves through ambient shapes and bakes kind-correct" {
                // `AmbientShapes` is a dependency package's type shapes, keyed by qualified
                // compiled name. A `Union` ambient must bake `TyUnion` at extraction time,
                // whether the reference is fully qualified or reached via an `open`.
                let widgetShape = ExternalTypeShape.Union(1, [||], [||], SymbolOrigin.Empty)

                let ambient name =
                    if name = "Dep.Widget" then
                        ValueSome widgetShape
                    else
                        ValueNone

                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nopen Dep\n\nmodule M =\n    val qualified: Dep.Widget<int> -> int\n    val viaOpen: Widget<int> -> int\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
                ctx.AmbientShapes <- ambient
                VesperLib.extractSymbols ctx parsed
                // Vals are stashed during extraction; the finalize pass builds them into
                // `ctx.Symbols` once the registry is complete.
                VesperLib.finalizeDeferred ctx

                // By source-name suffix, so the assertion does not hinge on the module path.
                let instOf (suffix: string) : SemType =
                    let mutable found = ValueNone

                    for kv in ctx.Symbols do
                        if found.IsNone && kv.Key.EndsWith("." + suffix) then
                            found <- ValueSome(ExternalSymbols.instantiateSymbol (TypeStore()) kv.Value 0)

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
                    | other ->
                        failtestf "%s: expected (Dep.Widget<int> -> int) with a TyUnion result, got %A" label other

                assertWidgetIntToInt "fully-qualified reference" (instOf "qualified")
                assertWidgetIntToInt "reference via open" (instOf "viaOpen")
            }

            test "a signature naming an out-of-scope type bakes TyUnknown" {
                // Nothing declares `Missing.Thing`, so the val is RETAINED with a `TyUnknown`
                // leaf carrying the unresolved name — a use-site diagnostic — rather than
                // dropped into `ctx.Skipped`.
                let ctx =
                    extractFsi "app.fsi" "namespace App\n\nmodule M =\n    val broken: Missing.Thing -> int\n"

                let mutable found = ValueNone

                for kv in ctx.Symbols do
                    if found.IsNone && kv.Key.EndsWith(".broken") then
                        found <- ValueSome(ExternalSymbols.instantiateSymbol (TypeStore()) kv.Value 0)

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

            test "module-function ValRepr / CompiledForm captured from the .fsi arity" {
                // A bare curried type erases the source arity, so the symbol records the
                // `.fsi`'s `ValRepr`. The crux is `tupleGroup` vs `singleTuple`: both are
                // `int * int -> int`, but the first flattens to two params, the second stays one.
                let ctx =
                    extractFsi
                        "testc.fsi"
                        ("module TestC\n"
                         + "val curried: int -> int -> int\n"
                         + "val tupleGroup: int * int -> int\n"
                         + "val singleTuple: (int * int) -> int\n"
                         + "val loneUnit: unit -> int\n"
                         + "val voidRet: int -> unit\n")

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

                // Derived from the captured `ValRepr` on demand, not stored on the symbol.
                let compiledOf (suffix: string) : TastAccessor.CompiledForm =
                    match (symOf suffix).ValRepr with
                    | ValueSome vr -> TastLower.compiledOf (TastPoolBuilder.openEmpty ()) vr
                    | ValueNone -> failtestf "val '%s' carries no ValRepr" suffix

                // The flat compiled parameter TYPES the member-ref would encode.
                let compiledParamTys (suffix: string) : FrozenType list =
                    (compiledOf suffix).Params |> List.map (fun p -> p.Ty)

                let compiledReturn (suffix: string) : TastAccessor.CompiledReturn = (compiledOf suffix).Return

                // A terse rendering of the source group shape (the `ValRepr` arity).
                let groupTags (suffix: string) : string list =
                    match (symOf suffix).ValRepr with
                    | ValueSome vr ->
                        vr.Groups
                        |> List.map (fun g ->
                            match g with
                            | ArgGroupG.GUnit _ -> "unit"
                            | ArgGroupG.GSimple _ -> "simple"
                            | ArgGroupG.GTuple pat -> sprintf "tuple%d" (TastAccessor.patChildren pat).Length

                        )
                    | ValueNone -> failtestf "val '%s' carries no ValRepr" suffix

                Expect.equal (groupTags "curried") [ "simple"; "simple" ] "curried source arity"
                Expect.equal (compiledParamTys "curried") [ intF; intF ] "curried flat params"
                Expect.equal (compiledReturn "curried") (CompiledReturnG.RValue intF) "curried return"

                Expect.equal (groupTags "tupleGroup") [ "tuple2" ] "tupled-group source arity"
                Expect.equal (compiledParamTys "tupleGroup") [ intF; intF ] "tupled group flattens to 2 params"

                Expect.equal (groupTags "singleTuple") [ "simple" ] "single-tuple-param source arity"
                Expect.equal (compiledParamTys "singleTuple") [ pairF ] "single tuple param stays one ValueTuple param"

                Expect.equal (groupTags "loneUnit") [ "unit" ] "lone-unit source arity"
                Expect.equal (compiledParamTys "loneUnit") [] "lone unit param erased (parameterless)"
                Expect.equal (compiledReturn "loneUnit") (CompiledReturnG.RValue intF) "lone-unit return"

                Expect.equal (compiledParamTys "voidRet") [ intF ] "void fn keeps its real param"
                Expect.equal (compiledReturn "voidRet") CompiledReturnG.RVoid "unit return → RVoid"
            }

            test "An enum body extracts as an Enum shape carrying its case → value table" {
                // The `.fsi` extractor reads the enum-case value grammar through the same
                // projection the Elaborate pass uses, so a referenced package's `E.C1`
                // resolves to the same constant a locally-compiled `E.C1` does. `-1` lexes as
                // one negative literal, `- 3` as a unary minus — the two arms that reach it.
                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Colour =\n        | Red = -1\n        | Green = 2uy\n        | Amber = - 3\n\n    type Verb =\n        | Get = \"GET\"\n        | Put = \"PUT\"\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
                VesperLib.extractSymbols ctx parsed

                let casesOf (suffix: string) : ExternalEnumCaseShape[] =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith suffix then
                            found <- ValueSome kv.Value

                    match found with
                    | ValueSome(ExternalTypeShape.Enum(cases = cases)) -> cases
                    | ValueSome other -> failtestf "expected an Enum shape for '%s'; got %A" suffix other
                    | ValueNone ->
                        failtestf "'%s' registered no shape. Shapes: %A" suffix (Seq.toList ctx.TypeShapes.Keys)

                Expect.equal
                    [ for c in casesOf "Colour" -> c.Name, c.Value ]
                    [
                        "Red", ExternalEnumCaseValue.IntVal -1L
                        "Green", ExternalEnumCaseValue.IntVal 2L
                        "Amber", ExternalEnumCaseValue.IntVal -3L
                    ]
                    "numeric cases keep source order, and every width lands as int64"

                Expect.equal
                    [ for c in casesOf "Verb" -> c.Name, c.Value ]
                    [
                        "Get", ExternalEnumCaseValue.StringVal "GET"
                        "Put", ExternalEnumCaseValue.StringVal "PUT"
                    ]
                    "a string enum's case values are the decoded literals"
            }

            test "An enum case with no constant value downgrades the whole enum" {
                // A partial case table would answer `E.Red` and then deny `E.Green`, so one
                // unreadable case makes the whole body Unmodelled — the union extractor's
                // rule for an unnamed case. The reason names the case, not the literal form:
                // the declaring package's own compilation reported that.
                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Thing =\n        | Red = true\n        | Green = 1\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
                VesperLib.extractSymbols ctx parsed

                let thingShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "Thing" then
                            found <- ValueSome kv.Value

                    found

                match thingShape with
                | ValueSome(ExternalTypeShape.Unmodelled(UnmodelledReason.ExtractionFailed reason, arity)) ->
                    Expect.stringContains reason "Red" "the reason names the case that did not read"
                    Expect.equal arity 0 "Unmodelled carries the declared arity"
                | ValueSome other -> failtestf "expected an Unmodelled shape for the enum; got %A" other
                | ValueNone ->
                    failtestf
                        "enum registered no shape (name-without-shape gap). Shapes: %A"
                        (Seq.toList ctx.TypeShapes.Keys)
            }

            test "A val naming an enum bakes FTEnum, not an opaque nominal" {
                // The kind-correct bake for an `Enum` shape. Before the extractor built one,
                // `mkNominal`'s `Enum` arm was reachable only from a dependency's frozen
                // shapes, so a `.fsi`-declared enum in a val signature raised instead.
                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Colour =\n        | Red = 0\n        | Green = 1\n\n    val paint: Colour -> int\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let mutable found = ValueNone

                for kv in ctx.Symbols do
                    if found.IsNone && kv.Key.EndsWith ".paint" then
                        found <- ValueSome(ExternalSymbols.instantiateSymbol (TypeStore()) kv.Value 0)

                match found with
                | ValueSome(TyFun(TyEnum key, TyConst(intKey, _))) ->
                    Expect.equal key.Name "Colour" "the param is the enum's own nominal"
                    Expect.equal (SymbolKeyOps.simpleName intKey) (DisplayName "int") "the return type still bakes"
                | ValueSome other -> failtestf "expected (Colour -> int) with a TyEnum param, got %A" other
                | ValueNone -> failtestf "val 'paint' was not extracted. Symbols: %A" (Seq.toList ctx.Symbols.Keys)
            }

            test "A `struct … end` value type extracts as a Class shape flagged IsValueType" {
                // If value-type-ness does not surface through the provider, a consumer's
                // encoder emits `ELEMENT_TYPE_CLASS` for a referenced-package struct and the
                // loader faults "value type mismatch".
                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Point =\n        struct\n            val X: int\n            val Y: int\n        end\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
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

            test "A type's `interface <ty>` impls publish into FrozenInterfaces" {
                // A directly-declared `interface IBox<'T>` surfaces on the extracted shape's
                // `FrozenInterfaces`, args over the declaring typars, so a consumer can recover
                // a typar from it. Deferred — the interface may forward-reference a sibling.
                let ctx =
                    extractFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type IBox<'T> =\n        abstract member Get: unit -> 'T\n\n    [<Struct>]\n    type Container<'T> =\n        new: value: 'T -> Container<'T>\n        interface IBox<'T>\n"

                let containerShape =
                    let mutable found = ValueNone

                    for kv in ctx.TypeShapes do
                        if found.IsNone && kv.Key.EndsWith "Container`1" then
                            found <- ValueSome kv.Value

                    found

                match containerShape with
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
                | ValueNone -> failtestf "Container registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)
            }

            test "`extern with` publishes interfaces into FrozenInterfaces and members" {
                // A NON-intrinsic `extern … with` body registers exactly as a bodied class
                // would. The intrinsic-primitive form (`type string = extern with …`) takes
                // another branch.
                let ctx =
                    extractFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type IBar<'T> =\n        abstract member Get: unit -> 'T\n\n    type Foo<'T> = extern with\n        interface IBar<'T>\n        member M: unit -> 'T\n"

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

                // A non-interface class's members ride `ctx.TypeMembers`, not the shape.
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
                // Operator cases with explicit return types (`([])`, `(::)`) are GADT syntax:
                // the cases are named by their canonical ctor form (`Empty` / `Cons`), fields
                // come from the `(::)` args, and the declared return type is ignored.
                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Thing<'T> =\n        | ([]): Thing<'T>\n        | (::): Head: 'T * Tail: Thing<'T> -> Thing<'T>\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
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
                // `[<RequireQualifiedAccess>]` on an extracted union reads into `ctx.RqaTypes`,
                // so a consumer's bare reference to one of its cases can be rejected.
                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    [<RequireQualifiedAccess>]\n    type Color =\n        | Red\n        | Green\n\n    type Hue =\n        | Blue\n        | Cyan\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
                VesperLib.extractSymbols ctx parsed

                Expect.isTrue
                    (ctx.RqaTypes |> Seq.exists (fun n -> n.EndsWith "Color"))
                    "the RQA Color union is recorded in RqaTypes"

                Expect.isFalse
                    (ctx.RqaTypes |> Seq.exists (fun n -> n.EndsWith "Hue"))
                    "the ordinary Hue union is not recorded as RQA"

                // The flag rides the reverse case-name index as `IsRequireQualifiedAccess`.
                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupUnionCase "Red" with
                | ValueSome uc -> Expect.isTrue uc.IsRequireQualifiedAccess "Red's union (Color) is RQA"
                | ValueNone -> failtest "Red case not found in the reverse index"

                match provider.TryLookupUnionCase "Blue" with
                | ValueSome uc -> Expect.isFalse uc.IsRequireQualifiedAccess "Blue's union (Hue) is not RQA"
                | ValueNone -> failtest "Blue case not found in the reverse index"
            }

            test "A reference to an Unmodelled-shaped type is refused at bake time" {
                // A type whose in-scope shape is `Unmodelled` has no kind to bake, so translating
                // the val in the finalize pass raises `BodylessExternalShape`. The pass
                // tolerates it as a PER-VAL skip rather than aborting the whole build.
                let ambient name =
                    if name = "Dep.Widget" then
                        ValueSome(ExternalTypeShape.Unmodelled(UnmodelledReason.Delegate, 1))
                    else
                        ValueNone

                let parsed =
                    parseFsi
                        "app.fsi"
                        "namespace App\n\nopen Dep\n\nmodule M =\n    val qualified: Dep.Widget<int> -> int\n"

                let ctx = VesperLib.ExtractCtx.empty "clr"
                ctx.AmbientShapes <- ambient

                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let qualifiedRegistered =
                    ctx.Symbols.Keys |> Seq.exists (fun k -> k.EndsWith ".qualified")

                Expect.isFalse
                    qualifiedRegistered
                    "a val whose result type has no modelled body is not registered as a symbol"

                let skipped = ctx.Skipped |> Seq.exists (fun (_, msg) -> msg.Contains "qualified")

                Expect.isTrue skipped "the dropped val is recorded in ctx.Skipped"
            }

            test "objnull abbrev (`obj | null`) extracts to the union `FTOr [obj; null]`" {
                // `type objnull = obj | null` is not a primitive but the ordinary union, so its
                // abbrev body freezes to `FTOr [obj; null]` — matching the front end's
                // `Type.Null` mint, so an extracted `objnull` unifies with a written `T | null`.
                let ctx =
                    extractFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type objnull = obj | null\n    val f: objnull -> int\n"

                let unfreezable = FTUnknown "<unfreezable external template>"

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
                // With NO `(# … #)` repr, `type disposable = extern interface with abstract
                // member Dispose …` extracts to a `Class{IsInterface=true}` carrying `Dispose`.
                // Interface-ness is the EXPLICIT tag, not inferred from an all-abstract body.
                let ctx =
                    extractFsi
                        "capabilities.fsi"
                        "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n"

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

            // A primitive has no type in the output to hang a method on, so a concrete member
            // on one can only be spliced — and the `.fsi` contract must say so.
            test "a concrete member on an intrinsic must be declared inline" {
                let ctx = VesperLib.ExtractCtx.empty "clr"
                // Intrinsic-ness is the target-blind marker, not the compiling target's repr.
                ctx.IntrinsicMarkers.Add "widget" |> ignore
                ctx.IntrinsicReprs.["widget"] <- "System.Widget"

                VesperLib.extractSymbols
                    ctx
                    (parseFsi
                        "prim-types-widget.fsi"
                        "namespace Vesper\n\ntype widget = extern with\n    member M : unit -> unit\n")

                let diagnosed =
                    ctx.Diagnostics
                    |> Seq.exists (fun (_, msg) -> msg.Contains "must be declared 'inline'")

                Expect.isTrue
                    diagnosed
                    (sprintf "the member-inline diagnostic fired; got %A" (List.ofSeq ctx.Diagnostics))
            }

            // A capability's slots declare no body, so there is nothing to splice: the rule
            // is about members WITH a body.
            test "an extern interface's abstract members do not want inline" {
                let ctx = VesperLib.ExtractCtx.empty "clr"
                ctx.IntrinsicMarkers.Add "disposable" |> ignore
                ctx.IntrinsicReprs.["disposable"] <- "System.IDisposable"

                VesperLib.extractSymbols
                    ctx
                    (parseFsi
                        "capabilities.fsi"
                        "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n")

                let diagnosed =
                    ctx.Diagnostics
                    |> Seq.exists (fun (_, msg) -> msg.Contains "must be declared 'inline'")

                Expect.isFalse diagnosed "an all-abstract capability surface is exempt"
            }

            // `override`/`default` have no `inline` slot in the signature grammar, so on a
            // host that publishes no method table neither the slot nor the remedy exists.
            test "an override on an intrinsic is rejected outright, not asked for inline" {
                let ctx = VesperLib.ExtractCtx.empty "clr"
                ctx.IntrinsicMarkers.Add "widget" |> ignore
                ctx.IntrinsicReprs.["widget"] <- "System.Widget"

                VesperLib.extractSymbols
                    ctx
                    (parseFsi
                        "prim-types-widget.fsi"
                        "namespace Vesper\n\ntype widget = extern with\n    override M : unit -> unit\n")

                let messages = [ for (_, msg) in ctx.Diagnostics -> msg ]

                Expect.isTrue
                    (messages
                     |> List.exists (fun m -> m.Contains "cannot declare an 'override' or 'default' member"))
                    (sprintf "the override diagnostic fired; got %A" messages)

                Expect.isFalse
                    (messages |> List.exists (fun m -> m.Contains "must be declared 'inline'"))
                    "an override is not asked to be inline — the grammar gives it no inline slot"
            }

            // `new: unit -> obj` NAMES a target-provided constructor, so there is no body
            // to splice.
            test "a heritable primitive's constructor signature is exempt" {
                let ctx = VesperLib.ExtractCtx.empty "clr"
                ctx.IntrinsicMarkers.Add "obj" |> ignore
                ctx.IntrinsicReprs.["obj"] <- "System.Object"

                VesperLib.extractSymbols
                    ctx
                    (parseFsi
                        "prim-types-object.fsi"
                        "namespace Vesper\n\ntype obj = extern class with\n    new: unit -> obj\n")

                let diagnosed =
                    ctx.Diagnostics
                    |> Seq.exists (fun (_, msg) ->
                        msg.Contains "must be declared 'inline'" || msg.Contains "cannot declare"
                    )

                Expect.isFalse diagnosed (sprintf "a `new:` sig is exempt; got %A" (List.ofSeq ctx.Diagnostics))
            }

            test
                "two-name capability interface: extern interface with abstract member + (# … #) repr → IntrinsicInterface carrying the platform name, NOT a reverse-canon entry" {
                // A `.fsi` interface member surface plus a `.fs` `(# "System.IDisposable" #)`
                // repr extract to ONE `IntrinsicInterface` carrying the members and
                // `{ Canon; Platform }`, so the type reconciles to its BCL spelling.
                let ctx = VesperLib.ExtractCtx.empty "clr"
                ctx.IntrinsicMarkers.Add "disposable" |> ignore
                ctx.IntrinsicReprs.["disposable"] <- "System.IDisposable"

                let parsed =
                    parseFsi
                        "capabilities.fsi"
                        "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n"

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
                        "IntrinsicInterface platform name is the `.fs` repr"
                | other -> failtestf "expected an IntrinsicInterface shape for disposable; got %A" other

                let provider = VesperLib.ExtractCtx.toProvider ctx

                // The member surface survived alongside the platform name.
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "Dispose") with
                | ValueSome _ -> ()
                | ValueNone -> failtest "Dispose member surface was dropped from the IntrinsicInterface"

                // The reverse map's reader turns a hit into an `FTConst` leaf, so an interface
                // entry would mis-present `System.IDisposable` as a scalar canon.
                // Reconciliation rides the `IntrinsicInterface` identity above instead.
                match provider.IntrinsicReverseCanon.TryFind "System.IDisposable" with
                | None -> ()
                | Some canons -> failtestf "capability interface must NOT enter the reverse-canon map; found %A" canons
            }

            test "CONCRETE member surface on an intrinsic primitive keeps the Intrinsic shape" {
                // The shape stays `Intrinsic`: a primitive declaring an operator surface (`int`
                // with `static member (+)`) must keep the `TyConst` identity that intrinsic
                // recognisers, repr lookup and literal inference key on. Members ride a table.
                let ctx = VesperLib.ExtractCtx.empty "clr"
                // `isIntrinsic` is decided by the BASE repr marker (the primitive's `.fs`).
                ctx.IntrinsicMarkers.Add "widget" |> ignore
                ctx.IntrinsicReprs.["widget"] <- "System.Widget"

                // A CONCRETE instance member (`member M`), NOT `abstract member`.
                let parsed =
                    parseFsi
                        "prim-types-widget.fsi"
                        "namespace Vesper\n\ntype widget = extern with\n    member inline M : unit -> unit\n"

                VesperLib.extractSymbols ctx parsed
                VesperLib.finalizeDeferred ctx

                let key =
                    match ctx.TypeShapes.Keys |> Seq.tryFind (fun k -> k.EndsWith "widget") with
                    | Some k -> k
                    | None -> failtestf "widget registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)

                match ctx.TypeShapes.[key] with
                | ExternalTypeShape.Intrinsic shape ->
                    Expect.equal
                        shape.Id.Platform
                        (IntrinsicPlatform.Repr "System.Widget")
                        "the intrinsic keeps its platform repr"

                    Expect.equal shape.Class ValueNone "an untagged member surface is not a heritable class"
                | other -> failtestf "expected the Intrinsic shape to survive for widget; got %A" other

                let rejected =
                    ctx.Diagnostics
                    |> Seq.exists (fun (_, msg) -> msg.Contains "concrete member surface on an intrinsic primitive")

                Expect.isFalse rejected "the lifted guardrail must NOT emit a rejection diagnostic"

                // The member is published from the type-keyed member table, even though the
                // `Intrinsic` shape carries no member slots.
                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "M") with
                | ValueSome _ -> ()
                | ValueNone -> failtest "concrete member surface `M` was dropped when the Intrinsic shape was restored"
            }

            // `T` is declared inside `module M` in `namespace Test.A`, so a consumer's local
            // containment mints an `InModule` key — and the contract's store must answer THAT
            // key, not a separately-spelled string.
            test "a module-held contract type answers the KEY a module containment mints" {
                let ctx =
                    extractFsi "a.fsi" "namespace Test.A\n\nmodule M =\n    type T = { X: int }\n"

                // Built from the containment chain, never from a name.
                let key: TypeKey =
                    {
                        Container = TypeContainer.InModule(SymbolKeyOps.moduleInNamespace "Test.A" "M")
                        Name = "T"
                        TyparArity = 0
                    }

                Expect.equal
                    (SymbolKeyOps.typeMetaName key)
                    "Test.A.M+T"
                    "the module's container class encloses the type, as the CLR spells a nested type"

                let provider = VesperLib.ExtractCtx.toProvider ctx

                match provider.TryLookupType(SymbolKey.Type key) with
                | ValueSome(ExternalTypeShape.Record _) -> ()
                | ValueSome other -> failtestf "the key answered, but with the wrong shape: %A" other
                | ValueNone -> failtest "the contract store must be addressable by the module-held type's key"
            }

            // …and the SOURCE still writes `Test.A.M.T`, which is not the metadata name
            // `Test.A.M+T`, so it reaches the identity by a redirect over the declared module
            // containment — re-cutting a key would absorb `M` into the namespace path.
            test "a module-held contract type still resolves by the name the source WRITES" {
                let ctx =
                    extractFsi "a.fsi" "namespace Test.A\n\nmodule M =\n    type T = { X: int }\n"

                Expect.isTrue
                    (ctx.TypeKeys.ContainsKey "Test.A.M+T")
                    "the identity index is keyed by the canonical `+`-nested rendering"

                Expect.isFalse
                    (ctx.TypeKeys.ContainsKey "Test.A.M.T")
                    "the written dotted spelling is not separately registered"

                let provider = VesperLib.ExtractCtx.toProvider ctx

                let expected: TypeKey =
                    {
                        Container = TypeContainer.InModule(SymbolKeyOps.moduleInNamespace "Test.A" "M")
                        Name = "T"
                        TyparArity = 0
                    }

                match provider.TryLookupType "Test.A.M.T" with
                | ValueSome(struct (key, ExternalTypeShape.Record _)) ->
                    Expect.equal key expected "the written name resolves to the registered InModule identity"
                | ValueSome(struct (_, other)) -> failtestf "the name resolved, but with the wrong shape: %A" other
                | ValueNone -> failtest "the written dotted spelling must still resolve"

                // The redirect is a containment lookup, not a name-shaped guess.
                Expect.isTrue
                    (provider.TryLookupType "Test.A.M.Nope" |> ValueOption.isNone)
                    "an unknown member of M misses"
            }

            test "`when 'T : equality` is captured, and applied to the fresh TyVar at instantiation" {
                // `when 'T : equality` must land on BOTH the symbol's structured `Constraints`
                // and — via `instantiateSymbol` — the fresh `TypeVar` minted for that typar
                // slot, which is the only half a use site's inference consults.
                let ctx =
                    extractFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    val contains: value: 'T -> source: 'T -> bool when 'T: equality\n"

                let sym =
                    let mutable found = ValueNone

                    for kv in ctx.Symbols do
                        if found.IsNone && kv.Key.EndsWith ".contains" then
                            found <- ValueSome kv.Value

                    match found with
                    | ValueSome s -> s
                    | ValueNone ->
                        failtestf "val 'contains' was not extracted. Symbols: %A" (Seq.toList ctx.Symbols.Keys)

                Expect.equal sym.TyparArity 1 "the val generalises over its single typar"

                // The clause is recorded against the typar's INDEX, not its name.
                let equalityTrait =
                    sym.Constraints
                    |> List.exists (fun c ->
                        match c with
                        | ExternalConstraint.Trait(0, SemanticConstraintKind.Equality) -> true
                        | _ -> false
                    )

                Expect.isTrue equalityTrait (sprintf "Constraints carries Equality on typar 0; got %A" sym.Constraints)

                let store = TypeStore()

                match ExternalSymbols.instantiateSymbol store sym 0 with
                | TyFun(TyVar a, TyFun(TyVar _, TyConst(k, _))) when SymbolKeyOps.simpleName k = DisplayName "bool" ->
                    Expect.isTrue
                        (store.Constraints.Items(UnionFind.find store a)
                         |> List.exists (fun c -> c.Kind = SemanticConstraintKind.Equality))
                        "the fresh TyVar minted for 'T carries Equality in the store's constraint table"
                | other -> failtestf "expected ('T -> 'T -> bool) over a fresh TyVar; got %A" other
            }

            test "SRTP member-trait clause is captured as a MemberTrait over the val's typars" {
                // `when ^T : (static member (+) : ^T * ^T -> ^T)` captures as a `MemberTrait`:
                // the COMPILED name (`op_Addition`) plus `FTTypar(Declaring, 0)` templates, not
                // the source spelling. Instantiation realises them and stamps `SrtpBounds`.
                let ctx =
                    extractFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    val inline add: x: ^T -> y: ^T -> ^T when ^T: (static member (+): ^T * ^T -> ^T)\n"

                let sym =
                    let mutable found = ValueNone

                    for kv in ctx.Symbols do
                        if found.IsNone && kv.Key.EndsWith ".add" then
                            found <- ValueSome kv.Value

                    match found with
                    | ValueSome s -> s
                    | ValueNone -> failtestf "val 'add' was not extracted. Symbols: %A" (Seq.toList ctx.Symbols.Keys)

                let trait_ =
                    sym.Constraints
                    |> List.tryPick (fun c ->
                        match c with
                        | ExternalConstraint.MemberTrait(idxs, name, args, ret) -> Some(idxs, name, args, ret)
                        | _ -> None
                    )

                match trait_ with
                | None -> failtestf "no MemberTrait captured for `add`; Constraints: %A" sym.Constraints
                | Some(idxs, name, args, ret) ->
                    Expect.equal (EqArray.toList idxs) [ 0 ] "the trait is borne by the val's single typar slot"
                    Expect.equal name "op_Addition" "`(+)` is captured by its COMPILED name"

                    Expect.equal
                        (List.ofArray args)
                        [ FTTypar(TyparAxis.Declaring, 0); FTTypar(TyparAxis.Declaring, 0) ]
                        "the tupled trait args flatten to two templates over the declaring typar"

                    Expect.equal ret (FTTypar(TyparAxis.Declaring, 0)) "the trait returns the declaring typar"

                // The realised signature lands in the store's `Srtp` table under the fresh
                // TyVar's representative id.
                let store = TypeStore()

                match ExternalSymbols.instantiateSymbol store sym 0 with
                | TyFun(TyVar a, TyFun(TyVar _, TyVar _)) ->
                    match store.Srtp.Live(UnionFind.find store a) with
                    | [ bound ] ->
                        Expect.equal bound.MemberName "op_Addition" "the stamped bound names the compiled member"
                        Expect.equal bound.ArgTypes.Length 2 "the stamped bound keeps both args"
                        Expect.isFalse (store.Srtp.IsSolved bound) "a freshly stamped bound is undischarged"
                    | other -> failtestf "expected exactly one SrtpBound on the fresh TyVar; got %A" other
                | other -> failtestf "expected (^T -> ^T -> ^T) over a fresh TyVar; got %A" other
            }
        ]
