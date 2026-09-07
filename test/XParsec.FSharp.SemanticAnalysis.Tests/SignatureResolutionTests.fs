module XParsec.FSharp.SemanticAnalysis.Tests.SignatureResolutionTests

open System.Collections.Generic
open Vesper
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// What one `.fsi` publishes, and everything it reported doing so.
type Resolved =
    {
        Surface: PublishedSurface
        Diagnostics: Diagnostic list
    }

    member this.Provider = PublishedSurface.toProvider this.Surface
    member this.Messages = [ for d in this.Diagnostics -> d.Message ]

let private bindingTable (bindings: (string * string) list) : IReadOnlyDictionary<string, PlatformTypeId> =
    let d = Dictionary<string, PlatformTypeId>(System.StringComparer.Ordinal)

    for (name, typeId) in bindings do
        d.[name] <- PlatformTypeId typeId

    d :> IReadOnlyDictionary<_, _>

/// Resolve one in-memory `.fsi` against `dependencies`, with `bindings` standing in for the
/// `(# … #)` bindings a paired `.fs` would supply. `relative` only identifies the snippet in a
/// diagnostic.
let resolveFsiWith
    (dependencies: IExternalSymbolProvider)
    (bindings: (string * string) list)
    (relative: string)
    (input: string)
    : Resolved =
    let parsed =
        match ParseChain.parseSignature Set.empty input with
        | Result.Error f -> failtestf "parse failed in %s: %A" relative [ for d in f.Diagnostics -> d.Message ]
        | Result.Ok p -> p

    let source =
        LexedFile.inAssembly (AssemblyName "App") (AssemblyFileId.ofRelative relative) parsed.Lexed

    let surface, diagnostics =
        SignatureResolution.resolveFile
            dependencies
            source
            {
                Assembly = AssemblyName "App"
                Target = "none"
                Bindings = bindingTable bindings
            }
            parsed.Tree

    {
        Surface = surface
        Diagnostics = diagnostics
    }

/// `resolveFsiWith` against the real contract stack, so a snippet may write `int` / `obj`.
let resolveFsi (relative: string) (input: string) : Resolved =
    resolveFsiWith realProvider.Value [] relative input

/// Resolve `files` in order, each against the ones BEFORE it, as a package's manifest order
/// does. The LAST file's result is what the fixtures assert on.
let resolveFsiFiles (files: (string * string) list) : Resolved =
    let mutable visible = [ realProvider.Value ]
    let mutable last = ValueNone

    for (relative, input) in files do
        let resolved =
            resolveFsiWith (ExternalSymbolProviders.composite visible) [] relative input

        visible <- PublishedSurface.toProvider resolved.Surface :: visible
        last <- ValueSome resolved

    match last with
    | ValueSome r -> r
    | ValueNone -> failtest "no files to resolve"

/// The shape published for the type whose compiled name ends `suffix` (`Box\`1`).
let shapeOf (r: Resolved) (suffix: string) : ExternalTypeShape =
    let named =
        [
            for e: SurfaceEntry<TypeKey, ExternalTypeShape> in r.Surface.ShapesByKey ->
                SymbolKeyOps.typeMetaName e.Key, e.Value
        ]

    match named |> List.tryFind (fun (name, _) -> name.EndsWith suffix) with
    | Some(_, shape) -> shape
    | None -> failtestf "type '%s' published no shape. Shapes: %A" suffix (List.map fst named)

/// The members published for the type whose compiled name ends `suffix`, in declaration order.
let membersOf (r: Resolved) (suffix: string) : ExternalMember list =
    let named =
        [
            for e: SurfaceEntry<TypeKey, Block<ExternalMember>> in r.Surface.MembersByKey ->
                SymbolKeyOps.typeMetaName e.Key, Block.toList e.Value
        ]

    match named |> List.tryFind (fun (name, _) -> name.EndsWith suffix) with
    | Some(_, members) -> members
    | None -> failtestf "no members published for '%s'. Member tables: %A" suffix (List.map fst named)

/// The symbol published for the val bound as `name`.
let symbolOf (r: Resolved) (name: string) : ExternalSymbol =
    let entries =
        [
            for e: SurfaceEntry<BindingKey, ExternalSymbol> in r.Surface.Symbols -> e.Key, e.Value
        ]

    match entries |> List.tryFind (fun (key, _) -> key.Name = name) with
    | Some(_, sym) -> sym
    | None -> failtestf "val '%s' was not published. Symbols: %A" name (List.map fst entries)

/// The `interface <ty>` impls a PRIMITIVE declares. Empty for a source that binds only a
/// representation, which is indistinguishable here from a primitive that declares none.
let declaredInterfaces (p: IExternalSymbolStore) (key: TypeKey) : Block<FrozenNominal> =
    match p.TryLookupType key with
    | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) -> surface.Interfaces
    | _ -> Block.empty

let private compiledNameText (CompiledName n) : string = n

/// Every module the `.fsi` publishes keyed against the class name it emits as. The key half is
/// asserted too: the channel IS the association, and a compiled name landing on the wrong
/// module is what the key flip risks.
let private compiledModuleNames (r: Resolved) : (string * string) list =
    [
        for e in r.Surface.Modules do
            match e.Value.Facts.CompiledName with
            | ValueSome compiled -> e.Key.DeclaredPath, compiledNameText compiled
            | ValueNone -> ()
    ]

/// Every value the `.fsi` publishes keyed against the method name it emits as.
let private compiledValueNames (r: Resolved) : (string * string) list =
    [
        for e in r.Surface.Symbols do
            match e.Value.CompiledName with
            | ValueSome cn -> SymbolKeyOps.qualifiedName (SymbolKey.Binding e.Key), compiledNameText cn
            | ValueNone -> ()
    ]

[<Tests>]
let tests =
    testList
        "SignatureResolution"
        [
            // The compiled name as a published fact of its own, independent of the key: what
            // the backends read once the keys hold semantic names.
            testList
                "publishes a compiled name"
                [
                    test "`module Foo` beside `type Foo` publishes `FooModule`" {
                        let r =
                            resolveFsi
                                "app.fsi"
                                "namespace App\n\ntype Foo =\n    | A\n\nmodule Foo =\n    val seed: int\n"

                        Expect.equal (compiledModuleNames r) [ "App.Foo", "FooModule" ] "the suffix reaches the surface"
                    }

                    test "`[<CompiledName>]` on a val publishes the attribute's name" {
                        let r =
                            resolveFsi
                                "app.fsi"
                                "namespace App\n\nmodule Bag =\n    [<CompiledName(\"Empty\")>]\n    val empty: int\n"

                        Expect.equal
                            (compiledValueNames r)
                            [ "App.Bag.empty", "Empty" ]
                            "the attribute reaches the surface"
                    }

                    test "CONTROL: a module and a val under their source names publish none" {
                        let r = resolveFsi "app.fsi" "namespace App\n\nmodule Bag =\n    val empty: int\n"

                        Expect.isEmpty (compiledModuleNames r) "`Bag` compiles under its source name"
                        Expect.isEmpty (compiledValueNames r) "`empty` compiles under its source name"
                    }

                    // GAP: an active-pattern `val` publishes nothing. `OperatorNames.ofDeclaredName`
                    // models no source form for one, so `registerValSig` mints no key and a
                    // consumer of the assembly cannot resolve the pattern at all. The
                    // implementation half agrees (`MemberNames.ofBinding` files no
                    // `ModuleBindingInfo`), so conformance stays silent about it too.
                    ptest "a `val` declaring an active pattern publishes it" {
                        let r =
                            resolveFsi
                                "app.fsi"
                                "namespace App\n\nmodule M =\n    val (|Even|Odd|): int -> Choice<unit, unit>\n"

                        Expect.isNonEmpty
                            [ for e in r.Surface.Symbols -> e.Key ]
                            "the active pattern reaches the published surface"
                    }
                ]

            test "cross-package nominal resolves through a dependency provider and bakes kind-correct" {
                // A dependency's `Union` shape must bake `TyUnion` at resolution time, whether
                // the reference is fully qualified or reached via an `open`.
                let dep =
                    providerOfTypes
                        [
                            SymbolKeyOps.typeKeyOfArity "Dep" "Widget" 1,
                            ExternalTypeShape.Union
                                {
                                    Typars = TyparList.positional 1<typeSlot>
                                    Cases = Block.empty
                                    Interfaces = Block.empty
                                    Origin = SymbolOrigin.Empty
                                    IsValueType = false
                                    RequiresQualifiedAccess = false
                                }
                        ]

                let r =
                    resolveFsiWith
                        (ExternalSymbolProviders.composite [ dep; realProvider.Value ])
                        []
                        "app.fsi"
                        "namespace App\n\nopen Dep\n\nmodule M =\n    val qualified: Dep.Widget<int> -> int\n    val viaOpen: Widget<int> -> int\n"

                let instOf (suffix: string) : SemType =
                    ExternalSymbols.instantiateSymbol (MeasuredThaw.noneOver (TypeStore())) (symbolOf r suffix) 0

                let assertWidgetIntToInt (label: string) (ty: SemType) =
                    match ty with
                    | TyFun(TyUnion("Dep.Widget`1", args), TyConst(k, _)) when
                        args.Length = 1 && SymbolKeyOps.typeSimpleName k = DisplayName "int"
                        ->
                        match args.[0] with
                        | TyConst(k, _) when SymbolKeyOps.typeSimpleName k = DisplayName "int" -> ()
                        | other -> failtestf "%s: expected Dep.Widget<int>, got arg %A" label other
                    | other ->
                        failtestf "%s: expected (Dep.Widget<int> -> int) with a TyUnion result, got %A" label other

                assertWidgetIntToInt "fully-qualified reference" (instOf "qualified")
                assertWidgetIntToInt "reference via open" (instOf "viaOpen")
            }

            test "a signature referencing an out-of-scope type is reported, and the val still publishes" {
                // Nothing declares `Missing.Thing`. The val is RETAINED carrying the unresolved
                // name — a use-site diagnostic — and the file says so where it was written.
                let r =
                    resolveFsi "app.fsi" "namespace App\n\nmodule M =\n    val broken: Missing.Thing -> int\n"

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "Thing"))
                    (sprintf "the unresolved name is reported; got %A" r.Messages)

                match
                    ExternalSymbols.instantiateSymbol (MeasuredThaw.noneOver (TypeStore())) (symbolOf r "broken") 0
                with
                | TyFun(TyUnknown(UnknownReason.UndefinedName name), TyConst(k, _)) when
                    SymbolKeyOps.typeSimpleName k = DisplayName "int"
                    ->
                    Expect.stringContains name "Thing" "TyUnknown carries the unresolved name"
                | other -> failtestf "expected (<unresolved> -> int); got %A" other
            }

            // A record field is the probe throughout: its frozen type is `FTRecord` when the
            // name resolved and `FTUnknown` when it did not.
            let fieldTypeOf (r: Resolved) (typeName: string) : FrozenType =
                // A module-nested type's compiled key joins with `+`, not `.`.
                match shapeOf r ("+" + typeName) with
                | ExternalTypeShape.Record { Fields = fields } when fields.Length = 1 -> fields.[0].Frozen
                | other -> failtestf "expected a one-field Record for '%s'; got %A" typeName other

            test "a type referencing one declared LATER in the file does not resolve" {
                // Declarations come into scope where they are written, as F# resolves them.
                let r =
                    resolveFsi
                        "app.fsi"
                        ("namespace App\n\nmodule M =\n"
                         + "    type Ahead = { P: Behind }\n"
                         + "    type Behind = { X: int }\n"
                         + "    type Trailing = { P: Behind }\n")

                match fieldTypeOf r "Ahead" with
                | FTUnknown(UnknownReason.UndefinedName name) ->
                    Expect.stringContains name "Behind" "the forward name is carried unresolved"
                | other -> failtestf "a forward reference must not resolve; got %A" other

                match fieldTypeOf r "Trailing" with
                | FTRecord _ -> ()
                | other -> failtestf "a BACKWARD reference to the same type must resolve; got %A" other
            }

            test "an `and`-joined type group resolves mutually" {
                // The group is registered whole before it is published, so the two may reference
                // each other — the one place a declaration sees a name written below it.
                let r =
                    resolveFsi
                        "app.fsi"
                        ("namespace App\n\nmodule M =\n"
                         + "    type Node = { Edge: Link }\n"
                         + "    and Link = { Target: int }\n")

                match fieldTypeOf r "Node" with
                | FTRecord _ -> ()
                | other -> failtestf "`Node.Edge` must resolve to the `Link` declared below it; got %A" other
            }

            test "a signature referencing a type declared in a LATER file does not resolve" {
                // Files fold top-down, so the same rule holds across them: `b.fsi`'s type is
                // not in scope while `a.fsi` is being resolved.
                let r =
                    resolveFsiFiles
                        [
                            "a.fsi", "namespace App\n\nmodule A =\n    val needsB: App.B.Thing -> int\n"
                            "b.fsi", "namespace App\n\nmodule B =\n    type Thing = { X: int }\n"
                        ]

                // `b.fsi` publishes the type; `a.fsi`'s val is what could not see it.
                match shapeOf r "+Thing" with
                | ExternalTypeShape.Record _ -> ()
                | other -> failtestf "the LATER file publishes its own type; got %A" other
            }

            test "module-function ValRepr / CompiledForm captured from the .fsi arity" {
                // A bare curried type erases the source arity, so the symbol records the
                // `.fsi`'s `ValRepr`. The crux is `tupleGroup` vs `singleTuple`: both are
                // `int * int -> int`, but the first flattens to two params, the second stays one.
                let r =
                    resolveFsi
                        "testc.fsi"
                        ("namespace App\n\nmodule TestC =\n"
                         + "    val curried: int -> int -> int\n"
                         + "    val tupleGroup: int * int -> int\n"
                         + "    val singleTuple: (int * int) -> int\n"
                         + "    val loneUnit: unit -> int\n"
                         + "    val voidRet: int -> unit\n")

                let intF = FTConst(RuntimeNames.intKey, Block.empty)
                let pairF = FTTuple(Block.ofList [ intF; intF ])

                // Derived from the captured `ValRepr` on demand, not stored on the symbol.
                let compiledOf (suffix: string) : TastAccessor.CompiledForm =
                    match (symbolOf r suffix).ValRepr with
                    | ValueSome vr -> TastLower.compiledOf (TastPoolBuilder.openEmpty ()) vr
                    | ValueNone -> failtestf "val '%s' carries no ValRepr" suffix

                // The flat compiled parameter TYPES the member-ref would encode.
                let compiledParamTys (suffix: string) : FrozenType list =
                    (compiledOf suffix).Params |> List.map (fun p -> p.Ty)

                let compiledReturn (suffix: string) : TastAccessor.CompiledReturn = (compiledOf suffix).Return

                // A terse rendering of the source group shape (the `ValRepr` arity).
                let groupTags (suffix: string) : string list =
                    match (symbolOf r suffix).ValRepr with
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

            test "An enum body publishes an Enum shape carrying its case → value table" {
                // The enum-case value grammar is read through the same projection the Elaborate
                // pass uses, so a referenced package's `E.C1` resolves to the same constant a
                // locally-compiled `E.C1` does. `-1` lexes as one negative literal, `- 3` as a
                // unary minus — the two arms that reach it.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Colour =\n        | Red = -1\n        | Green = 2uy\n        | Amber = - 3\n\n    type Verb =\n        | Get = \"GET\"\n        | Put = \"PUT\"\n"

                let casesOf (suffix: string) : Block<ExternalEnumCaseShape> =
                    match shapeOf r suffix with
                    | ExternalTypeShape.Enum { Cases = cases } -> cases
                    | other -> failtestf "expected an Enum shape for '%s'; got %A" suffix other

                Expect.equal
                    [ for c in casesOf "Colour" -> c.Name, c.Value ]
                    [
                        "Red", ExternalEnumCaseValue.IntVal(IntValue.Int32(-1))
                        "Green", ExternalEnumCaseValue.IntVal(IntValue.Byte 2uy)
                        "Amber", ExternalEnumCaseValue.IntVal(IntValue.Int32(-3))
                    ]
                    "numeric cases keep source order and each case's own kind"

                Expect.equal
                    [ for c in casesOf "Verb" -> c.Name, c.Value ]
                    [
                        "Get", ExternalEnumCaseValue.StringVal "GET"
                        "Put", ExternalEnumCaseValue.StringVal "PUT"
                    ]
                    "a string enum's case values are the decoded literals"
            }

            test "An enum case with no constant value downgrades the whole enum" {
                // A partial case table would resolve `E.Red` and then deny `E.Green`, so one
                // unreadable case makes the whole body Unmodelled. The reason cites the case,
                // not the literal form: the declaring package's own compilation reported that.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Thing =\n        | Red = true\n        | Green = 1\n"

                match shapeOf r "Thing" with
                | ExternalTypeShape.Unmodelled(UnmodelledReason.ExtractionFailed reason, typars) ->
                    Expect.stringContains reason "Red" "the reason names the case that did not read"
                    Expect.equal typars.Length 0 "Unmodelled carries the declared arity"
                | other -> failtestf "expected an Unmodelled shape for the enum; got %A" other
            }

            test "A `delegate` signature declaration is refused, and still publishes its gap" {
                let r =
                    resolveFsi "app.fsi" "namespace App\n\nmodule M =\n    type Handler = delegate of int -> int\n"

                Expect.exists
                    r.Messages
                    (fun m -> m.Contains "not yet supported: `delegate` type declarations")
                    "the declaration itself reports NotYetSupported"

                // The gap still publishes, so a use site can report which form is missing.
                match shapeOf r "Handler" with
                | ExternalTypeShape.Unmodelled(UnmodelledReason.Delegate, _) -> ()
                | other -> failtestf "expected Unmodelled(Delegate) for the delegate; got %A" other
            }

            test "The published shape states which family the declaration commits to" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Opaque\n\n    type Bodied =\n        member P: int\n\n    type Iface =\n        abstract M: int\n\n    type Rec = { X: int }\n\n    type Uni =\n        | A\n\n    type Alias = int\n"

                let familyOf (name: string) = (shapeOf r name).DeclaredFamily

                Expect.equal (familyOf "Opaque") ValueNone "an opaque `type T` commits to no family"

                match shapeOf r "Opaque" with
                | ExternalTypeShape.Class shape ->
                    Expect.isFalse shape.IsInterface "and still resolves as a non-interface class"
                | other -> failtestf "expected a Class shape for the opaque type; got %A" other

                Expect.equal
                    (familyOf "Bodied")
                    (ValueSome Conformance.TypeKindFamily.Class)
                    "a bodied class commits to Class"

                Expect.equal
                    (familyOf "Iface")
                    (ValueSome Conformance.TypeKindFamily.Interface)
                    "an all-abstract body commits to Interface"

                Expect.equal (familyOf "Rec") (ValueSome Conformance.TypeKindFamily.Record) "a record commits to Record"
                Expect.equal (familyOf "Uni") (ValueSome Conformance.TypeKindFamily.Union) "a union commits to Union"
                Expect.equal (familyOf "Alias") ValueNone "an abbreviation is transparent"
            }

            test "A val referencing an enum bakes FTEnum, not an opaque nominal" {
                // The kind-correct bake for an `Enum` shape: `mkNominal`'s `Enum` arm must be
                // reachable from a `.fsi`-declared enum, not only from a dependency's shapes.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Colour =\n        | Red = 0\n        | Green = 1\n\n    val paint: Colour -> int\n"

                match
                    ExternalSymbols.instantiateSymbol (MeasuredThaw.noneOver (TypeStore())) (symbolOf r "paint") 0
                with
                | TyFun(TyEnum key, TyConst(intKey, _)) ->
                    Expect.equal key.Name "Colour" "the param is the enum's own nominal"
                    Expect.equal (SymbolKeyOps.typeSimpleName intKey) (DisplayName "int") "the return type still bakes"
                | other -> failtestf "expected (Colour -> int) with a TyEnum param, got %A" other
            }

            test "A `struct … end` value type publishes as a Class shape flagged IsValueType" {
                // If value-type-ness does not surface through the provider, a consumer's
                // encoder emits `ELEMENT_TYPE_CLASS` for a referenced-package struct and the
                // loader faults "value type mismatch".
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Point =\n        struct\n            val X: int\n            val Y: int\n        end\n"

                match shapeOf r "Point" with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue shape.Flags.IsValueType "the struct's Class shape is flagged IsValueType"
                    Expect.isFalse shape.IsInterface "a struct is not an interface"
                | other -> failtestf "expected a Class shape for the struct; got %A" other
            }

            test "A type's `interface <ty>` impls publish into FrozenInterfaces" {
                // A directly-declared `interface IBox<'T>` surfaces on the published shape's
                // `FrozenInterfaces`, args over the declaring typars, so a consumer can recover
                // a typar from it.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type IBox<'T> =\n        abstract member Get: unit -> 'T\n\n    [<Struct>]\n    type Container<'T> =\n        new: value: 'T -> Container<'T>\n        interface IBox<'T>\n"

                match shapeOf r "Container`1" with
                | ExternalTypeShape.Class shape ->
                    match shape.FrozenInterfaces with
                    | BlockOne iface ->
                        Expect.equal iface.Key.Name "IBox" "the IBox interface is published"

                        // `IBox` is declared inside `module M`, so it publishes a module-held
                        // identity. Its rendering `App.M+IBox`1` re-cuts to a CLR-NESTED key,
                        // which is a different type and resolves to nothing.
                        match iface.Key.Container with
                        | TypeContainer.InModule m -> Expect.equal m.Name "M" "IBox is held by module M"
                        | other -> failtestf "expected a module-held interface identity; got %A" other

                        Expect.equal iface.Args.Length 1 "IBox<'T> carries one type arg"

                        match iface.Args.[0] with
                        | FTTypar(TyparScope.Type _, 0) -> ()
                        | other -> failtestf "the interface arg is the declaring typar 'T; got %A" other
                    | other -> failtestf "expected exactly one published interface (IBox); got %A" other
                | other -> failtestf "expected a Class shape for the struct; got %A" other
            }

            test "`extern with` publishes its declared interfaces onto the shape, and its members" {
                // An untagged `extern … with interface …` resolves as a bodied class would,
                // then publishes as an `Intrinsic` carrying that class surface: the declared
                // interface is carried on the SHAPE, the members on the member table.
                let r =
                    resolveFsiWith
                        realProvider.Value
                        [ "Foo", "App.Foo`1" ]
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type IBar<'T> =\n        abstract member Get: unit -> 'T\n\n    type Foo<'T> = extern with\n        interface IBar<'T>\n        member inline M: unit -> 'T\n"

                match shapeOf r "Foo`1" with
                | ExternalTypeShape.Intrinsic { Class = ValueSome surface } ->
                    match surface.Interfaces with
                    | BlockOne iface ->
                        Expect.equal iface.Key.Name "IBar" "the IBar interface is published"
                        Expect.equal iface.Args.Length 1 "IBar<'T> carries one type arg"

                        match iface.Args.[0] with
                        | FTTypar(TyparScope.Type _, 0) -> ()
                        | other -> failtestf "the interface arg is the declaring typar 'T; got %A" other
                    | other -> failtestf "expected exactly one published interface (IBar); got %A" other
                | other -> failtestf "expected an Intrinsic shape carrying a class surface; got %A" other

                Expect.isTrue
                    (membersOf r "Foo`1" |> List.exists (fun m -> m.Name = "M"))
                    (sprintf "member M is published; got %A" [ for m in membersOf r "Foo`1" -> m.Name ])
            }

            test "A GADT-cased union publishes a genuine Union shape" {
                // Operator cases with explicit return types (`([])`, `(::)`) are GADT syntax:
                // the cases are named by their canonical ctor form (`Empty` / `Cons`), fields
                // come from the `(::)` args, and the declared return type is ignored.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Thing<'T> =\n        | ([]): Thing<'T>\n        | (::): Head: 'T * Tail: Thing<'T> -> Thing<'T>\n"

                match shapeOf r "Thing`1" with
                | ExternalTypeShape.Union { Typars = typars; Cases = cases } ->
                    Expect.equal typars.Length 1 "Union carries the declared arity"
                    Expect.equal cases.Length 2 "two cases published"
                    Expect.equal cases.[0].Name "Empty" "`([])` names the nullary case by its canonical ctor form"
                    Expect.equal cases.[0].FrozenFieldTypes.Length 0 "the nullary case has no fields"
                    Expect.equal cases.[1].Name "Cons" "`(::)` names the cons case by its canonical ctor form"
                    Expect.equal cases.[1].FrozenFieldTypes.Length 2 "cons has Head + Tail fields"

                    Expect.equal
                        cases.[1].FieldNames
                        (Block.ofSeq [ ValueSome "Head"; ValueSome "Tail" ])
                        "cons field names"
                | other -> failtestf "expected a Union shape for the GADT-cased union; got %A" other
            }

            test "[<RequireQualifiedAccess>] is carried on the published case index" {
                // The flag is what lets a consumer's bare `Red` be rejected while a bare `Blue`
                // resolves; it reaches them through the declaring module's scope.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    [<RequireQualifiedAccess>]\n    type Color =\n        | Red\n        | Green\n\n    type Hue =\n        | Blue\n        | Cyan\n"

                let scope = r.Provider.Scope

                let m =
                    match scope.TryContainer "App.M" with
                    | ValueSome c -> c
                    | ValueNone -> failtest "App.M is a published module"

                match scope.UnionCasesNamed(m, "Red") with
                | BlockOne uc -> Expect.isTrue uc.IsRequireQualifiedAccess "Red's union (Color) is RQA"
                | other -> failtestf "Red case not declared in App.M: %A" other

                match scope.UnionCasesNamed(m, "Blue") with
                | BlockOne uc -> Expect.isFalse uc.IsRequireQualifiedAccess "Blue's union (Hue) is not RQA"
                | other -> failtestf "Blue case not declared in App.M: %A" other
            }

            test "objnull abbrev (`obj | null`) resolves to the union `FTOr [obj; null]`" {
                // `type objnull = obj | null` is not a primitive but the ordinary union, so its
                // abbrev body freezes to `FTOr [obj; null]` — matching the front end's
                // `Type.Null` mint, so a published `objnull` unifies with a written `T | null`.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type objnull = obj | null\n    val f: objnull -> int\n"

                match shapeOf r "objnull" with
                | ExternalTypeShape.Abbrev { Typars = typars; Body = frozen } ->
                    Expect.equal typars.Length 0 "objnull is nullary"

                    match frozen with
                    | FTUnknown(UnknownReason.UnfreezableExternal what) ->
                        failtestf "objnull froze to the unfreezable sentinel (%s)" what
                    | _ -> ()

                    match frozen with
                    | FTOr disjuncts ->
                        let names =
                            EqSet.toList disjuncts.Disjuncts
                            |> List.map (fun ft ->
                                match ft with
                                | FTConst(k, _) ->
                                    let (DisplayName name) = SymbolKeyOps.typeSimpleName k
                                    name
                                | other -> failtestf "expected FTConst members in objnull union; got %A" other
                            )
                            |> List.sort

                        Expect.equal names [ "null"; "obj" ] "objnull is the union of `obj` and `null`"
                    | other -> failtestf "expected objnull to freeze to FTOr [obj; null]; got %A" other
                | other -> failtestf "expected an Abbrev shape for objnull; got %A" other
            }

            test "extern interface with abstract member publishes an interface Class carrying its member surface" {
                // With NO `(# … #)` repr, `type disposable = extern interface with abstract
                // member Dispose …` publishes a `Class{IsInterface=true}` carrying `Dispose`.
                // Interface-ness is the EXPLICIT tag, not inferred from an all-abstract body.
                let r =
                    resolveFsi
                        "capabilities.fsi"
                        "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n"

                match shapeOf r "disposable" with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue
                        shape.IsInterface
                        "extern interface with abstract member publishes as an INTERFACE Class"
                | other -> failtestf "expected a Class shape for disposable; got %A" other

                Expect.isTrue
                    (membersOf r "disposable" |> List.exists (fun m -> m.Name = "Dispose"))
                    "the Dispose member surface survives"
            }

            // A primitive has no type in the output to hang a method on, so a concrete member on
            // one can only be spliced, which the signature file must say.
            test "a concrete member on an intrinsic must be declared inline" {
                let r =
                    resolveFsiWith
                        realProvider.Value
                        [ "widget", "System.Widget" ]
                        "prim-types-widget.fsi"
                        "namespace Vesper\n\ntype widget = extern with\n    member M : unit -> unit\n"

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "must be declared 'inline'"))
                    (sprintf "the member-inline diagnostic fired; got %A" r.Messages)
            }

            // A capability's slots declare no body, so there is nothing to splice: the rule
            // is about members WITH a body.
            test "an extern interface's abstract members do not want inline" {
                let r =
                    resolveFsiWith
                        realProvider.Value
                        [ "disposable", "System.IDisposable" ]
                        "capabilities.fsi"
                        "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n"

                Expect.isFalse
                    (r.Messages |> List.exists (fun m -> m.Contains "must be declared 'inline'"))
                    "an all-abstract capability surface is exempt"
            }

            // `override`/`default` have no `inline` slot in the signature grammar, so on a
            // host that publishes no method table neither the slot nor the remedy exists.
            test "an override on an intrinsic is rejected outright, not asked for inline" {
                let r =
                    resolveFsiWith
                        realProvider.Value
                        [ "widget", "System.Widget" ]
                        "prim-types-widget.fsi"
                        "namespace Vesper\n\ntype widget = extern with\n    override M : unit -> unit\n"

                Expect.isTrue
                    (r.Messages
                     |> List.exists (fun m -> m.Contains "cannot declare an 'override' or 'default' member"))
                    (sprintf "the override diagnostic fired; got %A" r.Messages)

                Expect.isFalse
                    (r.Messages |> List.exists (fun m -> m.Contains "must be declared 'inline'"))
                    "an override is not asked to be inline — the grammar gives it no inline slot"
            }

            // `new: unit -> obj` NAMES a target-provided constructor, so there is no body
            // to splice.
            test "a heritable primitive's constructor signature is exempt" {
                let r =
                    resolveFsiWith
                        realProvider.Value
                        [ "obj", "System.Object" ]
                        "prim-types-object.fsi"
                        "namespace Vesper\n\ntype obj = extern class with\n    new: unit -> obj\n"

                Expect.isFalse
                    (r.Messages
                     |> List.exists (fun m -> m.Contains "must be declared 'inline'" || m.Contains "cannot declare"))
                    (sprintf "a `new:` sig is exempt; got %A" r.Messages)
            }

            test
                "two-name capability interface: extern interface with abstract member + (# … #) repr → IntrinsicInterface carrying the platform name, NOT an intrinsic-axis entry" {
                // A `.fsi` interface member surface plus a `.fs` `(# "System.IDisposable" #)`
                // repr publish ONE `IntrinsicInterface` carrying the members and
                // `{ Canon; Platform }`, so the type reconciles to its BCL spelling.
                let r =
                    resolveFsiWith
                        realProvider.Value
                        [ "disposable", "System.IDisposable" ]
                        "capabilities.fsi"
                        "namespace Vesper\n\ntype disposable = extern interface with\n    abstract member Dispose : unit -> unit\n"

                match shapeOf r "disposable" with
                | ExternalTypeShape.IntrinsicInterface iface ->
                    Expect.equal
                        iface.Canon
                        (RuntimeNames.primitiveKey "disposable")
                        "IntrinsicInterface canon is the contract-sourced qualified identity (`namespace Vesper`)"

                    Expect.equal
                        iface.Platform
                        (PlatformTypeId "System.IDisposable")
                        "IntrinsicInterface platform name is the `.fs` repr"
                | other -> failtestf "expected an IntrinsicInterface shape for disposable; got %A" other

                // The member surface survived alongside the platform name.
                Expect.isTrue
                    (membersOf r "disposable" |> List.exists (fun m -> m.Name = "Dispose"))
                    "the Dispose member surface survives on the IntrinsicInterface"

                // A `canonsOf` reader turns a hit into an `FTConst`, so an interface entry
                // would mis-present `System.IDisposable` as a scalar canon. Reconciliation
                // uses the `IntrinsicInterface` identity above instead.
                match IntrinsicTypeMap.canonsOf (PlatformTypeId "System.IDisposable") r.Provider.IntrinsicTypeMap with
                | BlockEmpty -> ()
                | canons -> failtestf "capability interface must NOT enter the intrinsic axis; found %A" canons
            }

            test "CONCRETE member surface on an intrinsic primitive keeps the Intrinsic shape" {
                // The shape stays `Intrinsic`: a primitive declaring an operator surface (`int`
                // with `static member (+)`) must keep the `TyConst` identity that intrinsic
                // recognisers, repr lookup and literal inference key on. Members are published
                // from a separate table.
                let r =
                    resolveFsiWith
                        realProvider.Value
                        [ "widget", "System.Widget" ]
                        "prim-types-widget.fsi"
                        // A CONCRETE instance member (`member M`), NOT `abstract member`.
                        "namespace Vesper\n\ntype widget = extern with\n    member inline M : unit -> unit\n"

                match shapeOf r "widget" with
                | ExternalTypeShape.Intrinsic shape ->
                    Expect.equal
                        shape.Id.Platform
                        (IntrinsicPlatform.Bound(PlatformTypeId "System.Widget"))
                        "the intrinsic keeps its platform type id"

                    match shape.Class with
                    | ValueSome surface ->
                        Expect.isFalse surface.Heritable "an untagged member surface is not a heritable class"
                        Expect.isTrue surface.Members.IsEmpty "its members are held in the member table, not the shape"
                    | ValueNone -> failtest "an untagged `extern with` carries the interfaces it declared"
                | other -> failtestf "expected the Intrinsic shape to survive for widget; got %A" other

                // The member is published from the type-keyed member table, even though the
                // `Intrinsic` shape carries no member slots.
                Expect.isTrue
                    (membersOf r "widget" |> List.exists (fun m -> m.Name = "M"))
                    "concrete member surface `M` was dropped when the Intrinsic shape was restored"
            }

            // `T` is declared inside `module M` in `namespace Test.A`, so a consumer's local
            // containment mints an `InModule` key — and the contract's store must resolve THAT
            // key, not a separately-spelled string.
            test "a module-held contract type is addressable by the KEY a module containment mints" {
                let r =
                    resolveFsi "a.fsi" "namespace Test.A\n\nmodule M =\n    type T = { X: int }\n"

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

                match r.Provider.TryLookupType key with
                | ValueSome(ExternalTypeShape.Record _) -> ()
                | ValueSome other -> failtestf "the key resolved, but to the wrong shape: %A" other
                | ValueNone -> failtest "the contract store must be addressable by the module-held type's key"
            }

            // …and the SOURCE still writes `Test.A.M.T`, which is not the metadata name
            // `Test.A.M+T`, so it reaches the identity through the declaring module on the
            // scope — re-cutting a key would absorb `M` into the namespace path.
            test "a module-held contract type resolves through its declaring module" {
                let r =
                    resolveFsi "a.fsi" "namespace Test.A\n\nmodule M =\n    type T = { X: int }\n"

                let expected: TypeKey =
                    {
                        Container = TypeContainer.InModule(SymbolKeyOps.moduleInNamespace "Test.A" "M")
                        Name = "T"
                        TyparArity = 0
                    }

                let scope = r.Provider.Scope

                let m =
                    match scope.TryContainer "Test.A.M" with
                    | ValueSome c -> c
                    | ValueNone -> failtest "Test.A.M is a published module"

                match scope.TypesNamed(m, "T") with
                | BlockOne(struct (key, ExternalTypeShape.Record _)) ->
                    Expect.equal key expected "the written name resolves to the registered InModule identity"
                | other -> failtestf "the module publishes exactly one T, a Record: %A" other

                // A containment lookup, not a name-shaped guess.
                Expect.isEmpty (scope.TypesNamed(m, "Nope")) "an unknown member of M misses"

                Expect.isTrue
                    (r.Provider.TryLookupType expected |> ValueOption.isSome)
                    "the store resolves under the registered InModule identity"

                Expect.isTrue
                    (ExternalSymbols.tryMetaType r.Provider "Test.A.M.T" |> ValueOption.isNone)
                    "the dotted spelling read as a rendering misses; only the scope reaches it"
            }

            test "`when 'T : equality` is captured, and applied to the fresh TyVar at instantiation" {
                // `when 'T : equality` must land on BOTH the symbol's typar in `Generics`
                // and, via `instantiateSymbol`, the fresh `TypeVar` minted for that typar slot,
                // which is the half a use site's inference reads.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    val contains: value: 'T -> source: 'T -> bool when 'T: equality\n"

                let sym = symbolOf r "contains"

                Expect.equal sym.TyparArity 1<typeSlot> "the val generalises over its single typar"

                // The clause is recorded on the typar at its INDEX, not by its name.
                let typars = sym.Generics.Typars

                Expect.isTrue
                    (EqSet.contains TyparConstraintKindG.Equality typars.Types.[0<typeSlot>].Constraints.Kinds)
                    (sprintf "typar 0 carries Equality; got %A" typars)

                let store = TypeStore()

                match ExternalSymbols.instantiateSymbol (MeasuredThaw.noneOver store) sym 0 with
                | TyFun(TyVar a, TyFun(TyVar _, TyConst(k, _))) when SymbolKeyOps.typeSimpleName k = DisplayName "bool" ->
                    Expect.isTrue
                        (store.Constraints.Items(UnionFind.find store a)
                         |> List.exists (fun c -> c.Kind = SemanticConstraintKind.Equality))
                        "the fresh TyVar minted for 'T carries Equality in the store's constraint table"
                | other -> failtestf "expected ('T -> 'T -> bool) over a fresh TyVar; got %A" other
            }

            test "SRTP member-trait clause is captured as a MemberTrait over the val's typars" {
                // `when ^T : (static member (+) : ^T * ^T -> ^T)` captures as a `MemberTrait`:
                // the COMPILED name (`op_Addition`) plus `FTTypar(ModuleFunction add, 0)` templates, not
                // the source spelling. Instantiation substitutes them and stamps them on the `Srtp` table.
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    val inline add: x: ^T -> y: ^T -> ^T when ^T: (static member (+): ^T * ^T -> ^T)\n"

                let sym = symbolOf r "add"

                match Block.toList sym.Generics.Traits with
                | [] -> failtestf "no MemberTrait captured for `add`; Generics: %A" sym.Generics
                | trait_ :: _ ->
                    Expect.equal
                        (Block.toList trait_.TyparIndices)
                        [ 0<typeSlot> ]
                        "the trait is borne by the val's single typar slot"

                    Expect.equal trait_.MemberName "op_Addition" "`(+)` is captured by its COMPILED name"

                    let ownTypar = FTTypar(TyparScope.ModuleFunction sym.Key, 0)

                    Expect.equal
                        (Block.toList trait_.ArgTypes)
                        [ ownTypar; ownTypar ]
                        "the tupled trait args flatten to two templates over the val's own typar"

                    Expect.equal trait_.ReturnType ownTypar "the trait returns the val's own typar"

                // The instantiated trait lands in the store's `Srtp` table under the fresh
                // TyVar's representative id.
                let store = TypeStore()

                match ExternalSymbols.instantiateSymbol (MeasuredThaw.noneOver store) sym 0 with
                | TyFun(TyVar a, TyFun(TyVar _, TyVar _)) ->
                    match store.Srtp.Live(UnionFind.find store a) with
                    | [ memberTrait ] ->
                        Expect.equal memberTrait.MemberName "op_Addition" "the stamped trait names the compiled member"
                        Expect.equal memberTrait.ArgTypes.Length 2 "the stamped trait keeps both args"
                        Expect.isFalse (store.Srtp.IsSolved memberTrait) "a freshly stamped trait is undischarged"
                    | other -> failtestf "expected exactly one SRTP trait on the fresh TyVar; got %A" other
                | other -> failtestf "expected (^T -> ^T -> ^T) over a fresh TyVar; got %A" other
            }

            test "a `when` clause after a measure typar lands on the type slot, not the signature slot" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    val eqAfter<[<Measure>] 'u, 'a when 'a: equality> : 'a -> 'a\n"

                Expect.isEmpty r.Messages "a measure typar beside a constrained type typar resolves cleanly"

                let typars = (symbolOf r "eqAfter").Generics.Typars

                Expect.equal typars.Length 2 "both parameters are declared"
                Expect.equal typars.TypeArity 1<typeSlot> "only `'a` is type-kinded"
                Expect.equal typars.MeasureArity 1<measureSlot> "`'u` is measure-kinded"

                Expect.isTrue
                    (EqSet.contains TyparConstraintKindG.Equality typars.Types.[0<typeSlot>].Constraints.Kinds)
                    (sprintf "`'a` carries Equality at type slot 0; got %A" typars)
            }

            test "an SRTP support set after a measure typar indexes the type slot" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    val inline add<[<Measure>] 'u, ^T when ^T: (static member (+): ^T * ^T -> ^T)> : ^T -> ^T -> ^T\n"

                Expect.isEmpty r.Messages "a measure typar beside an SRTP type typar resolves cleanly"

                match Block.toList (symbolOf r "add").Generics.Traits with
                | [ trait_ ] ->
                    Expect.equal
                        (Block.toList trait_.TyparIndices)
                        [ 0<typeSlot> ]
                        "`^T` is type slot 0, though it is written second"
                | other -> failtestf "expected exactly one MemberTrait; got %A" other
            }

            test "a measure typar in an SRTP support set is FS0703" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    val inline zero<[<Measure>] 'u when 'u: (static member Zero: int)> : int\n"

                expectErrorIn r.Messages "Expected type parameter, not unit-of-measure parameter"
            }

            // --- property signatures (`member P: T with get, set`) ------------------

            test "an indexed property signature publishes as a get_ accessor method" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Box<'T> =\n        member Item: index: int -> 'T with get\n"

                let intF = FTConst(RuntimeNames.intKey, Block.empty)

                match membersOf r "Box`1" with
                | [ m ] ->
                    Expect.equal m.Name "get_Item" "an index makes the getter a method"
                    Expect.equal m.Storage MemberStorage.Method "storage is a method, not a property"
                    Expect.equal m.Key.Kind MemberKind.Method "and so is the key's kind"
                    Expect.equal m.Key.ArgSig (Block.singleton intF) "the index is the member's one argument"

                    Expect.equal
                        (ExternalSignature.tupledParameters m.Signature)
                        intF
                        "the index survives into the signature"

                    Expect.equal
                        m.Signature.Return
                        (FTTypar(TyparScope.Type m.Key.Decl, 0))
                        "the getter returns the element"
                | other -> failtestf "expected one member; got %A" [ for m in other -> m.Name ]
            }

            test "a parameterless property signature keeps its own name and property storage" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Box =\n        member Count: int with get\n"

                match membersOf r "Box" with
                | [ m ] ->
                    Expect.equal m.Name "Count" "a parameterless getter is the property itself"
                    Expect.equal m.Storage MemberStorage.Property "storage is a property"
                    Expect.isEmpty (Block.toList m.Key.ArgSig) "a property takes no argument"
                    Expect.equal m.Signature.Return (FTConst(RuntimeNames.intKey, Block.empty)) "the declared value"
                | other -> failtestf "expected one member; got %A" [ for m in other -> m.Name ]
            }

            test "the `set` half of a property signature publishes as a set_ accessor method" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Box =\n        member Count: int with get, set\n"

                let intF = FTConst(RuntimeNames.intKey, Block.empty)

                match membersOf r "Box" with
                | [ getter; setter ] ->
                    Expect.equal getter.Name "Count" "the getter half"
                    Expect.equal setter.Name "set_Count" "the setter half"
                    Expect.equal setter.Storage MemberStorage.Method "a setter is an accessor method"
                    Expect.equal setter.Key.ArgSig (Block.singleton intF) "it accepts the property's value"

                    Expect.equal
                        (ExternalSignature.tupledParameters setter.Signature)
                        intF
                        "which is the getter's result"

                    Expect.equal setter.Signature.Return ExternalSignature.unitFrozen "and it returns unit"
                | other -> failtestf "expected both halves; got %A" [ for m in other -> m.Name ]
            }

            test "an indexed setter takes the index and then the value" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Box<'T> =\n        member Item: index: int -> 'T with get, set\n"

                let intF = FTConst(RuntimeNames.intKey, Block.empty)

                match membersOf r "Box`1" with
                | [ getter; setter ] ->
                    let elemF = FTTypar(TyparScope.Type getter.Key.Decl, 0)

                    Expect.equal getter.Name "get_Item" "the getter half"
                    Expect.equal setter.Name "set_Item" "the setter half"
                    Expect.equal setter.Key.ArgSig (Block.ofList [ intF; elemF ]) "index then value"
                    Expect.equal setter.Signature.Return ExternalSignature.unitFrozen "a setter returns unit"
                | other -> failtestf "expected both halves; got %A" [ for m in other -> m.Name ]
            }

            test "a write-only property signature publishes only the setter" {
                let r =
                    resolveFsi
                        "app.fsi"
                        "namespace App\n\nmodule M =\n    type Box =\n        member Count: int with set\n"

                match membersOf r "Box" with
                | [ m ] ->
                    Expect.equal m.Name "set_Count" "no getter is declared, so none is published"
                    Expect.equal m.Signature.Return ExternalSignature.unitFrozen "a setter returns unit"
                | other -> failtestf "expected one member; got %A" [ for m in other -> m.Name ]
            }

            // A curried declaration compiles to the SAME one two-parameter slot a tupled one
            // does, so the key cannot tell them apart — the published groups are what makes
            // F# demand `x.Add 1 2` where the tupled declaration demands `x.Add(1, 2)`.
            test "a CURRIED member signature publishes its argument groups and instantiates curried" {
                let curried =
                    resolveFsi
                        "curried.fsi"
                        "namespace App\n\nmodule M =\n    type Box =\n        member Add: a: int -> b: int -> int\n"

                let tupled =
                    resolveFsi
                        "tupled.fsi"
                        "namespace App\n\nmodule M =\n    type Box =\n        member Add: a: int * b: int -> int\n"

                let intF = FTConst(RuntimeNames.intKey, Block.empty)
                let intTy = TyConst(RuntimeNames.intKey, Block.empty)

                match membersOf curried "Box", membersOf tupled "Box" with
                | [ c ], [ t ] ->
                    Expect.equal (Block.toList c.Signature.ArgGroups) [ intF; intF ] "two source argument groups"
                    Expect.equal (Block.toList t.Signature.ArgGroups) [ FTTuple(Block.ofList [ intF; intF ]) ] "one"

                    Expect.equal c.Key t.Key "both compile to the same key: one two-parameter slot"

                    Expect.equal
                        (ExternalSignature.tupledParameters c.Signature)
                        (ExternalSignature.tupledParameters t.Signature)
                        "and to the same .NET parameter slot"

                    Expect.equal
                        (ExternalSymbols.instantiateSignature (MeasuredThaw.noneOver (TypeStore())) c [||] 0)
                        (TyFun(intTy, TyFun(intTy, intTy)))
                        "the curried one is applied a group at a time"

                    Expect.equal
                        (ExternalSymbols.instantiateSignature (MeasuredThaw.noneOver (TypeStore())) t [||] 0)
                        (TyFun(TyTuple(Block.ofList [ intTy; intTy ]), intTy))
                        "the tupled one takes one tuple"
                | c, t ->
                    failtestf
                        "expected one member each; got %A and %A"
                        [ for m in c -> m.Name ]
                        [ for m in t -> m.Name ]
            }

            test "a member signature with arguments and no `with` clause keeps its own name" {
                // The `with get` clause is what makes an accessor: a plain method signature
                // that happens to take arguments must not gain a `get_` prefix.
                let r =
                    resolveFsi "app.fsi" "namespace App\n\nmodule M =\n    type Box =\n        member Get: int -> int\n"

                match membersOf r "Box" with
                | [ m ] ->
                    Expect.equal m.Name "Get" "an ordinary method signature"
                    Expect.equal m.Storage MemberStorage.Method "kept as a method"
                | other -> failtestf "expected one member; got %A" [ for m in other -> m.Name ]
            }

            // --- the real contracts, resolved through the package fold -------------------

            test "the cons-list's declared indexer publishes under the name a use site resolves" {
                // `list.fsi` declares `member Item: index: int -> 'T with get`, and
                // `x.[i]` resolves an indexer by the `get_Item` name only.
                let listKey = RuntimeNames.vesperListKey

                match realProvider.Value.TryLookupMembers(listKey, "get_Item") with
                | BlockOne m ->
                    Expect.equal m.Storage MemberStorage.Method "the declared indexer is an accessor method"

                    Expect.equal
                        m.Key.ArgSig
                        (Block.singleton (FTConst(RuntimeNames.intKey, Block.empty)))
                        "keyed on its index argument"
                | other ->
                    failtestf "expected exactly one `get_Item` on the cons-list; got %A" [ for m in other -> m.Name ]
            }

            // Over the REAL Vesper.Core contract, not a snippet: the publishing route is what
            // is under test. An unresolvable name degrades to `unfreezable` and a non-nominal
            // freeze is DROPPED from the interface list, so a declaration written before
            // `seq<'T>` — or a republish that stops carrying `Interfaces` — leaves the array a
            // bare `Scalar` with NO error. Asserting on the published shape is what catches
            // that; a use site would only report the eventual mismatch.
            test "the array publishes an intrinsic surface carrying `seq<'T>` over its element" {
                let key = RuntimeNames.arrayKey 1

                let surface =
                    match realProvider.Value.TryLookupType key with
                    | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) -> surface
                    | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueNone }) ->
                        failtest
                            "`'T[]` published as a SCALAR intrinsic: its `interface seq<'T>` was dropped between resolution and republish"
                    | other -> failtestf "expected an Intrinsic shape for `'T[]`; got %A" other

                let elem = TyConst(RuntimeNames.intKey, Block.empty)

                let instantiated =
                    ExternalSymbols.instantiateInterfacesOf
                        (MeasuredThaw.noneOver (TypeStore()))
                        surface.Interfaces
                        [| elem |]

                let carriesSeqOfInt =
                    instantiated
                    |> Array.exists (fun ifaceTy ->
                        // `TyClass` here is the TestHelpers pattern: it projects the key back
                        // to its metadata name.
                        match ifaceTy with
                        | TyClass(name, args) ->
                            args.Length = 1 && args.[0] = elem && SymbolKeyOps.shortName name = "seq"
                        | _ -> false
                    )

                Expect.isTrue
                    carriesSeqOfInt
                    (sprintf "`int[]`'s declared interfaces do not include `seq<int>`; they are %A" instantiated)
            }

            test "the contract declares equatable and comparable on `int`" {
                let declared = declaredInterfaces realProvider.Value RuntimeNames.intKey

                for anchor in [ RuntimeNames.equatableKey; RuntimeNames.comparableKey ] do
                    Expect.isTrue
                        (declared |> Block.exists (fun iface -> iface.Key = anchor))
                        (sprintf
                            "`int` does not declare `%s`; it declares %A"
                            (SymbolKeyOps.typeMetaName anchor)
                            declared)
            }

            // A frozen impl view of `prim-types-min.clr.fs` sits AHEAD of the contract in a
            // package's own build and publishes `int` as a bare `Scalar`: it binds a
            // representation and knows no surface. First-hit alone would read that silence as
            // "declares nothing", so the composite folds the surfaces instead.
            test "a repr-only source ahead of the contract does not shadow `int`'s declared interfaces" {
                let intKey = RuntimeNames.intKey

                let reprOnly =
                    let shapes = Dictionary<TypeKey, ExternalTypeShape>()

                    let canon = SymbolKeyOps.qualifiedTypeKeyOf (SymbolKeyOps.typeMetaName intKey) 0

                    shapes.[intKey] <-
                        ExternalTypeShape.Intrinsic(
                            IntrinsicShape.Scalar(
                                canon,
                                TyparList.empty,
                                IntrinsicPlatform.Bound(PlatformTypeId "System.Int32")
                            )
                        )

                    ExternalSymbolProviders.ofKeyIndexedChannels
                        { ExternalSymbolProviders.KeyIndexedChannels.empty with
                            ShapesByKey = shapes
                        }

                Expect.isTrue
                    (declaredInterfaces reprOnly intKey).IsEmpty
                    "the repr-only source alone declares nothing, which is what makes the fold necessary"

                let composed = ExternalSymbolProviders.composite [ reprOnly; realProvider.Value ]

                Expect.isTrue
                    (declaredInterfaces composed intKey
                     |> Block.exists (fun iface -> iface.Key = RuntimeNames.equatableKey))
                    "the contract's `equatable<int>` survives a source that binds only a repr"
            }

            test "a bodied signature's `inherit` of an undefined name diagnoses and publishes no base" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type C ="
                                "        inherit Unknown"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "Unknown"))
                    (sprintf "the undefined base is diagnosed (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape -> Expect.isTrue shape.FrozenBaseType.IsNone "no base type is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "a bodied signature's `inherit` of an interface diagnoses and publishes no base" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type I ="
                                "        abstract M: unit -> int"
                                ""
                                "    type C ="
                                "        inherit I"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages
                     |> List.exists (fun m -> m.Contains "Cannot inherit from interface 'I'"))
                    (sprintf "the interface base is diagnosed (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape -> Expect.isTrue shape.FrozenBaseType.IsNone "no base type is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "a bodied signature's `inherit` of a non-nominal type diagnoses and publishes no base" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type C ="
                                "        inherit (int * int)"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "only classes are inheritable"))
                    (sprintf "the non-nominal base is diagnosed (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape -> Expect.isTrue shape.FrozenBaseType.IsNone "no base type is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "an all-abstract bodied signature with an `inherit` is an interface, its clause interface inheritance" {
                // fsc kinds `type I2 = inherit I1  abstract M: …` an interface whatever the
                // clause resolves to, and its `inherit` is carried on the interface list.
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type I1 ="
                                "        abstract A: int"
                                ""
                                "    type I2 ="
                                "        inherit I1"
                                "        abstract M: unit -> int"
                                ""
                            ])

                Expect.isEmpty r.Messages "interface inheritance raises nothing"

                match shapeOf r "I2" with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue shape.IsInterface "I2 is an interface in F#"
                    Expect.isTrue shape.FrozenBaseType.IsNone "an interface has no base type"

                    Expect.isTrue
                        (shape.FrozenInterfaces |> Block.exists (fun i -> i.Key.Name = "I1"))
                        "the inherited I1 is published on the interface list"
                | other -> failtestf "expected a Class shape for I2; got %A" other
            }

            test "an all-abstract bodied signature inheriting a class diagnoses `not an interface` (FS0887 parity)" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type B ="
                                "        new: unit -> B"
                                "        member Q: int"
                                ""
                                "    type T ="
                                "        inherit B"
                                "        abstract M: unit -> int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "'B' is not an interface"))
                    (sprintf "the class-typed `inherit` on an interface is diagnosed (%A)" r.Messages)

                match shapeOf r "T" with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue shape.IsInterface "T is still an interface: kind inference is syntactic"
                    Expect.isTrue shape.FrozenInterfaces.IsEmpty "the rejected clause publishes nothing"
                | other -> failtestf "expected a Class shape for T; got %A" other
            }

            test "a bodied signature's `inherit` of a non-heritable primitive diagnoses and publishes no base" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type C ="
                                "        inherit int"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "only classes are inheritable"))
                    (sprintf "the primitive base is diagnosed (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape -> Expect.isTrue shape.FrozenBaseType.IsNone "no base type is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "a bodied signature's `inherit` of a heritable primitive still publishes the base" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type C ="
                                "        inherit exn"
                                "        member P: int"
                                ""
                            ])

                Expect.isEmpty r.Messages "a heritable base raises nothing"

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape ->
                    match shape.FrozenBaseType with
                    | ValueSome b -> Expect.equal b.Key.Name "exn" "the exn canon is the published base"
                    | ValueNone -> failtest "the heritable base is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "a bodied signature's `inherit` of a capability diagnoses and publishes no base" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type C ="
                                "        inherit disposable"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages
                     |> List.exists (fun m -> m.Contains "Cannot inherit from interface 'disposable'"))
                    (sprintf "the capability base is diagnosed as an interface (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape -> Expect.isTrue shape.FrozenBaseType.IsNone "no base type is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "a bodied signature's `interface` naming a class diagnoses and publishes no interface" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type B ="
                                "        member Q: int"
                                ""
                                "    type C ="
                                "        interface B"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "is not an interface"))
                    (sprintf "the class-typed `interface` clause is diagnosed (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue shape.FrozenInterfaces.IsEmpty "no interface is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "a bodied signature's `interface` naming a tuple diagnoses and publishes no interface" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type C ="
                                "        interface (int * int)"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "is not an interface"))
                    (sprintf "the non-nominal `interface` clause is diagnosed (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue shape.FrozenInterfaces.IsEmpty "no interface is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }

            test "a bodied signature's `interface` naming an undefined type publishes no interface" {
                let r =
                    resolveFsi
                        "app.fsi"
                        (String.concat
                            "\n"
                            [
                                "namespace App"
                                ""
                                "module M ="
                                "    type C ="
                                "        interface Unknown"
                                "        member P: int"
                                ""
                            ])

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "Unknown"))
                    (sprintf "the undefined `interface` clause is diagnosed (%A)" r.Messages)

                match shapeOf r "C" with
                | ExternalTypeShape.Class shape ->
                    Expect.isTrue shape.FrozenInterfaces.IsEmpty "no interface is published"
                | other -> failtestf "expected a Class shape for C; got %A" other
            }
        ]

/// `Lib.Mask`, a `[<Literal>]` declared by a referenced assembly's signature alone.
let private libSig = "namespace Lib\n\n[<Literal>]\nval Mask: int = 0x3"

/// The denotation of the `int` literal `n`.
let private intC (n: int) : TConstDenotation =
    let v = TConstValue.Integral(IntValue.Int32 n)

    {
        Result = TConstResult.Scalar v
        Ty = FTConst(TConstValue.canonKey v, Block.empty)
    }

/// The referenced assembly's signature stacked over the real contract, as a consumer sees it.
let private libProvider () : IExternalSymbolProvider =
    let r = resolveFsi "lib.fsi" libSig
    Expect.isEmpty r.Messages "the signature resolves cleanly"
    ExternalSymbolProviders.composite [ realProvider.Value; r.Provider ]

[<Tests>]
let literalTests =
    testList
        "SignatureResolution literals"
        [
            test "`[<Literal>] val X: int = 0x3` publishes its checked value" {
                let r = resolveFsi "app.fsi" libSig
                Expect.isEmpty r.Messages "clean"

                Expect.equal (symbolOf r "Mask").Literal (ValueSome(intC 3)) "the value as checked"
            }

            test "a value of another type than the annotation is reported" {
                let r = resolveFsi "app.fsi" "namespace Lib\n\n[<Literal>]\nval Mask: int64 = 3"

                Expect.equal
                    r.Messages
                    [ "This expression was expected to have type 'int64' but here has type 'int'" ]
                    "the literal keeps its own type"

                Expect.isTrue (symbolOf r "Mask").Literal.IsNone "no value is published"
            }

            test "a `[<Literal>]` without a value is reported" {
                let r = resolveFsi "app.fsi" "namespace Lib\n\n[<Literal>]\nval Mask: int"

                Expect.isTrue
                    (r.Messages |> List.exists (fun m -> m.Contains "must declare its value"))
                    (sprintf "%A" r.Messages)
            }

            test "a value without `[<Literal>]` is reported" {
                let r = resolveFsi "app.fsi" "namespace Lib\n\nval Mask: int = 3"

                Expect.isTrue
                    (r.Messages
                     |> List.exists (fun m -> m.Contains "Only a value marked [<Literal>]"))
                    (sprintf "%A" r.Messages)

                Expect.isTrue (symbolOf r "Mask").Literal.IsNone "no value is published"
            }

            test "a published literal is a constant at a use site in another assembly" {
                let lexed, file = parseFile "open Lib\n\nlet x = Mask"

                let tast =
                    Pipeline.analyseSemFor testCompiling (libProvider ()) (LexedFile.ofText lexed) file

                expectCleanTast tast

                match List.last (List.ofSeq tast.Decls) with
                | TDecl.Let({
                                Value = TExpr.Const(TConstValue.Integral(IntValue.Int32 3), _, _)
                            },
                            _,
                            _) -> ()
                | other -> failtestf "expected `x` to hold the published constant, got %A" other
            }

            test "a published literal folds inside a constant expression in another assembly" {
                let ctx, file = analyseNameRes (libProvider ()) "open Lib\n\nlet x = Mask ||| 4"
                let b = firstBinding file

                match ConstExprCheck.check ctx (ctx.UseSiteAt(CstKeys.ofBinding b)) ValueNone b.expr with
                | ValueSome node -> Expect.equal (TConstExpr.denotation node) (intC 7) "Mask ||| 4"
                | ValueNone -> failtestf "rejected: %A" [ for d in ctx.Diagnostics -> d.Message ]
            }
        ]
