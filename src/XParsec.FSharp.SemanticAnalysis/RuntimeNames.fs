namespace XParsec.FSharp.SemanticAnalysis

/// The canonical `*Key` identity of each well-known runtime type. Identity is the key, never
/// a string: a `TypeKey` carries its arity as a field, so recognition is `=` and strips nothing.
[<RequireQualifiedAccess>]
module RuntimeNames =

    [<Literal>]
    let private intrinsicNamespace = "Vesper"

    [<Literal>]
    let private collectionsNamespace = "Vesper.Collections"

    /// The namespaces resolving UNQUALIFIED in every compilation. A language fact, so fixed
    /// here rather than read from whatever package happens to be referenced.
    let preludeNamespaces: string list = [ intrinsicNamespace; collectionsNamespace ]

    let vesperListKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity collectionsNamespace "List" 1

    /// The cons-list's second accepted nominal form. Recogniser-only: no producer mints it.
    let private vesperListAbbrevKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity collectionsNamespace "list" 1

    /// The non-retargeted default. Never project-local.
    let fsharpCoreListKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity "Microsoft.FSharp.Collections" "list" 1

    let vesperRefKey: TypeKey = SymbolKeyOps.typeKeyOfArity intrinsicNamespace "Ref" 1

    /// Declared by the synthesised per-record/union `Format`. CLR-only: JS renders `%A`
    /// through hole-directed renderers.
    let structuralFormattableKey: TypeKey =
        SymbolKeyOps.typeKeyOf intrinsicNamespace "IStructuralFormattable"

    let formatSinkKey: TypeKey = SymbolKeyOps.typeKeyOf intrinsicNamespace "IFormatSink"

    /// Curried at 2, the flat overloads at 3–5: one name, distinguished by arity.
    let vesperFunKey (genericArity: int) : TypeKey =
        SymbolKeyOps.typeKeyOfArity intrinsicNamespace "Fun" genericArity

    /// The type a format literal freezes to.
    let printfFormatKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity "Microsoft.FSharp.Core" "PrintfFormat" 4

    /// What a source-level format ANNOTATION resolves to; `isPrintfFormatKey` admits both.
    let vesperPrintfFormatKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity intrinsicNamespace "PrintfFormat" 4

    /// The suffix every declared attribute class carries, and F#'s optional one at a use.
    [<Literal>]
    let AttributeSuffix = "Attribute"

    /// Named by the marker's BARE name, so every key provably carries the suffix.
    let private attributeKey (bareName: string) : TypeKey =
        SymbolKeyOps.typeKeyOf intrinsicNamespace (bareName + AttributeSuffix)

    let callAtMostOnceAttributeKey: TypeKey = attributeKey "CallAtMostOnce"
    let structuralEqualityAttributeKey: TypeKey = attributeKey "StructuralEquality"
    let structuralComparisonAttributeKey: TypeKey = attributeKey "StructuralComparison"
    let referenceEqualityAttributeKey: TypeKey = attributeKey "ReferenceEquality"
    let noEqualityAttributeKey: TypeKey = attributeKey "NoEquality"
    let customEqualityAttributeKey: TypeKey = attributeKey "CustomEquality"
    let noComparisonAttributeKey: TypeKey = attributeKey "NoComparison"
    let customComparisonAttributeKey: TypeKey = attributeKey "CustomComparison"
    let globalAttributeKey: TypeKey = attributeKey "Global"

    /// For the consumer that must recognise a marker's SPELLING, resolution having failed.
    let compilerAttributeKeys: TypeKey list =
        [
            callAtMostOnceAttributeKey
            structuralEqualityAttributeKey
            structuralComparisonAttributeKey
            referenceEqualityAttributeKey
            noEqualityAttributeKey
            customEqualityAttributeKey
            noComparisonAttributeKey
            customComparisonAttributeKey
            globalAttributeKey
        ]

    let objAbbrevName: string = "obj"

    /// What `obj` binds to, for the sites whose param model is a rendered signature string
    /// rather than a key.
    let systemObjectQualifiedName: string = "System.Object"

    // The printf sinks. CLR contracts with no JS analogue; `StringWriter` is the concrete
    // per-hole sink `%a`/`%t` instantiate, the abstract `TextWriter` not being `new`able.

    let textWriterTypeName: string = "System.IO.TextWriter"

    let stringBuilderTypeName: string = "System.Text.StringBuilder"

    let stringWriterTypeName: string = "System.IO.StringWriter"

    /// Rank 1 → `"[]"`; rank N → `"["` + (N-1) commas + `"]"`.
    let arrayName (rank: int) : string =
        if rank <= 1 then
            "[]"
        else
            "[" + System.String(',', rank - 1) + "]"

    /// The array's name as a member-bearing declaration must SPELL it: backtick-escaped, so
    /// it can carry no `` `N ``. THIS is the member-store / contract key, not `arrayName`.
    let arrayContractName: string = "``" + arrayName 1 + "``"

    /// The TYPE, not the `&` operator that constructs one. Legal only in parameter / return
    /// / local positions.
    let byrefName: string = "byref"

    /// An array of any rank, or a by-ref: the intrinsics with dedicated backend paths.
    let isStructuralConstructorName (name: string) : bool =
        name = byrefName
        || (name.Length >= 2
            && name.[0] = '['
            && name.[name.Length - 1] = ']'
            && (let mutable ok = true

                for i in 1 .. name.Length - 2 do
                    if name.[i] <> ',' then
                        ok <- false

                ok))

    /// The head an `[| … |]` literal lowers to; codegen emits the array directly from it.
    let arrayOfListName: string = "Microsoft.FSharp.Collections.ArrayModule.OfList"

    // Members of an anonymous union (`T | null`), not nominal types: no payload, so they
    // resolve to a bare `TyConst name`. `never` is the empty `TyOr`.

    let nullTypeName: string = "null"

    let undefinedTypeName: string = "undefined"

    /// Both spellings an impl can take: `Key` the PLATFORM/BCL name, `CanonKey` the BCL-free
    /// canonical one — `ValueNone` for a canon-only anchor (`seq`, and everything on JS).
    type CapabilityIdentity =
        {
            Key: TypeKey
            CanonKey: TypeKey voption
        }

        member this.SymKey: SymbolKey = SymbolKey.Type this.Key

        member this.Matches(k: TypeKey) : bool =
            this.Key = k
            || (
                match this.CanonKey with
                | ValueSome ck -> ck = k
                | ValueNone -> false
            )

        /// A non-type key names no capability.
        member this.Matches(k: SymbolKey) : bool =
            match k with
            | SymbolKey.Type t -> this.Matches t
            | _ -> false

        /// For a consumer holding the rendered interface name rather than a key.
        member this.MatchesName(name: string) : bool =
            this.Matches(SymbolKeyOps.qualifiedTypeKeyOf name 0)

    /// Resolved once per compilation THROUGH THE PROVIDER: one a provider does not name is
    /// `ValueNone`, never a hardcoded BCL fallback. A fixed record, not an open registry.
    type CapabilityIds =
        {
            Enumerable: CapabilityIdentity voption
            Enumerator: CapabilityIdentity voption
            Disposable: CapabilityIdentity voption
            Equatable: CapabilityIdentity voption
            Comparable: CapabilityIdentity voption
        }

        /// A provider-less compilation names no capability.
        static member none =
            {
                Enumerable = ValueNone
                Enumerator = ValueNone
                Disposable = ValueNone
                Equatable = ValueNone
                Comparable = ValueNone
            }

    /// Flattens the `ValueNone ⇒ no-match` convention at a recognizer site.
    let matchesKey (cap: CapabilityIdentity voption) (k: TypeKey) : bool =
        cap |> ValueOption.exists (fun c -> c.Matches k)

    /// `matchesKey` for a rendered interface name rather than a key.
    let matchesName (cap: CapabilityIdentity voption) (name: string) : bool =
        cap |> ValueOption.exists (fun c -> c.MatchesName name)

    /// Either nominal form: the `List` union or its `list` abbreviation.
    let isVesperListKey (k: TypeKey) : bool =
        k = vesperListKey || k = vesperListAbbrevKey

    let isFsharpCoreListKey (k: TypeKey) : bool = k = fsharpCoreListKey

    /// For the one consumer holding an extracted contract's compiled name, not a key.
    let isVesperListName (compiledName: string) : bool =
        compiledName = SymbolKeyOps.typeMetaName vesperListKey

    /// Either spelling of the format type.
    let isPrintfFormatKey (k: TypeKey) : bool =
        k = printfFormatKey || k = vesperPrintfFormatKey

    /// Alias and canonical spelling alike, since either can reach a consumer. Consumers
    /// union in their own non-numeric extras at the use site; the part that grows is here.
    let numericTypeNames: Set<string> =
        Set.ofList
            [
                "int"
                "int8"
                "int16"
                "int32"
                "int64"
                "uint"
                "uint8"
                "uint16"
                "uint32"
                "uint64"
                "byte"
                "sbyte"
                "nativeint"
                "unativeint"
                "float"
                "float32"
                "double"
                "single"
                "decimal"
            ]

    /// `objnull` is deliberately absent: it is the `obj | null` union, and must EXPAND to
    /// `FTOr [obj; null]` rather than dealias to bare `obj`.
    let referencePrimitiveNames: Set<string> =
        Set.ofList [ "bool"; "char"; "string"; "unit"; "obj"; "voidptr"; "exn" ]

    /// For a name the CALL SITE knows to be an intrinsic — nothing here classifies it. Taken
    /// verbatim at ARITY 0; a generic intrinsic (`seq`) is minted from the contract instead.
    let primitiveKey (name: string) : SymbolKey =
        SymbolKeyOps.typeKey intrinsicNamespace name

    /// For a name that is NOT a registered intrinsic. Distinct from `primitiveKey` so the
    /// intent is legible at each call site.
    let opaqueKey (name: string) : SymbolKey = SymbolKeyOps.typeKey "" name

    let private intrinsicHolder: TypeHolder =
        TypeHolder.InNamespace(SymbolKeyOps.namespaceKey intrinsicNamespace)

    /// Namespace and arity are compared, which a `simpleName ∈ names` test could not say.
    let isPrimitiveKeyIn (names: Set<string>) (k: SymbolKey) : bool =
        match k with
        | SymbolKey.Type t -> t.TyparArity = 0 && t.Holder = intrinsicHolder && names.Contains t.Name
        | _ -> false

    // One cached key per intrinsic: every producer reaches for the same object, so all
    // mints compare EQUAL. The generic ones mint off `arrayName`/`byrefName`.

    let unitKey: SymbolKey = primitiveKey "unit"
    let boolKey: SymbolKey = primitiveKey "bool"
    let charKey: SymbolKey = primitiveKey "char"
    let stringKey: SymbolKey = primitiveKey "string"
    let objKey: SymbolKey = primitiveKey objAbbrevName
    let exnKey: SymbolKey = primitiveKey "exn"
    let intKey: SymbolKey = primitiveKey "int"
    let int64Key: SymbolKey = primitiveKey "int64"
    let byteKey: SymbolKey = primitiveKey "byte"
    let uint32Key: SymbolKey = primitiveKey "uint32"
    let floatKey: SymbolKey = primitiveKey "float"
    /// JS shares `number` between this and `float`, so the key is the only carrier of the width.
    let float32Key: SymbolKey = primitiveKey "float32"
    let decimalKey: SymbolKey = primitiveKey "decimal"
    let undefinedKey: SymbolKey = primitiveKey "undefined"
    let byrefKey: SymbolKey = primitiveKey byrefName
    let arrayKey (rank: int) : SymbolKey = primitiveKey (arrayName rank)
    let dynamicKey: SymbolKey = primitiveKey "dynamic"

    /// NOT a `namespace Vesper` type: `null` is a keyword, so it has no declaring namespace
    /// and its identity is the bare name. A distinct type from `unit`.
    let nullKey: SymbolKey = opaqueKey nullTypeName

/// The well-known intrinsics by KEY IDENTITY, never a stringified name. The backends match
/// the frozen mirrors here too, so there is one recogniser per intrinsic across the tree.
[<AutoOpen>]
module IntrinsicTypePatterns =

    let private unitKey = RuntimeNames.unitKey
    let private boolKey = RuntimeNames.boolKey
    let private objKey = RuntimeNames.objKey
    let private stringKey = RuntimeNames.stringKey
    let private dynamicKey = RuntimeNames.dynamicKey
    let private arrayKey1 = RuntimeNames.arrayKey 1
    let private byrefKey = RuntimeNames.byrefKey
    let private nullKey = RuntimeNames.nullKey

    let (|TyUnit|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.IsEmpty && k = unitKey -> Some()
        | _ -> None

    let (|TyBool|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.IsEmpty && k = boolKey -> Some()
        | _ -> None

    let (|TyObj|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.IsEmpty && k = objKey -> Some()
        | _ -> None

    let (|TyString|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.IsEmpty && k = stringKey -> Some()
        | _ -> None

    let (|TyDynamic|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.IsEmpty && k = dynamicKey -> Some()
        | _ -> None

    let (|TyNull|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.IsEmpty && k = nullKey -> Some()
        | _ -> None

    let (|TyArray|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.Length = 1 && k = arrayKey1 -> Some a.[0]
        | _ -> None

    let (|TyByref|_|) (ty: SemType) =
        match ty with
        | TyConst(k, a) when a.Length = 1 && k = byrefKey -> Some a.[0]
        | _ -> None

    /// An array of any rank, or a by-ref: the intrinsics with dedicated backend paths.
    let (|TyStructuralCtor|_|) (ty: SemType) =
        match ty with
        | TyConst(k, _) when RuntimeNames.isStructuralConstructorName (SymbolKeyOps.intrinsicName k) -> Some()
        | _ -> None

    /// The one sanctioned route from a key onto the platform-repr string axis. Unlike
    /// `simpleName` it refuses any key with a declaring namespace.
    let (|PlatformName|_|) (k: SymbolKey) : string option =
        match k with
        | SymbolKey.Type t when t.TyparArity = 0 && t.Holder = TypeHolder.InNamespace NamespaceKey.Global -> Some t.Name
        | _ -> None

    let (|FTUnit|_|) (ft: FrozenType) =
        match ft with
        | FTConst(k, a) when a.IsEmpty && k = unitKey -> Some()
        | _ -> None

    /// The CLR encodes `obj` as the primitive `ELEMENT_TYPE_OBJECT`, not a `TypeRef`.
    let (|FTObj|_|) (ft: FrozenType) =
        match ft with
        | FTConst(k, a) when a.IsEmpty && k = objKey -> Some()
        | _ -> None

    let (|FTArray|_|) (ft: FrozenType) =
        match ft with
        | FTConst(k, a) when a.Length = 1 && k = arrayKey1 -> Some a.[0]
        | _ -> None

    let (|FTByref|_|) (ft: FrozenType) =
        match ft with
        | FTConst(k, a) when a.Length = 1 && k = byrefKey -> Some a.[0]
        | _ -> None

    let (|FTNull|_|) (ft: FrozenType) =
        match ft with
        | FTConst(k, a) when a.IsEmpty && k = nullKey -> Some()
        | _ -> None
