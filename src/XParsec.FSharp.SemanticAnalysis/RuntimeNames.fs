namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer

/// Single source of truth for the well-known runtime types the pipeline names. Every other
/// file refers to one through a key minted here, so a well-known type's SPELLING is written
/// exactly once in the tree. Recognition is KEY EQUALITY: a `TypeKey` carries its arity as
/// a field, so there is nothing for a matcher to strip. The few NAMES that remain are for
/// the axes string-keyed by design: a compiled-name probe, a platform-repr map, a source
/// spelling met before any identity exists for it.
[<RequireQualifiedAccess>]
module RuntimeNames =


    [<Literal>]
    let private intrinsicNamespace = "Vesper"

    [<Literal>]
    let private collectionsNamespace = "Vesper.Collections"

    /// The namespaces resolving UNQUALIFIED in every compilation.
    let preludeNamespaces: string list = [ intrinsicNamespace; collectionsNamespace ]

    // The packages carrying the intrinsics, spelled as a manifest's `depends-on` spells them,
    // because that is where an `IntrinsicNotInScope` diagnostic tells the author to add one.

    [<Literal>]
    let corePackageName = "Vesper.Core"

    [<Literal>]
    let listPackageName = "Vesper.List"

    let vesperListKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity collectionsNamespace "List" 1

    /// The cons-list's second accepted nominal form. No producer mints it: it is matched
    /// alongside `vesperListKey`, and a `[…]` literal probes the project's own abbreviation
    /// table by the SPELLING it carries.
    let vesperListAbbrevKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity collectionsNamespace "list" 1

    let fsharpCoreListKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity "Microsoft.FSharp.Collections" "list" 1

    let vesperRefKey: TypeKey = SymbolKeyOps.typeKeyOfArity intrinsicNamespace "Ref" 1

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

    // The five language-capability ANCHORS, as the contract declares them: what the
    // capability resolution asks the provider for, and the canon each resolved identity
    // carries when the provider answers with no platform name of its own.

    let seqKey: TypeKey = SymbolKeyOps.typeKeyOfArity collectionsNamespace "seq" 1

    let enumeratorKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity collectionsNamespace "enumerator" 1

    let disposableKey: TypeKey = SymbolKeyOps.typeKeyOf intrinsicNamespace "disposable"

    let equatableKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity intrinsicNamespace "equatable" 1

    let comparableKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity intrinsicNamespace "comparable" 1

    /// The suffix every declared attribute class carries, and F#'s optional one at a use.
    [<Literal>]
    let AttributeSuffix = "Attribute"

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
    let allowNullLiteralAttributeKey: TypeKey = attributeKey "AllowNullLiteral"
    let globalAttributeKey: TypeKey = attributeKey "Global"

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
            allowNullLiteralAttributeKey
            globalAttributeKey
        ]

    let objAbbrevName: string = "obj"

    let systemObjectQualifiedName: string = "System.Object"

    // The printf sinks. CLR contracts with no JS analogue.

    let textWriterTypeName: string = "System.IO.TextWriter"

    /// The `Vesper.Printf` write-through format handler, a Vesper RUNTIME type carried as a
    /// bare nominal name by the CLR backend's `%A` recipes.
    let formatterTypeName: string = "Vesper.Formatter"

    let stringBuilderTypeName: string = "System.Text.StringBuilder"

    let stringWriterTypeName: string = "System.IO.StringWriter"

    // Members of an anonymous union (`T | null`), not nominal types: no payload, so they
    // resolve to a bare `TyConst name`.

    let nullTypeName: string = "null"

    let undefinedTypeName: string = "undefined"

    /// Both spellings an impl can take: `Key` the PLATFORM/BCL name, `CanonKey` the BCL-free
    /// canonical one. `ValueNone` where there is no platform name — every anchor on JS.
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

        member this.Matches(k: SymbolKey) : bool =
            match k with
            | SymbolKey.Type t -> this.Matches t
            | _ -> false

    /// A capability a provider does not name is `ValueNone`, never a hardcoded BCL fallback.
    type CapabilityIds =
        {
            Enumerable: CapabilityIdentity voption
            Enumerator: CapabilityIdentity voption
            Disposable: CapabilityIdentity voption
            Equatable: CapabilityIdentity voption
            Comparable: CapabilityIdentity voption
        }

        static member none =
            {
                Enumerable = ValueNone
                Enumerator = ValueNone
                Disposable = ValueNone
                Equatable = ValueNone
                Comparable = ValueNone
            }

    let matchesKey (cap: CapabilityIdentity voption) (k: TypeKey) : bool =
        cap |> ValueOption.exists (fun c -> c.Matches k)

    /// Whether a DECLARED interface set carries `cap`: `int`'s surface lists `equatable<int>`
    /// because `prim-types-min.fsi` says so.
    let declaresCapability (cap: CapabilityIdentity voption) (interfaces: EqArray<FrozenInterface>) : bool =
        match cap with
        | ValueNone -> false
        | ValueSome c -> interfaces |> EqArray.exists (fun iface -> c.Matches iface.Key)

    /// The identity + type args a REALISED interface denotes. A `TyConst` counts: that is how
    /// an intrinsic interface (`seq<'T>` on JS) realises.
    let interfaceNominal (ty: SemType) : struct (SymbolKey * EqArray<SemType>) voption =
        match ty with
        | TyClass(k, args)
        | TyUnion(k, args)
        | TyRecord(k, args) -> ValueSome(struct (SymbolKey.Type k, args))
        | TyConst(k, args) -> ValueSome(struct (k, args))
        | _ -> ValueNone

    /// The type args of the first realised interface whose identity is `cap`.
    let tryCapabilityArgs (cap: CapabilityIdentity) (interfaces: SemType[]) : EqArray<SemType> voption =
        interfaces
        |> Array.tryPick (fun ty ->
            match interfaceNominal ty with
            | ValueSome(struct (k, args)) when cap.Matches k -> Some args
            | _ -> None
        )
        |> function
            | Some args -> ValueSome args
            | None -> ValueNone

    /// Whether a realised interface set carries `cap` at all. A capability the provider does
    /// not name is carried by nothing.
    let carriesCapability (cap: CapabilityIdentity voption) (interfaces: SemType[]) : bool =
        cap |> ValueOption.exists (fun c -> (tryCapabilityArgs c interfaces).IsSome)

    /// Either nominal form: the `List` union or its `list` abbreviation.
    let isVesperListKey (k: TypeKey) : bool =
        k = vesperListKey || k = vesperListAbbrevKey

    let isFsharpCoreListKey (k: TypeKey) : bool = k = fsharpCoreListKey

    let isVesperListName (compiledName: string) : bool =
        compiledName = SymbolKeyOps.typeMetaName vesperListKey

    /// Either spelling of the format type.
    let isPrintfFormatKey (k: TypeKey) : bool =
        k = printfFormatKey || k = vesperPrintfFormatKey

    /// Taken verbatim at ARITY 0; a generic intrinsic (`seq`) is minted from the contract
    /// instead.
    let primitiveKey (name: string) : SymbolKey =
        SymbolKeyOps.typeKey intrinsicNamespace name

    /// For a name that is NOT a registered intrinsic.
    let opaqueKey (name: string) : SymbolKey = SymbolKeyOps.typeKey "" name

    let private intrinsicContainer: TypeContainer =
        TypeContainer.InNamespace(SymbolKeyOps.namespaceKey intrinsicNamespace)

    /// A `namespace Vesper` intrinsic whose NAME satisfies `nameSatisfies`, for the one
    /// classification a finite key set cannot spell: an array of arbitrary rank, whose
    /// identity names (`"[]"`, `"[,]"`, …) are unbounded. Namespace and arity are compared
    /// too, confining the name test to keys already established to be intrinsics. A
    /// classification with FIXED membership names its keys instead.
    let isIntrinsicKeyWhere (nameSatisfies: string -> bool) (k: SymbolKey) : bool =
        match k with
        | SymbolKey.Type t -> t.TyparArity = 0 && t.Container = intrinsicContainer && nameSatisfies t.Name
        | _ -> false

    /// An array of any rank or a managed by-ref, compared as an IDENTITY so a user type named
    /// `byref` in its own namespace cannot claim the dedicated backend path these ride.
    let isStructuralConstructorKey (k: SymbolKey) : bool =
        isIntrinsicKeyWhere SymbolKeyOps.isStructuralConstructorName k

    let unitKey: SymbolKey = primitiveKey "unit"
    let boolKey: SymbolKey = primitiveKey "bool"
    let charKey: SymbolKey = primitiveKey "char"
    let stringKey: SymbolKey = primitiveKey "string"
    let objKey: SymbolKey = primitiveKey objAbbrevName
    let exnKey: SymbolKey = primitiveKey "exn"
    let voidptrKey: SymbolKey = primitiveKey "voidptr"
    let sbyteKey: SymbolKey = primitiveKey "sbyte"
    let byteKey: SymbolKey = primitiveKey "byte"
    let int16Key: SymbolKey = primitiveKey "int16"
    let uint16Key: SymbolKey = primitiveKey "uint16"
    let intKey: SymbolKey = primitiveKey "int"
    let uint32Key: SymbolKey = primitiveKey "uint32"
    let int64Key: SymbolKey = primitiveKey "int64"
    let uint64Key: SymbolKey = primitiveKey "uint64"
    let nativeintKey: SymbolKey = primitiveKey "nativeint"
    let unativeintKey: SymbolKey = primitiveKey "unativeint"
    let floatKey: SymbolKey = primitiveKey "float"
    /// JS shares `number` between this and `float`, so the key is the only carrier of the width.
    let float32Key: SymbolKey = primitiveKey "float32"
    let decimalKey: SymbolKey = primitiveKey "decimal"
    /// The arbitrary-precision integer (CLR `System.Numerics.BigInteger`, JS `bigint`),
    /// the type a `NumBigInteger*` literal token pins to. Outside `numericKeys`: it is not
    /// a fixed-width scalar, so none of the width-driven classifications admit it.
    let bigintKey: SymbolKey = primitiveKey "bigint"
    let undefinedKey: SymbolKey = primitiveKey undefinedTypeName
    let byrefKey: SymbolKey = primitiveKey SymbolKeyOps.byrefName

    let arrayTypeKey (rank: int) : TypeKey =
        SymbolKeyOps.typeKeyOf intrinsicNamespace (SymbolKeyOps.arrayName rank)

    let arrayKey (rank: int) : SymbolKey = SymbolKey.Type(arrayTypeKey rank)

    let dynamicKey: SymbolKey = primitiveKey "dynamic"

    /// The IDENTITY of the base primitive a structural literal erases to, so every erasing
    /// consumer shares one identity rather than re-minting a name. Lives here rather than on
    /// `LiteralConst`: the DU compiles before the intrinsic identities do.
    let literalBaseKey (v: LiteralConst) : SymbolKey =
        match v with
        | LiteralConst.String _ -> stringKey
        | LiteralConst.Int _ -> intKey

    /// THE width → type projection: the elaborator gives an enum its underlying type by it,
    /// `freeze` types an integral constant by it, and the CLR backend types the constant it
    /// loads by it, so a constant's width and the type it freezes at cannot disagree.
    let intWidthKey (w: IntWidth) : SymbolKey =
        match w with
        | IntWidth.SByte -> sbyteKey
        | IntWidth.Byte -> byteKey
        | IntWidth.Int16 -> int16Key
        | IntWidth.UInt16 -> uint16Key
        | IntWidth.Int32 -> intKey
        | IntWidth.UInt32 -> uint32Key
        | IntWidth.Int64 -> int64Key
        | IntWidth.UInt64 -> uint64Key
        | IntWidth.NativeInt -> nativeintKey
        | IntWidth.UNativeInt -> unativeintKey

    // --- Built-in primitive classification -------------------------------------------
    //
    // The shared cores that the consumers classifying a primitive union their own extras
    // into. Held as KEYS: a member carries the `Vesper` namespace and the arity, so a user
    // type of the same short name in another namespace cannot pass. Each consumer still
    // spells its own set at the use site, where the classification is visible.

    /// Membership over a FIXED set of intrinsic identities, resolved once into a hash set.
    /// `HashSet` and not `Set`: a `SymbolKey` carries `EqArray`s, which are `NoComparison`
    /// by design. Bind the result at module level, because the set is built per call.
    let isKeyIn (keys: SymbolKey seq) : SymbolKey -> bool =
        let set = HashSet(keys)
        set.Contains

    /// The built-in NUMERIC identities, in canonical spelling. The ALIASES (`int32`,
    /// `single`, `double`, …) have no key because they never reach one: an alias is expanded
    /// during name resolution, so a type arriving as a key has already dealiased.
    let numericKeys: SymbolKey list =
        [
            sbyteKey
            byteKey
            int16Key
            uint16Key
            intKey
            uint32Key
            int64Key
            uint64Key
            nativeintKey
            unativeintKey
            floatKey
            float32Key
            decimalKey
        ]

    /// The non-numeric built-in primitive identities. `objnull` is deliberately absent: it is
    /// the `obj | null` union, and must EXPAND to `FTOr [obj; null]` rather than dealias to
    /// bare `obj`.
    let referencePrimitiveKeys: SymbolKey list =
        [ boolKey; charKey; stringKey; unitKey; objKey; voidptrKey; exnKey ]

    /// The ALIAS spellings of the numeric primitives: the ones with no key of their own
    /// (`type int32 = int`, `type single = float32`). Only the NAME axis meets them, and
    /// only before dealiasing.
    let private numericAliasNames =
        [ "int8"; "int32"; "uint"; "uint8"; "double"; "single" ]

    /// The SOURCE SPELLINGS of the numeric primitives: each identity's own name plus the
    /// aliases that dealias onto one. For the consumers that meet a spelling BEFORE any
    /// identity exists for it: a source-written annotation, a TS manifest's bare reference.
    let numericTypeNames: Set<string> =
        numericKeys
        |> Seq.map SymbolKeyOps.intrinsicName
        |> Seq.append numericAliasNames
        |> Set.ofSeq

    /// The name-axis projection of `referencePrimitiveKeys`; see `numericTypeNames`. No
    /// aliases, because each of these is spelled one way.
    let referencePrimitiveNames: Set<string> =
        referencePrimitiveKeys |> Seq.map SymbolKeyOps.intrinsicName |> Set.ofSeq

    /// NOT a `namespace Vesper` type: `null` is a keyword, so it has no declaring namespace
    /// and its identity is the bare name.
    let nullKey: SymbolKey = opaqueKey nullTypeName

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

    let (|TyStructuralCtor|_|) (ty: SemType) =
        match ty with
        | TyConst(k, _) when RuntimeNames.isStructuralConstructorKey k -> Some()
        | _ -> None

    /// A key onto the platform-repr string axis: refuses any key with a declaring namespace.
    let (|PlatformName|_|) (k: SymbolKey) : string option =
        match k with
        | SymbolKey.Type t when t.TyparArity = 0 && t.Container = TypeContainer.InNamespace NamespaceKey.Global ->
            Some t.Name
        | _ -> None

    let (|FTUnit|_|) (ft: FrozenType) =
        match ft with
        | FTConst(k, a) when a.IsEmpty && k = unitKey -> Some()
        | _ -> None

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
