namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer

/// Single source of truth for the well-known runtime types the pipeline references. Every other
/// file refers to one through a key minted here, so a well-known type's SPELLING is written
/// exactly once in the tree. Recognition is KEY EQUALITY: a `TypeKey` carries its arity as
/// a field, so there is nothing for a matcher to strip. Platform type ids carry
/// `PlatformTypeId`; the few bare NAMES that remain are for the string axes: a compiled-name
/// probe, a source spelling met before any identity exists for it.
[<RequireQualifiedAccess>]
module RuntimeNames =


    [<Literal>]
    let private intrinsicNamespace = "Vesper"

    [<Literal>]
    let private collectionsNamespace = "Vesper.Collections"

    // The packages carrying the intrinsics, spelled as a manifest's `depends-on` spells them,
    // because a diagnostic quotes the line for the author to add.

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

    /// The one cons-list `[…]`, `h :: t` and `for … in` all default to, over `elemTy`.
    let consListTy (elemTy: SemType) : SemType =
        TyUnion(vesperListKey, EqArray.singleton elemTy)

    /// The cons-list's binary case: element, then the rest of the chain.
    [<Literal>]
    let consCaseName = "Cons"

    /// The cons-list's nullary case, terminating a chain.
    [<Literal>]
    let emptyCaseName = "Empty"

    let vesperRefKey: TypeKey = SymbolKeyOps.typeKeyOfArity intrinsicNamespace "Ref" 1

    let structuralFormattableKey: TypeKey =
        SymbolKeyOps.typeKeyOf intrinsicNamespace "IStructuralFormattable"

    let formatSinkKey: TypeKey = SymbolKeyOps.typeKeyOf intrinsicNamespace "IFormatSink"

    /// Curried at 2, the flat overloads at 3–5: one name, distinguished by arity.
    let vesperFunKey (genericArity: int) : TypeKey =
        SymbolKeyOps.typeKeyOfArity intrinsicNamespace "Fun" genericArity

    /// The type a format literal freezes to, and what a source-level format ANNOTATION
    /// resolves to.
    let printfFormatKey: TypeKey =
        SymbolKeyOps.typeKeyOfArity intrinsicNamespace "PrintfFormat" 4

    // The five language-capability ANCHORS, as the contract declares them: what the
    // capability resolution asks the provider for, and the canon each resolved identity
    // carries when the provider supplies no platform name of its own.

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
    let literalAttributeKey: TypeKey = attributeKey "Literal"
    let measureAttributeKey: TypeKey = attributeKey "Measure"
    let globalAttributeKey: TypeKey = attributeKey "Global"
    let importAttributeKey: TypeKey = attributeKey "Import"
    let abstractClassAttributeKey: TypeKey = attributeKey "AbstractClass"
    let sealedAttributeKey: TypeKey = attributeKey "Sealed"
    let structAttributeKey: TypeKey = attributeKey "Struct"

    let requireQualifiedAccessAttributeKey: TypeKey =
        attributeKey "RequireQualifiedAccess"

    let autoOpenAttributeKey: TypeKey = attributeKey "AutoOpen"
    let compiledNameAttributeKey: TypeKey = attributeKey "CompiledName"

    let compilationRepresentationAttributeKey: TypeKey =
        attributeKey "CompilationRepresentation"

    let attributeUsageAttributeKey: TypeKey = attributeKey "AttributeUsage"

    /// The `AttributeTargets` flags enum typing `[<AttributeUsage>]`'s first argument.
    let attributeTargetsKey: TypeKey =
        SymbolKeyOps.typeKeyOf intrinsicNamespace "AttributeTargets"

    /// `[<IsByRefLike>]` resolves to the BCL declaration: byref-likeness is a CLR-only
    /// concept, so no Vesper declaration exists and on JS the spelling is an ordinary
    /// unresolved attribute.
    let isByRefLikeAttributeKey: TypeKey =
        SymbolKeyOps.typeKeyOf "System.Runtime.CompilerServices" "IsByRefLikeAttribute"

    /// The template text of `nativeOnly`'s body: an intrinsic no backend lowers to a real
    /// body, marking a binding whose implementation is its `[<Import>]` declaration.
    [<Literal>]
    let importSentinelText = "$use-import-attribute"

    /// `Vesper.CompilerMarkers.nativeOnly`, the one binding whose body is the sentinel.
    let nativeOnlyBindingKey: BindingKey =
        SymbolKeyOps.moduleBindingKey intrinsicNamespace "CompilerMarkers" "nativeOnly"

    /// `nativeOnlyBindingKey`, widened to `SymbolKey` for lookup in mixed-key tables.
    let nativeOnlyKey: SymbolKey = SymbolKey.Binding nativeOnlyBindingKey

    let objAbbrevName: string = "obj"

    // The printf sinks. CLR contracts with no JS analogue.

    let textWriterTypeId: PlatformTypeId = PlatformTypeId "System.IO.TextWriter"

    /// The `Vesper.Printf` write-through format handler, a Vesper RUNTIME type carried as a
    /// bare nominal name by the CLR backend's `%A` recipes.
    let formatterTypeId: PlatformTypeId = PlatformTypeId "Vesper.Formatter"

    let stringBuilderTypeId: PlatformTypeId = PlatformTypeId "System.Text.StringBuilder"

    let stringWriterTypeId: PlatformTypeId = PlatformTypeId "System.IO.StringWriter"

    // Disjuncts of an anonymous union (`T | null`), not nominal types: no payload, so they
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

    /// A capability a provider does not resolve is `ValueNone`, never a hardcoded BCL fallback.
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

        /// The capability `key` spells under either of its two nominal names. The one probe
        /// over every field, shared by every key-folding caller. Allocation-free.
        member this.TryMatch(key: TypeKey) : CapabilityIdentity voption =
            let inline hit (cap: CapabilityIdentity voption) =
                match cap with
                | ValueSome c -> c.Matches key
                | ValueNone -> false

            if hit this.Enumerable then this.Enumerable
            elif hit this.Enumerator then this.Enumerator
            elif hit this.Disposable then this.Disposable
            elif hit this.Equatable then this.Equatable
            elif hit this.Comparable then this.Comparable
            else ValueNone

    let matchesKey (cap: CapabilityIdentity voption) (k: TypeKey) : bool =
        cap |> ValueOption.exists (fun c -> c.Matches k)

    /// Whether a DECLARED interface set carries `cap`: `int`'s surface lists `equatable<int>`
    /// because `prim-types-min.fsi` says so.
    let declaresCapability (cap: CapabilityIdentity voption) (interfaces: EqArray<FrozenNominal>) : bool =
        match cap with
        | ValueNone -> false
        | ValueSome c -> interfaces |> EqArray.exists (fun iface -> c.Matches iface.Key)

    /// The identity + type args an INSTANTIATED interface denotes. A `TyConst` counts, being the
    /// form an intrinsic interface (`seq<'T>` on JS) takes.
    let interfaceNominal (ty: SemType) : struct (TypeKey * EqArray<SemType>) voption =
        match ty with
        | TyClass(k, args)
        | TyUnion(k, args)
        | TyRecord(k, args) -> ValueSome(struct (k, args))
        | TyConst(k, args) -> ValueSome(struct (k, args))
        | _ -> ValueNone

    /// The type args of the first instantiated interface whose identity is `cap`.
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

    /// Whether an instantiated interface set carries `cap` at all. An unresolved capability
    /// yields `false`.
    let carriesCapability (cap: CapabilityIdentity voption) (interfaces: SemType[]) : bool =
        cap |> ValueOption.exists (fun c -> (tryCapabilityArgs c interfaces).IsSome)

    /// Either nominal form: the `List` union or its `list` abbreviation.
    let isVesperListKey (k: TypeKey) : bool =
        k = vesperListKey || k = vesperListAbbrevKey

    let isVesperListName (compiledName: string) : bool =
        compiledName = SymbolKeyOps.typeMetaName vesperListKey

    /// Taken verbatim at ARITY 0; a generic intrinsic (`seq`) is minted from the contract
    /// instead.
    let primitiveKey (name: string) : TypeKey =
        SymbolKeyOps.typeKeyOf intrinsicNamespace name

    /// For a name that is NOT a registered intrinsic.
    let opaqueKey (name: string) : TypeKey = SymbolKeyOps.typeKeyOf "" name

    /// The global-name key a platform type id resolves through.
    let platformKey (id: PlatformTypeId) : TypeKey = SymbolKeyOps.typeKeyOf "" id.Value

    let private intrinsicContainer: TypeContainer =
        TypeContainer.InNamespace(SymbolKeyOps.namespaceKey intrinsicNamespace)

    /// A `namespace Vesper` intrinsic whose NAME satisfies `nameSatisfies`, for the one
    /// classification a finite key set cannot spell: an array of arbitrary rank, whose
    /// identity names (`"[]"`, `"[,]"`, …) are unbounded. Namespace and arity are compared
    /// too, confining the name test to keys already established to be intrinsics. A
    /// classification with FIXED membership lists its keys instead.
    let isIntrinsicKeyWhere (nameSatisfies: string -> bool) (k: TypeKey) : bool =
        k.TyparArity = 0 && k.Container = intrinsicContainer && nameSatisfies k.Name

    /// An array of any rank or a managed by-ref, compared as an IDENTITY so a user type named
    /// `byref` in its own namespace cannot claim the dedicated backend path reserved for these.
    let isStructuralConstructorKey (k: TypeKey) : bool =
        isIntrinsicKeyWhere SymbolKeyOps.isStructuralConstructorName k

    let unitKey: TypeKey = primitiveKey "unit"
    let boolKey: TypeKey = primitiveKey "bool"
    let charKey: TypeKey = primitiveKey "char"
    let stringKey: TypeKey = primitiveKey "string"
    let objKey: TypeKey = primitiveKey objAbbrevName
    let exnKey: TypeKey = primitiveKey "exn"
    let voidptrKey: TypeKey = primitiveKey "voidptr"
    let sbyteKey: TypeKey = primitiveKey "sbyte"
    let byteKey: TypeKey = primitiveKey "byte"
    let int16Key: TypeKey = primitiveKey "int16"
    let uint16Key: TypeKey = primitiveKey "uint16"
    let intKey: TypeKey = primitiveKey "int"
    let uint32Key: TypeKey = primitiveKey "uint32"
    let int64Key: TypeKey = primitiveKey "int64"
    let uint64Key: TypeKey = primitiveKey "uint64"
    let nativeintKey: TypeKey = primitiveKey "nativeint"
    let unativeintKey: TypeKey = primitiveKey "unativeint"
    let floatKey: TypeKey = primitiveKey "float"
    /// JS shares `number` between this and `float`, so the key is the only carrier of the width.
    let float32Key: TypeKey = primitiveKey "float32"
    let decimalKey: TypeKey = primitiveKey "decimal"
    /// The arbitrary-precision integer (CLR `System.Numerics.BigInteger`, JS `bigint`),
    /// the type a `NumBigInteger*` literal token pins to. Outside `numericKeys`, since it is
    /// not a fixed-width scalar.
    let bigintKey: TypeKey = primitiveKey "bigint"
    let undefinedKey: TypeKey = primitiveKey undefinedTypeName
    let byrefKey: TypeKey = primitiveKey SymbolKeyOps.byrefName

    let arrayTypeKey (rank: int) : TypeKey =
        SymbolKeyOps.typeKeyOf intrinsicNamespace (SymbolKeyOps.arrayName rank)

    let arrayKey (rank: int) : TypeKey = arrayTypeKey rank

    let dynamicKey: TypeKey = primitiveKey "dynamic"

    /// The IDENTITY of the base primitive a structural literal erases to, so every erasing
    /// consumer shares one identity rather than re-minting a name. Lives here rather than on
    /// `LiteralConst`: the DU compiles before the intrinsic identities do.
    let literalBaseKey (v: LiteralConst) : TypeKey =
        match v with
        | LiteralConst.String _ -> stringKey
        | LiteralConst.Int _ -> intKey

    /// THE kind → type projection: the elaborator gives an enum its underlying type by it,
    /// `freeze` types an integral constant by it, and the CLR backend types the constant it
    /// loads by it, so a constant's kind and the type it freezes at cannot disagree.
    let intKindKey (k: IntKind) : TypeKey =
        match k with
        | IntKind.SByte -> sbyteKey
        | IntKind.Byte -> byteKey
        | IntKind.Int16 -> int16Key
        | IntKind.UInt16 -> uint16Key
        | IntKind.Int32 -> intKey
        | IntKind.UInt32 -> uint32Key
        | IntKind.Int64 -> int64Key
        | IntKind.UInt64 -> uint64Key
        | IntKind.NativeInt -> nativeintKey
        | IntKind.UNativeInt -> unativeintKey

    /// The inverse of `intKindKey`: the kind an integral primitive's identity denotes,
    /// `ValueNone` for every other type.
    let intKindOfKey: TypeKey -> IntKind voption =
        let byKey = Dictionary<TypeKey, IntKind>()

        for k in IntKind.all do
            byKey.[intKindKey k] <- k

        fun key ->
            match byKey.TryGetValue key with
            | true, k -> ValueSome k
            | _ -> ValueNone

    // --- Built-in primitive classification -------------------------------------------
    //
    // The shared cores that the consumers classifying a primitive union their own extras
    // into. Held as KEYS: a member carries the `Vesper` namespace and the arity, so a user
    // type of the same short name in another namespace cannot pass. Each consumer still
    // spells its own set at the use site, where the classification is visible.

    /// Membership over a FIXED set of intrinsic identities, resolved once into a hash set.
    /// `HashSet` and not `Set`: a `SymbolKey` carries `EqArray`s, which are `NoComparison`
    /// by design. Bind the result at module level, because the set is built per call.
    let isKeyIn (keys: TypeKey seq) : TypeKey -> bool =
        let set = HashSet(keys)
        set.Contains

    /// The built-in NUMERIC identities, in canonical spelling. The ALIASES (`int32`,
    /// `single`, `double`, …) have no key because they never reach one: an alias is expanded
    /// during name resolution, so a type arriving as a key has already dealiased.
    let numericKeys: TypeKey list =
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

    /// The types an INTEGER printf specifier (`%d` `%i` `%u` `%x` `%X` `%o` `%B`) accepts,
    /// in the order a diagnostic lists them. `int` is the default, so it leads.
    let integerFormatKeys: EqArray<TypeKey> =
        EqArray.ofList
            [
                intKey
                sbyteKey
                byteKey
                int16Key
                uint16Key
                uint32Key
                int64Key
                uint64Key
                nativeintKey
                unativeintKey
            ]

    /// The types a FLOAT printf specifier (`%f` `%e` `%E` `%g` `%G`) accepts. `float` is
    /// the default, so it leads. `%M` is `decimal` alone and takes no family.
    let floatFormatKeys: EqArray<TypeKey> =
        EqArray.ofList [ floatKey; float32Key; decimalKey ]

    /// The non-numeric built-in primitive identities. `objnull` is deliberately absent: it is
    /// the `obj | null` union, and must EXPAND to `FTOr [obj; null]` rather than dealias to
    /// bare `obj`.
    let referencePrimitiveKeys: TypeKey list =
        [ boolKey; charKey; stringKey; unitKey; objKey; voidptrKey; exnKey ]

    /// The `prim-types-min` contract's data types, which every target declares.
    let private minContractKeys: TypeKey list = [ intKey; boolKey; unitKey ]

    /// The primitive identities a TARGET may lack: every built-in primitive except the
    /// `prim-types-min` trio. Each is language-known — a literal token or a bare written
    /// name reaches its key with no contract declaration — so on a target whose contract
    /// omits it the key still mints, and `PlatformTypes` reports every mention as
    /// `UnsupportedOnTarget` rather than the name going undefined.
    let private targetOptionalPrimitiveKeys: TypeKey list =
        let mandatory = isKeyIn minContractKeys

        numericKeys @ referencePrimitiveKeys @ [ bigintKey; undefinedKey ]
        |> List.filter (mandatory >> not)

    let isTargetOptionalPrimitiveKey: TypeKey -> bool =
        isKeyIn targetOptionalPrimitiveKeys

    /// The identity for a target-optional primitive NAME; `ValueNone` for every other spelling.
    let tryTargetOptionalPrimitiveKey (name: string) : TypeKey voption =
        match targetOptionalPrimitiveKeys |> List.tryFind (fun k -> k.Name = name) with
        | Some k -> ValueSome k
        | None -> ValueNone

    /// NOT a `namespace Vesper` type: `null` is a keyword, so it has no declaring namespace
    /// and its identity is the bare name.
    let nullKey: TypeKey = opaqueKey nullTypeName

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

    /// A key onto the platform type id axis: refuses any key with a declaring namespace.
    let (|PlatformName|_|) (k: TypeKey) : PlatformTypeId option =
        if k.TyparArity = 0 && k.Container = TypeContainer.InNamespace NamespaceKey.Global then
            Some(PlatformTypeId k.Name)
        else
            None

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
