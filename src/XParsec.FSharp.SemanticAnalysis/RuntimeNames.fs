namespace XParsec.FSharp.SemanticAnalysis

/// Single source of truth for the well-known runtime type identities that flow
/// through the pipeline. Before the
/// SymbolKey refactor these names + the arity-strip that recognises them were
/// duplicated across `ClrEnv.isVesperListName` (codegen), `FreezeExpr`'s list-
/// retarget (front end), and `RefCellPromotion` — each had independently re-derived
/// "is this the Vesper cons-list / the ref cell?". This module collapses that to
/// one place: the canonical `*Key` constants below, which the producers stamp and
/// the recognisers match.
///
/// Identity is the `SymbolKey`, not a string. Each singleton has exactly one
/// canonical key (the cons-list additionally has its lowercase abbreviation key —
/// the abbreviation name is load-bearing for contract extraction
/// — so it's the one type with two accepted nominal
/// forms). Recognition is asm-blind structural field comparison against those keys
/// (`sameTypeAsmBlind`): same namespace + same bare simple name, home assembly
/// ignored, no qualified-string rebuild. The former parallel fully-qualified string
/// constants + the `isVesperList` string recogniser are gone — the key constants are
/// now the sole representation.
///
/// Scope: this module owns ONLY the well-known
/// singleton key constants + the recognisers built over them. The generic
/// `SymbolKey` ↔ string projection / minting helpers (`bareName`, `simpleName`,
/// `qualifiedName`, `keyAsm`, `externalTypeKey`, …) are NOT runtime-name-specific,
/// so they live with `valueKeyOf` in `module ExternalSymbols`.
[<RequireQualifiedAccess>]
module RuntimeNames =

    // --- Canonical SymbolKey identities --------------------------------------------
    //
    // The well-known runtime singletons above flow through the pipeline as bare
    // string `SemType` names; Phase 5 (sub-step 4) puts a `SymbolKey` on the
    // nominal `SemType` cases, at which point these consumers compare *key*
    // identity instead of normalising strings (3b: `isVesperList name` becomes
    // `key = vesperListKey`). These constants are the one canonical key each
    // singleton's producers stamp and its consumers match — minted here, next to
    // the string forms, so the identity lives in exactly one place. Nothing reads
    // them yet; sub-step 4 wires the producers/consumers onto them.
    //
    // Shape conventions (so a key here equals the key the rest of the pipeline
    // mints for the same type):
    //   * `asm = Some <home>` — the type's **home assembly** (Phase 6), invariant
    //     per type. When the cons-list / ref cell are compiled *locally*
    //     (self-hosting `Vesper.List` / `Vesper.Core`) the stamped `info.Key`
    //     carries the same home (`PassContext.AssemblyName`), and a *consumer*
    //     resolves the cross-package reference to the same home via its
    //     `SymbolOrigin.Assembly` — so this one key recognises the local
    //     definition and the cross-package reference alike. The home is the
    //     assembly's *simple name* as referenced (`Vesper.List`, not the
    //     namespace `Vesper.Collections`).
    //   * `name` is the **arity-qualified** simple name (`` List`1 ``), following
    //     `SymbolKeyOps.arityName`.

    /// Canonical identity for the Vesper cons-list `List` union. Home assembly
    /// `Vesper.List` (the simple name the cons-list is referenced by — `ProjectInfo`
    /// `Vesper.List` owns `Vesper.Collections.List\`1`), arity-qualified `List`1` to
    /// match the locally compiled `UnionTypeInfo.Key` (`TypeKey(Some "Vesper.List",
    /// "Vesper.Collections", "List`1")`). The producers' canonical key.
    let vesperListKey: SymbolKey =
        SymbolKey.TypeKey(Some "Vesper.List", "Vesper.Collections", "List`1")

    /// The cons-list's lowercase `list` abbreviation (the `'T list` convention) —
    /// the cons-list's *second* accepted nominal form, sharing the union's namespace.
    /// Recogniser-only (no producer mints the abbreviation; `isVesperListKey` matches
    /// it alongside `vesperListKey`), hence `private`.
    let private vesperListAbbrevKey: SymbolKey =
        SymbolKey.TypeKey(Some "Vesper.List", "Vesper.Collections", "list")

    /// Canonical identity for FSharp.Core's `list` — the non-retargeted default
    /// `FreezeExpr` / `Unification` fall back to. Home `FSharp.Core`, arity 1
    /// (`` list`1 ``); never project-local.
    let fsharpCoreListKey: SymbolKey =
        SymbolKey.TypeKey(Some "FSharp.Core", "Microsoft.FSharp.Collections", "list`1")

    /// Canonical identity for the heap ref-cell record (`Ref<'T>`, arity 1 ⇒
    /// `` Ref`1 ``). Home `Vesper.Core`, matching the locally compiled
    /// `Vesper.Core` `RecordTypeInfo.Key`.
    let vesperRefKey: SymbolKey =
        SymbolKey.TypeKey(Some "Vesper.Core", "Vesper", "Ref`1")

    /// Canonical identity for the `%A` structural-format interface
    /// `Vesper.IStructuralFormattable` (P3, non-generic). Home `Vesper.Core` (which
    /// owns it). Recogniser-only — `isStructuralFormattableKey`
    /// gates whether *this* compilation is `Vesper.Core` itself (then the per-type
    /// `Format` synthesis is suppressed; see codegen `Layout` / `Assembler`), so
    /// `private`.
    let private structuralFormattableKey: SymbolKey =
        SymbolKey.TypeKey(Some "Vesper.Core", "Vesper", "IStructuralFormattable")

    /// Canonical identity for `PrintfFormat<'Printer,'State,'Residue,'Result>`
    /// (arity 4 ⇒ `` PrintfFormat`4 ``) — the type a format literal freezes to
    /// (`PrintfSpec.printfFormatName`). Home `FSharp.Core`. The printf *entry
    /// points* are already key-based (`PrintfSpec.canonicalPrintfShortName`, a
    /// `ValueKey`); this is the format *type* identity.
    let printfFormatKey: SymbolKey =
        SymbolKey.TypeKey(Some "FSharp.Core", "Microsoft.FSharp.Core", "PrintfFormat`4")

    /// Canonical identity for the BCL `System.Object` — recognised at the unify
    /// boundary (an empty `TyClass` whose key denotes `System.Object` satisfies the
    /// equality/derives predicates). Non-generic. Recogniser-only (no producer mints
    /// it; `System.Object` arrives via external resolution), and the home assembly is
    /// a don't-care here (`isSystemObjectKey` is asm-blind), so `private`.
    let private systemObjectKey: SymbolKey =
        SymbolKey.TypeKey(Some "System.Runtime", "System", "Object")

    // --- Well-known BCL contract interfaces -------------------------------------------
    //
    // The handful of BCL interface identities the *target-agnostic* passes still
    // resolve against directly: `for … in` enumeration (`IEnumerable<'T>`),
    // `use`/`for-in` disposal (`IDisposable`), and the `[<CustomEquality>]` /
    // `[<CustomComparison>]` conformance checks (`IEquatable<'T>` / `IComparable<'T>`).
    // These are CLR/BCL contracts — on a JS target the same *capabilities* (the
    // iterable protocol, shape-based equality) wear different identities. Hoisting
    // them here does NOT make the passes target-independent; it collapses the
    // identities that were scattered as ad-hoc string literals across `InferControlFlow`
    // / `Infer` / `Unification` into one auditable place (matching the singleton-key
    // precedent above), so the CLR coupling is visible in a single file and can't
    // drift. Making them provider-resolved per target is the deferred next step.
    //
    // `asm = Some "System.Runtime"` mirrors `systemObjectKey`; it is a don't-care
    // for the asm-blind recognisers / qualified-name projections built below.

    /// Canonical identity for `System.Collections.Generic.IEnumerable<'T>` (arity 1) —
    /// the interface a `for … in` source is resolved against.
    let private ienumerableKey: SymbolKey =
        SymbolKey.TypeKey(Some "System.Runtime", "System.Collections.Generic", "IEnumerable`1")

    /// Canonical identity for `System.IDisposable` — the interface whose presence
    /// gates `use` / `for-in` disposal (the `finally` is emitted iff the source is
    /// `IDisposable`).
    let private idisposableKey: SymbolKey =
        SymbolKey.TypeKey(Some "System.Runtime", "System", "IDisposable")

    /// Canonical identity for `System.IEquatable<'T>` (arity 1) — a `[<CustomEquality>]`
    /// type must implement it (FS0378).
    let private iequatableKey: SymbolKey =
        SymbolKey.TypeKey(Some "System.Runtime", "System", "IEquatable`1")

    /// Canonical identity for `System.IComparable<'T>` (arity 1) — a
    /// `[<CustomComparison>]` type must implement it (FS0378).
    let private icomparableKey: SymbolKey =
        SymbolKey.TypeKey(Some "System.Runtime", "System", "IComparable`1")

    /// The user-facing abbreviation for the object root — `obj` — declared in
    /// `prim-types-object.fs` as `type obj = (# "System.Object" #)`. The front end
    /// carries it as `TyConst("obj", _)` (what `translateType` produces); codegen as
    /// `FTConst("obj", _)` or the rendered `"obj"` sig. The single source for the
    /// abbreviation name, so `obj ≡ System.Object` is decided in one place rather
    /// than re-spelled at each predicate (the `arrayName`/`prim-types-min.fs`
    /// precedent above). Pairs with `systemObjectQualifiedName` (the intrinsic it
    /// binds to) and `isSystemObjectKey` (the same identity by `SymbolKey`).
    let objAbbrevName: string = "obj"

    /// The intrinsic the `obj` abbreviation binds to — `System.Object`, the
    /// `(# "System.Object" #)` of `prim-types-object.fs`. Derived from the canonical
    /// `systemObjectKey` so the qualified string and the key identity can never
    /// drift. Used where the param model is a *rendered* signature string rather than
    /// a `SymbolKey` (an external member's `argSig`).
    let systemObjectQualifiedName: string = SymbolKeyOps.qualifiedName systemObjectKey

    /// The BCL `System.IO.TextWriter` nominal name, carried as the `SemType` of a
    /// printf writer *sink* (`fprintf`/`bprintf`, `PrintfSpec.tyTextWriter`). A CLR
    /// contract with no JS analogue — single-sourced here so the one consumer's
    /// hardcoded literal is an auditable, named coupling rather than a bare string
    /// buried in the printf spec (the deferred target-independent model resolves the
    /// sink type through the provider per target).
    let textWriterTypeName: string = "System.IO.TextWriter"

    /// The canonical identity name for a rank-`rank` array, sourced from the
    /// `prim-types-min.fs` declaration `type 'T ``[]`` ` (rank 1 → `"[]"`;
    /// rank N → `"[" + (N-1) commas + "]"`, e.g. `"[,]"` for 2-D). Arrays are a
    /// generic intrinsic carried as `TyConst(arrayName rank, [elem])`
    /// — this single name replaces the former
    /// `"array"` / `"arrayN"` / `"Microsoft.FSharp.Core.[]"` triple-naming.
    let arrayName (rank: int) : string =
        if rank <= 1 then
            "[]"
        else
            "[" + System.String(',', rank - 1) + "]"

    /// The canonical identity name for a managed by-ref (`T&`) — a *generic
    /// intrinsic* carried as `TyConst(byrefName, [elem])` / `FTConst(byrefName,
    /// [elem])`, exactly mirroring the array `arrayName` convention rather than a
    /// dedicated DU case (so it rides the existing `FrozenType`/`SemType` machinery
    /// — bridge, `RecoverOpenTypars` arg recursion, unification — untouched). A
    /// byref is legal only in parameter / return / local positions, never as a
    /// field or generic argument; `ClrEncoder.encodeType` emits its
    /// `ELEMENT_TYPE_BYREF` prefix at the return/param seam, not in the recursive
    /// type encoder. Its sole producer is the BCL-metadata resolver
    /// (`MetadataSymbols.tryBuildType`, e.g. `Span<T>.get_Item : T&`); the front end
    /// erases it to the element type at the value position (`inferIndexedLookup`).
    let byrefName: string = "&"

    /// True iff `name` is the identity name of a *structural type constructor* — an
    /// array of any rank (`arrayName`: `"[]"`, `"[,]"`, …) or a managed by-ref
    /// (`byrefName`: `"&"`). These are generic intrinsics (`arity ≥ 1`) representable
    /// by construction, lowered by dedicated backend paths (`SZArray`, the byref seam)
    /// rather than as a nominal receiver — so a member/representability resolver keyed
    /// on nominal BCL/contract types must let them keep their own path. Single source
    /// so the producers (`arrayName`/`byrefName`) and this recogniser can't drift.
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

    /// The external head an `[| … |]` array literal lowers to (`FreezeExpr`):
    /// `ArrayModule.OfList` applied to the literal cons-chain. The FSharp.Core
    /// path resolves it as a real module call; the BCL-only path recognises this
    /// exact head in codegen and emits the array directly (newarr + stelem) so an
    /// array literal needs no FSharp.Core. Single source so the producer
    /// (`FreezeExpr`) and the recogniser (`EmitCall`) can't drift.
    let arrayOfListName: string = "Microsoft.FSharp.Collections.ArrayModule.OfList"

    // --- Anonymous-union reserved member names ---------
    //
    // TypeScript-style literal types that are real *members* of an anonymous
    // structural union (`T | null`, `T | undefined`) rather than nominal types.
    // They carry no payload and resolve to a bare `TyConst name` — the same opaque
    // shape an unknown bare name produces, so `translateType` needs no dedicated arm
    // beyond `Type.Null` (the `null` keyword parses as `Type.Null`, not a named
    // type); `undefined` falls out of the named-type arm's opaque fallback. Codegen
    // erases them per backend (JS: literal `null`/`undefined`; CLR: a null reference
    // for `null`). `never` needs no name — it is the empty `TyOr` that `mkUnion []`
    // produces. Single-sourced here alongside the other well-known type names.

    /// The `null` literal type — the reserved member of `T | null`.
    let nullTypeName: string = "null"

    /// The `undefined` literal type — the reserved member of `T | undefined`.
    let undefinedTypeName: string = "undefined"

    // --- Well-known-singleton recognition by key (Phase 5.4) ---
    //
    // Recognition is asm-blind structural field comparison against the canonical
    // `*Key` constants above: same namespace + same bare (arity-stripped) simple
    // name, home assembly ignored. Asm-blind because a bare-named / origin-less mint
    // (a test helper, an asm-blind codegen path) carries no home assembly but still
    // denotes the singleton; allocation-free (no qualified-string rebuild) so it's
    // cheap on the hot unify / codegen paths. The canonical keys are the single
    // identity source — the former parallel FQ string constants + `isVesperList`
    // string recogniser are gone. (Where a *local* definition of
    // the same type must win — codegen's self-host cons-list — the caller checks the
    // project-local table first, then falls to these.)

    /// Asm-blind field match against a canonical `TypeKey`: same namespace and same
    /// bare (arity-stripped) simple name, home assembly ignored. Keys are always
    /// well-formed — every mint path (`externalTypeKey` / `qualifiedTypeKeyOf` /
    /// `LocalSymbolKey.ofType`) splits the namespace into `ns`, so the `name` segment
    /// never carries dots and one `bareName` strip suffices.
    let private sameTypeAsmBlind (canonical: SymbolKey) (k: SymbolKey) : bool =
        match canonical, k with
        | SymbolKey.TypeKey(_, cns, cn), SymbolKey.TypeKey(_, ns, n) ->
            cns = ns && SymbolKeyOps.bareName cn = SymbolKeyOps.bareName n
        | _ -> false

    /// One resolved capability identity: the `SymbolKey` plus its rendered qualified
    /// name — the two projections the consumer sites split across (some compare a
    /// `SymbolKey` asm-blind, some compare a rendered interface-name string from
    /// `ExternalSymbols.instantiateInterfaces`).
    type CapabilityIdentity =
        {
            Key: SymbolKey
            QualifiedName: string
        }

        /// Asm-blind key match (same namespace + bare name) — the `SymbolKey`-keyed
        /// consumers.
        member this.MatchesKey(k: SymbolKey) : bool = sameTypeAsmBlind this.Key k

        /// Rendered-name match — the string-keyed consumers.
        member this.MatchesName(s: string) : bool = s = this.QualifiedName

    /// The four language-capability identities, resolved once per compilation
    /// (`PassContext`). Iteration/disposal back the `for-in`/`use` lowering;
    /// equatable/comparable back the FS0378 custom-eq/comp conformance check.
    type CapabilityIds =
        {
            Enumerable: CapabilityIdentity
            Disposable: CapabilityIdentity
            Equatable: CapabilityIdentity
            Comparable: CapabilityIdentity
        }

    /// Resolve the capability identities. Provider-resolved per-target identities are
    /// the deferred next step; today this returns the CLR-literal identities
    /// unconditionally — a temporary fallback that keeps CLR green until the core
    /// `.fsi` names the capabilities and the literals are deleted.
    let resolveCapabilities () : CapabilityIds =
        let ident (key: SymbolKey) : CapabilityIdentity =
            {
                Key = key
                QualifiedName = SymbolKeyOps.qualifiedName key
            }

        {
            Enumerable = ident ienumerableKey
            Disposable = ident idisposableKey
            Equatable = ident iequatableKey
            Comparable = ident icomparableKey
        }

    /// True iff `k` denotes the Vesper cons-list in either of its nominal forms —
    /// the `List` union or its lowercase `list` abbreviation (both in
    /// `Vesper.Collections`).
    let isVesperListKey (k: SymbolKey) : bool =
        sameTypeAsmBlind vesperListKey k || sameTypeAsmBlind vesperListAbbrevKey k

    let isFsharpCoreListKey (k: SymbolKey) : bool = sameTypeAsmBlind fsharpCoreListKey k

    /// True iff the *compiled qualified type-name string* (`Vesper.Collections.List`1`)
    /// denotes the Vesper cons-list `List` union — the string-keyed analogue of
    /// `isVesperListKey`, for the one consumer holding the extracted contract's
    /// `TypeShapes` name (a string) rather than a `SymbolKey`: the reverse
    /// union-case index in `VesperLib.TyparCapture`, which excludes the cons-list's
    /// `Empty`/`Cons` cases from bare-ctor-name resolution.
    let isVesperListName (compiledName: string) : bool =
        compiledName = SymbolKeyOps.qualifiedName vesperListKey

    /// True iff `k` denotes the `%A` structural-format interface
    /// `Vesper.IStructuralFormattable`. The single source the codegen `Layout` and
    /// `Assembler` both consult to detect *this* compilation defining the interface
    /// (⇒ it is `Vesper.Core`, so suppress per-type `Format` synthesis); the two
    /// must agree, so they share this recogniser rather than each re-spelling the
    /// qualified name. Asm-blind, matching the list/object recognisers.
    let isStructuralFormattableKey (k: SymbolKey) : bool =
        sameTypeAsmBlind structuralFormattableKey k

    /// True iff `k` denotes the BCL `System.Object`. Asm-blind (the consumers — the
    /// unify equality/derives predicates — never compared the home assembly).
    let isSystemObjectKey (k: SymbolKey) : bool = sameTypeAsmBlind systemObjectKey k

    /// True iff `k` denotes `PrintfFormat<'Printer,'State,'Residue,'Result>` — the
    /// format type a `printf` / `sprintf` literal freezes to. Asm-blind, matching the
    /// list/object recognisers; replaces the inline `bareName (qualifiedName key) =
    /// PrintfSpec.printfFormatName` rebuild at the codegen / FreezeExpr consumer sites.
    let isPrintfFormatKey (k: SymbolKey) : bool = sameTypeAsmBlind printfFormatKey k

    // --- Built-in primitive type names -----------------------------------------------

    /// The built-in *numeric* type names — every integral / floating / decimal form,
    /// including both the alias and the canonical spelling (`int`/`int32`,
    /// `sbyte`/`int8`, `float`/`double`, `float32`/`single`), since either can reach
    /// a consumer depending on how a type was written or resolved.
    ///
    /// The single source the consumers that classify a primitive by name share, so a
    /// new numeric type is added in one place instead of drifting across four
    /// independently-maintained lists (the prior state — each had its own gaps):
    ///   * the unifier's SRTP-arithmetic synthesis (`Engine.numericPrimitives`);
    ///   * the `%A` faithfulness gate (`FreezeExpr.structuredArgFaithful`, ∪ string/char/bool);
    ///   * the codegen value-type predicate (`EmitPattern.isValueType`, ∪ bool/char);
    ///   * the front-end primitive recogniser (`TypeTranslate.isPrimitiveName`, ∪ the
    ///     reference primitives unit/obj/objnull/voidptr/exn).
    /// Each consumer unions in its own non-numeric extras at the use site (visible
    /// there); the numeric core — the part that grows — lives here.
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
