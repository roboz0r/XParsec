namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// The signature-type encoder over a `ClrEnv`: turns a `SemType` into a metadata signature, mapping
/// function arrows to `Vesper.Fun`2`, lists to `FSharpList`1` / `Vesper.Collections.List`1`, user
/// types to their predicted `TypeDefinition`, and external types through the symbol provider. Also
/// hosts the blob/signature builders that are pure `encodeType` wrappers.
type internal ClrEncoder(env: ClrEnv) =
    let ctx = env.Ctx
    let reprs = env.Reprs
    let markFSharpCoreDep c = env.MarkFSharpCoreDep c
    let userTypes = env.UserTypes
    let envAsm = env.EnvAsm
    let zonk t = env.Zonk t
    let externalClassRef n = env.ExternalClassRef n
    let externalIsValueType n = env.ExternalIsValueType n
    let externalRecordRef (n, a) = env.ExternalRecordRef(n, a)
    let externalUnionRef (n, a) = env.ExternalUnionRef(n, a)
    // Open typars are self-describing `TempTypar` nodes the structural match resolves
    // directly (frozen-type-plan keystone). The one exception is a *concrete* generic
    // member (B-12) whose method-owned typars are still live `TyVar`s — the leaf hook
    // resolves those to `!!i` via the ambient `methodTyparRoots` window (empty, hence
    // a no-op, outside a generic-member emission).
    let methodTyparLeaf = env.MethodTyparLeaf

    let eUnit = env.EUnit
    let eValueTuple = env.EValueTuple
    let eTextWriter = env.ETextWriter
    let eFormatter = env.EFormatter
    let eHashCode = env.EHashCode
    let eDecimal = env.EDecimal
    let eFun2 = env.EFun2
    let ePrintfFormat4 = env.EPrintfFormat4
    let eVesperList1 = env.EVesperList1
    let eFSharpList1 = env.EFSharpList1
    let eFSharpFunc2 = env.EFSharpFunc2

    /// `FSharpList`1<X>` where `X` is encoded by `inner` — shared by `encodeType`'s list case and the
    /// cons/nil recipe signatures.
    let encodeListOf (te: SignatureTypeEncoder) (inner: SignatureTypeEncoder -> unit) : unit =
        markFSharpCoreDep "Microsoft.FSharp.Collections.FSharpList`1"
        let g = te.GenericInstantiation(eFSharpList1.Value, 1, false)
        inner (g.AddArgument())

    // Referenced-assembly nominal recognisers. Each
    // projects the key to the string the (fundamentally string-keyed)
    // `IExternalSymbolProvider` is keyed by and looks the `TypeRef` up *once*,
    // replacing the prior `(externalXxxRef …).IsSome` guard + `.Value` body
    // double-call (which re-projected `qualifiedName key` on each side). Matching on
    // the whole node lets the pattern read `args` for the arity probe.
    let (|ExternalClass|_|) (t: SemType) =
        match t with
        | TyClass(key, args) ->
            let qual = ExternalSymbols.qualifiedName key

            match externalClassRef qual with
            // A struct external type (`List`1+Enumerator`, §4.4) must encode as a
            // `VALUETYPE` element; every reference type stays `false`.
            | ValueSome tref -> Some(tref, externalIsValueType qual, args)
            | ValueNone -> None
        | _ -> None

    let (|ExternalRecord|_|) (t: SemType) =
        match t with
        | TyRecord(key, args) ->
            match externalRecordRef (ExternalSymbols.qualifiedName key, args.Length) with
            | ValueSome(tref, _) -> Some(tref, args)
            | ValueNone -> None
        | _ -> None

    let (|ExternalUnion|_|) (t: SemType) =
        match t with
        | TyUnion(key, args) ->
            match externalUnionRef (ExternalSymbols.qualifiedName key, args.Length) with
            | ValueSome(tref, _) -> Some(tref, args)
            | ValueNone -> None
        | _ -> None

    /// Encode a (zonked) `SemType` into a metadata signature slot. `tryLeaf` gets first crack at each
    /// zonked node before the structural match: when it encodes the node (returns `true`) recursion
    /// stops there. The executable path passes a no-op; the library path passes a typar-marker resolver.
    let rec encodeTypeCore
        (tryLeaf: SignatureTypeEncoder -> SemType -> bool)
        (te: SignatureTypeEncoder)
        (t: SemType)
        : unit =
        let zt = zonk t

        if tryLeaf te zt then
            ()
        else
            match zt with
            // `TextWriter` / `Formatter` / `HashCode` precede the repr-keyed arm because their names
            // aren't in `reprs`. `unit` is NOT special-cased here: it falls through to the repr arm and
            // encodes off its `prim-types-min` binding (`System.ValueTuple`), keeping a `unit`-mentioning
            // contract BCL-only. `FSharp.Core.Unit` survives only on the cold-printf interop island
            // (`ClrRecipes.encodeFormatParam`, which names `eUnit` explicitly), R9.
            | TyConst("System.IO.TextWriter", _) -> te.Type(eTextWriter.Value, false)
            | TyConst("Vesper.Formatter", _) -> te.Type(eFormatter.Value, true)
            | TyConst("System.HashCode", _) -> te.Type(eHashCode.Value, true)
            // Only scalar (argless) intrinsics rekey off their repr string. A generic
            // intrinsic (the array `[]`, `args ≠ []`) has no `!n`-substituting encoder
            // yet, so it falls through to
            // the catch-all "cannot encode" error — the green suite proves none reaches here.
            | TyConst(name, args) when args.IsEmpty && reprs.ContainsKey name ->
                // Key the IL type off the representation string the name maps to (`"int"` →
                // `"System.Int32"` → `i4`), not the Vesper name (G7).
                let repr = reprs.[name]

                if IntrinsicRepr.tryEncodeValueType te repr then
                    ()
                elif repr = "System.Decimal" then
                    te.Type(eDecimal.Value, true)
                elif repr = "System.ValueTuple" then
                    // `unit` — the zero-field BCL struct; a value type with no external ref of its own.
                    te.Type(eValueTuple.Value, true)
                else
                    failwithf "ClrProvider: no IL encoding for intrinsic representation %s (type %s)" repr name
            // The array intrinsic `[]<elem>` (`'T[]`) → an SZArray (rank-1 vector) of
            // the element. Higher-rank arrays (`[,]`) aren't emitted yet.
            | TyConst("[]", args) when args.Length = 1 -> encodeTypeCore tryLeaf (te.SZArray()) args.[0]
            | TyFun(a, b) ->
                let g = te.GenericInstantiation(eFun2.Value, 2, false)
                encodeTypeCore tryLeaf (g.AddArgument()) a
                encodeTypeCore tryLeaf (g.AddArgument()) b
            | TyClass(key, args) when RuntimeNames.isPrintfFormatKey key ->
                markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
                let g = te.GenericInstantiation(ePrintfFormat4.Value, args.Length, false)

                for a in args do
                    encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(key, args) when RuntimeNames.isFsharpCoreListKey key && args.Length = 1 ->
                let elem = args.[0]
                encodeListOf te (fun arg -> encodeTypeCore tryLeaf arg elem)
            // A nominal is project-local iff its home `asm` is the assembly being
            // emitted (asm-discrimination). This
            // arm precedes the cons-list arm so a *self-host*
            // `Vesper.Collections.List` (asm = the emitted `Vesper.List`) resolves
            // to its emitted `TypeDef`, while a *referenced* cons-list (same key,
            // asm ≠ emitted) falls through to the cached external `eVesperList1`.
            | TyUnion(key, args) when ExternalSymbols.keyAsm key = envAsm ->
                let handle = userTypes.[key]

                if args.IsEmpty then
                    te.Type(handle, false)
                else
                    let g = te.GenericInstantiation(handle, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyUnion(key, args) when RuntimeNames.isVesperListKey key && args.Length = 1 ->
                // The Vesper cons-list (R3) ≡ `Vesper.Collections.List`1<elem>` — no FSharp.Core dep.
                // This arm follows the project-local arm above so a referenced (not self-host)
                // cons-list maps to the cached `eVesperList1` handle directly.
                let elem = args.[0]
                let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
                encodeTypeCore tryLeaf (g.AddArgument()) elem
            | TyRecord(key, args) when ExternalSymbols.keyAsm key = envAsm ->
                let handle = userTypes.[key]

                if args.IsEmpty then
                    te.Type(handle, false)
                else
                    let g = te.GenericInstantiation(handle, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyClass(key, args) when ExternalSymbols.keyAsm key = envAsm ->
                // Checked *before* the external-class arm so a project-local class wins over an
                // accidental same-named external one (asm-discrimination, Phase 6D).
                let handle = userTypes.[key]

                if args.IsEmpty then
                    te.Type(handle, false)
                else
                    let g = te.GenericInstantiation(handle, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | ExternalClass(tref, vt, args) ->
                // `vt` is the `VALUETYPE`-vs-`CLASS` flag from `externalIsValueType`;
                // the lookup + key projection happen once, in the active pattern.
                if args.IsEmpty then
                    te.Type(tref, vt)
                else
                    let g = te.GenericInstantiation(tref, args.Length, vt)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | ExternalRecord(tref, args) ->
                if args.IsEmpty then
                    te.Type(tref, false)
                else
                    let g = te.GenericInstantiation(tref, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | ExternalUnion(tref, args) ->
                // A referenced-package union (`Vesper.Option<int>`) — the case
                // factories' return type and any field typed in the union itself
                // (vesper-lib-test-plan Gap 2 Layer B). Same shape as the external
                // record arm; the union is a reference type, so never `VALUETYPE`.
                if args.IsEmpty then
                    te.Type(tref, false)
                else
                    let g = te.GenericInstantiation(tref, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyUnknown name ->
                // A nominal head that resolved to no in-scope type shape during
                // dependency-aware extraction. The front end refuses it at `unify`
                // with a use-site diagnostic, so it must never reach the backend;
                // this explicit arm makes that boundary self-documenting rather than
                // relying on the catch-all. The message mirrors the unify-time
                // string and flags that the front end should have errored first.
                failwithf
                    "ClrProvider: type '%s' could not be resolved during contract extraction — is a package dependency missing? (reached the backend; the front end should have errored first)"
                    name
            // A frozen open typar (frozen-type-plan): the index is in the node, so
            // encoding is context-free and unconditional — it supersedes the marker
            // `TypeVar`/`TyConst "'A"` mechanism the ambient windows used to resolve.
            // Declaring-axis → the enclosing type's `!i`; Method-axis → the method's
            // own `!!i`. The one exception is a closure body, where the enclosing
            // method's typars are re-projected onto the closure *class* — handled by
            // `closureTyparMode` flipping Method-axis to `GenericTypeParameter`.
            | TempTypar(TyparAxis.Declaring, i) -> te.GenericTypeParameter i
            | TempTypar(TyparAxis.Method, i) ->
                if env.ClosureTyparMode then
                    te.GenericTypeParameter i
                else
                    te.GenericMethodTypeParameter i
            | other -> failwithf "ClrProvider: cannot encode SemType: %A" other

    /// Encode for the executable path. The only leaf hook is the ambient generic-method-typar resolver
    /// (`!!i`), empty except while a generic static method is being emitted.
    and encodeType (te: SignatureTypeEncoder) (t: SemType) : unit = encodeTypeCore methodTyparLeaf te t

    /// Encode mapping each function arrow to FSharp.Core's `FSharpFunc`2` (curried, nested), not
    /// `Vesper.Fun` — for the FSharp.Core interop islands R1 leaves on the old representation (the cold
    /// printf printer, R9). Non-function leaves delegate to `encodeType`.
    let rec encodeFSharpFunc (te: SignatureTypeEncoder) (t: SemType) : unit =
        match zonk t with
        | TyFun(a, b) ->
            markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2"
            let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
            encodeFSharpFunc (g.AddArgument()) a
            encodeFSharpFunc (g.AddArgument()) b
        | TyConst("unit", _) ->
            // This encoder is exclusively the FSharp.Core interop island (the cold-printf printer, R9):
            // FSharp.Core's printf machinery types its result/state slots in `FSharp.Core.Unit`, so a
            // `unit` here must stay `Unit` — NOT the general `System.ValueTuple` the rest of the backend
            // uses (which would mint a `PrintfFunc`4<…,ValueTuple,…>` the runtime can't cast to its
            // `…,Unit,…` factory). The general encoder's `unit` arm resolves to `ValueTuple`.
            markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
            te.Type(eUnit.Value, false)
        | other -> encodeType te other

    /// Recover both open-typar axes by structurally matching a member's *open*
    /// signature — carrying self-describing `TempTypar(axis, i)` nodes (frozen-type-
    /// plan 2C) — against its *instantiated* use-site type. Returns `(declaringArgs,
    /// methodArgs)`, each index-keyed by the `TempTypar`'s own index (no reference
    /// identity, no marker `TypeVar`). First occurrence wins; an unrecovered slot is
    /// a bug.
    let recoverOpenTypars
        (declArity: int)
        (methodArity: int)
        (openT: SemType)
        (instT: SemType)
        : SemType list * SemType list =
        let decl = Array.create declArity ValueNone
        let meth = Array.create methodArity ValueNone

        let rec go (d: SemType) (a: SemType) =
            match zonk d, zonk a with
            | TempTypar(axis, i), act ->
                let slot =
                    match axis with
                    | TyparAxis.Declaring -> decl
                    | TyparAxis.Method -> meth

                if i >= 0 && i < slot.Length && slot.[i].IsNone then
                    slot.[i] <- ValueSome act
            | TyFun(a1, r1), TyFun(a2, r2) ->
                go a1 a2
                go r1 r2
            | TyTuple xs, TyTuple ys when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyRecord(_, xs), TyRecord(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyUnion(_, xs), TyUnion(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyClass(_, xs), TyClass(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyConst(_, xs), TyConst(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | _ -> ()

        go openT instT

        let collect (name: string) (slots: SemType voption[]) =
            [
                for i in 0 .. slots.Length - 1 ->
                    match slots.[i] with
                    | ValueSome t -> t
                    | ValueNone ->
                        failwithf
                            "ClrProvider: could not recover %s type argument %d (open %A vs %A)"
                            name
                            i
                            openT
                            instT
            ]

        collect "declaring" decl, collect "method" meth

    /// The member-ref parent: the declaring `TypeRef`, wrapped in a `TypeSpec` instantiation when
    /// generic (`EqualityComparer`1<int>`).
    let externalTypeSpec (tref: EntityHandle) (instArgs: SemType list) : EntityHandle =
        match instArgs with
        | [] -> tref
        | _ ->
            let tsB = BlobBuilder()
            let te = BlobEncoder(tsB).TypeSpecificationSignature()
            let g = te.GenericInstantiation(tref, List.length instArgs, false)

            for a in instArgs do
                encodeType (g.AddArgument()) (zonk a)

            toEntity (ctx.TypeSpec tsB)

    let encodeLocalSignature (locals: SemType list) : StandaloneSignatureHandle =
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeType (enc.AddVariable().Type()) (zonk t)

        ctx.AddStandaloneSignature blob

    member _.EncodeListOf(te, inner) = encodeListOf te inner
    member _.EncodeType(te, t) = encodeType te t
    member _.EncodeFSharpFunc(te, t) = encodeFSharpFunc te t

    member _.RecoverOpenTypars(declArity, methodArity, openT, instT) =
        recoverOpenTypars declArity methodArity openT instT

    member _.ExternalTypeSpec(tref, instArgs) = externalTypeSpec tref instArgs

    /// A `TypeSpec` token for an arbitrary `SemType`, encoded through the full
    /// `encodeType` path — so a struct external type lands as a `VALUETYPE`
    /// generic-inst (the duck-typed enumerator's member-ref parent, §4.4), unlike
    /// `externalTypeSpec`, which hardcodes the class tag. Mirrors `ClrRecipes.typeToken`.
    member _.TypeSpecOf(ty: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeType te (zonk ty)
        toEntity (ctx.TypeSpec tsB)

    member _.EncodeLocalSignature locals = encodeLocalSignature locals

    /// `instance void .ctor(fields…)` for a record / generic-type ctor. Field types
    /// carry their declaring typars as `TempTypar(Declaring, i)` nodes the encoder
    /// resolves to `!i` directly — no marker map.
    member _.RecordCtorSignature(paramTys: SemType list) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    /// A *generic method* (B-12: `member this.Map<'C> …`) whose body may also be inside
    /// a generic type. The declaring type's typars ride `TempTypar(Declaring, i)` nodes
    /// (`!i`); the method's own typars are still `TyVar` roots that `methodTyparLeaf`
    /// resolves to `GenericMethodParameter` (`!!i`) — so the caller MUST install them
    /// via `SetMethodTypars` first. `methodTyparCount` sets the `GENERIC` calling-
    /// convention header count.
    member _.GenericMethodOnTypeSignature
        (methodTyparCount: int, paramTys: SemType list, retTy: SemType, isInstanceMethod: bool)
        : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = methodTyparCount, isInstanceMethod = isInstanceMethod)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    member _.NullaryCtorSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

        s

    member _.CctorSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

        s

    member _.GenericStaticFnSignature(typarCount: int, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = typarCount, isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    member _.StaticMethodSignature(paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    member _.InstanceMethodSignature(paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    /// `instance b Invoke(a)` — the closure's concrete `Invoke` override signature.
    member _.InvokeSignature(a: SemType, b: SemType) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) b),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) a)
            )

        msig

    /// `instance void .ctor(captures…)` — one concrete parameter per captured value (in field order).
    member _.ClosureCtorSignature(captures: SemType list) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length captures,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for c in captures do
                        encodeType (pars.AddParameter().Type()) c
                )
            )

        msig

    member _.FieldSignature(ty: SemType) : BlobBuilder =
        let blob = BlobBuilder()
        let te = BlobEncoder(blob).FieldSignature()
        encodeType te ty
        blob

    /// `override bool Equals(object)` signature. The parameter is the compact `ELEMENT_TYPE_OBJECT`
    /// encoding (`.Object()`), not `class System.Object` — `System.Object::Equals(object)` uses the
    /// compact form and implicit override binding is by signature *blob* match, so the encodings must
    /// agree (else the method lands in a new vtable slot instead of overriding).
    member _.EqualsOverrideSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Boolean()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Object())
            )

        s

    member _.GetHashCodeOverrideSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()), (fun (_: ParametersEncoder) -> ()))

        s

    /// `instance bool Equals(Self)` — the typed `IEquatable<Self>::Equals` signature; implicit
    /// interface binding matches it to the instantiated `IEquatable<Self>::Equals(!0)`.
    member _.EqualsTypedSignature(selfTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Boolean()),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) (zonk selfTy))
            )

        s

    /// `override int32 CompareTo(object)` signature — `.Object()` to match how
    /// `IComparable::CompareTo(object)` is declared (signature-blob match).
    member _.CompareToOverrideSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Object())
            )

        s

    member _.CompareToTypedSignature(selfTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) (zonk selfTy))
            )

        s

    /// Encode an abstract interface-method signature (the library path, G5). The signature's open
    /// typars are self-describing `TempTypar` nodes — `Declaring` → `!i`, `Method` → `!!j` — that the
    /// structural `encodeType` match resolves directly.
    member _.EncodeAbstractType(te: SignatureTypeEncoder, t: SemType) : unit = encodeType te t
