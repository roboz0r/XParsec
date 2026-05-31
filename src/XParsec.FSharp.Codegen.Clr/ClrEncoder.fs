namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// The signature-type encoder over a `ClrEnv`: turns a `SemType` into a metadata signature, mapping
/// function arrows to `Vesper.Fun`2`, lists to `FSharpList`1` / `Vesper.Collections.List`1`, user
/// types to their predicted `TypeDefinition`, and external types through the symbol provider. Also
/// hosts the blob/signature builders that are pure `encodeType` / `encodeUnionType` wrappers.
type internal ClrEncoder(env: ClrEnv) =
    let ctx = env.Ctx
    let reprs = env.Reprs
    let markFSharpCoreDep c = env.MarkFSharpCoreDep c
    let userTypes = env.UserTypes
    let listTypeName = env.ListTypeName
    let isVesperListName n = env.IsVesperListName n
    let zonk t = env.Zonk t
    let externalClassRef n = env.ExternalClassRef n
    let externalIsValueType n = env.ExternalIsValueType n
    let externalRecordRef (n, a) = env.ExternalRecordRef(n, a)
    let externalUnionRef (n, a) = env.ExternalUnionRef(n, a)
    let ambientTyparLeaf = env.AmbientTyparLeaf
    let methodTyparLeaf = env.MethodTyparLeaf
    let typarIx typars = env.TyparIx typars

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
            | TyConst "System.IO.TextWriter" -> te.Type(eTextWriter.Value, false)
            | TyConst "Vesper.Formatter" -> te.Type(eFormatter.Value, true)
            | TyConst "System.HashCode" -> te.Type(eHashCode.Value, true)
            | TyConst name when reprs.ContainsKey name ->
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
            | TyFun(a, b) ->
                let g = te.GenericInstantiation(eFun2.Value, 2, false)
                encodeTypeCore tryLeaf (g.AddArgument()) a
                encodeTypeCore tryLeaf (g.AddArgument()) b
            | TyClass(name, args) when name = PrintfSpec.printfFormatName ->
                markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
                let g = te.GenericInstantiation(ePrintfFormat4.Value, args.Length, false)

                for a in args do
                    encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(name, args) when name = listTypeName && args.Length = 1 ->
                let elem = args.[0]
                encodeListOf te (fun arg -> encodeTypeCore tryLeaf arg elem)
            | TyRecord(name, args) when isVesperListName name && args.Length = 1 ->
                // The Vesper cons-list (R3) ≡ `Vesper.Collections.List`1<elem>` — no FSharp.Core dep.
                // The abbreviation name reaches here from the contract-typed literal, the union name
                // from the self-host path; both map to the same `List`1`.
                let elem = args.[0]
                let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
                encodeTypeCore tryLeaf (g.AddArgument()) elem
            | TyUnion(name, args) when userTypes.ContainsKey name ->
                if args.IsEmpty then
                    te.Type(userTypes.[name], false)
                else
                    let g = te.GenericInstantiation(userTypes.[name], args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(name, args) when userTypes.ContainsKey name ->
                if args.IsEmpty then
                    te.Type(userTypes.[name], false)
                else
                    let g = te.GenericInstantiation(userTypes.[name], args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyClass(name, args) when userTypes.ContainsKey name ->
                // Checked *before* the external-class arm so a project-local class wins over an
                // accidental same-named external one.
                if args.IsEmpty then
                    te.Type(userTypes.[name], false)
                else
                    let g = te.GenericInstantiation(userTypes.[name], args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyClass(name, args) when (externalClassRef name).IsSome ->
                let tref = (externalClassRef name).Value
                // A struct external type (`List`1+Enumerator`, §4.4) must encode as a
                // `VALUETYPE` element, not a class ref; every reference type stays
                // `false` (the cached `TryLookupType` is cheap on the hot encoder arm).
                let vt = externalIsValueType name

                if args.IsEmpty then
                    te.Type(tref, vt)
                else
                    let g = te.GenericInstantiation(tref, args.Length, vt)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(name, args) when (externalRecordRef (name, args.Length)).IsSome ->
                let tref, _ = (externalRecordRef (name, args.Length)).Value

                if args.IsEmpty then
                    te.Type(tref, false)
                else
                    let g = te.GenericInstantiation(tref, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyUnion(name, args) when (externalUnionRef (name, args.Length)).IsSome ->
                // A referenced-package union (`Vesper.Option<int>`) — the case
                // factories' return type and any field typed in the union itself
                // (vesper-lib-test-plan Gap 2 Layer B). Same shape as the external
                // record arm; the union is a reference type, so never `VALUETYPE`.
                let tref, _ = (externalUnionRef (name, args.Length)).Value

                if args.IsEmpty then
                    te.Type(tref, false)
                else
                    let g = te.GenericInstantiation(tref, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | other -> failwithf "ClrProvider: cannot encode SemType: %A" other

    /// Encode for the executable path. The only leaf hook is the ambient generic-method-typar resolver
    /// (`!!i`), empty except while a generic static method is being emitted.
    and encodeType (te: SignatureTypeEncoder) (t: SemType) : unit = encodeTypeCore ambientTyparLeaf te t

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
        | TyConst "unit" ->
            // This encoder is exclusively the FSharp.Core interop island (the cold-printf printer, R9):
            // FSharp.Core's printf machinery types its result/state slots in `FSharp.Core.Unit`, so a
            // `unit` here must stay `Unit` — NOT the general `System.ValueTuple` the rest of the backend
            // uses (which would mint a `PrintfFunc`4<…,ValueTuple,…>` the runtime can't cast to its
            // `…,Unit,…` factory). The general encoder's `unit` arm resolves to `ValueTuple`.
            markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
            te.Type(eUnit.Value, false)
        | other -> encodeType te other

    /// Encode a `SemType` written in the declaring type's *open* typars: a marker `TypeVar` (one of
    /// `markerRoots`) maps to its `GenericTypeParameter` index; every other leaf delegates to the
    /// structural encoder.
    let encodeOpen (markerRoots: TypeVar list) (te: SignatureTypeEncoder) (t: SemType) : unit =
        let tryLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
            match zt with
            | TyVar tv ->
                let r = UnionFind.find tv

                match markerRoots |> List.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                | Some i ->
                    te.GenericTypeParameter i
                    true
                | None -> false
            | _ -> false

        encodeTypeCore tryLeaf te t

    /// Recover the declaring type's instantiation by structurally matching the member's *open*
    /// signature (carrying the marker `TypeVar`s) against its *instantiated* type at the use site.
    /// First occurrence wins; an unmatched marker is a bug.
    let recoverTypeArgs (markerRoots: TypeVar list) (openT: SemType) (instT: SemType) : SemType list =
        let result = Array.create (List.length markerRoots) ValueNone

        let rec go (d: SemType) (a: SemType) =
            match zonk d, zonk a with
            | TyVar tv, act ->
                let r = UnionFind.find tv

                match markerRoots |> List.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                | Some i ->
                    if result.[i].IsNone then
                        result.[i] <- ValueSome act
                | None -> ()
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
            | _ -> ()

        go openT instT

        [
            for i in 0 .. result.Length - 1 ->
                match result.[i] with
                | ValueSome t -> t
                | ValueNone ->
                    failwithf "ClrProvider: could not recover external type argument %d (open %A vs %A)" i openT instT
        ]

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

    /// Encode a `SemType` declared *within* a generic union/record/class (a field type, a factory
    /// parameter/return): its own typar markers (`TyConst "'T"`) resolve to `GenericTypeParameter`
    /// indices, everything else delegates to `encodeType`.
    let encodeUnionType (typeIx: Map<string, int>) (te: SignatureTypeEncoder) (t: SemType) : unit =
        let tryLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
            match zt with
            | TyConst name when typeIx.ContainsKey name ->
                te.GenericTypeParameter(typeIx.[name])
                true
            // A `List<!!i>` member-ref instantiation arg inside a generic static method body resolves
            // the method's typar `TypeVar`s to `!!i` (R3); empty otherwise.
            | _ -> methodTyparLeaf te zt

        encodeTypeCore tryLeaf te t

    let encodeLocalSignature (locals: SemType list) : StandaloneSignatureHandle =
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeType (enc.AddVariable().Type()) (zonk t)

        ctx.AddStandaloneSignature blob

    member _.EncodeListOf(te, inner) = encodeListOf te inner
    member _.EncodeTypeCore(tryLeaf, te, t) = encodeTypeCore tryLeaf te t
    member _.EncodeType(te, t) = encodeType te t
    member _.EncodeFSharpFunc(te, t) = encodeFSharpFunc te t
    member _.EncodeOpen(markerRoots, te, t) = encodeOpen markerRoots te t
    member _.RecoverTypeArgs(markerRoots, openT, instT) = recoverTypeArgs markerRoots openT instT
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

    member _.EncodeUnionType(typeIx, te, t) = encodeUnionType typeIx te t
    member _.EncodeLocalSignature locals = encodeLocalSignature locals

    /// `<field-type>` field signature for a generic union case field, in the type's own typars.
    member _.GenericFieldSignature(typars: string list, declTy: SemType) : BlobBuilder =
        let blob = BlobBuilder()
        encodeUnionType (typarIx typars) (BlobEncoder(blob).FieldSignature()) declTy
        blob

    member _.GenericStaticMethodSignature(typars: string list, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let typeIx = typarIx typars
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeUnionType typeIx (pars.AddParameter().Type()) p
                )
            )

        s

    member _.GenericRecordCtorSignature(typars: string list, paramTys: SemType list) : BlobBuilder =
        let typeIx = typarIx typars
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeUnionType typeIx (pars.AddParameter().Type()) p
                )
            )

        s

    member _.GenericInstanceMethodSignature(typars: string list, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let typeIx = typarIx typars
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeUnionType typeIx (pars.AddParameter().Type()) p
                )
            )

        s

    /// A *generic method* (B-12: `member this.Map<'C> …`) whose body may also be
    /// inside a generic type. The declaring type's typars resolve to
    /// `GenericTypeParameter` (`!i`) via `typeTypars`; the method's own typars are
    /// `TypeVar` roots that `methodTyparLeaf` resolves to `GenericMethodParameter`
    /// (`!!i`) — so the caller MUST install them via `SetMethodTypars` first.
    /// `methodTyparCount` sets the `GENERIC` calling-convention header count.
    /// `typeTypars` is empty for a generic method on a monomorphic class.
    member _.GenericMethodOnTypeSignature
        (typeTypars: string list, methodTyparCount: int, paramTys: SemType list, retTy: SemType, isInstanceMethod: bool)
        : BlobBuilder =
        let typeIx = typarIx typeTypars
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = methodTyparCount, isInstanceMethod = isInstanceMethod)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeUnionType typeIx (pars.AddParameter().Type()) p
                )
            )

        s

    member _.EncodeGenericLocalSignature(typars: string list, locals: SemType list) : StandaloneSignatureHandle =
        let typeIx = typarIx typars
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeUnionType typeIx (enc.AddVariable().Type()) (zonk t)

        ctx.AddStandaloneSignature blob

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

    /// Encode an abstract interface-method signature leaf (the library path, G5). A typar marker
    /// resolves to a positional generic parameter — the method's own typars (`methodIx`) shadow the
    /// declaring type's (`typeIx`), so they are tried first — and every other (concrete) leaf delegates
    /// to `encodeType`. Recurses through structural types, intercepting typars at every depth.
    member _.EncodeAbstractType
        (typeIx: Map<string, int>, methodIx: Map<string, int>, te: SignatureTypeEncoder, t: SemType)
        : unit =
        let tryLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
            match zt with
            | TyConst name when methodIx.ContainsKey name ->
                te.GenericMethodTypeParameter(methodIx.[name])
                true
            | TyConst name when typeIx.ContainsKey name ->
                te.GenericTypeParameter(typeIx.[name])
                true
            | _ -> false

        encodeTypeCore tryLeaf te t
