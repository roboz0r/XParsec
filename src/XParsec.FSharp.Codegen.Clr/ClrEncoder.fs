namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// The signature-type encoder over a `ClrEnv`: turns a `FrozenType` into a metadata signature, mapping
/// function arrows to `Vesper.Fun`2`, lists to `FSharpList`1` / `Vesper.Collections.List`1`, user
/// types to their predicted `TypeDefinition`, and external types through the symbol provider. Also
/// hosts the blob/signature builders that are pure `encodeType` wrappers.
type internal ClrEncoder(env: ClrEnv) =
    let ctx = env.Ctx
    let markFSharpCoreDep c = env.MarkFSharpCoreDep c
    let userTypes = env.UserTypes
    let userValueTypes = env.UserValueTypes
    let envAsm = env.EnvAsm

    let externalClassRef key = env.ExternalClassRef key
    let externalIsValueType key = env.ExternalIsValueType key
    let externalRecordRef (key, a) = env.ExternalRecordRef(key, a)
    let externalUnionRef (key, a) = env.ExternalUnionRef(key, a)

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

    /// Single-sourced primitive repr (own intrinsics → provider-harvested `.fs`, no
    /// fallback), as an active pattern over an `FTConst` name. See
    /// `ClrEnv.TryPrimitiveRepr`.
    let (|PrimitiveRepr|_|) (name: string) = env.TryPrimitiveRepr name

    // `ValueTuple`n` handle bundles, cached by element-type list. Unlike `ctx.TypeRef`
    // (which dedups its rows), `ctx.TypeSpec` / `ctx.MemberRef` add a fresh metadata
    // row per call, so without this every tuple occurrence — even the two sites of a
    // single `let (a, b) = (1, 2)` — would mint duplicate TypeSpec / ctor / field rows.
    // `FrozenType list` keys on real structural equality (EqArray hashes its contents),
    // not `%A` (see reference_eqarray_percentA_cache_key).
    let valueTupleRefsCache =
        Dictionary<FrozenType list, ValueTupleHandles>(HashIdentity.Structural)

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
    let (|ExternalClass|_|) (t: FrozenType) =
        match t with
        | FTClass(key, args) ->
            match externalClassRef key with
            // A struct external type (`List`1+Enumerator`) must encode as a
            // `VALUETYPE` element; every reference type stays `false`.
            | ValueSome tref -> Some(tref, externalIsValueType key, args)
            | ValueNone -> None
        | _ -> None

    let (|ExternalRecord|_|) (t: FrozenType) =
        match t with
        | FTRecord(key, args) ->
            match externalRecordRef (key, args.Length) with
            | ValueSome(tref, _) -> Some(tref, args)
            | ValueNone -> None
        | _ -> None

    let (|ExternalUnion|_|) (t: FrozenType) =
        match t with
        | FTUnion(key, args) ->
            match externalUnionRef (key, args.Length) with
            | ValueSome(tref, _) -> Some(tref, args)
            | ValueNone -> None
        | _ -> None

    /// Encode a `FrozenType` into a metadata signature slot. Encoding is
    /// context-free: open typars are self-describing `FTTypar(axis, i)` nodes the
    /// structural match resolves by index, so there
    /// is no ambient typar window and no leaf hook — the match is total over the frozen
    /// type shapes that reach the backend.
    let rec encodeType (te: SignatureTypeEncoder) (t: FrozenType) : unit =
        match t with
        // `TextWriter` / `Formatter` / `HashCode` precede the repr-keyed arm because their names
        // aren't in `reprs`. `unit` is NOT special-cased here: it falls through to the repr arm and
        // encodes off its `prim-types-min` binding (`System.ValueTuple`), keeping a `unit`-mentioning
        // contract BCL-only. `FSharp.Core.Unit` survives only on the cold-printf interop island
        // (`ClrRecipes.encodeFormatParam`, which names `eUnit` explicitly).
        // `obj` → `ELEMENT_TYPE_OBJECT`, not `class System.Object`. This matters
        // for interface-impl / override matching: an interface method declared as
        // `CompareTo(object)` / `GetEnumerator()`-returning-`object` is encoded by
        // the BCL metadata with the primitive `object` token, so a user member
        // implementing it must match that encoding (see the `.Object()` recipes the
        // synthesised structural-equality triple already uses).
        | FTConst(n, _) when n = RuntimeNames.objAbbrevName -> te.Object()
        // `System.Object` arriving as an external CLASS — a BCL method's `object`
        // parameter read from metadata as a class `TypeRef` rather than the primitive
        // `obj` (e.g. `IEqualityComparer.GetHashCode(object)` /
        // `IEqualityComparer.Equals(object, object)`, called by `set.fs`'s
        // `IStructuralEquatable` members on a boxed `'T`). It must ALSO encode as the
        // compact `ELEMENT_TYPE_OBJECT`, not `class System.Object`: the BCL signature
        // uses the primitive token, so a member-ref whose parameter is `class
        // System.Object` fails signature match at JIT time (`MissingMethodException`).
        // Mirrors the `obj` arm + the `.Object()` override recipes.
        | FTClass(key, _) when RuntimeNames.isSystemObjectKey key -> te.Object()
        | FTConst("System.IO.TextWriter", _) -> te.Type(eTextWriter.Value, false)
        | FTConst("Vesper.Formatter", _) -> te.Type(eFormatter.Value, true)
        | FTConst("System.HashCode", _) -> te.Type(eHashCode.Value, true)
        // Only scalar (argless) intrinsics rekey off their repr string. A generic
        // intrinsic (the array `[]`, `args ≠ []`) has no `!n`-substituting encoder
        // yet, so it falls through to
        // the catch-all "cannot encode" error — the green suite proves none reaches here.
        | FTConst((name & PrimitiveRepr repr), args) when args.IsEmpty ->
            // Key the IL type off the representation string the name maps to (`"int"` →
            // `"System.Int32"` → `i4`), not the Vesper name.
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
        | FTConst("[]", args) when args.Length = 1 -> encodeType (te.SZArray()) args.[0]
        | FTFun(a, b) ->
            let g = te.GenericInstantiation(eFun2.Value, 2, false)
            encodeType (g.AddArgument()) a
            encodeType (g.AddArgument()) b
        | FTClass(key, args) when RuntimeNames.isPrintfFormatKey key ->
            markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
            let g = te.GenericInstantiation(ePrintfFormat4.Value, args.Length, false)

            for a in args do
                encodeType (g.AddArgument()) a
        | FTRecord(key, args) when RuntimeNames.isFsharpCoreListKey key && args.Length = 1 ->
            let elem = args.[0]
            encodeListOf te (fun arg -> encodeType arg elem)
        // A nominal is project-local iff its home `asm` is the assembly being
        // emitted (asm-discrimination). This
        // arm precedes the cons-list arm so a *self-host*
        // `Vesper.Collections.List` (asm = the emitted `Vesper.List`) resolves
        // to its emitted `TypeDef`, while a *referenced* cons-list (same key,
        // asm ≠ emitted) falls through to the cached external `eVesperList1`.
        | FTUnion(key, args) when SymbolKeyOps.keyAsm key = envAsm ->
            let handle = userTypes.[key]

            if args.IsEmpty then
                te.Type(handle, false)
            else
                let g = te.GenericInstantiation(handle, args.Length, false)

                for a in args do
                    encodeType (g.AddArgument()) a
        | FTUnion(key, args) when RuntimeNames.isVesperListKey key && args.Length = 1 ->
            // The Vesper cons-list ≡ `Vesper.Collections.List`1<elem>` — no FSharp.Core dep.
            // This arm follows the project-local arm above so a referenced (not self-host)
            // cons-list maps to the cached `eVesperList1` handle directly.
            let elem = args.[0]
            let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
            encodeType (g.AddArgument()) elem
        | FTRecord(key, args) when SymbolKeyOps.keyAsm key = envAsm ->
            let handle = userTypes.[key]

            if args.IsEmpty then
                te.Type(handle, false)
            else
                let g = te.GenericInstantiation(handle, args.Length, false)

                for a in args do
                    encodeType (g.AddArgument()) a
        | FTClass(key, args) when SymbolKeyOps.keyAsm key = envAsm ->
            // Checked *before* the external-class arm so a project-local class wins over an
            // accidental same-named external one (asm-discrimination).
            let handle = userTypes.[key]
            // A `[<Struct>]` value type must encode as `ELEMENT_TYPE_VALUETYPE`
            // so a signature referencing it matches the value-type `TypeDefinition`;
            // a plain class is `ELEMENT_TYPE_CLASS`.
            let isVt = userValueTypes.Contains key

            if args.IsEmpty then
                te.Type(handle, isVt)
            else
                let g = te.GenericInstantiation(handle, args.Length, isVt)

                for a in args do
                    encodeType (g.AddArgument()) a
        | ExternalClass(tref, vt, args) ->
            // `vt` is the `VALUETYPE`-vs-`CLASS` flag from `externalIsValueType`;
            // the lookup + key projection happen once, in the active pattern.
            if args.IsEmpty then
                te.Type(tref, vt)
            else
                let g = te.GenericInstantiation(tref, args.Length, vt)

                for a in args do
                    encodeType (g.AddArgument()) a
        | ExternalRecord(tref, args) ->
            if args.IsEmpty then
                te.Type(tref, false)
            else
                let g = te.GenericInstantiation(tref, args.Length, false)

                for a in args do
                    encodeType (g.AddArgument()) a
        | ExternalUnion(tref, args) ->
            // A referenced-package union (`Vesper.Option<int>`) — the case
            // factories' return type and any field typed in the union itself.
            // Same shape as the external record arm; the union is a reference
            // type, so never `VALUETYPE`.
            if args.IsEmpty then
                te.Type(tref, false)
            else
                let g = te.GenericInstantiation(tref, args.Length, false)

                for a in args do
                    encodeType (g.AddArgument()) a
        | FTUnknown name ->
            // A nominal head that resolved to no in-scope type shape during
            // dependency-aware extraction. The front end refuses it at `unify`
            // with a use-site diagnostic, so it must never reach the backend;
            // this explicit arm makes that boundary self-documenting rather than
            // relying on the catch-all. The message mirrors the unify-time
            // string and flags that the front end should have errored first.
            failwithf
                "ClrProvider: type '%s' could not be resolved during contract extraction — is a package dependency missing? (reached the backend; the front end should have errored first)"
                name
        // A frozen open typar: the index is in the node, so
        // encoding is context-free and unconditional — it supersedes the marker
        // `TypeVar`/`TyConst "'A"` mechanism the ambient windows used to resolve.
        // Declaring-axis → the enclosing type's `!i`; Method-axis → the method's
        // own `!!i`. The one exception is a closure body, where the enclosing
        // method's typars are re-projected onto the closure *class* — handled by
        // `closureTyparMode` flipping Method-axis to `GenericTypeParameter`.
        | FTTypar(TyparAxis.Declaring, i) -> te.GenericTypeParameter i
        | FTTypar(TyparAxis.Method, i) ->
            match env.ClosureTyparScope with
            // Inside a closure's own emission, the enclosing class typars occupy
            // the closure's first `d` slots, so a method-axis typar lands at
            // `!(d + i)` (a static-fn closure has `d = 0`, so `!i`).
            | ValueSome d -> te.GenericTypeParameter(d + i)
            | ValueNone -> te.GenericMethodTypeParameter i
        // A tuple is the arity-N member of the `System.ValueTuple` struct family,
        // a `VALUETYPE` generic instantiation (the `true` flag). `unit` never
        // reaches here (it encodes off its intrinsic repr above). Arity ≤ 7 is the
        // flat `ValueTuple`n`; arity ≥ 8 packs slots 0–6 then nests the residual
        // tail in `ValueTuple`8`'s 8th arg (`TRest`) — the standard .NET scheme,
        // index 7 is Rest. Recurse by offset into `items` to stay allocation-free.
        | FTTuple items ->
            let rec encodeFrom (out: SignatureTypeEncoder) (start: int) =
                let remaining = items.Length - start

                if remaining <= 7 then
                    let g = out.GenericInstantiation(env.EValueTupleN remaining, remaining, true)

                    for i in start .. items.Length - 1 do
                        encodeType (g.AddArgument()) items.[i]
                else
                    let g = out.GenericInstantiation(env.EValueTupleN 8, 8, true)

                    for i in start .. start + 6 do
                        encodeType (g.AddArgument()) items.[i]

                    encodeFrom (g.AddArgument()) (start + 7)

            encodeFrom te 0
        // A by-ref (`T&`) is legal only in parameter / return / local position,
        // where its `ELEMENT_TYPE_BYREF` prefix is emitted at the encoder seam
        // (`mintMemberRef`'s return encoder, the local-sig encoder). Reaching the
        // recursive type encoder means it appears as a field / generic argument —
        // illegal in CLR metadata — so flag it explicitly rather than via the opaque
        // catch-all.
        | FTConst(n, _) when n = RuntimeNames.byrefName ->
            failwithf
                "ClrProvider: by-ref type '%A' in a non-param/return position (illegal as a field or generic argument)"
                t
        // A project-local enum emitted into *this* assembly. It is always a value
        // type — a NUMERIC enum is a `System.Enum` subclass; a STRING/MIXED
        // enum is a `[<Struct>]` wrapper over `System.ValueType`. Either
        // encodes `ELEMENT_TYPE_VALUETYPE` off its emitted `TypeDefinition` handle —
        // the same shape as a project-local `[<Struct>]` class, minus generic args
        // (enums are never generic). All three variants register into `userTypes`
        // (`Layout` partitions numeric → `Enums`, string/mixed → `StructEnums`); an
        // external enum is absent and falls to the loud arm below.
        | FTEnum key when SymbolKeyOps.keyAsm key = envAsm && userTypes.ContainsKey key ->
            te.Type(userTypes.[key], true)
        | FTEnum _ ->
            failwithf
                "ClrProvider: cannot encode enum type reference %A — project-local enums only; external (TS-manifest) enums are a JS-target concern, unsupported on CLR"
                t
        // A structural literal ERASES to its base primitive on both backends (the
        // runtime value already IS the literal) — re-encode as that primitive rather
        // than hit the catch-all. External-vocabulary only (a TS/JS concern), so a
        // literal rarely reaches the CLR encoder, but erasing keeps it honest.
        | FTLiteral v -> encodeType te (FTConst(v.BaseName, EqArray.empty))
        // A carried type-level computation (keyof / indexed-access / conditional) is a
        // JS-seam construct that must be GROUND-EVALUATED by the front end (step 3)
        // before codegen — it has no CLR runtime repr in its unevaluated form. Like the
        // external-enum arm above, reaching the CLR encoder with one is unsupported.
        | FTKeyOf _
        | FTIndexedAccess _
        | FTConditional _ ->
            failwithf
                "ClrProvider: cannot encode unevaluated type-level computation %A — keyof/indexed-access/conditional are a JS-target concern and must be ground-evaluated before CLR emit"
                t
        // The residual case — a stray `TyVar` can no longer reach here (it fails one
        // hop out in `toFrozen`) — is unencodable.
        | other -> failwithf "ClrProvider: cannot encode FrozenType: %A" other

    /// Encode mapping each function arrow to FSharp.Core's `FSharpFunc`2` (curried, nested), not
    /// `Vesper.Fun` — for the FSharp.Core interop islands that leave the old representation (the cold
    /// printf printer). Non-function leaves delegate to `encodeType`.
    let rec encodeFSharpFunc (te: SignatureTypeEncoder) (t: FrozenType) : unit =
        match t with
        | FTFun(a, b) ->
            markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2"
            let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
            encodeFSharpFunc (g.AddArgument()) a
            encodeFSharpFunc (g.AddArgument()) b
        | FTConst("unit", _) ->
            // This encoder is exclusively the FSharp.Core interop island (the cold-printf printer):
            // FSharp.Core's printf machinery types its result/state slots in `FSharp.Core.Unit`, so a
            // `unit` here must stay `Unit` — NOT the general `System.ValueTuple` the rest of the backend
            // uses (which would mint a `PrintfFunc`4<…,ValueTuple,…>` the runtime can't cast to its
            // `…,Unit,…` factory). The general encoder's `unit` arm resolves to `ValueTuple`.
            markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
            te.Type(eUnit.Value, false)
        | other -> encodeType te other

    /// Recover both open-typar axes by structurally matching a member's *open*
    /// signature template — carrying self-describing `FTTypar(axis, i)` nodes
    /// against its *instantiated* use-site type (already ground, so
    /// frozen at the boundary). Returns `(declaringArgs, methodArgs)`, each index-keyed
    /// by the `FTTypar`'s own index. First occurrence wins; an unrecovered slot is a
    /// bug. The recovered slices stay `FrozenType` — the emit walk is `FrozenType`-native
    /// end to end.
    let recoverOpenTypars
        (declArity: int)
        (methodArity: int)
        (openT: FrozenType)
        (instT: FrozenType)
        : FrozenType list * FrozenType list =
        let decl = Array.create declArity ValueNone
        let meth = Array.create methodArity ValueNone

        let rec go (d: FrozenType) (a: FrozenType) =
            match d with
            | FTTypar(axis, i) ->
                let slot =
                    match axis with
                    | TyparAxis.Declaring -> decl
                    | TyparAxis.Method -> meth

                if i >= 0 && i < slot.Length && slot.[i].IsNone then
                    slot.[i] <- ValueSome a
            // Same-head pairwise descent (`iterChildren2`): a head mismatch declines
            // silently (no recovery from that subtree); `collect` below fails loud on
            // any slot left empty. See `iterChildren2`'s doc for the `FTOr` pairing
            // (positional when heads line up, head-keyed fallback otherwise).
            | d -> FrozenType.iterChildren2 go d a

        go openT instT

        let collect (name: string) (slots: FrozenType voption[]) =
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
    /// generic (`EqualityComparer`1<int>`). `isValueType` selects the `VALUETYPE` vs `CLASS` element
    /// tag of the generic-inst — a `Span`1<char>` / struct-union / struct-record declaring type must
    /// be tagged `VALUETYPE` or the runtime rejects the member ref ("value type mismatch").
    let externalTypeSpec (tref: EntityHandle) (isValueType: bool) (instArgs: FrozenType list) : EntityHandle =
        match instArgs with
        | [] -> tref
        | _ ->
            let tsB = BlobBuilder()
            let te = BlobEncoder(tsB).TypeSpecificationSignature()
            let g = te.GenericInstantiation(tref, List.length instArgs, isValueType)

            for a in instArgs do
                encodeType (g.AddArgument()) (a)

            toEntity (ctx.TypeSpec tsB)

    let encodeLocalSignature (locals: FrozenType list) : StandaloneSignatureHandle =
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeType (enc.AddVariable().Type()) (t)

        ctx.AddStandaloneSignature blob

    /// Wrap a generic-method handle in a `MethodSpec` instantiating it at `args`
    /// (`fold<int,int>`, `Enumerable.Take<int>`). A non-generic handle (`args = []`)
    /// returns unchanged. The single home for the `MethodSpecificationSignature` blob
    /// shape every generic-method call site shares.
    let methodSpec (handle: EntityHandle) (args: FrozenType list) : EntityHandle =
        match args with
        | [] -> handle
        | _ ->
            let inst = BlobBuilder()
            let specEnc = BlobEncoder(inst).MethodSpecificationSignature(List.length args)

            for t in args do
                encodeType (specEnc.AddArgument()) (t)

            toEntity (ctx.MethodSpec(handle, inst))

    member _.EncodeListOf(te, inner) = encodeListOf te inner
    member _.EncodeType(te, t: FrozenType) = encodeType te t

    member _.EncodeFSharpFunc(te, t) = encodeFSharpFunc te t

    member _.RecoverOpenTypars(declArity, methodArity, openT, instT) =
        recoverOpenTypars declArity methodArity openT instT

    member _.MethodSpec(handle, args) = methodSpec handle args

    member _.ExternalTypeSpec(tref, isValueType, instArgs) =
        externalTypeSpec tref isValueType instArgs

    /// A `TypeSpec` token for an arbitrary `FrozenType`, encoded through the full
    /// `encodeType` path — so a struct external type lands as a `VALUETYPE`
    /// generic-inst (the duck-typed enumerator's member-ref parent). Equivalent
    /// to `externalTypeSpec` once its caller supplies the right `isValueType`, but
    /// reads the tag straight off the type rather than from a separate flag. Mirrors
    /// `ClrRecipes.typeToken`.
    member _.TypeSpecOf(ty: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeType te ty
        toEntity (ctx.TypeSpec tsB)

    /// Resolve the `System.ValueTuple` handles for an N-tuple whose element types
    /// are `elemTys` — the single source of truth for "the .NET handles of a
    /// tuple", shared by construction and destructuring. The ctor / `Item` field
    /// signatures are element-type-independent — they name the type's own `!0…` —
    /// so only the parent `TypeSpec` carries the call-site instantiation.
    ///
    /// Arity ≤ 7 is the flat `ValueTuple`n`. Arity ≥ 8 packs slots 0–6 directly
    /// and nests the residual tail in `ValueTuple`8`'s `TRest` (8th arg), built
    /// recursively — the standard .NET scheme, index 7 is Rest.
    member _.ValueTupleRefs(elemTys: FrozenType list) : ValueTupleHandles =
        // A user-level tuple is arity ≥ 2 (`unit` and `(x)` are not tuples). The
        // `ValueTuple`1` family member is reachable only as an internal `TRest`
        // tail in the recursion below, never as a top-level request.
        if List.length elemTys < 2 then
            failwithf
                "ClrProvider: ValueTupleRefs needs arity ≥ 2, got %d (unit / 1-tuples are not tuple values)."
                (List.length elemTys)

        match valueTupleRefsCache.TryGetValue elemTys with
        | true, cached -> cached
        | _ ->
            let rec build (elems: FrozenType[]) : ValueTupleHandles =
                let n = elems.Length
                // The generic family member that *directly* holds these elements:
                // the flat `ValueTuple`n` for n ≤ 7, else `ValueTuple`8` (slots
                // 0–6 + a nested `TRest`).
                let k = if n <= 7 then n else 8

                // The full (possibly nested) tuple type, doubling as the member-ref
                // parent `TypeSpec`. `encodeType`'s `FTTuple` arm does the recursive
                // `TRest` nesting, so this is the genuine CLR type of the tuple.
                let typeSpec =
                    let tsB = BlobBuilder()
                    let te = BlobEncoder(tsB).TypeSpecificationSignature()
                    encodeType te (FTTuple(EqArray.ofSeq elems))
                    toEntity (ctx.TypeSpec tsB)

                // `instance void .ctor(!0…!{k-1})` — for k = 8 the last param `!7`
                // is the nested `TRest` value. Pushing the args then `newobj` this
                // leaves the struct on the stack.
                let ctorRef =
                    let s = BlobBuilder()

                    BlobEncoder(s)
                        .MethodSignature(isInstanceMethod = true)
                        .Parameters(
                            k,
                            (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                            (fun (pars: ParametersEncoder) ->
                                for i in 0 .. k - 1 do
                                    pars.AddParameter().Type().GenericTypeParameter i
                            )
                        )

                    toEntity (ctx.MemberRef(typeSpec, ".ctor", s))

                // `public !i Item{i+1}` — `ValueTuple` exposes public *fields*, not
                // properties, so element access is `ldfld`, not `call get_ItemN`.
                // For arity ≥ 8 only the 7 directly-stored slots get `Item` fields;
                // the tail rides the `Rest` field below.
                let directCount = if n <= 7 then n else 7

                let itemFields =
                    [|
                        for i in 0 .. directCount - 1 ->
                            let s = BlobBuilder()
                            BlobEncoder(s).FieldSignature().GenericTypeParameter i
                            toEntity (ctx.MemberRef(typeSpec, sprintf "Item%d" (i + 1), s))
                    |]

                let rest =
                    if n <= 7 then
                        ValueNone
                    else
                        // `public TRest Rest` — the 8th generic parameter (`!7`).
                        let restField =
                            let s = BlobBuilder()
                            BlobEncoder(s).FieldSignature().GenericTypeParameter 7
                            toEntity (ctx.MemberRef(typeSpec, "Rest", s))

                        ValueSome
                            {
                                RestField = restField
                                Nested = build elems.[7..]
                            }

                {
                    TypeSpec = typeSpec
                    Ctor = ctorRef
                    ItemFields = itemFields
                    Rest = rest
                }

            let handles = build (List.toArray elemTys)
            valueTupleRefsCache.[elemTys] <- handles
            handles

    member _.EncodeLocalSignature locals = encodeLocalSignature locals

    /// `instance void .ctor(fields…)` for a record / generic-type ctor. Field types
    /// carry their declaring typars as `TyTypar(Declaring, i)` nodes the encoder
    /// resolves to `!i` directly — no marker map.
    member _.RecordCtorSignature(paramTys: FrozenType list) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
                )
            )

        s

    /// A *generic method* (`member this.Map<'C> …`) whose body may also be inside
    /// a generic type. The declaring type's typars ride `TyTypar(Declaring, i)` nodes
    /// (`!i`) and the method's own typars ride `TyTypar(Method, i)` nodes (`!!i`); the
    /// structural `encodeType` match resolves both by index, so no ambient window is
    /// needed. `methodTyparCount` sets the `GENERIC` calling-convention header count.
    member _.GenericMethodOnTypeSignature
        (methodTyparCount: int, paramTys: FrozenType list, retTy: FrozenType, isInstanceMethod: bool)
        : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = methodTyparCount, isInstanceMethod = isInstanceMethod)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) (retTy)),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
                )
            )

        s

    /// A *generic method* whose return is genuine `void` — the generic counterpart
    /// of `InstanceMethodSignatureVoid`. A `unit`-returning generic instance method
    /// (e.g. `Formatter.AppendFormatted<'T> : 'T -> unit`) must encode `void` so a
    /// cross-assembly consumer's member-ref (which maps `unit → void`) binds; the
    /// `unit`-as-`ValueTuple` return would otherwise mismatch (a `MissingMethodException`
    /// when the Vesper-compiled handler is bound in place of the C# one). The body is
    /// emitted in void mode (the trailing `unit` value is popped).
    member _.GenericMethodOnTypeSignatureVoid
        (methodTyparCount: int, paramTys: FrozenType list, isInstanceMethod: bool)
        : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = methodTyparCount, isInstanceMethod = isInstanceMethod)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
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

    member _.GenericStaticFnSignature(typarCount: int, paramTys: FrozenType list, retTy: FrozenType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = typarCount, isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) (retTy)),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
                )
            )

        s

    member _.StaticMethodSignature(paramTys: FrozenType list, retTy: FrozenType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) (retTy)),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
                )
            )

        s

    /// `static void M(params…)` — a static method whose return is genuine `void`,
    /// the static counterpart of `InstanceMethodSignatureVoid`. A `unit`-returning
    /// module function / static member now encodes `void` (full F# fidelity)
    /// rather than the
    /// `unit`-as-`ValueTuple` the general `StaticMethodSignature` emits, so it
    /// matches the consumer convention (`unit → void` member-refs) the instance path
    /// already used. The body is emitted in void mode (the trailing `unit` value is
    /// popped). A *generic* static void method reuses `GenericMethodOnTypeSignatureVoid`
    /// with `isInstanceMethod = false`.
    member _.StaticMethodSignatureVoid(paramTys: FrozenType list) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
                )
            )

        s

    member _.InstanceMethodSignature(paramTys: FrozenType list, retTy: FrozenType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) (retTy)),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
                )
            )

        s

    /// `instance void M(params…)` — an instance method whose return is genuine
    /// `void`. A Vesper `unit`-returning method normally encodes its return as
    /// `System.ValueTuple` (the `unit`-as-value convention) and leaves that value
    /// on the stack, but an interface-impl member conforming to a BCL slot whose
    /// return is `void` (e.g. `IDisposable.Dispose` / `IEnumerator.Reset`) must
    /// match the slot's `void` signature, or the runtime reports the method
    /// "does not have an implementation". The body is emitted in void mode
    /// (`Emit.buildMember ~voidReturn:true` pops the trailing `unit` value).
    member _.InstanceMethodSignatureVoid(paramTys: FrozenType list) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) (p)
                )
            )

        s

    /// `instance resultTy Invoke(paramTys…)` — the closure's concrete flat `Invoke`
    /// override signature: one concrete parameter per `paramTys` entry (arity
    /// `List.length paramTys`, `1..4`), returning `resultTy`. The single Invoke-sig
    /// encoder for every closure arity — curried arity-1 (`Fun`2`, one param) through
    /// flat arity-4 (`Fun`5`, four params).
    member _.InvokeSignatureN(paramTys: FrozenType list, resultTy: FrozenType) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) (resultTy)),
                (fun (pars: ParametersEncoder) ->
                    for pty in paramTys do
                        encodeType (pars.AddParameter().Type()) (pty)
                )
            )

        msig

    /// `instance void .ctor(captures…)` — one concrete parameter per captured value (in field order).
    member _.ClosureCtorSignature(captures: FrozenType list) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length captures,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for c in captures do
                        encodeType (pars.AddParameter().Type()) (c)
                )
            )

        msig

    member _.FieldSignature(ty: FrozenType) : BlobBuilder =
        let blob = BlobBuilder()
        let te = BlobEncoder(blob).FieldSignature()
        encodeType te ty
        blob

    /// A field signature whose type is the closure's OWN (reference) type, given its
    /// `TypeDefinition` handle directly — a closure type has no `FrozenType` the
    /// encoder resolves, so the cached-singleton field is encoded
    /// here from its handle rather than through `FieldSignature ty`.
    member _.ClosureSelfFieldSignature(closureTypeHandle: EntityHandle) : BlobBuilder =
        let blob = BlobBuilder()
        BlobEncoder(blob).FieldSignature().Type(closureTypeHandle, false)
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
    member _.EqualsTypedSignature(selfTy: FrozenType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Boolean()),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) (selfTy))
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

    member _.CompareToTypedSignature(selfTy: FrozenType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) (selfTy))
            )

        s

    /// Encode an abstract interface-method signature. The signature's open
    /// typars are self-describing `TyTypar` nodes — `Declaring` → `!i`, `Method` → `!!j` — that the
    /// structural `encodeType` match resolves directly.
    member _.EncodeAbstractType(te: SignatureTypeEncoder, t: FrozenType) : unit = encodeType te t
