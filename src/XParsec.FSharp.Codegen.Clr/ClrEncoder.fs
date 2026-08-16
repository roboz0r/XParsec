namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Turns a `FrozenType` into a metadata signature: function types → `Vesper.Fun`2`, lists →
/// `Vesper.Collections.List`1`, user types → their predicted `TypeDefinition`, external types
/// through the symbol provider. Also hosts the blob builders that wrap it.
type internal ClrEncoder(env: ClrEnv) =
    let ctx = env.Ctx
    let markFSharpCoreDep c = env.MarkFSharpCoreDep c
    let userTypes = env.UserTypes
    let userValueTypes = env.UserValueTypes

    let externalClassRef key = env.ExternalClassRef key
    let externalIsValueType key = env.ExternalIsValueType key
    let externalRecordRef (key, a) = env.ExternalRecordRef(key, a)
    let externalUnionRef (key, a) = env.ExternalUnionRef(key, a)

    let eValueTuple = env.EValueTuple
    let eTextWriter = env.ETextWriter
    let eFormatter = env.EFormatter
    let eHashCode = env.EHashCode
    let eFun2 () = env.EFun2()
    let ePrintfFormat4 = env.EPrintfFormat4
    let eVesperList1 = env.EVesperList1

    /// Single-sourced primitive repr, as an active pattern over an `FTConst` canon key.
    let (|PrimitiveRepr|_|) (key: TypeKey) = env.TryPrimitiveRepr key

    // `ValueTuple`n` handle bundles, cached by element-type list. Unlike `ctx.TypeRef` (which
    // dedups its rows), `ctx.TypeSpec` / `ctx.MemberRef` add a fresh metadata row per call, so
    // without this even the two sites of one `let (a, b) = (1, 2)` would mint duplicate rows.
    let valueTupleRefsCache =
        Dictionary<FrozenType list, ValueTupleHandles>(HashIdentity.Structural)

    // Referenced-assembly nominal recognisers: each looks the `TypeRef` up ONCE and matches on
    // the whole node, so the pattern can read `args` for the arity probe.
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

    /// Encode a `FrozenType` into a metadata signature slot. Context-free: open typars are
    /// self-describing `FTTypar(axis, i)` nodes resolved by index, so there is no ambient
    /// typar window and no caller-supplied replacement hook.
    let rec encodeType (te: SignatureTypeEncoder) (t: FrozenType) : unit =
        match t with
        // `obj` → `ELEMENT_TYPE_OBJECT`, not `class System.Object`. A BCL interface method
        // declared `CompareTo(object)` is encoded with the primitive token, and implicit override
        // binding is by signature-BLOB match, so an implementing member must match the encoding.
        | FTObj -> te.Object()
        | FTConst(key, _) when key = ClrSinkKeys.textWriter -> te.Type(eTextWriter.Value, false)
        | FTConst(key, _) when key = ClrSinkKeys.formatter -> te.Type(eFormatter.Value, true)
        | FTConst(key, _) when key = ClrSinkKeys.hashCode -> te.Type(eHashCode.Value, true)
        // The array intrinsic `[]<elem>` (`'T[]`) → an SZArray (rank-1 vector) of
        // the element. Higher-rank arrays (`[,]`) aren't emitted yet.
        | FTArray elem -> encodeType (te.SZArray()) elem
        | FTFun(a, b) ->
            let g = te.GenericInstantiation(eFun2 (), 2, false)
            encodeType (g.AddArgument()) a
            encodeType (g.AddArgument()) b
        | FTClass(key, args) when RuntimeNames.isPrintfFormatKey key ->
            markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
            let g = te.GenericInstantiation(ePrintfFormat4.Value, args.Length, false)

            for a in args do
                encodeType (g.AddArgument()) a
        // `userTypes` holds exactly the types emitted into THIS assembly. A self-host
        // `Vesper.Collections.List` and a referenced one share a key; membership is the only
        // thing separating the emitted `TypeDef` from the cached external `eVesperList1`.
        | FTUnion(key, args) when userTypes.ContainsKey key ->
            let handle = userTypes.[key]

            if args.IsEmpty then
                te.Type(handle, false)
            else
                let g = te.GenericInstantiation(handle, args.Length, false)

                for a in args do
                    encodeType (g.AddArgument()) a
        | FTUnion(key, args) when RuntimeNames.isVesperListKey key && args.Length = 1 ->
            // The Vesper cons-list ≡ `Vesper.Collections.List`1<elem>`, so no FSharp.Core dep.
            let elem = args.[0]
            let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
            encodeType (g.AddArgument()) elem
        | FTRecord(key, args) when userTypes.ContainsKey key ->
            let handle = userTypes.[key]
            // A `[<Struct>]` record encodes `ELEMENT_TYPE_VALUETYPE` so a signature matches its
            // value-type `TypeDefinition`; a reference record is `ELEMENT_TYPE_CLASS`.
            let isVt = userValueTypes.Contains key

            if args.IsEmpty then
                te.Type(handle, isVt)
            else
                let g = te.GenericInstantiation(handle, args.Length, isVt)

                for a in args do
                    encodeType (g.AddArgument()) a
        | FTClass(key, args) when userTypes.ContainsKey key ->
            let handle = userTypes.[key]
            let isVt = userValueTypes.Contains key

            if args.IsEmpty then
                te.Type(handle, isVt)
            else
                let g = te.GenericInstantiation(handle, args.Length, isVt)

                for a in args do
                    encodeType (g.AddArgument()) a
        | ExternalClass(tref, vt, args) ->
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
            // A referenced-package union (`Vesper.Option<int>`) is a reference type, never
            // `VALUETYPE`.
            if args.IsEmpty then
                te.Type(tref, false)
            else
                let g = te.GenericInstantiation(tref, args.Length, false)

                for a in args do
                    encodeType (g.AddArgument()) a
        | FTUnknown reason ->
            // The two reasons a contract bakes are reported at `unify`; a value carrying any of
            // the rest is skipped before emit. So nothing untyped reaches signature encoding.
            failwithf
                "ClrProvider: the untyped position %s reached signature encoding (the front end should have errored first)"
                reason.Render
        | FTLocalTypar(scheme, i) ->
            // A typar of a body-local `let`'s own generalized scheme. It may reach the backend,
            // because it is phantom wherever a closure over it is `Vesper.Fun`-boxed, but not
            // signature encoding: unlike an `FTTypar` it occupies no slot in any enclosing
            // generic parameter list.
            failwithf
                "ClrProvider: local typar #%d of body-local %O reached signature encoding — it occupies no generic parameter slot, so it has no CLR representation (the emitting site should have declined or boxed it)"
                i
                scheme
        // Declaring-axis → the enclosing type's `!i`; Method-axis → the method's own `!!i`.
        | FTTypar(TyparAxis.Declaring, i) -> te.GenericTypeParameter i
        | FTTypar(TyparAxis.Method, i) ->
            match env.ClosureTyparScope with
            // Inside a closure's own emission the enclosing class typars hold the first `d`
            // slots, so a method-axis typar lands at `!(d + i)` (a static-fn closure: `d = 0`).
            | ValueSome d -> te.GenericTypeParameter(d + i)
            | ValueNone -> te.GenericMethodTypeParameter i
        // A tuple is a member of the `System.ValueTuple` struct family, a `VALUETYPE` generic
        // instantiation, and nests whatever does not fit one member, per the standard .NET scheme.
        | FTTuple items ->
            let rec encodeFrom (out: SignatureTypeEncoder) (start: int) =
                let remaining = items.Length - start

                if ClrTuples.fitsOneMember remaining then
                    let g = out.GenericInstantiation(env.EValueTupleN remaining, remaining, true)

                    for i in start .. items.Length - 1 do
                        encodeType (g.AddArgument()) items.[i]
                else
                    let g =
                        out.GenericInstantiation(env.EValueTupleN ClrTuples.MaxArity, ClrTuples.MaxArity, true)

                    for i in start .. start + ClrTuples.MaxDirect - 1 do
                        encodeType (g.AddArgument()) items.[i]

                    encodeFrom (g.AddArgument()) (start + ClrTuples.MaxDirect)

            encodeFrom te 0
        // A by-ref (`T&`) is legal only in parameter / return / local position, where the
        // `ELEMENT_TYPE_BYREF` prefix is emitted at the encoder seam. Reaching the RECURSIVE
        // encoder means field or generic-argument position, which is illegal in CLR metadata.
        | FTByref _ ->
            failwithf
                "ClrProvider: by-ref type '%A' in a non-param/return position (illegal as a field or generic argument)"
                t
        // A project-local enum, always a value type: a NUMERIC enum subclasses `System.Enum`, a
        // STRING/MIXED one is a `[<Struct>]` wrapper over `System.ValueType`. Both encode
        // `ELEMENT_TYPE_VALUETYPE` off the emitted handle, with no generic args (enums aren't).
        | FTEnum key when userTypes.ContainsKey key -> te.Type(userTypes.[key], true)
        | FTEnum _ ->
            failwithf
                "ClrProvider: cannot encode enum type reference %A — project-local enums only; external (TS-manifest) enums are a JS-target concern, unsupported on CLR"
                t
        // A structural literal has no IL repr of its own, so erase to its base primitive.
        // External (TS/JS) vocabulary only, so this arm is rarely reached on CLR.
        | FTLiteral v -> encodeType te (FTConst(RuntimeNames.literalBaseKey v, EqArray.empty))
        // A carried type-level computation (keyof / indexed-access / conditional) is a JS-seam
        // construct with no CLR repr; the front end must ground-evaluate it before codegen.
        | FTKeyOf _
        | FTIndexedAccess _
        | FTConditional _ ->
            failwithf
                "ClrProvider: cannot encode unevaluated type-level computation %A — keyof/indexed-access/conditional are a JS-target concern and must be ground-evaluated before CLR emit"
                t
        // A nullable REFERENCE union `T | null` IS the CLR reference-null repr of `T`, because
        // `obj | null` and `obj` are the same `System.Object` slot, so erase `null` and encode
        // the survivor. `int | string` has no anonymous-union IL repr and falls to the error below.
        | FTOr disjuncts ->
            let nonNull =
                disjuncts.Disjuncts
                |> EqSet.toList
                |> List.filter (
                    function
                    | FTNull -> false
                    | _ -> true
                )

            match nonNull with
            | [ single ] -> encodeType te single
            | _ ->
                failwithf
                    "ClrProvider: cannot encode anonymous union %A — only a nullable reference `T | null` is representable on CLR (erased to `T`)"
                    t
        // Keys the IL type off the repr string (`"int"` → `"System.Int32"` → `i4`), not the
        // Vesper name, which survives only for the failure diagnostic. `obj` and `'T[]` are
        // `FTConst` spellings too, so this arm follows theirs.
        | FTConst(PrimitiveRepr repr & key, args) ->
            if args.IsEmpty && IntrinsicRepr.tryEncodeValueType te repr then
                ()
            elif args.IsEmpty && repr = "System.ValueTuple" then
                // `unit` — the zero-field BCL struct; a value type with no external ref of its own.
                te.Type(eValueTuple.Value, true)
            else
                // An intrinsic whose repr is a BCL TYPE, not a primitive: `exn` →
                // `System.Exception`. A generic repr spells its own `` `N ``, hence arity 0.
                let platformKey = SymbolKeyOps.qualifiedTypeKeyOf repr 0

                match externalClassRef platformKey with
                // The repr's OWN value-ness: a struct encoded as `class X` only dies at JIT time.
                | ValueSome tref when args.IsEmpty -> te.Type(tref, externalIsValueType platformKey)
                | ValueSome tref ->
                    let g = te.GenericInstantiation(tref, args.Length, externalIsValueType platformKey)

                    for a in args do
                        encodeType (g.AddArgument()) a
                | ValueNone ->
                    let (DisplayName name) = SymbolKeyOps.typeSimpleName key
                    failwithf "ClrProvider: no IL encoding for intrinsic representation %s (type %s)" repr name
        | other -> failwithf "ClrProvider: cannot encode FrozenType: %A" other

    /// Recover both open-typar axes by structurally matching a member's OPEN signature template
    /// (carrying `FTTypar(axis, i)` nodes) against its INSTANTIATED, already-ground use-site type.
    /// Returns `(declaringArgs, methodArgs)`, index-keyed; first occurrence wins.
    let recoverOpenTypars
        (declTyparArity: int)
        (methodTyparArity: int)
        (openT: FrozenType)
        (instT: FrozenType)
        : FrozenType list * FrozenType list =
        let decl = Array.create declTyparArity ValueNone
        let meth = Array.create methodTyparArity ValueNone

        let rec go (d: FrozenType) (a: FrozenType) =
            match d with
            | FTTypar(axis, i) ->
                let slot =
                    match axis with
                    | TyparAxis.Declaring -> decl
                    | TyparAxis.Method -> meth

                if i >= 0 && i < slot.Length && slot.[i].IsNone then
                    slot.[i] <- ValueSome a
            // Pairwise descent under a shared type constructor: a mismatch declines silently
            // (no recovery from that subtree), and `collect` below fails loud on any slot
            // left empty.
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
    /// generic (`EqualityComparer`1<int>`). A struct declaring type (`Span`1<char>`, a struct
    /// record) must be tagged `VALUETYPE` or the runtime rejects the ref ("value type mismatch").
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
    /// (`fold<int,int>`). A non-generic handle (`args = []`) returns unchanged.
    let methodSpec (handle: EntityHandle) (args: FrozenType list) : EntityHandle =
        match args with
        | [] -> handle
        | _ ->
            let inst = BlobBuilder()
            let specEnc = BlobEncoder(inst).MethodSpecificationSignature(List.length args)

            for t in args do
                encodeType (specEnc.AddArgument()) (t)

            toEntity (ctx.MethodSpec(handle, inst))

    member _.EncodeType(te, t: FrozenType) = encodeType te t

    member _.RecoverOpenTypars(declTyparArity, methodTyparArity, openT, instT) =
        recoverOpenTypars declTyparArity methodTyparArity openT instT

    member _.MethodSpec(handle, args) = methodSpec handle args

    member _.ExternalTypeSpec(tref, isValueType, instArgs) =
        externalTypeSpec tref isValueType instArgs

    /// A `TypeSpec` token for an arbitrary `FrozenType`, encoded through the full `encodeType`
    /// path, so a struct external type lands as a `VALUETYPE` generic-inst (the duck-typed
    /// enumerator's member-ref parent), with the tag read off the type rather than a flag.
    member _.TypeSpecOf(ty: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeType te ty
        toEntity (ctx.TypeSpec tsB)

    /// The `System.ValueTuple` handles for an N-tuple of `elemTys`, shared by construction and
    /// destructuring. The ctor / `Item` signatures name the type's own `!0…`, so they are
    /// element-type-independent and only the parent `TypeSpec` carries the instantiation.
    member _.ValueTupleRefs(elemTys: FrozenType list) : ValueTupleHandles =
        if not (ClrTuples.isTupleArity (List.length elemTys)) then
            failwithf
                "ClrProvider: ValueTupleRefs needs arity ≥ 2, got %d (unit / 1-tuples are not tuple values)."
                (List.length elemTys)

        match valueTupleRefsCache.TryGetValue elemTys with
        | true, cached -> cached
        | _ ->
            let rec build (elems: FrozenType[]) : ValueTupleHandles =
                let n = elems.Length
                let k = ClrTuples.memberArity n

                // The full (possibly nested) tuple type, doubling as the member-ref parent
                // `TypeSpec`. The recursive `TRest` nesting happens inside `encodeType`.
                let typeSpec =
                    let tsB = BlobBuilder()
                    let te = BlobEncoder(tsB).TypeSpecificationSignature()
                    encodeType te (FTTuple(EqArray.ofSeq elems))
                    toEntity (ctx.TypeSpec tsB)

                // `instance void .ctor(!0…!{k-1})` — for k = 8 the last param `!7` is the
                // nested `TRest` value.
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

                // `public !i Item{i+1}` — `ValueTuple` exposes public FIELDS, not properties, so
                // element access is `ldfld`, not `call get_ItemN`. Only the directly-stored
                // slots get `Item` fields; the rest ride the `Rest` field below.
                let directCount = if ClrTuples.fitsOneMember n then n else ClrTuples.MaxDirect

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

    /// `instance void .ctor(fields…)` for a record / generic-type ctor. Field types carry their
    /// declaring typars as `FTTypar(Declaring, i)`, which the encoder resolves to `!i` directly.
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

    /// A *generic method* (`member this.Map<'C> …`) whose body may also be inside a generic type:
    /// the declaring type's typars ride `FTTypar(Declaring, i)` (`!i`), the method's own ride
    /// `FTTypar(Method, i)` (`!!i`). `methodTyparCount` sets the `GENERIC` header count.
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

    /// A *generic method* whose return is genuine `void`. A `unit`-returning generic instance
    /// method (`Formatter.AppendFormatted<'T> : 'T -> unit`) must encode `void`, or a consumer's
    /// `unit → void` member-ref misses it (`MissingMethodException`). Body emitted in void mode.
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

    /// `static void M(params…)` — a `unit`-returning module function / static member, encoding
    /// `void` rather than the `unit`-as-`ValueTuple` `StaticMethodSignature` emits, so it matches
    /// the `unit → void` member-ref convention. The body is emitted in void mode.
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

    /// `instance void M(params…)`. A `unit`-returning method normally encodes its return as
    /// `System.ValueTuple`, but an interface-impl member on a `void` BCL slot must match it, or
    /// the runtime reports "does not have an implementation". Body emitted in void mode.
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

    /// `instance resultTy Invoke(paramTys…)` — a closure's flat `Invoke` override, one concrete
    /// parameter per entry: curried arity-1 (`Fun`2`) through flat arity-4 (`Fun`5`).
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

    /// A field signature typed as the closure's OWN (reference) type. A closure type has no
    /// `FrozenType`, so the cached-singleton field is encoded straight from its handle.
    member _.ClosureSelfFieldSignature(closureTypeHandle: EntityHandle) : BlobBuilder =
        let blob = BlobBuilder()
        BlobEncoder(blob).FieldSignature().Type(closureTypeHandle, false)
        blob

    /// `override bool Equals(object)`. The parameter is the compact `ELEMENT_TYPE_OBJECT` encoding,
    /// not `class System.Object`: `System.Object::Equals(object)` uses the compact form, and a
    /// mismatched blob lands the method in a NEW vtable slot instead of overriding.
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

    /// The `override int32 CompareTo(object)` signature encodes `.Object()` because
    /// `IComparable::CompareTo(object)` is declared that way (signature-blob match).
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

    /// Encode an abstract interface-method signature. Its open typars are self-describing
    /// `FTTypar` nodes, resolved directly by index: `Declaring` → `!i`, `Method` → `!!j`.
    member _.EncodeAbstractType(te: SignatureTypeEncoder, t: FrozenType) : unit = encodeType te t
