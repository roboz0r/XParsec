namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// The `externalMemberCache` key — a *structural* value, NOT a `sprintf "%A"`
/// string. `%A` renders an `EqArray` (a `FrozenType`'s type args) as its bare
/// runtime type name, so two members differing only inside their args — e.g.
/// `IEnumerable<!!0>.GetEnumerator` vs `IEnumerable<!!1>.GetEnumerator`, the two
/// enumerator sources of a 1-typar and a 2-typar method in one module — would key
/// identically and the first method's `MemberRef` (parent `TypeSpec` baking `!!1`)
/// would be reused by the second (which has no `!!1`), emitting a malformed token
/// (`BadImageFormatException` at JIT). `FrozenType`/`SymbolKey` are immutable with
/// structural equality (via `EqArray`), so keying on the values themselves is both
/// correct and cheap.
[<RequireQualifiedAccess>]
type internal ExternalMemberCacheKey =
    | Ref of key: SymbolKey * isProperty: bool * isStatic: bool * memberTy: FrozenType
    | On of key: SymbolKey * declTy: FrozenType * isProperty: bool * isStatic: bool * memberTy: FrozenType
    | Field of key: SymbolKey * declTy: FrozenType voption * memberTy: FrozenType

/// The identity bridge: a resolved external symbol's
/// `Origin`/`SymbolKey` → an `AssemblyRef`/`TypeRef`/`TypeSpec`/`MemberRef`, with no per-member
/// hand-coding. Mints member / constructor / field references and generic-static-method specs against
/// referenced-assembly types.
type internal ClrExternalMembers(env: ClrEnv, enc: ClrEncoder) =
    let ctx = env.Ctx
    let symbols = env.Symbols
    let arityOfMetaName n = env.ArityOfMetaName n
    let externalClassRef key = env.ExternalClassRef key
    let externalRecordRef key arity = env.ExternalRecordRef(key, arity)
    let externalUnionRef key arity = env.ExternalUnionRef(key, arity)
    let encodeType te t = enc.EncodeType(te, t)
    let methodSpec handle args = enc.MethodSpec(handle, args)

    let recoverOpenTypars declTyparArity methodTyparArity (openT: FrozenType) (instT: FrozenType) =
        enc.RecoverOpenTypars(declTyparArity, methodTyparArity, openT, instT)

    // The declaring-type key drives the `VALUETYPE` vs `CLASS` tag of the parent generic-inst
    // (`Span`1<char>` and struct unions/records are value types); a non-value-type or an
    // unresolvable name tags `CLASS`, as before.
    let externalTypeSpec (key: SymbolKey) tref instArgs =
        enc.ExternalTypeSpec(tref, env.ExternalIsValueType key, instArgs)

    let typeSpecOf ty = enc.TypeSpecOf ty

    /// `SymbolKey` (+ instantiation) → minted `MemberRef`, so a member is reified once across a
    /// compilation.
    let externalMemberCache = Dictionary<ExternalMemberCacheKey, EntityHandle>()

    /// Build the member-ref signature blob (property getter, or tupled-flattened method with the BCL
    /// `void`-return fix) directly from the member's open `FrozenType` signature template:
    /// `paramsT` is the .NET-tupled argument slot and `retT` the
    /// return, each carrying self-describing `FTTypar(Declaring, i)` / `FTTypar(Method, j)` placeholders
    /// the `encodeType` arm resolves to `!i` / `!!j` directly. Replaces running the legacy
    /// `BuildSignature` closure on marker typars then decurrying — the template already carries the
    /// single top-level tupled split. `methodTyparArity > 0` sets the `GENERIC` calling-convention header
    /// count for a generic external method (`Enumerable.Take<TSource>`); the caller wraps the result in
    /// a `MethodSpec`. Shared by `externalMemberRef` (parent recovered by signature match) and
    /// `externalMemberRefOn` (parent encoded straight from the declaring type).
    let mintMemberRef
        (parent: EntityHandle)
        (methodTyparArity: int)
        (paramsT: FrozenType)
        (retT: FrozenType)
        (isProperty: bool)
        (isStatic: bool)
        (argSigLen: int)
        (memberName: string)
        : EntityHandle =
        let metaName = if isProperty then "get_" + memberName else memberName
        let s = BlobBuilder()

        if isProperty then
            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retT),
                    (fun (_: ParametersEncoder) -> ())
                )
        else
            // A .NET method of arity ≥ 2 is modelled tupled (`(p1*…*pN) → ret`), so the single
            // `Parameters` template is the argument `FTTuple` — flatten it back to N parameters, driven
            // by the chosen key's `argSig` length (authoritative: a genuine single `(int*int)` param has
            // argSig length 1 and stays one parameter). Arity ≤ 1 / `unit` unchanged.
            let paramTys =
                match paramsT with
                | FTUnit -> []
                | FTTuple elems when argSigLen >= 2 && elems.Length = argSigLen -> EqArray.toList elems
                | p -> [ p ]

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = methodTyparArity, isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    // A `System.Void` return maps to `TyConst "unit"`,
                    // but a BCL method's `void` is a
                    // genuine `void` slot — encoding it as `FSharp.Core.Unit`
                    // (the value-position `unit` encoding) mints a `MemberRef`
                    // whose signature no external void method matches, so the
                    // runtime fails to bind it (`MissingMethodException`). Emit
                    // `void` directly here (`IDisposable.Dispose`, `List.Add`).
                    (fun (ret: ReturnTypeEncoder) ->
                        match retT with
                        | FTUnit -> ret.Void()
                        // A by-ref return (`Span<T>.get_Item : T&`) emits the
                        // `ELEMENT_TYPE_BYREF` prefix via the *return* encoder's
                        // `isByRef` flag, then the element — byref is not a standalone
                        // `SignatureTypeEncoder` shape, it rides the param/return seam.
                        // The member-ref signature must match the BCL method's
                        // by-ref return exactly or it fails to bind at JIT.
                        | FTByref elem -> encodeType (ret.Type(true)) elem
                        | _ -> encodeType (ret.Type()) retT
                    ),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            match p with
                            // A by-ref / `out` / `ref` parameter (`Int32.TryParse(string,
                            // int&)`, `ISpanFormattable.TryFormat(…, int&, …)`) emits the
                            // `ELEMENT_TYPE_BYREF` prefix via the *parameter* encoder's
                            // `isByRef` flag, then the element — symmetric with the byref
                            // *return* arm above. The signature must match the BCL
                            // method's by-ref parameter exactly or it fails to bind at JIT;
                            // the caller pushes the argument's *address* (`ldloca`).
                            //
                            // TODO(inref): a C# `in` parameter is `T&` plus a
                            // `modreq(System.Runtime.InteropServices.InAttribute)` the CLR
                            // includes in member-ref matching; this emits a bare `T&`, so an
                            // `in`-parameter call would `MissingMethodException`. To support
                            // it, surface the modreq in `tryBuildType` (paired TODO in
                            // MetadataSymbols.fs) and emit it here via
                            // `CustomModifiers(...).Type(true)`. No printf-port consumer needs
                            // it (TryFormat/TryParse use `out` + by-value spans).
                            | FTByref elem -> encodeType (pars.AddParameter().Type(true)) elem
                            | _ -> encodeType (pars.AddParameter().Type()) p
                    )
                )

        toEntity (ctx.MemberRef(parent, metaName, s))

    /// Look up the resolved external member for `key` — the *exact* overload the front end committed
    /// (its key, incl. `argSig`, matches), NOT a singular re-pick (which would re-collapse a resolved
    /// overload to the most-params one and disagree with the node's `memberTy`). The singular
    /// `TryLookupMember` is the fallback for providers exposing only that surface.
    let lookupChosen (declFullName: string) (memberName: string) (key: MemberKey) : ExternalMember =
        let chosen =
            match
                symbols.TryLookupMembers(declFullName, memberName)
                |> ExternalSymbols.memberByKey key
            with
            | ValueSome m -> ValueSome m
            | ValueNone -> symbols.TryLookupMember(declFullName, memberName)

        match chosen with
        | ValueSome m -> m
        | ValueNone -> failwithf "ClrProvider: external member '%s.%s' did not resolve at emit" declFullName memberName

    /// The member's open signature as a single `FrozenType` template: the bare value type
    /// for a property, else the .NET-tupled `FTFun(params, ret)`. The form
    /// `recoverOpenTypars` matches against the instantiated use-site type (the two-axis
    /// split having been baked by the producer); `mintMemberRef` consumes the split fields.
    let openTemplate (chosen: ExternalMember) (isProperty: bool) : FrozenType =
        let s = chosen.Signature

        if isProperty then
            s.Return
        else
            FTFun(s.Parameters, s.Return)

    /// Mint the `MemberRef` for a `TExpr.ExternalMember`. The member's *open* signature is read
    /// from the (key-pinned, provider-cached) lookup over `FTTypar(Declaring, i)` markers — and, for a
    /// generic method, with `FTTypar(Method, j)` baked into the member's `ExternalSignature` template.
    /// Both axes' use-site instantiations are recovered by matching that open form against `memberTy`:
    /// the declaring args parameterise the parent `TypeSpec`; the method args (if any) the `MethodSpec`.
    let externalMemberRef (key: SymbolKey) (isProperty: bool) (isStatic: bool) (memberTy: FrozenType) : EntityHandle =
        // The IR still carries the wide `SymbolKey` in member position, so the narrowing is
        // stated once (`asMemberKey`) rather than re-matched here. `MemberKey.Decl` IS a
        // `TypeKey`, so the declaring type needs no further check; `name` is its simple
        // metadata name — `+`-joined for a CLR nested type, produced by the ONE renderer.
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external member ref" key
        let declKey, memberName, argSig = mk.Decl, mk.Name, mk.ArgSig

        let name = SymbolKeyOps.typeNestedName declKey

        let memoKey = ExternalMemberCacheKey.Ref(key, isProperty, isStatic, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let declTyparArity = arityOfMetaName name

            let chosen = lookupChosen declFullName memberName mk
            let methodTyparArity = chosen.MethodTyparArity
            let sig_ = chosen.Signature

            let declArgs, methodArgs =
                recoverOpenTypars declTyparArity methodTyparArity (openTemplate chosen isProperty) memberTy

            let tref =
                match externalClassRef (SymbolKey.Type declKey) with
                | ValueSome t -> t
                | ValueNone ->
                    failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

            let parent = externalTypeSpec (SymbolKey.Type declKey) tref (declArgs)

            let handle =
                methodSpec
                    (mintMemberRef
                        parent
                        methodTyparArity
                        sig_.Parameters
                        sig_.Return
                        isProperty
                        isStatic
                        argSig.Length
                        memberName)
                    methodArgs

            externalMemberCache.[memoKey] <- handle
            handle

    /// A capability member call (`enumerator<'T>.MoveNext()`) resolves against its
    /// canonical declaring capability (`Vesper.Collections.enumerator`), which reconciles
    /// to a BCL platform face (`System.Collections.Generic.IEnumerator`1`). But a member's
    /// TRUE declaring type may be a *base* of that face — `MoveNext` is declared on the
    /// non-generic `System.Collections.IEnumerator`, NOT on `IEnumerator`1`, which merely
    /// inherits it — so a member-ref minted against the face faults at runtime
    /// (`MissingMethodException`). Re-resolve the member on the platform face's metadata
    /// interface hierarchy (which reports each member's real declaring type) and, when that
    /// declarer is a base of the face, return the member key rebased onto it. This is the
    /// manual-call analogue of the BCL declarers the `for … in` lowering mints by hand
    /// (`EmitLoops` — `MoveNext` on `IEnumerator`, `Current` on `IEnumerator`1`), here
    /// derived from metadata rather than hardcoded string literals. `ValueNone` when the
    /// declaring type is not a capability interface, the member is declared on the platform
    /// face itself (`GetEnumerator` on `IEnumerable`1`, `Current` on `IEnumerator`1` — no
    /// rebase needed), or no base member of the requested `kind` exists (decline rather than
    /// rebase onto an arbitrary same-named member, which would mint a wrong ref).
    let tryCapabilityBaseMemberKey (key: SymbolKey) : SymbolKey voption =
        match key with
        | SymbolKey.Member {
                               Decl = declKey
                               Name = memberName
                               Kind = kind
                           } ->
            match env.LookupTypeByKey(SymbolKey.Type declKey) with
            | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                let members = symbols.TryLookupMembers(platform, memberName)

                let declaredOn (m: ExternalMember) = SymbolKeyOps.typeMetaName m.Key.Decl

                // Declared on the platform face itself → the existing face-parented
                // member-ref already binds; no rebase.
                if members |> Array.exists (fun m -> declaredOn m = platform) then
                    ValueNone
                else
                    // Inherited from a base interface: rebase onto the base member of the
                    // requested `kind`, whose `Key` names its real declaring base (e.g.
                    // `IEnumerator`), so the later member-ref mints against the base, non-generic
                    // parent. No such base member ⇒ decline (`ValueNone`), leaving the original
                    // face-parented key: rebasing onto an arbitrary same-named member would mint
                    // a wrong ref, no safer than the un-rebased key the caller falls back to.
                    match members |> Array.tryFind (fun m -> m.Key.Kind = kind) with
                    | Some m -> ValueSome(SymbolKey.Member m.Key)
                    | None -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// `externalMemberRef` for a member whose declaring type's instantiation cannot be recovered from
    /// the member's *open* signature — a T-free member like `MoveNext(): bool` on a generic enumerator.
    /// Instead of recovering it by signature match, the declaring instantiation is read off `declTy`
    /// (the resolved `enumeratorTy`, e.g. `List`1+Enumerator<int>`); its `args` length sets the marker
    /// count (NOT `arityOfMetaName`, which yields 0 for a nested `…List`1+Enumerator` name). The parent
    /// `TypeSpec` is encoded through `enc.TypeSpecOf`, so a struct enumerator's parent lands as a
    /// `VALUETYPE` generic-inst (and nested-correct via the fixed `externalClassRef`).
    let externalMemberRefOn
        (key: SymbolKey)
        (declTy: FrozenType)
        (isProperty: bool)
        (isStatic: bool)
        (memberTy: FrozenType)
        : EntityHandle =
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external member ref" key
        let declKey, memberName, argSig = mk.Decl, mk.Name, mk.ArgSig

        let memoKey = ExternalMemberCacheKey.On(key, declTy, isProperty, isStatic, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let chosen = lookupChosen declFullName memberName mk
            let methodTyparArity = chosen.MethodTyparArity
            let sig_ = chosen.Signature

            // The declaring args come straight off `declTy` (the whole point of this entry point); only
            // the method axis (if any) is recovered by matching the open signature template against
            // `memberTy`. Passing declTyparArity 0 leaves the template's `FTTypar(Declaring, i)` unrecorded —
            // those slots encode as `!i` straight off the node when the signature blob is minted.
            let _, methodArgs =
                recoverOpenTypars 0 methodTyparArity (openTemplate chosen isProperty) memberTy

            // The parent is the declaring type encoded directly (value-type / nested correct), not
            // recovered+rebuilt — that is the whole point of this entry point.
            let parent = typeSpecOf declTy

            let handle =
                methodSpec
                    (mintMemberRef
                        parent
                        methodTyparArity
                        sig_.Parameters
                        sig_.Return
                        isProperty
                        isStatic
                        argSig.Length
                        memberName)
                    methodArgs

            externalMemberCache.[memoKey] <- handle
            handle

    /// Mint a field `MemberRef` for a genuine external public field (`String.Empty`,
    /// `ValueTuple`2<_,_>.Item1`) — read via `ldfld`/`ldsfld`, not a `get_X` accessor.
    /// A field is a value member: its open type template is `Signature.Return`, encoded
    /// (in `!i` form) as the FIELD-calling-convention signature so it matches the generic
    /// field definition. The PARENT `TypeSpec`'s instantiation comes from `declTy` when
    /// the access has a receiver (the receiver's resolved type is authoritative — a
    /// generic struct's field may mention only some typars, so recovery from the field
    /// type alone under-determines them), else (a static field) it is recovered by
    /// matching the open field type against the use-site `memberTy`. No `MethodSpec`.
    let externalFieldRef (key: SymbolKey) (declTy: FrozenType voption) (memberTy: FrozenType) : EntityHandle =
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external field ref" key
        let declKey, fieldName = mk.Decl, mk.Name

        let name = SymbolKeyOps.typeNestedName declKey

        let memoKey = ExternalMemberCacheKey.Field(key, declTy, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let chosen = lookupChosen declFullName fieldName mk
            let openFieldTy = chosen.Signature.Return

            let parent =
                match declTy with
                | ValueSome dt ->
                    // Instance access: the receiver type pins the declaring instantiation
                    // (and value-type / nested correctness), like `externalMemberRefOn`.
                    typeSpecOf dt
                | ValueNone ->
                    // Static field: recover the declaring args from the open field type.
                    let declTyparArity = arityOfMetaName name
                    let declArgs, _ = recoverOpenTypars declTyparArity 0 openFieldTy memberTy

                    let tref =
                        match externalClassRef (SymbolKey.Type declKey) with
                        | ValueSome t -> t
                        | ValueNone ->
                            failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

                    externalTypeSpec (SymbolKey.Type declKey) tref declArgs

            let s = BlobBuilder()
            encodeType (BlobEncoder(s).FieldSignature()) openFieldTy

            let handle = toEntity (ctx.MemberRef(parent, fieldName, s))
            externalMemberCache.[memoKey] <- handle
            handle

    /// Mint the `MemberRef` for a referenced-assembly record's `.ctor`, instantiated at `args`.
    /// Parameter types are the declared fields in their *open* typar form.
    let externalRecordCtor (key: SymbolKey) (args: FrozenType list) : EntityHandle voption =
        let arity = List.length args

        match externalRecordRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            let parent = externalTypeSpec key tref args

            // The fields in their *open* (`FTTypar(Declaring, i)`) form, read straight off the
            // descriptor template — no closure run on marker typars.
            let paramTys = [ for f in fields -> f.Frozen ]

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

            ValueSome(toEntity (ctx.MemberRef(parent, ".ctor", s)))

    /// Mint the `MemberRef` for a referenced-assembly union's case factory — the static method
    /// `<caseName>(fields…) : Union<…>` the union emitter writes (`NominalEmit.fs`). The mirror of
    /// `externalRecordCtor` for cross-package case construction (`Some` / `None`): parameter types are
    /// the case's declared fields in their *open* typar form and the
    /// return type is the union itself, both written over fresh marker typars so the signature matches the
    /// emitted generic factory. Returns the handle + the field count. `ValueNone` ⇒ the union (or the case)
    /// is unknown to the provider, in which case the caller falls back to its hard error.
    let externalUnionFactory
        (key: SymbolKey)
        (caseName: string)
        (args: FrozenType list)
        : (EntityHandle * int) voption =
        let fullName = SymbolKeyOps.qualifiedName key
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | None -> ValueNone
            | Some case ->
                let parent = externalTypeSpec key tref args

                // The case fields in their *open* (`FTTypar(Declaring, i)`) form, read straight off the
                // descriptor template; the return type is the union itself over the same open markers, so
                // the signature matches the emitted generic factory.
                let paramTys = List.ofArray case.FrozenFieldTypes

                let retTy =
                    FTUnion(
                        SymbolKeyOps.qualifiedTypeKeyOfT fullName arity,
                        EqArray.ofArray [| for i in 0 .. arity - 1 -> FTTypar(TyparAxis.Declaring, i) |]
                    )

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

                ValueSome(toEntity (ctx.MemberRef(parent, caseName, s)), List.length paramTys)

    /// Mint the `_tag : int` field `MemberRef` on a referenced-package union, instantiated at `args`, and
    /// return it with the discriminator value for `caseName` (its zero-based index in declaration order).
    /// The discriminator a cross-package `match` reads: the field name + type mirror the union emitter
    /// (`NominalEmit.fs`: a public `_tag` of type `int`, and tags assigned by case declaration order).
    /// `_tag` is non-generic, so its signature needs no marker typars even on a generic union.
    /// `ValueNone` ⇒ the union (or the case) is unknown to the provider.
    let externalUnionTag (key: SymbolKey) (args: FrozenType list) (caseName: string) : (EntityHandle * int) voption =
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFindIndex (fun c -> c.Name = caseName) with
            | None -> ValueNone
            | Some tag ->
                let parent = externalTypeSpec key tref args
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) (FTConst(RuntimeNames.intKey, EqArray.empty))
                ValueSome(toEntity (ctx.MemberRef(parent, "_tag", s)), tag)

    /// Mint the `MemberRef` for one field of one case on a referenced-package union — the `<caseName>_<i>`
    /// public field the union emitter writes (`NominalEmit.fs`), instantiated at `args`. The field-extract
    /// slot a cross-package `match … Some x` reads (Gap 2 Layer C); the mirror of `externalRecordField`.
    /// The field's *open* type (its declaring-typar form, `!i`) drives the signature blob so it matches the
    /// generic field definition; the returned `FrozenType` is that type after the use-site substitution.
    /// `ValueNone` ⇒ unknown union / case / field index.
    let externalUnionCaseField
        (key: SymbolKey)
        (args: FrozenType list)
        (caseName: string)
        (fieldIndex: int)
        : (EntityHandle * FrozenType) voption =
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some case when fieldIndex >= 0 && fieldIndex < case.FrozenFieldTypes.Length ->
                let parent = externalTypeSpec key tref args

                // The field's *open* (`FTTypar(Declaring, i)`) template drives the signature blob so it
                // matches the generic field definition; `substituteDeclaring` substitutes the use-site
                // args for the returned (use-site) `FrozenType`.
                let openFieldTy = case.FrozenFieldTypes.[fieldIndex]

                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) openFieldTy

                let handle =
                    toEntity (ctx.MemberRef(parent, sprintf "%s_%d" caseName fieldIndex, s))

                let substitutedTy = substituteDeclaring (List.toArray args) openFieldTy
                ValueSome(handle, substitutedTy)
            | _ -> ValueNone

    /// Mint the `MemberRef` for a referenced-assembly class's constructor, instantiated at `tyArgs` and
    /// picked by call-site arity **and argument types**. Two ctors of the same arity (e.g.
    /// `ArgumentException(string, string)` vs `(string, Exception)`) are disambiguated by re-running the
    /// front end's `pickBestOverload` against the zonked call-site arg types — the chosen `SymbolKey`
    /// isn't carried on `TExpr.New`, so codegen re-picks rather than threading it through. Picking the
    /// wrong same-arity ctor mints a `newobj` whose signature disagrees with the pushed values (a `string`
    /// landing where an `Exception` is expected), which produces a malformed object that faults the CLR
    /// at throw/dispatch time. Falls back to the first arity match when the types can't disambiguate
    /// (single overload, or arg types the metadata params don't equal).
    let externalCtor (key: SymbolKey) (tyArgs: FrozenType list) (argTypes: FrozenType list) : CtorRecipe voption =
        // The member table is genuinely string-keyed — the `.ctor` overload set is
        // looked up by the declaring type's compiled name (the genuine string
        // boundary); only the *type-shape* ref routes through the key funnel.
        let fullName = SymbolKeyOps.qualifiedName key
        let candidates = symbols.TryLookupMembers(fullName, ".ctor")
        let arity = List.length argTypes

        let applicable = candidates |> Array.filter (fun m -> m.Key.ArgSig.Length = arity)

        match applicable with
        | [||] -> ValueNone
        | _ ->
            let chosen =
                let typeArgsArr = tyArgs |> List.toArray
                let argElems = argTypes

                match Passes.UnificationInferOverload.pickBestOverloadFrozen typeArgsArr applicable argElems with
                | ValueSome m -> m
                | ValueNone -> applicable.[0]

            let argSigLen = chosen.Key.ArgSig.Length

            match externalClassRef key with
            | ValueNone -> ValueNone
            | ValueSome tref ->
                let parent = externalTypeSpec key tref tyArgs

                // The ctor's parameters in their *open* (`FTTypar(Declaring, i)`) form, read off the
                // descriptor template's single tupled `Parameters` slot and flattened by the chosen key's
                // `argSig` length, exactly as `mintMemberRef` does.
                let paramTys =
                    match chosen.Signature.Parameters with
                    | FTUnit -> []
                    | FTTuple elems when argSigLen >= 2 && elems.Length = argSigLen -> EqArray.toList elems
                    | p -> [ p ]

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

                let handle = toEntity (ctx.MemberRef(parent, ".ctor", s))

                ValueSome
                    {
                        Handle = handle
                        ArgCount = List.length paramTys
                    }

    /// Mint the `MemberRef` for one named field on a referenced-assembly record, instantiated at `args`.
    /// Returns the field handle plus its *substituted* declared type so a `FieldGet` knows the value
    /// type a subsequent encode expects.
    let externalRecordField
        (key: SymbolKey)
        (args: FrozenType list)
        (fieldName: string)
        : (EntityHandle * FrozenType) voption =
        let arity = List.length args

        match externalRecordRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            match fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | None -> ValueNone
            | Some field ->
                let parent = externalTypeSpec key tref args

                // The field's *open* (`FTTypar(Declaring, i)`) template drives the signature blob;
                // `substituteDeclaring` substitutes the use-site args for the returned (use-site)
                // `FrozenType` a subsequent `FieldGet` encode expects.
                let openFieldTy = field.Frozen

                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) openFieldTy
                let handle = toEntity (ctx.MemberRef(parent, fieldName, s))

                let substitutedTy = substituteDeclaring (List.toArray args) openFieldTy
                ValueSome(handle, substitutedTy)

    /// Mint the `MemberRef` for the parameterless `.ctor()` of a HERITABLE external
    /// base class (`type X = (# class "System.Attribute" #)`), the chain target a
    /// derived class's primary `.ctor` calls. Minted DIRECTLY off the external
    /// `TypeRef` rather than via `externalCtor`'s member harvest: a base ctor is often
    /// `protected` (`System.Attribute::.ctor()`) and may not be surfaced, yet `call`ing
    /// it from a subclass ctor is legal. `ValueNone` ⇒ the key isn't an external class.
    let externalParameterlessBaseCtor (key: SymbolKey) : EntityHandle voption =
        match externalClassRef key with
        | ValueNone -> ValueNone
        | ValueSome tref ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            ValueSome(toEntity (ctx.MemberRef(tref, ".ctor", s)))

    member _.ExternalParameterlessBaseCtor(key) = externalParameterlessBaseCtor key

    /// Resolve an intrinsic-CLASS `inherit` parent — an `FTConst` canon (`exn`) whose
    /// platform repr is a heritable BCL reference class — to its platform external
    /// key (`System.Exception`, the identity base-ctor `MemberRef`s are minted
    /// against) plus its raw `TypeRef` (the derived type's `extends` token).
    /// Heritable-ness is CONTRACT-sourced: only a `(# class "…" #)`-tagged primitive
    /// carries a class surface, so the shape lookup IS the predicate — a value-repr
    /// intrinsic (`int`, `decimal`, `unit`) has none and never matches. (An own-unit
    /// heritable extern never arrives here: it resolves to an `FTClass` base, the
    /// `ExternalClassTypeRef` path.)
    member _.IntrinsicClassBase(canon: SymbolKey) : struct (SymbolKey * EntityHandle) voption =
        match env.Symbols.TryLookupType(SymbolKeyOps.qualifiedName canon) with
        | ValueSome(ExternalTypeShape.Intrinsic {
                                                    Id = { Platform = Some repr }
                                                    Class = ValueSome _
                                                }) ->
            let platformKey = SymbolKeyOps.qualifiedTypeKey repr 0

            match externalClassRef platformKey with
            | ValueSome tref -> ValueSome(struct (platformKey, tref))
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// The raw external `TypeRef` for `key` (an external class), the token a derived
    /// type's `extends` (base-type) column names. A raw `TypeRef` — not a
    /// `TypeSpec`-wrapped one (`TypeToken`) — because the Extends column wants the
    /// bare ref for a non-generic external base. `ValueNone` ⇒ not an external class.
    member _.ExternalClassTypeRef(key) = externalClassRef key

    member _.TryCapabilityBaseMemberKey(key) = tryCapabilityBaseMemberKey key

    member _.ExternalMemberRef(key, isProperty, isStatic, memberTy) =
        externalMemberRef key isProperty isStatic memberTy

    member _.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy) =
        externalMemberRefOn key declTy isProperty isStatic memberTy

    member _.ExternalFieldRef(key, declTy, memberTy) = externalFieldRef key declTy memberTy

    member _.ExternalRecordCtor(key, args) = externalRecordCtor key args

    member _.ExternalUnionFactory(key, caseName, args) = externalUnionFactory key caseName args

    member _.ExternalUnionTag(key, args, caseName) = externalUnionTag key args caseName

    member _.ExternalUnionCaseField(key, args, caseName, fieldIndex) =
        externalUnionCaseField key args caseName fieldIndex

    member _.ExternalCtor(key, tyArgs, argTypes) = externalCtor key tyArgs argTypes

    member _.ExternalRecordField(key, args, fieldName) = externalRecordField key args fieldName

    /// `MethodSpec` instantiating a generic static method — a call site (`fold<int,int>`) or a
    /// recursive self-call (`fold<!!0,!!1>`).
    member _.StaticFnMethodSpec(handle, instTypes) = methodSpec handle instTypes
