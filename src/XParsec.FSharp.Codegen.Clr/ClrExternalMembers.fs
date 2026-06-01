namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// The identity bridge (symbol-resolution-plan §3/§7.2): a resolved external symbol's
/// `Origin`/`SymbolKey` → an `AssemblyRef`/`TypeRef`/`TypeSpec`/`MemberRef`, with no per-member
/// hand-coding. Mints member / constructor / field references and generic-static-method specs against
/// referenced-assembly types.
type internal ClrExternalMembers(env: ClrEnv, enc: ClrEncoder) =
    let ctx = env.Ctx
    let symbols = env.Symbols
    let zonk t = env.Zonk t
    let arityOfMetaName n = env.ArityOfMetaName n
    let decurryTy t = env.DecurryTy t
    let externalClassRef n = env.ExternalClassRef n
    let externalRecordRef fullName arity = env.ExternalRecordRef(fullName, arity)
    let externalUnionRef fullName arity = env.ExternalUnionRef(fullName, arity)
    let encodeType te t = enc.EncodeType(te, t)
    let encodeOpen markerRoots te t = enc.EncodeOpen(markerRoots, te, t)

    let recoverTypeArgs markerRoots openT instT =
        enc.RecoverTypeArgs(markerRoots, openT, instT)

    let externalTypeSpec tref instArgs = enc.ExternalTypeSpec(tref, instArgs)
    let typeSpecOf ty = enc.TypeSpecOf ty

    /// `SymbolKey` (+ instantiation) → minted `MemberRef`, so a member is reified once across a
    /// compilation (mechanism B's codegen memo, §7.2).
    let externalMemberCache = Dictionary<string, EntityHandle>()

    /// Build the member-ref signature blob (property getter, or tupled-flattened method with the BCL
    /// `void`-return fix) over already-resolved `markerRoots` + open signature, and mint it on `parent`.
    /// Shared by `externalMemberRef` (parent recovered by signature match) and `externalMemberRefOn`
    /// (parent encoded straight from the declaring type) — only the parent derivation differs.
    let mintMemberRef
        (parent: EntityHandle)
        (markerRoots: TypeVar list)
        (openSig: SemType)
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
                    (fun (ret: ReturnTypeEncoder) -> encodeOpen markerRoots (ret.Type()) openSig),
                    (fun (_: ParametersEncoder) -> ())
                )
        else
            let rawParams, retTy = decurryTy openSig

            // A .NET method of arity ≥ 2 is modelled tupled (`(p1*…*pN) → ret`, type-args-bug.md
            // Layer 1), so the lone decurried "parameter" is the argument `TyTuple` — flatten it back
            // to N parameters, driven by the chosen key's `argSig` length (authoritative: a genuine
            // single `(int*int)` param has argSig length 1 and stays one parameter). Arity ≤ 1 unchanged.
            let paramTys =
                match rawParams with
                | [ TyConst("unit", _) ] -> []
                | [ TyTuple elems ] when argSigLen >= 2 && elems.Length = argSigLen -> EqArray.toList elems
                | ps -> ps

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    // A `System.Void` return maps to `TyConst "unit"`
                    // (MetadataSymbols §6.1), but a BCL method's `void` is a
                    // genuine `void` slot — encoding it as `FSharp.Core.Unit`
                    // (the value-position `unit` encoding) mints a `MemberRef`
                    // whose signature no external void method matches, so the
                    // runtime fails to bind it (`MissingMethodException`). Emit
                    // `void` directly here (`IDisposable.Dispose`, `List.Add`).
                    (fun (ret: ReturnTypeEncoder) ->
                        match retTy with
                        | TyConst("unit", _) -> ret.Void()
                        | _ -> encodeOpen markerRoots (ret.Type()) retTy
                    ),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeOpen markerRoots (pars.AddParameter().Type()) p
                    )
                )

        toEntity (ctx.MemberRef(parent, metaName, s))

    /// Mint the `MemberRef` for a `TExpr.ExternalMember` (P4). The member's *open* signature is read
    /// from the (key-pinned, provider-cached) `TryLookupMember` over fresh marker typars; the use-site
    /// instantiation is recovered by matching that open form against `memberTy`.
    let externalMemberRef (key: SymbolKey) (isProperty: bool) (isStatic: bool) (memberTy: SemType) : EntityHandle =
        let declKey, memberName, argSig =
            match key with
            | SymbolKey.MemberKey(d, m, a, _) -> d, m, a
            | other -> failwithf "ClrProvider: ExternalMember key is not a MemberKey: %A" other

        let asm, ns, name =
            match declKey with
            | SymbolKey.TypeKey(asm, ns, name) -> asm, ns, name
            | other -> failwithf "ClrProvider: ExternalMember declaring key is not a TypeKey: %A" other

        let instTy = zonk memberTy
        let memoKey = sprintf "%A|%b|%b|%A" key isProperty isStatic instTy

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = if ns = "" then name else ns + "." + name

            let markers = [ for _ in 1 .. arityOfMetaName name -> TypeVar() ]
            let markerRoots = markers |> List.map UnionFind.find
            let markerTys = markers |> List.map TyVar |> List.toArray

            // Build the open signature from the *exact* overload the front end committed (its key, incl.
            // `argSig`, matches `key`) — NOT a singular re-pick, which would re-collapse a resolved
            // overload back to the most-params one and disagree with the node's `memberTy`
            // (type-args-bug.md Layer 2). The singular `TryLookupMember` is the fallback for providers
            // exposing only that surface.
            let openSig =
                let chosen =
                    match
                        symbols.TryLookupMembers(declFullName, memberName)
                        |> Array.tryFind (fun m -> m.Key = key)
                    with
                    | Some m -> ValueSome m
                    | None -> symbols.TryLookupMember(declFullName, memberName)

                match chosen with
                | ValueSome m -> m.BuildSignature markerTys
                | ValueNone ->
                    failwithf "ClrProvider: external member '%s.%s' did not resolve at emit" declFullName memberName

            let instArgs = recoverTypeArgs markerRoots openSig instTy

            let tref =
                match externalClassRef declFullName with
                | ValueSome t -> t
                | ValueNone ->
                    failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

            let parent = externalTypeSpec tref (List.map zonk instArgs)

            let handle =
                mintMemberRef parent markerRoots openSig isProperty isStatic argSig.Length memberName

            externalMemberCache.[memoKey] <- handle
            handle

    /// `externalMemberRef` for a member whose declaring type's instantiation cannot be recovered from
    /// the member's *open* signature — a T-free member like `MoveNext(): bool` on a generic enumerator
    /// (§4.4). Instead of `recoverTypeArgs`, the declaring instantiation is read straight off `declTy`
    /// (the resolved `enumeratorTy`, e.g. `List`1+Enumerator<int>`); its `args` length sets the marker
    /// count (NOT `arityOfMetaName`, which yields 0 for a nested `…List`1+Enumerator` name). The parent
    /// `TypeSpec` is encoded through `enc.TypeSpecOf`, so a struct enumerator's parent lands as a
    /// `VALUETYPE` generic-inst (and nested-correct via the fixed `externalClassRef`).
    let externalMemberRefOn
        (key: SymbolKey)
        (declTy: SemType)
        (isProperty: bool)
        (isStatic: bool)
        (memberTy: SemType)
        : EntityHandle =
        let declKey, memberName, argSig =
            match key with
            | SymbolKey.MemberKey(d, m, a, _) -> d, m, a
            | other -> failwithf "ClrProvider: ExternalMember key is not a MemberKey: %A" other

        let ns, name =
            match declKey with
            | SymbolKey.TypeKey(_, ns, name) -> ns, name
            | other -> failwithf "ClrProvider: ExternalMember declaring key is not a TypeKey: %A" other

        let declZ = zonk declTy

        let declArgs =
            match declZ with
            | TyClass(_, a)
            | TyUnion(_, a)
            | TyRecord(_, a) -> EqArray.toList a
            | _ -> []

        let instTy = zonk memberTy
        let memoKey = sprintf "on|%A|%A|%b|%b|%A" key declZ isProperty isStatic instTy

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = if ns = "" then name else ns + "." + name

            // Marker count = the declaring type's generic arity, read off `declTy`'s own args (the
            // resolved enumerator instantiation), since `arityOfMetaName` returns 0 for a nested
            // `…List`1+Enumerator` name. `BuildSignature`/`encodeOpen` then see exactly these `!i` slots.
            let markers = [ for _ in 1 .. List.length declArgs -> TypeVar() ]
            let markerRoots = markers |> List.map UnionFind.find
            let markerTys = markers |> List.map TyVar |> List.toArray

            let openSig =
                let chosen =
                    match
                        symbols.TryLookupMembers(declFullName, memberName)
                        |> Array.tryFind (fun m -> m.Key = key)
                    with
                    | Some m -> ValueSome m
                    | None -> symbols.TryLookupMember(declFullName, memberName)

                match chosen with
                | ValueSome m -> m.BuildSignature markerTys
                | ValueNone ->
                    failwithf "ClrProvider: external member '%s.%s' did not resolve at emit" declFullName memberName

            // The parent is the declaring type encoded directly (value-type / nested correct), not
            // recovered+rebuilt — that is the whole point of this entry point.
            let parent = typeSpecOf declZ

            let handle =
                mintMemberRef parent markerRoots openSig isProperty isStatic argSig.Length memberName

            externalMemberCache.[memoKey] <- handle
            handle

    /// Mint the `MemberRef` for a referenced-assembly record's `.ctor`, instantiated at `args`.
    /// Parameter types are the declared fields in their *open* typar form.
    let externalRecordCtor (fullName: string) (args: SemType list) : EntityHandle voption =
        let arity = List.length args

        match externalRecordRef fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            let parent = externalTypeSpec tref (List.map zonk args)

            let markers = [ for _ in 1..arity -> TypeVar() ]
            let markerRoots = markers |> List.map UnionFind.find
            let markerTys = markers |> List.map TyVar |> List.toArray
            let paramTys = [ for f in fields -> f.BuildType markerTys ]

            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeOpen markerRoots (pars.AddParameter().Type()) p
                    )
                )

            ValueSome(toEntity (ctx.MemberRef(parent, ".ctor", s)))

    /// Mint the `MemberRef` for a referenced-assembly union's case factory — the static method
    /// `<caseName>(fields…) : Union<…>` the union emitter writes (`NominalEmit.fs`). The mirror of
    /// `externalRecordCtor` for cross-package case construction (`Some` / `None`, vesper-lib-test-plan
    /// Gap 2 Layer B): parameter types are the case's declared fields in their *open* typar form and the
    /// return type is the union itself, both written over fresh marker typars so the signature matches the
    /// emitted generic factory. Returns the handle + the field count. `ValueNone` ⇒ the union (or the case)
    /// is unknown to the provider, in which case the caller falls back to its hard error.
    let externalUnionFactory (fullName: string) (caseName: string) (args: SemType list) : (EntityHandle * int) voption =
        let arity = List.length args

        match externalUnionRef fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | None -> ValueNone
            | Some case ->
                let parent = externalTypeSpec tref (List.map zonk args)

                let markers = [ for _ in 1..arity -> TypeVar() ]
                let markerRoots = markers |> List.map UnionFind.find
                let markerTys = markers |> List.map TyVar |> List.toArray
                let paramTys = [ for b in case.BuildFieldTypes -> b markerTys ]

                let retTy =
                    TyUnion(ExternalSymbols.qualifiedTypeKey fullName arity, EqArray.ofArray markerTys)

                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = false)
                    .Parameters(
                        List.length paramTys,
                        (fun (ret: ReturnTypeEncoder) -> encodeOpen markerRoots (ret.Type()) retTy),
                        (fun (pars: ParametersEncoder) ->
                            for p in paramTys do
                                encodeOpen markerRoots (pars.AddParameter().Type()) p
                        )
                    )

                ValueSome(toEntity (ctx.MemberRef(parent, caseName, s)), List.length paramTys)

    /// Mint the `_tag : int` field `MemberRef` on a referenced-package union, instantiated at `args`, and
    /// return it with the discriminator value for `caseName` (its zero-based index in declaration order).
    /// The discriminator a cross-package `match` reads (vesper-lib-test-plan Gap 2 Layer C): the field name
    /// + type mirror the union emitter (`NominalEmit.fs`: a public `_tag` of type `int`, and tags assigned
    /// by case declaration order). `_tag` is non-generic, so its signature needs no marker typars even on a
    /// generic union. `ValueNone` ⇒ the union (or the case) is unknown to the provider.
    let externalUnionTag (fullName: string) (args: SemType list) (caseName: string) : (EntityHandle * int) voption =
        let arity = List.length args

        match externalUnionRef fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFindIndex (fun c -> c.Name = caseName) with
            | None -> ValueNone
            | Some tag ->
                let parent = externalTypeSpec tref (List.map zonk args)
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) (TyConst("int", EqArray.empty))
                ValueSome(toEntity (ctx.MemberRef(parent, "_tag", s)), tag)

    /// Mint the `MemberRef` for one field of one case on a referenced-package union — the `<caseName>_<i>`
    /// public field the union emitter writes (`NominalEmit.fs`), instantiated at `args`. The field-extract
    /// slot a cross-package `match … Some x` reads (Gap 2 Layer C); the mirror of `externalRecordField`.
    /// The field's *open* type (its declaring-typar form, `!i`) drives the signature blob so it matches the
    /// generic field definition; the returned `SemType` is that type after the use-site substitution.
    /// `ValueNone` ⇒ unknown union / case / field index.
    let externalUnionCaseField
        (fullName: string)
        (args: SemType list)
        (caseName: string)
        (fieldIndex: int)
        : (EntityHandle * SemType) voption =
        let arity = List.length args

        match externalUnionRef fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some case when fieldIndex >= 0 && fieldIndex < case.BuildFieldTypes.Length ->
                let parent = externalTypeSpec tref (List.map zonk args)
                let markers = [ for _ in 1..arity -> TypeVar() ]
                let markerRoots = markers |> List.map UnionFind.find
                let markerTys = markers |> List.map TyVar |> List.toArray
                let openFieldTy = case.BuildFieldTypes.[fieldIndex] markerTys

                let s = BlobBuilder()
                encodeOpen markerRoots (BlobEncoder(s).FieldSignature()) openFieldTy

                let handle =
                    toEntity (ctx.MemberRef(parent, sprintf "%s_%d" caseName fieldIndex, s))

                let substitutedTy = case.BuildFieldTypes.[fieldIndex](List.toArray args)
                ValueSome(handle, substitutedTy)
            | _ -> ValueNone

    /// Mint the `MemberRef` for a referenced-assembly class's constructor, instantiated at `tyArgs` and
    /// picked by call-site arity **and argument types**. Two ctors of the same arity (e.g.
    /// `ArgumentException(string, string)` vs `(string, Exception)`) are disambiguated by re-running the
    /// front end's `pickStaticOverload` against the zonked call-site arg types — the chosen `SymbolKey`
    /// isn't carried on `TExpr.New`, so codegen re-picks rather than threading it through. Picking the
    /// wrong same-arity ctor mints a `newobj` whose signature disagrees with the pushed values (a `string`
    /// landing where an `Exception` is expected), which produces a malformed object that faults the CLR
    /// at throw/dispatch time. Falls back to the first arity match when the types can't disambiguate
    /// (single overload, or arg types the metadata params don't equal).
    let externalCtor (fullName: string) (tyArgs: SemType list) (argTypes: SemType list) : CtorRecipe voption =
        let candidates = symbols.TryLookupMembers(fullName, ".ctor")
        let arity = List.length argTypes

        let applicable =
            candidates
            |> Array.filter (fun m ->
                match m.Key with
                | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length = arity
                | _ -> false
            )

        match applicable with
        | [||] -> ValueNone
        | _ ->
            let chosen =
                let typeArgsArr = tyArgs |> List.map zonk |> List.toArray
                let argElems = argTypes |> List.map zonk

                match Passes.UnificationInferOverload.pickStaticOverload typeArgsArr applicable argElems with
                | ValueSome m -> m
                | ValueNone -> applicable.[0]

            let argSigLen =
                match chosen.Key with
                | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length
                | _ -> 0

            match externalClassRef fullName with
            | ValueNone -> ValueNone
            | ValueSome tref ->
                let parent = externalTypeSpec tref (List.map zonk tyArgs)
                let typeArity = arityOfMetaName fullName
                let markers = [ for _ in 1..typeArity -> TypeVar() ]
                let markerRoots = markers |> List.map UnionFind.find
                let markerTys = markers |> List.map TyVar |> List.toArray
                let openSig = chosen.BuildSignature markerTys
                let rawParams, _ = decurryTy openSig

                let paramTys =
                    match rawParams with
                    | [ TyConst("unit", _) ] -> []
                    | [ TyTuple elems ] when argSigLen >= 2 && elems.Length = argSigLen -> EqArray.toList elems
                    | ps -> ps

                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = true)
                    .Parameters(
                        List.length paramTys,
                        (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                        (fun (pars: ParametersEncoder) ->
                            for p in paramTys do
                                encodeOpen markerRoots (pars.AddParameter().Type()) p
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
        (fullName: string)
        (args: SemType list)
        (fieldName: string)
        : (EntityHandle * SemType) voption =
        let arity = List.length args

        match externalRecordRef fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            match fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | None -> ValueNone
            | Some field ->
                let parent = externalTypeSpec tref (List.map zonk args)
                let markers = [ for _ in 1..arity -> TypeVar() ]
                let markerRoots = markers |> List.map UnionFind.find
                let markerTys = markers |> List.map TyVar |> List.toArray
                let openFieldTy = field.BuildType markerTys

                let s = BlobBuilder()
                encodeOpen markerRoots (BlobEncoder(s).FieldSignature()) openFieldTy
                let handle = toEntity (ctx.MemberRef(parent, fieldName, s))

                let substitutedTy = field.BuildType(List.toArray args)
                ValueSome(handle, substitutedTy)

    /// `MethodSpec` instantiating a generic static method (R3) — a call site (`fold<int,int>`) or a
    /// recursive self-call (`fold<!!0,!!1>`).
    let staticFnMethodSpec (handle: EntityHandle) (instTypes: SemType list) : EntityHandle =
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(List.length instTypes)

        for t in instTypes do
            encodeType (specEnc.AddArgument()) (zonk t)

        toEntity (ctx.MethodSpec(handle, inst))

    member _.ExternalMemberRef(key, isProperty, isStatic, memberTy) =
        externalMemberRef key isProperty isStatic memberTy

    member _.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy) =
        externalMemberRefOn key declTy isProperty isStatic memberTy

    member _.ExternalRecordCtor(fullName, args) = externalRecordCtor fullName args

    member _.ExternalUnionFactory(fullName, caseName, args) =
        externalUnionFactory fullName caseName args

    member _.ExternalUnionTag(fullName, args, caseName) = externalUnionTag fullName args caseName

    member _.ExternalUnionCaseField(fullName, args, caseName, fieldIndex) =
        externalUnionCaseField fullName args caseName fieldIndex

    member _.ExternalCtor(fullName, tyArgs, argTypes) = externalCtor fullName tyArgs argTypes

    member _.ExternalRecordField(fullName, args, fieldName) =
        externalRecordField fullName args fieldName

    member _.StaticFnMethodSpec(handle, instTypes) = staticFnMethodSpec handle instTypes
