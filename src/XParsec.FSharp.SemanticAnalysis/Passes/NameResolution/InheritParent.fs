namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open NameResolutionContainers
open NameResolutionLongIdent
open NameResolutionTypeRefStamp

// Resolving an `inherit` clause's PARENT, at registration time and in the scope the clause is
// written. The parent is an arbitrary written name reached through the same opens-aware engine
// as any other type reference, so the miss cases are as much of the work as the hits.

module NameResolutionInheritParent =

    /// The use site the type named at `li` speaks from: its own place in the file, under
    /// the module and `open`s the registration scan currently stands in.
    let private useSiteOfTypeName (ctx: PassContext) (li: LongIdent<SyntaxToken>) : UseSite =
        ctx.UseSiteAt(NodeKey.ofToken li.Idents.[li.Idents.Length - 1] NodeKind.TypeNamed)

    /// Resolve a named type in an `inherit` clause *argument* position (`inherit
    /// Box<int>(v)`'s `int`) to a best-effort `SemType`: a registration-time mini
    /// translation, so an abbrev-named arg lands as an opaque `TyConst`, expanded later.
    let rec private translateInheritArg
        (ctx: PassContext)
        (typarScope: Map<string, TyVarId>)
        (t: Type<SyntaxToken>)
        : SemType =
        let freshTv () =
            let tv = ctx.NewTypeVar()
            ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)
            TyVar tv

        match t with
        | Type.ParenType(typ = inner) -> translateInheritArg ctx typarScope inner
        | Type.VarType(Typar.Named(ident = id))
        | Type.VarType(Typar.Static(ident = id)) ->
            // A typar in the inherit clause binds to the derived class's prototype
            // TyVar so generic inheritance substitutes correctly at member-lookup
            // time (`type Wrapper<'a>(v: 'a) = inherit Box<'a>(v)`).
            match typarScope.TryFind(ctx.NameOf id) with
            | Some tv -> TyVar tv
            | None -> freshTv ()
        | Type.VarType(Typar.Anon _) -> freshTv ()
        | Type.NamedType li when li.Idents.Length = 1 ->
            resolveInheritArgName ctx (useSiteOfTypeName ctx li) (ctx.NameOf li.Idents.[0]) EqArray.empty
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let targs =
                EqArray.ofList
                    [
                        for a in args do
                            match a with
                            | TypeArg.Type at -> yield translateInheritArg ctx typarScope at
                            | TypeArg.Measure _ -> ()
                    ]

            resolveInheritArgName ctx (useSiteOfTypeName ctx li) (ctx.NameOf li.Idents.[0]) targs
        | Type.SuffixedType(baseType = bt; longIdent = li) when li.Idents.Length = 1 ->
            resolveInheritArgName
                ctx
                (useSiteOfTypeName ctx li)
                (ctx.NameOf li.Idents.[0])
                (EqArray.singleton (translateInheritArg ctx typarScope bt))
        | Type.TupleType(types = types) ->
            TyTuple(EqArray.ofList [ for ty in types -> translateInheritArg ctx typarScope ty ])
        | Type.FunctionType(fromType = f; toType = into) ->
            TyFun(translateInheritArg ctx typarScope f, translateInheritArg ctx typarScope into)
        | _ -> freshTv ()

    and private resolveInheritArgName
        (ctx: PassContext)
        (useSite: UseSite)
        (name: string)
        (args: EqArray<SemType>)
        : SemType =
        match TypeRegistry.tryRecord ctx.Types useSite name with
        | ValueSome info -> TyRecord(info.TypeKey, args)
        | ValueNone ->
            match TypeRegistry.tryUnionBare ctx.Types useSite name with
            | ValueSome info -> TyUnion(info.TypeKey, args)
            | ValueNone ->
                match TypeRegistry.tryClass ctx.Types useSite name with
                | ValueSome info -> TyClass(info.TypeKey, args)
                | ValueNone ->
                    match ctx.Types.IntrinsicKeys.TryGetValue name with
                    | true, k -> TyConst(k, args)
                    | _ ->
                        match
                            tryPickExternalWritten
                                ctx
                                useSite
                                WrittenArity.Any
                                (fun _ shape -> ExternalSymbols.intrinsicCanonOf shape)
                                Qualifier.Bare
                                name
                        with
                        | ValueSome c -> TyConst(c, args)
                        | ValueNone -> TyConst(RuntimeNames.opaqueKey name, EqArray.empty)

    /// An `inherit` parent that a provider answers for, rather than the project-local type
    /// registry. Already discriminated: the arm a caller takes is fixed here, so no caller
    /// re-inspects a surface to choose one.
    [<RequireQualifiedAccess>]
    type private ProviderBase =
        /// A class published by a prior file of this assembly, or by a reference.
        | Class of TypeKey
        /// A heritable primitive whose contract declares `.ctor`s (`exn`): the derived class
        /// inherits by CANON, and an `inherit` argument list checks against those ctors.
        | HeritableCanon of IntrinsicIdentity
        /// A heritable primitive with no declared ctor (`Attribute`): the derived class
        /// inherits the PLATFORM type the identity names.
        | HeritablePlatform of IntrinsicIdentity
        /// An external interface, which `inherit` cannot name.
        | Interface

    /// A published type at the written arity, else a heritable primitive. Any other shape
    /// declines, so the open-prefix scan continues past it.
    let private providerBaseOf (arity: int) (key: TypeKey) (shape: ExternalTypeShape) : ProviderBase voption =
        match shape with
        | ExternalTypeShape.Class info when info.TyparArity = arity ->
            if info.IsInterface then
                ValueSome ProviderBase.Interface
            else
                ValueSome(ProviderBase.Class key)
        | _ ->
            ExternalSymbols.intrinsicClassOf shape
            |> ValueOption.map (fun (struct (id, surface)) ->
                if surface.Members |> EqArray.exists (fun m -> m.Name = ".ctor") then
                    ProviderBase.HeritableCanon id
                else
                    ProviderBase.HeritablePlatform id
            )

    /// Resolve an `inherit` clause's parent type to a `TyClass` under the derived class's
    /// typar scope. Diagnoses (and returns `ValueNone`) when the parent is a non-class type,
    /// an unknown name, or a multi-segment name.
    let resolveInheritParent
        (ctx: PassContext)
        (typarScope: Map<string, TyVarId>)
        (t: Type<SyntaxToken>)
        : SemType voption =
        let rec nameAndArgs (t: Type<SyntaxToken>) : (LongIdent<SyntaxToken> * SemType list) voption =
            match t with
            | Type.ParenType(typ = inner) -> nameAndArgs inner
            | Type.NamedType li -> ValueSome(li, [])
            | Type.GenericType(longIdent = li; typeArgs = args) ->
                let targs =
                    [
                        for a in args do
                            match a with
                            | TypeArg.Type at -> yield translateInheritArg ctx typarScope at
                            | TypeArg.Measure _ -> ()
                    ]

                ValueSome(li, targs)
            | Type.SuffixedType(baseType = bt; longIdent = li) ->
                ValueSome(li, [ translateInheritArg ctx typarScope bt ])
            | _ -> ValueNone

        let diagnose (tok: SyntaxToken) (kind: Kind) = ctx.Report(tok, kind)

        match nameAndArgs t with
        | ValueNone -> ValueNone
        | ValueSome(li, targs) ->
            let nameTok = li.Idents.[li.Idents.Length - 1]
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeNamed

            if li.Idents.Length <> 1 then
                let qual = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

                diagnose nameTok (Kind.NotYetSupported(sprintf "inheriting from a qualified base type '%s'" qual))
                ValueNone
            else
                let name = ctx.NameOf nameTok

                let notAClass () =
                    diagnose
                        nameTok
                        (Kind.Message(
                            sprintf "Cannot inherit from type '%s', because only classes are inheritable" name
                        ))

                    ValueNone

                let notAnInheritableInterface () =
                    diagnose
                        nameTok
                        (Kind.Message(
                            sprintf
                                "Cannot inherit from interface '%s'; implement it with 'interface %s with'"
                                name
                                name
                        ))

                    ValueNone

                // The contract's ctor-bearing intrinsic surface for the name, as its canon.
                let tryCtorBearingCanon () =
                    match
                        tryPickExternalWritten
                            ctx
                            (ctx.UseSiteAt diagKey)
                            (WrittenArity.Exact targs.Length)
                            (fun _ shape -> ExternalSymbols.intrinsicClassOf shape)
                            Qualifier.Bare
                            name
                    with
                    | ValueSome(struct (id, surface)) when surface.Members |> EqArray.exists (fun m -> m.Name = ".ctor") ->
                        ValueSome(TyConst(id.Canon, EqArray.ofList targs))
                    | _ -> ValueNone

                // A heritable base's platform repr → its external `TyClass`. A sentinel repr
                // (`"!Vesper.Attribute"`) denotes no external type, so a base with declared
                // ctors falls back to inheriting by canon, and only a base with neither gets
                // the "did not resolve" diagnostic.
                let reprToExternalBase (repr: string) =
                    match ExternalSymbols.tryReprTypeAt ctx.Provider repr targs.Length with
                    | ValueSome(struct (extKey, _)) -> ValueSome(TyClass(extKey, EqArray.ofList targs))
                    | ValueNone ->
                        match tryCtorBearingCanon () with
                        | ValueSome t -> ValueSome t
                        | ValueNone ->
                            diagnose
                                nameTok
                                (Kind.Message(
                                    sprintf
                                        "Cannot inherit from external base '%s': its representation '%s' did not resolve to a known external type (is a package dependency missing?)"
                                        name
                                        repr
                                ))

                            ValueNone

                // The name is not a project-local class: a class published by a prior file or a
                // reference, or a heritable primitive published by a provider (`exn`, or a
                // prior file's `(# class … #)` base like `Attribute`).
                let resolveThroughProvider () =
                    // `inherit X` is a name WRITTEN AT A SITE, so it resolves through the same
                    // opens-aware engine as any written type name.
                    match
                        tryPickExternalWritten
                            ctx
                            (ctx.UseSiteAt diagKey)
                            (WrittenArity.Exact targs.Length)
                            (providerBaseOf targs.Length)
                            Qualifier.Bare
                            name
                    with
                    | ValueSome(ProviderBase.Class key) -> ValueSome(TyClass(key, EqArray.ofList targs))
                    | ValueSome(ProviderBase.HeritableCanon id) -> ValueSome(TyConst(id.Canon, EqArray.ofList targs))
                    | ValueSome(ProviderBase.HeritablePlatform id) ->
                        match id.Platform with
                        | IntrinsicPlatform.Repr repr -> reprToExternalBase repr
                        | IntrinsicPlatform.Unsupported target ->
                            diagnose nameTok (Kind.UnsupportedOnTarget(name, target))
                            ValueNone
                    | ValueSome ProviderBase.Interface -> notAnInheritableInterface ()
                    | ValueNone ->
                        // A name the name table knows at any arity is a project-local type of
                        // some other kind; one it does not know is unknown *here*, which
                        // includes a type declared below this group.
                        if TypeRegistry.isTypeNameInScope ctx.Types (ctx.UseSiteAt diagKey) name then
                            notAClass ()
                        else
                            diagnose nameTok (Kind.Message(sprintf "Cannot inherit from unknown type '%s'" name))
                            ValueNone

                match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey) name with
                | ValueSome info when info.IsInterface -> notAnInheritableInterface ()
                | ValueSome info -> ValueSome(TyClass(info.TypeKey, EqArray.ofList targs))
                | ValueNone ->
                    // Heritable-local arm: a `(# class … #)` intrinsic of THIS file. One read
                    // yields both the repr and the `class`-tag verdict. An `inherit` parent is
                    // an ARBITRARY written name (a record, a typo, a provider class), so the
                    // name → key step must be allowed to miss here.
                    let heritableLocalRepr =
                        match TypeRegistry.tryIntrinsicKeyOf ctx.Types name with
                        | ValueNone -> ValueNone
                        | ValueSome canon ->
                            match ctx.Types.IntrinsicReprKeys.TryGetValue canon with
                            | true, repr when repr.Heritable -> ValueSome repr.Platform
                            | _ -> ValueNone

                    match heritableLocalRepr with
                    // The EXTERNAL type the repr denotes, not the opaque value-repr `TyConst`.
                    | ValueSome platform -> reprToExternalBase platform
                    | ValueNone -> resolveThroughProvider ()
