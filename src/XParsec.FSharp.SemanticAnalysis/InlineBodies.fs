namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// The SPLICE TEMPLATES a frozen file publishes, collected apart from the signatures that
// publish its declarations. A template is keyed by `SymbolKey` and by nothing else, so a
// signature is free to hide the declaration it was written on without taking it along.

module InlineBodies =

    /// Mint the `this`-first inline `TDecl.Let` for a `member inline`: an accessor
    /// `member inline _.M p0 p1 = body` IS the inline function `M this p0 p1 = body`, `this`
    /// the OUTERMOST curried param (a static member, `ThisKey = ValueNone`, prepends none).
    let liftMemberBody (file: LexedFile) (m: TastAccessor.TypeMember) : InlineBody option =
        if not m.IsInline then
            None
        else
            let pool = m.Body.Pool
            // Every node minted below takes the body's own anchor, so the lifted tree indexes
            // exactly one file and the collection can record ONE declaring file over the whole thing.
            let bodyTok = TastAccessor.exprTok m.Body

            let curried =
                [
                    match m.ThisKey with
                    | ValueSome tk -> yield (tk, m.ThisTy)
                    | ValueNone -> ()

                    for (k, ty) in m.Params do
                        yield (k, ty)
                ]

            let mutable body = m.Body
            let mutable resultTy = m.ReturnTy

            // Fold innermost-last so the outermost lambda's type is the whole curried
            // function (`this -> p0 -> … -> ret`).
            for i = curried.Length - 1 downto 0 do
                let (pk, pty) = curried.[i]
                let lamTy = FTFun(pty, resultTy)
                let param = TastAccessor.mintNamedPat pool (BoundVarKey.identity pk) pty bodyTok
                body <- TastAccessor.mintLambda param body lamTy bodyTok
                resultTy <- lamTy

            let declTy = resultTy
            // `inlineExpand` reads only the value, so this bound variable is filler the decl's
            // shape requires, minted rather than taken from anything.
            let decl =
                TastAccessor.mintLetDecl
                    (TastAccessor.mintNamedPat pool (TastPoolBuilder.mintBoundVar pool) declTy bodyTok)
                    body
                    true
                    false
                    Recursion.NonRecursive
                    declTy

            // One entry per curried position, so `this` takes a leading default. No member
            // param carries a decoded attribute today, so every entry is `ParamAttrs.Default`.
            let paramAttrs = EqArray.init curried.Length (fun _ -> ParamAttrs.Default)

            Some(InlineBody.anchoredIn file (TastPoolBuilder.declTree pool decl.Id) paramAttrs)

    type KeyedInlineBody = { Key: SymbolKey; Body: InlineBody }

    /// One file's templates, split by what keys them: a module binding by its own binding key,
    /// a lifted member by the member key a call site resolves through.
    type FileInlineBodies =
        {
            Values: KeyedInlineBody list
            Members: KeyedInlineBody list
        }

    let empty: FileInlineBodies = { Values = []; Members = [] }

    /// The files' templates flattened in input order, so a later file's template wins a clash
    /// under `index`.
    let concat (files: FileInlineBodies list) : FileInlineBodies =
        {
            Values = files |> List.collect (fun f -> f.Values)
            Members = files |> List.collect (fun f -> f.Members)
        }

    /// Read every splice template out of one frozen file, anchored to the file it was
    /// declared in, since a spliced node resolves only against that file's own text.
    let collect (file: LexedFile) (tast: FrozenPools) : FileInlineBodies =
        // The file's trees as columns, plus an append-only overlay for the wrapper lambdas.
        // The overlay dies with this call.
        let pool = TastPoolBuilder.openOver tast

        let anchored = InlineBody.anchoredIn file

        // Unpooled off their own pool roots: the wire form is DU-typed because a pool id
        // means nothing in the compiling file's pool.
        let values =
            [
                for iv in tast.InlineTemplates ->
                    {
                        Key = iv.Key
                        Body = anchored (TastPoolBuilder.declTree pool iv.Decl) iv.ParamAttrs
                    }
            ]

        let members =
            [
                for d in TastAccessor.roots pool do
                    // A `member inline` on ANY member-bearing host (class / union / record)
                    // is a splice template; `liftMemberBody` skips every other member.
                    match TastAccessor.declKind d with
                    | DeclShape.Type ->
                        let tdecl = TastAccessor.declType d

                        for m in TTypeKindG.members tdecl.Kind do
                            match liftMemberBody file m with
                            | Some body ->
                                let kind = TMemberKind.keyKind m.Kind

                                let key =
                                    SymbolKeyOps.memberKey
                                        tdecl.TypeKey
                                        m.Name
                                        (m.Params |> EqArray.map snd)
                                        m.MethodTypeParams.Length
                                        kind

                                yield { Key = key; Body = body }
                            | None -> ()
                    | _ -> ()
            ]

        { Values = values; Members = members }

    /// The VALUE templates by simple source name, a later template winning a clash. NOT a
    /// resolution channel (a provider folds a body onto the entry that owns its key); the
    /// introspection seam tests assert against.
    let valuesByName (bodies: FileInlineBodies) : Map<string, InlineBody> =
        (Map.empty, bodies.Values)
        ||> List.fold (fun m v -> Map.add (SymbolKeyOps.intrinsicName v.Key) v.Body m)

    /// One file's templates as a flat lookup. A binding key and a member key are distinct
    /// `SymbolKey` cases, so the two halves cannot collide.
    let index (bodies: FileInlineBodies) : SymbolKey -> InlineBody voption =
        let d = Dictionary<SymbolKey, InlineBody>()

        for b in bodies.Values do
            d.[b.Key] <- b.Body

        for b in bodies.Members do
            d.[b.Key] <- b.Body

        fun key ->
            match d.TryGetValue key with
            | true, body -> ValueSome body
            | _ -> ValueNone
