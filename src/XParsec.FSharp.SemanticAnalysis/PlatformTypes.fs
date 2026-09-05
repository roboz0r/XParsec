namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Post: ctx.Diagnostics carries an Error per TDecl that MENTIONS an intrinsic the COMPILING
//       target binds no representation for. ANY mention, not only a use that demands the
//       representation, so `nativeptr<'T>` in a signature is as unsupported as `voidptr`.

module PlatformTypes =

    /// One nominal a decl mentions that the target has no representation for. Field-ordered
    /// comparison, so sorting the accumulated set IS the report order.
    type private Unsupported = { TypeName: string; Target: string }

    /// The target that binds no representation for `key`, or `ValueNone` when the compiling
    /// target represents it. A capability interface (`disposable` …) is never an `Intrinsic`
    /// and has no value representation, so matching only `Intrinsic` is right, not a gap.
    let private unsupportedOn (ctx: PassContext) (key: TypeKey) : string voption =
        match ctx.Provider.TryLookupType key with
        // The identity axis is one pattern, so a scalar and a heritable primitive match alike.
        | ValueSome(ExternalTypeShape.Intrinsic {
                                                    Id = {
                                                             Platform = IntrinsicPlatform.Unsupported target
                                                         }
                                                }) -> ValueSome target
        | ValueSome _ -> ValueNone
        // A language-known primitive with NO declaration anywhere — no contract entry and no
        // local `(# … #)` binding — is the target not supporting it: its key mints from a
        // literal token or a bare written name, never from a declaration.
        | ValueNone when
            RuntimeNames.isTargetOptionalPrimitiveKey key
            && not (ctx.Types.IntrinsicKeys.ContainsKey key.Name)
            ->
            ValueSome ctx.Target
        | ValueNone -> ValueNone

    /// Accumulate every nominal in `t` the target has no representation for. Zonks first, so
    /// a `TyVar` already linked to a concrete shape is judged by that shape.
    let private addUnsupported (ctx: PassContext) (acc: HashSet<Unsupported>) (t: SemType) : unit =
        let rec go ty =
            match ty with
            | TyConst(key, args) ->
                match unsupportedOn ctx key with
                | ValueSome target -> acc.Add { TypeName = key.Name; Target = target } |> ignore
                | ValueNone -> ()

                for a in args do
                    go a
            | ty -> SemType.iterChildren go ty

        go (Unification.zonk ctx.Store t)

    /// Visit every expression / pattern type, plus a `Format` hole's side type, which the
    /// default walker doesn't surface.
    let private buildIter (ctx: PassContext) (acc: HashSet<Unsupported>) : TastWalk.Iter =
        { TastWalk.identityIter with
            VisitExpr =
                fun it e ->
                    addUnsupported ctx acc (TastWalk.exprTy e)

                    match e with
                    | TExpr.Format(sink, segments, _, _) ->
                        match sink with
                        | FormatSink.ToWriter(w, _)
                        | FormatSink.ToBuilder w -> TastWalk.iterExpr it w
                        | FormatSink.ToStdOut _
                        | FormatSink.ToStdErr _
                        | FormatSink.ToString -> ()

                        for seg in segments do
                            match seg with
                            | FormatSeg.Lit _ -> ()
                            | FormatSeg.Hole(hole, arg) ->
                                addUnsupported ctx acc hole.Ty
                                TastWalk.iterExpr it arg
                            | FormatSeg.DynHole d ->
                                addUnsupported ctx acc d.Spec.Ty
                                d.Width |> ValueOption.iter (TastWalk.iterExpr it)
                                d.Precision |> ValueOption.iter (TastWalk.iterExpr it)
                                TastWalk.iterExpr it d.Value
                            | FormatSeg.CallbackHole(spec, residue) ->
                                addUnsupported ctx acc spec.Ty
                                TastWalk.iterExpr it residue

                        false
                    | _ -> true
            VisitPat =
                fun _ p ->
                    addUnsupported ctx acc (TastWalk.patTy p)
                    true
        }

    /// The DECLARED type surface: field / case-payload / ctor-parameter / base / interface
    /// / abstract-method types. None of these reaches an expression or pattern, so only the
    /// any-mention rule catches them.
    let private addDeclSurface (ctx: PassContext) (acc: HashSet<Unsupported>) (kind: TTypeKind) : unit =
        let add = addUnsupported ctx acc

        let addField (f: TRecordFieldG<SemType>) = add f.Type

        match kind with
        | TTypeKindG.Interface methods ->
            for m in methods do
                add m.Signature
        | TTypeKindG.Union u ->
            for c in u.Cases do
                for (_, ty) in c.Fields do
                    add ty

            for (iface, _) in u.Interfaces do
                add iface
        | TTypeKindG.Record r ->
            for f in r.Fields do
                addField f

            for (iface, _) in r.Interfaces do
                add iface
        | TTypeKindG.Class c ->
            for f in c.Fields do
                addField f

            for p in c.CtorParams do
                addField p

            c.Base |> ValueOption.iter (fun b -> add (BaseParent.ty b.Parent))

            for (iface, _) in c.Interfaces do
                add iface
        // An enum case value is an integer or string literal, never a typed term.
        | TTypeKindG.Enum _ -> ()
        | TTypeKindG.Abbrev body -> add body
        // A measure term holds only base-measure keys and exponents.
        | TTypeKindG.Measure _ -> ()

    let private walkDecl (ctx: PassContext) (d: TDecl) : unit =
        let acc = HashSet<Unsupported>()
        let iter = buildIter ctx acc

        match d with
        | TDecl.Let(binding, value, _, ty) ->
            addUnsupported ctx acc ty
            TastWalk.iterPat iter binding
            TastWalk.iterExpr iter value
        | TDecl.Expression(e, ty) ->
            addUnsupported ctx acc ty
            TastWalk.iterExpr iter e
        | TDecl.Type td ->
            addDeclSurface ctx acc td.Kind

            // Member bodies a backend lowers alongside the type.
            match td.Kind with
            | TTypeKindG.Record _
            | TTypeKindG.Union _ ->
                for m in TTypeKindG.members td.Kind do
                    TastWalk.iterExpr iter m.Body

                for m in TTypeKindG.interfaceMembers td.Kind do
                    TastWalk.iterExpr iter m.Body
            | TTypeKindG.Class c ->
                for m in c.Members do
                    TastWalk.iterExpr iter m.Body

                for (_, ifaceMembers) in c.Interfaces do
                    for m in ifaceMembers do
                        TastWalk.iterExpr iter m.Body
            | _ -> ()

        // One diagnostic per distinct type named.
        for u in acc |> Seq.sort do
            ctx.Report(ResolvedTypes.declSite d, Kind.UnsupportedOnTarget(u.TypeName, u.Target))

    let run (ctx: PassContext) (tast: TastFile) : unit =
        for d in tast.Decls do
            walkDecl ctx d
