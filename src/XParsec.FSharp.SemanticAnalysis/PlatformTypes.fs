namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Post: ctx.Diagnostics carries an Error per TDecl that MENTIONS an intrinsic the COMPILING
//       target binds no representation for. ANY mention, not only a use that demands the
//       representation, so `nativeptr<'T>` in a signature is as unsupported as `voidptr`.

module PlatformTypes =

    /// The target that binds no representation for `key`, or `ValueNone` when the compiling
    /// target represents it. A capability interface (`disposable` …) is never an `Intrinsic`
    /// and has no value representation, so matching only `Intrinsic` is right, not a gap.
    let private unsupportedOn (ctx: PassContext) (key: SymbolKey) : string voption =
        match ctx.Provider.TryLookupType key with
        // The identity axis is one pattern, so a scalar and a heritable primitive match alike.
        | ValueSome(ExternalTypeShape.Intrinsic {
                                                    Id = {
                                                             Platform = IntrinsicPlatform.Unsupported target
                                                         }
                                                }) -> ValueSome target
        | _ -> ValueNone

    /// Add every nominal name in `t` with no target representation to `acc`, paired with the
    /// target that lacks it. Zonks first, so a `TyVar` already linked to a concrete shape is
    /// judged by that shape.
    let private addUnsupported (ctx: PassContext) (acc: HashSet<string * string>) (t: SemType) : unit =
        let rec go ty =
            match ty with
            | TyConst(key, args) ->
                match unsupportedOn ctx key with
                | ValueSome target -> acc.Add(SymbolKeyOps.intrinsicName key, target) |> ignore
                | ValueNone -> ()

                for a in args do
                    go a
            | ty -> SemType.iterChildren go ty

        go (Unification.zonk ctx.Store t)

    /// Visit every expression / pattern type, plus a `Format` hole's side type, which the
    /// default walker doesn't surface.
    let private buildIter (ctx: PassContext) (acc: HashSet<string * string>) : TastWalk.Iter =
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
    let private addDeclSurface (ctx: PassContext) (acc: HashSet<string * string>) (kind: TTypeKind) : unit =
        let add = addUnsupported ctx acc

        let addField (f: TRecordFieldG<SemType>) = add f.Type

        match kind with
        | TTypeKindG.Interface methods ->
            for m in methods do
                add m.Signature
        | TTypeKindG.Union(cases, _, interfaces) ->
            for c in cases do
                for (_, ty) in c.Fields do
                    add ty

            for (iface, _) in interfaces do
                add iface
        | TTypeKindG.Record(fields, _, interfaces, _) ->
            for f in fields do
                addField f

            for (iface, _) in interfaces do
                add iface
        | TTypeKindG.Class c ->
            for f in c.Fields do
                addField f

            for p in c.CtorParams do
                addField p

            c.BaseType |> ValueOption.iter add

            for (iface, _) in c.Interfaces do
                add iface
        // An enum case value is an integer or string literal, never a typed term.
        | TTypeKindG.Enum _ -> ()

    let private walkDecl (ctx: PassContext) (d: TDecl) : unit =
        let acc = HashSet<string * string>()
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

            // Member bodies a backend lowers alongside the type. Class members are NOT
            // walked: a backend that does not emit them would make flagging a type they
            // name a premature reject.
            match td.Kind with
            | TTypeKindG.Record(_, members, interfaces, _)
            | TTypeKindG.Union(_, members, interfaces) ->
                for m in members do
                    TastWalk.iterExpr iter m.Body

                for (_, ifaceMembers) in interfaces do
                    for m in ifaceMembers do
                        TastWalk.iterExpr iter m.Body
            | _ -> ()

        // One diagnostic per distinct type named; sorted for a stable report order.
        for (name, target) in acc |> Seq.sort do
            ctx.Report(ResolvedTypes.declSite d, Kind.UnsupportedOnTarget(name, target))

    let run (ctx: PassContext) (tast: TastFile) : unit =
        for d in tast.Decls do
            walkDecl ctx d
