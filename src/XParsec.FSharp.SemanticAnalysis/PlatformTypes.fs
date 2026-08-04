namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Pre:  Unification + Elaborate have settled every type to its ground SemType.
// Post: ctx.Diagnostics carries an Error per TDecl that MENTIONS an intrinsic the
//       COMPILING target binds no representation for.
//
// This is the same class of error as an unresolved generic (`ResolvedTypes`): a
// type the back end cannot lower. The rule is DERIVED, never listed — an intrinsic
// marker (some target's `.fs` binds a `(# … #)` repr for it) whose compiling target
// ships no such body surfaces as `IntrinsicPlatform.Unsupported`, so absence of a body
// IS the statement and adding a target never re-lists what it lacks. The verdict is
// read straight off the provider and reported HERE, as a graceful semantic diagnostic,
// rather than as a `failwith` deep in one backend's emitter (the JS code never has to
// re-walk the frozen tree to rediscover it). On a target where every primitive has a
// representation (CLR — the base `.fs` IS the platform repr) nothing is `Unsupported`,
// so this never fires.
//
// ANY MENTION is the error, not only a use that demands the representation: a
// signature naming `nativeint` on JS is confused whether or not it is ever
// instantiated. So there is no arity carve-out — `nativeptr<'T>` and `ilsigptr<'T>`
// are as unsupported as `voidptr`, and a target that CAN represent a structural
// constructor (`'T []`) says so by binding its repr.

module PlatformTypes =

    /// The target its own declaration names as binding no representation for `key`, or
    /// `ValueNone` when the compiling target represents it. Answered by the receiver's
    /// own resolved `SymbolKey` on the store view: the caller holds the key, and a canon
    /// intrinsic key carries its namespace, so no ambient-prelude re-resolution of a
    /// short name is needed.
    ///
    /// A capability interface (`disposable` …) is an `IntrinsicInterface` (CLR) or a plain
    /// interface `Class` (JS) — never an `Intrinsic` — so it is excluded here BY CONSTRUCTION:
    /// an interface has no value representation, so "no platform repr" is correct, not a gap.
    /// Keep this match `Intrinsic`-only — do NOT broaden it to flag interfaces.
    let private unsupportedOn (ctx: PassContext) (key: SymbolKey) : string voption =
        match ctx.Provider.TryLookupType key with
        // Scalar or heritable primitive alike — the identity axis is one pattern.
        | ValueSome(ExternalTypeShape.Intrinsic {
                                                    Id = {
                                                             Platform = IntrinsicPlatform.Unsupported target
                                                         }
                                                }) -> ValueSome target
        | _ -> ValueNone

    /// Add every nominal name in `t` (and its type args) with no target representation
    /// to `acc`, paired with the target that lacks it. `zonk` first so a `TyVar` linked to
    /// a concrete shape is resolved, the same chase `Elaborate` does before it lowers the
    /// type.
    let private addUnsupported (ctx: PassContext) (acc: HashSet<string * string>) (t: SemType) : unit =
        let rec go ty =
            match ty with
            | TyConst(key, args) ->
                match unsupportedOn ctx key with
                | ValueSome target -> acc.Add(SymbolKeyOps.intrinsicName key, target) |> ignore
                | ValueNone -> ()

                for a in args do
                    go a
            // Every other node is judged through its children (a child anywhere —
            // the type-level computations included — could name an unrepresentable
            // platform type); leaves contribute nothing.
            | ty -> SemType.iterChildren go ty

        go (Unification.zonk ctx.Store t)

    /// Visit every expression / pattern type, plus a `Format` hole's side type (which
    /// the default walker doesn't surface) — the same coverage `ResolvedTypes` uses.
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

    /// The DECLARED type surface of a type declaration — field / case-payload /
    /// ctor-parameter / base / interface / abstract-method types. A mention here never
    /// reaches an expression or pattern, and it is exactly the "signature it never
    /// instantiates" the any-mention rule is about.
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

            // The augmentation member bodies a backend lowers alongside the type.
            // Class members are intentionally NOT walked: a backend may not emit them
            // yet (the JS back end does not), so flagging a type they reference would
            // be a premature reject — match the set the emitter actually lowers.
            // Records and unions walk identically: augmentation member bodies, then
            // interface-impl member bodies. Both kinds' interface impls ARE lowered by
            // the backends (CLR `InterfaceImpl` rows / JS attached + base-class capability
            // methods like `[Symbol.iterator]`), so an unrepresentable type in one is a
            // real reject.
            match td.Kind with
            | TTypeKindG.Record(_, members, interfaces, _)
            | TTypeKindG.Union(_, members, interfaces) ->
                for m in members do
                    TastWalk.iterExpr iter m.Body

                for (_, ifaceMembers) in interfaces do
                    for m in ifaceMembers do
                        TastWalk.iterExpr iter m.Body
            | _ -> ()

        // One diagnostic per distinct type named, so the message can be about the type
        // rather than about a set of them.
        for (name, target) in acc |> Seq.sort do
            ctx.Report(ResolvedTypes.declSite d, Kind.UnsupportedOnTarget(name, target))

    let run (ctx: PassContext) (tast: TastFile) : unit =
        for d in tast.Decls do
            walkDecl ctx d
