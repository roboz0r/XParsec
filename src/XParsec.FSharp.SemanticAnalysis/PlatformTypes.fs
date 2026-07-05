namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis.Passes

// Pre:  Unification + Elaborate have settled every type to its ground SemType.
// Post: ctx.Diagnostics carries an Error per TDecl that references a primitive
//       with NO representation on the COMPILING target.
//
// This is the same class of error as an unresolved generic (`ResolvedTypes`): a
// type the back end cannot lower. The target's representation knowledge already
// rides the provider — a SCALAR primitive the target ships no `(# … #)` companion
// for surfaces as `ExternalTypeShape.Intrinsic(arity = 0, platform = None)`
// (`decimal`, `nativeint`/pointers on JS). So the verdict is read straight off the
// provider and reported HERE, as a graceful semantic diagnostic, rather than as a
// `failwith` deep in one backend's emitter (the JS code never has to re-walk the
// frozen tree to rediscover it). Target-AGNOSTIC: on a target where every
// primitive has a representation (CLR — base `.fs` IS the platform repr) no
// `Intrinsic` is `platform = None`, so this never fires.
//
// The `arity = 0` guard is load-bearing: the structural type constructors are
// intrinsics too (`'T []`/`byref`, arity ≥ 1) and ship no per-target `(# … #)`
// overlay (a JS array needs no repr string — `FreezeExpr` lowers `'T []` straight
// to `FTConst("[]")`), so they too carry `platform = None`. But a generic intrinsic
// is representable BY CONSTRUCTION — only its element type can be unrepresentable,
// and the walker already recurses into a `TyConst`'s args, so `decimal[]` still
// flags `decimal` while `int[]` passes. Keying on `arity` (recorded on the shape)
// makes the rule total over its domain — it no longer depends on whether `[]`
// happens to miss `tryRuntimeType`'s name probe.

module PlatformTypes =

    /// `true` when `name` resolves to a NULLARY primitive the provider declares has
    /// no representation on the compiling target. A generic intrinsic (`'T []`) is
    /// representable structurally regardless of its own `platform` face, so it is
    /// never flagged here (its args are judged by the caller's recursion). Resolved
    /// over the provider's stable `AmbientOpenPrefixes` (`ExternalSymbols.tryRuntimeType`)
    /// — NOT the per-element mutable `OpenScope`, which is meaningless in this
    /// end-of-pipeline whole-file pass.
    ///
    /// A capability interface (`disposable` …) is a `Class`, not an `Intrinsic`, so it is
    /// never matched here even when its `CapabilityFace` is `ValueNone` (JS): an interface
    /// has no value representation, so "no platform repr" is correct, not a gap. Keep this
    /// match `Intrinsic`-only — do NOT broaden it to flag interface `Class`es.
    let private isUnrepresentable (ctx: PassContext) (name: string) : bool =
        match ExternalSymbols.tryRuntimeType ctx.Provider name with
        | ValueSome(ExternalTypeShape.Intrinsic(arity = 0; platform = None)) -> true
        | _ -> false

    /// Add every nominal name in `t` (and its type args) with no target representation
    /// to `acc`. `zonk` first so a `TyVar` linked to a concrete shape is resolved, the
    /// same chase `Freeze` does before it lowers the type.
    let private addUnrepresentable (ctx: PassContext) (acc: HashSet<string>) (t: SemType) : unit =
        let rec go ty =
            match ty with
            | TyConst(n, args) ->
                if isUnrepresentable ctx n then
                    acc.Add n |> ignore

                for a in args do
                    go a
            // Every other node is judged through its children (a child anywhere —
            // the type-level computations included — could name an unrepresentable
            // platform type); leaves contribute nothing.
            | ty -> SemType.iterChildren go ty

        go (Unification.zonk t)

    /// Visit every expression / pattern type, plus a `Format` hole's side type (which
    /// the default walker doesn't surface) — the same coverage `ResolvedTypes` uses.
    let private buildIter (ctx: PassContext) (acc: HashSet<string>) : TastWalk.Iter =
        { TastWalk.identityIter with
            VisitExpr =
                fun it e ->
                    addUnrepresentable ctx acc (TastWalk.exprTy e)

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
                                addUnrepresentable ctx acc hole.Ty
                                TastWalk.iterExpr it arg
                            | FormatSeg.DynHole d ->
                                addUnrepresentable ctx acc d.Spec.Ty
                                d.Width |> ValueOption.iter (TastWalk.iterExpr it)
                                d.Precision |> ValueOption.iter (TastWalk.iterExpr it)
                                TastWalk.iterExpr it d.Value
                            | FormatSeg.CallbackHole(spec, callback, value) ->
                                addUnrepresentable ctx acc spec.Ty
                                TastWalk.iterExpr it callback
                                value |> ValueOption.iter (TastWalk.iterExpr it)

                        false
                    | _ -> true
            VisitPat =
                fun _ p ->
                    addUnrepresentable ctx acc (TastWalk.patTy p)
                    true
        }

    /// Best-effort attribution NodeKey, mirroring `ResolvedTypes.declKey`: the binding
    /// site for a `NamedSimple` let, else a synthetic-at-0 key.
    let private declKey (d: TDecl) : NodeKey =
        match d with
        | TDecl.Let(TPat.NamedSimple(k, _, _), _, _, _) -> k
        | _ -> NodeKey(0UL)

    let private walkDecl (ctx: PassContext) (d: TDecl) : unit =
        let acc = HashSet<string>()
        let iter = buildIter ctx acc

        match d with
        | TDecl.Let(binding, value, _, ty) ->
            addUnrepresentable ctx acc ty
            TastWalk.iterPat iter binding
            TastWalk.iterExpr iter value
        | TDecl.Expression(e, ty) ->
            addUnrepresentable ctx acc ty
            TastWalk.iterExpr iter e
        | TDecl.Type td ->
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
            | TTypeKindG.Record(_, members, interfaces)
            | TTypeKindG.Union(_, members, interfaces) ->
                for m in members do
                    TastWalk.iterExpr iter m.Body

                for (_, ifaceMembers) in interfaces do
                    for m in ifaceMembers do
                        TastWalk.iterExpr iter m.Body
            | _ -> ()

        if acc.Count > 0 then
            let names = acc |> Seq.sort |> String.concat ", "

            ctx.Diagnostics.Add
                {
                    Key = declKey d
                    Message =
                        sprintf
                            "PlatformTypes: type(s) with no representation on the target platform: %s — they exist only as a .NET/BCL runtime type"
                            names
                    Code = ""
                    Severity = Severity.Error
                }

    let run (ctx: PassContext) (tast: TastFile) : unit =
        for d in tast.Decls do
            walkDecl ctx d
