namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// Pre:  Unification has settled the TypeVar graph (post-generalisation).
// Post: ctx.Diagnostics carries a Warning per `recv?name` site whose `^TResult`
//       escaped `dynamic` to a concrete type through context.
//
// `d?foo` target-types: an unconstrained context lets the `default ^TResult :
// dynamic` fire (the result stays `dynamic`, chains stay dynamic), but a pinned
// context (`d?foo + 1`, an `int` parameter position) unifies `^TResult` to the
// concrete type BEFORE defaulting, so the default never fires. That is an
// *unchecked assertion* — the compiler cannot verify `d.foo` really is that type —
// and so warrants a warning, mirroring F#'s posture on implicit conversions.
//
// Suppressed by an explicit ascription directly on the `?` expression
// (`(d?foo : int)`), recorded in `ctx.DynamicEscapeSuppressed` by
// `inferTypeAnnotation`. `#nowarn`-number suppression is intentionally out of
// scope (no warning-number infrastructure reaches the semantic diagnostics yet).

module DynamicEscape =

    /// A short display name for the escaped type in the warning message. Scalars
    /// (`int`, `string`) are the overwhelming case; a nominal or anything else
    /// falls back to the structural render.
    let private shown (t: SemType) : string =
        match t with
        | TyConst(key, _) ->
            let (DisplayName name) = SymbolKeyOps.simpleName key
            name
        | TyClass(n, _) -> SymbolKeyOps.typeMetaName n
        | other -> sprintf "%A" other

    let run (ctx: PassContext) : unit =
        for site in ctx.DynamicEscapes do
            if not (ctx.DynamicEscapeSuppressed.Contains site.Key) then
                match Unification.zonk ctx.Store (TyVar site.Root) with
                // Default fired (stayed `dynamic`) or still open (a genuine leak is
                // ResolvedTypes' concern) — no unchecked escape.
                | TyDynamic -> ()
                | TyVar _ -> ()
                | escaped ->
                    let name = shown escaped

                    ctx.Warn(
                        site.Key,
                        sprintf
                            "implicit escape from 'dynamic' to '%s': the compiler cannot verify this member access. Annotate the '?' expression — '(expr : %s)' — to assert the type explicitly."
                            name
                            name
                    )
