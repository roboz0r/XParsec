namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// A pinned context (`d?foo + 1`, an `int` parameter position) unifies `^TResult` to the
// concrete type BEFORE `default ^TResult : dynamic` can fire — an unchecked assertion that
// `d.foo` really is that type, so each such site warns. Runs after generalisation.

module DynamicEscape =

    let run (ctx: PassContext) : unit =
        for site in ctx.DynamicEscapes do
            if not (ctx.DynamicEscapeSuppressed.Contains site.Node.Key) then
                match Unification.zonk ctx.Store (TyVar site.Root) with
                // Default fired (stayed `dynamic`) or still open — no unchecked escape.
                | TyDynamic -> ()
                | TyVar _ -> ()
                | escaped ->
                    ctx.Report(site.Node.Tok, Kind.DynamicEscape(UnificationEngineCore.shown ctx.Store escaped))
