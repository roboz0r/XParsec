namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

// The TAST walker: `TExpr` → IL, via the depth-tracked untyped `Cil` helpers
// (the dynamic compiled-name → recipe dispatch can't preserve the phantom
// stack types across the provider boundary; hand-written bodies that can use
// the typed `Op` surface live in the tests).
//
// Slice 2 grows coverage to arithmetic intrinsics, `let`/`Var` (locals), and
// the *consumption* half of the function-representation problem: applying an
// `FSharpFunc` value that FSharp.Core handed back (e.g. the `int -> unit`
// printer of `printfn "%d"`) via `callvirt Invoke`.
//
// Slice 3 adds `let inline`: an inline binding is collected into a side table
// and emits no value; a call site whose spine head is a `Var` bound to such a
// decl is *expanded* — the retained body is freshened, beta-reduced against
// the call-site args into a `TExpr.Let` chain, and emitted with only slice-2
// mechanics (a local slot + the `add` intrinsic). No function value, no
// `Invoke`, no IL or metadata changes. Closure *synthesis* (passing a
// function as a value) is still out of scope. Unhandled nodes fail loudly
// rather than mis-emit.

module Emit =

    /// The threaded codegen context for one `Main` build. `Slots` maps a
    /// binding `NodeKey` to its local slot; `Inlines` maps an inline binding's
    /// key to its whole decl; `Mint` hands out fresh keys for each expansion's
    /// freshened binders (a build-wide counter, so no two expansions collide).
    /// `il` stays a separate parameter — it is the output buffer, not context.
    type private EmitEnv =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            Slots: Dictionary<NodeKey, int>
            Inlines: Dictionary<NodeKey, TDecl>
            Mint: unit -> NodeKey
        }

    /// The inferred type carried inline on any `TExpr` node.
    let private typeOfExpr (e: TExpr) : SemType =
        match e with
        | TExpr.Const(_, ty) -> ty
        | TExpr.Var(_, ty) -> ty
        | TExpr.External(_, ty) -> ty
        | TExpr.Lambda(_, _, ty) -> ty
        | TExpr.App(_, _, ty) -> ty
        | TExpr.Let(_, _, _, ty) -> ty
        | TExpr.IfThenElse(_, _, _, ty) -> ty
        | TExpr.Tuple(_, ty) -> ty
        | TExpr.Sequential(_, ty) -> ty
        | TExpr.While(_, _, ty) -> ty
        | TExpr.ForTo(_, _, _, _, ty) -> ty
        | TExpr.ForIn(_, _, _, ty) -> ty
        | TExpr.Match(_, _, ty) -> ty
        | TExpr.TryWith(_, _, ty) -> ty
        | TExpr.TryFinally(_, _, ty) -> ty
        | TExpr.Assignment(_, _, ty) -> ty
        | TExpr.Null ty -> ty
        | TExpr.Range(_, _, _, ty) -> ty
        | TExpr.RecordCons(_, ty) -> ty
        | TExpr.RecordClone(_, _, ty) -> ty
        | TExpr.FieldGet(_, _, ty) -> ty
        | TExpr.FieldSet(_, _, _, ty) -> ty
        | TExpr.UnionCons(_, _, ty) -> ty
        | TExpr.New(_, _, ty) -> ty
        | TExpr.MethodCall(_, _, _, ty) -> ty
        | TExpr.PropertyGet(_, _, ty) -> ty
        | TExpr.StaticMethodCall(_, _, _, ty) -> ty
        | TExpr.StaticPropertyGet(_, _, ty) -> ty

    /// Peel a curried `App` chain into its head and the arguments paired with
    /// each `App` node's *result* type. The result types drive `Invoke`
    /// instantiation: the type after applying arg `i` is the function value
    /// (or final value) on the stack when arg `i+1` is applied.
    let rec private collectSpine (acc: (TExpr * SemType) list) (e: TExpr) : TExpr * (TExpr * SemType) list =
        match e with
        | TExpr.App(fn, arg, ty) -> collectSpine ((arg, ty) :: acc) fn
        | head -> head, acc

    /// Run a recipe whose operands are already on the stack, then settle the
    /// tracked depth by its net effect (`Pushes - ArgCount`).
    let private applyRecipe (il: Il) (recipe: CallRecipe) : unit =
        recipe.Emit il
        il.Adjust(recipe.Pushes - recipe.ArgCount)

    /// Beta-reduce a curried lambda (an inline expansion's freshened output)
    /// against its spine arguments, lowering each application to a `TExpr.Let`
    /// that binds the parameter to its argument and wraps the next reduction.
    /// Reusing `Let` keeps one lowering path that allocates param slots. The
    /// lambda count must equal the spine-arg count: a residual lambda (partial
    /// application) reaches `emitExpr` as a standalone `Lambda` and fails
    /// there; surplus args fail here. Both mean a closure — slice 5.
    let rec private betaReduce (fn: TExpr) (args: (TExpr * SemType) list) : TExpr =
        match fn, args with
        | _, [] -> fn
        | TExpr.Lambda(TPat.NamedSimple(k, paramTy), lamBody, _), (arg, _) :: rest ->
            let reduced = betaReduce lamBody rest
            TExpr.Let(TPat.NamedSimple(k, paramTy), arg, reduced, typeOfExpr reduced)
        | TExpr.Lambda(param, _, _), _ -> failwithf "Emit: inline parameter destructuring is out of scope: %A" param
        | _, _ :: _ -> failwith "Emit: over-application of an inline function (surplus args → closure, slice 5)"

    /// Emit an expression, leaving its value on the stack (one value, or none
    /// for a `unit` literal which has no runtime representation here).
    let rec private emitExpr (env: EmitEnv) (il: Il) (e: TExpr) : unit =
        match e with
        | TExpr.Const(TConstValue.String s, _) -> Cil.emitLdstr il (env.Ctx.UserString s)
        | TExpr.Const(TConstValue.Int n, _) -> Cil.emitLdcI4 il n
        | TExpr.Const(TConstValue.Bool b, _) -> Cil.emitLdcI4 il (if b then 1 else 0)
        | TExpr.Const(TConstValue.Byte n, _) -> Cil.emitLdcI4 il (int n)

        | TExpr.Var(binding, _) ->
            // An inline value reference (`let inline k = 5`) expands in place
            // — check `Inlines` before the slot table so the inline rule is
            // uniform with the call-site case below.
            match env.Inlines.TryGetValue binding with
            | true, decl -> emitExpr env il (Inline.inlineExpand decl [||] |> Inline.freshen env.Mint)
            | false, _ ->
                match env.Slots.TryGetValue binding with
                | true, slot -> Cil.emitLdloc il slot
                | false, _ -> failwithf "Emit: no local slot for variable %O" binding

        | TExpr.Let(TPat.NamedSimple(binding, ty), value, body, _) ->
            // The expression-level twin of `emitMain`'s top-level let, and the
            // lowering target for each beta-reduced inline parameter.
            let slot = il.DeclareLocal ty
            env.Slots.[binding] <- slot
            emitExpr env il value
            Cil.emitStloc il slot
            emitExpr env il body
        | TExpr.Let(pat, _, _, _) -> failwithf "Emit: destructuring let-binding is out of scope: %A" pat

        | TExpr.Lambda _ ->
            // A `Lambda` only legitimately reaches the walker as inline-
            // expansion output that beta reduction immediately consumes;
            // emitting one as a value is closure synthesis (slice 5). A residual
            // lambda from a partially applied inline lands here too.
            failwith
                "Emit: a Lambda reached the walker as a value — closure synthesis (or partial inline application) is slice 5"

        | TExpr.New(className, args, ty) ->
            for a in args do
                emitExpr env il a

            let tyArgs =
                match ty with
                | TyClass(_, xs) -> xs
                | _ -> []

            match env.Provider.TryEmitCtor(className, tyArgs) with
            | ValueSome recipe -> Cil.emitNewobj il recipe.Handle recipe.ArgCount
            | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className

        | TExpr.App _ ->
            let head, spineArgs = collectSpine [] e

            match head with
            | TExpr.External(name, _) ->
                // The recipe's generic instantiation (e.g. the `printfn`
                // printer) is read off the first application's result type.
                let recipeResultTy =
                    match spineArgs with
                    | (_, ty) :: _ -> ty
                    | [] -> typeOfExpr head

                match env.Provider.TryEmitCall(name, recipeResultTy) with
                | ValueSome recipe ->
                    let leading, rest = List.splitAt recipe.ArgCount spineArgs

                    for (a, _) in leading do
                        emitExpr env il a

                    applyRecipe il recipe

                    // Whatever the recipe left on the stack — for a partially
                    // applied call that's a function value the rest of the
                    // spine is applied to.
                    let funcTy =
                        match List.tryLast leading with
                        | Some(_, ty) -> ty
                        | None -> typeOfExpr head

                    foldInvoke env il funcTy rest
                | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

            | TExpr.Var(binding, _) when env.Inlines.ContainsKey binding ->
                // Inline call site: expand the retained body (monomorphic →
                // `[||]`), freshen its binders, beta-reduce against the spine
                // args into a `Let` chain, and emit that. No function value,
                // no `Invoke`.
                let decl = env.Inlines.[binding]
                let expanded = Inline.inlineExpand decl [||] |> Inline.freshen env.Mint
                emitExpr env il (betaReduce expanded spineArgs)

            | TExpr.Var(binding, _) ->
                // A non-inline local of function type applied as a value needs
                // closure synthesis — slice 5.
                failwithf "Emit: applying local function value %O requires a closure (slice 5)" binding

            | _ ->
                // The head is itself a function value: emit it, then apply
                // every argument through `Invoke`.
                emitExpr env il head
                foldInvoke env il (typeOfExpr head) spineArgs

        | other -> failwithf "Emit: unsupported expression in slice 3: %A" other

    /// Apply each remaining argument to the function value on the stack via
    /// `FSharpFunc.Invoke`, threading the running function type so each
    /// `Invoke` is instantiated for the right `a -> b`.
    and private foldInvoke (env: EmitEnv) (il: Il) (funcTy0: SemType) (args: (TExpr * SemType) list) : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitInvoke funcTy with
            | ValueSome recipe ->
                emitExpr env il arg
                applyRecipe il recipe
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to value of type %A" funcTy

    /// Emit a top-level expression decl as a statement: evaluate it and
    /// discard any value it left (top-level `do` expressions are `unit`).
    let private emitStatement (env: EmitEnv) (il: Il) (e: TExpr) : unit =
        emitExpr env il e

        while il.Depth > 0 do
            Cil.emitPop il

    /// Build the `Main` body. A pre-pass collects every `let inline` into the
    /// `Inlines` side table (emitting nothing for them) so an inline used
    /// before its textual definition still resolves. The second pass binds
    /// each non-inline top-level `let` to a `Main` local (its value evaluated
    /// then `stloc`-d) and emits each effectful expression in source order;
    /// then `ldc.i4.0; ret`. Top-level lets are `Main` locals in slice 2/3
    /// (script-style) — the static-field / `.cctor` split is deferred until a
    /// slice needs cross-method visibility. The side tables are local to this
    /// build, keyed by binding NodeKey.
    let emitMain (provider: ICodegenProvider) (ctx: MetadataContext) (decls: TDecl list) (il: Il) : unit =
        let slots = Dictionary<NodeKey, int>()
        let inlines = Dictionary<NodeKey, TDecl>()

        for d in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(binding, _), _, true, _) -> inlines.[binding] <- d
            | _ -> ()

        // Build-wide monotone counter: every freshened binder key is unique
        // across all expansions, so nested call sites don't share a slot.
        let mutable counter = 0

        let mint () =
            let k = NodeKey.ofSynthetic counter NodeKind.SynthInlineExpansion
            counter <- counter + 1
            k

        let env =
            {
                Provider = provider
                Ctx = ctx
                Slots = slots
                Inlines = inlines
                Mint = mint
            }

        for d in decls do
            match d with
            | TDecl.Expression(e, _) -> emitStatement env il e
            | TDecl.Let(TPat.NamedSimple(_, _), _, true, _) -> () // inline: collected above, emits no value
            | TDecl.Let(TPat.NamedSimple(binding, _), value, _, ty) ->
                let slot = il.DeclareLocal ty
                slots.[binding] <- slot
                emitExpr env il value
                Cil.emitStloc il slot
            | TDecl.Let _ -> ()

        Cil.emitLdcI4 il 0
        Cil.emitRet il
