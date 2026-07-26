namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers
open EmitJsCapabilities
open EmitJsTypes
open EmitJsContext

/// The `TAST → JsAst` walker. Every un-handled node is an explicit `failwithf`,
/// so an unsupported arm fails loudly rather than dropping silently.
///
/// Durable conventions:
///   * Functions are **curried unary arrows** — `Lambda` → nested `(a) => (b) => …`,
///     `App` → unary calls (`f a b` → `f(a)(b)`). Tail self-recursion trampolines to
///     `while (true)` with param-shadow mutation for constant stack.
///   * Operator bodies arrive pre-spliced as `ILIntrinsic` `$N`-templates, already
///     JS-emit-able. `TastLower.lower` drops `type` decls — record/union/member shapes
///     are read off the un-lowered decls in `collectTypes`.
///   * Records/unions emit as data-only JS `class`es (positional ctor; union = base
///     `tag` + one `extends`-subclass per case). Members emit as free, curried,
///     *receiver-first* functions — never prototype methods (match + the structural
///     runtime read `.tag`/own-keys, never `instanceof`).
///   * `Match` lowers to an IIFE testing each arm in order, an unmatched value throwing.
///
/// The `buildExpr`-free foundation (`WalkCtx`, name/type resolution, `compileMatchPattern`)
/// lives in `EmitJsContext`; the printf/format projection in `EmitJsFormat`; the member /
/// capability-method emitters in `EmitJsMembers`. Those reach back into this walker only
/// through the `buildExpr` callback `buildProgram` threads in.
module EmitJs =

    // The Fable-style FLAT module-function helpers — flat-call collapse, the curried
    // adapter, external-`ValRepr` resolution — live in `JsFlatFns`, decoupled from this
    // walker via a `build` callback (mirroring the CLR `EmitCall.flattenGroupPushes`
    // `recur` parameter). The EXTERNAL-member lowerings — provider flags, the `exn`
    // repr climb, the attached / erased / mangled call shapes — live in
    // `JsExternalMembers` on the same seam. Only the trampoline-coupled
    // `emitFlatModuleFn` / `trampolineOrExpr` stay in this recursion group.

    // ---- The walker ----------------------------------------------------------

    let rec buildExpr (ctx: WalkCtx) (e: TastAccessor.ExprId) : JsExpr =
        let loc = locOf ctx (TastAccessor.exprTok e)

        match TastAccessor.exprKind e with
        | ExprShape.Const -> constExpr (TastAccessor.exprConstValue e) loc

        // A bare reference to a local module FUNCTION is a value-use (an escape): it
        // wraps the flat function in an inline curried adapter so a higher-order
        // consumer (or a partial application) sees the SOURCE-shaped currying. A simple
        // single-arg / lone-unit function needs no adapter (flat == curried there).
        | ExprShape.Var ->
            let k = TastAccessor.exprVarBinding e

            let ident =
                JsExpr.Identifier(binderName ctx.Source (TastAccessor.exprVarNaming e), loc)

            match ctx.CompiledFns.TryGetValue k with
            | true, cf when JsFlatFns.needsAdapter cf.Groups -> JsFlatFns.curryAdapter ident cf.Groups k.Offset loc
            | _ -> ident

        // An external module function — imported from its package's JS runtime module.
        // A value-use of a multi-arg / tupled external function gets the same curried
        // adapter (its producer emits flat); a saturated call flattens at the `App` arm.
        | ExprShape.External ->
            let ext = TastAccessor.exprExternal e

            let alias =
                JsExpr.Identifier(
                    JsImports.addRef ctx.Imports ext.CompiledName (externalValueRef ctx.Provider ext.Key),
                    loc
                )

            match JsFlatFns.externalGroups ctx.Provider ext.Key with
            | ValueSome groups when JsFlatFns.needsAdapter groups ->
                JsFlatFns.curryAdapter alias groups (TastAccessor.exprTok e).StartIndex loc
            | _ -> alias

        | ExprShape.IfThenElse ->
            let i = TastAccessor.exprIfThenElse e
            JsExpr.Conditional(buildExpr ctx i.Cond, buildExpr ctx i.ThenExpr, buildExpr ctx i.ElseExpr, loc)

        // A `Sequential` in expression position is a comma expression (top-level it
        // expands to statements via `buildStatements`).
        | ExprShape.Sequential -> JsExpr.Sequence([ for x in TastAccessor.exprChildren e -> buildExpr ctx x ], loc)

        // A tuple `(a, b, …)` is a JS array `[a, b, …]`; a pattern reads elements by index.
        | ExprShape.Tuple -> JsExpr.Array([ for x in TastAccessor.exprChildren e -> buildExpr ctx x ], loc)

        | ExprShape.Let ->
            match e with
            // Pure `let` in expression position: substitute into uses (collapse operator
            // templates). A *mutable* binder (assigned in the body) is excluded — it must
            // stay a real binding so its writes land; it falls to the IIFE arm, where the
            // arrow parameter is the (reassignable) mutable cell.
            | InlinableLet reduced -> buildExpr ctx reduced
            | _ ->
                let l = TastAccessor.exprLet e

                match TastAccessor.patKind l.Binding with
                // Non-pure (or mutable) `let` in expression position: JS has no let-expression,
                // so lowers to an IIFE `((x) => <body>)(<value>)` — the binder evaluated once,
                // and (for a mutable binder) reassignable as the arrow parameter.
                | PatShape.NamedSimple ->
                    let name = binderName ctx.Source (TastAccessor.patBinderNaming l.Binding).Value

                    JsExpr.Call(
                        JsExpr.Arrow([ name ], JsFnBody.Expr(buildExpr ctx l.Body), ValueNone),
                        [ buildExpr ctx l.Value ],
                        loc
                    )

                // `let _ = value in body` — a Wildcard binder discards the value, kept only for
                // its effects (`let _ = renderInto buf` over `|> ignore`, which leaves a bare
                // recipe value). A pure value contributes nothing, so drop it; otherwise a comma
                // sequence evaluates `value` then yields `body` (JS has no let-expression).
                | PatShape.Wildcard when isPureValue l.Value -> buildExpr ctx l.Body
                | PatShape.Wildcard -> JsExpr.Sequence([ buildExpr ctx l.Value; buildExpr ctx l.Body ], loc)
                | _ -> failwithf "EmitJs: unsupported expression %A" e

        // Anonymous lambda — no binder key, so no self-tail-call analysis applies.
        | ExprShape.Lambda -> emitFunction ctx ValueNone e

        // Application. A SATURATED call to a module function (local or external)
        // collapses its whole spine into a single FLAT call (`f(a, b)`, tuple groups
        // flattened, lone unit dropped); any residual over-application folds on as unary
        // calls. Everything else — closures, members, under-applied module functions —
        // keeps the curried `f(a)(b)` shape (one unary call per `App`); an under-applied
        // module function reaches its head's curried adapter through this fallback.
        | ExprShape.App ->
            let av = TastAccessor.exprApp e
            let head, spine = TastAccessor.collectSpine [] e

            // Flat dispatch: a capability-protocol member call (`tryCapabilityCall` —
            // `src.GetEnumerator()`, `e.MoveNext()`, `e.Dispose()`) folds head + the lone
            // `unit` spine element into its JS form; a native attached-member call
            // (`JsExternalMembers.tryAttachedCall`) folds the whole spine into ONE
            // `receiver.member(args)`; else a saturated module-function call collapses to a
            // flat call; anything else keeps the curried unary fallback.
            let folded =
                match tryCapabilityCall ctx.Capabilities ctx.Imports (buildExpr ctx) head spine loc with
                | ValueSome call -> ValueSome call
                | ValueNone -> JsExternalMembers.tryAttachedCall ctx.Provider (buildExpr ctx) head spine loc

            match folded with
            | ValueSome call -> call
            | ValueNone ->
                // Resolve a spine head that names a module function to its flat callee +
                // SOURCE groups — a local `CompiledFns` entry or an external `ValRepr`. The
                // groups are non-empty by construction (both `gather` and the external
                // `ValRepr` capture require ≥ 1 source group), so the saturation predicate
                // below is written ONCE for both kinds: a flat call exactly when the spine
                // is at least the group count. Anything else keeps the curried fallback.
                let flatHead: (JsExpr * TastAccessor.ArgGroup list) voption =
                    let identAt name =
                        JsExpr.Identifier(name, locOf ctx (TastAccessor.exprTok head))

                    match TastAccessor.exprKind head with
                    | ExprShape.Var ->
                        let k = TastAccessor.exprVarBinding head

                        match ctx.CompiledFns.TryGetValue k with
                        | true, cf -> ValueSome(identAt (binderNameOf ctx.Source k), cf.Groups)
                        | _ -> ValueNone
                    | ExprShape.External ->
                        let ext = TastAccessor.exprExternal head

                        JsFlatFns.externalGroups ctx.Provider ext.Key
                        |> ValueOption.map (fun groups ->
                            identAt (
                                JsImports.addRef ctx.Imports ext.CompiledName (externalValueRef ctx.Provider ext.Key)
                            ),
                            groups
                        )
                    | _ -> ValueNone

                match flatHead with
                | ValueSome(callee, groups) when List.length spine >= List.length groups ->
                    JsFlatFns.emitFlatCall (buildExpr ctx) callee groups spine loc
                | _ -> JsExpr.Call(buildExpr ctx av.Fn, [ buildExpr ctx av.Arg ], loc)

        // A record literal `{ X = e1; Y = e2 }` → `new R(args…)`, the args
        // reordered from source order to the class's *declaration*-order
        // positional constructor.
        | ExprShape.RecordCons ->
            let info = recordInfoOf ctx "RecordCons" (TastAccessor.exprTy e)
            let srcMap = Map.ofSeq (TastAccessor.exprRecordConsFields e)

            let args =
                [
                    for f in info.Fields ->
                        match Map.tryFind f srcMap with
                        | Some e -> buildExpr ctx e
                        | None -> failwithf "EmitJs (Step 3): record literal for '%s' is missing field '%s'" info.Name f
                ]

            JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), args, loc)

        // `{ r with X = v; … }` → reconstruction `new R(…)`: each field takes its
        // override if listed, else reads `<src>.field`. `<src>` is read once per
        // copied field, so a bare `Var` is spliced inline; any other source is bound
        // once through an IIFE binder (avoids re-evaluating / duplicating it).
        | ExprShape.RecordClone ->
            let rc = TastAccessor.exprRecordClone e
            let info = recordInfoOf ctx "RecordClone" (TastAccessor.exprTy e)
            let overrideMap = Map.ofSeq rc.Overrides

            let argsFrom (srcRef: JsExpr) =
                [
                    for f in info.Fields ->
                        match Map.tryFind f overrideMap with
                        | Some ov -> buildExpr ctx ov
                        | None -> JsExpr.Member(srcRef, JsExpr.Identifier(f, ValueNone), false, ValueNone)
                ]

            match TastAccessor.exprKind rc.Source with
            | ExprShape.Var ->
                JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), argsFrom (buildExpr ctx rc.Source), loc)
            | _ ->
                let sName = "_rc" + string (TastAccessor.exprTok e).StartIndex

                let newExpr =
                    JsExpr.New(
                        JsExpr.Identifier(info.Name, ValueNone),
                        argsFrom (JsExpr.Identifier(sName, ValueNone)),
                        loc
                    )

                JsExpr.Call(JsExpr.Arrow([ sName ], JsFnBody.Expr newExpr, ValueNone), [ buildExpr ctx rc.Source ], loc)

        // `r.X` → `r.X` — a member access on the record's like-named property
        // (the emitted class stores each field under its source field name).
        | ExprShape.FieldGet ->
            let fg = TastAccessor.exprFieldGet e
            JsExpr.Member(buildExpr ctx fg.Receiver, JsExpr.Identifier(fg.FieldName, ValueNone), false, loc)

        // `r.X <- v` → `(r.X = v)` — a mutable (`val mutable`) instance-field write,
        // the field analogue of the mutable-local `Assignment` arm. Unit-typed in F#,
        // so the yielded value is unused; in statement position `buildStatements`
        // wraps it as an expression statement.
        | ExprShape.FieldSet ->
            let fs = TastAccessor.exprFieldSet e

            JsExpr.Assign(
                JsExpr.Member(buildExpr ctx fs.Receiver, JsExpr.Identifier(fs.FieldName, ValueNone), false, loc),
                buildExpr ctx fs.Value,
                loc
            )

        // A union constructor `Case e0 e1 …` → `new <Union>_<Case>(args…)`. The
        // args already arrive in declaration (field) order, so — unlike a record
        // literal — no reordering is needed; the subclass constructor stores them
        // positionally under the case's field names.
        | ExprShape.UnionCons ->
            let info = unionInfoOf ctx "UnionCons" (TastAccessor.exprTy e)
            let c = unionCaseFromInfo info "UnionCons" (TastAccessor.exprUnionConsCaseName e)

            // A local union's class is in this file; an external union's case class is
            // imported from its home module (no local re-emit).
            let callee =
                match info.Home with
                | ValueSome asm -> JsExpr.Identifier(JsImports.addTypeRef ctx.Imports asm c.ClassName, loc)
                | ValueNone -> JsExpr.Identifier(c.ClassName, ValueNone)

            JsExpr.New(callee, [ for a in TastAccessor.exprChildren e -> buildExpr ctx a ], loc)

        // External exception construction → `new <exn repr>(msg)`. The repr is sourced
        // from the `inherit` chain via `exnReprOf`; only the leading message arg is kept
        // (`Error` has no slot for further args). Non-`exn`-subtype external `New` fails loudly.
        | ExprShape.New ->
            let ty = TastAccessor.exprTy e
            let args = TastAccessor.exprChildren e
            // A locally-emitted class constructs by its emitted name with positional
            // args (the ctor stores each into the like-named field). Resolved before
            // the external `exn`-repr path.
            let localClassName =
                match TastLower.receiverShape ty with
                | ValueSome(key, _) ->
                    match ctx.Classes.TryGetValue(SymbolKey.Type key) with
                    | true, name -> ValueSome name
                    | _ -> ValueNone
                | ValueNone -> ValueNone

            // A GLOBAL (ambient) external class — its home is a global pack (`Js.Map`,
            // `Js.Widget`) — constructs by its BARE export name with NO import: the JS
            // runtime provides it intrinsically. `Global` rides the HOME (the resolved
            // shape's `ExternalClassFlags.Global`, reached via the receiver-shape key),
            // so this fires for an es2015-home type but not a real package. The bare
            // name is the key's simple name (`Js.Widget` → `Widget`). Resolved after
            // the local-class path and before the external `exn`-repr fallback.
            let globalClassName =
                match TastLower.receiverShape ty with
                | ValueSome(key, _) ->
                    JsExternalMembers.classFlagsOf ctx.Provider key
                    |> ValueOption.filter (fun flags -> flags.Global)
                    // Backend name emission: the global class is `new`d under the name the
                    // runtime knows it by.
                    |> ValueOption.map (fun _ ->
                        let (DisplayName name) = SymbolKeyOps.typeSimpleName key
                        name
                    )
                | ValueNone -> ValueNone

            match localClassName, globalClassName with
            | ValueSome name, _
            | ValueNone, ValueSome name ->
                JsExpr.New(JsExpr.Identifier(name, ValueNone), [ for a in args -> buildExpr ctx a ], loc)
            | ValueNone, ValueNone ->
                match JsExternalMembers.exnReprOf ctx.Provider ty with
                | ValueSome repr ->
                    let errArgs =
                        if Array.isEmpty args then
                            []
                        else
                            [ buildExpr ctx args.[0] ]

                    JsExpr.New(JsExpr.Identifier(repr, ValueNone), errArgs, loc)
                | ValueNone ->
                    failwithf
                        "EmitJs (Step 8): construction of external type '%s' has no JS analogue (only `exn` subtypes lower to `new <exn repr>`)"
                        (TastAccessor.exprNewClassName e)

        // A member access through a LOCAL interface slot (`(r :> IRank).Rank`): the impl
        // is an ATTACHED method on the receiver's class (the plain-attached partition), so
        // dispatch as a flat member access `receiver.<member>(args)` — the free-function
        // `<Type>__<member>` form names no emitted function for an interface member. The
        // member's declaring type being a local interface is the signal (not the node's
        // `CallVia` — see `WalkCtx.LocalInterfaces`). An interface-impl PROPERTY emits as a
        // zero-arg attached method, so its read is the same member access called with no
        // args. Otherwise the member is on a local record/union: each is a free
        // receiver-first function. (The `LocalInterfaces` test is inside the arm, not a
        // `when` guard, so the payload view is materialized once — never in a guard.)
        | ExprShape.PropertyGet ->
            let pg = TastAccessor.exprPropertyGet e

            if ctx.LocalInterfaces.Contains(JsExternalMembers.declKey pg.Key) then
                JsExpr.Call(attachedAccess ctx loc pg.Receiver pg.Key, [], loc)
            else
                JsExpr.Call(Members.localFn ctx pg.Key false true ValueNone, [ buildExpr ctx pg.Receiver ], loc)

        | ExprShape.MethodCall ->
            let mc = TastAccessor.exprMethodCall e

            if ctx.LocalInterfaces.Contains(JsExternalMembers.declKey mc.Key) then
                JsExpr.Call(attachedAccess ctx loc mc.Receiver mc.Key, [ for a in mc.Args -> buildExpr ctx a ], loc)
            else
                let withRecv =
                    JsExpr.Call(Members.localFn ctx mc.Key false false ValueNone, [ buildExpr ctx mc.Receiver ], loc)

                applyArgs ctx withRecv mc.Args

        | ExprShape.StaticPropertyGet -> Members.localFn ctx (TastAccessor.exprStaticPropertyGetKey e) true true loc

        // An enum-case reference `E.Ci` → a property read on the frozen object map.
        // `StaticFieldGet` is the general static-field carrier (a class `static let`
        // backing-field read also lowers to it), so route to `enumCaseAccess` ONLY
        // when the node's type is the enum itself (`FTEnum`, stamped by Unification's
        // enum arm). A non-enum key is a class `static let` backing field, stored as a
        // property on the emitted class object (`ClassName.field`) — the store analogue
        // is `StaticFieldSet`.
        | ExprShape.StaticFieldGet ->
            let sfg = TastAccessor.exprStaticFieldGet e

            match TastAccessor.exprTy e with
            | FTEnum _ -> enumCaseAccess ctx sfg.Key sfg.FieldName loc
            | _ -> staticFieldRef ctx sfg.Key sfg.FieldName loc

        // `x <- v` on a `static let mutable` backing field → `(ClassName.field = v)`.
        // Unit-typed like `FieldSet`; the yielded value is unused in statement position.
        | ExprShape.StaticFieldSet ->
            let sfs = TastAccessor.exprStaticFieldSet e
            JsExpr.Assign(staticFieldRef ctx sfs.Key sfs.FieldName loc, buildExpr ctx sfs.Value, loc)

        | ExprShape.StaticMethodCall ->
            applyArgs
                ctx
                (Members.localFn ctx (TastAccessor.exprStaticMethodCallKey e) true false loc)
                (EqArray.ofArray (TastAccessor.exprChildren e))

        // A member on an external type. The declaring type's provider flags × the
        // receiver's presence pick the lowering — the whole dispatch in one table:
        //   * an ERASED grouping type erases to the bare module export
        //     (`erasedGroupingRef`); it holds only STATIC members, so a receiver is
        //     an invariant break;
        //   * an ATTACH-MEMBERS instance member reached WITHOUT an applying spine —
        //     the CALL form is folded in the `App` head-case — is a native value
        //     read: a data-property READ for a property, an eta-wrapped method
        //     value for a method. (R2 scope is INSTANCE members: a static member /
        //     ctor — `receiver = ValueNone` — falls through until its native
        //     lowering lands.)
        //   * everything else — including statics/ctors on an AttachMembers type —
        //     takes the mangled-import path, which is only satisfiable by a
        //     Vesper-provided runtime module (`JsImports.entryFor` fails loudly
        //     when the package has none — a real npm package cannot export a
        //     mangled name).
        // A capability-member property read (`e.Current`) — the applied calls (`e.MoveNext()`)
        // are folded in the `App` arm. Same convention as the `LocalInterfaces` arm above and
        // `emitIteratorMethod`: an interface property is a zero-arg method, so the read is the
        // call.
        | ExprShape.ExternalMember ->
            match e with
            | CapabilityRead ctx.Capabilities ctx.Imports (recv, emit) -> emit (buildExpr ctx recv) loc
            | _ ->
                let em = TastAccessor.exprExternalMember e
                let declKey = JsExternalMembers.declKey em.Key
                // JS has no field/property distinction at access — both are a value member
                // (the `get_`-style mangled import); only a `Method` is an arrow. (A `Field`
                // here would gain only `readonly` fidelity, not yet modelled.)
                let isProperty = em.Storage.IsValueMember

                match JsExternalMembers.classFlagsOf ctx.Provider declKey, em.Receiver with
                | ValueSome {
                                MemberLowering = MemberLowering.ErasedBare
                            },
                  ValueSome _ ->
                    failwithf
                        "EmitJs (Step 9b): erased grouping type member '%s' has an instance receiver, but a synthetic free-function-overload type carries only static members"
                        em.MemberName
                | ValueSome {
                                MemberLowering = MemberLowering.ErasedBare
                                ImportForm = form
                            },
                  ValueNone ->
                    JsExternalMembers.erasedGroupingRef ctx.Provider ctx.Imports declKey em.MemberName form loc
                | ValueSome {
                                MemberLowering = MemberLowering.AttachedNative
                            },
                  ValueSome r when isProperty ->
                    // A manifest Property is a JS DATA property — native access is a plain
                    // member READ `recv.prop`, NOT a zero-arg call. (Contrast the LOCAL
                    // interface-impl property path, which emits `Call(attachedAccess, [])`
                    // because Vesper compiles interface properties as zero-arg methods; a
                    // TS property is genuinely a data slot, not a method.)
                    JsExternalMembers.attachedMember (buildExpr ctx r) em.MemberName loc
                | ValueSome {
                                MemberLowering = MemberLowering.AttachedNative
                            },
                  ValueSome r ->
                    JsExternalMembers.etaWrapAttachedMethod
                        (buildExpr ctx)
                        r
                        em.Key
                        em.MemberName
                        (TastAccessor.exprTok e).StartIndex
                        loc
                | _ ->
                    JsExternalMembers.mangledMemberAccess
                        ctx.Provider
                        ctx.Imports
                        (buildExpr ctx)
                        declKey
                        em.Receiver
                        em.MemberName
                        isProperty
                        loc

        // `match scrut with …` → an IIFE binding the scrutinee once, then testing each
        // arm in order and `return`ing the first whose pattern (+ guard) matches; an
        // unmatched value `throw`s. Sequential test (not `switch(tag)`) so it covers
        // guards, constants, nested patterns, and non-union scrutinees uniformly.
        | ExprShape.Match ->
            let m = TastAccessor.exprMatch e
            let mv = "_m" + string (TastAccessor.exprTok e).StartIndex
            let access = JsExpr.Identifier(mv, ValueNone)

            let body =
                [
                    for arm in m.Arms do
                        yield! buildMatchArm ctx access arm
                    yield matchFailure
                ]

            JsExpr.Call(JsExpr.Arrow([ mv ], JsFnBody.Block body, loc), [ buildExpr ctx m.Scrutinee ], loc)

        // A mutable-local / array-element write `lhs <- rhs` → the JS assignment
        // expression `(lhs = rhs)`. Unit-typed in F#, so its yielded value is unused;
        // in statement position `buildStatements` wraps it as an expression statement.
        | ExprShape.Assignment ->
            let a = TastAccessor.exprAssignment e
            JsExpr.Assign(buildExpr ctx a.Lhs, buildExpr ctx a.Rhs, loc)

        // `while cond do body` in expression position. JS `while` is a statement, so it
        // lowers to a zero-arg IIFE `(() => { while (<cond>) { <body> } })()` that yields
        // `undefined` (the F# `unit` result). Statement position keeps the bare loop —
        // see `buildStatements`.
        | ExprShape.While ->
            let w = TastAccessor.exprWhile e
            let loop = JsStatement.While(buildExpr ctx w.Cond, buildStatements ctx w.Body)
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block [ loop ], loc), [], loc)

        // `for i = a to b do body` in expression position — same IIFE wrapper as `while`;
        // `buildStatements` produces the hoisted-limit `const` + the `for` statement.
        | ExprShape.ForTo -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // `for x in source do body` in expression position — same IIFE wrapper as `for…to`
        // (the loop yields `unit`); `buildStatements` produces the `for…of`.
        | ExprShape.ForIn -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // `use x = value in body` in expression position. JS `try/finally` is a
        // statement, so it lowers to a zero-arg IIFE that parks the binder, `return`s
        // the body's value from the `try`, and disposes in the `finally`. The body is
        // a single expression (`Sequential` becomes a comma expression, a nested `let`
        // its own IIFE), so returning `buildExpr ctx body` preserves the result through
        // the disposal in the `finally`.
        | ExprShape.Use ->
            let u = TastAccessor.exprUse e
            let name = useBinderName ctx u.Binding

            let tryFinally =
                JsStatement.TryFinally([ JsStatement.Return(buildExpr ctx u.Body) ], disposeStmts ctx u.Dispose name)

            let block = [ JsStatement.Const(name, buildExpr ctx u.Value); tryFinally ]
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block block, loc), [], loc)

        // `try body finally cleanup` in expression position. As with `use` (which desugars
        // through the same JS `try/finally`), the region has a value, but JS `try/finally`
        // is a statement — so wrap it in a zero-arg IIFE that `return`s the body's value from
        // the `try` and runs `cleanup` (a unit expression) for effect in the `finally`.
        | ExprShape.TryFinally ->
            let tf = TastAccessor.exprTryFinally e

            let tryFinally =
                JsStatement.TryFinally([ JsStatement.Return(buildExpr ctx tf.Body) ], buildStatements ctx tf.Cleanup)

            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block [ tryFinally ], loc), [], loc)

        // `e :> obj` (value→`obj` box, synthesised at Elaborate for an `obj` parameter/field).
        // JS is dynamically typed — every value is already a boxed `obj` — so the box is a
        // no-op; emit the source verbatim. The downcast `e :?> T` is likewise identity (no
        // runtime nominal type to check).
        | ExprShape.Upcast
        | ExprShape.Downcast -> buildExpr ctx (TastAccessor.exprChild e 0)

        // `expr when ^T : int = …`: the clauses are a COMPILE-TIME selection made when a
        // splice pins the operand type (`Inline.inlineExpand`). Reaching the backend means
        // no type was pinned — this is the `inline` binding's ordinary compiled form, or a
        // use of it as a first-class value — and the node carries `defaultExpr` for
        // precisely that case. Emit the default, as the CLR backend does; a clause is an
        // optimisation over it, never a different meaning.
        | ExprShape.StaticOptimization -> buildExpr ctx (TastAccessor.exprStaticOptimizationDefault e)

        // The tokenful array intrinsics — `Array.zeroCreate` / `arr.[i]` / `arr.[i] <- v`
        // / `arr.Length`, desugared to `newarr`/`ldelem`/`stelem`/`ldlen` (the same
        // mnemonics the CLR backend reads; they are target-neutral, the element-type
        // operand is dropped on JS). They reach the backend because their inline bodies
        // live in `ops-platform.js.fs` (`array.fs`'s `zeroCreate` for `newarr`).
        | ExprShape.ILIntrinsic ->
            let args = TastAccessor.exprChildren e

            match TastAccessor.exprILIntrinsicOpCode e with
            | "newarr" ->
                // `Array.zeroCreate count` → `Array(count).fill(null)`: a *dense* array (not
                // the sparse `new Array(count)`), so `Object.keys` / iteration observe every
                // slot. Unset slots read as `null`, not the element type's zero — the JS
                // zero-init erasure corner (callers fill before reading).
                match args with
                | [| count |] ->
                    let alloc =
                        JsExpr.Call(JsExpr.Identifier("Array", ValueNone), [ buildExpr ctx count ], ValueNone)

                    let fill =
                        JsExpr.Member(alloc, JsExpr.Identifier("fill", ValueNone), false, ValueNone)

                    JsExpr.Call(fill, [ JsExpr.Identifier("null", ValueNone) ], loc)
                | _ -> failwith "EmitJs: 'newarr' expects one operand (the element count)"

            // `arr.[i]` → `arr[i]` (a computed member read).
            | "ldelem" ->
                match args with
                | [| arr; idx |] -> JsExpr.Member(buildExpr ctx arr, buildExpr ctx idx, true, loc)
                | _ -> failwith "EmitJs: 'ldelem' expects two operands (array, index)"

            // `arr.[i] <- v` → `(arr[i] = v)` (a computed-member assignment expression).
            | "stelem" ->
                match args with
                | [| arr; idx; value |] ->
                    let target = JsExpr.Member(buildExpr ctx arr, buildExpr ctx idx, true, ValueNone)
                    JsExpr.Assign(target, buildExpr ctx value, loc)
                | _ -> failwith "EmitJs: 'stelem' expects three operands (array, index, value)"

            // `arr.Length` → `arr.length`.
            | "ldlen" ->
                match args with
                | [| arr |] -> JsExpr.Member(buildExpr ctx arr, JsExpr.Identifier("length", ValueNone), false, loc)
                | _ -> failwith "EmitJs: 'ldlen' expects one operand (the array)"

            // The empty-string identity intrinsic `(# "" x : 'U #)` — FSharp.Core's
            // erasing reinterpret (`retype`, the primitive `dynamic` enter/exit builds on).
            // It has NO runtime effect: emit the lone operand verbatim, re-typed (the CLR
            // emits nothing likewise). Handled before the generic `$N`-template expander,
            // which would (correctly) reject an operand-bearing template with no hole.
            | "" when args.Length = 1 -> buildExpr ctx args.[0]

            | opCode -> JsExpr.Raw(EmitJsFormat.expandTemplate buildExpr ctx opCode (List.ofArray args), loc)

        | ExprShape.Format ->
            let fv = TastAccessor.exprFormat e
            let arg = EmitJsFormat.buildFormatArg buildExpr ctx fv.Segments

            match fv.Sink with
            | TastAccessor.FormatSinkView.ToStdOut true -> JsExpr.Call(console "log", [ arg ], loc)
            | TastAccessor.FormatSinkView.ToStdErr true -> JsExpr.Call(console "error", [ arg ], loc)
            // `sprintf` (`State = unit`, `Residue = string`): the spliced concatenation
            // IS the result string, yielded directly as a value (no console call).
            | TastAccessor.FormatSinkView.ToString -> arg
            | other -> failwithf "EmitJs: unsupported format sink %A" other

        | _ -> failwithf "EmitJs: unsupported expression %A" e

    /// Build one `match` arm's statements: when the pattern matches (and the guard,
    /// if any, passes) the arm `return`s its body. An always-matching arm
    /// (wildcard / bare variable, `test = None`) emits a bare `Block` so its
    /// bindings stay scoped (two arms may bind the same source name); a refutable
    /// arm guards that block with `if (test)`.
    and private buildMatchArm (ctx: WalkCtx) (access: JsExpr) (arm: TastAccessor.ArmView) : JsStatement list =
        let test, binds = compileMatchPattern ctx access arm.Pat

        let inner =
            match arm.Guard with
            | ValueNone -> binds @ [ JsStatement.Return(buildExpr ctx arm.Body) ]
            | ValueSome g ->
                binds
                @ [
                    JsStatement.If(buildExpr ctx g, [ JsStatement.Return(buildExpr ctx arm.Body) ], [])
                ]

        match test with
        | None -> [ JsStatement.Block inner ]
        | Some t -> [ JsStatement.If(t, inner, []) ]

    /// The body of a function whose params are `names`: a `while (true)` trampoline
    /// (`buildTailBody`) when `selfKey` names the binding and its body makes a saturated
    /// tail self-call at `arity` (constant-stack recursion), else the plain expression.
    /// `arity` is the SOURCE-group count — for a flat module fn it differs from
    /// `names.Length` (tuple groups expand, lone unit erases), so the caller passes it.
    and private trampolineOrExpr
        (ctx: WalkCtx)
        (selfKey: NodeKey voption)
        (arity: int)
        (names: string list)
        (body: TastAccessor.ExprId)
        : JsFnBody =
        match selfKey with
        | ValueSome k when hasTailSelfCall k arity body ->
            JsFnBody.Block
                [
                    JsStatement.While(JsExpr.Literal(JsLiteral.Boolean true, ValueNone), buildTailBody ctx k names body)
                ]
        | _ -> JsFnBody.Expr(buildExpr ctx body)

    /// Emit a function value as nested *unary* arrows. When `selfKey` names the
    /// binding and its body makes a saturated tail self-call, the innermost arrow
    /// becomes a `while (true)` trampoline for constant-stack recursion; else the
    /// innermost body is the plain expression. The nested-unary shape keeps every
    /// arrow's param in scope at the innermost body, which is what lets the trampoline
    /// write them back and `continue`.
    and emitFunction (ctx: WalkCtx) (selfKey: NodeKey voption) (lam: TastAccessor.ExprId) : JsExpr =
        let loc = locOf ctx (TastAccessor.exprTok lam)
        let names, body = peelArrow ctx.Source lam
        nestUnaryArrows loc names (trampolineOrExpr ctx selfKey (List.length names) names body)

    /// Build the statements of a self-tail-call trampoline's loop body, walking
    /// tail position. A saturated tail self-call writes its arguments back to the
    /// parameter variables — through per-argument temporaries first, so an
    /// argument that reads a parameter (`sum (n-1) (acc+n)`) sees the *old* value
    /// — then `continue`s. Tail `if`/`let`/`Sequential`-tail thread through;
    /// every other tail expression `return`s its value.
    and buildTailBody
        (ctx: WalkCtx)
        (selfKey: NodeKey)
        (paramNames: string list)
        (e: TastAccessor.ExprId)
        : JsStatement list =
        let arity = List.length paramNames
        let recur = buildTailBody ctx selfKey paramNames

        match e with
        | InlinableLet reduced -> recur reduced
        | TailSelfCall selfKey arity args ->
            // `_tc<i>` temporaries: evaluate every new argument before any write-back,
            // so a self-call arg that mentions a parameter reads its pre-iteration
            // value. (Not collision-proof against a source param literally named
            // `_tc0` — synthetic names are keyed off strings, not `NodeKey`s.)
            let tmp i = "_tc" + string i

            [ for i, a in List.indexed args -> JsStatement.Const(tmp i, buildExpr ctx a) ]
            @ [
                for i, name in List.indexed paramNames -> JsStatement.Assign(name, JsExpr.Identifier(tmp i, ValueNone))
            ]
            @ [ JsStatement.Continue ]
        | _ ->
            match TastAccessor.exprKind e with
            | ExprShape.IfThenElse ->
                let i = TastAccessor.exprIfThenElse e
                [ JsStatement.If(buildExpr ctx i.Cond, recur i.ThenExpr, recur i.ElseExpr) ]
            | ExprShape.Let ->
                let l = TastAccessor.exprLet e

                match TastAccessor.patKind l.Binding with
                | PatShape.NamedSimple ->
                    let k = (TastAccessor.patBinder l.Binding).Value

                    let binding =
                        localBinding k l.Body (binderNameOf ctx.Source k) (buildExpr ctx l.Value)

                    binding :: recur l.Body
                // `let _ = value in body` — discard the value (effects only); body stays in tail
                // position. A pure value drops away (see `buildExpr`).
                | PatShape.Wildcard when isPureValue l.Value -> recur l.Body
                | PatShape.Wildcard -> buildStatements ctx l.Value @ recur l.Body
                | _ -> [ JsStatement.Return(buildExpr ctx e) ]
            | ExprShape.Sequential ->
                let xs = TastAccessor.exprChildren e

                if xs.Length > 0 then
                    let n = xs.Length

                    let init =
                        [
                            for i in 0 .. n - 2 do
                                yield! buildStatements ctx xs.[i]
                        ]

                    init @ recur xs.[n - 1]
                else
                    [ JsStatement.Return(buildExpr ctx e) ]
            | _ -> [ JsStatement.Return(buildExpr ctx e) ]

    /// Curry `base` over `args` — one unary `Call` per argument, in source order
    /// (`base(a)(b)…`). Shared by the `MethodCall` / `StaticMethodCall` lowerings.
    and private applyArgs (ctx: WalkCtx) (baseExpr: JsExpr) (args: EqArray<TastAccessor.ExprId>) : JsExpr =
        args
        |> EqArray.fold (fun acc a -> JsExpr.Call(acc, [ buildExpr ctx a ], ValueNone)) baseExpr

    /// Emit a local module FUNCTION as one FLAT arrow over its compiled parameters
    /// (`let f x y` → `(x, y) => …`; tuple groups flattened, a lone unit erased to
    /// `() => …`). When every group is a plain binder and the body makes a saturated
    /// tail self-call, the body becomes a `while (true)` trampoline — the flat
    /// parameters are the mutated slots (only the all-`GSimple` shape maps a self-call's
    /// spine one-to-one onto them).
    and private emitFlatModuleFn
        (ctx: WalkCtx)
        (k: NodeKey)
        (cf: CompiledFns.CompiledFn)
        (loc: JsLoc voption)
        : JsExpr =
        let names = [ for p in cf.Params -> JsFlatFns.paramNameOf ctx.Source p ]

        // Only the all-`GSimple` shape maps a self-call's spine one-to-one onto the flat
        // params, so the trampoline is gated on it; otherwise no self-key is offered.
        let selfKey =
            if TastLower.allSimpleGroups cf.Groups then
                ValueSome k
            else
                ValueNone

        JsExpr.Arrow(names, trampolineOrExpr ctx selfKey (List.length cf.Groups) names cf.Body, loc)

    /// `receiver.<member>` for a call dispatched through a local interface slot — the
    /// member resolves to the attached method `partitionClassMembers` emitted on the
    /// receiver's class. Shared by the `PropertyGet`/`MethodCall` `CallVia.Interface` arms.
    and attachedAccess (ctx: WalkCtx) (loc: JsLoc voption) (receiver: TastAccessor.ExprId) (key: SymbolKey) : JsExpr =
        // Backend name emission: the attached method is reached under its JS member name.
        let (DisplayName memberName) = SymbolKeyOps.simpleName key
        JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(memberName, ValueNone), false, loc)

    /// A value bound to a name (a module value, or a `let` binder). A `Lambda`
    /// value routes through `emitFunction` carrying its binder key, so a
    /// recursive binding (`let rec`) can recognise its own tail calls; any other
    /// value is a plain `buildExpr`.
    and emitBound (ctx: WalkCtx) (k: NodeKey) (value: TastAccessor.ExprId) : JsExpr =
        match TastAccessor.exprKind value with
        | ExprShape.Lambda -> emitFunction ctx (ValueSome k) value
        | _ -> buildExpr ctx value

    /// An expression in statement position. `Sequential` flattens; a `let` binder
    /// becomes a `const`; anything else is one `ExpressionStatement`.
    and buildStatements (ctx: WalkCtx) (e: TastAccessor.ExprId) : JsStatement list =
        match e with
        // Pure, immutable binder: substitute away so synthetic operand lets don't
        // surface as `const`s. A mutable binder is excluded (see `buildExpr`).
        | InlinableLet reduced -> buildStatements ctx reduced
        | _ ->
            match TastAccessor.exprKind e with
            | ExprShape.Sequential ->
                [
                    for x in TastAccessor.exprChildren e do
                        yield! buildStatements ctx x
                ]
            | ExprShape.Let ->
                let l = TastAccessor.exprLet e

                match TastAccessor.patKind l.Binding with
                // A mutable binder emits a reassignable `let`; an immutable one a `const`.
                | PatShape.NamedSimple ->
                    let k = (TastAccessor.patBinder l.Binding).Value

                    let binding =
                        localBinding k l.Body (binderNameOf ctx.Source k) (emitBound ctx k l.Value)

                    binding :: buildStatements ctx l.Body
                // `let _ = value in body` — emit the discarded value as its own statement(s)
                // (effects only), then the body. A pure value drops away (see `buildExpr`).
                | PatShape.Wildcard when isPureValue l.Value -> buildStatements ctx l.Body
                | PatShape.Wildcard -> buildStatements ctx l.Value @ buildStatements ctx l.Body
                | _ -> [ JsStatement.Expression(buildExpr ctx e) ]
            // `while cond do body` as a bare loop statement (no IIFE wrapper needed here).
            | ExprShape.While ->
                let w = TastAccessor.exprWhile e
                [ JsStatement.While(buildExpr ctx w.Cond, buildStatements ctx w.Body) ]
            // `for i = a to b do body` — F# evaluates `b` once, so hoist the limit into a
            // `const` before the loop; the JS `for` then counts `i` from `a` up to that
            // limit inclusive. (JS numbers are doubles, so the CLR overflow-at-MaxValue
            // dance the IL backend needs is unnecessary — `i <= limit` is safe.)
            | ExprShape.ForTo ->
                let ft = TastAccessor.exprForTo e
                let name = binderNameOf ctx.Source ft.Var
                let limit = "_lim" + string (TastAccessor.exprTok e).StartIndex

                [
                    JsStatement.Const(limit, buildExpr ctx ft.EndExpr)
                    JsStatement.For(
                        name,
                        buildExpr ctx ft.StartExpr,
                        JsExpr.Identifier(limit, ValueNone),
                        buildStatements ctx ft.Body
                    )
                ]
            // `for x in source do body` — lower to a JS `for…of`, which drives the source's
            // own `Symbol.iterator` at runtime. Only the `Interface` enumerator (a source
            // typed `IEnumerable<'T>`) reaches JS codegen, and it carries no member keys (the
            // CLR backend mints the `IEnumerator` interface slots itself; JS defers to the
            // iterator protocol), so there is nothing to resolve — `for…of` over the source is
            // the whole lowering. A duck-typed `Pattern` enumerator never type-checks against
            // the BCL-free JS provider, so it is unsupported here.
            | ExprShape.ForIn ->
                let fi = TastAccessor.exprForIn e

                match fi.Enumerator with
                | ForInEnumeratorG.Interface ->
                    match TastAccessor.patKind fi.Pat with
                    // Simple/wildcard binder: `for (const x of src)` directly.
                    | PatShape.NamedSimple
                    | PatShape.Wildcard ->
                        let name = patBinderName ctx "_forin" fi.Pat

                        [
                            JsStatement.ForOf(name, buildExpr ctx fi.Source, buildStatements ctx fi.Body)
                        ]
                    // Destructuring binder — `for (k, v) in map` over `[K,V]` pairs: bind a
                    // fresh loop temp and reuse `compileMatchPattern` (the SAME lowering
                    // `let (k, v) = …` uses) to deconstruct it into the body head. The binder
                    // must be irrefutable — a `Some test` means a nested refutable sub-pattern,
                    // which a `for … in` binder cannot express, so reject it rather than emit
                    // the binds without the guard.
                    | PatShape.Tuple ->
                        let tmp = "_forin" + string (TastAccessor.patTok fi.Pat).StartIndex

                        match compileMatchPattern ctx (JsExpr.Identifier(tmp, ValueNone)) fi.Pat with
                        | None, binds ->
                            [
                                JsStatement.ForOf(tmp, buildExpr ctx fi.Source, binds @ buildStatements ctx fi.Body)
                            ]
                        | Some _, _ -> failwithf "EmitJs: refutable `for … in` binder pattern is unsupported %A" fi.Pat
                    | _ -> failwithf "EmitJs: unsupported `for … in` binder pattern %A" fi.Pat
                | ForInEnumeratorG.Pattern _ ->
                    failwith
                        "EmitJs: duck-typed `for...in` (Pattern enumerator) is unsupported on JS; only IEnumerable<'T> sources lower to `for...of`"
            // `use x = value in body` — park the binder in a `const`, run the body inside a
            // `try`, and dispose the binder in the `finally` (the IL backend's exception
            // region, lowered to JS `try/finally`). The body keeps statement position.
            | ExprShape.Use ->
                let u = TastAccessor.exprUse e
                let name = useBinderName ctx u.Binding

                [
                    JsStatement.Const(name, buildExpr ctx u.Value)
                    JsStatement.TryFinally(buildStatements ctx u.Body, disposeStmts ctx u.Dispose name)
                ]
            // `try body finally cleanup` in statement position — the body and cleanup both
            // keep statement position (no IIFE needed, unlike the expression form), mapping
            // straight onto JS `try/finally`.
            | ExprShape.TryFinally ->
                let tf = TastAccessor.exprTryFinally e

                [
                    JsStatement.TryFinally(buildStatements ctx tf.Body, buildStatements ctx tf.Cleanup)
                ]
            | _ -> [ JsStatement.Expression(buildExpr ctx e) ]

    /// The JS binder name for a single-binder loop/scope pattern (`use x = …`,
    /// `for x in …`). A wildcard binder has no source name, so it gets a fresh
    /// `<prefix><tok>` slot — the value is still bound (parked/iterated) even though
    /// the body can't name it. Only simple/wildcard binders are supported; a
    /// destructuring binder (e.g. a tuple pattern) is rejected.
    and private patBinderName (ctx: WalkCtx) (prefix: string) (binding: TastAccessor.PatId) : string =
        match TastAccessor.patKind binding with
        | PatShape.NamedSimple -> binderName ctx.Source (TastAccessor.patBinderNaming binding).Value
        | PatShape.Wildcard -> prefix + string (TastAccessor.patTok binding).StartIndex
        | _ -> failwithf "EmitJs: unsupported single binder pattern %A" binding

    and private useBinderName (ctx: WalkCtx) (binding: TastAccessor.PatId) : string = patBinderName ctx "_use" binding

    /// The `finally` body that disposes a `use` binder: a null-guarded disposal call.
    /// F# `use` is null-safe — JS loose `!= null` catches both `null` and `undefined`
    /// (matching the `Null` pattern convention). The disposal capability's JS slot is the
    /// native `binder[Symbol.dispose]()` — the same slot a disposable impl emits its method
    /// under (`emitDisposeMethod`) — whether the binder is a project-local impl or an external
    /// one reached through the capability interface (`use e = src.GetEnumerator()`, since
    /// `enumerator<'T> : disposable`). Only the carve-out (`ViaOwnMember`) calls a keyed
    /// member's free receiver-first function.
    and private disposeStmts (ctx: WalkCtx) (dispose: Disposal) (name: string) : JsStatement list =
        let binder = JsExpr.Identifier(name, ValueNone)

        let guard =
            JsExpr.Binary("!=", binder, JsExpr.Identifier("null", ValueNone), ValueNone)

        let disposeCall =
            match dispose with
            // The capability's slot: a COMPUTED member access on the well-known symbol, no
            // args. The CLR-only interface `slot` key the node carries is irrelevant here —
            // JS names its own slot.
            | Disposal.ViaCapability _ -> disposeSlotCall binder ValueNone
            // Ref-struct carve-out / an external type's own pattern `Dispose()`: call the
            // keyed member's free receiver-first function.
            | Disposal.ViaOwnMember key ->
                let disposeFn = Members.localFn ctx key false false ValueNone
                JsExpr.Call(disposeFn, [ binder ], ValueNone)
            | Disposal.Unresolved ->
                failwithf
                    "EmitJs: `use` over a binder with no resolved disposal ('%s') — Unification reported an error, so this file should never have reached codegen"
                    name

        [ JsStatement.If(guard, [ JsStatement.Expression disposeCall ], []) ]

    /// A class's instance preamble as the TAIL of its primary constructor, in declaration
    /// order — which is load-bearing, and is why the entries emit as ctor statements
    /// rather than as class-field initialisers.
    ///
    /// A preamble `let` is an instance FIELD (the same lowering a ctor param already gets,
    /// with the value coming from an initialiser rather than an argument), so it emits as
    /// `this.<name> = <init>;` — including a `let mutable` (a field, never a ref cell: a
    /// closure over it must capture `this`, so every reader shares one storage) and a
    /// function-valued `let` (a field holding an arrow closed over `this`; assigned before
    /// any call can read it, so `let rec` needs nothing extra). A binder's field is named
    /// by its SOURCE name — sound only because a preamble binder may not shadow a ctor
    /// param or an earlier binder (the front-end rejects it); binder uniquification is a
    /// separate, cross-cutting pass.
    let private emitInstancePreamble (ctx: WalkCtx) (p: EmitJsTypes.ClassPreamble) : JsStatement list =
        [
            yield! EmitJsMembers.thisAlias ctx p.ThisKey

            for entry in p.Entries do
                match entry with
                | TPreambleEntryG.Let l ->
                    let field =
                        JsExpr.Member(
                            JsExpr.Identifier("this", ValueNone),
                            JsExpr.Identifier(l.Name, ValueNone),
                            false,
                            ValueNone
                        )

                    yield JsStatement.Expression(JsExpr.Assign(field, buildExpr ctx l.Init, ValueNone))
                | TPreambleEntryG.Do e -> yield! buildStatements ctx e
        ]

    /// A class's `static let` / `static do` preamble → module-load statements that
    /// initialise the class's static backing fields. A `static let x = init` stores
    /// `ClassName.x = init`; the read/write sites resolve the same `ClassName.x` slot
    /// (`staticFieldRef`). Entries run in declaration order — load-bearing, exactly as the
    /// instance preamble — so an initialiser may read an earlier `static let`.
    let private emitStaticPreamble
        (ctx: WalkCtx)
        (className: string)
        (entries: TastAccessor.PreambleEntry list)
        : JsStatement list =
        [
            for entry in entries do
                match entry with
                | TPreambleEntryG.Let l ->
                    let field =
                        JsExpr.Member(
                            JsExpr.Identifier(className, ValueNone),
                            JsExpr.Identifier(l.Name, ValueNone),
                            false,
                            ValueNone
                        )

                    yield JsStatement.Expression(JsExpr.Assign(field, buildExpr ctx l.Init, ValueNone))
                | TPreambleEntryG.Do e -> yield! buildStatements ctx e
        ]

    /// The whole frozen file → a `Program`. Type declarations become JS `class`es first
    /// (classes are not hoisted); remaining decls are lowered — `let inline` templates
    /// and `type` decls drop out, leaving module values and effectful expressions.
    let buildProgram (ctx0: WalkCtx) : JsProgram =
        let decls = TastAccessor.roots ctx0.Pool |> List.ofArray

        let collected = collectTypes ctx0.Capabilities ctx0.ExportTopLevel decls

        let lowered = TastLower.lower decls

        // The top-level module functions and their flat compiled form — the same
        // `Codegen.Common.CompiledFns` analysis the CLR backend reads. Drives the FLAT
        // (Fable-style) emission of every module function and the spine-collapsing of
        // its saturated call sites.
        let compiledFns =
            System.Collections.Generic.Dictionary<NodeKey, CompiledFns.CompiledFn>()

        for f in CompiledFns.gather lowered do
            compiledFns.[f.Key] <- f

        // The file's locally-declared interface keys — drives the attached-method
        // dispatch of a `(r :> ILocal).M()` / `.Prop` access (see `WalkCtx.LocalInterfaces`).
        let localInterfaces = System.Collections.Generic.HashSet<TypeKey>()

        for decl in decls do
            match TastAccessor.declKind decl with
            | DeclShape.Type ->
                let td = TastAccessor.declType decl

                match td.Kind with
                | TTypeKindG.Interface _ -> localInterfaces.Add td.TypeKey |> ignore
                | _ -> ()
            | _ -> ()

        let ctx =
            { ctx0 with
                Records = collected.Records
                Unions = collected.Unions
                Classes = collected.Classes
                Enums = collected.Enums
                CompiledFns = compiledFns
                LocalInterfaces = localInterfaces
            }

        // Class decls (with their attached instance methods) are built now — their
        // method bodies need the full ctx, unlike record/union decls which carry no
        // bodies. They join the record/union decls ahead of members and the body.
        let classDecls =
            [
                for pc in collected.PendingClasses ->
                    let ctorBody =
                        match pc.Preamble with
                        | ValueSome p -> emitInstancePreamble ctx p
                        | ValueNone -> []

                    JsStatement.Class(
                        pc.Name,
                        pc.Fields,
                        ctorBody,
                        EmitJsMembers.emitCapabilityMethods buildExpr ctx pc.Members,
                        ctx.ExportTopLevel
                    )
            ]

        // Unions whose interface impls became base-class methods are built now too —
        // their `[Symbol.iterator]` / protocol bodies need the full ctx (the method
        // bodies may `new` a class), exactly like the pending classes. They render
        // base-first so the case subclasses inherit the protocol members.
        let pendingUnionDecls =
            [
                for pu in collected.PendingUnions ->
                    JsStatement.Union(
                        pu.Name,
                        pu.Brand,
                        pu.Cases,
                        EmitJsMembers.emitCapabilityMethods buildExpr ctx pu.Members,
                        ctx.ExportTopLevel
                    )
            ]

        // Member functions emitted after the class decls (they reference the classes
        // via `new`/match, and `const` arrows are not hoisted) and before the body.
        let memberDecls =
            [
                for (typeName, m) in collected.Members -> EmitJsMembers.emitMemberFn buildExpr ctx typeName m
            ]

        // Static preambles run at module load AFTER every class + member const-arrow is
        // defined (a `static let` may call a static member) and BEFORE the body reads a
        // static field. Declaration order across classes matches `collectTypes`.
        let staticPreambleStmts =
            [
                for pc in collected.PendingClasses do
                    yield! emitStaticPreamble ctx pc.Name pc.StaticPreamble
            ]

        // A module binder mutated by a later module-level `Assignment` (a top-level
        // `let mutable m … m <- e`) must emit as `let`/`export let`, not `const`.
        let reassignedAtTop (k: NodeKey) =
            lowered
            |> List.exists (fun d ->
                match TastAccessor.declKind d with
                | DeclShape.Expression -> isAssignedIn k (TastAccessor.declExpression d)
                | DeclShape.Let -> isAssignedIn k (TastAccessor.declLet d).Value
                | DeclShape.Type -> false
            )

        let body =
            [
                for decl in lowered do
                    match TastAccessor.declKind decl with
                    | DeclShape.Expression -> yield! buildStatements ctx (TastAccessor.declExpression decl)
                    | DeclShape.Let ->
                        let dl = TastAccessor.declLet decl

                        match TastAccessor.patKind dl.Binding with
                        | PatShape.NamedSimple ->
                            let k = (TastAccessor.patBinder dl.Binding).Value
                            let value = dl.Value
                            // A module FUNCTION emits FLAT (Fable-style); a plain value
                            // routes through `emitBound` (closures stay curried).
                            let init =
                                match ctx.CompiledFns.TryGetValue k with
                                | true, cf -> emitFlatModuleFn ctx k cf (locOf ctx (TastAccessor.exprTok value))
                                | _ -> emitBound ctx k value

                            topLevelBinding ctx (reassignedAtTop k) (binderNameOf ctx.Source k) init
                        | _ -> failwithf "EmitJs: unsupported declaration %A" decl
                    | DeclShape.Type -> failwithf "EmitJs: unsupported declaration %A" decl
            ]

        // Imports lead the program; local class decls follow — classes are not hoisted
        // and must precede every `new`/match site. External union case classes are not
        // emitted here: a `UnionCons` imports them from the union's home module.
        {
            Body =
                JsImports.importStatements ctx.Imports
                @ collected.Decls
                @ classDecls
                @ pendingUnionDecls
                @ memberDecls
                @ staticPreambleStmts
                @ body
        }
