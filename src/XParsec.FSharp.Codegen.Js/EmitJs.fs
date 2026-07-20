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

    let rec buildExpr (ctx: WalkCtx) (e: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok e)

        match e with
        | TExprG.Const(value, _, _) -> constExpr value loc

        // A bare reference to a local module FUNCTION is a value-use (an escape): it
        // wraps the flat function in an inline curried adapter so a higher-order
        // consumer (or a partial application) sees the SOURCE-shaped currying. A simple
        // single-arg / lone-unit function needs no adapter (flat == curried there).
        | TExprG.Var(k, _, _) ->
            let ident = JsExpr.Identifier(binderName ctx.Source k, loc)

            match ctx.CompiledFns.TryGetValue k with
            | true, cf when JsFlatFns.needsAdapter cf.Groups -> JsFlatFns.curryAdapter ident cf.Groups k.Offset loc
            | _ -> ident

        // An external module function — imported from its package's JS runtime module.
        // A value-use of a multi-arg / tupled external function gets the same curried
        // adapter (its producer emits flat); a saturated call flattens at the `App` arm.
        | TExprG.External(compiledName, key, _, _) ->
            let alias =
                JsExpr.Identifier(JsImports.addRef ctx.Imports compiledName (externalValueRef ctx.Provider key), loc)

            match JsFlatFns.externalGroups ctx.Provider key with
            | ValueSome groups when JsFlatFns.needsAdapter groups ->
                JsFlatFns.curryAdapter alias groups (TastWalk.exprTok e).StartIndex loc
            | _ -> alias

        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            JsExpr.Conditional(buildExpr ctx cond, buildExpr ctx thenE, buildExpr ctx elseE, loc)

        // A `Sequential` in expression position is a comma expression (top-level it
        // expands to statements via `buildStatements`).
        | TExprG.Sequential(xs, _, _) -> JsExpr.Sequence([ for x in xs -> buildExpr ctx x ], loc)

        // A tuple `(a, b, …)` is a JS array `[a, b, …]`; a pattern reads elements by index.
        | TExprG.Tuple(items, _, _) -> JsExpr.Array([ for x in items -> buildExpr ctx x ], loc)

        // Pure `let` in expression position: substitute into uses (collapse operator
        // templates). A *mutable* binder (assigned in the body) is excluded — it must
        // stay a real binding so its writes land; it falls to the IIFE arm, where the
        // arrow parameter is the (reassignable) mutable cell.
        | InlinableLet reduced -> buildExpr ctx reduced

        // Non-pure (or mutable) `let` in expression position: JS has no let-expression,
        // so lowers to an IIFE `((x) => <body>)(<value>)` — the binder evaluated once,
        // and (for a mutable binder) reassignable as the arrow parameter.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let name = binderName ctx.Source k

            JsExpr.Call(
                JsExpr.Arrow([ name ], JsFnBody.Expr(buildExpr ctx body), ValueNone),
                [ buildExpr ctx value ],
                loc
            )

        // `let _ = value in body` — a Wildcard binder discards the value, kept only for
        // its effects (`let _ = renderInto buf` over `|> ignore`, which leaves a bare
        // recipe value). A pure value contributes nothing, so drop it; otherwise a comma
        // sequence evaluates `value` then yields `body` (JS has no let-expression).
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) when isPureValue value -> buildExpr ctx body
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) ->
            JsExpr.Sequence([ buildExpr ctx value; buildExpr ctx body ], loc)

        // Anonymous lambda — no binder key, so no self-tail-call analysis applies.
        | TExprG.Lambda _ -> emitFunction ctx ValueNone e

        // Application. A SATURATED call to a module function (local or external)
        // collapses its whole spine into a single FLAT call (`f(a, b)`, tuple groups
        // flattened, lone unit dropped); any residual over-application folds on as unary
        // calls. Everything else — closures, members, under-applied module functions —
        // keeps the curried `f(a)(b)` shape (one unary call per `App`); an under-applied
        // module function reaches its head's curried adapter through this fallback.
        | TExprG.App(fn, arg, _, _) ->
            let head, spine = TastWalk.collectSpine [] e

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
                let flatHead: (JsExpr * Frozen.ArgGroup list) voption =
                    let identAt name =
                        JsExpr.Identifier(name, locOf ctx (TastWalk.exprTok head))

                    match head with
                    | TExprG.Var(k, _, _) ->
                        match ctx.CompiledFns.TryGetValue k with
                        | true, cf -> ValueSome(identAt (binderName ctx.Source k), cf.Groups)
                        | _ -> ValueNone
                    | TExprG.External(compiledName, key, _, _) ->
                        JsFlatFns.externalGroups ctx.Provider key
                        |> ValueOption.map (fun groups ->
                            identAt (JsImports.addRef ctx.Imports compiledName (externalValueRef ctx.Provider key)),
                            groups
                        )
                    | _ -> ValueNone

                match flatHead with
                | ValueSome(callee, groups) when List.length spine >= List.length groups ->
                    JsFlatFns.emitFlatCall (buildExpr ctx) callee groups spine loc
                | _ -> JsExpr.Call(buildExpr ctx fn, [ buildExpr ctx arg ], loc)

        // A record literal `{ X = e1; Y = e2 }` → `new R(args…)`, the args
        // reordered from source order to the class's *declaration*-order
        // positional constructor.
        | TExprG.RecordCons(srcFields, ty, _) ->
            let info = recordInfoOf ctx "RecordCons" ty
            let srcMap = Map.ofSeq (EqArray.toList srcFields)

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
        | TExprG.RecordClone(source, overrides, ty, _) ->
            let info = recordInfoOf ctx "RecordClone" ty
            let overrideMap = Map.ofSeq (EqArray.toList overrides)

            let argsFrom (srcRef: JsExpr) =
                [
                    for f in info.Fields ->
                        match Map.tryFind f overrideMap with
                        | Some ov -> buildExpr ctx ov
                        | None -> JsExpr.Member(srcRef, JsExpr.Identifier(f, ValueNone), false, ValueNone)
                ]

            match source with
            | TExprG.Var _ -> JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), argsFrom (buildExpr ctx source), loc)
            | _ ->
                let sName = "_rc" + string (TastWalk.exprTok e).StartIndex

                let newExpr =
                    JsExpr.New(
                        JsExpr.Identifier(info.Name, ValueNone),
                        argsFrom (JsExpr.Identifier(sName, ValueNone)),
                        loc
                    )

                JsExpr.Call(JsExpr.Arrow([ sName ], JsFnBody.Expr newExpr, ValueNone), [ buildExpr ctx source ], loc)

        // `r.X` → `r.X` — a member access on the record's like-named property
        // (the emitted class stores each field under its source field name).
        | TExprG.FieldGet(receiver, fieldName, _, _) ->
            JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(fieldName, ValueNone), false, loc)

        // `r.X <- v` → `(r.X = v)` — a mutable (`val mutable`) instance-field write,
        // the field analogue of the mutable-local `Assignment` arm. Unit-typed in F#,
        // so the yielded value is unused; in statement position `buildStatements`
        // wraps it as an expression statement.
        | TExprG.FieldSet(receiver, fieldName, value, _, _) ->
            JsExpr.Assign(
                JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(fieldName, ValueNone), false, loc),
                buildExpr ctx value,
                loc
            )

        // A union constructor `Case e0 e1 …` → `new <Union>_<Case>(args…)`. The
        // args already arrive in declaration (field) order, so — unlike a record
        // literal — no reordering is needed; the subclass constructor stores them
        // positionally under the case's field names.
        | TExprG.UnionCons(caseName, args, ty, _) ->
            let info = unionInfoOf ctx "UnionCons" ty
            let c = unionCaseFromInfo info "UnionCons" caseName

            // A local union's class is in this file; an external union's case class is
            // imported from its home module (no local re-emit).
            let callee =
                match info.Home with
                | ValueSome asm -> JsExpr.Identifier(JsImports.addTypeRef ctx.Imports asm c.ClassName, loc)
                | ValueNone -> JsExpr.Identifier(c.ClassName, ValueNone)

            JsExpr.New(callee, [ for a in args -> buildExpr ctx a ], loc)

        // External exception construction → `new <exn repr>(msg)`. The repr is sourced
        // from the `inherit` chain via `exnReprOf`; only the leading message arg is kept
        // (`Error` has no slot for further args). Non-`exn`-subtype external `New` fails loudly.
        | TExprG.New(className, _, args, ty, _) ->
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
                        match EqArray.toList args with
                        | [] -> []
                        | msg :: _ -> [ buildExpr ctx msg ]

                    JsExpr.New(JsExpr.Identifier(repr, ValueNone), errArgs, loc)
                | ValueNone ->
                    failwithf
                        "EmitJs (Step 8): construction of external type '%s' has no JS analogue (only `exn` subtypes lower to `new <exn repr>`)"
                        className

        // A member access through a LOCAL interface slot (`(r :> IRank).Rank`): the impl
        // is an ATTACHED method on the receiver's class (the plain-attached partition), so
        // dispatch as a flat member access `receiver.<member>(args)` — the free-function
        // `<Type>__<member>` form names no emitted function for an interface member. The
        // member's declaring type being a local interface is the signal (not the node's
        // `CallVia` — see `WalkCtx.LocalInterfaces`). An interface-impl PROPERTY emits as a
        // zero-arg attached method, so its read is the same member access called with no
        // args.
        | TExprG.PropertyGet(receiver, key, _, _, _) when ctx.LocalInterfaces.Contains(JsExternalMembers.declKey key) ->
            JsExpr.Call(attachedAccess ctx loc receiver key, [], loc)

        | TExprG.MethodCall(receiver, key, _, args, _, _) when
            ctx.LocalInterfaces.Contains(JsExternalMembers.declKey key)
            ->
            JsExpr.Call(attachedAccess ctx loc receiver key, [ for a in args -> buildExpr ctx a ], loc)

        // Member access on a local record/union: each member is a free receiver-first function.
        | TExprG.PropertyGet(receiver, key, _, _, _) ->
            JsExpr.Call(Members.localFn ctx key false true ValueNone, [ buildExpr ctx receiver ], loc)

        | TExprG.MethodCall(receiver, key, _, args, _, _) ->
            let withRecv =
                JsExpr.Call(Members.localFn ctx key false false ValueNone, [ buildExpr ctx receiver ], loc)

            applyArgs ctx withRecv args

        | TExprG.StaticPropertyGet(key, _, _) -> Members.localFn ctx key true true loc

        // An enum-case reference `E.Ci` → a property read on the frozen object map.
        // `StaticFieldGet` is the general static-field carrier (a class `static let`
        // backing-field read also lowers to it), so route to `enumCaseAccess` ONLY
        // when the node's type is the enum itself (`FTEnum`, stamped by Unification's
        // enum arm). A non-enum key is a class `static let` backing field, stored as a
        // property on the emitted class object (`ClassName.field`) — the store analogue
        // is `StaticFieldSet`.
        | TExprG.StaticFieldGet(enumKey, caseName, FTEnum _, _) -> enumCaseAccess ctx enumKey caseName loc
        | TExprG.StaticFieldGet(declKey, fieldName, _, _) -> staticFieldRef ctx declKey fieldName loc

        // `x <- v` on a `static let mutable` backing field → `(ClassName.field = v)`.
        // Unit-typed like `FieldSet`; the yielded value is unused in statement position.
        | TExprG.StaticFieldSet(declKey, fieldName, value, _, _) ->
            JsExpr.Assign(staticFieldRef ctx declKey fieldName loc, buildExpr ctx value, loc)

        | TExprG.StaticMethodCall(key, args, _, _) -> applyArgs ctx (Members.localFn ctx key true false loc) args

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
        | CapabilityRead ctx.Capabilities ctx.Imports (recv, emit) -> emit (buildExpr ctx recv) loc

        | TExprG.ExternalMember(receiver, key, memberName, storage, _, _) ->
            let declKey = JsExternalMembers.declKey key
            // JS has no field/property distinction at access — both are a value member
            // (the `get_`-style mangled import); only a `Method` is an arrow. (A `Field`
            // here would gain only `readonly` fidelity, not yet modelled.)
            let isProperty = storage.IsValueMember

            match JsExternalMembers.classFlagsOf ctx.Provider declKey, receiver with
            | ValueSome {
                            MemberLowering = MemberLowering.ErasedBare
                        },
              ValueSome _ ->
                failwithf
                    "EmitJs (Step 9b): erased grouping type member '%s' has an instance receiver, but a synthetic free-function-overload type carries only static members"
                    memberName
            | ValueSome {
                            MemberLowering = MemberLowering.ErasedBare
                            ImportForm = form
                        },
              ValueNone -> JsExternalMembers.erasedGroupingRef ctx.Provider ctx.Imports declKey memberName form loc
            | ValueSome {
                            MemberLowering = MemberLowering.AttachedNative
                        },
              ValueSome r when isProperty ->
                // A manifest Property is a JS DATA property — native access is a plain
                // member READ `recv.prop`, NOT a zero-arg call. (Contrast the LOCAL
                // interface-impl property path, which emits `Call(attachedAccess, [])`
                // because Vesper compiles interface properties as zero-arg methods; a
                // TS property is genuinely a data slot, not a method.)
                JsExternalMembers.attachedMember (buildExpr ctx r) memberName loc
            | ValueSome {
                            MemberLowering = MemberLowering.AttachedNative
                        },
              ValueSome r ->
                JsExternalMembers.etaWrapAttachedMethod
                    (buildExpr ctx)
                    r
                    key
                    memberName
                    (TastWalk.exprTok e).StartIndex
                    loc
            | _ ->
                JsExternalMembers.mangledMemberAccess
                    ctx.Provider
                    ctx.Imports
                    (buildExpr ctx)
                    declKey
                    receiver
                    memberName
                    isProperty
                    loc

        // `match scrut with …` → an IIFE binding the scrutinee once, then testing each
        // arm in order and `return`ing the first whose pattern (+ guard) matches; an
        // unmatched value `throw`s. Sequential test (not `switch(tag)`) so it covers
        // guards, constants, nested patterns, and non-union scrutinees uniformly.
        | TExprG.Match(scrutinee, arms, _, _) ->
            let mv = "_m" + string (TastWalk.exprTok e).StartIndex
            let access = JsExpr.Identifier(mv, ValueNone)

            let body =
                [
                    for arm in EqArray.toList arms do
                        yield! buildMatchArm ctx access arm
                    yield matchFailure
                ]

            JsExpr.Call(JsExpr.Arrow([ mv ], JsFnBody.Block body, loc), [ buildExpr ctx scrutinee ], loc)

        // A mutable-local / array-element write `lhs <- rhs` → the JS assignment
        // expression `(lhs = rhs)`. Unit-typed in F#, so its yielded value is unused;
        // in statement position `buildStatements` wraps it as an expression statement.
        | TExprG.Assignment(lhs, rhs, _, _) -> JsExpr.Assign(buildExpr ctx lhs, buildExpr ctx rhs, loc)

        // `while cond do body` in expression position. JS `while` is a statement, so it
        // lowers to a zero-arg IIFE `(() => { while (<cond>) { <body> } })()` that yields
        // `undefined` (the F# `unit` result). Statement position keeps the bare loop —
        // see `buildStatements`.
        | TExprG.While(cond, body, _, _) ->
            let loop = JsStatement.While(buildExpr ctx cond, buildStatements ctx body)
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block [ loop ], loc), [], loc)

        // `for i = a to b do body` in expression position — same IIFE wrapper as `while`;
        // `buildStatements` produces the hoisted-limit `const` + the `for` statement.
        | TExprG.ForTo _ -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // `for x in source do body` in expression position — same IIFE wrapper as `for…to`
        // (the loop yields `unit`); `buildStatements` produces the `for…of`.
        | TExprG.ForIn _ -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // `use x = value in body` in expression position. JS `try/finally` is a
        // statement, so it lowers to a zero-arg IIFE that parks the binder, `return`s
        // the body's value from the `try`, and disposes in the `finally`. The body is
        // a single expression (`Sequential` becomes a comma expression, a nested `let`
        // its own IIFE), so returning `buildExpr ctx body` preserves the result through
        // the disposal in the `finally`.
        | TExprG.Use(binding, value, body, dispose, _ty, _tok) ->
            let name = useBinderName ctx binding

            let tryFinally =
                JsStatement.TryFinally([ JsStatement.Return(buildExpr ctx body) ], disposeStmts ctx dispose name)

            let block = [ JsStatement.Const(name, buildExpr ctx value); tryFinally ]
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block block, loc), [], loc)

        // `e :> obj` (value→`obj` box, synthesised at Elaborate for an `obj` parameter/field).
        // JS is dynamically typed — every value is already a boxed `obj` — so the box is a
        // no-op; emit the source verbatim. The downcast `e :?> T` is likewise identity (no
        // runtime nominal type to check).
        | TExprG.Upcast(source, _, _)
        | TExprG.Downcast(source, _, _) -> buildExpr ctx source

        // The tokenful array intrinsics — `Array.zeroCreate` / `arr.[i]` / `arr.[i] <- v`
        // / `arr.Length`, desugared to `newarr`/`ldelem`/`stelem`/`ldlen` (the same
        // mnemonics the CLR backend reads; they are target-neutral, the element-type
        // operand is dropped on JS). They reach the backend because their inline bodies
        // live in `ops-platform.js.fs` (`array.fs`'s `zeroCreate` for `newarr`).
        | TExprG.ILIntrinsic("newarr", _, args, _, _) ->
            // `Array.zeroCreate count` → `Array(count).fill(null)`: a *dense* array (not
            // the sparse `new Array(count)`), so `Object.keys` / iteration observe every
            // slot. Unset slots read as `null`, not the element type's zero — the JS
            // zero-init erasure corner (callers fill before reading).
            match EqArray.toList args with
            | [ count ] ->
                let alloc =
                    JsExpr.Call(JsExpr.Identifier("Array", ValueNone), [ buildExpr ctx count ], ValueNone)

                let fill =
                    JsExpr.Member(alloc, JsExpr.Identifier("fill", ValueNone), false, ValueNone)

                JsExpr.Call(fill, [ JsExpr.Identifier("null", ValueNone) ], loc)
            | _ -> failwith "EmitJs: 'newarr' expects one operand (the element count)"

        // `arr.[i]` → `arr[i]` (a computed member read).
        | TExprG.ILIntrinsic("ldelem", _, args, _, _) ->
            match EqArray.toList args with
            | [ arr; idx ] -> JsExpr.Member(buildExpr ctx arr, buildExpr ctx idx, true, loc)
            | _ -> failwith "EmitJs: 'ldelem' expects two operands (array, index)"

        // `arr.[i] <- v` → `(arr[i] = v)` (a computed-member assignment expression).
        | TExprG.ILIntrinsic("stelem", _, args, _, _) ->
            match EqArray.toList args with
            | [ arr; idx; value ] ->
                let target = JsExpr.Member(buildExpr ctx arr, buildExpr ctx idx, true, ValueNone)
                JsExpr.Assign(target, buildExpr ctx value, loc)
            | _ -> failwith "EmitJs: 'stelem' expects three operands (array, index, value)"

        // `arr.Length` → `arr.length`.
        | TExprG.ILIntrinsic("ldlen", _, args, _, _) ->
            match EqArray.toList args with
            | [ arr ] -> JsExpr.Member(buildExpr ctx arr, JsExpr.Identifier("length", ValueNone), false, loc)
            | _ -> failwith "EmitJs: 'ldlen' expects one operand (the array)"

        // The empty-string identity intrinsic `(# "" x : 'U #)` — FSharp.Core's
        // erasing reinterpret (`retype`, the primitive `dynamic` enter/exit builds on).
        // It has NO runtime effect: emit the lone operand verbatim, re-typed (the CLR
        // emits nothing likewise). Handled before the generic `$N`-template expander,
        // which would (correctly) reject an operand-bearing template with no hole.
        | TExprG.ILIntrinsic("", _, args, _, _) when args.Length = 1 -> buildExpr ctx args.[0]

        | TExprG.ILIntrinsic(opCode, _, args, _, _) ->
            JsExpr.Raw(EmitJsFormat.expandTemplate buildExpr ctx opCode (EqArray.toList args), loc)

        | TExprG.Format(sink, segments, _, _) ->
            let arg = EmitJsFormat.buildFormatArg buildExpr ctx segments

            match sink with
            | FormatSinkG.ToStdOut true -> JsExpr.Call(console "log", [ arg ], loc)
            | FormatSinkG.ToStdErr true -> JsExpr.Call(console "error", [ arg ], loc)
            // `sprintf` (`State = unit`, `Residue = string`): the spliced concatenation
            // IS the result string, yielded directly as a value (no console call).
            | FormatSinkG.ToString -> arg
            | other -> failwithf "EmitJs: unsupported format sink %A" other

        | other -> failwithf "EmitJs: unsupported expression %A" other

    /// Build one `match` arm's statements: when the pattern matches (and the guard,
    /// if any, passes) the arm `return`s its body. An always-matching arm
    /// (wildcard / bare variable, `test = None`) emits a bare `Block` so its
    /// bindings stay scoped (two arms may bind the same source name); a refutable
    /// arm guards that block with `if (test)`.
    and private buildMatchArm (ctx: WalkCtx) (access: JsExpr) (arm: Frozen.TMatchArm) : JsStatement list =
        let test, binds = compileMatchPattern ctx access arm.Pat

        let inner =
            match arm.Guard with
            | None -> binds @ [ JsStatement.Return(buildExpr ctx arm.Body) ]
            | Some g ->
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
        (body: Frozen.TExpr)
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
    and emitFunction (ctx: WalkCtx) (selfKey: NodeKey voption) (lam: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok lam)
        let names, body = peelArrow ctx.Source lam
        nestUnaryArrows loc names (trampolineOrExpr ctx selfKey (List.length names) names body)

    /// Build the statements of a self-tail-call trampoline's loop body, walking
    /// tail position. A saturated tail self-call writes its arguments back to the
    /// parameter variables — through per-argument temporaries first, so an
    /// argument that reads a parameter (`sum (n-1) (acc+n)`) sees the *old* value
    /// — then `continue`s. Tail `if`/`let`/`Sequential`-tail thread through;
    /// every other tail expression `return`s its value.
    and buildTailBody (ctx: WalkCtx) (selfKey: NodeKey) (paramNames: string list) (e: Frozen.TExpr) : JsStatement list =
        let arity = List.length paramNames
        let recur = buildTailBody ctx selfKey paramNames

        match e with
        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            [ JsStatement.If(buildExpr ctx cond, recur thenE, recur elseE) ]
        | InlinableLet reduced -> recur reduced
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let binding = localBinding k body (binderName ctx.Source k) (buildExpr ctx value)
            binding :: recur body
        // `let _ = value in body` — discard the value (effects only); body stays in tail
        // position. A pure value drops away (see `buildExpr`).
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) when isPureValue value -> recur body
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) -> buildStatements ctx value @ recur body
        | TExprG.Sequential(xs, _, _) when xs.Length > 0 ->
            let items = EqArray.toList xs
            let init = items.[.. items.Length - 2]
            let last = items.[items.Length - 1]
            (init |> List.collect (buildStatements ctx)) @ recur last
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
        | _ -> [ JsStatement.Return(buildExpr ctx e) ]

    /// Curry `base` over `args` — one unary `Call` per argument, in source order
    /// (`base(a)(b)…`). Shared by the `MethodCall` / `StaticMethodCall` lowerings.
    and private applyArgs (ctx: WalkCtx) (baseExpr: JsExpr) (args: EqArray<Frozen.TExpr>) : JsExpr =
        EqArray.toList args
        |> List.fold (fun acc a -> JsExpr.Call(acc, [ buildExpr ctx a ], ValueNone)) baseExpr

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
    and attachedAccess (ctx: WalkCtx) (loc: JsLoc voption) (receiver: Frozen.TExpr) (key: SymbolKey) : JsExpr =
        // Backend name emission: the attached method is reached under its JS member name.
        let (DisplayName memberName) = SymbolKeyOps.simpleName key
        JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(memberName, ValueNone), false, loc)

    /// A value bound to a name (a module value, or a `let` binder). A `Lambda`
    /// value routes through `emitFunction` carrying its binder key, so a
    /// recursive binding (`let rec`) can recognise its own tail calls; any other
    /// value is a plain `buildExpr`.
    and emitBound (ctx: WalkCtx) (k: NodeKey) (value: Frozen.TExpr) : JsExpr =
        match value with
        | TExprG.Lambda _ -> emitFunction ctx (ValueSome k) value
        | _ -> buildExpr ctx value

    /// An expression in statement position. `Sequential` flattens; a `let` binder
    /// becomes a `const`; anything else is one `ExpressionStatement`.
    and buildStatements (ctx: WalkCtx) (e: Frozen.TExpr) : JsStatement list =
        match e with
        | TExprG.Sequential(xs, _, _) ->
            [
                for x in xs do
                    yield! buildStatements ctx x
            ]
        // Pure, immutable binder: substitute away so synthetic operand lets don't
        // surface as `const`s. A mutable binder is excluded (see `buildExpr`).
        | InlinableLet reduced -> buildStatements ctx reduced
        // A mutable binder emits a reassignable `let`; an immutable one a `const`.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let binding = localBinding k body (binderName ctx.Source k) (emitBound ctx k value)
            binding :: buildStatements ctx body
        // `let _ = value in body` — emit the discarded value as its own statement(s)
        // (effects only), then the body. A pure value drops away (see `buildExpr`).
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) when isPureValue value -> buildStatements ctx body
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) -> buildStatements ctx value @ buildStatements ctx body
        // `while cond do body` as a bare loop statement (no IIFE wrapper needed here).
        | TExprG.While(cond, body, _, _) -> [ JsStatement.While(buildExpr ctx cond, buildStatements ctx body) ]
        // `for i = a to b do body` — F# evaluates `b` once, so hoist the limit into a
        // `const` before the loop; the JS `for` then counts `i` from `a` up to that
        // limit inclusive. (JS numbers are doubles, so the CLR overflow-at-MaxValue
        // dance the IL backend needs is unnecessary — `i <= limit` is safe.)
        | TExprG.ForTo(var, _, startExpr, endExpr, body, _, _) ->
            let name = binderName ctx.Source var
            let limit = "_lim" + string (TastWalk.exprTok e).StartIndex

            [
                JsStatement.Const(limit, buildExpr ctx endExpr)
                JsStatement.For(
                    name,
                    buildExpr ctx startExpr,
                    JsExpr.Identifier(limit, ValueNone),
                    buildStatements ctx body
                )
            ]
        // `for x in source do body` — lower to a JS `for…of`, which drives the source's
        // own `Symbol.iterator` at runtime. Only the `Interface` enumerator (a source
        // typed `IEnumerable<'T>`) reaches JS codegen, and it carries no member keys (the
        // CLR backend mints the `IEnumerator` interface slots itself; JS defers to the
        // iterator protocol), so there is nothing to resolve — `for…of` over the source is
        // the whole lowering. A duck-typed `Pattern` enumerator never type-checks against
        // the BCL-free JS provider, so it is unsupported here.
        | TExprG.ForIn(pat, source, body, enumerator, _ty, _tok) ->
            match enumerator with
            | ForInEnumeratorG.Interface ->
                match pat with
                // Simple/wildcard binder: `for (const x of src)` directly.
                | TPatG.NamedSimple _
                | TPatG.Wildcard _ ->
                    let name = patBinderName ctx "_forin" pat
                    [ JsStatement.ForOf(name, buildExpr ctx source, buildStatements ctx body) ]
                // Destructuring binder — `for (k, v) in map` over `[K,V]` pairs: bind a
                // fresh loop temp and reuse `compileMatchPattern` (the SAME lowering
                // `let (k, v) = …` uses) to deconstruct it into the body head. The binder
                // must be irrefutable — a `Some test` means a nested refutable sub-pattern,
                // which a `for … in` binder cannot express, so reject it rather than emit
                // the binds without the guard.
                | TPatG.Tuple(_, _, tok) ->
                    let tmp = "_forin" + string tok.StartIndex

                    match compileMatchPattern ctx (JsExpr.Identifier(tmp, ValueNone)) pat with
                    | None, binds ->
                        [
                            JsStatement.ForOf(tmp, buildExpr ctx source, binds @ buildStatements ctx body)
                        ]
                    | Some _, _ -> failwithf "EmitJs: refutable `for … in` binder pattern is unsupported %A" pat
                | other -> failwithf "EmitJs: unsupported `for … in` binder pattern %A" other
            | ForInEnumeratorG.Pattern _ ->
                failwith
                    "EmitJs: duck-typed `for...in` (Pattern enumerator) is unsupported on JS; only IEnumerable<'T> sources lower to `for...of`"
        // `use x = value in body` — park the binder in a `const`, run the body inside a
        // `try`, and dispose the binder in the `finally` (the IL backend's exception
        // region, lowered to JS `try/finally`). The body keeps statement position.
        | TExprG.Use(binding, value, body, dispose, _ty, _tok) ->
            let name = useBinderName ctx binding

            [
                JsStatement.Const(name, buildExpr ctx value)
                JsStatement.TryFinally(buildStatements ctx body, disposeStmts ctx dispose name)
            ]
        | _ -> [ JsStatement.Expression(buildExpr ctx e) ]

    /// The JS binder name for a single-binder loop/scope pattern (`use x = …`,
    /// `for x in …`). A wildcard binder has no source name, so it gets a fresh
    /// `<prefix><tok>` slot — the value is still bound (parked/iterated) even though
    /// the body can't name it. Only simple/wildcard binders are supported; a
    /// destructuring binder (e.g. a tuple pattern) is rejected.
    and private patBinderName (ctx: WalkCtx) (prefix: string) (binding: Frozen.TPat) : string =
        match binding with
        | TPatG.NamedSimple(k, _, _) -> binderName ctx.Source k
        | TPatG.Wildcard(_, tok) -> prefix + string tok.StartIndex
        | other -> failwithf "EmitJs: unsupported single binder pattern %A" other

    and private useBinderName (ctx: WalkCtx) (binding: Frozen.TPat) : string = patBinderName ctx "_use" binding

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
        (entries: Frozen.TPreambleEntry list)
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
    let buildProgram (ctx0: WalkCtx) (tast: Frozen.TastFile) : JsProgram =
        let collected = collectTypes ctx0.Capabilities ctx0.ExportTopLevel tast

        let lowered = TastLower.lower tast.Decls

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

        for decl in tast.Decls do
            match decl with
            | TDeclG.Type({ Kind = TTypeKindG.Interface _ } as td) -> localInterfaces.Add td.TypeKey |> ignore
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
                match d with
                | TDeclG.Expression(e, _) -> isAssignedIn k e
                | TDeclG.Let(_, value, _, _) -> isAssignedIn k value
                | _ -> false
            )

        let body =
            [
                for decl in lowered do
                    match decl with
                    | TDeclG.Expression(e, _) -> yield! buildStatements ctx e
                    | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                        // A module FUNCTION emits FLAT (Fable-style); a plain value
                        // routes through `emitBound` (closures stay curried).
                        let init =
                            match ctx.CompiledFns.TryGetValue k with
                            | true, cf -> emitFlatModuleFn ctx k cf (locOf ctx (TastWalk.exprTok value))
                            | _ -> emitBound ctx k value

                        topLevelBinding ctx (reassignedAtTop k) (binderName ctx.Source k) init
                    | other -> failwithf "EmitJs: unsupported declaration %A" other
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
