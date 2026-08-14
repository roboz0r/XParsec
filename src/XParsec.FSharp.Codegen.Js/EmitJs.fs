namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers
open JsMapSources
open EmitJsCapabilities
open EmitJsTypes
open EmitJsContext

/// The `TAST → JsAst` walker. Functions emit as curried unary arrows: `f a b` → `f(a)(b)`.
module EmitJs =

    // ---- The walker ----------------------------------------------------------

    let rec buildExpr (ctx: WalkCtx) (e: TastAccessor.ExprId) : JsExpr =
        let loc = locOf ctx e

        match TastAccessor.exprKind e with
        | ExprShape.Const -> constExpr (TastAccessor.exprConstValue e) loc

        // A module function compiles FLAT (`add(a, b)`), so a bare value-use must re-curry it:
        // `let f = add` emits `(c0) => (c1) => add(c0, c1)`.
        | ExprShape.Var ->
            let k = TastAccessor.exprVarBoundVar e

            let ident = JsExpr.Identifier(boundVarName (TastAccessor.exprVarNaming e), loc)

            match ctx.CompiledFns.TryGetValue k with
            | true, cf when JsFlatFns.needsAdapter cf.Groups -> JsFlatFns.curryAdapter ctx.Pool ident cf.Groups loc
            | _ -> ident

        // An external module function, imported from its package's JS runtime module. Its
        // producer emits flat too, so a value-use gets the same curried adapter.
        | ExprShape.External ->
            let ext = TastAccessor.exprExternal e

            let alias =
                JsExpr.Identifier(
                    JsImports.addRef ctx.Imports ext.CompiledName (externalValueRef ctx.Provider ext.Key),
                    loc
                )

            match JsFlatFns.externalGroups ctx.Provider ext.Key with
            | ValueSome groups when JsFlatFns.needsAdapter groups -> JsFlatFns.curryAdapter ctx.Pool alias groups loc
            | _ -> alias

        | ExprShape.IfThenElse ->
            let i = TastAccessor.exprIfThenElse e
            JsExpr.Conditional(buildExpr ctx i.Cond, buildExpr ctx i.ThenExpr, buildExpr ctx i.ElseExpr, loc)

        // In expression position a `Sequential` is a JS comma expression.
        | ExprShape.Sequential -> JsExpr.Sequence([ for x in TastAccessor.exprChildren e -> buildExpr ctx x ], loc)

        // A tuple `(a, b, …)` is a JS array `[a, b, …]`; a pattern reads elements by index.
        | ExprShape.Tuple
        | ExprShape.ArrayLit -> JsExpr.Array([ for x in TastAccessor.exprChildren e -> buildExpr ctx x ], loc)

        | ExprShape.Let ->
            match e with
            // Pure `let` in expression position: substitute into uses. A *mutable* bound variable
            // falls below instead: it must stay a real binding so its writes land.
            | InlinableLet ctx reduced -> buildExpr ctx reduced
            | _ ->
                let l = TastAccessor.exprLet e

                match l.Pattern with
                // JS has no let-expression, so a non-pure (or mutable) `let` lowers to an IIFE
                // `((x) => <body>)(<value>)`: evaluated once, and reassignable as the parameter.
                | TastAccessor.PNamedNaming naming ->
                    JsExpr.Call(
                        JsExpr.Arrow([ boundVarName naming ], JsFnBody.Expr(buildExpr ctx l.Body), ValueNone),
                        [ buildExpr ctx l.Value ],
                        loc
                    )

                | _ ->
                    match TastAccessor.patKind l.Pattern with
                    // `let _ = value in body` keeps the value only for its effects: a pure value
                    // drops, else a comma sequence evaluates it then yields `body`.
                    | PatShape.Wildcard when isPureValue l.Value -> buildExpr ctx l.Body
                    | PatShape.Wildcard -> JsExpr.Sequence([ buildExpr ctx l.Value; buildExpr ctx l.Body ], loc)
                    // A destructuring `let (a, b) = value in body` — the same IIFE, its arrow
                    // parameter the array destructuring a tuple parameter already takes.
                    | PatShape.Tuple ->
                        JsExpr.Call(
                            JsExpr.Arrow(
                                [ lambdaParamName ctx.Pool l.Pattern ],
                                JsFnBody.Expr(buildExpr ctx l.Body),
                                ValueNone
                            ),
                            [ buildExpr ctx l.Value ],
                            loc
                        )
                    | _ -> failwithf "EmitJs: unsupported expression %A" e

        // An anonymous lambda has no bound variable key, so no self-tail-call analysis applies.
        | ExprShape.Lambda -> emitFunction ctx ValueNone e

        // A SATURATED call to a module function (local or external) collapses all its
        // arguments into a single FLAT call (`f(a, b)`; tuple groups flattened, lone unit
        // dropped). Everything else keeps the curried `f(a)(b)`, one unary call per `App`.
        | ExprShape.App ->
            let av = TastAccessor.exprApp e
            let fn, appArgs = TastAccessor.collectAppChain [] e

            let folded =
                match tryCapabilityCall ctx.Capabilities ctx.Imports (buildExpr ctx) fn appArgs loc with
                | ValueSome call -> ValueSome call
                | ValueNone -> JsExternalMembers.tryAttachedCall ctx.Provider ctx.Pool (buildExpr ctx) fn appArgs loc

            match folded with
            | ValueSome call -> call
            | ValueNone ->
                // An applied function naming a module function → its flat callee + SOURCE
                // groups. The groups are non-empty by construction, so saturation is the
                // argument count reaching the group count; anything else stays curried.
                let flatFn: (JsExpr * TastAccessor.ArgGroup list) voption =
                    let identAt name = JsExpr.Identifier(name, locOf ctx fn)

                    match TastAccessor.exprKind fn with
                    | ExprShape.Var ->
                        let k = TastAccessor.exprVarBoundVar fn

                        match ctx.CompiledFns.TryGetValue k with
                        | true, cf -> ValueSome(identAt (boundVarNameOf ctx.Pool k), cf.Groups)
                        | _ -> ValueNone
                    | ExprShape.External ->
                        let ext = TastAccessor.exprExternal fn

                        JsFlatFns.externalGroups ctx.Provider ext.Key
                        |> ValueOption.map (fun groups ->
                            identAt (
                                JsImports.addRef ctx.Imports ext.CompiledName (externalValueRef ctx.Provider ext.Key)
                            ),
                            groups
                        )
                    | _ -> ValueNone

                match flatFn with
                | ValueSome(callee, groups) when List.length appArgs >= List.length groups ->
                    JsFlatFns.emitFlatCall ctx.Pool (buildExpr ctx) callee groups appArgs loc
                | _ -> JsExpr.Call(buildExpr ctx av.Fn, [ buildExpr ctx av.Arg ], loc)

        // A record literal `{ X = e1; Y = e2 }` → `new R(args…)`, the args reordered from
        // source order to the class's *declaration*-order positional constructor.
        | ExprShape.RecordCons ->
            let info = recordInfoOf ctx "RecordCons" (TastAccessor.exprTy e)
            let srcMap = Map.ofSeq (TastAccessor.exprRecordConsFields e)

            let args =
                [
                    for f in info.Fields ->
                        match Map.tryFind f srcMap with
                        | Some e -> buildExpr ctx e
                        | None -> failwithf "EmitJs: record literal for '%s' is missing field '%s'" info.Name f
                ]

            JsExpr.New(nominalCtorRef ctx info.Home info.Name ValueNone, args, loc)

        // `{ r with X = v; … }` → reconstruction `new R(…)`: each field takes its override
        // if listed, else reads `<src>.field`. `<src>` is read once per copied field, so a
        // bare `Var` splices inline and any other source binds once through an IIFE.
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

            let ctor = nominalCtorRef ctx info.Home info.Name ValueNone

            match TastAccessor.exprKind rc.Source with
            | ExprShape.Var -> JsExpr.New(ctor, argsFrom (buildExpr ctx rc.Source), loc)
            | _ ->
                let sName = freshTemp ctx.Pool "_rc"

                let newExpr = JsExpr.New(ctor, argsFrom (JsExpr.Identifier(sName, ValueNone)), loc)

                JsExpr.Call(JsExpr.Arrow([ sName ], JsFnBody.Expr newExpr, ValueNone), [ buildExpr ctx rc.Source ], loc)

        // `r.X` → `r.X`: the emitted class stores each field under its source field name.
        | ExprShape.FieldGet ->
            let fg = TastAccessor.exprFieldGet e
            JsExpr.Member(buildExpr ctx fg.ObjArg, JsExpr.Identifier(fg.FieldName, ValueNone), false, loc)

        // `r.X <- v` → `(r.X = v)`, a `val mutable` instance-field write. Unit-typed in F#,
        // so the yielded value is unused; statement position wraps it as a statement.
        | ExprShape.FieldSet ->
            let fs = TastAccessor.exprFieldSet e

            JsExpr.Assign(
                JsExpr.Member(buildExpr ctx fs.ObjArg, JsExpr.Identifier(fs.FieldName, ValueNone), false, loc),
                buildExpr ctx fs.Value,
                loc
            )

        // A union constructor `Case e0 e1 …` → `new <Union>_<Case>(args…)`. The args already
        // arrive in declaration (field) order, so no reordering, unlike a record literal.
        | ExprShape.UnionCons ->
            let info = unionInfoOf ctx "UnionCons" (TastAccessor.exprTy e)
            let c = unionCaseFromInfo info "UnionCons" (TastAccessor.exprUnionConsCaseName e)

            // A local union's class is in this file; an external union's case class is
            // imported from its home module, never re-emitted here.
            let callee = nominalCtorRef ctx info.Home c.ClassName loc

            JsExpr.New(callee, [ for a in TastAccessor.exprChildren e -> buildExpr ctx a ], loc)

        // External exception construction → `new <exn repr>(msg)`, the repr sourced from the
        // `inherit` chain; only the leading message arg is kept, as `Error` has no slot for
        // further ones. An external type that is no `exn` subtype has no analogue and faults.
        | ExprShape.New ->
            let ty = TastAccessor.exprTy e
            let args = TastAccessor.exprChildren e
            // A locally-emitted class constructs by its emitted name with positional args,
            // the ctor storing each into the like-named field.
            let localClassName =
                match TastLower.objArgShape ty with
                | ValueSome(key, _) ->
                    match ctx.Classes.TryGetValue key with
                    | true, name -> ValueSome name
                    | _ -> ValueNone
                | ValueNone -> ValueNone

            // A GLOBAL (ambient) external class constructs by its BARE export name with NO
            // import, because the JS runtime provides it intrinsically. That name is the key's
            // simple name (`Js.Widget` → `Widget`).
            let globalClassName =
                match TastLower.objArgShape ty with
                | ValueSome(key, _) ->
                    JsExternalMembers.classFlagsOf ctx.Provider key
                    |> ValueOption.filter (fun flags -> flags.Global)
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
                        "EmitJs: construction of external type '%s' has no JS analogue (only `exn` subtypes lower to `new <exn repr>`)"
                        (TastAccessor.exprNewClassName e)

        // A LOCAL interface slot's impl is an ATTACHED method on the object argument's class, so
        // it dispatches as `objArg.<member>(args)`. No free `<Type>__<member>` function is
        // emitted for an interface member. An impl PROPERTY is a zero-arg attached method.
        | ExprShape.PropertyGet ->
            let pg = TastAccessor.exprPropertyGet e

            if ctx.LocalInterfaces.Contains(JsExternalMembers.declKey pg.Key) then
                JsExpr.Call(attachedAccess ctx loc pg.ObjArg pg.Key, [], loc)
            else
                JsExpr.Call(Members.localFn ctx pg.Key false true ValueNone, [ buildExpr ctx pg.ObjArg ], loc)

        | ExprShape.MethodCall ->
            let mc = TastAccessor.exprMethodCall e

            if ctx.LocalInterfaces.Contains(JsExternalMembers.declKey mc.Key) then
                JsExpr.Call(attachedAccess ctx loc mc.ObjArg mc.Key, [ for a in mc.Args -> buildExpr ctx a ], loc)
            else
                let withObjArg =
                    JsExpr.Call(Members.localFn ctx mc.Key false false ValueNone, [ buildExpr ctx mc.ObjArg ], loc)

                applyArgs ctx withObjArg mc.Args

        | ExprShape.StaticPropertyGet -> Members.localFn ctx (TastAccessor.exprStaticPropertyGetKey e) true true loc

        // `StaticFieldGet` is the general static-field carrier, so an enum-case reference
        // `E.Ci` (a property read on the frozen object map) is recognised by the node's TYPE
        // being the enum. Any other key is a `static let` backing field, `ClassName.field`.
        | ExprShape.StaticFieldGet ->
            let sfg = TastAccessor.exprStaticFieldGet e
            let declKey = SymbolKeyOps.asTypeKey "EmitJs: static field" sfg.Key

            match TastAccessor.exprTy e with
            | FTEnum _ -> enumCaseAccess ctx declKey sfg.FieldName loc
            | _ -> staticFieldRef ctx declKey sfg.FieldName loc

        // `x <- v` on a `static let mutable` backing field → `(ClassName.field = v)`.
        | ExprShape.StaticFieldSet ->
            let sfs = TastAccessor.exprStaticFieldSet e
            let declKey = SymbolKeyOps.asTypeKey "EmitJs: static field" sfs.Key
            JsExpr.Assign(staticFieldRef ctx declKey sfs.FieldName loc, buildExpr ctx sfs.Value, loc)

        | ExprShape.StaticMethodCall ->
            applyArgs
                ctx
                (Members.localFn ctx (TastAccessor.exprStaticMethodCallKey e) true false loc)
                (EqArray.ofArray (TastAccessor.exprChildren e))

        // A member on an external type, reached WITHOUT being applied; the call forms fold at
        // the applied function off the same `MemberDispatch`. A STATIC member has no native
        // lowering, so every dispatch but the erased one falls to the mangled import.
        | ExprShape.ExternalMember ->
            match e with
            | CapabilityRead ctx.Capabilities ctx.Imports (objArg, emit) -> emit (buildExpr ctx objArg) loc
            | _ ->
                let em = TastAccessor.exprExternalMember e
                let declKey = JsExternalMembers.declKey em.Key

                match JsExternalMembers.dispatchOf ctx.Provider declKey em.Storage, em.ObjArg with
                | MemberDispatch.ErasedBare _, ValueSome _ ->
                    failwithf
                        "EmitJs: erased grouping type member '%s' has an object argument, but a synthetic free-function-overload type carries only static members"
                        em.MemberName
                | MemberDispatch.ErasedBare form, ValueNone ->
                    JsExternalMembers.erasedGroupingRef ctx.Provider ctx.Imports declKey em.MemberName form loc
                | MemberDispatch.Application, ValueSome r ->
                    JsExternalMembers.etaWrapApplication (buildExpr ctx) r em.ArgGroupWidths ctx.Pool loc
                | MemberDispatch.NativeData, ValueSome r ->
                    JsExternalMembers.attachedMember (buildExpr ctx r) em.MemberName loc
                | MemberDispatch.AttachedMethod, ValueSome r ->
                    JsExternalMembers.etaWrapAttachedMethod
                        (buildExpr ctx)
                        r
                        em.ArgGroupWidths
                        em.MemberName
                        ctx.Pool
                        loc
                | MemberDispatch.InterfaceProperty, ValueSome r ->
                    JsExpr.Call(JsExternalMembers.attachedMember (buildExpr ctx r) em.MemberName loc, [], loc)
                // JS has no field/property distinction at access, so a `Field` and a `Property`
                // both take the `get_`-style mangled import; only a `Method` is a function.
                | MemberDispatch.TypePrefixedImport, _
                | MemberDispatch.Application, ValueNone
                | MemberDispatch.NativeData, ValueNone
                | MemberDispatch.AttachedMethod, ValueNone
                | MemberDispatch.InterfaceProperty, ValueNone ->
                    JsExternalMembers.mangledMemberAccess
                        ctx.Provider
                        ctx.Imports
                        (buildExpr ctx)
                        declKey
                        em.ObjArg
                        em.MemberName
                        em.Storage.IsValueMember
                        loc

        // `match scrut with …` → an IIFE binding the scrutinee once, testing each arm in
        // order and `return`ing the first that matches; an unmatched value `throw`s. Tested
        // sequentially, not as `switch(tag)`, so guards and nested patterns fit uniformly.
        | ExprShape.Match ->
            let m = TastAccessor.exprMatch e
            let mv = freshTemp ctx.Pool "_m"
            let access = JsExpr.Identifier(mv, ValueNone)

            let body =
                [
                    for arm in m.Arms do
                        yield! buildMatchArm ctx access arm
                    yield matchFailure
                ]

            JsExpr.Call(JsExpr.Arrow([ mv ], JsFnBody.Block body, loc), [ buildExpr ctx m.Scrutinee ], loc)

        // A mutable-local / array-element write `lhs <- rhs` → the JS assignment expression
        // `(lhs = rhs)`. Unit-typed in F#, so its yielded value is unused.
        | ExprShape.Assignment ->
            let a = TastAccessor.exprAssignment e
            JsExpr.Assign(buildExpr ctx a.Lhs, buildExpr ctx a.Rhs, loc)

        // JS `while` is a statement, so `while cond do body` in expression position lowers to
        // a zero-arg IIFE `(() => { while (<cond>) { <body> } })()` yielding `undefined`.
        | ExprShape.While ->
            let w = TastAccessor.exprWhile e
            let loop = JsStatement.While(buildExpr ctx w.Cond, buildStatements ctx w.Body)
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block [ loop ], loc), [], loc)

        // `for i = a to b do body` in expression position — the same IIFE wrapper as `while`.
        | ExprShape.ForTo -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // `for x in source do body` in expression position — the same IIFE wrapper.
        | ExprShape.ForIn -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // JS `try/finally` is a statement, so `use x = value in body` in expression position
        // lowers to a zero-arg IIFE parking the bound variable, `return`ing the body's value from the
        // `try` and disposing in the `finally`.
        | ExprShape.Use ->
            let u = TastAccessor.exprUse e
            let name = useBoundVarName ctx u.Pattern

            let tryFinally =
                JsStatement.TryFinally([ JsStatement.Return(buildExpr ctx u.Body) ], disposeStmts ctx u.Dispose name)

            let block = [ JsStatement.Const(name, buildExpr ctx u.Value); tryFinally ]
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block block, loc), [], loc)

        // `try body finally cleanup` in expression position — the same zero-arg IIFE `use`
        // takes, `return`ing the body's value and running `cleanup` for effect.
        | ExprShape.TryFinally ->
            let tf = TastAccessor.exprTryFinally e

            let tryFinally =
                JsStatement.TryFinally([ JsStatement.Return(buildExpr ctx tf.Body) ], buildStatements ctx tf.Cleanup)

            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block [ tryFinally ], loc), [], loc)

        // Every JS value is already a boxed `obj`, so `e :> obj` is a no-op emitting its source
        // verbatim, and `e :?> T` is likewise identity.
        | ExprShape.Upcast
        | ExprShape.Downcast -> buildExpr ctx (TastAccessor.exprChild e 0)

        // `expr when ^T : int = …` clauses are a COMPILE-TIME selection made when a splice
        // pins the operand type. Reaching the backend means none was pinned, so emit the
        // default; a clause is an optimisation over it, never a different meaning.
        | ExprShape.StaticOptimization -> buildExpr ctx (TastAccessor.exprStaticOptimizationDefault e)

        // The array intrinsics carry the same `newarr`/`ldelem`/`stelem`/`ldlen` mnemonics the
        // CLR backend reads: target-neutral, with the element-type operand dropped on JS.
        | ExprShape.ILIntrinsic ->
            let args = TastAccessor.exprChildren e

            match TastAccessor.exprILIntrinsicOpCode e with
            | "newarr" ->
                // `Array.zeroCreate count` → `Array(count).fill(null)`: DENSE, not the sparse
                // `new Array(count)`, so `Object.keys` / iteration observe every slot. Unset
                // slots read as `null`, not the element type's zero.
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

            // The empty-string identity intrinsic `(# "" x : 'U #)` is FSharp.Core's erasing
            // reinterpret: NO runtime effect, so emit the lone operand verbatim. Handled ahead
            // of the `$N`-template expander, which rejects an operand-bearing template.
            | "" when args.Length = 1 -> buildExpr ctx args.[0]

            | opCode -> JsExpr.Raw(EmitJsFormat.expandTemplate buildExpr ctx opCode (List.ofArray args), loc)

        | ExprShape.Format ->
            let fv = TastAccessor.exprFormat e
            let arg = EmitJsFormat.buildFormatArg buildExpr ctx fv.Segments

            match fv.Sink with
            | FormatSinkG.ToStdOut true -> JsExpr.Call(console "log", [ arg ], loc)
            | FormatSinkG.ToStdErr true -> JsExpr.Call(console "error", [ arg ], loc)
            // For `sprintf` the spliced concatenation IS the result string, yielded as a value.
            | FormatSinkG.ToString -> arg
            | other -> failwithf "EmitJs: unsupported format sink %A" other

        // Legitimate F# the walker has no lowering for yet: `TryWith` wants catch-side arm
        // matching, and `TypeTest` (`e :? T`) has no runtime nominal identity to test on this
        // backend. That is the expression form of the gap `compileMatchPattern` also refuses.
        | ExprShape.Null
        | ExprShape.TryWith
        | ExprShape.TypeTest -> failwithf "EmitJs: unsupported expression %A" e

        // NOT a target gap: `..` is an ordinary F# operator, so a range wants no node of its
        // own, and `Range` exists only to carry a use no seq is materialised for. Elaborate
        // reports every one it mints, so reaching here means a program already known bad.
        | ExprShape.Range -> failwithf "EmitJs: unsupported expression %A" e

        | ExprShape.InlineCall -> TastLower.inlineCallUnexpanded (TastAccessor.exprInlineCallSpec e)
        | ExprShape.CallerExpr -> TastLower.callerExprUnexpanded ()
        | ExprShape.TraitCall -> TastLower.traitCallUnresolved (TastAccessor.exprTraitCallMemberName e)

    /// Build one `match` arm's statements: a matching pattern (and passing guard) `return`s
    /// the body. An always-matching arm emits a bare `Block` so its bindings stay scoped, since
    /// two arms may bind the same source name; a refutable arm guards it with `if (test)`.
    and private buildMatchArm (ctx: WalkCtx) (access: JsExpr) (arm: TastAccessor.Arm) : JsStatement list =
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

    /// The body of a function with parameters `ps`: a `while (true)` trampoline when
    /// `selfKey`'s body makes a saturated tail self-call, else the plain expression.
    and private trampolineOrExpr
        (ctx: WalkCtx)
        (selfKey: BoundVarId voption)
        (ps: TrampolineParams)
        (body: TastAccessor.ExprId)
        : JsFnBody =
        match selfKey with
        | ValueSome k when hasTailSelfCall k ps body ->
            JsFnBody.Block
                [
                    JsStatement.While(
                        JsExpr.Literal(JsLiteral.Boolean true, ValueNone),
                        buildTrampolineBody ctx k ps body
                    )
                ]
        | _ -> JsFnBody.Expr(buildExpr ctx body)

    /// Emit a function value as nested *unary* arrows. That shape keeps every arrow's param
    /// in scope at the innermost body, which is what lets a `while (true)` trampoline there
    /// write them back and `continue`.
    and emitFunction (ctx: WalkCtx) (selfKey: BoundVarId voption) (lam: TastAccessor.ExprId) : JsExpr =
        let loc = locOf ctx lam
        let names, body = peelLambdas ctx.Pool lam
        nestUnaryArrows loc names (trampolineOrExpr ctx selfKey (TrampolineParams.Unary names) body)

    /// Build the statements of a self-tail-call trampoline's loop body, walking tail
    /// position. A saturated tail self-call writes its arguments back to the parameter
    /// variables and `continue`s; every other tail expression `return`s its value.
    and buildTrampolineBody
        (ctx: WalkCtx)
        (selfKey: BoundVarId)
        (ps: TrampolineParams)
        (e: TastAccessor.ExprId)
        : JsStatement list =
        let recur = buildTrampolineBody ctx selfKey ps

        match e with
        | InlinableLet ctx reduced -> recur reduced
        | TailSelfCall selfKey ps args ->
            // Args arrive one per SOURCE application: a tuple group opens onto several flat
            // params (an impure tuple spilling to `_tg`), a lone unit group onto none.
            let flatArgs, spills =
                match ps with
                | TrampolineParams.Unary _ -> [ for a in args -> buildExpr ctx a ], []
                | TrampolineParams.Flat(groups, _) -> JsFlatFns.flattenGroupArgs ctx.Pool (buildExpr ctx) groups args

            // `_tc<i>` temporaries: evaluate every new argument before any write-back, so a
            // self-call arg mentioning a parameter reads its pre-iteration value.
            let tmp i = "_tc" + string i

            [ for (n, v) in spills -> JsStatement.Const(n, v) ]
            @ [ for i, a in List.indexed flatArgs -> JsStatement.Const(tmp i, a) ]
            @ [
                for i, name in List.indexed ps.Names -> JsStatement.Assign(name, JsExpr.Identifier(tmp i, ValueNone))
            ]
            @ [ JsStatement.Continue ]
        | _ ->
            match TastAccessor.exprKind e with
            | ExprShape.IfThenElse ->
                let i = TastAccessor.exprIfThenElse e
                [ JsStatement.If(buildExpr ctx i.Cond, recur i.ThenExpr, recur i.ElseExpr) ]
            | ExprShape.Let ->
                let l = TastAccessor.exprLet e

                match l.Pattern with
                | TastAccessor.PNamed k ->
                    let binding =
                        localBinding k l.Body (boundVarNameOf ctx.Pool k) (buildExpr ctx l.Value)

                    binding :: recur l.Body
                | _ ->
                    match TastAccessor.patKind l.Pattern with
                    // `let _ = value in body` — the body stays in tail position.
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

    /// `base(a)(b)…` — one unary `Call` per argument, in source order.
    and private applyArgs (ctx: WalkCtx) (baseExpr: JsExpr) (args: EqArray<TastAccessor.ExprId>) : JsExpr =
        args
        |> EqArray.fold (fun acc a -> JsExpr.Call(acc, [ buildExpr ctx a ], ValueNone)) baseExpr

    /// Emit a local module FUNCTION as one FLAT arrow over its compiled parameters
    /// (`let f x y` → `(x, y) => …`; tuple groups flattened, a lone unit erased to
    /// `() => …`). A trampoline mutates those flat parameters in place.
    and private emitFlatModuleFn
        (ctx: WalkCtx)
        (k: BoundVarId)
        (cf: CompiledFns.CompiledFn)
        (loc: JsLoc voption)
        : JsExpr =
        let names = [ for p in cf.Params -> JsFlatFns.paramNameOf ctx.Pool p ]
        let ps = TrampolineParams.Flat(cf.Groups, names)
        JsExpr.Arrow(names, trampolineOrExpr ctx (ValueSome k) ps cf.Body, loc)

    /// `objArg.<member>` for a call dispatched through a local interface slot. The member
    /// resolves to the attached method emitted on the object argument's class, under its JS name.
    and attachedAccess (ctx: WalkCtx) (loc: JsLoc voption) (objArg: TastAccessor.ExprId) (key: SymbolKey) : JsExpr =
        let (DisplayName memberName) = SymbolKeyOps.simpleName key
        JsExpr.Member(buildExpr ctx objArg, JsExpr.Identifier(memberName, ValueNone), false, loc)

    /// A value bound to a name. A `Lambda` value carries its bound variable key down, so a recursive
    /// binding (`let rec`) can recognise its own tail calls.
    and emitBound (ctx: WalkCtx) (k: BoundVarId) (value: TastAccessor.ExprId) : JsExpr =
        match TastAccessor.exprKind value with
        | ExprShape.Lambda -> emitFunction ctx (ValueSome k) value
        | _ -> buildExpr ctx value

    /// An expression in statement position. `Sequential` flattens; a `let` bound variable
    /// becomes a `const`; anything else is one `ExpressionStatement`.
    and buildStatements (ctx: WalkCtx) (e: TastAccessor.ExprId) : JsStatement list =
        match e with
        // Pure, immutable bound variable: substitute away so synthetic operand lets don't surface
        // as `const`s.
        | InlinableLet ctx reduced -> buildStatements ctx reduced
        | _ ->
            match TastAccessor.exprKind e with
            | ExprShape.Sequential ->
                [
                    for x in TastAccessor.exprChildren e do
                        yield! buildStatements ctx x
                ]
            | ExprShape.Let ->
                let l = TastAccessor.exprLet e

                match l.Pattern with
                // A mutable bound variable emits a reassignable `let`; an immutable one a `const`.
                | TastAccessor.PNamed k ->
                    let binding =
                        localBinding k l.Body (boundVarNameOf ctx.Pool k) (emitBound ctx k l.Value)

                    binding :: buildStatements ctx l.Body
                | _ ->
                    match TastAccessor.patKind l.Pattern with
                    // `let _ = value in body` — the discarded value emits as its own
                    // statement(s), for its effects, then the body.
                    | PatShape.Wildcard when isPureValue l.Value -> buildStatements ctx l.Body
                    | PatShape.Wildcard -> buildStatements ctx l.Value @ buildStatements ctx l.Body
                    | _ -> [ JsStatement.Expression(buildExpr ctx e) ]
            // `while cond do body` as a bare loop statement — no IIFE wrapper needed here.
            | ExprShape.While ->
                let w = TastAccessor.exprWhile e
                [ JsStatement.While(buildExpr ctx w.Cond, buildStatements ctx w.Body) ]
            // F# evaluates `b` once, so the limit hoists into a `const` before the loop and
            // `i` counts up to it inclusive. JS numbers are doubles, so `i <= limit` is safe
            // without the overflow-at-MaxValue dance the IL backend needs.
            | ExprShape.ForTo ->
                let ft = TastAccessor.exprForTo e
                let name = boundVarNameOf ctx.Pool ft.Var
                let limit = freshTemp ctx.Pool "_lim"

                [
                    JsStatement.Const(limit, buildExpr ctx ft.EndExpr)
                    JsStatement.For(
                        name,
                        buildExpr ctx ft.StartExpr,
                        JsExpr.Identifier(limit, ValueNone),
                        buildStatements ctx ft.Body
                    )
                ]
            // `for x in source do body` → a JS `for…of`, which drives the source's own
            // `Symbol.iterator` at runtime. The `Interface` enumerator carries no member keys
            // to resolve, because JS defers to the iterator protocol where the CLR mints slots.
            | ExprShape.ForIn ->
                let fi = TastAccessor.exprForIn e

                match fi.Enumerator with
                | ForInEnumeratorG.Interface ->
                    match TastAccessor.patKind fi.Pat with
                    // Simple/wildcard bound variable: `for (const x of src)` directly.
                    | PatShape.NamedSimple
                    | PatShape.Wildcard ->
                        let name = patBoundVarName ctx "_forin" fi.Pat

                        [
                            JsStatement.ForOf(name, buildExpr ctx fi.Source, buildStatements ctx fi.Body)
                        ]
                    // `for (k, v) in map`: a fresh loop temp deconstructed into the body's first
                    // statement. It must be irrefutable; a `Some test` is not.
                    | PatShape.Tuple ->
                        let tmp = freshTemp ctx.Pool "_forin"

                        match compileMatchPattern ctx (JsExpr.Identifier(tmp, ValueNone)) fi.Pat with
                        | None, binds ->
                            [
                                JsStatement.ForOf(tmp, buildExpr ctx fi.Source, binds @ buildStatements ctx fi.Body)
                            ]
                        | Some _, _ ->
                            failwithf "EmitJs: refutable `for … in` bound variable pattern is unsupported %A" fi.Pat
                    | _ -> failwithf "EmitJs: unsupported `for … in` bound variable pattern %A" fi.Pat
                | ForInEnumeratorG.Pattern _ ->
                    failwith
                        "EmitJs: duck-typed `for...in` (Pattern enumerator) is unsupported on JS; only IEnumerable<'T> sources lower to `for...of`"
            // `use x = value in body` — park the bound variable in a `const`, run the body inside a
            // `try`, dispose in the `finally`. The body keeps statement position.
            | ExprShape.Use ->
                let u = TastAccessor.exprUse e
                let name = useBoundVarName ctx u.Pattern

                [
                    JsStatement.Const(name, buildExpr ctx u.Value)
                    JsStatement.TryFinally(buildStatements ctx u.Body, disposeStmts ctx u.Dispose name)
                ]
            // In statement position the body and cleanup stay statements, mapping straight
            // onto JS `try/finally` with no IIFE.
            | ExprShape.TryFinally ->
                let tf = TastAccessor.exprTryFinally e

                [
                    JsStatement.TryFinally(buildStatements ctx tf.Body, buildStatements ctx tf.Cleanup)
                ]
            | _ -> [ JsStatement.Expression(buildExpr ctx e) ]

    /// The JS name for a single-name loop/scope pattern (`use x = …`, `for x in …`).
    /// A wildcard gets a fresh temporary: still bound, though the body cannot name it.
    and private patBoundVarName (ctx: WalkCtx) (prefix: string) (pattern: TastAccessor.PatId) : string =
        match pattern with
        | TastAccessor.PNamedNaming naming -> boundVarName naming
        | _ ->
            match TastAccessor.patKind pattern with
            | PatShape.Wildcard -> freshTemp ctx.Pool prefix
            | _ -> failwithf "EmitJs: unsupported single-name pattern %A" pattern

    and private useBoundVarName (ctx: WalkCtx) (pattern: TastAccessor.PatId) : string =
        patBoundVarName ctx "_use" pattern

    /// The `finally` body that disposes a `use` bound variable: a null-guarded disposal call, since
    /// F# `use` is null-safe and JS loose `!= null` catches both `null` and `undefined`. The
    /// capability's JS slot is the same `boundVar[Symbol.dispose]()` a disposable impl emits.
    and private disposeStmts (ctx: WalkCtx) (dispose: Disposal) (name: string) : JsStatement list =
        let boundVar = JsExpr.Identifier(name, ValueNone)

        let guard =
            JsExpr.Binary("!=", boundVar, JsExpr.Identifier("null", ValueNone), ValueNone)

        let disposeCall =
            match dispose with
            // The CLR-only interface `slot` key the node carries is irrelevant here, because
            // JS names its own slot.
            | Disposal.ViaCapability _ -> disposeSlotCall boundVar ValueNone
            // Ref-struct carve-out / an external type's own pattern `Dispose()`: call the
            // keyed member's free type-prefixed function.
            | Disposal.ViaOwnMember key ->
                let disposeFn = Members.localFn ctx key false false ValueNone
                JsExpr.Call(disposeFn, [ boundVar ], ValueNone)
            | Disposal.Unresolved ->
                failwithf
                    "EmitJs: `use` over a bound variable with no resolved disposal ('%s') — Unification reported an error, so this file should never have reached codegen"
                    name

        [ JsStatement.If(guard, [ JsStatement.Expression disposeCall ], []) ]

    /// A class's instance preamble is the END of its primary constructor: declaration order
    /// is load-bearing, hence ctor statements rather than class-field initialisers. Every `let`
    /// is an instance FIELD `this.<name> = <init>`, `let mutable` too: one storage, no ref cell.
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

    /// A `val`-form class's explicit `new(args) = let … in { f = e; … }` → the JS constructor
    /// it IS: its own parameters, its `let`s as `const` locals, then one `this.f = e` store
    /// per field initialiser. Fields the source leaves out stay absent.
    let private emitExplicitCtor (ctx: WalkCtx) (sc: TastAccessor.SecondaryCtor) : JsCtor =
        {
            Params =
                [
                    for (pk, _) in sc.Params -> boundVarNameOf ctx.Pool (BoundVarKey.identity pk)
                ]
            Body =
                [
                    for l in sc.Lets ->
                        JsStatement.Const(
                            boundVarNameOf ctx.Pool (BoundVarKey.identity l.BoundVar),
                            buildExpr ctx l.Init
                        )
                    for fi in sc.FieldInits -> JsStatement.FieldStore(fi.Field, buildExpr ctx fi.Init)
                ]
        }

    /// A class's `static let` / `static do` preamble → module-load statements storing the
    /// `ClassName.x` slot. Declaration order is load-bearing: an initialiser may read an earlier
    /// `static let`.
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

    /// The whole frozen file → a `Program`. Type declarations become JS `class`es first, as
    /// classes are not hoisted; lowering the rest drops `let inline` templates and `type` decls.
    let buildProgram (ctx0: WalkCtx) : JsProgram =
        // The specialization graph splices HERE, before anything reads a decl: an edge left
        // standing in a member body would reach the emit router with no body to emit.
        let expansion =
            InlineExpand.expand ctx0.Pool (TastAccessor.roots ctx0.Pool |> List.ofArray)

        let moduleMembers = TastPoolBuilder.moduleMembers ctx0.Pool

        let declaredSymbol (d: TastAccessor.DeclId) : SymbolKey voption =
            match TastAccessor.declKind d with
            | DeclShape.Type -> ValueSome (TastAccessor.declType d).Key
            | DeclShape.Let ->
                match (TastAccessor.declLet d).Pattern with
                | TastAccessor.PNamed b ->
                    match moduleMembers.TryGetValue b with
                    | true, info -> ValueSome info.Key
                    | _ -> ValueNone
                | _ -> ValueNone
            | DeclShape.Expression -> ValueNone

        // Declared to be the TARGET'S OWN: `type x = (# "repr" #)` names a platform
        // representation (`int` IS `number`) and `[<Global>]` a target global (`undefined`).
        // Neither can emit a definition, and each reference emits the front end's splice.
        let intrinsicReprs = TastPoolBuilder.intrinsicReprKeys ctx0.Pool
        let globals = TastPoolBuilder.globalValueKeys ctx0.Pool

        let decls =
            expansion.Decls
            |> List.filter (fun d ->
                match declaredSymbol d with
                | ValueSome key -> not (intrinsicReprs.ContainsKey key || globals.Contains key)
                | ValueNone -> true
            )

        // Where each spliced node was WRITTEN, plus the authorship chain those origins are keyed
        // along. The walk keeps deriving nodes, so it takes that relation over unfinished.
        let reached = System.Collections.Generic.HashSet<OriginPath>()

        for KeyValue(node, origin) in expansion.Origins do
            ctx0.NodeOrigins.[node] <- origin
            reached.Add origin.File.Path |> ignore

        InlineExpand.Derivation.absorb ctx0.Derivation expansion.Derived

        // The producer files this program reached get a slot in the map's `sources[]`. It walks
        // the RETENTION, which yields in path order, keeping what the expansion named but not
        // the expansion's dictionary order, so two builds of one program publish the same map.
        match ctx0.Resolver with
        | ValueNone -> ()
        | ValueSome r ->
            for src in OriginSources.toList r.Origins do
                if reached.Contains src.File.Path then
                    MapSources.publish src ctx0.MapSources

        let collected = collectTypes ctx0.Capabilities ctx0.ExportTopLevel decls

        let lowered = TastLower.lower decls

        // The top-level module functions and their flat compiled form, from the same analysis
        // the CLR backend reads. Drives both FLAT emission and saturated-call collapsing.
        let compiledFns =
            System.Collections.Generic.Dictionary<BoundVarId, CompiledFns.CompiledFn>()

        for f in CompiledFns.gather lowered do
            compiledFns.[f.Key] <- f

        // The file's locally-declared interface keys, which drive the attached-method
        // dispatch of a `(r :> ILocal).M()` / `.Prop` access.
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
                Records = LocalThenExternal.withLocal collected.Records ctx0.Records
                Unions = LocalThenExternal.withLocal collected.Unions ctx0.Unions
                Classes = collected.Classes
                Enums = collected.Enums
                CompiledFns = compiledFns
                LocalInterfaces = localInterfaces
            }

        // Class decls (with their attached instance methods) are built now, because their method
        // bodies need the full ctx, unlike record/union decls, which carry no bodies.
        let classDecls =
            [
                for pc in collected.PendingClasses ->
                    let preamble =
                        match pc.Preamble with
                        | ValueSome p -> emitInstancePreamble ctx p
                        | ValueNone -> []

                    let ctor =
                        match pc.Ctor with
                        | EmitJsTypes.PendingCtor.Positional fields -> JsCtor.positional fields preamble
                        // A class with no primary ctor can carry no instance preamble.
                        | EmitJsTypes.PendingCtor.Explicit sc -> emitExplicitCtor ctx sc

                    JsStatement.Class(
                        pc.Name,
                        ctor,
                        EmitJsMembers.emitClassMethods buildExpr ctx pc.Members,
                        ctx.ExportTopLevel
                    )
            ]

        // Unions whose interface impls became base-class methods need the full ctx too. They
        // render base-first, so the case subclasses inherit the protocol members.
        let pendingUnionDecls =
            [
                for pu in collected.PendingUnions ->
                    JsStatement.Union(
                        pu.Name,
                        pu.Brand,
                        pu.Cases,
                        EmitJsMembers.emitClassMethods buildExpr ctx pu.Members,
                        ctx.ExportTopLevel
                    )
            ]

        // Member functions emit after the class decls and before the body: they reference the
        // classes via `new`/match, and `const` arrows are not hoisted.
        let memberDecls =
            [
                for (typeName, m) in collected.Members -> EmitJsMembers.emitMemberFn buildExpr ctx typeName m
            ]

        // Static preambles run at module load AFTER every class + member const-arrow is defined
        // (a `static let` may call a static member) and BEFORE the body reads a static field.
        let staticPreambleStmts =
            [
                for pc in collected.PendingClasses do
                    yield! emitStaticPreamble ctx pc.Name pc.StaticPreamble
            ]

        // A module bound variable mutated by a later module-level `Assignment` must emit as
        // `let`/`export let`, not `const`.
        let reassignedAtTop (k: BoundVarId) =
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

                        match dl.Pattern with
                        | TastAccessor.PNamed k ->
                            let value = dl.Value
                            // A module FUNCTION emits FLAT; a plain value stays curried.
                            let init =
                                match ctx.CompiledFns.TryGetValue k with
                                | true, cf -> emitFlatModuleFn ctx k cf (locOf ctx value)
                                | _ -> emitBound ctx k value

                            topLevelBinding ctx (reassignedAtTop k) (boundVarNameOf ctx.Pool k) init
                        | _ -> failwithf "EmitJs: unsupported declaration %A" decl
                    | DeclShape.Type -> failwithf "EmitJs: unsupported declaration %A" decl
            ]

        // Imports lead the program; local class decls follow, classes not being hoisted and
        // so needing to precede every `new`/match site.
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
