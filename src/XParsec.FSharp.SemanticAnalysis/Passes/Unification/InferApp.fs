namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open Vesper
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch
open UnificationInferLiteralExpr
open UnificationInferExternalCall
open UnificationInferCtor
open UnificationInferIdentExpr

module internal UnificationInferApp =

    /// Report an unresolved operator in F#'s FS0043 shape against the operand types,
    /// spelled as the user WROTE it (`<`, `>=>`) rather than as the compiled name.
    let private unresolvedOperator
        (ctx: PassContext)
        (tok: SyntaxToken)
        (supportTys: SemType list)
        (name: string)
        : SemType =
        let spelling =
            match OperatorData.sourceSpelling name with
            | ValueSome symbol -> symbol
            // A virtual token has no source text, leaving the compiled name.
            | ValueNone when tok.Token.IsVirtual -> name
            // A composed operator (`>=>`): the token carries its written spelling.
            | ValueNone -> ctx.NameOf tok

        let shownTys =
            supportTys
            |> List.map (shown ctx.Store)
            |> List.distinct
            |> List.toArray
            |> Block.ofArray

        errorTy ctx tok (Kind.TraitNotSupported(shownTys, MemberNoun.Operator, spelling))

    /// Unify an operator's scheme against its operand types and a fresh result type.
    let private applyOperatorScheme
        (ctx: PassContext)
        (tok: SyntaxToken)
        (scheme: SemType)
        (argTys: SemType list)
        : SemType =
        let resultTy = TyVar(ctx.FreshTyVar())
        unify ctx tok scheme (List.foldBack (fun argTy acc -> TyFun(argTy, acc)) argTys resultTy)
        resultTy

    /// Type an operator application once the special forms (`::`, `&`) are peeled. A `let`
    /// binding stamped at the node shadows both measured arithmetic and the provider symbol.
    /// A provider hit stamps `IntrinsicKey`, which splices the contract's `let inline` body
    /// over the erased carrier; measured arithmetic types the node itself and keeps the stamp.
    let private inferOperatorApp
        (ctx: PassContext)
        (node: NodeSite)
        (name: string)
        (tryMeasured: unit -> SemType option)
        (argTys: SemType list)
        : SemType =
        match ctx.Bindings.Binding.TryGetValue node.Key with
        | ValueSome rb -> applyOperatorScheme ctx node.Tok (instantiateBinding ctx rb) argTys
        | ValueNone ->
            let stamped = ctx.Resolution.ExternalSymbolStamp.TryGetValue node.Key

            match stamped with
            | ValueSome sym -> ctx.Resolution.IntrinsicKey.Set(node.Key, sym.Key)
            | ValueNone -> ()

            match tryMeasured (), stamped with
            | Some resultTy, _ -> resultTy
            | None, ValueSome sym ->
                applyOperatorScheme ctx node.Tok (ExternalSymbols.instantiateSymbol ctx sym ctx.CurrentLevel) argTys
            | None, ValueNone -> unresolvedOperator ctx node.Tok argTys name

    /// Key each SOURCE lambda argument landing on a parameter constrained `:> Fun<a,b>` to that
    /// flat arity, which is what makes codegen emit a value-struct closure for it.
    let private recordFunArityVerdicts (ctx: PassContext) (args: ImmutableArray<Expr<SyntaxToken>>) (fnTy: SemType) =
        // Each verdict lambda paired with the typar `dom` it landed on. The result-typar
        // position is only readable from the result type, so a second pass fills it.
        let lambdaSlots = ResizeArray<LambdaKey * SemType>()

        // `mapSeq (fun x -> x) src` walks `'TF -> Seq<'a> -> MapSeq<'a,'TF,'b>` to `MapSeq<…>`.
        // `ValueNone` once a step is not a `TyFun`: the `:> Fun<a,b>` constraint sits on a declared
        // parameter, and the chain's domains ARE the declared parameters.
        let rec walkFunChain i currTy =
            if i >= args.Length then
                ValueSome currTy
            else
                match resolveStep ctx.Store currTy with
                | TyFun(dom, cod) ->
                    // Peel the wrappers of a parenthesised argument lambda (`apply2 (fun … )`). The
                    // frozen `Lambda` anchors on its FIRST parameter pattern's token, so key on that.
                    let rec peelLambda e =
                        match e with
                        | Expr.EnclosedBlock(expr = inner)
                        | Expr.TypeAnnotation(expr = inner) -> peelLambda inner
                        | Expr.Fun(argumentPats = argPats) when argPats.Length > 0 -> ValueSome argPats.[0]
                        | _ -> ValueNone

                    match peelLambda args.[i] with
                    | ValueSome arg0Pat ->
                        match funSlotArityOf ctx.Store dom with
                        | ValueSome arity ->
                            let lamKey = LambdaKey(Anchor.ofToken (CstKeys.firstTokenOfPat arg0Pat))

                            ctx.FunVerdicts.Set(
                                lamKey,
                                {
                                    Arity = arity
                                    ResultTyparPos = ValueNone
                                }
                            )

                            lambdaSlots.Add(lamKey, dom)
                        | ValueNone -> ()
                    | ValueNone -> ()

                    walkFunChain (i + 1) cod
                | _ -> ValueNone

        // A transformer combinator's result nominal (`MapSeq<…,'TF,…>`) carries the lambda's
        // typar at some arg index. Match by typar IDENTITY, not shape, because two
        // function-valued args would otherwise conflate.
        match walkFunChain 0 fnTy with
        | ValueSome resultTy when lambdaSlots.Count > 0 ->
            match resolveStep ctx.Store resultTy with
            | TyConst(_, resArgs)
            | TyRecord(_, resArgs)
            | TyUnion(_, resArgs)
            | TyClass(_, resArgs)
            | TyTuple resArgs ->
                // Typar identity is the union-find ROOT: two vars linked to each other share
                // one root while the unresolved `SemType`s they sit in still differ.
                let rootOf (t: SemType) : TyVarId voption =
                    match resolveStep ctx.Store t with
                    | TyVar tv -> ValueSome((UnionFind.find ctx.Store tv).Id)
                    | _ -> ValueNone

                for (lambdaKey, dom) in lambdaSlots do
                    match rootOf dom with
                    | ValueSome domRoot ->
                        let mutable found = ValueNone

                        for i in 0 .. resArgs.Length - 1 do
                            if ValueOption.isNone found then
                                match rootOf resArgs.[i] with
                                | ValueSome r when r = domRoot -> found <- ValueSome i
                                | _ -> ()

                        match found with
                        | ValueSome idx ->
                            // Upgrade the arity-only verdict with the result-typar position.
                            match ctx.FunVerdicts.TryGetValue lambdaKey with
                            | ValueSome v ->
                                ctx.FunVerdicts.Set(
                                    lambdaKey,
                                    { v with
                                        ResultTyparPos = ValueSome idx
                                    }
                                )
                            | ValueNone -> ()
                        | ValueNone -> ()
                    | ValueNone -> ()
            | _ -> ()
        | _ -> ()

    /// A syntactic string CONSTANT admits into a literal / literal-union parameter by set
    /// membership; a plain `string`-typed expression does not. `true` = handled (admitted as
    /// is, or reported as outside the set); `false` falls through to the ordinary `unifyArg`.
    let private tryAdmitLiteralConstArg
        (ctx: PassContext)
        (tok: SyntaxToken)
        (argExpr: Expr<SyntaxToken>)
        (dom: SemType)
        : bool =
        // Syntactic check FIRST: the peel is cheap, while the slot check ground-folds the
        // parameter type. Result-identical either way; this confines the fold to constants.
        match constStringArg ctx argExpr with
        | ValueNone -> false
        | ValueSome s ->
            let lit = LiteralConst.String s

            // Ground-fold first: a `keyof T` parameter (`TyKeyOf`) folds to its literal-name
            // union, so a string constant admits by the same rule as an explicit literal
            // union (a bare `TyLiteral` slot is a singleton set).
            match tryLiteralDisjuncts ctx.Store (evalTypeLevel ctx (resolveStep ctx.Store dom)) with
            | ValueNone -> false
            | ValueSome disjuncts ->
                if List.contains lit disjuncts then
                    true
                else
                    let allowed = disjuncts |> List.map (fun v -> v.Render) |> String.concat " | "

                    ctx.Report(
                        tok,
                        Kind.Message(sprintf "%s is not one of the allowed literal values: %s" lit.Render allowed)
                    )

                    true

    /// For a printf entry point with a plain-literal format argument, the format spec drives
    /// the call's curried result type, not the literal's apparent `string` type. The
    /// format argument itself types as `PrintfFormat<printer, …>`.
    let tryInferPrintfApp
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType voption =
        let fnKey = CstKeys.ofExpr fn

        // A local binding shadowing a printf name is an ordinary function, so the special
        // rule does not apply.
        if ctx.Bindings.Binding.ContainsKey fnKey then
            ValueNone
        else
            match fn with
            | Expr.Ident _
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
                match PrintfSpec.tryFamily (qualifiedNameOf ctx fn) with
                | ValueNone -> ValueNone
                | ValueSome fam ->
                    // Resolve the family's sink slot (`fprintf`'s `TextWriter`) the SAME way a
                    // real sink argument resolves, so both carry one `TypeKey`. An unsurfaced
                    // name keeps its by-name `TyConst`.
                    let fam =
                        fam
                        |> PrintfSpec.resolveExternalSlots (fun id ->
                            // The sink slot ids (`System.IO.TextWriter`, …) are FIXED and fully
                            // qualified, so the type resolves by KEY, not an opens-aware lookup.
                            match ctx.Provider.TryLookupType(SymbolKeyOps.qualifiedTypeKeyOf id.Value 0) with
                            | ValueSome(ExternalTypeShape.Class info) when info.TyparArity = 0 ->
                                ValueSome(TyClass(SymbolKeyOps.qualifiedTypeKeyOf id.Value 0, Block.empty))
                            | _ -> ValueNone
                        )

                    let idx = fam.FormatArgIndex

                    if args.Length <= idx then
                        // Format argument not supplied (e.g. partially-applied
                        // `fprintf writer`); defer to standard inference.
                        ValueNone
                    else
                        // The format position may hold an `Ident` bound to a literal (`let fmt =
                        // "%d" in sprintf fmt …`); recover it and drive the literal path.
                        let recoveredFormat = ctx.TryRecoverFormatLiteral args.[idx]
                        let formatArg = ValueOption.defaultValue args.[idx] recoveredFormat
                        let formatRecovered = recoveredFormat.IsSome

                        match formatSpecifiers ctx formatArg with
                        | ValueNone -> ValueNone
                        | ValueSome specs ->
                            let applied = PrintfSpec.appliedTypeOf (freshHoleTy ctx node.Key) specs fam
                            let fnTy = applied.FnTy
                            let fmtTy = applied.FormatTy

                            // Stamp the function node so Elaborate threads the
                            // curried result type through the App chain.
                            ctx.Store.SetLink(UnionFind.find ctx.Store (freshTv ctx fnKey), ValueSome fnTy)

                            let mutable currTy = fnTy

                            for i in 0 .. args.Length - 1 do
                                let a = args.[i]

                                let argTy =
                                    if i = idx then
                                        // The format literal types as the PrintfFormat, not `string`.
                                        ctx.Store.SetLink(
                                            UnionFind.find ctx.Store (freshTv ctx (CstKeys.ofExpr a)),
                                            ValueSome fmtTy
                                        )

                                        fmtTy
                                    else
                                        infer ctx a

                                match resolveStep ctx.Store currTy with
                                | TyFun(dom, cod) ->
                                    // Uniform over every arg, leading writer included: that
                                    // slot is a resolved `TyClass` a real writer unifies with.
                                    unify ctx node.Tok argTy dom
                                    currTy <- cod
                                | _ ->
                                    let resultTy = TyVar(ctx.FreshTyVar())
                                    unify ctx node.Tok currTy (TyFun(argTy, resultTy))
                                    currTy <- resultTy

                            // A `%a`/`%t` callback hole lowers only where THIS target's
                            // provider surfaces the family's sink type; there is no cold
                            // fallback, so an unavailable sink is diagnosed here.
                            let hasCallbackHole =
                                specs |> List.exists (fun p -> PrintfSpec.isCallbackHole p.Type)

                            let rejectCallback = hasCallbackHole && not (PrintfSpec.callbackSinkAvailable fam)

                            if rejectCallback then
                                ctx.Report(
                                    node.Tok,
                                    Kind.Message
                                        "printf %a/%t requires a sink type (System.IO.TextWriter / System.Text.StringBuilder) not available on this target"
                                )

                            // The first specifier that does not lower: a rejected flag
                            // combination is an error, a cold residual (a runtime-width
                            // zero-pad such as `%0*d`) keeps the generic printf shape.
                            let firstNotLowerable =
                                specs
                                |> List.tryPick (fun p ->
                                    match PrintfHoleForm.classify p with
                                    | PrintfHoleForm.HoleVerdict.Lowerable _ -> None
                                    | verdict -> Some(struct (p, verdict))
                                )

                            match firstNotLowerable with
                            | Some(struct (p, PrintfHoleForm.HoleVerdict.SignLeftAlignZeroPad)) ->
                                ctx.Report(
                                    node.Tok,
                                    Kind.Message(
                                        sprintf
                                            "printf format specifier %s combines a sign flag ('+'/' ') with both '-' and '0'; remove one of the flags"
                                            (PrintfHoleForm.renderPlaceholder p)
                                    )
                                )
                            | Some(struct (p, PrintfHoleForm.HoleVerdict.OversizedDimension)) ->
                                ctx.Report(
                                    node.Tok,
                                    Kind.Message(
                                        sprintf
                                            "printf format specifier %s has a width or precision outside the range %d..%d"
                                            (PrintfHoleForm.renderPlaceholder p)
                                            System.Int32.MinValue
                                            System.Int32.MaxValue
                                    )
                                )
                            | Some(struct (p, _)) ->
                                ctx.Report(
                                    node.Tok,
                                    Kind.Message(
                                        sprintf
                                            "printf format specifier %s cannot be lowered on this target"
                                            (PrintfHoleForm.renderPlaceholder p)
                                    )
                                )
                            | None -> ()

                            match PrintfSpec.sinkOf (qualifiedNameOf ctx fn) with
                            | ValueSome sink when
                                not rejectCallback
                                && args.Length = PrintfSpec.totalArity specs + idx + 1
                                && lowerablePlaceholders specs
                                && (idx = 0
                                    || (idx = 1
                                        && (
                                            match sink with
                                            | PrintfSpec.PrintfSink.Writer _
                                            | PrintfSpec.PrintfSink.Builder -> true
                                            | _ -> false
                                        )))
                                ->
                                // Keyed on the FAMILY, not the sink kind: `printf` prints to
                                // `StdOut` and still needs a capture-first scratch.
                                let scratch =
                                    if hasCallbackHole && PrintfSpec.familyNeedsScratch fam then
                                        match fam.ScratchSink with
                                        | TyClass(scratchKey, _) as scratchTy ->
                                            let scratchName = SymbolKeyOps.typeMetaName scratchKey

                                            // `ToString` is overloaded (`StringBuilder.ToString(int,
                                            // int)`); pick the parameterless override.
                                            let toString =
                                                ctx.Provider.TryLookupMembers(scratchKey, "ToString")
                                                |> Block.tryFind (fun m -> m.Key.ArgSig.Length = 0)

                                            match toString with
                                            | ValueSome m ->
                                                ValueSome
                                                    {
                                                        PrintfSpec.CallbackScratch.ScratchClassName = scratchName
                                                        PrintfSpec.CallbackScratch.ScratchTy = scratchTy
                                                        PrintfSpec.CallbackScratch.ToStringKey = SymbolKey.Member m.Key
                                                    }
                                            | ValueNone ->
                                                failwithf
                                                    "InferApp: writer/builder %%a/%%t scratch sink %s resolved to a class with no parameterless ToString, so capture-first cannot be lowered"
                                                    scratchName
                                        | other ->
                                            failwithf
                                                "InferApp: writer/builder %%a/%%t scratch sink is unresolved (%A) though callbackSinkAvailable passed the State gate, so resolveExternalSlots and the gate disagree"
                                                other
                                    else
                                        ValueNone

                                ctx.PrintfLowering.Set(node.Key, PrintfSpec.PrintfLowering.Full(sink, scratch))
                            // Partial-application marker: a fully-unapplied literal partial
                            // (`printfn "%d"`), 1..K holes, no `%A`/`%O` (an unapplied one is
                            // an unpinned typar). Elaborate synthesises a heap closure.
                            | ValueSome sink when
                                idx = 0
                                && not formatRecovered
                                && args.Length = idx + 1
                                && specs.Length >= 1
                                && lowerablePlaceholders specs
                                && specs |> List.forall PrintfSpec.isUnaryConcreteHole
                                ->
                                ctx.PrintfLowering.Set(node.Key, PrintfSpec.PrintfLowering.Partial sink)
                            | _ -> ()

                            ValueSome currTy
            | _ -> ValueNone

    let inferApp
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        // The .NET-style probes below are TUPLED: they apply to one argument only.
        let single probe =
            if args.Length = 1 then probe args.[0] else ValueNone

        // The generic curried-application fallback. The function's type is threaded in rather than
        // inferred here, so the optional-argument fill and this loop share one inference of it.
        let inferGenericAppFrom (fnTy: SemType) (argTys: SemType[]) =
            let mutable currTy = fnTy

            for i in 0 .. argTys.Length - 1 do
                let argTy = argTys.[i]

                match resolveStep ctx.Store currTy with
                | TyFun(dom, cod) ->
                    // A literal-union parameter consults the argument EXPRESSION for a syntactic
                    // constant; skip `unifyArg` when that handles the slot, as it rejects a
                    // `string`. It otherwise accepts a ground subtype upcast.
                    if not (tryAdmitLiteralConstArg ctx node.Tok args.[i] dom) then
                        unifyArg ctx node.Tok argTy dom

                    currTy <- cod
                | _ ->
                    let resultTy = TyVar(ctx.FreshTyVar())
                    unify ctx node.Tok currTy (TyFun(argTy, resultTy))
                    currTy <- resultTy

            currTy

        // Specialised resolution probes; the first `ValueSome` wins. An external ctor must be
        // recognised before the generic fallback types its class name as a function and leaks
        // an unpinned, dangling result TyVar.
        tryInferPrintfApp infer ctx node fn args
        |> ValueOption.orElseWith (fun () -> single (tryInferExternalStaticMethodCall infer ctx node.Tok fn))
        |> ValueOption.orElseWith (fun () -> tryInferExternalCtorApp infer ctx node fn args)
        |> ValueOption.orElseWith (fun () -> single (tryInferExternalGenericCtorApp infer ctx node fn))
        |> ValueOption.orElseWith (fun () -> single (tryInferLocalCtorApp infer ctx node fn))
        |> ValueOption.orElseWith (fun () -> single (tryInferExternalInstanceMethodCall infer ctx node.Tok fn))
        |> ValueOption.orElseWith (fun () -> single (tryInferLocalInstanceMethodCall infer ctx node fn))
        |> ValueOption.defaultWith (fun () ->
            // The optional-argument fill may inspect the arguments and then decline, and the
            // curried loop needs them too, so infer each exactly once, here.
            let fnTy = infer ctx fn
            let argTys = [| for a in args -> infer ctx a |]

            // BEFORE the curried loop below links each domain to its function type, which
            // erases the `:> Fun` constraint the verdict is read from.
            recordFunArityVerdicts ctx args fnTy

            tryFillOptionalCall ctx node.Tok fn args argTys
            |> ValueOption.defaultWith (fun () -> inferGenericAppFrom fnTy argTys)
        )

    let inferHighPrecApp
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        : SemType =
        // `f(x)` — the same call as `Expr.App fn [|arg|]`, only a separate CST case (the parser
        // splits on the space before `(`), so it must get the SAME probe chain.
        inferApp infer ctx node fn (ImmutableArray.Create arg)

    let inferRange
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (fromE: Expr<SyntaxToken>)
        (stepE: Expr<SyntaxToken> voption)
        (toE: Expr<SyntaxToken>)
        : SemType =
        // Endpoints and step are constrained to int; the result type is NOT modelled. A range
        // materialises no seq value, so it is legal ONLY as a `for … in` source. Reaching
        // here is a range in VALUE position, rejected later where position is known.
        let fromTy = infer ctx fromE
        unify ctx tok fromTy ctx.Intrinsics.Int

        match stepE with
        | ValueSome s ->
            let stepTy = infer ctx s
            unify ctx tok stepTy ctx.Intrinsics.Int
        | ValueNone -> ()

        let toTy = infer ctx toE
        unify ctx tok toTy ctx.Intrinsics.Int
        TyUnknown UnknownReason.NoValueType

    let inferInfix
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        let leftTy = infer ctx left
        let rightTy = infer ctx right

        // `node.Tok` is the operator token: `CstKeys` keys an `InfixApp` on it.
        match node.Tok.Token with
        | Token.KWColonColon ->
            // `h :: t` builds the list union directly (not a provider operator): `h` is the
            // element type, `t` unifies to the same list type, which is the result.
            let listTy = listLiteralTy ctx node.Tok leftTy
            unify ctx node.Tok rightTy listTy
            listTy
        | _ ->

            match OperatorNames.ofSymbolic (ctx.NameOf node.Tok) node.Tok with
            | ValueSome name ->
                inferOperatorApp
                    ctx
                    node
                    name
                    (fun () -> tryMeasuredArith ctx node.Tok name leftTy rightTy)
                    [ leftTy; rightTy ]
            | ValueNone ->
                // Not a compiled-named operator, so leave the result free.
                TyVar(ctx.FreshTyVar())

    /// `x?name` — the dynamic-access operator (F# spec 6.4.5: `x ? ident` desugars to `(?)
    /// x "ident"`), unified against `x -> string -> ^TResult`. Its `target: dynamic`
    /// parameter rejects a static object argument; `default ^TResult : dynamic` keeps a chain dynamic.
    let inferDynamicLookup (infer: Infer) (ctx: PassContext) (node: NodeSite) (objArg: Expr<SyntaxToken>) : SemType =
        let objArgTy = infer ctx objArg

        match ctx.Resolution.ExternalSymbolStamp.TryGetValue node.Key with
        | ValueSome sym ->
            // Thread the resolved `op_Dynamic` identity to the `External` node minted at this
            // same key, so the `$0[$1]` body splices by KEY.
            ctx.Resolution.IntrinsicKey.Set(node.Key, sym.Key)
            let resultVar = ctx.FreshTyVar()
            let resultTy = TyVar resultVar

            unify
                ctx
                node.Tok
                (ExternalSymbols.instantiateSymbol ctx sym ctx.CurrentLevel)
                (TyFun(objArgTy, TyFun(ctx.Intrinsics.String, resultTy)))

            // Record for the post-settle escape sweep: a context that pins `resultVar` to a
            // concrete non-`dynamic` type is an unchecked assertion, warned on unless ascribed.
            ctx.DynamicEscapes.Add { Root = resultVar; Node = node }

            resultTy
        | ValueNone -> errorTy ctx node.Tok (Kind.IntrinsicNotInScope Intrinsic.DynamicGet)

    /// `x?name <- value` — the dynamic-set operator (`(?<-) x "name" value`), unified
    /// against `x -> string -> value -> unit`.
    let inferDynamicSet
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (objArg: Expr<SyntaxToken>)
        (value: Expr<SyntaxToken>)
        : SemType =
        let objArgTy = infer ctx objArg
        let valueTy = infer ctx value

        match ctx.Resolution.ExternalSymbolStamp.TryGetValue node.Key with
        | ValueSome sym ->
            // Thread the resolved `op_DynamicAssignment` identity to the `External` node
            // minted at the enclosing assignment, so the `$0[$1] = $2` body splices by KEY.
            ctx.Resolution.IntrinsicKey.Set(node.Key, sym.Key)

            unify
                ctx
                node.Tok
                (ExternalSymbols.instantiateSymbol ctx sym ctx.CurrentLevel)
                (TyFun(objArgTy, TyFun(ctx.Intrinsics.String, TyFun(valueTy, ctx.Intrinsics.Unit))))

            ctx.Intrinsics.Unit
        | ValueNone -> errorTy ctx node.Tok (Kind.IntrinsicNotInScope Intrinsic.DynamicSet)

    let inferPrefix (infer: Infer) (ctx: PassContext) (node: NodeSite) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        // `node.Tok` is the operator token: `CstKeys` keys a `PrefixApp` on it.
        match OperatorNames.ofPrefix (ctx.NameOf node.Tok) node.Tok with
        | ValueSome OperatorData.OpAddressOf ->
            // `&local` (managed address-of) is the byref intrinsic, because `op_AddressOf` has
            // no provider symbol. Typing it `byref<operandTy>` matches a BCL byref/`out`
            // parameter (`Int32.TryParse(string, int&)`); addressability is checked at codegen.
            TyConst(RuntimeNames.byrefKey, Block.singleton operandTy)
        | ValueSome name -> inferOperatorApp ctx node name (fun () -> None) [ operandTy ]
        | ValueNone -> TyVar(ctx.FreshTyVar())
