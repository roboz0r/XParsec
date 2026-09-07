namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve
open EmitPattern

module EmitExpr =

    /// Emit an expression at `pos`. An arm that does not take `pos` has its value discarded
    /// here.
    let rec buildExprAt (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let baseDepth = b.Depth

        match TastAccessor.exprKind e with
        | ExprShape.Unresolved -> failwith "Emit: an unresolved reference reached codegen"
        | ExprShape.Const ->
            match TastAccessor.exprConstValue e with
            | TConstValue.String s -> b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
            | TConstValue.Integral(k, bits) -> EmitTypes.pushIntConst b k bits
            | TConstValue.Bool v -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
            | TConstValue.Float x -> b.Add(ILInstr.LdcR8 x)
            | TConstValue.Float32 x -> b.Add(ILInstr.LdcR4 x)
            | TConstValue.Char c -> b.Add(ILInstr.LdcI4(int c))
            | TConstValue.Decimal d ->
                // `Decimal..ctor(lo, mid, hi, isNegative, scale)` from the value's bits:
                // `GetBits` packs sign and scale into element 3.
                let bits = System.Decimal.GetBits d
                let flags = bits.[3]
                b.Add(ILInstr.LdcI4 bits.[0]) // lo
                b.Add(ILInstr.LdcI4 bits.[1]) // mid
                b.Add(ILInstr.LdcI4 bits.[2]) // hi
                b.Add(ILInstr.LdcI4(if flags < 0 then 1 else 0)) // sign (high bit of flags)
                b.Add(ILInstr.LdcI4((flags >>> 16) &&& 0xFF)) // scale
                b.Add(ILInstr.Newobj(env.Provider.DecimalCtor, 5))
            | TConstValue.Unit -> ExprPos.reifyUnit env b pos

        | ExprShape.Null -> b.Add ILInstr.Ldnull

        | ExprShape.Var ->
            let boundVar = TastAccessor.exprVarBoundVar e

            if env.LiftedLocals.ContainsKey boundVar then
                // A bare reference to a lifted local: a parameterless one is called as a
                // generic module value is; a function-typed one was eta-bridged, so it is
                // never bare here.
                EmitCall.buildAppCall buildExpr pos env b e
            elif env.StaticMethods.ContainsKey boundVar then
                // A generic module value (`let empty : SetTree<'T> = …`) lowers to a 0-arg
                // generic static method, since a non-generic module class cannot host a
                // `SetTree<'T>` field. Its instantiation comes from this use's own type.
                let varTy = TastAccessor.exprTy e
                let sm = env.StaticMethods.[boundVar]

                let callHandle =
                    if sm.Scheme.TyparArity = 0 then
                        sm.Handle
                    else
                        let inst = matchInstantiation sm.Scheme.TyparArity [ sm.ResultTy ] [ varTy ]
                        env.Provider.StaticFnMethodSpec(sm.Handle, inst)

                b.Add(ILInstr.Call(callHandle, 0, 1))
            else
                buildVarLoad env b boundVar

        | ExprShape.Let -> EmitBindings.buildLet buildExprAt pos env b e
        | ExprShape.LetGroup -> EmitBindings.buildLetGroup buildExprAt pos env b e
        | ExprShape.Use -> EmitBindings.buildUse buildExprAt pos env b e
        | ExprShape.TryFinally -> EmitBindings.buildTryFinally buildExprAt pos env b e

        | ExprShape.ForIn -> EmitLoops.buildForIn buildExprAt pos env b e
        | ExprShape.ForTo -> EmitLoops.buildForTo buildExprAt pos env b e
        | ExprShape.While -> EmitLoops.buildWhile buildExprAt pos env b e

        | ExprShape.Sequential -> EmitMatch.buildSequential buildExprAt pos env b e
        | ExprShape.IfThenElse -> EmitMatch.buildIfThenElse buildExprAt pos env b e
        | ExprShape.Match -> EmitMatch.buildMatch buildExprAt pos env b e

        | ExprShape.Lambda -> EmitConstruct.buildLambda env b e
        | ExprShape.New -> EmitConstruct.buildNew buildExpr env b e
        | ExprShape.RecordCons -> EmitConstruct.buildRecordCons buildExpr env b e
        | ExprShape.RecordClone -> EmitConstruct.buildRecordClone buildExpr env b e
        | ExprShape.UnionCons -> EmitConstruct.buildUnionCons buildExpr env b e
        | ExprShape.Tuple -> EmitConstruct.buildTuple buildExpr env b e
        | ExprShape.ArrayLit -> EmitConstruct.buildArrayLit buildExpr env b e

        | ExprShape.App -> EmitCall.buildAppCall buildExpr pos env b e

        // `Set.empty` compiles to `SetModule.Empty<'T>()`, so a bare external value takes the
        // same applied-function dispatch as an application, with no arguments.
        | ExprShape.External -> EmitCall.buildAppCall buildExpr pos env b e

        | ExprShape.FieldGet -> EmitMember.buildFieldGet buildExpr env b e
        | ExprShape.Assignment -> EmitMember.buildAssignment buildExpr pos env b e
        | ExprShape.FieldSet -> EmitMember.buildFieldSet buildExpr pos env b e
        | ExprShape.PropertyGet -> EmitMember.buildPropertyGet buildExpr env b e
        | ExprShape.MethodCall -> EmitMember.buildMethodCall buildExpr pos env b e
        | ExprShape.StaticPropertyGet -> EmitMember.buildStaticPropertyGet env b e
        | ExprShape.StaticFieldGet -> EmitMember.buildStaticFieldGet env b e
        | ExprShape.StaticFieldSet -> EmitMember.buildStaticFieldSet buildExpr pos env b e
        | ExprShape.StaticMethodCall -> EmitMember.buildStaticMethodCall buildExpr pos env b e
        | ExprShape.ExternalMember -> EmitMember.buildExternalMember buildExpr env b e

        | ExprShape.Format ->
            let view = TastAccessor.exprFormat e
            EmitFormat.buildFormat buildExpr pos env b view.Sink view.Segments

        | ExprShape.ILIntrinsic -> EmitIntrinsic.buildILIntrinsic buildExpr pos env b e
        | ExprShape.StaticOptimization -> EmitIntrinsic.buildStaticOptimization buildExprAt pos env b e
        | ExprShape.Upcast -> EmitIntrinsic.buildUpcast buildExpr env b e
        | ExprShape.Downcast -> EmitIntrinsic.buildDowncast buildExpr env b e
        | ExprShape.TypeTest -> EmitIntrinsic.buildTypeTest buildExpr env b e

        // `TryWith` reaches Emit because no pass lowers it away, and closure discovery walks
        // its bodies.
        | ExprShape.TryWith -> failwithf "Emit: unsupported expression: %A" e

        // A surviving `Range` was reported at Elaborate as `RangeNotFirstClassValue`.
        | ExprShape.Range -> failwithf "Emit: unsupported expression: %A" e

        | ExprShape.InlineCall -> TastLower.inlineCallUnexpanded (TastAccessor.exprInlineCall e).Spec
        | ExprShape.CallerExpr -> TastLower.callerExprUnexpanded ()
        | ExprShape.TraitCall -> TastLower.traitCallUnresolved (TastAccessor.exprTraitCallMemberName e)

        ExprPos.discardTo b baseDepth pos

    /// Emit an expression whose value a consumer takes.
    and buildExpr (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit = buildExprAt ExprPos.Value env b e

    /// Emit an expression for effect, leaving the operand stack as it was.
    let buildStatement (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        buildExprAt ExprPos.Statement env b e
