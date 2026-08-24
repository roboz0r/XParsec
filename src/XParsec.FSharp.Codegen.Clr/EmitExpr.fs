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

    let rec buildExpr (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        match TastAccessor.exprKind e with
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
            | TConstValue.Unit -> EmitTypes.buildUnitValue env b

        | ExprShape.Null -> b.Add ILInstr.Ldnull

        | ExprShape.Var ->
            let boundVar = TastAccessor.exprVarBoundVar e

            if env.StaticMethods.ContainsKey boundVar then
                // A generic module value (`let empty : SetTree<'T> = …`) lowers to a 0-arg
                // generic static method, since a non-generic module class cannot host a
                // `SetTree<'T>` field. Its instantiation comes from this use's own type.
                let varTy = TastAccessor.exprTy e
                let sm = env.StaticMethods.[boundVar]

                let callHandle =
                    if sm.Typars = 0 then
                        sm.Handle
                    else
                        let inst = matchInstantiation sm.Typars [ sm.ResultTy ] [ varTy ]
                        env.Provider.StaticFnMethodSpec(sm.Handle, inst)

                b.Add(ILInstr.Call(callHandle, 0, 1))
            else
                buildVarLoad env b boundVar

        | ExprShape.Let -> EmitBindings.buildLet buildExpr env b e
        | ExprShape.Use -> EmitBindings.buildUse buildExpr env b e
        | ExprShape.TryFinally -> EmitBindings.buildTryFinally buildExpr env b e

        | ExprShape.ForIn -> EmitLoops.buildForIn buildExpr env b e
        | ExprShape.ForTo -> EmitLoops.buildForTo buildExpr env b e
        | ExprShape.While -> EmitLoops.buildWhile buildExpr env b e

        | ExprShape.Sequential -> EmitMatch.buildSequential buildExpr env b e
        | ExprShape.IfThenElse -> EmitMatch.buildIfThenElse buildExpr env b e
        | ExprShape.Match -> EmitMatch.buildMatch buildExpr env b e

        | ExprShape.Lambda -> EmitConstruct.buildLambda env b e
        | ExprShape.New -> EmitConstruct.buildNew buildExpr env b e
        | ExprShape.RecordCons -> EmitConstruct.buildRecordCons buildExpr env b e
        | ExprShape.RecordClone -> EmitConstruct.buildRecordClone buildExpr env b e
        | ExprShape.UnionCons -> EmitConstruct.buildUnionCons buildExpr env b e
        | ExprShape.Tuple -> EmitConstruct.buildTuple buildExpr env b e
        | ExprShape.ArrayLit -> EmitConstruct.buildArrayLit buildExpr env b e

        | ExprShape.App -> EmitCall.buildAppCall buildExpr env b e

        // `Set.empty` compiles to `SetModule.Empty<'T>()`, so a bare external value takes the
        // same applied-function dispatch as an application, with no arguments.
        | ExprShape.External -> EmitCall.buildAppCall buildExpr env b e

        | ExprShape.FieldGet -> EmitMember.buildFieldGet buildExpr env b e
        | ExprShape.Assignment -> EmitMember.buildAssignment buildExpr env b e
        | ExprShape.FieldSet -> EmitMember.buildFieldSet buildExpr env b e
        | ExprShape.PropertyGet -> EmitMember.buildPropertyGet buildExpr env b e
        | ExprShape.MethodCall -> EmitMember.buildMethodCall buildExpr env b e
        | ExprShape.StaticPropertyGet -> EmitMember.buildStaticPropertyGet env b e
        | ExprShape.StaticFieldGet -> EmitMember.buildStaticFieldGet env b e
        | ExprShape.StaticFieldSet -> EmitMember.buildStaticFieldSet buildExpr env b e
        | ExprShape.StaticMethodCall -> EmitMember.buildStaticMethodCall buildExpr env b e
        | ExprShape.ExternalMember -> EmitMember.buildExternalMember buildExpr env b e

        | ExprShape.Format ->
            let view = TastAccessor.exprFormat e
            EmitFormat.buildFormat buildExpr env b view.Sink view.Segments

        | ExprShape.ILIntrinsic -> EmitIntrinsic.buildILIntrinsic buildExpr env b e
        | ExprShape.StaticOptimization -> EmitIntrinsic.buildStaticOptimization buildExpr env b e
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

    /// Emit an expression as a statement: evaluate it and discard any value.
    let buildStatement (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        buildExpr env b e

        while b.Depth > 0 do
            b.Add ILInstr.Pop
