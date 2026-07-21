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

/// The expression-emission dispatcher. `buildExpr` is a thin router: trivial
/// leaf arms (`Const` / `Null` / `Var`) stay inline; every structured case
/// delegates to a per-concern `Emit*` module, passing `buildExpr` itself as the
/// `Recur` back-edge (the one seam that crosses a file boundary — see
/// `EmitDispatch`). The router matches each node's `ExprShape` *exhaustively* — the
/// shapes the CLR backend does not yet emit (`TryWith`/`Range`/`TraitCall`)
/// are explicit `failwith` arms, not a catch-all — so adding an `ExprShape` case breaks
/// the build here and forces a routing decision.
module EmitExpr =

    let rec buildExpr (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match TastAccessor.exprKind e with
        | ExprShape.Const ->
            match TastAccessor.exprConstValue e with
            | TConstValue.String s -> b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
            // The load follows from the width alone (`EmitTypes.pushIntConst` — shared with
            // the `Const` pattern and the enum-case load).
            | TConstValue.Integral(w, bits) -> EmitTypes.pushIntConst b w bits
            | TConstValue.Bool v -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
            | TConstValue.Float x -> b.Add(ILInstr.LdcR8 x)
            | TConstValue.Float32 x -> b.Add(ILInstr.LdcR4 x)
            | TConstValue.Char c -> b.Add(ILInstr.LdcI4(int c))
            | TConstValue.Decimal d ->
                // Materialise via `Decimal..ctor(lo, mid, hi, isNegative, scale)` from
                // the value's bit representation — the same shape F#/Roslyn emit.
                let bits = System.Decimal.GetBits d
                let flags = bits.[3]
                b.Add(ILInstr.LdcI4 bits.[0]) // lo
                b.Add(ILInstr.LdcI4 bits.[1]) // mid
                b.Add(ILInstr.LdcI4 bits.[2]) // hi
                b.Add(ILInstr.LdcI4(if flags < 0 then 1 else 0)) // sign (high bit of flags)
                b.Add(ILInstr.LdcI4((flags >>> 16) &&& 0xFF)) // scale
                b.Add(ILInstr.Newobj(env.Provider.DecimalCtor, 5))
            | TConstValue.Unit ->
                // `()` literal — reify the `unit` value (a zero-field `System.ValueTuple`
                // struct, not FSharp.Core's null `Unit`). Pushed when a closure
                // invocation needs a unit arg (`c ()`) or a unit value is otherwise
                // reified.
                EmitTypes.buildUnitValue env b

        | ExprShape.Null -> b.Add ILInstr.Ldnull

        | ExprShape.Var ->
            let binding = TastAccessor.exprVarBinding e

            if env.StaticMethods.ContainsKey binding then
                // A generic module value (`let empty : SetTree<'T> = …` at module scope)
                // lowers to a zero-arg generic static method on its holder (a non-generic
                // module holder cannot host a `SetTree<'T>` field). A module value is
                // never applied, so — unlike a static function, which `collectStaticFns`
                // proves is always saturated and therefore only ever reaches codegen as an
                // `App` head — it appears here as a bare `Var`. (Hence: a bare `Var`
                // whose key is a static method is always one of these 0-arg value
                // methods.) Emit a 0-arg `call` to its `MethodSpec`, the instantiation
                // recovered by matching the method's declared result template against this
                // reference's own type.
                let varTy = TastAccessor.exprTy e
                let sm = env.StaticMethods.[binding]

                let callHandle =
                    if sm.Typars = 0 then
                        sm.Handle
                    else
                        let inst = matchInstantiation sm.Typars [ sm.ResultTy ] [ varTy ]
                        env.Provider.StaticFnMethodSpec(sm.Handle, inst)

                b.Add(ILInstr.Call(callHandle, 0, 1))
            else
                buildVarLoad env b binding

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

        | ExprShape.App -> EmitCall.buildAppCall buildExpr env b e

        // A bare external value with no application — a zero-arg module value such
        // as `Set.empty` (the `[<GeneralizableValue>]` generic value compiled to a
        // generic static method `SetModule.Empty<'T>()`). Route it through the same
        // head dispatch as an application with an empty spine: `buildAppCall`
        // collects a zero-length spine, `TryEmitCall` emits the 0-arg recipe, and
        // the generic instantiation is read from the value's (result) type.
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

        // Not yet emitted by the CLR backend. These shapes still occur in the frozen
        // tree (closure discovery walks `TryWith` bodies, for one), so they reach the
        // emitter rather than being lowered away. An explicit arm each keeps the dispatch
        // exhaustive over `ExprShape`: a newly added shape breaks the build here and
        // forces a routing decision instead of silently falling through.
        | ExprShape.TryWith
        | ExprShape.Range
        | ExprShape.TraitCall -> failwithf "Emit: unsupported expression: %A" e

    /// Emit an expression as a statement: evaluate it and discard any value.
    let buildStatement (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        buildExpr env b e

        while b.Depth > 0 do
            b.Add ILInstr.Pop
