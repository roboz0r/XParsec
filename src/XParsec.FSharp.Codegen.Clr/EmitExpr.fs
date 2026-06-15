namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve
open EmitPattern

/// The expression-emission dispatcher. `buildExpr` is a thin router: trivial
/// leaf arms (`Const` / `Null` / `Var`) stay inline; every structured case
/// delegates to a per-concern `Emit*` module, passing `buildExpr` itself as the
/// `Recur` back-edge (the one seam that crosses a file boundary — see
/// `EmitDispatch`). The match here is exhaustive over `TExprG`, so a new node
/// surfaces as a missing route rather than a silent fallthrough.
module EmitExpr =

    let rec buildExpr (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Const(TConstValue.String s, _, _) -> b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
        | TExprG.Const(TConstValue.Int n, _, _) -> b.Add(ILInstr.LdcI4 n)
        // `uint32` shares the 32-bit stack representation of `int32`; `ldc.i4`
        // pushes its two's-complement bit pattern (the value's signedness is a
        // type-level distinction the verifier reads off the slot, not the load).
        | TExprG.Const(TConstValue.UInt n, _, _) -> b.Add(ILInstr.LdcI4(int n))
        | TExprG.Const(TConstValue.Int64 n, _, _) -> b.Add(ILInstr.LdcI8 n)
        | TExprG.Const(TConstValue.Bool v, _, _) -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
        | TExprG.Const(TConstValue.Byte n, _, _) -> b.Add(ILInstr.LdcI4(int n))
        | TExprG.Const(TConstValue.Float x, _, _) -> b.Add(ILInstr.LdcR8 x)
        | TExprG.Const(TConstValue.Float32 x, _, _) -> b.Add(ILInstr.LdcR4 x)
        | TExprG.Const(TConstValue.Char c, _, _) -> b.Add(ILInstr.LdcI4(int c))
        | TExprG.Const(TConstValue.Decimal d, _, _) ->
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
        | TExprG.Const(TConstValue.Unit, _, _) ->
            // `()` literal — reify the `unit` value (a zero-field `System.ValueTuple`
            // struct, not FSharp.Core's null `Unit`). Pushed when a closure
            // invocation needs a unit arg (`c ()`) or a unit value is otherwise
            // reified — F3 (Phase 2 §1 mkCounter pattern).
            EmitTypes.buildUnitValue env b

        | TExprG.Null _ -> b.Add ILInstr.Ldnull

        | TExprG.Var(binding, varTy, _) when env.StaticMethods.ContainsKey binding ->
            // A *generic* module value (`let empty : SetTree<'T> = …` at module
            // scope) lowers to a zero-arg generic static method on its holder (a
            // non-generic module holder cannot host a `SetTree<'T>` *field*;
            // module-representation-plan). A module value is never applied, so —
            // unlike a static *function*, which `collectStaticFns` proves is always
            // saturated and therefore only ever reaches codegen as an `App` head —
            // it appears here as a bare `Var`. (Hence: a bare `Var` whose key is a
            // static method is always one of these 0-arg value methods.) Emit a
            // 0-arg `call` to its `MethodSpec`, the instantiation recovered by
            // matching the method's declared result template against this
            // reference's own type.
            let sm = env.StaticMethods.[binding]

            let callHandle =
                if sm.Typars = 0 then
                    sm.Handle
                else
                    let inst = matchInstantiation sm.Typars [ sm.ResultTy ] [ varTy ]
                    env.Provider.StaticFnMethodSpec(sm.Handle, inst)

            b.Add(ILInstr.Call(callHandle, 0, 1))

        | TExprG.Var(binding, _, _) -> buildVarLoad env b binding

        | TExprG.Let _ -> EmitBindings.buildLet buildExpr env b e
        | TExprG.Use _ -> EmitBindings.buildUse buildExpr env b e

        | TExprG.ForIn _ -> EmitLoops.buildForIn buildExpr env b e
        | TExprG.ForTo _ -> EmitLoops.buildForTo buildExpr env b e
        | TExprG.While _ -> EmitLoops.buildWhile buildExpr env b e

        | TExprG.Sequential _ -> EmitMatch.buildSequential buildExpr env b e
        | TExprG.IfThenElse _ -> EmitMatch.buildIfThenElse buildExpr env b e
        | TExprG.Match _ -> EmitMatch.buildMatch buildExpr env b e

        | TExprG.Lambda _ -> EmitConstruct.buildLambda env b e
        | TExprG.New _ -> EmitConstruct.buildNew buildExpr env b e
        | TExprG.RecordCons _ -> EmitConstruct.buildRecordCons buildExpr env b e
        | TExprG.RecordClone _ -> EmitConstruct.buildRecordClone buildExpr env b e
        | TExprG.UnionCons _ -> EmitConstruct.buildUnionCons buildExpr env b e
        | TExprG.Tuple _ -> EmitConstruct.buildTuple buildExpr env b e

        | TExprG.App _ -> EmitCall.buildAppCall buildExpr env b e

        // A bare external value with no application — a zero-arg module value such
        // as `Set.empty` (the `[<GeneralizableValue>]` generic value compiled to a
        // generic static method `SetModule.Empty<'T>()`). Route it through the same
        // head dispatch as an application with an empty spine: `buildAppCall`
        // collects a zero-length spine, `TryEmitCall` emits the 0-arg recipe, and
        // the generic instantiation is read from the value's (result) type.
        | TExprG.External _ -> EmitCall.buildAppCall buildExpr env b e

        | TExprG.FieldGet _ -> EmitMember.buildFieldGet buildExpr env b e
        | TExprG.Assignment _ -> EmitMember.buildAssignment buildExpr env b e
        | TExprG.FieldSet _ -> EmitMember.buildFieldSet buildExpr env b e
        | TExprG.PropertyGet _ -> EmitMember.buildPropertyGet buildExpr env b e
        | TExprG.MethodCall _ -> EmitMember.buildMethodCall buildExpr env b e
        | TExprG.StaticPropertyGet _ -> EmitMember.buildStaticPropertyGet env b e
        | TExprG.StaticFieldGet _ -> EmitMember.buildStaticFieldGet env b e
        | TExprG.StaticMethodCall _ -> EmitMember.buildStaticMethodCall buildExpr env b e
        | TExprG.ExternalMember _ -> EmitMember.buildExternalMember buildExpr env b e

        | TExprG.Format(sink, segments, _, _) -> EmitFormat.buildFormat buildExpr env b sink segments

        | TExprG.ILIntrinsic _ -> EmitIntrinsic.buildILIntrinsic buildExpr env b e
        | TExprG.StaticOptimization _ -> EmitIntrinsic.buildStaticOptimization buildExpr env b e
        | TExprG.Upcast _ -> EmitIntrinsic.buildUpcast buildExpr env b e
        | TExprG.Downcast _ -> EmitIntrinsic.buildDowncast buildExpr env b e
        | TExprG.TypeTest _ -> EmitIntrinsic.buildTypeTest buildExpr env b e

        | other -> failwithf "Emit: unsupported expression: %A" other

    /// Emit an expression as a statement: evaluate it and discard any value.
    let buildStatement (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        buildExpr env b e

        while b.Depth > 0 do
            b.Add ILInstr.Pop
