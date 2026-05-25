module XParsec.FSharp.Codegen.Clr.Tests.IlIrTests

open Expecto
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The reified IL-buffer's own unit suite (XParsec.FSharp.Codegen.Clr.IlIr). The two
// demonstrators below stand in for the two real producers — `buildExpr` for the
// dynamic TAST walker, `guardChainEquality` for a per-type template — built into an
// `ILBody`, run through `IlIr.analyze`/`verify`/`lower`. The data points: both
// producers share one structure + one `lower`; `verify`'s independently-computed
// maxStack matches what lowering tracks; and `verify` rejects an imbalance built
// dynamically (a label reached at two depths, an underflow) — the check the production
// walker has no static type to give it.

// ---- Demonstrators: the two producer shapes over the IlIr buffer ----

/// A bounded slice of the dynamic TAST walker, retargeted from eager `emit` to
/// "append to the buffer": enough node kinds (`Const`, `IfThenElse`, `ILIntrinsic`)
/// to exercise a real branch-merge on a real `TExpr`. The shape mirrors the
/// production `Emit.buildExpr` arm — a plain recursive append.
let rec private buildExpr (b: IlBuilder) (e: TExpr) : unit =
    match e with
    | TExpr.Const(TConstValue.Int n, _) -> b.Add(ILInstr.LdcI4 n)
    | TExpr.Const(TConstValue.Bool v, _) -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
    | TExpr.IfThenElse(c, t, f, _) ->
        let elseL = b.Label()
        let endL = b.Label()
        buildExpr b c
        b.Add(ILInstr.Brfalse elseL)
        buildExpr b t
        b.Add(ILInstr.Br endL)
        b.Add(ILInstr.Mark elseL)
        buildExpr b f
        b.Add(ILInstr.Mark endL)
    | TExpr.ILIntrinsic(op, args, _) ->
        for a in args do
            buildExpr b a

        match Cil.tryOpCodeOfMnemonic op with
        | ValueSome code ->
            match args with
            | [ _; _ ] -> b.Add(ILInstr.Bin code)
            | [ _ ] -> b.Add(ILInstr.Un code)
            | _ -> failwithf "IlIrTests.buildExpr: %d-ary intrinsic '%s' unsupported in the demo" (List.length args) op
        | ValueNone -> failwithf "IlIrTests.buildExpr: unknown intrinsic '%s'" op
    | other -> failwithf "IlIrTests.buildExpr (demo subset): %A" other

/// Wrap a value-producing expression as a `Main` body (`<expr>; ret`).
let private mainOf (e: TExpr) : ILBody =
    let b = IlBuilder()
    buildExpr b e
    b.Add ILInstr.Ret
    b.Body

/// The template shape: the early-exit skeleton of `Emit.buildUnionEqualsTyped` — N
/// guards each branching to one shared `false` tail, then two `ret`s. Built over
/// constants so it runs standalone (returns 1 when every pair is equal, else 0). It
/// exercises exactly the false-tail / dual-`ret` depth bookkeeping the production
/// template leans on `analyze` to derive.
let private guardChainEquality (pairs: (int * int) list) : ILBody =
    let b = IlBuilder()
    let falseL = b.Label()

    for (x, y) in pairs do
        b.Add(ILInstr.LdcI4 x)
        b.Add(ILInstr.LdcI4 y)
        b.Add(ILInstr.BneUn falseL)

    b.Add(ILInstr.LdcI4 1)
    b.Add ILInstr.Ret
    b.Add(ILInstr.Mark falseL)
    b.Add(ILInstr.LdcI4 0)
    b.Add ILInstr.Ret
    b.Body

[<Tests>]
let tests =
    let tyInt = TyConst "int"
    let tyBool = TyConst "bool"
    let cInt n = TExpr.Const(TConstValue.Int n, tyInt)

    let ceq a b =
        TExpr.ILIntrinsic("ceq", [ a; b ], tyBool)

    // Lower into a standalone `Il` (no metadata context — the demo bodies carry no
    // tokens) to read the maxStack the live tracker computes, for cross-checking
    // `verify`'s independent number.
    let loweredMaxStack (body: ILBody) : int =
        let il = Il(InstructionEncoder(BlobBuilder(), ControlFlowBuilder()))
        IlIr.lower body il
        il.MaxStack

    let runBody (name: string) (body: ILBody) : int =
        let bytes =
            Codegen.assembleMainEmit (ProjectInfo.defaults name) (IlIr.lower body)
            |> Codegen.toBytes

        let code, _ = runEntryPoint bytes
        code

    testList
        "IlIr"
        [
            // ---- general TAST → buffer (a dynamic walker arm) ----
            test "IfThenElse over a real TExpr lowers + runs (then-arm)" {
                let e = TExpr.IfThenElse(ceq (cInt 5) (cInt 5), cInt 42, cInt 7, tyInt)
                Expect.equal (runBody "IrIfTrue" (mainOf e)) 42 "5 = 5 → 42"
            }

            test "IfThenElse over a real TExpr lowers + runs (else-arm)" {
                let e = TExpr.IfThenElse(ceq (cInt 5) (cInt 4), cInt 42, cInt 7, tyInt)
                Expect.equal (runBody "IrIfFalse" (mainOf e)) 7 "5 = 4 → 7"
            }

            test "a nested IfThenElse merges correctly through the buffer" {
                // if (1=2) then 99 else (if (3=3) then 42 else 0)
                let inner = TExpr.IfThenElse(ceq (cInt 3) (cInt 3), cInt 42, cInt 0, tyInt)
                let e = TExpr.IfThenElse(ceq (cInt 1) (cInt 2), cInt 99, inner, tyInt)
                Expect.equal (runBody "IrIfNest" (mainOf e)) 42 "false → inner true → 42"
            }

            // ---- template early-exit shape (the union-equals skeleton) ----
            test "the guard-chain template returns 1 when every pair is equal" {
                Expect.equal (runBody "IrGuardEq" (guardChainEquality [ 1, 1; 2, 2; 3, 3 ])) 1 "all equal → 1"
            }

            test "the guard-chain template returns 0 when a pair differs" {
                Expect.equal (runBody "IrGuardNe" (guardChainEquality [ 1, 1; 2, 9; 3, 3 ])) 0 "a pair differs → 0"
            }

            // ---- verify: maxStack faithfulness + imbalance detection ----
            test "verify's maxStack matches what lowering actually tracks" {
                let bodies =
                    [
                        mainOf (TExpr.IfThenElse(ceq (cInt 5) (cInt 5), cInt 42, cInt 7, tyInt))
                        guardChainEquality [ 1, 1; 2, 2 ]
                    ]

                for body in bodies do
                    match IlIr.verify body with
                    | Result.Ok ms -> Expect.equal ms (loweredMaxStack body) "verify maxStack = Il-tracked maxStack"
                    | Result.Error e -> failtestf "verify rejected a valid body: %s" e
            }

            test "verify accepts a balanced body with the expected maxStack" {
                match IlIr.verify (guardChainEquality [ 1, 1 ]) with
                | Result.Ok ms -> Expect.equal ms 2 "two ints on the stack at the bne.un"
                | Result.Error e -> failtestf "unexpected imbalance: %s" e
            }

            test "verify catches a label reached at two different depths" {
                // ldc 1; brfalse skip; ldc 10; skip: ret
                // fall-through into `skip` is depth 1, but the brfalse targets it at
                // depth 0 — an imbalance only an analysis over the built buffer can see.
                let b = IlBuilder()
                let skip = b.Label()
                b.Add(ILInstr.LdcI4 1)
                b.Add(ILInstr.Brfalse skip)
                b.Add(ILInstr.LdcI4 10)
                b.Add(ILInstr.Mark skip)
                b.Add ILInstr.Ret

                match IlIr.verify b.Body with
                | Result.Error _ -> ()
                | Result.Ok ms -> failtestf "expected an imbalance error, got maxStack %d" ms
            }

            test "verify catches a stack underflow" {
                // pop on an empty stack
                let b = IlBuilder()
                b.Add ILInstr.Pop
                b.Add(ILInstr.LdcI4 0)
                b.Add ILInstr.Ret

                match IlIr.verify b.Body with
                | Result.Error _ -> ()
                | Result.Ok ms -> failtestf "expected underflow error, got maxStack %d" ms
            }
        ]
