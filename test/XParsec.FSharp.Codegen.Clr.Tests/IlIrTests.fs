module XParsec.FSharp.Codegen.Clr.Tests.IlIrTests

open Expecto
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Parser
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

let private dummyTok: SyntaxToken =
    SyntaxToken.virtualToken (XParsec.FSharp.Lexer.PositionedToken.Create(XParsec.FSharp.Lexer.Token.EOF, 0))

/// The real Vesper.Core provider, so these hand-written-IL fixtures read primitive reprs
/// from Core's own `.fs` like every other build. `buildContract` caches.
let private coreProvider: Lazy<IExternalSymbolProvider> =
    lazy ClrSymbolProviders.buildContract [ vesperCorePackage ]

// The reified IL-buffer's own unit suite. The two demonstrators below stand in for the two
// real producers, a dynamic TAST walker and a per-type template, each built into an
// `ILBody` and run through `analyze` / `verify` / `lower`.

// ---- Demonstrators: the two producer shapes over the IlIr buffer ----

/// A bounded slice of the dynamic TAST walker, appending to the buffer instead of
/// emitting: enough node kinds (`Const`, `IfThenElse`, `ILIntrinsic`) to exercise a real
/// branch-merge on a real `TExpr`.
let rec private buildExpr (b: IlBuilder) (e: TExpr) : unit =
    match e with
    | TExpr.Const(TConstValue.Integral(w, bits), _, _) -> b.Add(EmitTypes.intConstLoad w bits)
    | TExpr.Const(TConstValue.Bool v, _, _) -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
    | TExpr.IfThenElse(c, t, f, _, _) ->
        let elseL = b.Label()
        let endL = b.Label()
        buildExpr b c
        b.Add(ILInstr.Brfalse elseL)
        buildExpr b t
        b.Add(ILInstr.Br endL)
        b.Add(ILInstr.Mark elseL)
        buildExpr b f
        b.Add(ILInstr.Mark endL)
    | TExpr.ILIntrinsic(op, _, args, _, _) ->
        for a in args do
            buildExpr b a

        match Cil.tryOpCodeOfMnemonic op with
        | ValueSome code ->
            match args.Length with
            | 2 -> b.Add(ILInstr.Bin code)
            | 1 -> b.Add(ILInstr.Un code)
            | n -> failwithf "IlIrTests.buildExpr: %d-ary intrinsic '%s' unsupported in the demo" n op
        | ValueNone -> failwithf "IlIrTests.buildExpr: unknown intrinsic '%s'" op
    | other -> failwithf "IlIrTests.buildExpr (demo subset): %A" other

/// Wrap a value-producing expression as a `Main` body (`<expr>; ret`).
let private mainOf (e: TExpr) : ILBody =
    let b = IlBuilder()
    buildExpr b e
    b.Add ILInstr.Ret
    b.Body

/// The template shape: N guards each branching to one shared `false` tail, then two
/// `ret`s. Over constants so it runs standalone, returning 1 when every pair is equal and
/// 0 otherwise, which exercises the false-tail / dual-`ret` depth bookkeeping.
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
    let tyInt = TyConst(RuntimeNames.intKey, EqArray.empty)
    let tyBool = TyConst(RuntimeNames.boolKey, EqArray.empty)

    let cInt (n: int) =
        TExpr.Const(TConstValue.Integral(IntWidth.Int32, int64 n), tyInt, dummyTok)

    let ceq a b =
        TExpr.ILIntrinsic("ceq", ValueNone, EqArray.ofList [ a; b ], tyBool, dummyTok)

    // The demo bodies carry no metadata tokens, so a standalone `Il` can lower them and
    // report the maxStack the live tracker computes, against `verify`'s own number.
    let loweredMaxStack (body: ILBody) : int =
        let il = Il(InstructionEncoder(BlobBuilder(), ControlFlowBuilder()))
        IlIr.lower body il
        il.MaxStack

    let runBody (name: string) (body: ILBody) : int =
        let bytes =
            Codegen.assembleMainEmit coreProvider.Value (ProjectInfo.defaults name) (IlIr.lower body)
            |> Codegen.toBytes

        let code, _ = runEntryPoint bytes
        code

    testList
        "IlIr"
        [
            // ---- general TAST → buffer (a dynamic walker arm) ----
            test "IfThenElse over a real TExpr lowers + runs (then-arm)" {
                let e = TExpr.IfThenElse(ceq (cInt 5) (cInt 5), cInt 42, cInt 7, tyInt, dummyTok)
                Expect.equal (runBody "IrIfTrue" (mainOf e)) 42 "5 = 5 → 42"
            }

            test "IfThenElse over a real TExpr lowers + runs (else-arm)" {
                let e = TExpr.IfThenElse(ceq (cInt 5) (cInt 4), cInt 42, cInt 7, tyInt, dummyTok)
                Expect.equal (runBody "IrIfFalse" (mainOf e)) 7 "5 = 4 → 7"
            }

            test "a nested IfThenElse merges correctly through the buffer" {
                // if (1=2) then 99 else (if (3=3) then 42 else 0)
                let inner =
                    TExpr.IfThenElse(ceq (cInt 3) (cInt 3), cInt 42, cInt 0, tyInt, dummyTok)

                let e = TExpr.IfThenElse(ceq (cInt 1) (cInt 2), cInt 99, inner, tyInt, dummyTok)
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
                        mainOf (TExpr.IfThenElse(ceq (cInt 5) (cInt 5), cInt 42, cInt 7, tyInt, dummyTok))
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
                // Fall-through reaches `skip` at depth 1, the brfalse at depth 0.
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

            // ---- Exception regions ------------------------------------------
            // The `Try`/`BeginFinally`/`BeginCatch`/`Leave` pseudo-marks bracket protected
            // regions. Handler entry depth is 0 for a finally, 1 for a catch.

            test "try/finally with no thrown exception runs both halves" {
                // result = 0; try { result = 42 } finally { result += 100 }; return result
                let b = IlBuilder()
                let result = b.Local(FTConst(RuntimeNames.intKey, EqArray.empty))
                let exitL = b.Label()
                b.Add(ILInstr.LdcI4 0)
                b.Add(ILInstr.Stloc result)
                b.Add ILInstr.Try
                b.Add(ILInstr.LdcI4 42)
                b.Add(ILInstr.Stloc result)
                b.Add(ILInstr.Leave exitL)
                b.Add ILInstr.BeginFinally
                b.Add(ILInstr.Ldloc result)
                b.Add(ILInstr.LdcI4 100)
                b.Add(ILInstr.Bin ILOpCode.Add)
                b.Add(ILInstr.Stloc result)
                b.Add ILInstr.EndFinally
                b.Add(ILInstr.Mark exitL)
                b.Add(ILInstr.Ldloc result)
                b.Add ILInstr.Ret

                Expect.equal (runBody "IrTryFin" b.Body) 142 "42 (try) + 100 (finally) = 142"
            }

            test "verify accepts a try/finally body" {
                // Same shape as above; just confirms analyze's region rules.
                let b = IlBuilder()
                let r = b.Local(FTConst(RuntimeNames.intKey, EqArray.empty))
                let exitL = b.Label()
                b.Add(ILInstr.LdcI4 0)
                b.Add(ILInstr.Stloc r)
                b.Add ILInstr.Try
                b.Add(ILInstr.LdcI4 1)
                b.Add(ILInstr.Stloc r)
                b.Add(ILInstr.Leave exitL)
                b.Add ILInstr.BeginFinally
                b.Add(ILInstr.LdcI4 2)
                b.Add(ILInstr.Stloc r)
                b.Add ILInstr.EndFinally
                b.Add(ILInstr.Mark exitL)
                b.Add(ILInstr.Ldloc r)
                b.Add ILInstr.Ret

                match IlIr.verify b.Body with
                | Result.Ok _ -> ()
                | Result.Error e -> failtestf "verify rejected a valid try/finally: %s" e
            }

            test "BeginCatch entry pushes the exception object (depth = 1)" {
                // `pop` right after BeginCatch must not underflow: the runtime already
                // pushed the exception object.
                let b = IlBuilder()
                let r = b.Local(FTConst(RuntimeNames.intKey, EqArray.empty))
                let exitL = b.Label()
                b.Add(ILInstr.LdcI4 0)
                b.Add(ILInstr.Stloc r)
                b.Add ILInstr.Try
                b.Add ILInstr.Ldnull
                b.Add ILInstr.Throw
                b.Add(ILInstr.BeginCatch(System.Reflection.Metadata.EntityHandle()))
                b.Add ILInstr.Pop // pops the exception object
                b.Add(ILInstr.LdcI4 7)
                b.Add(ILInstr.Stloc r)
                b.Add(ILInstr.Leave exitL)
                b.Add ILInstr.EndCatch
                b.Add(ILInstr.Mark exitL)
                b.Add(ILInstr.Ldloc r)
                b.Add ILInstr.Ret

                match IlIr.verify b.Body with
                | Result.Ok ms -> Expect.isGreaterThanOrEqual ms 1 "maxStack includes the catch exception"
                | Result.Error e -> failtestf "verify rejected a valid try/catch: %s" e
            }

            test "lower fails fast on an unclosed exception region" {
                // `Try` with no matching `BeginFinally`/`BeginCatch` — `lower`
                // surfaces this as a hard error rather than emitting malformed IL.
                let b = IlBuilder()
                b.Add ILInstr.Try
                b.Add(ILInstr.LdcI4 0)
                b.Add ILInstr.Ret

                let il = Il(InstructionEncoder(BlobBuilder(), ControlFlowBuilder()))

                Expect.throws (fun () -> IlIr.lower b.Body il) "lower must reject an unclosed region"
            }

            // A real `catch (System.Object)` needs a `System.Object` `EntityHandle`, which
            // only a wired provider mints, hence the provider-aware entry point.
            test "try/finally that throws: finally runs, outer catch sees it" {
                // r = 0; try { try { throw } finally { r = 100 } } catch (object) { pop }; r
                let buildBody (provider: ICodegenProvider) (il: Il) : unit =
                    let b = IlBuilder()
                    let r = b.Local(FTConst(RuntimeNames.intKey, EqArray.empty))
                    let outerExit = b.Label()
                    let innerExit = b.Label()
                    b.Add(ILInstr.LdcI4 0)
                    b.Add(ILInstr.Stloc r)
                    // outer try
                    b.Add ILInstr.Try
                    // inner try
                    b.Add ILInstr.Try
                    b.Add ILInstr.Ldnull
                    b.Add ILInstr.Throw
                    b.Add(ILInstr.Leave innerExit) // dead, but a try body cannot fall through
                    b.Add ILInstr.BeginFinally
                    b.Add(ILInstr.LdcI4 100)
                    b.Add(ILInstr.Stloc r)
                    b.Add ILInstr.EndFinally
                    b.Add(ILInstr.Mark innerExit)
                    // outer try body's exit (also dead under exception path)
                    b.Add(ILInstr.Leave outerExit)
                    b.Add(ILInstr.BeginCatch provider.ObjectType)
                    b.Add ILInstr.Pop
                    b.Add(ILInstr.Leave outerExit)
                    b.Add ILInstr.EndCatch
                    b.Add(ILInstr.Mark outerExit)
                    b.Add(ILInstr.Ldloc r)
                    b.Add ILInstr.Ret
                    IlIr.lower b.Body il

                let bytes =
                    Codegen.assembleMainEmitWithProvider
                        coreProvider.Value
                        (ProjectInfo.defaults "IrTryCatch")
                        buildBody
                    |> Codegen.toBytes

                let code, _ = runEntryPoint bytes
                Expect.equal code 100 "finally ran (set 100), catch swallowed the exception, return value = 100"
            }
        ]
