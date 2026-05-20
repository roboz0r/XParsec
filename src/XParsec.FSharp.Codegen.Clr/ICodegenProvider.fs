namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

// The codegen (target) provider — mirror of `IExternalSymbolProvider`. The
// symbol provider knows a compiled name's *shape*; the codegen provider knows
// how to *emit a call* to it. `TExpr.External` / `TExpr.New` are the only
// nodes the walker consults it for. See
// [codegen-clr-plan](../XParsec.FSharp.SemanticAnalysis/docs/codegen-clr-plan.md)
// §"The codegen (target) provider".

/// How to emit a resolved call once its arguments are on the stack. `Emit`
/// performs the call itself — a `call` / `callvirt` against a metadata handle,
/// or a bare intrinsic opcode like `add` (which has no handle). `ArgCount` is
/// how many stack values the operation consumes (for an instance call this
/// includes the receiver) and `Pushes` the count it produces (0 or 1); the
/// walker adjusts the tracked depth by `Pushes - ArgCount`.
type CallRecipe =
    {
        Emit: Il -> unit
        ArgCount: int
        Pushes: int
    }

/// How to emit a constructor (`newobj`) once its arguments are on the stack.
type CtorRecipe = { Handle: EntityHandle; ArgCount: int }

/// Resolves compiled names to emission recipes for one target. The .NET
/// implementation is `ClrProvider`. The interface is intentionally minimal in
/// v1 — per the backend posture, the contract crystallises from the working
/// implementation rather than up-front design.
type ICodegenProvider =
    /// Resolve a function/operator compiled name (as carried by
    /// `TExpr.External`) to a call recipe. `resultTy` is the application's
    /// result type, used to instantiate generic methods (e.g. the printer
    /// type argument of `PrintFormatLine`).
    abstract TryEmitCall: compiledName: string * resultTy: SemType -> CallRecipe voption

    /// Resolve a constructor (as carried by `TExpr.New` / `TExpr.UnionCons`)
    /// to a ctor recipe. `tyArgs` are the constructed type's instantiation
    /// arguments (e.g. `PrintfFormat`'s four type parameters).
    abstract TryEmitCtor: className: string * tyArgs: SemType list -> CtorRecipe voption

    /// Resolve the application of a function *value* of type `funcTy`
    /// (a `TyFun(a, b)`) to one argument — `FSharpFunc\`2::Invoke`. The
    /// receiver function and the argument are both already on the stack
    /// (receiver beneath), so the recipe's `ArgCount` is 2.
    abstract TryEmitInvoke: funcTy: SemType -> CallRecipe voption

    /// Encode a method body's declared locals into a standalone
    /// local-variable signature. Lives on the provider because encoding a
    /// `SemType` needs the target's type references.
    abstract EncodeLocalSignature: locals: SemType list -> StandaloneSignatureHandle

    /// The `System.Object` type reference, for emitted classes' base type.
    abstract ObjectType: EntityHandle
