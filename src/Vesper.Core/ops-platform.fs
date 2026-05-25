namespace Vesper

open System.Collections.Generic

// ops-platform.fs — the per-target *implementation* of `ops-platform.fsi`
// (symbol-resolution-plan §5.2: the `.fsi` is the target-agnostic contract, the
// `.fs` is the binding). Each inline body here is read across the package
// boundary by the codegen inline-body loader (`SymbolProviders.inlineBodies`)
// and spliced at each use site by `Emit.lowerWith`'s `External`→inline-body
// routing — the same cross-package-inline mechanism `hash` introduced
// (milestone M, symbol-resolution-handoff.md).
//
// The EQUALITY family (`=` / `<>`) is the first OPERATOR family sourced from here
// rather than the codegen `Emit.BuiltinOps` stopgap (core-operators-handoff.md
// "Phase 3"): closing the operator-named-binding freeze gap lets these `.fs`
// bodies freeze, be read by `SymbolProviders.inlineBodies`, and be spliced at use
// sites — the operator surface now flows through the same cross-package-inline +
// static-optimization machinery as `hash`, not a hard-coded codegen table.
//
// Each is an F# static-optimization over inline IL, *simplified* from
// FSharp.Core's `prim-types.fs` `(=)`/`(<>)`: the per-primitive clauses lower to
// `(# "ceq" … #)` IL the codegen `EmitIntrinsic` path interprets. The fall-clause
// (the static-opt *base*, taken when no primitive clause matches — an aggregate
// operand) is the structural `EqualityComparer<^T>.Default.Equals(x, y)` — the
// same family `hash` and the generated DU triple use, so `=`/`hash` agree by
// construction. (This routes a 2-arg external instance call through the
// cross-package inline; the tupled-arg member-emit + `recoverTypeArgs` fixes that
// makes it work are type-args-bug.md Layers 1+3.) An **unpinned** generic operand
// (`let f a b = a = b`, where `^T` is a free typar `EqualityComparer<!0>` can't
// encode) falls back to `Emit.BuiltinOps`'s `ceq` via the codegen `isGround` guard
// — the comparer can't encode a free `!0` without the deferred generic-member
// machinery.
//
// The arithmetic / bitwise OPERATOR bodies declared in `ops-platform.fsi` are
// NOT implemented here yet (they return `^T`/narrow-int types and need the
// per-clause static-opt return typing — core-operators-handoff.md); they remain
// on the `Emit.BuiltinOps` fallback until then.

[<AutoOpen>]
module EqualityOperators =

    /// Structural equality. A primitive operand lowers to a CIL `ceq` through its
    /// `when ^T : …` clause; an aggregate operand falls to the structural
    /// `EqualityComparer<^T>.Default.Equals(x, y)` base (the same comparer `hash`
    /// and the generated DU triple use). See the module comment for the unpinned
    /// generic fallback.
    let inline (=) (x: ^T) (y: ^T) : bool =
        EqualityComparer< ^T >.Default.Equals(x, y)
        when ^T: int = (# "ceq" x y : bool #)
        when ^T: int64 = (# "ceq" x y : bool #)
        when ^T: float = (# "ceq" x y : bool #)
        when ^T: float32 = (# "ceq" x y : bool #)
        when ^T: bool = (# "ceq" x y : bool #)
        when ^T: char = (# "ceq" x y : bool #)
        when ^T: byte = (# "ceq" x y : bool #)

    /// Structural inequality — the negation of `(=)`. `<>` has no CIL opcode of
    /// its own, so each form negates a `ceq` by comparing it to `false`
    /// (`ceq(b, false)` is `not b`); the base negates the structural comparer
    /// result the same way.
    let inline (<>) (x: ^T) (y: ^T) : bool =
        (# "ceq" (EqualityComparer< ^T >.Default.Equals(x, y)) false : bool #)
        when ^T: int = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: int64 = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: float = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: float32 = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: bool = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: char = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: byte = (# "ceq" (# "ceq" x y : bool #) false : bool #)

[<AutoOpen>]
module Operators =

    /// Generate a hash value for the given value. No dedicated runtime member: it
    /// rides the BCL `EqualityComparer<'T>` — the same family the generated DU
    /// equality triple hashes its fields through — so `hash` and `=` agree by
    /// construction (equal values hash equal). BCL-only (no FSharp.Core, no Vesper
    /// runtime library).
    let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj
