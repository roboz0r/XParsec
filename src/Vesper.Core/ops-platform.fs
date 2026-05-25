namespace Vesper

open System.Collections.Generic

// ops-platform.fs — the per-target *implementation* of `ops-platform.fsi`
// (symbol-resolution-plan §5.2: the `.fsi` is the target-agnostic contract, the
// `.fs` is the binding). Milestone M (symbol-resolution-handoff.md) implements
// ONLY `hash` here: it is a normal identifier (so it clears the operator-named-
// binding freeze gap that still blocks `=`/`+`/… — core-operators-handoff.md),
// and its inline body lowers to a BCL `EqualityComparer<'T>` call. That body is
// read across the package boundary by the codegen inline-body loader
// (`SymbolProviders.inlineBodies`) and spliced at each `hash` use site by
// `Emit.lower`'s `External`→inline-body routing, replacing the `Emit.isHash`
// stopgap.
//
// The arithmetic / equality / bitwise OPERATOR bodies (the `(# "ceq" … #)`
// static-optimization clauses `ops-platform.fsi` declares) are NOT implemented
// here yet — they are still served by the codegen `Emit.BuiltinOps` stopgap,
// blocked on the operator-named-binding freeze gap. Adding them is the next step
// after M (core-operators-handoff.md, "operator-named bindings don't freeze").

[<AutoOpen>]
module Operators =

    /// Generate a hash value for the given value. No dedicated runtime member: it
    /// rides the BCL `EqualityComparer<'T>` — the same family the generated DU
    /// equality triple hashes its fields through — so `hash` and `=` agree by
    /// construction (equal values hash equal). BCL-only (no FSharp.Core, no Vesper
    /// runtime library).
    let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj
