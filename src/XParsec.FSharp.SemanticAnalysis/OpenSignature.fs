namespace XParsec.FSharp.SemanticAnalysis

// The open (method-axis) signature of an external symbol — the `TryLookupOpenSignature`
// channel's answer. It lets `ClrRecipes.emitExternalCall` reconstruct an external call's
// signature without ever authoring a `TyVar`.
//
// Distinct from `ExternalSignature`, which is a MEMBER's two-axis `(Parameters, Return)`
// descriptor; this is a free function's whole curried monotype projected onto one axis.

module OpenSignature =

    /// The open method signature of an external symbol: its full curried monotype with the
    /// method-owned typars resolved to self-describing `TyTypar(Method, i)` nodes
    /// (`MethodTyparArity` of them).
    ///
    /// The fresh `TyVar`s `Instantiate` mints are transient and never escape
    /// `openMethodSignature` — the returned `Signature` is `TyVar`-free.
    type OpenMethodSignature =
        {
            /// Curried `param -> … -> return` frozen template with method typars as
            /// `FTTypar(Method, i)`: the codegen-facing open signature is immutable
            /// `FrozenType` data, not a `SemType`.
            Signature: FrozenType
            /// Count of distinct method typars — the `MethodSpec` generic-parameter
            /// count. INCLUDES phantom typars present only in `Coercion` bounds (the
            /// enumerator `'E` in `'S :> IStructSeq<'T,'E>`), recovered by the
            /// dependent-typar pass so the count matches the producer's emitted IL.
            MethodTyparArity: int
            /// The symbol's `when 'a :> <ty>` bounds, frozen over the method-typar
            /// axis (`FTTypar(Method, i)` leaves) in the SAME `FrozenConstraint` shape
            /// the project-local `EmitCall` phantom-typar solve consumes — so the
            /// external solve is head-agnostic. Empty for a symbol with
            /// no subtype bounds.
            Constraints: FrozenConstraint list
        }

    /// Project a free function's contract `Scheme` onto the method axis: its own
    /// typars are baked `FTTypar(Declaring, i)`, with `i` the contract's CANONICAL
    /// order — explicit `<'T>` first in declaration order, then inferred typars by
    /// first appearance (`VesperLib`'s `registerExplicitTypars` then the finalize
    /// walk). That is EXACTLY the order the producer's static-method emit assigns
    /// its `!!i` slots (`Elaborate.mkMethodQuantEnv` ▸ `GeneralizedTypars.canonical`).
    ///
    /// So each `Declaring i ↦ Method i` maps POSITIONALLY, preserving that order. Re-deriving
    /// it by first appearance over the monotype would drop an explicit `<'b,'a>`'s declared
    /// order, and a call to `Set.fold<'T,'State>` (whose declared order differs from
    /// appearance) would emit a `MethodSpec` permuted from the callee's emitted
    /// `GenericParam` order — a `MissingMethodException` at JIT.
    ///
    /// `MethodTyparArity` is the scheme's own typar count. A free function's scheme carries no
    /// `Method`-axis typars, but the freshener maps that branch identically for totality.
    let ofSymbol (sym: ExternalSymbol) : OpenMethodSignature =
        let openSig =
            FrozenTypeBridge.instantiateWith
                (fun i -> TyTypar(TyparAxis.Method, i))
                (fun j -> TyTypar(TyparAxis.Method, j))
                (FrozenTypeBridge.localTyparInTemplate "OpenSignature.ofSymbol")
                sym.Scheme

        // The scheme's `Coercion` bounds, re-expressed over the method-typar axis in the
        // SAME `FrozenConstraint` shape `EmitCall`'s project-local solve consumes — so the
        // external phantom-typar solve is head-agnostic. `typarIndex` is the CONSTRAINED
        // typar's method index (the `'S` receiver `EmitCall` reads); `target` (e.g.
        // `IStructSeq<'T,'E>`) carries the phantom typars to recover. A phantom (the
        // enumerator `'E`) is a declaring typar of the scheme that appears only inside a
        // `Coercion` target, never in a parameter/result — so it carries no `Signature`
        // position, but IS counted in `TyparArity` (hence `MethodTyparArity`) and gets its
        // method slot. Mapped POSITIONALLY (`Declaring i ↦ Method i`), matching
        // `Signature`'s declared-order projection.
        let constraints =
            [
                for c in sym.Constraints do
                    match c with
                    | ExternalConstraint.Coercion(i, target) ->
                        let openTarget =
                            FrozenTypeBridge.instantiateWith
                                (fun k -> TyTypar(TyparAxis.Method, k))
                                (fun k -> TyTypar(TyparAxis.Method, k))
                                (FrozenTypeBridge.localTyparInTemplate "OpenSignature.ofSymbol")
                                target

                        FrozenConstraint.Coercion(i, toFrozen openTarget)
                    | _ -> ()
            ]

        {
            Signature = toFrozen openSig
            MethodTyparArity = sym.TyparArity
            Constraints = constraints
        }
