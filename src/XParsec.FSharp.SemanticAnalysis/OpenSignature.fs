namespace XParsec.FSharp.SemanticAnalysis

module OpenSignature =

    type OpenMethodSignature =
        {
            /// Curried `param -> … -> return`, method typars as `FTTypar(Method, i)`.
            Signature: FrozenType
            /// The `MethodSpec` generic-parameter count. INCLUDES a phantom typar that
            /// occurs only in a `Coercion` bound (the enumerator `'E` in
            /// `'S :> IStructSeq<'T,'E>`), so it matches the producer's emitted arity.
            MethodTyparArity: int
            Constraints: FrozenConstraint list
        }

    /// Projects a symbol's contract scheme onto the method axis, `Declaring i ↦ Method i`
    /// POSITIONALLY — the producer assigns its `!!i` slots in that same declared order.
    let ofSymbol (sym: ExternalSymbol) : OpenMethodSignature =
        let openSig =
            FrozenTypeBridge.instantiateWith
                (fun i -> TyTypar(TyparAxis.Method, i))
                (fun j -> TyTypar(TyparAxis.Method, j))
                (FrozenTypeBridge.localTyparInTemplate "OpenSignature.ofSymbol")
                sym.Scheme

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
