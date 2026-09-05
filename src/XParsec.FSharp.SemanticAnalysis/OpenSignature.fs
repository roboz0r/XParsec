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
            Constraints: EqSet<FrozenConstraint>
        }

    /// Projects a symbol's contract scheme onto the method axis, `Declaring i ↦ Method i`
    /// POSITIONALLY, because the producer assigns its `!!i` slots in that same declared order.
    let ofSymbol (sym: ExternalSymbol) : OpenMethodSignature =
        let toMethodAxis = FrozenTypeBridge.reaxisTo TyparAxis.Method

        let constraints =
            EqSet.ofSeq
                [
                    for c in sym.Constraints do
                        match c with
                        | ExternalConstraint.Bound b -> TyparConstraint.map toMethodAxis b
                        // Resolved during inference; neither has a metadata encoding.
                        | ExternalConstraint.MemberTrait _
                        | ExternalConstraint.Default _ -> ()
                ]

        {
            Signature = toMethodAxis sym.Scheme
            MethodTyparArity = sym.TyparArity
            Constraints = constraints
        }
