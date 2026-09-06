namespace XParsec.FSharp.SemanticAnalysis

module OpenSignature =

    type OpenMethodSignature =
        {
            /// Curried `param -> … -> return`, the function's own typars as
            /// `FTTypar(ModuleFunction _, i)`.
            Signature: FrozenType
            /// The `MethodSpec` generic-parameter count and the constraints over it. The
            /// arity INCLUDES a phantom typar that occurs only in a `Coercion` constraint
            /// (the enumerator `'E` in `'S :> IStructSeq<'T,'E>`), so it matches the
            /// producer's emitted arity.
            Scheme: GenericFnScheme
        }

    /// A symbol's contract scheme with its encodable constraints. The scheme's typar index
    /// is the producer's `!!i` slot, assigned in the same declared order.
    let ofSymbol (sym: ExternalSymbol) : OpenMethodSignature =
        let constraints =
            EqSet.ofSeq
                [
                    for c in sym.Constraints do
                        match c with
                        | ExternalConstraint.Encodable b -> b
                        // Resolved during inference; neither has a metadata encoding.
                        | ExternalConstraint.MemberTrait _
                        | ExternalConstraint.Default _ -> ()
                ]

        {
            Signature = sym.Scheme
            Scheme = GenericFnScheme.create sym.TyparArity constraints
        }
