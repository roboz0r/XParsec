namespace XParsec.FSharp.SemanticAnalysis

// SEMANTIC typar-COUNT and typar-ORDER conformance between a `.fsi` and its `.fs`.

module ConformanceTypars =

    /// A binding whose `.fs`-inferred generic scheme disagrees with its `.fsi`-declared
    /// one: a different typar COUNT or a different typar ORDER.
    [<NoEquality; NoComparison>]
    type TyparMismatch =
        {
            /// The binding key's qualified rendering, for the message alone
            /// (`Vesper.ListModule.fold`).
            Name: string
            Declared: FrozenType
            Inferred: FrozenType
        }

    /// One binding's disagreement as a compiler message.
    let describe (m: TyparMismatch) : string =
        sprintf
            "%s: declared %s, but the implementation infers %s"
            m.Name
            (Conformance.describeType m.Declared)
            (Conformance.describeType m.Inferred)

    /// True iff the `.fsi`-declared and `.fs`-inferred schemes agree WITH typar order. Both
    /// are written under the binding's `ModuleFunction` scope, so a `.fs` `<'b,'a>` against a
    /// `.fsi` `<'a,'b>` fails.
    let schemesAgree (declared: FrozenType) (inferred: FrozenType) : bool = declared = inferred

    /// Check every generic module binding of a frozen implementation file against the contract
    /// `provider`, in source-declaration order. A binding the provider does not publish, or a
    /// monomorphic one, has no typar order to compare and is skipped.
    ///
    /// `inline` included: a binding's typars are the slots a splice fills, so a body folding
    /// `^T1 -> ^T2 -> ^T3` into one `^T` binds `y` at `x`'s type.
    let checkFile (provider: IExternalSymbolProvider) (pools: FrozenPools) : TyparMismatch list =
        let pool = TastPoolBuilder.openOver pools

        let check (boundVar: BoundVarId) (ty: FrozenType) : TyparMismatch voption =
            let key = (TastPoolBuilder.moduleMemberOf pool boundVar).BindingKey

            match provider.TryLookupByKey key with
            | ValueSome sym when sym.TyparArity > 0 && not (schemesAgree sym.Scheme ty) ->
                ValueSome
                    {
                        Name = SymbolKeyOps.qualifiedBindingName key
                        Declared = sym.Scheme
                        Inferred = ty
                    }
            | _ -> ValueNone

        [
            for m in TastAccessor.rootBindings pool do
                match m.Pattern with
                | TastAccessor.PNamed boundVar ->
                    match check boundVar m.Ty with
                    | ValueSome mismatch -> yield mismatch
                    | ValueNone -> ()
                | _ -> ()
        ]
