namespace XParsec.FSharp.SemanticAnalysis

// SEMANTIC typar-COUNT and typar-ORDER conformance between a `.fsi` and its `.fs`.

module ConformanceTypars =

    /// A scheme as a compiler MESSAGE reads it: `'a -> ('a * int) -> 'b`. Typars render by
    /// index off their axis, which is what a disagreement here is always about. Approximate
    /// by design — a shape with no source spelling (`keyof`, a conditional) renders as its
    /// constructor and children rather than growing a second type syntax.
    let rec private describeType (t: FrozenType) : string =
        let args (name: string) (xs: EqArray<FrozenType>) =
            match xs.Length with
            | 0 -> name
            | _ -> sprintf "%s<%s>" name (xs |> Seq.map describeType |> String.concat ", ")

        // A nested function or tuple is parenthesised: both are left-ambiguous otherwise.
        let nested (x: FrozenType) =
            match x with
            | FTFun _
            | FTTuple _ -> "(" + describeType x + ")"
            | _ -> describeType x

        match t with
        | FTConst(key, a) -> args key.Name a
        | FTRecord(key, a)
        | FTUnion(key, a)
        | FTClass(key, a) -> args key.Name a
        | FTEnum key -> key.Name
        | FTFun(arg, result) -> sprintf "%s -> %s" (nested arg) (describeType result)
        | FTTuple items -> items |> Seq.map nested |> String.concat " * "
        | FTOr disjuncts ->
            disjuncts.Disjuncts
            |> EqSet.toList
            |> List.map describeType
            |> String.concat " | "
        | FTLiteral value -> sprintf "%A" value
        | FTKeyOf ty -> sprintf "keyof %s" (nested ty)
        | FTIndexedAccess(objTy, index) -> sprintf "%s[%s]" (nested objTy) (describeType index)
        | FTConditional p ->
            sprintf
                "%s extends %s ? %s : %s"
                (nested p.Check)
                (nested p.Extends)
                (describeType p.WhenTrue)
                (describeType p.WhenFalse)
        // The INDEX is the quantification order, which is exactly what these checks compare,
        // so it is what the name shows: `'0`, `'1`, … rather than a source-invented `'a`.
        | FTTypar(_, index) -> sprintf "'%d" index
        | FTLocalTypar(_, index) -> sprintf "'local%d" index
        | FTUnknown reason -> reason.Render

    /// A binding whose `.fs`-inferred generic scheme disagrees with its `.fsi`-declared
    /// one: a different typar COUNT or a different typar ORDER.
    [<NoEquality; NoComparison>]
    type TyparMismatch =
        {
            /// The provider name the binding resolved under (e.g. `ListModule.fold`).
            Name: string
            Declared: FrozenType
            Inferred: FrozenType
        }

    /// One binding's disagreement as a compiler message.
    let describe (m: TyparMismatch) : string =
        sprintf
            "%s: declared %s, but the implementation infers %s"
            m.Name
            (describeType m.Declared)
            (describeType m.Inferred)

    /// Rewrite every `FTTypar` onto `axis`, index kept: a free value/function has exactly
    /// ONE axis, so `FTTypar(Declaring, i)` and `FTTypar(Method, i)` denote the same slot.
    let rec normAxisTo (axis: TyparAxis) (t: FrozenType) : FrozenType =
        match t with
        | FTTypar(_, i) -> FTTypar(axis, i)
        | t -> FrozenType.mapChildren (normAxisTo axis) t

    let normAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Method t

    /// Land a FREE value/function's single typar axis on `Declaring`, the axis a provider
    /// scheme speaks, because instantiating one throws on a `Method` typar.
    let toDeclaringAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Declaring t

    /// True iff the `.fsi`-declared and `.fs`-inferred schemes are α-equivalent WITH typar
    /// order: `FTTypar`'s index IS the quantification order, so axis-normalized structural
    /// equality fails exactly when a `.fs` `<'b,'a>` meets a `.fsi` `<'a,'b>`.
    let schemesAgree (declared: FrozenType) (inferred: FrozenType) : bool = normAxis declared = normAxis inferred

    /// The lookup names to try, most-specific first: the binding's own key
    /// (`Vesper.ArithmeticOperators.op_Addition`, the name a `.fsi` in a namespace publishes),
    /// then the module-qualified one (`ListModule.fold`), then the bare one, which covers a
    /// top-level binding.
    let private lookupNames (info: ModuleBindingInfo option) (name: string) : string list =
        match info with
        | Some mi ->
            [
                SymbolKeyOps.qualifiedName mi.Key

                match mi.DeclaringModule with
                | ValueSome m -> m.Name + "." + mi.Name
                | ValueNone -> ()

                mi.Name
            ]
            |> List.distinct
        | None -> [ name ]

    /// Check every generic module binding of a frozen implementation file against the contract
    /// `provider`, in source-declaration order. A binding the provider does not publish, or a
    /// monomorphic one, has no typar order to compare and is skipped.
    ///
    /// `inline` included: a binding's typars are the slots a splice fills, so a body folding
    /// `^T1 -> ^T2 -> ^T3` into one `^T` binds `y` at `x`'s type.
    let checkFile (provider: IExternalSymbolProvider) (pools: FrozenPools) : TyparMismatch list =
        let pool = TastPoolBuilder.openOver pools
        let moduleMembers = DenseTable.index pools.ModuleMembers

        [
            for decl in TastAccessor.roots pool do
                match decl with
                | TastAccessor.DLet {
                                        Pattern = TastAccessor.PNamed boundVar
                                        Ty = ty
                                    } ->
                    let info =
                        match moduleMembers.TryGetValue boundVar with
                        | true, mi -> Some mi
                        | _ -> None

                    // A binding inside a named module publishes under its COMPILED name
                    // (`[<CompiledName>]`), which only the member table knows.
                    let nameOpt =
                        match info with
                        | Some mi -> Some mi.Name
                        | None ->
                            match TastPoolBuilder.boundVarNaming pool boundVar with
                            | BoundVarNaming.Source n -> Some n
                            | BoundVarNaming.Minted _ -> None

                    match nameOpt with
                    | None -> ()
                    | Some name ->
                        let resolved =
                            lookupNames info name
                            |> List.tryPick (fun n ->
                                match provider.TryLookup n with
                                | ValueSome s -> Some(n, s)
                                | ValueNone -> None
                            )

                        match resolved with
                        | Some(n, sym) when sym.TyparArity > 0 ->
                            if not (schemesAgree sym.Scheme ty) then
                                yield
                                    {
                                        Name = n
                                        Declared = normAxis sym.Scheme
                                        Inferred = normAxis ty
                                    }
                        | _ -> ()
                | _ -> ()
        ]

    /// A generic type MEMBER whose `.fs`-inferred signature disagrees with every published
    /// `.fsi` overload of matching method arity.
    [<NoEquality; NoComparison>]
    type MemberMismatch =
        {
            /// The declaring type's compiled name (`Vesper.Formatter`).
            TypeName: string
            MemberName: string
            /// Always `> 0`: this pass only checks generic members.
            MethodTyparArity: int
            Inferred: FrozenType
            /// The matching-arity published overloads' signatures.
            Published: FrozenType list
        }

    /// One member's disagreement as a compiler message. The published overloads are listed:
    /// with more than one of the same arity, which one was MEANT is the reader's question.
    let describeMember (m: MemberMismatch) : string =
        sprintf
            "%s.%s: the implementation's %s matches no declared overload of %d method type parameter(s) (declared: %s)"
            m.TypeName
            m.MemberName
            (describeType m.Inferred)
            m.MethodTyparArity
            (m.Published |> List.map describeType |> String.concat "; ")

    let private tupledParams (ps: EqArray<BoundVarKeyG<'id> * FrozenType>) : FrozenType =
        ExternalSignature.tupledParams (EqArray.map snd ps)

    /// The single-`FrozenType` shape both the `.fs` member and the `.fsi` overload fold
    /// to, so a `=` is the conformance check.
    let private memberSigOf (isProperty: bool) (parameters: FrozenType) (ret: FrozenType) : FrozenType =
        if isProperty then ret else FTFun(parameters, ret)

    /// The published half folded to the ONE .NET parameter slot it compiles to, curried groups
    /// and all: a `.fs` binding flattens its curried patterns to one parameter vector, so
    /// grouping is not what this pass compares. Typar ORDER is.
    let private extractedSigOf (m: ExternalMember) : FrozenType =
        memberSigOf m.IsValueMember (ExternalSignature.tupledParameters m.Signature) m.Signature.Return

    /// Check every generic (method-owned-typar) MEMBER of a frozen implementation file against
    /// its contract `provider`: conformance holds when one published overload of the
    /// same name + method arity equals the inferred signature.
    let checkMembers (provider: IExternalSymbolProvider) (pools: FrozenPools) : MemberMismatch list =
        let pool = TastPoolBuilder.openOver pools

        [
            for decl in TastAccessor.roots pool do
                match TastAccessor.declKind decl with
                | DeclShape.Type ->
                    let td = TastAccessor.declType decl
                    let typeName = SymbolKeyOps.qualifiedName td.Key

                    for m in TTypeKindG.members td.Kind do
                        let arity = m.MethodTypeParams.Length

                        if arity > 0 then
                            let isProperty = (m.Kind = TMemberKind.Property)
                            let inferred = memberSigOf isProperty (tupledParams m.Params) m.ReturnTy
                            let overloads = provider.TryLookupMembers(td.TypeKey, m.Name)
                            // A different-arity overload is a different generic member.
                            let candidates = overloads |> EqArray.filter (fun em -> em.MethodTyparArity = arity)

                            // No matching-arity overload at all is member PRESENCE, not a
                            // typar-order disagreement, so it is skipped.
                            if
                                candidates.Length > 0
                                && not (candidates |> EqArray.exists (fun em -> extractedSigOf em = inferred))
                            then
                                yield
                                    {
                                        TypeName = typeName
                                        MemberName = m.Name
                                        MethodTyparArity = arity
                                        Inferred = inferred
                                        Published = [ for em in candidates -> extractedSigOf em ]
                                    }
                | _ -> ()
        ]
