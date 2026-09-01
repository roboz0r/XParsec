namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

/// The bindings after `member x.P with …`, read as the accessors they declare. The parser
/// admits any binding there, so rejecting the ones declaring neither `get` nor `set` is this
/// module's job rather than the grammar's.
module PropertyAccessors =

    /// One half of `member x.P with get … and set …`, named and kinded as the member it
    /// declares.
    type PropertyAccessor =
        {
            /// `P` for a parameterless getter; `get_P` / `set_P` for every other accessor.
            Name: string
            Kind: TMemberKind
            /// The `get` / `set` token's site, so the two accessors of one property key apart.
            Site: NodeSite
            /// A parameterless getter's `()` is dropped, so the member types as `T` rather
            /// than `unit -> T`, identical to a `member x.P = …` binding.
            Defn: Binding<SyntaxToken>
        }

    /// `get ()` and `get` alike take no argument; `get (i)` is an indexer.
    let private isParameterless (b: Binding<SyntaxToken>) : bool =
        match b.argumentPats.Length with
        | 0 -> true
        | 1 ->
            match b.argumentPats.[0] with
            | Pat.EmptyBlock _ -> true
            | _ -> false
        | _ -> false

    /// The accessor one `with`-clause binding declares; `ValueNone` for a binding that
    /// declares neither `get` nor `set`.
    let private accessorOf (ctx: PassContext) (propName: string) (b: Binding<SyntaxToken>) : PropertyAccessor voption =
        match MemberNames.ofBinding ctx b with
        | ValueSome m ->
            match m.Name with
            | "get" when isParameterless b ->
                ValueSome
                    {
                        Name = propName
                        Kind = TMemberKind.Property
                        Site = m.Site
                        Defn =
                            { b with
                                argumentPats = ImmutableArray.Empty
                            }
                    }
            | "get" ->
                ValueSome
                    {
                        Name = AccessorNames.getterName propName
                        Kind = TMemberKind.Accessor(propName, TAccessorRole.Getter)
                        Site = m.Site
                        Defn = b
                    }
            | "set" ->
                ValueSome
                    {
                        Name = AccessorNames.setterName propName
                        Kind = TMemberKind.Accessor(propName, TAccessorRole.Setter)
                        Site = m.Site
                        Defn = b
                    }
            | _ -> ValueNone
        | ValueNone -> ValueNone

    /// The accessors a `with` clause declares, in source order.
    let accessors
        (ctx: PassContext)
        (propIdent: SyntaxToken)
        (defns: ImmutableArray<Binding<SyntaxToken>>)
        : PropertyAccessor[] =
        let propName = ctx.NameOf propIdent

        [|
            for b in defns do
                match accessorOf ctx propName b with
                | ValueSome a -> yield a
                | ValueNone -> ()
        |]

    /// A binding the PARSER rejected and replaced with a placeholder. Its error is already
    /// reported, so it is owed no further complaint.
    let private isRecovered (b: Binding<SyntaxToken>) : bool =
        match b.pattern with
        | Pat.Missing
        | Pat.SkipsTokens _ -> true
        | _ -> false

    /// A `with` clause binds `get` / `set` and nothing else — a property with no clause at
    /// all is an implicit get, and the parser rejects any other name — so anything else
    /// reaching here is an invariant break. Registration is the only consumer: every other
    /// pass walks the accessors alone.
    let reportNonAccessors
        (ctx: PassContext)
        (propIdent: SyntaxToken)
        (defns: ImmutableArray<Binding<SyntaxToken>>)
        : unit =
        let propName = ctx.NameOf propIdent

        for b in defns do
            match accessorOf ctx propName b with
            | ValueSome _ -> ()
            | ValueNone when isRecovered b -> ()
            | ValueNone ->
                let tok =
                    match MemberNames.ofBinding ctx b with
                    | ValueSome m -> m.Site.Tok
                    | ValueNone -> (CstKeys.siteOfBinding b).Tok

                ctx.Report(tok, Kind.Internal(InternalBreak.NonAccessorInWithClause(propName, ctx.NameOf tok)))
