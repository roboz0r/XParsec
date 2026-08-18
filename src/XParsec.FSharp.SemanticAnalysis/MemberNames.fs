namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

/// The name and declaration site a member binding contributes. `member this.M …` parses the
/// member name as the binding's bound pattern, so both are read off that pattern.
module MemberNames =

    /// The pattern actually naming the member, under any annotation / attribute / paren
    /// wrapping: `member this.M` names by `NamedSimple`, `member (+)` by `Op`.
    let rec private namingPat (p: Pat<SyntaxToken>) : Pat<SyntaxToken> voption =
        match p with
        | Pat.NamedSimple _
        | Pat.Op _ -> ValueSome p
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner)
        | Pat.Attributed(pat = inner) -> namingPat inner
        | _ -> ValueNone

    /// The key a member registers its `DeclSite` under: its naming pattern's, so
    /// `static member (+) (a, b) = …` keys on `(lParen, PatOp)`, not on the operator token.
    let declKeyOfBinding (b: Binding<SyntaxToken>) : NodeKey voption =
        namingPat b.pattern |> ValueOption.map CstKeys.ofPat

    /// A member's name and declaration site. An operator-named member takes its compiled
    /// name (`op_Addition`), which is how a use site refers to it.
    let ofBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : {| Name: string; Site: NodeSite |} voption =
        match namingPat b.pattern with
        | ValueSome(Pat.NamedSimple id as named) ->
            ValueSome
                {|
                    Name = ctx.NameOf id
                    Site = CstKeys.siteOfPat named
                |}
        | ValueSome(Pat.Op io as named) ->
            OperatorNames.ofPatOp ctx.NameOf io
            |> ValueOption.map (fun n ->
                {|
                    Name = n
                    Site = CstKeys.siteOfPat named
                |}
            )
        | _ -> ValueNone
