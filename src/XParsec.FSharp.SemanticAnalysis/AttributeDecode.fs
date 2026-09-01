namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// One written attribute, resolved: the declaration's `TypeKey`, the construction as
/// written (where the arguments are read from), and the attribute's own type reference.
[<NoEquality; NoComparison>]
type ResolvedAttribute =
    {
        Key: TypeKey
        TypeRef: CstKeys.TypeRef
        Construction: ObjectConstruction<SyntaxToken>
    }

/// A declaration's attributes, each resolved by `TypeKey`. An unresolved attribute is
/// diagnosed at resolution and absent here.
[<NoEquality; NoComparison>]
type ResolvedAttributes =
    {
        Entries: ResolvedAttribute list
    }

    member this.Has(k: TypeKey) : bool =
        this.Entries |> List.exists (fun e -> e.Key = k)

    member this.TryFind(k: TypeKey) : ObjectConstruction<SyntaxToken> voption =
        match this.Entries |> List.tryFind (fun e -> e.Key = k) with
        | Some e -> ValueSome e.Construction
        | None -> ValueNone

    static member None: ResolvedAttributes = { Entries = [] }

/// A well-formed `[<Import>]`: the binding's implementation is the export `Selector` of the
/// committed runtime asset `Path` names, relative to the declaring package.
[<Struct>]
type ImportRef = { Selector: string; Path: string }

/// `[<Import(selector, path)>]` on a binding, as written.
[<RequireQualifiedAccess>]
type ImportDecl =
    | Import of ImportRef
    /// The attribute is present but its arguments are not two non-empty string literals.
    | Malformed
    | NoImport

/// A well-formed `[<Import>]` binding, awaiting the assembly gate's discharge: `Path` must
/// resolve through the target's module system and the asset it lists must provide
/// `Selector`. `Site` positions the resulting finding in the declaring `.fs`.
[<NoEquality; NoComparison>]
type ImportObligation =
    {
        /// The binding's source name, as a finding spells it.
        Binding: string
        Selector: string
        Path: string
        Site: SyntaxToken
    }

// The readers over `ResolvedAttributes`: presence by key, arguments off the construction.

module AttributeDecode =

    [<Struct>]
    type ClassAttributeVerdict =
        {
            IsSealed: bool
            AllowNullLiteral: bool
            IsValueType: bool
            IsByRefLike: bool
        }

        static member Default =
            {
                IsSealed = false
                AllowNullLiteral = false
                IsValueType = false
                IsByRefLike = false
            }

    let private constructionExpr (oc: ObjectConstruction<SyntaxToken>) : Expr<SyntaxToken> voption =
        match oc with
        | ObjectConstruction(_, e) -> ValueSome e
        | InterfaceConstruction _ -> ValueNone

    let rec private stripParens (e: Expr<SyntaxToken>) =
        match e with
        | Expr.EnclosedBlock(_, inner, _) -> stripParens inner
        | _ -> e

    /// Text of a parsed string-literal expression. Ignores expression holes and other
    /// interpolation artefacts: a compiled-name argument is never interpolated.
    let private stringExprText
        (nameOf: SyntaxToken -> string)
        (parts: System.Collections.Immutable.ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for p in parts do
            match p with
            // Source-level text, escapes and all: decoding them is the lexer's job and
            // an attribute argument never needs it.
            | StringPart.Text tok
            | StringPart.EscapeSequence tok -> sb.Append(nameOf tok) |> ignore
            | _ -> ()

        sb.ToString()

    /// The text of a string-literal argument; `ValueNone` when the argument is not one.
    let private stringArgText (nameOf: SyntaxToken -> string) (e: Expr<SyntaxToken>) : string voption =
        match stripParens e with
        | Expr.String(_, parts, _) -> ValueSome(stringExprText nameOf parts)
        | Expr.Const(Constant.Literal tok) -> ValueSome((nameOf tok).Trim([| '"' |]))
        | _ -> ValueNone

    /// The text of an attribute's single string-literal argument. `ValueNone` when the
    /// construction takes no argument or its argument is not a string literal.
    let tryStringArgument (nameOf: SyntaxToken -> string) (oc: ObjectConstruction<SyntaxToken>) : string voption =
        constructionExpr oc |> ValueOption.bind (stringArgText nameOf)

    /// The name a declaration emits under, read from the `[<CompiledName("Foo")>]` it
    /// carries; `source` is the name its source writes. `ValueNone` where the attribute is
    /// absent, its argument is empty, or it restates `source`.
    let compiledNameOf
        (nameOf: SyntaxToken -> string)
        (source: string)
        (attrs: ResolvedAttributes)
        : CompiledName voption =
        match
            attrs.TryFind RuntimeNames.compiledNameAttributeKey
            |> ValueOption.bind constructionExpr
            |> ValueOption.bind (stringArgText nameOf)
        with
        | ValueNone
        | ValueSome "" -> ValueNone
        | ValueSome compiled -> CompiledName.OfPair(source, compiled)

    /// The `[<Import(selector, path)>]` a binding carries.
    let tryImport (nameOf: SyntaxToken -> string) (attrs: ResolvedAttributes) : ImportDecl =
        match attrs.TryFind RuntimeNames.importAttributeKey with
        | ValueNone -> ImportDecl.NoImport
        | ValueSome construction ->
            match constructionExpr construction |> ValueOption.map stripParens with
            | ValueSome(Expr.Tuple(exprs, _)) when exprs.Length = 2 ->
                match stringArgText nameOf exprs.[0], stringArgText nameOf exprs.[1] with
                | ValueSome selector, ValueSome path when selector <> "" && path <> "" ->
                    ImportDecl.Import { Selector = selector; Path = path }
                | _ -> ImportDecl.Malformed
            | _ -> ImportDecl.Malformed

    /// True iff the attributes carry
    /// `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`, which
    /// pins a module's compiled name to `<name>Module`.
    let hasModuleSuffix (nameOf: SyntaxToken -> string) (attrs: ResolvedAttributes) : bool =
        match
            attrs.TryFind RuntimeNames.compilationRepresentationAttributeKey
            |> ValueOption.bind constructionExpr
        with
        | ValueNone -> false
        | ValueSome argExpr ->
            let shortName (li: LongIdent<SyntaxToken>) =
                match li.Idents.Length with
                | 0 -> ""
                | n -> nameOf li.Idents.[n - 1]

            // The flags are an enum this compiler does not fold, so the written name of the
            // one flag that matters is what is read.
            match stripParens argExpr with
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li)
            | Expr.DotLookup(_, _, LongIdentOrOp.LongIdent li) -> shortName li = "ModuleSuffix"
            | _ -> false

    /// True iff the module-level attributes carry `[<AutoOpen>]`, so its members are in
    /// scope unqualified for a consumer.
    let isAutoOpen (attrs: ResolvedAttributes) : bool =
        attrs.Has RuntimeNames.autoOpenAttributeKey

    /// True iff the WRITTEN attribute name is `AutoOpen`, under F#'s `Attribute`-suffix rule.
    let isWrittenAutoOpen (nameOf: SyntaxToken -> string) (li: LongIdent<SyntaxToken>) : bool =
        match li.Idents.Length with
        | 0 -> false
        | n ->
            match nameOf li.Idents.[n - 1] with
            | "AutoOpen" -> true
            | written -> written = "AutoOpen" + RuntimeNames.AttributeSuffix

    /// The type reference an attribute's construction writes, `ValueNone` when the CST
    /// carries none.
    let writtenTypeRef (construction: ObjectConstruction<SyntaxToken>) : CstKeys.TypeRef voption =
        match construction with
        | ObjectConstruction(typ = t)
        | InterfaceConstruction(typ = t) -> CstKeys.ofTypeRef t

    let decodeClassAttributes (attrs: ResolvedAttributes) : ClassAttributeVerdict =
        let isByRefLike = attrs.Has RuntimeNames.isByRefLikeAttributeKey

        {
            IsSealed = attrs.Has RuntimeNames.sealedAttributeKey
            AllowNullLiteral = attrs.Has RuntimeNames.allowNullLiteralAttributeKey
            // `[<IsByRefLike>]` alone implies a value type. A bare `type X = struct … end`
            // carries no attribute, so this flag is not the only path to a value type.
            IsValueType = attrs.Has RuntimeNames.structAttributeKey || isByRefLike
            IsByRefLike = isByRefLike
        }
