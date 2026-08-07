namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Conformance of a `.fsi` contract against its `.fs` implementation, over the parsed
// CSTs: every `type X = extern` is met by a `type X = (# "repr" #)` and vice versa, and
// every other declared type and `val` is present on both sides. Presence, not signatures.

module Conformance =

    /// A type declaration as seen in the `.fsi` contract.
    [<RequireQualifiedAccess>]
    type SigShape =
        /// `type X = extern` — a target capability with no Vesper representation.
        | Extern
        /// `type X = extern class` — a heritable external base, paired with the impl's
        /// `(# class "repr" #)`.
        | ExternClass
        /// `type X = Y` — a transparent abbreviation. F# resolves it transitively to its
        /// target, so a sig-only abbreviation (`ref = Ref<'T>`) is conformant with no
        /// `.fs` companion of its own.
        | Abbrev
        /// `type X = | C = v | …`, conforming as a plain nominal type does: no
        /// extern/intrinsic pairing.
        | Enum
        /// Any other signature type (union, record, interface, …) — a concrete type that
        /// does require an implementation. The label is for diagnostics.
        | Other of label: string

    /// A type declaration as seen in the `.fs` implementation.
    [<RequireQualifiedAccess>]
    type ImplShape =
        /// `type X = (# "repr" #)` — the intrinsic representation of a primitive.
        | Intrinsic of repr: string
        /// `type X = (# class "repr" #)` — a heritable external base, paired with the
        /// sig's `extern class`.
        | IntrinsicClass of repr: string
        /// `type X = | C = v | …`.
        | Enum
        /// Any other implementation type (abbrev, union, record, …).
        | Other of label: string

    [<Struct; NoEquality; NoComparison>]
    type SigDecl =
        {
            Name: string
            Shape: SigShape
            NameKey: NodeKey
        }

    [<Struct; NoEquality; NoComparison>]
    type ImplDecl =
        {
            Name: string
            Shape: ImplShape
            NameKey: NodeKey
        }

    [<RequireQualifiedAccess>]
    type ConformanceError =
        /// A concrete type (union/record/class/…) declared in the `.fsi` but not defined
        /// in the `.fs` — the FS0240 analogue. A transparent `Abbrev` is exempt.
        | MissingInImpl of name: string
        /// `extern` in the signature but the implementation provides no `(# … #)`
        /// intrinsic representation (a capability promised with nothing behind it).
        | ExternWithoutIntrinsic of name: string
        /// `(# … #)` intrinsic in the implementation but the signature does not
        /// declare it `extern` (a representation with no declared capability).
        | IntrinsicWithoutExtern of name: string
        /// The heritability tag disagrees across the pair: one side marks the type a
        /// heritable external base (`extern class` / `(# class … #)`) and the other an
        /// opaque value repr (`extern` / `(# … #)`).
        | HeritabilityMismatch of name: string
        /// A module-level `val` declared in the `.fsi` with no corresponding `let` in the
        /// `.fs` — the value-granularity FS0240 analogue. The converse is not reported:
        /// F# hides an impl value the signature omits, so a private helper is not drift.
        | ValueMissingInImpl of name: string

    let describe (e: ConformanceError) : string =
        match e with
        | ConformanceError.MissingInImpl n ->
            sprintf "type '%s' is declared in the signature (.fsi) but not defined in the implementation (.fs)" n
        | ConformanceError.ExternWithoutIntrinsic n ->
            sprintf
                "type '%s' is declared 'extern' in the signature (.fsi) but the implementation (.fs) provides no intrinsic representation"
                n
        | ConformanceError.IntrinsicWithoutExtern n ->
            sprintf
                "type '%s' has an intrinsic representation in the implementation (.fs) but is not declared 'extern' in the signature (.fsi)"
                n
        | ConformanceError.HeritabilityMismatch n ->
            sprintf
                "type '%s' disagrees on heritability across the pair: one side marks it a heritable external base ('extern class' / '(# class … #)'), the other an opaque value repr"
                n
        | ConformanceError.ValueMissingInImpl n ->
            sprintf "value '%s' is declared in the signature (.fsi) but not defined in the implementation (.fs)" n

    let private nameOfTok (lexed: Lexed) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular iT -> lexed.GetTokenString(iT)
        | TokenIndex.Virtual -> ""

    /// A type declaration names a single ident, so its last segment is the short name.
    let private typeNameText (lexed: Lexed) (tn: TypeName<SyntaxToken>) : string =
        let (TypeName(ident = li)) = tn

        if li.Idents.Length = 0 then
            ""
        else
            nameOfTok lexed li.Idents.[li.Idents.Length - 1]

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS:
    /// `(# "System.Int32" #)` → `"System.Int32"`.
    let private ilReprString (lexed: Lexed) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(nameOfTok lexed t) |> ignore
            | StringPart.Expr _ -> ()

        sb.ToString()

    let private sigTypeName (ts: TypeSignature<SyntaxToken>) : TypeName<SyntaxToken> =
        match ts with
        | TypeSignature.Abbrev(typeName = tn)
        | TypeSignature.Record(typeName = tn)
        | TypeSignature.Union(typeName = tn)
        | TypeSignature.Anon(typeName = tn)
        | TypeSignature.Class(typeName = tn)
        | TypeSignature.Struct(typeName = tn)
        | TypeSignature.Interface(typeName = tn)
        | TypeSignature.Enum(typeName = tn)
        | TypeSignature.Delegate(typeName = tn)
        | TypeSignature.TypeExtension(typeName = tn)
        | TypeSignature.Extern(typeName = tn)
        | TypeSignature.AbstractType(typeName = tn) -> tn

    let private sigShape (ts: TypeSignature<SyntaxToken>) : SigShape =
        match ts with
        | TypeSignature.Extern(kindTag = ValueSome(ExternKind.Class _)) -> SigShape.ExternClass
        // `extern interface with …` pairs with the UNTAGGED `(# "System.IDisposable" #)`:
        // a platform interface identity is an opaque value repr, not a heritable base.
        | TypeSignature.Extern _ -> SigShape.Extern
        | TypeSignature.Abbrev _ -> SigShape.Abbrev
        | TypeSignature.Record _ -> SigShape.Other "record"
        | TypeSignature.Union _ -> SigShape.Other "union"
        | TypeSignature.Anon _ -> SigShape.Other "object-model"
        | TypeSignature.Class _ -> SigShape.Other "class"
        | TypeSignature.Struct _ -> SigShape.Other "struct"
        | TypeSignature.Interface _ -> SigShape.Other "interface"
        | TypeSignature.Enum _ -> SigShape.Enum
        | TypeSignature.Delegate _ -> SigShape.Other "delegate"
        | TypeSignature.TypeExtension _ -> SigShape.Other "type-extension"
        | TypeSignature.AbstractType _ -> SigShape.Other "abstract"

    /// `ValueNone` for `TypeDefn` shapes that carry no name (parse failures).
    let private implTypeName (td: TypeDefn<SyntaxToken>) : TypeName<SyntaxToken> voption =
        match td with
        | TypeDefn.Abbrev(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.Delegate(typeName = tn)
        | TypeDefn.TypeExtension(typeName = tn)
        | TypeDefn.AbstractType(typeName = tn) -> ValueSome tn
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ValueNone

    let private implShape (lexed: Lexed) (td: TypeDefn<SyntaxToken>) : ImplShape =
        match td with
        // An abbrev whose RHS is `(# … #)` binds a primitive; it is not a transparent alias.
        | TypeDefn.Abbrev(typ = Type.ILIntrinsic(kindTag = ValueSome _; instrParts = parts)) ->
            ImplShape.IntrinsicClass(ilReprString lexed parts)
        | TypeDefn.Abbrev(typ = Type.ILIntrinsic(instrParts = parts)) -> ImplShape.Intrinsic(ilReprString lexed parts)
        | TypeDefn.Abbrev _ -> ImplShape.Other "abbrev"
        | TypeDefn.Record _ -> ImplShape.Other "record"
        | TypeDefn.Union _ -> ImplShape.Other "union"
        | TypeDefn.Anon _ -> ImplShape.Other "object-model"
        | TypeDefn.Class _ -> ImplShape.Other "class"
        | TypeDefn.Struct _ -> ImplShape.Other "struct"
        | TypeDefn.Interface _ -> ImplShape.Other "interface"
        | TypeDefn.Enum _ -> ImplShape.Enum
        | TypeDefn.Delegate _ -> ImplShape.Other "delegate"
        | TypeDefn.TypeExtension _ -> ImplShape.Other "type-extension"
        | TypeDefn.AbstractType _ -> ImplShape.Other "abstract"
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ImplShape.Other "invalid"

    /// `NodeKey` of a type's last-segment name token; the zero key when the `TypeName`
    /// has no idents at all (a parse failure).
    let private nameKeyOf (tn: TypeName<SyntaxToken>) : NodeKey =
        let (TypeName(ident = li)) = tn

        if li.Idents.Length = 0 then
            NodeKey(0UL)
        else
            NodeKey.ofToken li.Idents.[li.Idents.Length - 1] NodeKind.DeclType

    /// Summarise a parsed signature (`.fsi`) file as its declared types, in source
    /// order. Namespace groups and nested modules are flattened.
    let summariseSig (lexed: Lexed) (file: SignatureFile<SyntaxToken>) : SigDecl list =
        let acc = ResizeArray<SigDecl>()

        let addSig (ts: TypeSignature<SyntaxToken>) =
            let tn = sigTypeName ts
            let name = typeNameText lexed tn

            if name <> "" then
                acc.Add
                    {
                        Name = name
                        Shape = sigShape ts
                        NameKey = nameKeyOf tn
                    }

        for e in CstWalk.sigFileElems file do
            match e with
            | ModuleSignatureElement.Type(_, TypeSignatures(first, rest)) ->
                addSig first

                for (_, ts) in rest do
                    addSig ts
            | _ -> ()

        List.ofSeq acc

    /// Summarise a parsed implementation (`.fs`) file as its defined types, in
    /// source order. Namespace groups and nested modules are flattened.
    let summariseImpl (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : ImplDecl list =
        let acc = ResizeArray<ImplDecl>()

        for e in CstWalk.implFileElems file do
            match e with
            | ModuleElem.Type defns ->
                for td in defns do
                    match implTypeName td with
                    | ValueSome tn ->
                        let name = typeNameText lexed tn

                        if name <> "" then
                            acc.Add
                                {
                                    Name = name
                                    Shape = implShape lexed td
                                    NameKey = nameKeyOf tn
                                }
                    | ValueNone -> ()
            | _ -> ()

        List.ofSeq acc

    /// Compare a signature summary against an implementation summary. Sig-side findings
    /// come first, in signature source order, then impl-only types in impl source order.
    let check (sigDecls: SigDecl list) (implDecls: ImplDecl list) : ConformanceError list =
        // First declaration of a name wins, so a duplicate cannot mask the result.
        let sigMap = Dictionary<string, SigShape>()

        for d in sigDecls do
            if not (sigMap.ContainsKey d.Name) then
                sigMap.[d.Name] <- d.Shape

        let implMap = Dictionary<string, ImplShape>()

        for d in implDecls do
            if not (implMap.ContainsKey d.Name) then
                implMap.[d.Name] <- d.Shape

        let errors = ResizeArray<ConformanceError>()
        let seenSig = HashSet<string>()

        for d in sigDecls do
            if seenSig.Add d.Name then
                match implMap.TryGetValue d.Name with
                | false, _ ->
                    match d.Shape with
                    | SigShape.Abbrev -> ()
                    | _ -> errors.Add(ConformanceError.MissingInImpl d.Name)
                | true, iShape ->
                    // `Enum` neither demands nor supplies an intrinsic, so it groups
                    // with `Other` on both sides.
                    match d.Shape, iShape with
                    | SigShape.Extern, ImplShape.Intrinsic _
                    | SigShape.ExternClass, ImplShape.IntrinsicClass _ -> ()
                    | SigShape.Extern, ImplShape.IntrinsicClass _
                    | SigShape.ExternClass, ImplShape.Intrinsic _ ->
                        errors.Add(ConformanceError.HeritabilityMismatch d.Name)
                    | (SigShape.Extern | SigShape.ExternClass), (ImplShape.Other _ | ImplShape.Enum) ->
                        errors.Add(ConformanceError.ExternWithoutIntrinsic d.Name)
                    // The sig understates a repr the contract should have declared
                    // `extern`: `type foo = int` in the `.fsi`, `(# … #)` in the `.fs`.
                    | (SigShape.Other _ | SigShape.Abbrev | SigShape.Enum),
                      (ImplShape.Intrinsic _ | ImplShape.IntrinsicClass _) ->
                        errors.Add(ConformanceError.IntrinsicWithoutExtern d.Name)
                    | (SigShape.Other _ | SigShape.Abbrev | SigShape.Enum), (ImplShape.Other _ | ImplShape.Enum) -> ()

        let seenImpl = HashSet<string>()

        // A plain impl type absent from the sig is hidden by F#, not drift. An impl
        // `(# … #)` with no `extern` is a repr the contract never declares — reported.
        for d in implDecls do
            if seenImpl.Add d.Name then
                if not (sigMap.ContainsKey d.Name) then
                    match d.Shape with
                    | ImplShape.Intrinsic _
                    | ImplShape.IntrinsicClass _ -> errors.Add(ConformanceError.IntrinsicWithoutExtern d.Name)
                    | ImplShape.Other _
                    | ImplShape.Enum -> ()

        List.ofSeq errors

    // ---- Value-binding presence -------------------------------------------------
    // Module-level `val`/`let` NAMES only — comparing written signatures would flag false
    // drift, as `.fsi` and `.fs` legally differ (`'a list` vs `List<'a>`) until resolved.

    [<Struct; NoEquality; NoComparison>]
    type ValDecl = { Name: string; NameKey: NodeKey }

    /// The raw source spelling of a binding head (`+`, not `op_Addition`): the `.fsi`
    /// `val` and `.fs` `let` spell an operator identically, so it matches across sides.
    /// `ValueNone` for active-pattern heads, whose compiled names are non-trivial.
    let private identOrOpRaw (lexed: Lexed) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed tok)
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp op, _) -> ValueSome(nameOfTok lexed op)
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDot _), _) -> ValueSome ".."
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDotDotDot _), _) -> ValueSome ".. .."
        | IdentOrOp.ParenOp(_, OpName.NilOp _, _) -> ValueSome "[]"
        | IdentOrOp.ParenOp(_, OpName.ActivePatternOp _, _) -> ValueNone

    /// The bound name of a `let` head pattern, unwrapping `Pat.EnclosedBlock` and
    /// `Pat.Typed`. An operator head applied to arguments (`let (+) a b`) is a
    /// `Pat.OpNamed` and yields `ValueNone`.
    let rec private patHeadName (lexed: Lexed) (p: Pat<SyntaxToken>) : (string * SyntaxToken) voption =
        match p with
        | Pat.NamedSimple ident -> ValueSome(nameOfTok lexed ident, ident)
        | Pat.Named(longIdent = li) when li.Idents.Length > 0 ->
            let t = li.Idents.[li.Idents.Length - 1]
            ValueSome(nameOfTok lexed t, t)
        | Pat.Op io ->
            match identOrOpRaw lexed io with
            | ValueSome n -> ValueSome(n, identOrOpHeadTok io)
            | ValueNone -> ValueNone
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner) -> patHeadName lexed inner
        | _ -> ValueNone

    and private identOrOpHeadTok (io: IdentOrOp<SyntaxToken>) : SyntaxToken =
        match io with
        | IdentOrOp.Ident tok -> tok
        | IdentOrOp.ParenOp(lParen = lp) -> lp

    /// Summarise a parsed signature (`.fsi`) as its module-level `val` bindings (incl.
    /// `[<Literal>]` vals), in source order, flattened across nested modules.
    let summariseSigVals (lexed: Lexed) (file: SignatureFile<SyntaxToken>) : ValDecl list =
        let acc = ResizeArray<ValDecl>()

        let addName (name: string) (keyTok: SyntaxToken) =
            if name <> "" then
                acc.Add
                    {
                        Name = name
                        NameKey = NodeKey.ofToken keyTok NodeKind.DeclLetBinding
                    }

        for e in CstWalk.sigFileElems file do
            match e with
            | ModuleSignatureElement.Val(ValSig(ident = io)) ->
                match identOrOpRaw lexed io with
                | ValueSome n -> addName n (identOrOpHeadTok io)
                | ValueNone -> ()
            | ModuleSignatureElement.ValLiteral(binding = b) ->
                match patHeadName lexed b.headPat with
                | ValueSome(n, t) -> addName n t
                | ValueNone -> ()
            | _ -> ()

        List.ofSeq acc

    /// Summarise a parsed implementation (`.fs`) as its module-level `let` bindings,
    /// in source order, flattened across nested modules.
    let summariseImplVals (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : ValDecl list =
        let acc = ResizeArray<ValDecl>()

        for e in CstWalk.implFileElems file do
            match e with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) ->
                for b in bs do
                    match patHeadName lexed b.headPat with
                    | ValueSome(n, t) when n <> "" ->
                        acc.Add
                            {
                                Name = n
                                NameKey = NodeKey.ofToken t NodeKind.DeclLetBinding
                            }
                    | _ -> ()
            | _ -> ()

        List.ofSeq acc

    /// Check that every `.fsi` `val` has a matching `.fs` `let` of the same name.
    /// Errors come in signature source order, one per name.
    let checkValuePresence (sigVals: ValDecl list) (implVals: ValDecl list) : ConformanceError list =
        let implNames = HashSet<string>()

        for v in implVals do
            implNames.Add v.Name |> ignore

        let errors = ResizeArray<ConformanceError>()
        let seen = HashSet<string>()

        for v in sigVals do
            if seen.Add v.Name then
                if not (implNames.Contains v.Name) then
                    errors.Add(ConformanceError.ValueMissingInImpl v.Name)

        List.ofSeq errors

    /// Both checks over a parsed `.fsi` / `.fs` pair, each with its own `Lexed`:
    /// type findings first, then value findings.
    let checkPair
        (sigLexed: Lexed)
        (sigFile: SignatureFile<SyntaxToken>)
        (implLexed: Lexed)
        (implFile: ImplementationFile<SyntaxToken>)
        : ConformanceError list =
        let typeErrors =
            check (summariseSig sigLexed sigFile) (summariseImpl implLexed implFile)

        let valueErrors =
            checkValuePresence (summariseSigVals sigLexed sigFile) (summariseImplVals implLexed implFile)

        typeErrors @ valueErrors
