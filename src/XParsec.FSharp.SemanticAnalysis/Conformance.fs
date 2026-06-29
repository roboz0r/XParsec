namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Sig/impl conformance for a Vesper.Core contract (`.fsi`) and implementation
// (`.fs`) pair.
//
// The contract declares each target primitive `type X = extern` — "the target
// provides this; there is no Vesper representation here." That set of `extern`
// declarations is the target's *primitive capability set*. The implementation
// pairs each with a `type X = (# "repr" #)` intrinsic carrying the
// *representation*. Conformance requires the two coincide: every `extern` has an
// intrinsic and every intrinsic has an `extern`; every other declared type is
// present on both sides.
//
// This is a source-level check (it compares the parsed CSTs of the `.fsi` and
// `.fs`), so — unlike a reflection round-trip over a compiled `Vesper.Core.dll`
// — it runs the moment both files parse. v1 compares declaration *presence* and
// the extern↔intrinsic pairing; it does not deep-compare member signatures of
// nominal types.

module Conformance =

    /// A type declaration as seen in the `.fsi` contract.
    [<RequireQualifiedAccess>]
    type SigShape =
        /// `type X = extern` — a target capability with no Vesper representation.
        | Extern
        /// `type X = extern class` — a HERITABLE external reference base (paired with
        /// the impl's `(# class "repr" #)`); distinct from the opaque `Extern` so the
        /// two species do not cross-pair.
        | ExternClass
        /// Any other signature type (abbrev, union, record, interface, …). The
        /// label names the shape for diagnostics only; v1 does not deep-compare
        /// members.
        | Other of label: string

    /// A type declaration as seen in the `.fs` implementation.
    [<RequireQualifiedAccess>]
    type ImplShape =
        /// `type X = (# "repr" #)` — the intrinsic representation of a primitive.
        | Intrinsic of repr: string
        /// `type X = (# class "repr" #)` — a HERITABLE external reference base (paired
        /// with the sig's `extern class`); distinct from the opaque `Intrinsic`.
        | IntrinsicClass of repr: string
        /// Any other implementation type (abbrev, union, record, …).
        | Other of label: string

    /// One declaration extracted from a signature (`.fsi`) file.
    ///
    /// `NameKey` points at the type's name token so future class-equivalence
    /// drift can range-attach a diagnostic; v1 only consumes `Name` + `Shape`.
    [<Struct; NoEquality; NoComparison>]
    type SigDecl =
        {
            Name: string
            Shape: SigShape
            NameKey: NodeKey
        }

    /// One declaration extracted from an implementation (`.fs`) file. See
    /// [`SigDecl`](#SigDecl).
    [<Struct; NoEquality; NoComparison>]
    type ImplDecl =
        {
            Name: string
            Shape: ImplShape
            NameKey: NodeKey
        }

    [<RequireQualifiedAccess>]
    type ConformanceError =
        /// Declared in the signature (`.fsi`) but not defined in the
        /// implementation (`.fs`).
        | MissingInImpl of name: string
        /// Defined in the implementation (`.fs`) but not declared in the
        /// signature (`.fsi`).
        | MissingInSig of name: string
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

    /// Human-readable rendering of a conformance error.
    let describe (e: ConformanceError) : string =
        match e with
        | ConformanceError.MissingInImpl n ->
            sprintf "type '%s' is declared in the signature (.fsi) but not defined in the implementation (.fs)" n
        | ConformanceError.MissingInSig n ->
            sprintf "type '%s' is defined in the implementation (.fs) but not declared in the signature (.fsi)" n
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

    let private nameOfTok (lexed: Lexed) (input: string) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular iT -> lexed.GetTokenString(iT, input)
        | TokenIndex.Virtual -> ""

    /// The declared (short) name of a type from its `TypeName`. A type
    /// declaration always names a single ident, so the last segment is the name.
    let private typeNameText (lexed: Lexed) (input: string) (tn: TypeName<SyntaxToken>) : string =
        let (TypeName(ident = li)) = tn

        if li.Idents.Length = 0 then
            ""
        else
            nameOfTok lexed input li.Idents.[li.Idents.Length - 1]

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS
    /// (`(# "System.Int32" #)` → `"System.Int32"`). Mirrors
    /// `NameResolution.ilIntrinsicString`.
    let private ilReprString (lexed: Lexed) (input: string) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(nameOfTok lexed input t) |> ignore
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
        | TypeSignature.Extern(kindTag = ValueSome _) -> SigShape.ExternClass
        | TypeSignature.Extern _ -> SigShape.Extern
        | TypeSignature.Abbrev _ -> SigShape.Other "abbrev"
        | TypeSignature.Record _ -> SigShape.Other "record"
        | TypeSignature.Union _ -> SigShape.Other "union"
        | TypeSignature.Anon _ -> SigShape.Other "object-model"
        | TypeSignature.Class _ -> SigShape.Other "class"
        | TypeSignature.Struct _ -> SigShape.Other "struct"
        | TypeSignature.Interface _ -> SigShape.Other "interface"
        | TypeSignature.Enum _ -> SigShape.Other "enum"
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

    let private implShape (lexed: Lexed) (input: string) (td: TypeDefn<SyntaxToken>) : ImplShape =
        match td with
        // The intrinsic-impl rule: an abbrev whose RHS is `(# … #)` is a
        // primitive *binding*, not a transparent alias (see
        // NameResolution.registerAbbreviationDefn).
        | TypeDefn.Abbrev(typ = Type.ILIntrinsic(kindTag = ValueSome _; instrParts = parts)) ->
            ImplShape.IntrinsicClass(ilReprString lexed input parts)
        | TypeDefn.Abbrev(typ = Type.ILIntrinsic(instrParts = parts)) ->
            ImplShape.Intrinsic(ilReprString lexed input parts)
        | TypeDefn.Abbrev _ -> ImplShape.Other "abbrev"
        | TypeDefn.Record _ -> ImplShape.Other "record"
        | TypeDefn.Union _ -> ImplShape.Other "union"
        | TypeDefn.Anon _ -> ImplShape.Other "object-model"
        | TypeDefn.Class _ -> ImplShape.Other "class"
        | TypeDefn.Struct _ -> ImplShape.Other "struct"
        | TypeDefn.Interface _ -> ImplShape.Other "interface"
        | TypeDefn.Enum _ -> ImplShape.Other "enum"
        | TypeDefn.Delegate _ -> ImplShape.Other "delegate"
        | TypeDefn.TypeExtension _ -> ImplShape.Other "type-extension"
        | TypeDefn.AbstractType _ -> ImplShape.Other "abstract"
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ImplShape.Other "invalid"

    /// `NodeKey` of a type's last-segment name token; the zero key when the
    /// `TypeName` has no idents (parse failure — caller filters empty names
    /// from the summary, so this branch is unreachable in well-formed input).
    let private nameKeyOf (tn: TypeName<SyntaxToken>) : NodeKey =
        let (TypeName(ident = li)) = tn

        if li.Idents.Length = 0 then
            NodeKey(0UL)
        else
            NodeKey.ofToken li.Idents.[li.Idents.Length - 1] NodeKind.DeclType

    /// Summarise a parsed signature (`.fsi`) file as its declared types, in
    /// source order. Namespace groups and nested modules are flattened (v1 has no
    /// namespace-/module-scoped types).
    let summariseSig (lexed: Lexed) (input: string) (file: SignatureFile<SyntaxToken>) : SigDecl list =
        let acc = ResizeArray<SigDecl>()

        let addSig (ts: TypeSignature<SyntaxToken>) =
            let tn = sigTypeName ts
            let name = typeNameText lexed input tn

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
    let summariseImpl (lexed: Lexed) (input: string) (file: ImplementationFile<SyntaxToken>) : ImplDecl list =
        let acc = ResizeArray<ImplDecl>()

        for e in CstWalk.implFileElems file do
            match e with
            | ModuleElem.Type defns ->
                for td in defns do
                    match implTypeName td with
                    | ValueSome tn ->
                        let name = typeNameText lexed input tn

                        if name <> "" then
                            acc.Add
                                {
                                    Name = name
                                    Shape = implShape lexed input td
                                    NameKey = nameKeyOf tn
                                }
                    | ValueNone -> ()
            | _ -> ()

        List.ofSeq acc

    /// Compare a signature summary against an implementation summary. Returns the
    /// conformance errors in a deterministic order: sig-side findings (missing
    /// impl, extern/intrinsic mismatch) in signature source order, then
    /// impl-only types in implementation source order.
    let check (sigDecls: SigDecl list) (implDecls: ImplDecl list) : ConformanceError list =
        // First declaration of a name wins; a duplicate within a file is its own
        // (separately diagnosed) error and must not mask the conformance result.
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
                | false, _ -> errors.Add(ConformanceError.MissingInImpl d.Name)
                | true, iShape ->
                    match d.Shape, iShape with
                    | SigShape.Extern, ImplShape.Intrinsic _
                    | SigShape.ExternClass, ImplShape.IntrinsicClass _ -> ()
                    // extern-family ↔ intrinsic-family, but the heritability tag differs.
                    | SigShape.Extern, ImplShape.IntrinsicClass _
                    | SigShape.ExternClass, ImplShape.Intrinsic _ ->
                        errors.Add(ConformanceError.HeritabilityMismatch d.Name)
                    | (SigShape.Extern | SigShape.ExternClass), ImplShape.Other _ ->
                        errors.Add(ConformanceError.ExternWithoutIntrinsic d.Name)
                    | SigShape.Other _, (ImplShape.Intrinsic _ | ImplShape.IntrinsicClass _) ->
                        errors.Add(ConformanceError.IntrinsicWithoutExtern d.Name)
                    | SigShape.Other _, ImplShape.Other _ -> ()

        let seenImpl = HashSet<string>()

        for d in implDecls do
            if seenImpl.Add d.Name then
                if not (sigMap.ContainsKey d.Name) then
                    errors.Add(ConformanceError.MissingInSig d.Name)

        List.ofSeq errors

    /// Convenience over `summariseSig` + `summariseImpl` + `check` for a parsed
    /// `.fsi` / `.fs` pair (each with its own `Lexed` + source text).
    let checkPair
        (sigLexed: Lexed)
        (sigInput: string)
        (sigFile: SignatureFile<SyntaxToken>)
        (implLexed: Lexed)
        (implInput: string)
        (implFile: ImplementationFile<SyntaxToken>)
        : ConformanceError list =
        check (summariseSig sigLexed sigInput sigFile) (summariseImpl implLexed implInput implFile)
