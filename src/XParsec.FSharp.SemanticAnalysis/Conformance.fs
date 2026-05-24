namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Sig/impl conformance for a Vesper.Core contract (`.fsi`) and implementation
// (`.fs`) pair — selfhost-handoff P4 / minimal-core-lib-plan "Contract/impl
// drift".
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
// — it is *not* gated on the self-hosting rungs: it runs the moment both files
// parse. v1 compares declaration *presence* and the extern↔intrinsic pairing; it
// does not deep-compare member signatures of nominal types.

module Conformance =

    /// A type declaration as seen in the `.fsi` contract.
    [<RequireQualifiedAccess>]
    type SigShape =
        /// `type X = extern` — a target capability with no Vesper representation.
        | Extern
        /// Any other signature type (abbrev, union, record, interface, …). The
        /// label names the shape for diagnostics only; v1 does not deep-compare
        /// members.
        | Other of label: string

    /// A type declaration as seen in the `.fs` implementation.
    [<RequireQualifiedAccess>]
    type ImplShape =
        /// `type X = (# "repr" #)` — the intrinsic representation of a primitive.
        | Intrinsic of repr: string
        /// Any other implementation type (abbrev, union, record, …).
        | Other of label: string

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

    /// Summarise a parsed signature (`.fsi`) file as its declared types, in
    /// source order. Namespace groups and nested modules are flattened (v1 has no
    /// namespace-/module-scoped types).
    let summariseSig (lexed: Lexed) (input: string) (file: SignatureFile<SyntaxToken>) : (string * SigShape) list =
        let acc = ResizeArray<string * SigShape>()

        let addSig (ts: TypeSignature<SyntaxToken>) =
            let name = typeNameText lexed input (sigTypeName ts)

            if name <> "" then
                acc.Add(name, sigShape ts)

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
    let summariseImpl
        (lexed: Lexed)
        (input: string)
        (file: ImplementationFile<SyntaxToken>)
        : (string * ImplShape) list =
        let acc = ResizeArray<string * ImplShape>()

        for e in CstWalk.implFileElems file do
            match e with
            | ModuleElem.Type defns ->
                for td in defns do
                    match implTypeName td with
                    | ValueSome tn ->
                        let name = typeNameText lexed input tn

                        if name <> "" then
                            acc.Add(name, implShape lexed input td)
                    | ValueNone -> ()
            | _ -> ()

        List.ofSeq acc

    /// Compare a signature summary against an implementation summary. Returns the
    /// conformance errors in a deterministic order: sig-side findings (missing
    /// impl, extern/intrinsic mismatch) in signature source order, then
    /// impl-only types in implementation source order.
    let check (sigDecls: (string * SigShape) list) (implDecls: (string * ImplShape) list) : ConformanceError list =
        // First declaration of a name wins; a duplicate within a file is its own
        // (separately diagnosed) error and must not mask the conformance result.
        let sigMap = Dictionary<string, SigShape>()

        for (n, s) in sigDecls do
            if not (sigMap.ContainsKey n) then
                sigMap.[n] <- s

        let implMap = Dictionary<string, ImplShape>()

        for (n, s) in implDecls do
            if not (implMap.ContainsKey n) then
                implMap.[n] <- s

        let errors = ResizeArray<ConformanceError>()
        let seenSig = HashSet<string>()

        for (name, sShape) in sigDecls do
            if seenSig.Add name then
                match implMap.TryGetValue name with
                | false, _ -> errors.Add(ConformanceError.MissingInImpl name)
                | true, iShape ->
                    match sShape, iShape with
                    | SigShape.Extern, ImplShape.Intrinsic _ -> ()
                    | SigShape.Extern, ImplShape.Other _ -> errors.Add(ConformanceError.ExternWithoutIntrinsic name)
                    | SigShape.Other _, ImplShape.Intrinsic _ ->
                        errors.Add(ConformanceError.IntrinsicWithoutExtern name)
                    | SigShape.Other _, ImplShape.Other _ -> ()

        let seenImpl = HashSet<string>()

        for (name, _) in implDecls do
            if seenImpl.Add name then
                if not (sigMap.ContainsKey name) then
                    errors.Add(ConformanceError.MissingInSig name)

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
