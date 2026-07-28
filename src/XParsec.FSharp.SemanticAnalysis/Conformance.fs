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
        /// `type X = Y` — a transparent type ABBREVIATION. F# resolves it transitively
        /// to its (implemented or external) target, so it requires NO `.fs` companion of
        /// its own — a sig-only abbreviation (`ref = Ref<'T>`, `ResizeArray = List<'T>`,
        /// `seq = …`) is conformant, not a `MissingInImpl`. Kept distinct from `Other` so
        /// the presence check exempts it by construction (matching F#'s transparent-alias
        /// resolution), rather than the test pinning it as "expected drift".
        | Abbrev
        /// `type X = | C = v | …` — a numeric / string enum declaration. A
        /// first-class shape (not `Other "enum"`); it conforms exactly as a plain
        /// nominal type does (no extern/intrinsic pairing).
        | Enum
        /// Any other signature type (union, record, interface, …) — a CONCRETE type that
        /// does require an implementation. The label names the shape for diagnostics
        /// only; v1 does not deep-compare members.
        | Other of label: string

    /// A type declaration as seen in the `.fs` implementation.
    [<RequireQualifiedAccess>]
    type ImplShape =
        /// `type X = (# "repr" #)` — the intrinsic representation of a primitive.
        | Intrinsic of repr: string
        /// `type X = (# class "repr" #)` — a HERITABLE external reference base (paired
        /// with the sig's `extern class`); distinct from the opaque `Intrinsic`.
        | IntrinsicClass of repr: string
        /// `type X = | C = v | …` — a numeric / string enum declaration. A
        /// first-class shape (not `Other "enum"`); it conforms exactly as a plain
        /// nominal type does (no extern/intrinsic pairing).
        | Enum
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
        /// A CONCRETE type (union/record/class/…) is declared in the signature (`.fsi`)
        /// but not defined in the implementation (`.fs`) — the FS0240 analogue. A
        /// transparent `Abbrev` (`type X = Y`) is exempt: F# resolves it to its target,
        /// so a sig-only abbreviation needs no companion.
        //
        // NOTE: there is deliberately NO "impl type absent from the sig" error. F# hides
        // an implementation type that the signature omits (a HiddenTycon — a private impl
        // detail like Set's AVL-tree nodes), so it is not drift. An impl `(# … #)` repr
        // with no `extern` IS reported, as `IntrinsicWithoutExtern`.
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
        /// A module-level `val` is declared in the signature (`.fsi`) but no
        /// corresponding `let` is defined in the implementation (`.fs`) — the
        /// value-granularity FS0240 analogue (F#'s "RequiredButNotSpecified"). The
        /// converse (a `let` with no `val`) is NOT reported: F# silently allows an
        /// implementation value absent from the signature (a HiddenVal), so a private
        /// helper `let` is not drift. The signature/typar-ORDER half is the SEMANTIC
        /// `ConformanceTypars` kernel, which runs over the frozen TAST, not the raw CST
        /// this presence check sees.
        | ValueMissingInImpl of name: string

    /// Human-readable rendering of a conformance error.
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
        | TypeSignature.Extern(kindTag = ValueSome(ExternKind.Class _)) -> SigShape.ExternClass
        // `extern interface with …` (a capability interface) pairs with the UNTAGGED impl
        // repr `(# "System.IDisposable" #)` — the platform interface identity is an opaque
        // value repr, not a `(# class #)` heritable base — so it conforms as `Extern`, not
        // `ExternClass`.
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
        | TypeDefn.Enum _ -> ImplShape.Enum
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
                // ABSENT impl: a transparent abbreviation resolves to its target and needs
                // no `.fs` companion (F#'s alias resolution); any other (concrete) sig type
                // requires one (the FS0240 analogue).
                | false, _ ->
                    match d.Shape with
                    | SigShape.Abbrev -> ()
                    | _ -> errors.Add(ConformanceError.MissingInImpl d.Name)
                | true, iShape ->
                    // An enum shape conforms exactly as a plain nominal (`Other`)
                    // does: it neither demands nor supplies an intrinsic, so it is
                    // grouped with `Other` on both sides — preserving the former
                    // `Other "enum"` routing now that the shape is first-class.
                    match d.Shape, iShape with
                    | SigShape.Extern, ImplShape.Intrinsic _
                    | SigShape.ExternClass, ImplShape.IntrinsicClass _ -> ()
                    // extern-family ↔ intrinsic-family, but the heritability tag differs.
                    | SigShape.Extern, ImplShape.IntrinsicClass _
                    | SigShape.ExternClass, ImplShape.Intrinsic _ ->
                        errors.Add(ConformanceError.HeritabilityMismatch d.Name)
                    | (SigShape.Extern | SigShape.ExternClass), (ImplShape.Other _ | ImplShape.Enum) ->
                        errors.Add(ConformanceError.ExternWithoutIntrinsic d.Name)
                    // An abbreviation, an enum, OR a plain `Other` sig paired with an impl intrinsic is
                    // a repr the contract should have declared `extern` — the sig understates
                    // it (`type foo = int` / a union, but the impl is `(# … #)`).
                    | (SigShape.Other _ | SigShape.Abbrev | SigShape.Enum),
                      (ImplShape.Intrinsic _ | ImplShape.IntrinsicClass _) ->
                        errors.Add(ConformanceError.IntrinsicWithoutExtern d.Name)
                    | (SigShape.Other _ | SigShape.Abbrev | SigShape.Enum), (ImplShape.Other _ | ImplShape.Enum) -> ()

        let seenImpl = HashSet<string>()

        // Impl types absent from the signature: a plain type is a HiddenTycon (F# hides
        // it — a private impl detail, NOT drift), so it is not reported. But an impl
        // `(# … #)` intrinsic with no `extern` in the sig is a primitive repr the contract
        // never declares — that IS reported, as `IntrinsicWithoutExtern`.
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
    //
    // Extracts MODULE-LEVEL `val` (signature) / `let` (impl) bindings — flattened
    // across nested modules by `CstWalk` — and checks that every `.fsi` `val` has a
    // matching `.fs` `let`. PRESENCE only: it does NOT compare the signatures or typar
    // order. That half cannot run faithfully on the raw CST — `.fsi`/`.fs` differ
    // legally (`'a list` vs `List<'a>`, `seq<'a>` vs `IEnumerable<'a>`), so a syntactic
    // comparison flags false drift; it runs in the semantic, abbreviation-resolved
    // `FrozenType` layer instead — the `ConformanceTypars` kernel, which compares an
    // extracted `.fsi` scheme against the frozen `.fs` binding.
    //
    // TYPE MEMBERS (members inside a `type`, e.g. `formatter`'s `AppendFormatted`
    // overloads) are NOT extracted here (generic members, cross-package — a later rung).
    // Only `ModuleElem.FunctionOrValue` lets / `ModuleSignatureElement.Val` sigs participate.

    /// One module-level value binding extracted from a `.fsi` or `.fs` file.
    [<Struct; NoEquality; NoComparison>]
    type ValDecl = { Name: string; NameKey: NodeKey }

    /// The RAW source spelling of an `IdentOrOp` binding head (operator token text,
    /// not the `op_*` compiled name) — enough for cross-side PRESENCE matching, since
    /// the `.fsi` `val` and `.fs` `let` spell the same operator identically.
    /// `ValueNone` for active-pattern heads (compiled names are non-trivial; skipped).
    let private identOrOpRaw (lexed: Lexed) (input: string) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed input tok)
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp op, _) -> ValueSome(nameOfTok lexed input op)
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDot _), _) -> ValueSome ".."
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDotDotDot _), _) -> ValueSome ".. .."
        | IdentOrOp.ParenOp(_, OpName.NilOp _, _) -> ValueSome "[]"
        | IdentOrOp.ParenOp(_, OpName.ActivePatternOp _, _) -> ValueNone

    /// The bound name of a `let` binding head pattern (the `.fs` side). Mirrors
    /// `MemberRegistration.memberNameOf`'s walk: a plain name (`Pat.NamedSimple`), an
    /// operator/active-pattern head (`Pat.Op` — NOT `Pat.OpNamed`, which is an
    /// *argument* application pattern), unwrapping `Pat.EnclosedBlock`/`Pat.Typed`.
    /// `identOrOpRaw` keys operators identically to the `.fsi` `val` side.
    let rec private patHeadName (lexed: Lexed) (input: string) (p: Pat<SyntaxToken>) : (string * SyntaxToken) voption =
        match p with
        | Pat.NamedSimple ident -> ValueSome(nameOfTok lexed input ident, ident)
        | Pat.Named(longIdent = li) when li.Idents.Length > 0 ->
            let t = li.Idents.[li.Idents.Length - 1]
            ValueSome(nameOfTok lexed input t, t)
        | Pat.Op io ->
            match identOrOpRaw lexed input io with
            | ValueSome n -> ValueSome(n, identOrOpHeadTok io)
            | ValueNone -> ValueNone
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner) -> patHeadName lexed input inner
        | _ -> ValueNone

    /// A representative token for an `IdentOrOp` head, for `NodeKey` attachment.
    and private identOrOpHeadTok (io: IdentOrOp<SyntaxToken>) : SyntaxToken =
        match io with
        | IdentOrOp.Ident tok -> tok
        | IdentOrOp.ParenOp(lParen = lp) -> lp

    /// Summarise a parsed signature (`.fsi`) as its module-level `val` bindings (incl.
    /// `[<Literal>]` vals), in source order, flattened across nested modules.
    let summariseSigVals (lexed: Lexed) (input: string) (file: SignatureFile<SyntaxToken>) : ValDecl list =
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
                match identOrOpRaw lexed input io with
                | ValueSome n -> addName n (identOrOpHeadTok io)
                | ValueNone -> ()
            | ModuleSignatureElement.ValLiteral(binding = b) ->
                match patHeadName lexed input b.headPat with
                | ValueSome(n, t) -> addName n t
                | ValueNone -> ()
            | _ -> ()

        List.ofSeq acc

    /// Summarise a parsed implementation (`.fs`) as its module-level `let` bindings,
    /// in source order, flattened across nested modules.
    let summariseImplVals (lexed: Lexed) (input: string) (file: ImplementationFile<SyntaxToken>) : ValDecl list =
        let acc = ResizeArray<ValDecl>()

        for e in CstWalk.implFileElems file do
            match e with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) ->
                for b in bs do
                    match patHeadName lexed input b.headPat with
                    | ValueSome(n, t) when n <> "" ->
                        acc.Add
                            {
                                Name = n
                                NameKey = NodeKey.ofToken t NodeKind.DeclLetBinding
                            }
                    | _ -> ()
            | _ -> ()

        List.ofSeq acc

    /// Check that every `.fsi` `val` has a matching `.fs` `let` (by name). The reverse
    /// (impl `let` with no sig `val`) is intentionally NOT reported — F# silently
    /// allows it as a HiddenVal, so a private helper is not drift. Errors are returned
    /// in signature source order (a duplicate name is diagnosed once).
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

    /// Convenience over `summariseSig` + `summariseImpl` + `check` for a parsed
    /// `.fsi` / `.fs` pair (each with its own `Lexed` + source text). Includes the
    /// value-presence check: type findings first, then value findings.
    let checkPair
        (sigLexed: Lexed)
        (sigInput: string)
        (sigFile: SignatureFile<SyntaxToken>)
        (implLexed: Lexed)
        (implInput: string)
        (implFile: ImplementationFile<SyntaxToken>)
        : ConformanceError list =
        let typeErrors =
            check (summariseSig sigLexed sigInput sigFile) (summariseImpl implLexed implInput implFile)

        let valueErrors =
            checkValuePresence
                (summariseSigVals sigLexed sigInput sigFile)
                (summariseImplVals implLexed implInput implFile)

        typeErrors @ valueErrors
