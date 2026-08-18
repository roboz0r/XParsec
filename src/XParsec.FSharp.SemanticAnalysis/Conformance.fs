namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Conformance of a signature file against its implementation file, over the parsed
// CSTs: every `type X = extern` is met by a `type X = (# "repr" #)` and vice versa, and
// every other declared type and `val` is present on both sides. Presence, not signatures.

module Conformance =

    /// A type declaration as seen in the signature file.
    [<RequireQualifiedAccess>]
    type SigShape =
        /// `type X = extern` — a target capability with no Vesper representation.
        | Extern
        /// `type X = extern class` — a heritable external base, paired with the impl's
        /// `(# class "repr" #)`.
        | ExternClass
        /// `type X = extern interface with …` — a capability anchor. Its repr names an
        /// interface, so a target without them binds a sentinel that names nothing.
        | ExternInterface
        /// `type X = Y` — a transparent abbreviation, restated by the `.fs` as fsc requires.
        | Abbrev
        /// `type X = | C = v | …`, conforming as a plain nominal type does: no
        /// extern/intrinsic pairing.
        | Enum
        /// Any other signature type (union, record, interface, …) — a concrete type that
        /// does require an implementation. The label is for diagnostics.
        | Other of label: string

        /// The `extern` family: the `.fs` must answer with a `(# … #)` repr, and the two
        /// sides' heritability must agree.
        member this.DemandsIntrinsic =
            match this with
            | SigShape.Extern
            | SigShape.ExternInterface
            | SigShape.ExternClass -> true
            | SigShape.Abbrev
            | SigShape.Enum
            | SigShape.Other _ -> false

        /// A downstream file may `inherit` it, so the `.fs` must bind `(# class … #)`.
        member this.IsHeritable = this = SigShape.ExternClass

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

        /// `ValueSome heritable` when the `.fs` supplies a `(# … #)` repr; `ValueNone`
        /// when it declares an ordinary type, which supplies none.
        member this.SuppliesIntrinsic =
            match this with
            | ImplShape.Intrinsic _ -> ValueSome false
            | ImplShape.IntrinsicClass _ -> ValueSome true
            // `Enum` neither demands nor supplies an intrinsic, so it groups with `Other`.
            | ImplShape.Enum
            | ImplShape.Other _ -> ValueNone

    [<Struct; NoEquality; NoComparison>]
    type SigDecl = { Name: string; Shape: SigShape }

    [<Struct; NoEquality; NoComparison>]
    type ImplDecl = { Name: string; Shape: ImplShape }

    [<RequireQualifiedAccess>]
    type ConformanceError =
        /// A concrete type (union/record/class/…) declared in the `.fsi` but not defined
        /// in the `.fs`, the FS0240 analogue. A transparent `Abbrev` is exempt.
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
        /// `.fs`, the value-granularity FS0240 analogue. The converse is not reported:
        /// F# hides an impl value the signature omits, so a private helper is not drift.
        | ValueMissingInImpl of name: string
        /// An `[<Import>]` binding whose body is not `jsNative` — the attribute is the
        /// implementation, so marking a real body would silently discard it.
        | ImportBodyNotJsNative of name: string
        /// A `jsNative` body with no `[<Import>]` declaring the export that serves it.
        | JsNativeWithoutImport of name: string
        /// The `[<Import>]` selector differs from the binding's emitted name, so the
        /// emitted import would bind a different export than the one declared.
        | ImportSelectorMismatch of name: string * selector: string
        /// An `[<Import>]` whose arguments are not two non-empty string literals.
        | ImportMalformed of name: string
        /// The `[<Import>]` path is not `./` plus a `[core] runtime` asset of the
        /// declaring package, or the named asset file is absent.
        | ImportUnknownAsset of name: string * path: string
        /// The named runtime asset exports no binding of the declared selector.
        | ImportMissingExport of name: string * selector: string * asset: string

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
        | ConformanceError.ImportBodyNotJsNative n ->
            sprintf
                "binding '%s' carries [<Import>], whose implementation is the imported export, so its body must be exactly 'jsNative'"
                n
        | ConformanceError.JsNativeWithoutImport n ->
            sprintf "binding '%s' has body 'jsNative' but no [<Import>] declaring the runtime export that serves it" n
        | ConformanceError.ImportSelectorMismatch(n, selector) ->
            sprintf
                "binding '%s' declares [<Import>] selector '%s'; a reference imports the binding's emitted name, so the selector must equal it"
                n
                selector
        | ConformanceError.ImportMalformed n ->
            sprintf
                "binding '%s' carries an [<Import>] whose arguments are not two non-empty string literals ([<Import(\"selector\", \"./asset.mjs\")>])"
                n
        | ConformanceError.ImportUnknownAsset(n, path) ->
            sprintf
                "binding '%s' imports from '%s', which is not './' plus a '[core] runtime' asset of the declaring package"
                n
                path
        | ConformanceError.ImportMissingExport(n, selector, asset) ->
            sprintf "binding '%s' imports '%s', which '%s' does not export" n selector asset

    /// A type declaration writes a single ident, so its last segment is the short name.
    let private typeNameText (lexed: Lexed) (tn: TypeName<SyntaxToken>) : string =
        let (TypeName(ident = li)) = tn

        if li.Idents.Length = 0 then
            ""
        else
            SyntaxToken.nameIn lexed li.Idents.[li.Idents.Length - 1]

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
        | TypeSignature.Extern(kindTag = ValueSome(ExternKind.Interface _)) -> SigShape.ExternInterface
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
        let repr (parts: ImmutableArray<StringPart<SyntaxToken>>) =
            IntrinsicReprs.ilString (SyntaxToken.nameIn lexed) parts

        match td with
        // An abbrev whose RHS is `(# … #)` binds a primitive; it is not a transparent alias.
        | TypeDefn.Abbrev(typ = Type.ILIntrinsic(kindTag = ValueSome _; instrParts = parts)) ->
            ImplShape.IntrinsicClass(repr parts)
        | TypeDefn.Abbrev(typ = Type.ILIntrinsic(instrParts = parts)) -> ImplShape.Intrinsic(repr parts)
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

    /// Summarise a parsed signature (`.fsi`) file as its declared types, in source
    /// order. Namespace groups and nested modules are flattened.
    let summariseSig (lexed: Lexed) (file: SignatureFile<SyntaxToken>) : SigDecl list =
        let acc = ResizeArray<SigDecl>()

        let addSig (ts: TypeSignature<SyntaxToken>) =
            let tn = sigTypeName ts
            let name = typeNameText lexed tn

            if name <> "" then
                acc.Add { Name = name; Shape = sigShape ts }

        for e in CstModuleTree.sigFileElems file do
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

        for e in CstModuleTree.implFileElems file do
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
                | false, _ -> errors.Add(ConformanceError.MissingInImpl d.Name)
                | true, iShape ->
                    match d.Shape.DemandsIntrinsic, iShape.SuppliesIntrinsic with
                    | true, ValueNone -> errors.Add(ConformanceError.ExternWithoutIntrinsic d.Name)
                    // The sig understates a repr the contract should have declared
                    // `extern`: `type foo = int` in the `.fsi`, `(# … #)` in the `.fs`.
                    | false, ValueSome _ -> errors.Add(ConformanceError.IntrinsicWithoutExtern d.Name)
                    | true, ValueSome implHeritable when implHeritable <> d.Shape.IsHeritable ->
                        errors.Add(ConformanceError.HeritabilityMismatch d.Name)
                    | _ -> ()

        let seenImpl = HashSet<string>()

        // A plain impl type absent from the sig is hidden by F#, not drift. An impl
        // `(# … #)` with no `extern` is a repr the contract never declares, so it is reported.
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
    // Module-level `val`/`let` NAMES only, because comparing written signatures would flag
    // false drift, as `.fsi` and `.fs` legally differ (`'a list` vs `List<'a>`) until resolved.

    /// The raw source spelling of a bound name (`+`, not `op_Addition`): the `.fsi`
    /// `val` and `.fs` `let` spell an operator identically, so it matches across sides.
    /// `ValueNone` for active patterns, whose compiled names are non-trivial.
    let private identOrOpRaw (lexed: Lexed) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.Ident tok -> ValueSome(SyntaxToken.nameIn lexed tok)
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp op, _) -> ValueSome(SyntaxToken.nameIn lexed op)
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDot _), _) -> ValueSome ".."
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDotDotDot _), _) -> ValueSome ".. .."
        | IdentOrOp.ParenOp(_, OpName.NilOp _, _) -> ValueSome "[]"
        | IdentOrOp.ParenOp(_, OpName.ActivePatternOp _, _) -> ValueNone

    /// The name a `let` binding's pattern binds, unwrapping `Pat.EnclosedBlock` and
    /// `Pat.Typed`. An operator applied to arguments (`let (+) a b`) is a
    /// `Pat.OpNamed` and yields `ValueNone`.
    let rec private boundName (lexed: Lexed) (p: Pat<SyntaxToken>) : string voption =
        match p with
        | Pat.NamedSimple ident -> ValueSome(SyntaxToken.nameIn lexed ident)
        | Pat.Named(longIdent = li) when li.Idents.Length > 0 ->
            ValueSome(SyntaxToken.nameIn lexed li.Idents.[li.Idents.Length - 1])
        | Pat.Op io -> identOrOpRaw lexed io
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner) -> boundName lexed inner
        | _ -> ValueNone

    /// Summarise a parsed signature (`.fsi`) as its module-level `val` bindings (incl.
    /// `[<Literal>]` vals), in source order, flattened across nested modules.
    let summariseSigVals (lexed: Lexed) (file: SignatureFile<SyntaxToken>) : string list =
        let acc = ResizeArray<string>()

        let addName (name: string) =
            if name <> "" then
                acc.Add name

        for e in CstModuleTree.sigFileElems file do
            match e with
            | ModuleSignatureElement.Val(ValSig(ident = io)) ->
                match identOrOpRaw lexed io with
                | ValueSome n -> addName n
                | ValueNone -> ()
            | ModuleSignatureElement.ValLiteral(binding = b) ->
                match boundName lexed b.pattern with
                | ValueSome n -> addName n
                | ValueNone -> ()
            | _ -> ()

        List.ofSeq acc

    /// Summarise a parsed implementation (`.fs`) as its module-level `let` bindings,
    /// in source order, flattened across nested modules.
    let summariseImplVals (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : string list =
        let acc = ResizeArray<string>()

        for e in CstModuleTree.implFileElems file do
            match e with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) ->
                for b in bs do
                    match boundName lexed b.pattern with
                    | ValueSome n when n <> "" -> acc.Add n
                    | _ -> ()
            | _ -> ()

        List.ofSeq acc

    // ---- `[<Import>]` bindings ---------------------------------------------------

    /// The body is the bare identifier `jsNative`, parens stripped.
    let rec private isJsNativeBody (lexed: Lexed) (e: Expr<SyntaxToken>) : bool =
        match e with
        | Expr.Ident tok -> SyntaxToken.nameIn lexed tok = "jsNative"
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            SyntaxToken.nameIn lexed li.Idents.[0] = "jsNative"
        | Expr.EnclosedBlock(_, inner, _) -> isJsNativeBody lexed inner
        | _ -> false

    /// A module-level binding carrying a well-formed `[<Import>]`.
    [<Struct; NoEquality; NoComparison>]
    type ImportBinding =
        {
            Name: string
            Ref: AttributeDecode.ImportRef
        }

    /// The `[<Import>]` bindings of an implementation file, in source order, plus every
    /// CST-level finding about them: a malformed attribute, a body other than `jsNative`,
    /// a selector that is not the binding's emitted name, and a `jsNative` body with no
    /// attribute. The path-vs-manifest half of the check is the caller's, which holds the
    /// manifest.
    let summariseImports
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : ImportBinding list * ConformanceError list =
        let imports = ResizeArray<ImportBinding>()
        let errors = ResizeArray<ConformanceError>()
        let nameOf (tok: SyntaxToken) = SyntaxToken.nameIn lexed tok

        for e in CstModuleTree.implFileElems file do
            match e with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) ->
                for b in bs do
                    let name =
                        match boundName lexed b.pattern with
                        | ValueSome n -> n
                        | ValueNone -> ""

                    let emittedName =
                        match AttributeDecode.tryCompiledName nameOf b.attributes with
                        | ValueSome n -> n
                        | ValueNone -> name

                    match AttributeDecode.tryImport nameOf b.attributes with
                    | AttributeDecode.ImportDecl.NoImport ->
                        if isJsNativeBody lexed b.expr then
                            errors.Add(ConformanceError.JsNativeWithoutImport name)
                    | AttributeDecode.ImportDecl.Malformed -> errors.Add(ConformanceError.ImportMalformed name)
                    | AttributeDecode.ImportDecl.Import r ->
                        if not (isJsNativeBody lexed b.expr) then
                            errors.Add(ConformanceError.ImportBodyNotJsNative name)

                        if r.Selector <> emittedName then
                            errors.Add(ConformanceError.ImportSelectorMismatch(name, r.Selector))

                        imports.Add { Name = name; Ref = r }
            | _ -> ()

        List.ofSeq imports, List.ofSeq errors

    /// Check that every `.fsi` `val` has a matching `.fs` `let` of the same name.
    /// Errors come in signature source order, one per name.
    let checkValuePresence (sigVals: string list) (implVals: string list) : ConformanceError list =
        let implNames = HashSet<string>(implVals)
        let errors = ResizeArray<ConformanceError>()
        let seen = HashSet<string>()

        for name in sigVals do
            if seen.Add name && not (implNames.Contains name) then
                errors.Add(ConformanceError.ValueMissingInImpl name)

        List.ofSeq errors

    /// The leading `module` / `namespace` declarations of a `.fsi` and its `.fs` disagree, so
    /// the two files are not a pair at all and every finding below them is about the wrong
    /// companion.
    [<Struct; NoEquality; NoComparison>]
    type ModuleDeclMismatch = { SigDecl: string; ImplDecl: string }

    let private longIdentText (lexed: Lexed) (li: LongIdent<SyntaxToken>) : string =
        li.Idents |> Seq.map (SyntaxToken.nameIn lexed) |> String.concat "."

    /// The dotted leading declaration of a `.fsi`, F#'s `QualifiedNameOfFile` pairing key.
    /// `"global"` for an explicit `namespace global`; `""` for an anonymous module.
    let sigDeclPath (lexed: Lexed) (file: SignatureFile<SyntaxToken>) : string =
        match file with
        | SignatureFile.Namespaces groups when groups.Length > 0 ->
            match groups.[0] with
            | NamespaceDeclGroupSignature.Named(longIdent = li) -> longIdentText lexed li
            | NamespaceDeclGroupSignature.Global _ -> "global"
        | SignatureFile.Namespaces _ -> ""
        | SignatureFile.NamedModule(NamedModuleSignature.NamedModuleSignature(longIdent = li)) -> longIdentText lexed li
        | SignatureFile.AnonymousModule _ -> ""

    /// `sigDeclPath` for the implementation half.
    let implDeclPath (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : string =
        match file with
        | ImplementationFile.Namespaces groups when groups.Length > 0 ->
            match groups.[0] with
            | NamespaceDeclGroup.Named(longIdent = li) -> longIdentText lexed li
            | NamespaceDeclGroup.Global _ -> "global"
        | ImplementationFile.Namespaces _ -> ""
        | ImplementationFile.NamedModule(NamedModule.NamedModule(longIdent = li)) -> longIdentText lexed li
        | ImplementationFile.AnonymousModule _ -> ""

    /// Every CST-level verdict about one `.fsi` / `.fs` pair.
    [<NoEquality; NoComparison>]
    type UnitConformance =
        {
            ModuleMismatch: ModuleDeclMismatch voption
            /// Empty = the implementation answers the signature.
            Errors: ConformanceError list
            /// The implementation's `[<Import>]` bindings, awaiting the manifest-held half
            /// of the check: path against the `runtime` list, selector against the asset's
            /// exports.
            Imports: ImportBinding list
        }

    /// EVERY check over a parsed `.fsi` / `.fs` pair, each with its own `Lexed`: the two files
    /// pair on their leading declaration, then type findings, then value findings. The one
    /// rule set, so a pair gets the same verdict whether a manifest or a compilation unit
    /// brought the two halves together.
    let checkUnit
        (sigLexed: Lexed)
        (sigFile: SignatureFile<SyntaxToken>)
        (implLexed: Lexed)
        (implFile: ImplementationFile<SyntaxToken>)
        : UnitConformance =
        let sigPath = sigDeclPath sigLexed sigFile
        let implPath = implDeclPath implLexed implFile

        let typeErrors =
            check (summariseSig sigLexed sigFile) (summariseImpl implLexed implFile)

        let valueErrors =
            checkValuePresence (summariseSigVals sigLexed sigFile) (summariseImplVals implLexed implFile)

        let imports, importErrors = summariseImports implLexed implFile

        {
            ModuleMismatch =
                if sigPath = implPath then
                    ValueNone
                else
                    ValueSome
                        {
                            SigDecl = sigPath
                            ImplDecl = implPath
                        }
            Errors = typeErrors @ valueErrors @ importErrors
            Imports = imports
        }
