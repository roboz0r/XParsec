namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The `.fsi` ↔ `.fs` conformance vocabulary, shared by every producer, plus the one check
// that stays syntactic: whether the two files' leading declarations pair at all.

module Conformance =

    /// The coarse nominal family a type declaration commits to; the sig/impl pair must
    /// agree on it. The `struct` forms group with `Class`.
    [<RequireQualifiedAccess>]
    type TypeKindFamily =
        | Class
        | Interface
        | Record
        | Union
        | Enum

        member this.Label: string =
            match this with
            | TypeKindFamily.Class -> "a class"
            | TypeKindFamily.Interface -> "an interface"
            | TypeKindFamily.Record -> "a record"
            | TypeKindFamily.Union -> "a union"
            | TypeKindFamily.Enum -> "an enum"

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
        /// The nominal family disagrees across the pair (a class-published signature over a
        /// record implementation, say).
        | TypeKindMismatch of name: string * declared: TypeKindFamily * defined: TypeKindFamily
        /// A module-level `val` declared in the `.fsi` with no corresponding `let` in the
        /// `.fs`, the value-granularity FS0240 analogue. The converse is not reported:
        /// F# hides an impl value the signature omits, so a private helper is not drift.
        | ValueMissingInImpl of name: string
        /// An `[<Import>]` binding whose body is not `nativeOnly`. Because the attribute is
        /// the implementation, a real body beside it would be silently discarded.
        | ImportBodyNotNativeOnly of name: string
        /// A `nativeOnly` body with no `[<Import>]` declaring the export that serves it.
        | NativeOnlyWithoutImport of name: string
        /// The `[<Import>]` selector differs from the binding's emitted name, so the
        /// emitted import would bind a different export than the one declared.
        | ImportSelectorMismatch of name: string * selector: string
        /// An `[<Import>]` whose arguments are not two non-empty string literals.
        | ImportMalformed of name: string
        /// The `[<Import>]` path is not a module reference the target can read.
        | ImportPathMalformed of name: string * path: string
        /// The `[<Import>]` path is well-formed and does not resolve to a `[core] runtime`
        /// asset of the declaring package's manifest.
        | ImportAssetNotListed of name: string * path: string
        /// An `[<Import>]` binding compiled for a target with no runtime module system.
        | ImportUnsupportedTarget of name: string
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
        | ConformanceError.TypeKindMismatch(n, declared, defined) ->
            sprintf
                "type '%s' is declared as %s in the signature (.fsi) but defined as %s in the implementation (.fs)"
                n
                declared.Label
                defined.Label
        | ConformanceError.ValueMissingInImpl n ->
            sprintf "value '%s' is declared in the signature (.fsi) but not defined in the implementation (.fs)" n
        | ConformanceError.ImportBodyNotNativeOnly n ->
            sprintf
                "binding '%s' carries [<Import>], whose implementation is the imported export, so its body must be exactly 'nativeOnly'"
                n
        | ConformanceError.NativeOnlyWithoutImport n ->
            sprintf "binding '%s' has body 'nativeOnly' but no [<Import>] declaring the runtime export that serves it" n
        | ConformanceError.ImportSelectorMismatch(n, selector) ->
            sprintf
                "binding '%s' declares [<Import>] selector '%s'; a reference imports the binding's emitted name, so the selector must equal it"
                n
                selector
        | ConformanceError.ImportMalformed n ->
            sprintf
                "binding '%s' carries an [<Import>] whose arguments are not two non-empty string literals ([<Import(\"selector\", \"./asset.mjs\")>])"
                n
        | ConformanceError.ImportPathMalformed(n, path) ->
            sprintf
                "binding '%s' imports from '%s', which is not a module reference this target can read ('./' plus the asset's file name)"
                n
                path
        | ConformanceError.ImportAssetNotListed(n, path) ->
            sprintf
                "binding '%s' imports from '%s', which names no '[core] runtime' asset of the declaring package's manifest"
                n
                path
        | ConformanceError.ImportUnsupportedTarget n ->
            sprintf "binding '%s' declares [<Import>], but this target has no runtime module system to serve it" n
        | ConformanceError.ImportMissingExport(n, selector, asset) ->
            sprintf "binding '%s' imports '%s', which '%s' does not export" n selector asset

    /// One attribute written on both halves of a `.fsi` / `.fs` pair with differing arguments.
    /// `Attribute` is the attribute's metadata name.
    [<NoComparison>]
    type AttributeDivergence =
        {
            Declaration: string
            Attribute: string
        }

    let describeDivergence (d: AttributeDivergence) : string =
        sprintf
            "'%s' carries attribute '%s' on both halves of the pair with differing arguments; the signature's (.fsi) arguments are the ones compiled"
            d.Declaration
            d.Attribute

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
