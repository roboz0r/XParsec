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

    /// The rendering of a type in a diagnostic message: `'0 -> ('0 * int) -> '1`, typars by
    /// index. `keyof`, indexed-access and conditional shapes render in TypeScript syntax
    /// (`keyof T`, `T[K]`, `T extends U ? A : B`).
    let rec describeType (t: FrozenType) : string =
        let args (name: string) (xs: EqArray<FrozenType>) =
            match xs.Length with
            | 0 -> name
            | _ -> sprintf "%s<%s>" name (xs |> Seq.map describeType |> String.concat ", ")

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
        | FTTypar(_, index) -> sprintf "'%d" index
        | FTLocalTypar(_, index) -> sprintf "'local%d" index
        | FTUnknown reason -> reason.Render
        | FTMeasure units -> string units

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
        /// The pair's halves declare different emitted names for one value, so a consumer
        /// resolving through the signature would reference a name the implementation never
        /// emits. `declared` and `defined` are the emitted short names.
        | CompiledNameDiffers of name: string * declared: string * defined: string
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
        /// A record field the signature declares that the implementation omits (FS0313).
        | FieldMissingInImpl of typeName: string * field: string
        /// A record field the implementation defines that the signature omits (FS0313).
        | FieldMissingInSig of typeName: string * field: string
        /// One record field written on both halves with a differing type or mutability
        /// (FS0193). `declared` and `defined` are each half's rendered field.
        | FieldDiffers of typeName: string * field: string * declared: string * defined: string
        /// The same record fields on both halves in a different order.
        | FieldOrderDiffers of typeName: string
        /// The halves declare a different number of union cases.
        | UnionCaseCountDiffers of typeName: string * declared: int * defined: int
        /// The union case at `index` differs in name or payload across the pair. `declared`
        /// and `defined` are each half's rendered case.
        | UnionCaseDiffers of typeName: string * index: int * declared: string * defined: string
        /// An enum case the signature declares that the implementation omits.
        | EnumCaseMissingInImpl of typeName: string * case: string
        /// An enum case the implementation defines that the signature omits.
        | EnumCaseMissingInSig of typeName: string * case: string
        /// One enum case written on both halves with a differing constant value.
        | EnumCaseValueDiffers of typeName: string * case: string * declared: string * defined: string
        /// A type abbreviation whose right-hand side differs across the pair.
        | AbbreviationDiffers of typeName: string * declared: string * defined: string
        /// A member the signature declares for which the implementation defines no member of
        /// the same name, staticness, kind and signature (FS0193). `declared` is the rendered
        /// member. The converse is hidden by F#, so an implementation-only member is not drift.
        | MemberMissingInImpl of typeName: string * memberName: string * declared: string
        /// A class whose base type differs across the pair. Each side renders `obj` for an
        /// absent base.
        | BaseTypeDiffers of typeName: string * declared: string * defined: string
        /// A class whose directly-declared interface set differs across the pair.
        | InterfacesDiffer of typeName: string * declared: string * defined: string
        /// A declaration-shape flag (`sealed`, `abstract`, `struct`) written on one half alone
        /// (FS0296/FS0297 for `sealed`, FS0193 for `abstract`).
        | ShapeFlagDiffers of typeName: string * flag: string * declared: bool

    let describe (e: ConformanceError) : string =
        let half (present: bool) = if present then "is" else "is not"

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
        | ConformanceError.CompiledNameDiffers(n, declared, defined) ->
            sprintf
                "value '%s' emits as '%s' in the signature (.fsi) but as '%s' in the implementation (.fs); a reference resolves through the signature, so it would call a method the implementation never emits"
                n
                declared
                defined
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
        | ConformanceError.FieldMissingInImpl(t, field) ->
            sprintf
                "type '%s': the field '%s' was required by the signature (.fsi) but was not specified by the implementation (.fs)"
                t
                field
        | ConformanceError.FieldMissingInSig(t, field) ->
            sprintf
                "type '%s': the field '%s' is present in the implementation (.fs) but not in the signature (.fsi)"
                t
                field
        | ConformanceError.FieldDiffers(t, field, declared, defined) ->
            sprintf
                "type '%s': the field '%s' is declared as '%s' in the signature (.fsi) but defined as '%s' in the implementation (.fs)"
                t
                field
                declared
                defined
        | ConformanceError.FieldOrderDiffers t ->
            sprintf
                "type '%s': the fields are declared in a different order in the signature (.fsi) and the implementation (.fs)"
                t
        | ConformanceError.UnionCaseCountDiffers(t, declared, defined) ->
            sprintf
                "type '%s': the signature (.fsi) declares %d union case(s) but the implementation (.fs) defines %d"
                t
                declared
                defined
        | ConformanceError.UnionCaseDiffers(t, index, declared, defined) ->
            sprintf
                "type '%s': union case %d is declared as '%s' in the signature (.fsi) but defined as '%s' in the implementation (.fs)"
                t
                index
                declared
                defined
        | ConformanceError.EnumCaseMissingInImpl(t, case) ->
            sprintf
                "type '%s': the enum case '%s' was required by the signature (.fsi) but was not specified by the implementation (.fs)"
                t
                case
        | ConformanceError.EnumCaseMissingInSig(t, case) ->
            sprintf
                "type '%s': the enum case '%s' is present in the implementation (.fs) but not in the signature (.fsi)"
                t
                case
        | ConformanceError.EnumCaseValueDiffers(t, case, declared, defined) ->
            sprintf
                "type '%s': the enum case '%s' has value %s in the signature (.fsi) but %s in the implementation (.fs)"
                t
                case
                declared
                defined
        | ConformanceError.AbbreviationDiffers(t, declared, defined) ->
            sprintf
                "type '%s' abbreviates '%s' in the signature (.fsi) but '%s' in the implementation (.fs)"
                t
                declared
                defined
        | ConformanceError.MemberMissingInImpl(t, m, declared) ->
            sprintf
                "type '%s' requires the member '%s' declared as '%s' in the signature (.fsi), which the implementation (.fs) does not define"
                t
                m
                declared
        | ConformanceError.BaseTypeDiffers(t, declared, defined) ->
            sprintf
                "type '%s' inherits '%s' in the signature (.fsi) but '%s' in the implementation (.fs)"
                t
                declared
                defined
        | ConformanceError.InterfacesDiffer(t, declared, defined) ->
            sprintf
                "type '%s' implements [%s] in the signature (.fsi) but [%s] in the implementation (.fs)"
                t
                declared
                defined
        | ConformanceError.ShapeFlagDiffers(t, flag, declared) ->
            sprintf
                "type '%s' %s '%s' in the signature (.fsi) but %s in the implementation (.fs)"
                t
                (half declared)
                flag
                (half (not declared))

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
