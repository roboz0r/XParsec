namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Side tables hold all in-flight semantic information. CST is never mutated.
// See docs/architecture.md.

/// Per-field metadata for a `TypeDefn.Record`. Field types start life as
/// fresh TyVars stamped by NameResolution and get linked to the real
/// translated type by Unification before any expression is typed.
[<Sealed>]
type RecordFieldInfo(name: string, ty: SemType, isMutable: bool, declKey: NodeKey) =
    member val Name = name
    member val Type = ty
    member val IsMutable = isMutable
    member val DeclKey = declKey

/// One entry per `TypeDefn.Record` declaration. Indexed by name on
/// `PassContext.RecordTypes`; the reverse `FieldIndex` lets literal /
/// pattern field-set inference find candidates by field name.
[<Sealed>]
type RecordTypeInfo(name: string, fields: RecordFieldInfo[], declKey: NodeKey) =
    member val Name = name
    member val Fields = fields
    member val DeclKey = declKey

/// Per-case metadata for a `TypeDefn.Union`. Field types start as fresh
/// TyVar placeholders stamped by NameResolution and are linked to the
/// translated CST types by Unification's field-fill-in pass before any
/// expression is typed. `FieldNames` carries per-field names when the
/// source uses named fields (`| Case of x: int * y: int`); positional
/// fields have `ValueNone`. `UnionName` is the declaring type's name —
/// used by ctor reference / pattern inference to mint `TyUnion`.
[<Sealed>]
type UnionCaseInfo(name: string, unionName: string, fields: SemType[], fieldNames: string voption[], declKey: NodeKey) =
    member val Name = name
    member val UnionName = unionName
    member val Fields = fields
    member val FieldNames = fieldNames
    member val DeclKey = declKey

/// One entry per `TypeDefn.Union` declaration. Indexed by name on
/// `PassContext.UnionTypes`; the reverse `CtorIndex` lets ctor reference /
/// pattern inference find candidates by case name.
[<Sealed>]
type UnionTypeInfo(name: string, cases: UnionCaseInfo[], declKey: NodeKey) =
    member val Name = name
    member val Cases = cases
    member val DeclKey = declKey

[<Sealed>]
type SideTable<'V>() =
    let dict = Dictionary<NodeKey, 'V>(HashIdentity.Structural)

    member _.Count = dict.Count

    member _.TryGetValue(key: NodeKey) =
        match dict.TryGetValue(key) with
        | true, v -> ValueSome v
        | false, _ -> ValueNone

    member _.Set(key: NodeKey, value: 'V) = dict[key] <- value

    member _.ContainsKey(key: NodeKey) = dict.ContainsKey key

    /// Callers must treat the returned dictionary as read-only once Freeze starts.
    member _.AsDictionary() : IReadOnlyDictionary<NodeKey, 'V> = dict :> _

/// **Thread-safety:** a `PassContext` is single-threaded — its side tables,
/// `Diagnostics` channel, and the `TypeVar` graph it owns all mutate in
/// place and are not safe to access from multiple threads. Parallelism
/// happens at file granularity by allocating one `PassContext` per file
/// and analysing them concurrently; the shared `IExternalSymbolProvider`
/// is the only object that crosses thread boundaries (and its contract
/// requires thread-safe `TryLookup`). See [`docs/architecture.md`](docs/architecture.md#parallelism).
[<Sealed>]
type PassContext(provider: IExternalSymbolProvider, input: string, lexed: Lexed) =
    member val Provider = provider
    member val Input = input
    member val Lexed = lexed
    /// Written by Desugar.
    member val Desugared = SideTable<DesugaredForm>() with get
    /// Written by NameResolution.
    member val Binding = SideTable<ResolvedBinding>() with get
    /// Written by Unification.
    member val TypeVar = SideTable<TypeVar>() with get
    /// Written by Unification after generalisation. Keyed by the binding's
    /// headPat NodeKey (which is also the `BindingSite` NameResolution records).
    /// Present only for `let`-bound names that pass `shouldGeneralise` —
    /// module-level, nested, and `let rec` single-name bindings. Compound
    /// destructuring heads and lambda parameters do NOT get schemes.
    member val Scheme = SideTable<TypeScheme>() with get
    /// Written by Regions. RegionId itself lives on the TypeVar; this table
    /// carries the classified EscapeState per expression.
    member val Escape = SideTable<EscapeState>() with get
    member val Diagnostics = ResizeArray<Diagnostic>() with get
    /// Current let-depth (Rémy's levels). Owned by Unification — push on
    /// entering a binding group's RHSes, pop after typing them. Generalisation
    /// uses the pre-push value as the threshold for "which TyVars do I
    /// quantify?". Lives on PassContext (not threaded as a parameter) to
    /// match ctx.Diagnostics — same scope, same mutation pattern, same wide
    /// call-site reach without parameter pollution.
    member val CurrentLevel = 0 with get, set
    /// Written by NameResolution from `TypeDefn.Record`s; field types are
    /// filled in by Unification after the registry is populated. Name-keyed
    /// (single-segment v1). Cross-file resolution will overlay the local
    /// table with a provider-backed equivalent when modules / namespaces
    /// land.
    member val RecordTypes = Dictionary<string, RecordTypeInfo>() with get
    /// Reverse index: field name → list of record types that declare it.
    /// Built once by NameResolution alongside RecordTypes; used by the
    /// literal / pattern field-set inference path.
    member val FieldIndex = Dictionary<string, RecordTypeInfo list>() with get
    /// Written by NameResolution from `TypeDefn.Union`s; case field types
    /// are filled in by Unification after the registry is populated.
    /// Name-keyed (single-segment v1).
    member val UnionTypes = Dictionary<string, UnionTypeInfo>() with get
    /// Reverse index: ctor name → list of case-info entries (each tagged
    /// with the declaring union type). Used by ctor-reference /
    /// ctor-pattern resolution.
    member val CtorIndex = Dictionary<string, UnionCaseInfo list>() with get

    /// Source text of `token`. Empty for virtual (synthesised) tokens.
    member this.NameOf(token: SyntaxToken) : string =
        match token.Index with
        | TokenIndex.Regular iT -> this.Lexed.GetTokenString(iT, this.Input)
        | TokenIndex.Virtual -> ""

/// TODO: flesh out (range, code, sub-severities) once passes need to differentiate.
and [<Struct>] Diagnostic =
    {
        Key: NodeKey
        Message: string
        Severity: Severity
    }

and [<Struct>] Severity =
    | Error
    | Warning
    | Info
