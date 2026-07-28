namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The FILE: `FrozenPools`, the columns every node of `TastPoolNodes.fs` is addressed in,
// plus the containers its side tables take. The logic that fills the columns lives in
// `TastPools.fs` and the logic that drains them in `TastUnpool.fs`.
//
// The pools are the working representation, the stored one, AND freeze's output:
// `TastAccessor` reads these columns and nothing else, `FrozenCodec.flatten`/`thaw`
// serialize them verbatim, and `Freeze.run` yields them. So `FrozenPools` must be a
// self-contained, serializable value (nothing may ride it that only makes sense with the
// source file still in hand — see `FrozenFileResidue`). The DU survives as freeze's own
// internal construction shape, as the cross-unit inline-template wire, and as a debug/test
// facility; the pools' correctness obligation is that the two are INTERCONVERTIBLE —
// `toPools`/`ofPools` round-trip a `Frozen.TastFile` — which is what proves the columns
// carry the whole tree, and is gated structurally over the corpus.
//
// Layout: EVERY pool is struct-of-arrays — parallel dense columns indexed by the matching
// `*PoolId`. The expr columns are `ExprTys`/`ExprToks`, the child-id columns
// `ExprChildren`/`ExprPatChildren`, the sparse `ExprVarBinder`, plus `ExprPayloads`; the pat
// columns `PatTys`/`PatToks`/`PatChildren` plus `PatPayloads`; the decl columns
// `DeclExprChildren`/`DeclPatChildren` plus `DeclPayloads` (decls carry no node-level
// `ty`/`tok`, so the type rides the payload).
//
// EVERY tree the file bears is in those columns, not just the emittable decls: a `Type`
// decl's member bodies are named by id inside its declaration shape (`PooledTypeDecl`) and
// the inline vocabulary has its own root array (`InlineTemplates`). `FrozenFileResidue`
// correspondingly holds NO tree either.
//
// The cross-references that named a definition by content key during analysis —
// `Var.binding` and six of the seven side tables — name it by `BinderId` here. (The
// seventh, `FunVerdicts`, is keyed by a lambda-EXPRESSION key, not a binder, and takes the
// lambda id space — its dense id is the lambda's `ExprPoolId`.) One of the six goes further
// and names it by nothing at all: a per-binder SCALAR is a `BinderColumn`, positionally
// aligned with the binder pool, so there is no stored key left to go stale (see
// `BinderColumn` for when that applies and when it does not). `ofPools` rebuilds the tree
// AT those ids and re-derives every side-table key by asking the rebuilt nodes what they
// bind, so the round-trip exercises the remap rather than copying the keys back verbatim: a
// reference or side-table key that resolves to no interned binder faults there, which is
// the gate that keeps the enumeration honest.

/// A side table in its STORED form: a sparse association over a dense id space — an entry
/// iff the source `Map<NodeKey, _>` held that binder / lambda. An array because that is
/// what the wire and the round-trip want; it answers no question on its own, so every
/// consumer indexes it first (`DenseTable.index`).
type DenseTable<'id, 'v> = ('id * 'v)[]

[<RequireQualifiedAccess>]
module DenseTable =

    /// The stored table as the LOOKUP every consumer turns it into. One container choice,
    /// made once: a `Dictionary` over keys that are already dense ints, rather than each
    /// reader picking its own (an `F# Map` tree here, a hand-filled `Dictionary` loop
    /// there) and restating why.
    ///
    /// A repeated id FAULTS rather than letting the later entry win. The tables are built
    /// from a `Map`, so a repeat means the producer lost an entry — which `readOnlyDict`
    /// would swallow.
    let index (table: DenseTable<'id, 'v>) : System.Collections.Generic.IReadOnlyDictionary<'id, 'v> =
        let d = System.Collections.Generic.Dictionary<'id, 'v>(table.Length)

        for (id, v) in table do
            if not (d.TryAdd(id, v)) then
                failwithf "DenseTable.index: id %O appears twice" id

        d

/// A per-binder SCALAR in its stored form: a COLUMN, one slot per binder of
/// `FrozenPools`' binder pool and in that same order, `ValueNone` where the binder carries
/// no such fact.
///
/// The point is what it does NOT have. A `DenseTable<BinderId, _>` still holds a key, and a
/// key can name a binder the frozen file does not bear — the stale-entry defect
/// `TastPools.toPools`' `binderIdOf` faults on. A column has no key to be stale: the fact is
/// AT the binder, so it cannot desync from it, and the producer's key is consumed once at
/// fill time and never stored. Only a per-binder fact that is genuinely independent of the
/// node AND scalar takes this form; a per-binder record or list stays a `DenseTable`, since
/// a column of them would be mostly empty payloads rather than mostly empty slots.
type BinderColumn<'v> = 'v voption[]

[<RequireQualifiedAccess>]
module BinderColumn =

    /// The fact recorded for `id`, if any — the one read every consumer makes, so the
    /// absence convention is stated once here rather than at each of them.
    ///
    /// An id PAST the column's end also reads `ValueNone`, and honestly: the binder id
    /// space is STACKED (`TastPoolBuilder.mintBinder` hands out ids above the base pool's,
    /// which is what the columns are aligned to), and a binder a lowering minted is not a
    /// source binding, so it has no such fact by construction.
    let tryItem (col: BinderColumn<'v>) (BinderId i) : 'v voption =
        if i < col.Length then col.[i] else ValueNone

/// One entry of the pooled inline VOCABULARY: a published template's identity and parameter
/// attributes, with its declaration named by pool id.
///
/// A template is a SEPARATE ROOT from the emitted function of the same name, and the two
/// trees are deliberately NOT shared: `Freeze` publishes the UNWALKED snapshot, because a
/// template's static-opt clauses and trait calls must resolve against a CALL SITE's operand
/// types rather than against the nothing that is ground at its definition. Pooling preserves
/// that split by giving the templates their own roots.
type PooledInlineValue =
    {
        Key: SymbolKey
        Decl: DeclPoolId
        ParamAttrs: ParamAttrs[]
    }

/// Everything of a `Frozen.TastFile` that has NO pooled form — the file MINUS its trees (the
/// columns) and MINUS the seven side tables (the dense `BinderId`/`ExprPoolId` associations
/// and the two per-binder `BinderColumn`s).
/// NO field here carries a tree, which is the property that matters: every expression and
/// pattern in the file is in the columns, so the residue can never drag a subtree along.
/// Exactly these three fields, each for its own reason:
///
///   * `Diagnostics` — a flat list keyed by `NodeKey`, in no pooled domain (a diagnostic can
///     name a node the emittable tree does not contain, so it cannot take a pool id).
///   * `IntrinsicReprKeys` / `Accessibility` — the two `SymbolKey`-keyed dictionaries. Their
///     key space is the SYMBOL identity, not the positional node identity the pools give, so
///     they are untouched by the dense-id remap.
///
/// Naming the residue is the point: `FrozenPools` is then a self-contained, serializable
/// value, and what remains outside the columnar form is visible in the type rather than
/// hidden inside a retained whole `TastFile` (which would also make the pools unserializable
/// without re-serializing the DU file they were built from).
type FrozenFileResidue =
    {
        // Qualified: this file `open`s `XParsec.FSharp.Parser`, which also declares a
        // `Diagnostic`; the bare name would bind to the parser's, mistyping the field —
        // the same shadowing `TastFileG.Diagnostics` guards against.
        Diagnostics: XParsec.FSharp.SemanticAnalysis.Diagnostic list
        IntrinsicReprKeys: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, IntrinsicReprInfo>
        Accessibility: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, Accessibility>
    }

/// THE frozen file: the expr struct-of-arrays columns plus the pat/decl columns (indexable
/// by the matching `*PoolId`) and the decl roots, in source order. This is what `Freeze.run`
/// yields and every consumer reads; the `Frozen.TastFile` DU it is built from
/// (`TastPools.toPools`) does not outlive the freeze. Kept OFF `TastFileG` — that record is
/// shared with the `SemType` instantiation, which has no pools.
///
/// The binder pool and the dense side tables give the file's identity keys a positional
/// home: a binder IS its slot, indexable by `BinderId`; the source file's side tables are
/// re-expressed as `BinderId`-keyed (or, for `FunVerdicts`, `ExprPoolId`-keyed)
/// associations, and the one that is a per-binder SCALAR sheds the key entirely for a slot
/// aligned with the binder pool (`BinderColumn`). `ofPools` rebuilds the maps from these,
/// so the round-trip proves the remap is a faithful bijection over every referenced binder
/// rather than trivially copying the source maps.
type FrozenPools =
    {
        /// The expression pool as struct-of-arrays: these columns are parallel, each
        /// indexed by `ExprPoolId`. `ExprTys`/`ExprToks` are the node's `ty`/`tok`;
        /// `ExprChildren` the immediate child-expr ids in `TastPools.exprChildren` order;
        /// `ExprPatChildren` the owned pat ids in `TastPools.exprPatChildren` order;
        /// `ExprVarBinder` the `Var` reference id (`ValueSome` only at a `Var`);
        /// `ExprPayloads` the residual per-case payload, which is also the node's shape
        /// tag (`ExprPayload.shape`) — there is no separate tag column, so the two cannot
        /// disagree. No DU node is retained — the columns are Node-sufficient, which the
        /// round-trip gate proves.
        ExprTys: FrozenType[]
        /// Each node's anchor as a token INDEX into this file's own `Lexed` (`Anchor`), not
        /// the token struct: everything the struct carried is recoverable from the index
        /// against that `Lexed`, and the index is a quarter of its width on the column that
        /// dominates a frozen file. Read through `TastPoolBuilder.exprTok`, which is where
        /// the absence convention is decoded.
        ExprToks: int<token>[]
        ExprChildren: ExprPoolId[][]
        ExprPatChildren: PatPoolId[][]
        ExprVarBinder: BinderId voption[]
        ExprPayloads: ExprPayload[]
        /// The pattern pool as struct-of-arrays: parallel columns indexed by `PatPoolId`.
        /// `PatTys`/`PatToks` are the node's `ty`/`tok`; `PatChildren` the immediate
        /// sub-pat ids in `TastPools.patChildren` order (patterns own no child
        /// expressions); `PatPayloads` the residual per-case payload, tag included. No DU
        /// node is retained.
        PatTys: FrozenType[]
        /// The pattern twin of `ExprToks`.
        PatToks: int<token>[]
        PatChildren: PatPoolId[][]
        PatPayloads: PatPayload[]
        /// The declaration pool as struct-of-arrays, indexed by `DeclPoolId`.
        /// `DeclExprChildren`/`DeclPatChildren` are the decl's immediate expr/pat roots
        /// (the `Let` binding's value + head pattern, or the `Expression` body — a `Type`
        /// decl surfaces none); `DeclPayloads` the residual per-case payload, tag included
        /// (it also carries the decl's type, there being no node-level `ty` column). No DU
        /// node is retained.
        DeclExprChildren: ExprPoolId[][]
        DeclPatChildren: PatPoolId[][]
        DeclPayloads: DeclPayload[]
        /// The pool ids of the source file's `Decls`, in source order — the entry points
        /// for a pool walk / rebuild.
        Roots: DeclPoolId[]
        /// The inline vocabulary's roots: one per published template, in publication order.
        /// A SECOND root array rather than entries of `Roots`, because a template is not an
        /// emittable decl and must not be walked as one — and it is a genuinely distinct
        /// tree from the emitted function of the same name (see `PooledInlineValue`).
        InlineTemplates: PooledInlineValue[]
        /// The binder pool: two parallel dense columns indexed by `BinderId`, their own
        /// arrays disjoint from the `Pat*` columns. A binder has NO stored identity beside
        /// its slot — the slot IS the identity, a drained tree names its binders by
        /// `BinderId` (`Pooled.*`), and every pooled reference resolves against the id.
        /// What these two carry is what a slot alone cannot answer: how the source SPELLS
        /// the binder, and WHERE. A `NamedSimple` pattern still also appears in the pat
        /// columns for the tree walk; this is the additional dense column references
        /// resolve against, not a re-pointing.
        ///
        /// `BinderNames` is the identifier the source spells the binder with, EMPTY where
        /// none does — a class's `this`/`base`, a freshened inline binder. Read through
        /// `BinderNaming.ofColumn`, which is where that convention is decoded. The text is
        /// the source's, unmangled: a target dialect's reserved-word and punctuation rules
        /// belong to the backend that emits the name.
        BinderNames: string[]
        /// The token the binder's name is spelled at, as an index into this file's `Lexed`
        /// (`Anchor`) — what a span or a line/column is taken from. NEGATIVE for a
        /// definition site NO NODE SPELLS: a declaration's pattern-less key slots
        /// (`TTypeMemberG.ThisKey`/`BaseKey`/`Params`, a secondary ctor's params and lets,
        /// the base-ctor call's view of the primary params) hold a binder key and no token,
        /// so there is none to store. Those slots still carry a `BinderNames` entry: a
        /// member parameter's key is projected from its CST pattern and so names a real
        /// source position even though the frozen shape keeps no token for it.
        BinderToks: int<token>[]
        /// The not-yet-pooled remainder of the source file, carried verbatim.
        Residue: FrozenFileResidue
        /// The source `Map<NodeKey,_>` side tables that keep a KEY, re-keyed by `BinderId`.
        /// `ofPools` rebuilds each map from its dense form; a reader indexes it with
        /// `DenseTable.index`. A per-binder table is here rather than a `BinderColumn`
        /// because its value is not a scalar — a record, a list, or (`ClosureReprs`) a fact
        /// its own producer already filters against the tree.
        ModuleMembers: DenseTable<BinderId, ModuleBindingInfo>
        ClosureReprs: DenseTable<BinderId, ClosureRepr>
        /// The one side table keyed by a lambda EXPRESSION (`LambdaKey`)
        /// rather than a binder, so it is re-keyed onto the lambda id space — a lambda's
        /// dense id IS its `ExprPoolId` (positional: every `Lambda` expr is already pooled),
        /// off the binder pool. `ofPools` inverts by recomputing that key from the lambda's
        /// `ExprToks` column, the Node now gone.
        ///
        /// The re-key is NOT a bijection and must not be built as one: the key is
        /// one-to-MANY over this space, since a spliced inline body and the template it was
        /// spliced from carry their definition-site tokens into every copy. So several rows
        /// may hold the same verdict, and the inverse folds them back onto the one key.
        FunVerdicts: DenseTable<ExprPoolId, FunVerdict>
        GenericFnSchemes: DenseTable<BinderId, FrozenConstraint list>
        BindingValReprs: DenseTable<BinderId, PooledValRepr>
        /// The one per-binder SCALAR, held as a `BinderColumn` — positionally aligned with
        /// the binder pool, no key: a binding's typar-axis width at the index-minting point
        /// (`Elaborate`'s `quantEnv`), read for `ExternalSymbol.TyparArity` and for a
        /// `PooledValRepr`'s `Typars`. Sparse over the binder pool — most binders are
        /// parameters and locals — so most slots are `ValueNone`.
        BindingTyparArities: BinderColumn<int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrozenPools =

    /// The zero column set — a pool that is nobody's file. Lives with the type because it
    /// is a property OF the type, not of any one consumer:
    /// `TastPoolBuilder.openEmpty` stacks an overlay on it for nodes that belong to no
    /// frozen tree at all (an EXTERNAL symbol's `.fsi`-minted `ValRepr` patterns, a
    /// provider's re-axised copies), and they are read through the same accessor as any
    /// other node.
    let empty: FrozenPools =
        {
            ExprTys = [||]
            ExprToks = [||]
            ExprChildren = [||]
            ExprPatChildren = [||]
            ExprVarBinder = [||]
            ExprPayloads = [||]
            PatTys = [||]
            PatToks = [||]
            PatChildren = [||]
            PatPayloads = [||]
            DeclExprChildren = [||]
            DeclPatChildren = [||]
            DeclPayloads = [||]
            Roots = [||]
            InlineTemplates = [||]
            BinderNames = [||]
            BinderToks = [||]
            Residue =
                {
                    Diagnostics = []
                    IntrinsicReprKeys = readOnlyDict []
                    Accessibility = readOnlyDict []
                }
            ModuleMembers = [||]
            ClosureReprs = [||]
            FunVerdicts = [||]
            GenericFnSchemes = [||]
            BindingValReprs = [||]
            // Zero binders, so the column is zero-length — aligned with the binder pool
            // exactly as a filled pool's is.
            BindingTyparArities = [||]
        }

    /// How the file spells the binder at `id`, and nothing more: the naming column read
    /// through its own decoder, so no consumer has to know that an empty slot means
    /// "no identifier spells this binder". ONE home, shared by the accessor's stacked
    /// read (`TastPoolBuilder.binderNaming`) and by any direct reader of the columns.
    let binderNaming (pools: FrozenPools) (id: BinderId) : BinderNaming =
        let (BinderId i) = id
        BinderNaming.ofColumn pools.BinderNames.[i] id

    /// The typar-axis width recorded for `binder`, an empty slot reading 0. That is not a
    /// fallback but the ANSWER: `Elaborate` files an arity only for a binding whose head
    /// introduces a binder, and a binder with no recorded width quantifies nothing. One
    /// home, because two readers defaulting apart is how a value silently regeneralises —
    /// `TastPools`' `PooledValRepr.Typars` and `FrozenSignature`'s
    /// `ExternalSymbol.TyparArity` are the same fact and must agree.
    let typarArity (pools: FrozenPools) (binder: BinderId) : int =
        BinderColumn.tryItem pools.BindingTyparArities binder
        |> ValueOption.defaultValue 0
