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
// internal construction shape, as the cross-file inline-template wire, and as a debug/test
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
// the inline vocabulary has its own root array (`InlineTemplates`), as does the resolved
// specialization table (`Specializations`). `FrozenFileResidue` correspondingly holds NO
// tree either.
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

/// A CHILD-ID column: every slot's child ids, concatenated into one flat `Ids` array, with
/// `Start.[i] .. Start.[i+1]` delimiting slot `i`'s. `Start` is one longer than the pool it
/// indexes, so the last slot needs no special case.
///
/// Two arrays for the whole column, not one per node. The jagged form paid an object header
/// plus an outer reference for EVERY slot — the leaves included, which are most of a tree
/// and have no children at all — where here a childless slot is just `Start.[i] =
/// Start.[i+1]` and occupies four bytes of `Start`.
///
/// `count`/`item` are the read surface, and they index the flat array with no row
/// materialised: a view that names two children (`App`'s fn/arg, `Let`'s value/body) is two
/// array reads. `slice` is for the paths that genuinely speak in whole arrays — a row
/// transpose, a DU drain — and hands back the shared empty array where there are no
/// children, so the common leaf costs nothing there either.
///
/// The representation is PRIVATE, and the two ways in are `ChildColumnBuilder` (which cannot
/// produce a malformed one) and `ofStored` (which checks). The reason is that the invariant is
/// not self-announcing: `Start` out of step with `Ids` does not fault, it hands back a
/// DIFFERENT, in-range child list for every slot after the discrepancy, silently re-parenting
/// the tree. That is the same class of quiet failure `RowTable`'s seed check exists to rule
/// out, and the arrays here arrive from the same place — the wire.
type ChildColumn<'id> = private { Start: int[]; Ids: 'id[] }

[<RequireQualifiedAccess>]
module ChildColumn =

    /// The zero column — one start and no ids, which is what a pool with no slots has.
    let empty<'id> : ChildColumn<'id> = { Start = [| 0 |]; Ids = [||] }

    /// How many slots the column delimits. `Start` carries the CSR `n+1` entries, so this is
    /// one less — and it is what a reader checks a decoded column against its pool with.
    let length (col: ChildColumn<'id>) : int = col.Start.Length - 1

    /// How many children slot `i` has — the bound `item` indexes into.
    let count (col: ChildColumn<'id>) (i: int) : int = col.Start.[i + 1] - col.Start.[i]

    /// Slot `i`'s `k`-th child.
    let item (col: ChildColumn<'id>) (i: int) (k: int) : 'id = col.Ids.[col.Start.[i] + k]

    /// Slot `i`'s children as their own array.
    let slice (col: ChildColumn<'id>) (i: int) : 'id[] =
        let s = col.Start.[i]

        match col.Start.[i + 1] - s with
        | 0 -> Array.empty
        | n -> Array.sub col.Ids s n

    /// The stored arrays, for the CODEC and nothing else — `starts` is the `n+1` CSR form and
    /// `ids` the flat concatenation. Paired with `ofStored`, which is the checked way back.
    let starts (col: ChildColumn<'id>) : int[] = col.Start
    let ids (col: ChildColumn<'id>) : 'id[] = col.Ids

    /// Re-admit a column from its two stored arrays — the CHECKED narrowing, and the only way
    /// to build one that did not come from a `ChildColumnBuilder`.
    ///
    /// It is checked rather than trusted for the reason the type's own doc gives: the arrays
    /// come off the wire, and a `Start` that is short, non-monotone, or does not end at
    /// `ids.Length` re-parents the tree instead of faulting. Four conditions, and together
    /// they are exactly the CSR well-formedness `ChildColumnBuilder` maintains by
    /// construction — so a column reaching a reader is well-formed whichever way it was made.
    /// O(n) over an array the decoder just built element by element, which is noise beside the
    /// decode itself.
    let ofStored (start: int[]) (ids: 'id[]) : ChildColumn<'id> =
        if start.Length = 0 then
            failwith "ChildColumn: a stored column has no slot-start array (CSR needs n+1 entries)"

        if start.[0] <> 0 then
            failwithf "ChildColumn: a stored column starts at %d, not 0" start.[0]

        for i in 1 .. start.Length - 1 do
            if start.[i] < start.[i - 1] then
                failwithf "ChildColumn: stored slot starts decrease at %d (%d < %d)" i start.[i] start.[i - 1]

        if start.[start.Length - 1] <> ids.Length then
            failwithf
                "ChildColumn: stored slot starts end at %d but the column holds %d ids"
                start.[start.Length - 1]
                ids.Length

        { Start = start; Ids = ids }

/// A `ChildColumn` under construction: slots appended one whole child list at a time, in
/// pool order. That is how every producer of one fills it — the pooling sink adds a row's
/// children exactly where the row takes its id — so the append is the only operation, and
/// the running `Ids` count IS the next slot's start.
type ChildColumnBuilder<'id>() =
    // Seeded with slot 0's start, so `Start` is already the CSR form's `n+1` entries after
    // `n` appends and an unfilled builder is `ChildColumn.empty`.
    let starts = ResizeArray<int>([ 0 ])
    let ids = ResizeArray<'id>()

    member _.Add(kids: 'id[]) =
        ids.AddRange kids
        starts.Add ids.Count

    member _.ToColumn() : ChildColumn<'id> =
        {
            Start = starts.ToArray()
            Ids = ids.ToArray()
        }

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

/// One entry of the pooled RESOLVED-SPECIALIZATION table: the grounding it was resolved at,
/// with its declaration named by pool id — the pooled form of `TSpecializationG`, which is
/// where the entry's own doc lives.
///
/// `Key` and `Origin` ride across the pooling unchanged: a `SymbolKey`, frozen types and a
/// file identity, none of which the columns address, exactly as a payload's embedded types do.
/// `Origin` in particular MUST survive the pools and the wire — the entry's anchors are
/// indices into that file and are unreadable without it.
type PooledSpecialization =
    {
        Key: Frozen.SpecializationKey
        Origin: OriginFile
        Decl: DeclPoolId
    }

/// Everything of a `Frozen.TastFile` that has NO pooled form — the file MINUS its trees (the
/// columns) and MINUS the seven side tables (the dense `BinderId`/`ExprPoolId` associations
/// and the two per-binder `BinderColumn`s).
/// NO field here carries a tree, which is the property that matters: every expression and
/// pattern in the file is in the columns, so the residue can never drag a subtree along.
/// Exactly these three fields, each for its own reason:
///
///   * `Diagnostics` — a flat list positioned in the file's own token space, in no pooled
///     domain (a diagnostic can name a node the emittable tree does not contain, so it
///     cannot take a pool id).
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
[<NoEquality; NoComparison>]
type FrozenPools =
    {
        /// WHICH FILE this file's `Anchor` columns index. Every `Anchor` here — the expr, pat
        /// and binder token columns — is a position in this file and means nothing against
        /// another's, so the identity travels with the columns rather than beside them: a pool
        /// handed one separately can be handed the wrong one, and nothing downstream could
        /// tell (`OriginSources.tokenAt` faults only for a file it was NAMED against).
        ///
        /// It is also the only thing that can say whether a stated domain — an entry's
        /// `Origin`, a node's — is this file's own or a producer's, which is a question no
        /// consumer past the freeze can otherwise answer: the identity is minted before the
        /// parse (`Hashing.originSource`) and reconstructing it downstream from a path and a
        /// re-read would be a different value that compares unequal.
        Origin: OriginFile
        /// The file's own interned type and key tables — what the `ty` columns index. Every
        /// `TypeId` in this record is a row of THIS table and of no other: the tables are
        /// per file (see `FrozenTypeTable`), so an id from another file's pools would name a
        /// different, valid type rather than miss.
        ///
        /// Hash-consed as the pools are filled, so two structurally equal types of this file
        /// occupy one row — which makes a within-file `TypeId` comparison a structural type
        /// comparison, and allocates one `FrozenType` per DISTINCT type rather than per
        /// occurrence.
        Types: FrozenTypeTable
        /// The expression pool as struct-of-arrays: these columns are parallel, each
        /// indexed by `ExprPoolId`. `ExprTys`/`ExprToks` are the node's `ty`/`tok`;
        /// `ExprChildren` the immediate child-expr ids in `TastPoolShapes.exprChildren` order;
        /// `ExprPatChildren` the owned pat ids in `TastPoolShapes.exprPatChildren` order;
        /// `ExprVarBinder` the `Var` reference id (`ValueSome` only at a `Var`);
        /// `ExprPayloads` the residual per-case payload, which is also the node's shape
        /// tag (`ExprPayload.shape`) — there is no separate tag column, so the two cannot
        /// disagree. No DU node is retained — the columns are Node-sufficient, which the
        /// round-trip gate proves.
        ///
        /// The node's type as a row of `Types`, not the tree: a 4-byte id on the column that
        /// dominates a frozen file, resolved through `TastPoolBuilder.exprTy`, which is
        /// where it becomes the `FrozenType` every consumer already matches on.
        ExprTys: TypeId[]
        /// Each node's anchor as a token INDEX into this file's own `Lexed` (`Anchor`), not
        /// the token struct: everything the struct carried is recoverable from the index
        /// against that `Lexed`, and the index is a quarter of its width on the column that
        /// dominates a frozen file. Read through `TastPoolBuilder.exprTok`, which is where
        /// the absence convention is decoded.
        ExprToks: Anchor[]
        /// The child edges in CSR form (`ChildColumn`) — one flat id array per column
        /// rather than one array per node. Read through `TastPoolBuilder.exprChildCount` /
        /// `exprChild`, which index it without materialising the sibling list.
        ExprChildren: ChildColumn<ExprPoolId>
        ExprPatChildren: ChildColumn<PatPoolId>
        ExprVarBinder: BinderId voption[]
        ExprPayloads: ExprPayload[]
        /// The pattern pool as struct-of-arrays: parallel columns indexed by `PatPoolId`.
        /// `PatTys`/`PatToks` are the node's `ty`/`tok`; `PatChildren` the immediate
        /// sub-pat ids in `TastPoolShapes.patChildren` order (patterns own no child
        /// expressions); `PatPayloads` the residual per-case payload, tag included. No DU
        /// node is retained.
        ///
        /// The pattern twin of `ExprTys` — a row of `Types`, read through
        /// `TastPoolBuilder.patTy`.
        PatTys: TypeId[]
        /// The pattern twin of `ExprToks`.
        PatToks: Anchor[]
        /// The pattern twin of `ExprChildren`.
        PatChildren: ChildColumn<PatPoolId>
        PatPayloads: PatPayload[]
        /// The declaration pool as struct-of-arrays, indexed by `DeclPoolId`.
        /// `DeclExprChildren`/`DeclPatChildren` are the decl's immediate expr/pat roots
        /// (the `Let` binding's value + head pattern, or the `Expression` body — a `Type`
        /// decl surfaces none); `DeclPayloads` the residual per-case payload, tag included
        /// (it also carries the decl's type, there being no node-level `ty` column). No DU
        /// node is retained.
        DeclExprChildren: ChildColumn<ExprPoolId>
        DeclPatChildren: ChildColumn<PatPoolId>
        DeclPayloads: DeclPayload[]
        /// The pool ids of the source file's `Decls`, in source order — the entry points
        /// for a pool walk / rebuild.
        Roots: DeclPoolId[]
        /// The inline vocabulary's roots: one per published template, in publication order.
        /// A SECOND root array rather than entries of `Roots`, because a template is not an
        /// emittable decl and must not be walked as one — and it is a genuinely distinct
        /// tree from the emitted function of the same name (see `PooledInlineValue`).
        InlineTemplates: PooledInlineValue[]
        /// The resolved-specialization table's roots: one per entry, indexed by the
        /// `SpecializationId` an `ExprPayload.InlineCall` carries — a THIRD root array,
        /// beside `Roots` and `InlineTemplates`, for the same reason the second one exists:
        /// an entry is not an emittable decl and must not be walked as one.
        ///
        /// The array is a DAG's node list, and an entry's body may name a LATER slot, so a
        /// consumer resolves an id against the whole array rather than assuming a
        /// definition-before-use order.
        Specializations: PooledSpecialization[]
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
        BinderToks: Anchor[]
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
            Origin = OriginFile.nowhere
            Types = FrozenTypeTable.Empty
            ExprTys = [||]
            ExprToks = [||]
            ExprChildren = ChildColumn.empty
            ExprPatChildren = ChildColumn.empty
            ExprVarBinder = [||]
            ExprPayloads = [||]
            PatTys = [||]
            PatToks = [||]
            PatChildren = ChildColumn.empty
            PatPayloads = [||]
            DeclExprChildren = ChildColumn.empty
            DeclPatChildren = ChildColumn.empty
            DeclPayloads = [||]
            Roots = [||]
            InlineTemplates = [||]
            Specializations = [||]
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
