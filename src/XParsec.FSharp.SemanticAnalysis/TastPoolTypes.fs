namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// A side table in its STORED form: a sparse association over a dense id space.
type DenseTable<'id, 'v> = ('id * 'v)[]

[<RequireQualifiedAccess>]
module DenseTable =

    /// A repeated id FAULTS rather than letting the later entry win.
    let index (table: DenseTable<'id, 'v>) : System.Collections.Generic.IReadOnlyDictionary<'id, 'v> =
        let d = System.Collections.Generic.Dictionary<'id, 'v>(table.Length)

        for (id, v) in table do
            if not (d.TryAdd(id, v)) then
                failwithf "DenseTable.index: id %O appears twice" id

        d

/// A per-bound-variable SCALAR in its stored form: one slot per bound variable of the bound-variable pool and in
/// that same order, `ValueNone` where the bound variable carries no such fact.
type BoundVarColumn<'v> = 'v voption[]

[<RequireQualifiedAccess>]
module BoundVarColumn =

    /// An id PAST the column's end reads `ValueNone`: an overlay mints bound variable ids above the
    /// base pool the column is aligned to, and a minted bound variable is no source binding.
    let tryItem (col: BoundVarColumn<'v>) (BoundVarId i) : 'v voption =
        if i < col.Length then col.[i] else ValueNone

/// A CHILD-ID column: every slot's child ids concatenated into one flat `Ids` array, with
/// `Start.[i] .. Start.[i+1]` delimiting slot `i`'s. `Start` is one longer than the pool it
/// indexes, so the last slot needs no special case.
type ChildColumn<'id> = private { Start: int[]; Ids: 'id[] }

[<RequireQualifiedAccess>]
module ChildColumn =

    let empty<'id> : ChildColumn<'id> = { Start = [| 0 |]; Ids = [||] }

    /// How many SLOTS the column delimits, not how many ids it holds.
    let length (col: ChildColumn<'id>) : int = col.Start.Length - 1

    /// How many children slot `i` has.
    let count (col: ChildColumn<'id>) (i: int) : int = col.Start.[i + 1] - col.Start.[i]

    /// Slot `i`'s `k`-th child.
    let item (col: ChildColumn<'id>) (i: int) (k: int) : 'id = col.Ids.[col.Start.[i] + k]

    /// Slot `i`'s children as their own array.
    let slice (col: ChildColumn<'id>) (i: int) : 'id[] =
        let s = col.Start.[i]

        match col.Start.[i + 1] - s with
        | 0 -> Array.empty
        | n -> Array.sub col.Ids s n

    let starts (col: ChildColumn<'id>) : int[] = col.Start
    let ids (col: ChildColumn<'id>) : 'id[] = col.Ids

    /// Re-admit a column from its two stored arrays, off the wire. CHECKED because a `Start`
    /// out of step with `Ids` does not fault on read, but hands back a different, in-range
    /// child list for every slot after the discrepancy.
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

/// A `ChildColumn` under construction: one whole child list appended per slot, in pool order.
type ChildColumnBuilder<'id>() =
    // Seeded with slot 0's start, so `Start` is the `n+1` form after `n` appends.
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

/// One entry of the pooled inline VOCABULARY: a published template, rooted separately from
/// the emitted function of the same name.
type PooledInlineValue =
    {
        Key: SymbolKey
        Decl: DeclPoolId
        ParamAttrs: EqArray<ParamAttrs>
    }

/// One entry of the pooled RESOLVED-SPECIALIZATION table. `Source` is the file the entry's
/// anchors index, so without it they cannot be read.
type PooledSpecialization =
    {
        Key: Frozen.SpecializationKey
        Path: AssemblyFilePath
        Decl: DeclPoolId
    }

/// Everything of a frozen file that has NO pooled form: the file MINUS its trees and
/// MINUS the side tables.
type FrozenFileResidue =
    {
        // Qualified: `XParsec.FSharp.Parser` is opened here and declares its own
        // `Diagnostic`, which the bare name would bind to.
        Diagnostics: XParsec.FSharp.SemanticAnalysis.Diagnostic list
        IntrinsicReprKeys: System.Collections.Generic.IReadOnlyDictionary<TypeKey, IntrinsicReprInfo>
        GlobalValueKeys: System.Collections.Generic.IReadOnlySet<SymbolKey>
        Accessibility: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, Accessibility>
    }

/// THE frozen file: the expr, pat and decl struct-of-arrays columns, each indexable by the
/// matching `*PoolId`, plus the decl roots in source order. A bound variable IS its slot, indexable
/// by `BoundVarId`, and the file's side tables are re-expressed over that dense id space.
[<NoEquality; NoComparison>]
type FrozenPools =
    {
        /// WHICH FILE the `Anchor` columns index: expr, pat and bound variable token alike.
        Path: AssemblyFilePath
        /// The file's own interned type and key tables, which the `ty` columns index.
        /// Interning is injective on structural equality, so equal types share a row.
        Types: FrozenTypeTable
        /// The expression pool's parallel columns, each indexed by `ExprPoolId`. This one is
        /// the node's type, a row of `Types`.
        ExprTys: TypeId[]
        /// The node's anchor: a token INDEX into this file's own `Lexed`, not the token
        /// struct.
        ExprToks: Anchor[]
        /// The immediate child-expr ids, in the order the pooling walk enumerated them.
        ExprChildren: ChildColumn<ExprPoolId>
        /// The pat ids the node owns directly: a bound variable pattern, a match arm's.
        ExprPatChildren: ChildColumn<PatPoolId>
        /// The `Var` reference's bound variable, `ValueSome` only at a `Var`.
        ExprVarBoundVar: BoundVarId voption[]
        /// The residual per-case payload, which is also the node's shape tag: there is no
        /// separate tag column.
        ExprPayloads: ExprPayload[]
        /// The pattern pool's parallel columns, each indexed by `PatPoolId`. This one is
        /// the node's type, a row of `Types`.
        PatTys: TypeId[]
        /// The pattern twin of `ExprToks`.
        PatToks: Anchor[]
        /// The immediate sub-pat ids (patterns own no child expressions).
        PatChildren: ChildColumn<PatPoolId>
        /// The residual per-case payload, tag included.
        PatPayloads: PatPayload[]
        /// The declaration pool's columns, indexed by `DeclPoolId`: the decl's immediate
        /// expr/pat roots. A `Type` decl surfaces none.
        DeclExprChildren: ChildColumn<ExprPoolId>
        DeclPatChildren: ChildColumn<PatPoolId>
        /// The residual per-case payload, tag included. It also carries the decl's type,
        /// there being no node-level `ty` column.
        DeclPayloads: DeclPayload[]
        /// The pool ids of the file's `Decls`, in source order.
        Roots: DeclPoolId[]
        /// The inline vocabulary's roots: one per published template, in publication order.
        InlineTemplates: PooledInlineValue[]
        /// The resolved-specialization table's roots, indexed by the `SpecializationId` an
        /// `InlineCall` payload carries. An entry's body may reference a LATER slot, so resolve
        /// an id against the whole array, not in definition order.
        Specializations: PooledSpecialization[]
        /// The bound variable pool's two parallel columns, indexed by `BoundVarId`. This one is the
        /// identifier the source spells the bound variable with, EMPTY where none does, as at a
        /// class's `this`/`base` or a freshened inline bound variable.
        BoundVarNames: string[]
        /// The token the bound variable's name is spelled at, as an index into this file's `Lexed`.
        /// `Anchor.nowhere` for a definition site no node spells, which still has a name.
        BoundVarToks: Anchor[]
        /// The remainder of the file that has no pooled form, carried verbatim.
        Residue: FrozenFileResidue
        /// The side tables that keep a KEY, re-keyed by `BoundVarId`. A value that is not a
        /// scalar, such as a record or a list, stays here rather than becoming a `BoundVarColumn`.
        ModuleMembers: DenseTable<BoundVarId, ModuleBindingInfo>
        ClosureReprs: DenseTable<BoundVarId, ClosureRepr>
        /// Keyed by the lambda EXPRESSION: a lambda's dense id IS its `ExprPoolId`. Several
        /// rows may share a verdict, because every copy of a spliced inline body keeps the
        /// definition-site token the verdict was filed under.
        FunVerdicts: DenseTable<ExprPoolId, FunVerdict>
        GenericFnSchemes: DenseTable<BoundVarId, FrozenConstraint list>
        BindingValReprs: DenseTable<BoundVarId, PooledValRepr>
        /// A binding's typar-axis width. Most bound variables are parameters and locals,
        /// so most slots are `ValueNone`.
        BindingTyparArities: BoundVarColumn<int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrozenPools =

    /// The zero column set, a pool that is nobody's file.
    let empty: FrozenPools =
        {
            Path = AssemblyFilePath.nowhere
            Types = FrozenTypeTable.Empty
            ExprTys = [||]
            ExprToks = [||]
            ExprChildren = ChildColumn.empty
            ExprPatChildren = ChildColumn.empty
            ExprVarBoundVar = [||]
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
            BoundVarNames = [||]
            BoundVarToks = [||]
            Residue =
                {
                    Diagnostics = []
                    IntrinsicReprKeys = readOnlyDict []
                    GlobalValueKeys = System.Collections.Generic.HashSet()
                    Accessibility = readOnlyDict []
                }
            ModuleMembers = [||]
            ClosureReprs = [||]
            FunVerdicts = [||]
            GenericFnSchemes = [||]
            BindingValReprs = [||]
            BindingTyparArities = [||]
        }

    let boundVarNaming (pools: FrozenPools) (id: BoundVarId) : BoundVarNaming =
        let (BoundVarId i) = id
        BoundVarNaming.ofColumn pools.BoundVarNames.[i] id

    /// The typar-axis width recorded for `boundVar`. An empty slot reads 0 as the ANSWER, not
    /// as a fallback: a bound variable with no recorded width quantifies nothing.
    let typarArity (pools: FrozenPools) (boundVar: BoundVarId) : int =
        BoundVarColumn.tryItem pools.BindingTyparArities boundVar
        |> ValueOption.defaultValue 0

    // Qualified for the same reason `FrozenFileResidue.Diagnostics` is: the opened
    // `XParsec.FSharp.Parser` declares its own `Diagnostic`, which the bare name binds to.

    /// The findings that block emission: the error-severity ones. A tree free of these may
    /// still carry warnings, and emits with them.
    let blockingErrors (pools: FrozenPools) : XParsec.FSharp.SemanticAnalysis.Diagnostic list =
        Diagnostic.errors pools.Residue.Diagnostics
