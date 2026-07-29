namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable

// The frozen type domain in ROW form: one unit's `FrozenType`s and the `SymbolKey` cluster
// they interlock with, hash-consed into dense tables so a stored type is a 4-byte id rather
// than an object reference, and so two structurally equal types occupy ONE row.
//
// THE TABLES ARE PER UNIT. Every id here is a row index into ONE unit's own tables, and a
// unit's blob ships the tables its columns index. An id from another unit's blob means
// nothing here — not "missing", but a DIFFERENT, valid row — which is why nothing ever
// widens an id across the `FrozenSignature` seam. It is the discipline `SchemeId` and
// `BinderId` already carry.
//
// Hash-consing happens AS THE TABLE IS BUILT (`FrozenTypeTableBuilder`, driven from the
// freeze for the `ty` columns and from `FrozenCodec.flatten` for the types a PAYLOAD
// embeds, which the columns never carried). The tables only ever GROW — `OfRows` re-admits
// a stored table with every id intact — so there is no re-interning tier and no id ever
// moves. WITHIN one unit, id equality IS structural equality, which is what makes an
// overload's `MemberKey` identity an int compare rather than a walk over
// `EqArray<FrozenType>` graphs.
//
// The DU is untouched. `FrozenTypeTable` hands back the same `FrozenType` / `SymbolKey`
// values every consumer already matches on, so the ~470 `FT*` match sites across the
// codebase see a storage change and nothing else.
//
// WHY THE ROWS ARE NOT ONE FLAT `(tag, a, b)` ARRAY. Each key SORT gets its own row array
// and its own id type, so decoding is TOTAL: a `TypeHolderRow.InModule` names a `ModuleId`,
// which indexes the module rows and can index nothing else. One flat key space would make
// "this holder id is a namespace row" a runtime check every reader inherits — the exact
// class of quiet failure per-unit tables exist to rule out. The variadic cases likewise
// hold their own `EqArray<TypeId>` rather than a span into a shared array: the row is then
// its OWN intern key (two rows with equal children ARE equal), where a span is a location
// and two equal argument lists at different offsets would intern apart.

/// A string in the unit's string heap: a name, a namespace segment, an `FTUnknown`'s name,
/// a `LiteralConst.String`'s value.
[<Struct>]
type StrId = | StrId of int

/// A row of the unit's namespace table — a `NamespaceKey`'s segment path.
[<Struct>]
type NamespaceId = | NamespaceId of int

/// A row of the unit's module table (`ModuleKey`).
[<Struct>]
type ModuleId = | ModuleId of int

/// A row of the unit's type-key table (`TypeKey`).
[<Struct>]
type TypeKeyId = | TypeKeyId of int

/// A row of the unit's binding-key table (`BindingKey`).
[<Struct>]
type BindingKeyId = | BindingKeyId of int

/// A row of the unit's member-key table (`MemberKey`).
[<Struct>]
type MemberKeyId = | MemberKeyId of int

/// A row of the unit's symbol-key table (`SymbolKey`) — the three-way choice between a
/// type, a binding and a member key, interned once so a repeated `FTConst` head costs one
/// int.
[<Struct>]
type SymbolId = | SymbolId of int

/// A row of the unit's type table — the identity a frozen node's `ty` column holds. Two
/// structurally equal `FrozenType`s of one unit share this id, so `=` on two `TypeId`s of
/// the SAME unit is structural type equality.
[<Struct>]
type TypeId = | TypeId of int

/// What holds a module, in row form: the `ModuleHolder` cases over row ids. The CASE is on
/// the row and not recovered by reading the target, so a decode cannot mistake a module row
/// for a namespace row.
[<RequireQualifiedAccess>]
type ModuleHolderRow =
    | InNamespace of ns: NamespaceId
    | InModule of parent: ModuleId

/// A `ModuleKey` row.
type ModuleRow =
    { Holder: ModuleHolderRow; Name: StrId }

/// What holds a type, in row form — see `ModuleHolderRow`.
[<RequireQualifiedAccess>]
type TypeHolderRow =
    | InNamespace of ns: NamespaceId
    | InModule of parent: ModuleId
    | InType of outer: TypeKeyId

/// A `TypeKey` row. `TyparArity` stays an int: it is part of the identity, not a reference.
type TypeKeyRow =
    {
        Holder: TypeHolderRow
        Name: StrId
        TyparArity: int
    }

/// A `BindingKey` row.
type BindingKeyRow = { Decl: ModuleHolderRow; Name: StrId }

/// A `MemberKind` row — the two interface cases name their declaring interface by row id.
[<RequireQualifiedAccess>]
type MemberKindRow =
    | Method
    | Property
    | InterfaceMethod of iface: TypeKeyId
    | ExplicitInterfaceImpl of iface: TypeKeyId

/// A `MemberKey` row. `ArgSig` is where the key cluster reaches back INTO the type table —
/// the one edge that makes keys and types one interning problem rather than two.
type MemberKeyRow =
    {
        Decl: TypeKeyId
        Name: StrId
        ArgSig: EqArray<TypeId>
        MethodTyparArity: int
        Kind: MemberKindRow
    }

/// A `SymbolKey` row.
[<RequireQualifiedAccess>]
type SymbolRow =
    | Type of TypeKeyId
    | Binding of BindingKeyId
    | Member of MemberKeyId

/// A `LiteralConst` row — the string case's value goes to the heap, the int case is its own
/// value.
[<RequireQualifiedAccess>]
type LiteralRow =
    | String of StrId
    | Int of int64

/// The row form of `FTConditionalPayload`. A record, not four positional fields, for the
/// same reason the payload is one: all four branches are `TypeId`, so a `WhenTrue`/
/// `WhenFalse` swap would typecheck silently.
type ConditionalRow =
    {
        Check: TypeId
        Extends: TypeId
        WhenTrue: TypeId
        WhenFalse: TypeId
    }

/// One `FrozenType`, with every child replaced by the id it interned to. Case for case with
/// `FrozenType`, so a new frozen type constructor fails to compile here rather than
/// silently sharing a row shape with another.
///
/// The row is its own INTERN KEY: its children are ids, so structural equality on a row is
/// O(arity) and two structurally equal types produce equal rows. `Or` holds an `EqSet` for
/// exactly that reason — set-semantic equality means `A|B` and `B|A` intern to one row,
/// while the stored insertion order keeps the declared `.d.ts` order (`SemanticInfo`'s
/// `FTOr`).
[<RequireQualifiedAccess>]
type TypeRow =
    | Const of key: SymbolId * args: EqArray<TypeId>
    | Fun of arg: TypeId * result: TypeId
    | Tuple of items: EqArray<TypeId>
    | Record of key: TypeKeyId * args: EqArray<TypeId>
    | Union of key: TypeKeyId * args: EqArray<TypeId>
    | Class of key: TypeKeyId * args: EqArray<TypeId>
    | Enum of key: TypeKeyId
    | Or of members: EqSet<TypeId>
    | Literal of value: LiteralRow
    | KeyOf of ty: TypeId
    | IndexedAccess of objTy: TypeId * index: TypeId
    | Conditional of ConditionalRow
    | Typar of axis: TyparAxis * index: int
    | LocalTypar of scheme: SchemeId * index: int
    | Unknown of name: StrId

/// The unit's tables as STORED — the whole of what the codec writes and reads back, and the
/// whole of what a `FrozenTypeTable` is built from. Each array is in MINT order, so a row's
/// children are rows of a table already at least this far built; nothing here is sorted or
/// canonicalised after the fact.
///
/// `ImmutableArray`, not `[]`, because a `FrozenTypeTable` hands these out (`Rows`) while
/// still reading them: the table's whole contract is that one row materialises to ONE object
/// shared by every id that names it, and a base table's immutability is what makes the
/// overlay's two-layer type accessor sound (`TastPoolBuilder.exprTy`). A mutable array would
/// leave both resting on prose. It is also the shape the stored form wants — a value type
/// over one contiguous buffer, which is what a future mmap'd or reinterpret-cast tier can be
/// a facade over without the columns changing type.
type FrozenTypeRows =
    {
        Strings: ImmutableArray<string>
        Namespaces: ImmutableArray<EqArray<StrId>>
        Modules: ImmutableArray<ModuleRow>
        TypeKeys: ImmutableArray<TypeKeyRow>
        Bindings: ImmutableArray<BindingKeyRow>
        Members: ImmutableArray<MemberKeyRow>
        Symbols: ImmutableArray<SymbolRow>
        Types: ImmutableArray<TypeRow>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrozenTypeRows =

    /// A unit that interned nothing — the tables of `FrozenPools.empty`.
    let empty: FrozenTypeRows =
        {
            Strings = ImmutableArray.Empty
            Namespaces = ImmutableArray.Empty
            Modules = ImmutableArray.Empty
            TypeKeys = ImmutableArray.Empty
            Bindings = ImmutableArray.Empty
            Members = ImmutableArray.Empty
            Symbols = ImmutableArray.Empty
            Types = ImmutableArray.Empty
        }

/// One hash-consed table under construction: the rows in mint order, plus the index that
/// makes a repeat of a row answer with the id its first occurrence took. Eight of these are
/// the whole of the builder's state, so "intern" means the same thing in every table rather
/// than being restated per domain.
///
/// `seed` re-admits rows minted by an EARLIER builder, preserving their ids: a stored array
/// is distinct and in mint order, so replaying it through `intern` reproduces exactly the
/// indices it was written with. It is a constructor argument and not a method because it is
/// meaningful only on an empty table — and it is CHECKED, because the arrays reaching it come
/// off the wire: a stored array that repeated a row would compact here, shifting every id
/// after it, and every `ty` column entry in the file would then name a different, valid type.
/// That is the one failure per-unit tables cannot rule out by typing, so it faults instead.
type private RowTable<'row, 'id when 'row: equality and 'id: equality>(ofIndex: int -> 'id, seed: ImmutableArray<'row>)
    =
    let rows = ResizeArray<'row>(seed.Length)
    let ids = Dictionary<'row, 'id>(seed.Length)

    let intern (row: 'row) : 'id =
        match ids.TryGetValue row with
        | true, id -> id
        | false, _ ->
            let id = ofIndex rows.Count
            rows.Add row
            ids.Add(row, id)
            id

    do
        for i in 0 .. seed.Length - 1 do
            if intern seed.[i] <> ofIndex i then
                failwithf "FrozenTypeTable: stored row %d is not distinct and in mint order" i

    member _.Intern(row: 'row) : 'id = intern row

    /// A SNAPSHOT — the builder may be interned into afterwards without disturbing what was
    /// handed out, which is what lets `Rows` be taken mid-build.
    member _.ToImmutable() : ImmutableArray<'row> = ImmutableArray.CreateRange rows

/// Fill-on-demand memo over a materialisation array. The row graph is ACYCLIC but spans the
/// arrays in both directions — a member row names types, and a type row names the symbol
/// that member belongs to — so there is no array-at-a-time order to build them in, and each
/// row is instead built the first time it is asked for.
///
/// The race a concurrent read could lose is benign: two builders of one row produce
/// structurally equal values and a reference write is atomic, so a reader sees one of two
/// equal answers. Identity is the ID, never the object.
///
/// `inline`, because the seven call sites below sit under `TastPoolBuilder.exprTy`/`patTy`
/// — a per-node read on both backends' emit walks. A closure literal passed to a non-inline
/// function is allocated before the call and thrown away on the hit path, which is every
/// call but the first per row.
module private Materialise =

    let inline get<'a when 'a: not struct> (cache: 'a[]) (i: int) ([<InlineIfLambda>] build: unit -> 'a) : 'a =
        match box cache.[i] with
        | null ->
            let v = build ()
            cache.[i] <- v
            v
        | _ -> cache.[i]

/// Interns a unit's frozen types and keys as the freeze walks them. Bottom-up: a node's
/// children are interned before the node, so a row only ever names rows already minted —
/// which is what lets `FrozenTypeTable` materialise by plain recursion with no cycle check.
///
/// Not thread-safe, and belongs to ONE freeze: the ids it hands out are meaningful only
/// against the tables it is holding.
///
/// Constructed either EMPTY (a fresh freeze) or over stored rows, which it re-admits with
/// every id they were minted with intact — what a consumer that must intern MORE into a
/// unit's finished tables starts from (`FrozenCodec.flatten`, whose payloads carry types the
/// `ty` columns never did). Growth only: nothing already interned can move, so an id handed
/// out before the tables were stored still names the same row after they are extended.
[<Sealed>]
type FrozenTypeTableBuilder private (rows: FrozenTypeRows) =
    let strings = RowTable<string, StrId>(StrId, rows.Strings)
    let namespaces = RowTable<EqArray<StrId>, NamespaceId>(NamespaceId, rows.Namespaces)
    let modules = RowTable<ModuleRow, ModuleId>(ModuleId, rows.Modules)
    let typeKeys = RowTable<TypeKeyRow, TypeKeyId>(TypeKeyId, rows.TypeKeys)
    let bindings = RowTable<BindingKeyRow, BindingKeyId>(BindingKeyId, rows.Bindings)
    let members = RowTable<MemberKeyRow, MemberKeyId>(MemberKeyId, rows.Members)
    let symbols = RowTable<SymbolRow, SymbolId>(SymbolId, rows.Symbols)
    let types = RowTable<TypeRow, TypeId>(TypeId, rows.Types)

    let str (s: string) = strings.Intern s

    let namespaceKey (ns: NamespaceKey) =
        namespaces.Intern(EqArray.map str ns.Path)

    let rec moduleHolder (h: ModuleHolder) : ModuleHolderRow =
        match h with
        | ModuleHolder.InNamespace ns -> ModuleHolderRow.InNamespace(namespaceKey ns)
        | ModuleHolder.InModule parent -> ModuleHolderRow.InModule(moduleKey parent)

    and moduleKey (m: ModuleKey) : ModuleId =
        modules.Intern
            {
                Holder = moduleHolder m.Holder
                Name = str m.Name
            }

    // The key cluster and `FrozenType` are ONE recursive group, exactly as they are in
    // `SemanticInfo`: a member key's `ArgSig` holds frozen types and an `FTConst` holds a
    // symbol key, so neither can be interned without the other.
    let rec typeHolder (h: TypeHolder) : TypeHolderRow =
        match h with
        | TypeHolder.InNamespace ns -> TypeHolderRow.InNamespace(namespaceKey ns)
        | TypeHolder.InModule parent -> TypeHolderRow.InModule(moduleKey parent)
        | TypeHolder.InType outer -> TypeHolderRow.InType(typeKey outer)

    and typeKey (k: TypeKey) : TypeKeyId =
        typeKeys.Intern
            {
                Holder = typeHolder k.Holder
                Name = str k.Name
                TyparArity = k.TyparArity
            }

    and bindingKey (b: BindingKey) : BindingKeyId =
        bindings.Intern
            {
                Decl = moduleHolder b.Decl
                Name = str b.Name
            }

    and memberKind (k: MemberKind) : MemberKindRow =
        match k with
        | MemberKind.Method -> MemberKindRow.Method
        | MemberKind.Property -> MemberKindRow.Property
        | MemberKind.InterfaceMethod iface -> MemberKindRow.InterfaceMethod(typeKey iface)
        | MemberKind.ExplicitInterfaceImpl iface -> MemberKindRow.ExplicitInterfaceImpl(typeKey iface)

    and memberKey (m: MemberKey) : MemberKeyId =
        members.Intern
            {
                Decl = typeKey m.Decl
                Name = str m.Name
                ArgSig = args m.ArgSig
                MethodTyparArity = m.MethodTyparArity
                Kind = memberKind m.Kind
            }

    and symbolKey (k: SymbolKey) : SymbolId =
        symbols.Intern(
            match k with
            | SymbolKey.Type tk -> SymbolRow.Type(typeKey tk)
            | SymbolKey.Binding bk -> SymbolRow.Binding(bindingKey bk)
            | SymbolKey.Member mk -> SymbolRow.Member(memberKey mk)
        )

    and args (xs: EqArray<FrozenType>) : EqArray<TypeId> = EqArray.map frozenType xs

    and literal (v: LiteralConst) : LiteralRow =
        match v with
        | LiteralConst.String s -> LiteralRow.String(str s)
        | LiteralConst.Int n -> LiteralRow.Int n

    and frozenType (t: FrozenType) : TypeId =
        types.Intern(
            match t with
            | FTConst(key, xs) -> TypeRow.Const(symbolKey key, args xs)
            | FTFun(arg, result) -> TypeRow.Fun(frozenType arg, frozenType result)
            | FTTuple items -> TypeRow.Tuple(args items)
            | FTRecord(key, xs) -> TypeRow.Record(typeKey key, args xs)
            | FTUnion(key, xs) -> TypeRow.Union(typeKey key, args xs)
            | FTClass(key, xs) -> TypeRow.Class(typeKey key, args xs)
            | FTEnum key -> TypeRow.Enum(typeKey key)
            // The member set carries into an `EqSet<TypeId>`, so the row's own set-semantic
            // equality is what collapses `A|B` with `B|A`. Members are already distinct as
            // `FrozenType`s, and interning is injective on structural equality, so the id
            // set has the same cardinality — the stored order is the declared one.
            | FTOr ms -> TypeRow.Or(EqSet.ofSeq (seq { for m in ms -> frozenType m }))
            | FTLiteral value -> TypeRow.Literal(literal value)
            | FTKeyOf ty -> TypeRow.KeyOf(frozenType ty)
            | FTIndexedAccess(objTy, index) -> TypeRow.IndexedAccess(frozenType objTy, frozenType index)
            | FTConditional payload ->
                TypeRow.Conditional
                    {
                        Check = frozenType payload.Check
                        Extends = frozenType payload.Extends
                        WhenTrue = frozenType payload.WhenTrue
                        WhenFalse = frozenType payload.WhenFalse
                    }
            | FTTypar(axis, index) -> TypeRow.Typar(axis, index)
            | FTLocalTypar(scheme, index) -> TypeRow.LocalTypar(scheme, index)
            | FTUnknown name -> TypeRow.Unknown(str name)
        )

    /// The id `t` interns to in this unit's type table, minting rows for whatever of it is
    /// new. Idempotent: the same type always answers with the same id.
    member _.Intern(t: FrozenType) : TypeId = frozenType t

    /// The id `k` interns to in this unit's symbol-key table — see `Intern`.
    member _.InternSymbol(k: SymbolKey) : SymbolId = symbolKey k

    /// The id `k` interns to in this unit's type-key table. A nominal key reaches the wire
    /// on its own (a `PooledTypeDecl`'s identity, a constrained interface) and not only
    /// inside a type, so it needs an entry point of its own — see `Intern`.
    member _.InternTypeKey(k: TypeKey) : TypeKeyId = typeKey k

    /// The id `m` interns to in this unit's module table — see `InternTypeKey`.
    member _.InternModule(m: ModuleKey) : ModuleId = moduleKey m

    /// A builder for a unit that has interned nothing yet.
    new() = FrozenTypeTableBuilder(FrozenTypeRows.empty)

    /// A builder holding `rows` already — see the type's own doc.
    static member OfRows(rows: FrozenTypeRows) : FrozenTypeTableBuilder = FrozenTypeTableBuilder(rows)

    /// The tables as they stand. Snapshots the row arrays; the builder may be interned into
    /// afterwards, which simply produces a longer table next time.
    member _.Rows: FrozenTypeRows =
        {
            Strings = strings.ToImmutable()
            Namespaces = namespaces.ToImmutable()
            Modules = modules.ToImmutable()
            TypeKeys = typeKeys.ToImmutable()
            Bindings = bindings.ToImmutable()
            Members = members.ToImmutable()
            Symbols = symbols.ToImmutable()
            Types = types.ToImmutable()
        }

/// A unit's interned type and key tables, READ SIDE: it resolves an id back to the very
/// `FrozenType` / `SymbolKey` the DU declares, so a consumer of the columns matches on the
/// same values it always did.
///
/// One row materialises to ONE object, shared by every id-holder that names it. That is the
/// second half of hash-consing's payoff: the columns hold 4-byte ids, and the types those
/// ids denote are allocated once per DISTINCT type rather than once per occurrence.
[<Sealed>]
type FrozenTypeTable private (rows: FrozenTypeRows) =
    let namespaceCache: NamespaceKey[] = Array.zeroCreate rows.Namespaces.Length
    let moduleCache: ModuleKey[] = Array.zeroCreate rows.Modules.Length
    let typeKeyCache: TypeKey[] = Array.zeroCreate rows.TypeKeys.Length
    let bindingCache: BindingKey[] = Array.zeroCreate rows.Bindings.Length
    let memberCache: MemberKey[] = Array.zeroCreate rows.Members.Length
    let symbolCache: SymbolKey[] = Array.zeroCreate rows.Symbols.Length
    let typeCache: FrozenType[] = Array.zeroCreate rows.Types.Length

    let str (StrId i) = rows.Strings.[i]

    let namespaceKey (NamespaceId i) =
        Materialise.get
            namespaceCache
            i
            (fun () ->
                {
                    Path = EqArray.map str rows.Namespaces.[i]
                }
            )

    let rec moduleHolder (h: ModuleHolderRow) : ModuleHolder =
        match h with
        | ModuleHolderRow.InNamespace ns -> ModuleHolder.InNamespace(namespaceKey ns)
        | ModuleHolderRow.InModule parent -> ModuleHolder.InModule(moduleKey parent)

    and moduleKey (ModuleId i) : ModuleKey =
        Materialise.get
            moduleCache
            i
            (fun () ->
                let row = rows.Modules.[i]

                {
                    Holder = moduleHolder row.Holder
                    Name = str row.Name
                }
            )

    let rec typeHolder (h: TypeHolderRow) : TypeHolder =
        match h with
        | TypeHolderRow.InNamespace ns -> TypeHolder.InNamespace(namespaceKey ns)
        | TypeHolderRow.InModule parent -> TypeHolder.InModule(moduleKey parent)
        | TypeHolderRow.InType outer -> TypeHolder.InType(typeKey outer)

    and typeKey (TypeKeyId i) : TypeKey =
        Materialise.get
            typeKeyCache
            i
            (fun () ->
                let row = rows.TypeKeys.[i]

                {
                    Holder = typeHolder row.Holder
                    Name = str row.Name
                    TyparArity = row.TyparArity
                }
            )

    and bindingKey (BindingKeyId i) : BindingKey =
        Materialise.get
            bindingCache
            i
            (fun () ->
                let row = rows.Bindings.[i]

                {
                    Decl = moduleHolder row.Decl
                    Name = str row.Name
                }
            )

    and memberKind (k: MemberKindRow) : MemberKind =
        match k with
        | MemberKindRow.Method -> MemberKind.Method
        | MemberKindRow.Property -> MemberKind.Property
        | MemberKindRow.InterfaceMethod iface -> MemberKind.InterfaceMethod(typeKey iface)
        | MemberKindRow.ExplicitInterfaceImpl iface -> MemberKind.ExplicitInterfaceImpl(typeKey iface)

    and memberKey (MemberKeyId i) : MemberKey =
        Materialise.get
            memberCache
            i
            (fun () ->
                let row = rows.Members.[i]

                {
                    Decl = typeKey row.Decl
                    Name = str row.Name
                    ArgSig = args row.ArgSig
                    MethodTyparArity = row.MethodTyparArity
                    Kind = memberKind row.Kind
                }
            )

    and symbolKey (SymbolId i) : SymbolKey =
        Materialise.get
            symbolCache
            i
            (fun () ->
                match rows.Symbols.[i] with
                | SymbolRow.Type tk -> SymbolKey.Type(typeKey tk)
                | SymbolRow.Binding bk -> SymbolKey.Binding(bindingKey bk)
                | SymbolRow.Member mk -> SymbolKey.Member(memberKey mk)
            )

    and args (xs: EqArray<TypeId>) : EqArray<FrozenType> = EqArray.map frozenType xs

    and literal (v: LiteralRow) : LiteralConst =
        match v with
        | LiteralRow.String s -> LiteralConst.String(str s)
        | LiteralRow.Int n -> LiteralConst.Int n

    // Rebuilt DIRECTLY, never through `FrozenType.MkUnion` (which flattens / dedupes /
    // collapses): the interned row is already canonical, and routing a materialisation
    // through a normaliser would make the table's contents unrecoverable from their own ids.
    and frozenType (TypeId i) : FrozenType =
        Materialise.get
            typeCache
            i
            (fun () ->
                match rows.Types.[i] with
                | TypeRow.Const(key, xs) -> FTConst(symbolKey key, args xs)
                | TypeRow.Fun(arg, result) -> FTFun(frozenType arg, frozenType result)
                | TypeRow.Tuple items -> FTTuple(args items)
                | TypeRow.Record(key, xs) -> FTRecord(typeKey key, args xs)
                | TypeRow.Union(key, xs) -> FTUnion(typeKey key, args xs)
                | TypeRow.Class(key, xs) -> FTClass(typeKey key, args xs)
                | TypeRow.Enum key -> FTEnum(typeKey key)
                | TypeRow.Or ms -> FTOr(EqSet.ofSeq (seq { for m in ms -> frozenType m }))
                | TypeRow.Literal value -> FTLiteral(literal value)
                | TypeRow.KeyOf ty -> FTKeyOf(frozenType ty)
                | TypeRow.IndexedAccess(objTy, index) -> FTIndexedAccess(frozenType objTy, frozenType index)
                | TypeRow.Conditional row ->
                    FTConditional
                        {
                            Check = frozenType row.Check
                            Extends = frozenType row.Extends
                            WhenTrue = frozenType row.WhenTrue
                            WhenFalse = frozenType row.WhenFalse
                        }
                | TypeRow.Typar(axis, index) -> FTTypar(axis, index)
                | TypeRow.LocalTypar(scheme, index) -> FTLocalTypar(scheme, index)
                | TypeRow.Unknown name -> FTUnknown(str name)
            )

    /// The stored rows — what the codec writes. The materialised side is derived from these
    /// and is never serialized.
    ///
    /// Handed out DIRECTLY, no copy: `FrozenTypeRows` is immutable, so sharing the arrays a
    /// live table is reading through costs nothing and risks nothing.
    member _.Rows: FrozenTypeRows = rows

    /// Resolve an id to the value it names — one operation, four id types, the ID's TYPE
    /// choosing the table. That is what the eight distinct id types buy on the read side:
    /// a ninth table adds a row type and an overload, not a ninth method name to learn, and
    /// `t.[id]` cannot reach the wrong table because no id indexes two of them.
    ///
    /// The inverses of `FrozenTypeTableBuilder`'s four `Intern*` entry points, in the same
    /// order. THE read is the first: `TastPoolBuilder.exprTy`/`patTy` resolve the `ty`
    /// columns through it, so a consumer meets a `FrozenType` and never a row.
    member _.Item
        with get (id: TypeId): FrozenType = frozenType id

    member _.Item
        with get (id: SymbolId): SymbolKey = symbolKey id

    member _.Item
        with get (id: TypeKeyId): TypeKey = typeKey id

    member _.Item
        with get (id: ModuleId): ModuleKey = moduleKey id

    /// How many DISTINCT types the unit interned. The row count, not an occurrence count:
    /// the columns may name any of them any number of times.
    member _.TypeCount: int = rows.Types.Length

    static member OfRows(rows: FrozenTypeRows) : FrozenTypeTable = FrozenTypeTable(rows)

    /// The tables of a unit that interned nothing — `FrozenPools.empty`'s, and an overlay
    /// pool's, neither of which owns a frozen file's types.
    static member Empty: FrozenTypeTable = FrozenTypeTable(FrozenTypeRows.empty)
