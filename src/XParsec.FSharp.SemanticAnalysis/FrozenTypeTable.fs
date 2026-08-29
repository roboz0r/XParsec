namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable

// THE TABLES ARE PER FILE: every id is a row index into ONE file's own tables, so an id from
// another file's blob identifies a DIFFERENT, valid row rather than a missing one. Nothing ever
// widens an id across the `FrozenSignature` seam. Within one file, id equality IS structural.

/// A string in the file's string heap.
[<Struct>]
type StrId = | StrId of int

[<Struct>]
type NamespaceId = | NamespaceId of int

[<Struct>]
type ModuleId = | ModuleId of int

[<Struct>]
type TypeKeyId = | TypeKeyId of int

[<Struct>]
type BindingKeyId = | BindingKeyId of int

[<Struct>]
type MemberKeyId = | MemberKeyId of int

/// A row of the file's symbol-key table: a type, a binding or a member key, interned once
/// so a repeated `FTConst` type constructor costs one int.
[<Struct>]
type SymbolId = | SymbolId of int

/// A row of the file's type table. Two structurally equal `FrozenType`s of one file share
/// this id, so `=` on two `TypeId`s of the SAME file is structural type equality.
[<Struct>]
type TypeId = | TypeId of int

/// A row of the file's file-path table: the declaring file a specialization entry's anchors
/// point into. Interned because a realistic program draws many entries from ONE such file.
[<Struct>]
type FilePathId = | FilePathId of int

[<RequireQualifiedAccess>]
type ModuleContainerRow =
    | InNamespace of ns: NamespaceId
    | InModule of parent: ModuleId

type ModuleRow =
    {
        Container: ModuleContainerRow
        Name: StrId
    }

[<RequireQualifiedAccess>]
type TypeContainerRow =
    | InNamespace of ns: NamespaceId
    | InModule of parent: ModuleId
    | InType of outer: TypeKeyId

/// `TyparArity` stays an int: it is part of the identity, not a reference.
type TypeKeyRow =
    {
        Container: TypeContainerRow
        Name: StrId
        TyparArity: int
    }

type BindingKeyRow =
    {
        Decl: ModuleContainerRow
        Name: StrId
    }

[<RequireQualifiedAccess>]
type MemberKindRow =
    | Method
    | Property
    | InterfaceMethod of iface: TypeKeyId
    | ExplicitInterfaceImpl of iface: TypeKeyId

/// `ArgSig` is the one edge from the key cluster back INTO the type table, which is why
/// keys and types intern as a single recursive problem rather than two.
type MemberKeyRow =
    {
        Decl: TypeKeyId
        Name: StrId
        ArgSig: EqArray<TypeId>
        MethodTyparArity: int
        Kind: MemberKindRow
    }

[<RequireQualifiedAccess>]
type SymbolRow =
    | Type of TypeKeyId
    | Binding of BindingKeyId
    | Member of MemberKeyId

[<RequireQualifiedAccess>]
type LiteralRow =
    | String of StrId
    | Int of int64

/// `UnknownReason` with its payloads interned.
[<RequireQualifiedAccess>]
type UnknownReasonRow =
    | UndefinedName of name: StrId
    | UnfreezableExternal of what: StrId
    | UnresolvedTypar
    | Deferred
    | ArityMismatch
    | NoValueType

/// A record, not four positional fields: all four are `TypeId`, so a `WhenTrue`/`WhenFalse`
/// swap would typecheck silently.
type ConditionalRow =
    {
        Check: TypeId
        Extends: TypeId
        WhenTrue: TypeId
        WhenFalse: TypeId
    }

/// One `FrozenType` with every child replaced by the id it interned to. The row is its own
/// INTERN KEY, because children being ids makes structural equality O(arity). `Or` holds an
/// `EqSet` so `A|B` and `B|A` intern to one row, insertion order keeping the declared order.
[<RequireQualifiedAccess>]
type TypeRow =
    | Const of key: TypeKeyId * args: EqArray<TypeId>
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
    | Unknown of reason: UnknownReasonRow

/// Two entries drawn from one declaring file carry the same pair and intern to one row.
type FilePathRow = { Assembly: StrId; Relative: StrId }

/// The file's tables as STORED. Each array is in MINT order, so a row's children are rows of
/// a table already at least this far built; nothing here is sorted or canonicalised after the
/// fact. `ImmutableArray` because a live table hands these out while still reading them.
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
        FilePaths: ImmutableArray<FilePathRow>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrozenTypeRows =

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
            FilePaths = ImmutableArray.Empty
        }

/// Rows in mint order, plus the index mapping a repeated row to the id of its first
/// occurrence. `seed` re-admits stored rows with their ids intact, and is CHECKED: a repeated
/// stored row would compact, shifting every id after it onto a different, valid type.
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

    /// A SNAPSHOT: the builder may be interned into afterwards without disturbing what was
    /// handed out, so this may be taken mid-build.
    member _.ToImmutable() : ImmutableArray<'row> = ImmutableArray.CreateRange rows

/// Fill-on-demand memo. The row graph is acyclic, but a member row references types while a
/// type row references the symbol that member belongs to, so there is no array-at-a-time order.
/// `inline`: the hit path must not allocate the closure.
module private Materialise =

    let inline get<'a when 'a: not struct> (cache: 'a[]) (i: int) ([<InlineIfLambda>] build: unit -> 'a) : 'a =
        match box cache.[i] with
        | null ->
            let v = build ()
            cache.[i] <- v
            v
        | _ -> cache.[i]

/// Interns a file's frozen types and keys BOTTOM-UP: a node's children are interned before
/// the node, so a row only ever points to rows already minted, which is what lets the read side
/// materialise by plain recursion with no cycle check. Not thread-safe; belongs to ONE freeze.
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
    let filePaths = RowTable<FilePathRow, FilePathId>(FilePathId, rows.FilePaths)

    let str (s: string) = strings.Intern s

    // Not in the key/type recursive group below: nothing in the type domain reaches a
    // file path, nor the reverse.
    let filePath (f: AssemblyFilePath) =
        filePaths.Intern
            {
                Assembly = str (AssemblyName.toStored f.Assembly)
                Relative = str f.Relative.Name
            }

    let namespaceKey (ns: NamespaceKey) =
        namespaces.Intern(EqArray.map str ns.Path)

    let rec moduleContainer (h: ModuleContainer) : ModuleContainerRow =
        match h with
        | ModuleContainer.InNamespace ns -> ModuleContainerRow.InNamespace(namespaceKey ns)
        | ModuleContainer.InModule parent -> ModuleContainerRow.InModule(moduleKey parent)

    and moduleKey (m: ModuleKey) : ModuleId =
        modules.Intern
            {
                Container = moduleContainer m.Container
                Name = str m.Name
            }

    // One recursive group: a member key's `ArgSig` holds frozen types and an `FTConst` holds
    // a symbol key, so neither can be interned without the other.
    let rec typeContainer (h: TypeContainer) : TypeContainerRow =
        match h with
        | TypeContainer.InNamespace ns -> TypeContainerRow.InNamespace(namespaceKey ns)
        | TypeContainer.InModule parent -> TypeContainerRow.InModule(moduleKey parent)
        | TypeContainer.InType outer -> TypeContainerRow.InType(typeKey outer)

    and typeKey (k: TypeKey) : TypeKeyId =
        typeKeys.Intern
            {
                Container = typeContainer k.Container
                Name = str k.Name
                TyparArity = k.TyparArity
            }

    and bindingKey (b: BindingKey) : BindingKeyId =
        bindings.Intern
            {
                Decl = moduleContainer b.Decl
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

    and unknownReason (r: UnknownReason) : UnknownReasonRow =
        match r with
        | UnknownReason.UndefinedName name -> UnknownReasonRow.UndefinedName(str name)
        | UnknownReason.UnfreezableExternal what -> UnknownReasonRow.UnfreezableExternal(str what)
        | UnknownReason.UnresolvedTypar -> UnknownReasonRow.UnresolvedTypar
        | UnknownReason.Deferred -> UnknownReasonRow.Deferred
        | UnknownReason.ArityMismatch -> UnknownReasonRow.ArityMismatch
        | UnknownReason.NoValueType -> UnknownReasonRow.NoValueType

    and frozenType (t: FrozenType) : TypeId =
        types.Intern(
            match t with
            | FTConst(key, xs) -> TypeRow.Const(typeKey key, args xs)
            | FTFun(arg, result) -> TypeRow.Fun(frozenType arg, frozenType result)
            | FTTuple items -> TypeRow.Tuple(args items)
            | FTRecord(key, xs) -> TypeRow.Record(typeKey key, args xs)
            | FTUnion(key, xs) -> TypeRow.Union(typeKey key, args xs)
            | FTClass(key, xs) -> TypeRow.Class(typeKey key, args xs)
            | FTEnum key -> TypeRow.Enum(typeKey key)
            // Interning is injective on structural equality, so distinct `FrozenType`
            // members give distinct ids: the set keeps its cardinality and its order.
            | FTOr ds -> TypeRow.Or(EqSet.ofSeq (seq { for d in ds.Disjuncts -> frozenType d }))
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
            | FTUnknown reason -> TypeRow.Unknown(unknownReason reason)
        )

    /// Mints rows for whatever of `t` is new. Idempotent: the same type always yields the
    /// same id.
    member _.Intern(t: FrozenType) : TypeId = frozenType t

    member _.InternSymbol(k: SymbolKey) : SymbolId = symbolKey k

    /// A nominal key reaches the wire on its own, not only inside a type, so it needs an
    /// entry point of its own.
    member _.InternTypeKey(k: TypeKey) : TypeKeyId = typeKey k

    member _.InternModule(m: ModuleKey) : ModuleId = moduleKey m

    member _.InternFilePath(f: AssemblyFilePath) : FilePathId = filePath f

    new() = FrozenTypeTableBuilder(FrozenTypeRows.empty)

    /// Re-admits `rows` with every id they were minted with intact. This is where a consumer
    /// that must intern MORE into a file's finished tables starts.
    static member OfRows(rows: FrozenTypeRows) : FrozenTypeTableBuilder = FrozenTypeTableBuilder(rows)

    /// Snapshots the row arrays; the builder may be interned into afterwards, which simply
    /// produces a longer table next time.
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
            FilePaths = filePaths.ToImmutable()
        }

/// A file's interned type and key tables, READ SIDE: an id resolves back to the very
/// `FrozenType` / `SymbolKey` the DU declares. One row materialises to ONE object, shared by
/// every id that resolves to it, so a type is allocated once per DISTINCT type.
[<Sealed>]
type FrozenTypeTable private (rows: FrozenTypeRows) =
    let namespaceCache: NamespaceKey[] = Array.zeroCreate rows.Namespaces.Length
    let moduleCache: ModuleKey[] = Array.zeroCreate rows.Modules.Length
    let typeKeyCache: TypeKey[] = Array.zeroCreate rows.TypeKeys.Length
    let bindingCache: BindingKey[] = Array.zeroCreate rows.Bindings.Length
    let memberCache: MemberKey[] = Array.zeroCreate rows.Members.Length
    let symbolCache: SymbolKey[] = Array.zeroCreate rows.Symbols.Length
    let typeCache: FrozenType[] = Array.zeroCreate rows.Types.Length
    let pathCache: AssemblyFilePath[] = Array.zeroCreate rows.FilePaths.Length

    let str (StrId i) = rows.Strings.[i]

    let filePath (FilePathId i) : AssemblyFilePath =
        Materialise.get
            pathCache
            i
            (fun () ->
                let row = rows.FilePaths.[i]

                {
                    Assembly = AssemblyName.ofStored (str row.Assembly)
                    Relative = AssemblyFileId.ofStored (str row.Relative)
                }
            )

    let namespaceKey (NamespaceId i) =
        Materialise.get
            namespaceCache
            i
            (fun () ->
                {
                    Path = EqArray.map str rows.Namespaces.[i]
                }
            )

    let rec moduleContainer (h: ModuleContainerRow) : ModuleContainer =
        match h with
        | ModuleContainerRow.InNamespace ns -> ModuleContainer.InNamespace(namespaceKey ns)
        | ModuleContainerRow.InModule parent -> ModuleContainer.InModule(moduleKey parent)

    and moduleKey (ModuleId i) : ModuleKey =
        Materialise.get
            moduleCache
            i
            (fun () ->
                let row = rows.Modules.[i]

                {
                    Container = moduleContainer row.Container
                    Name = str row.Name
                }
            )

    let rec typeContainer (h: TypeContainerRow) : TypeContainer =
        match h with
        | TypeContainerRow.InNamespace ns -> TypeContainer.InNamespace(namespaceKey ns)
        | TypeContainerRow.InModule parent -> TypeContainer.InModule(moduleKey parent)
        | TypeContainerRow.InType outer -> TypeContainer.InType(typeKey outer)

    and typeKey (TypeKeyId i) : TypeKey =
        Materialise.get
            typeKeyCache
            i
            (fun () ->
                let row = rows.TypeKeys.[i]

                {
                    Container = typeContainer row.Container
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
                    Decl = moduleContainer row.Decl
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

    and unknownReason (r: UnknownReasonRow) : UnknownReason =
        match r with
        | UnknownReasonRow.UndefinedName name -> UnknownReason.UndefinedName(str name)
        | UnknownReasonRow.UnfreezableExternal what -> UnknownReason.UnfreezableExternal(str what)
        | UnknownReasonRow.UnresolvedTypar -> UnknownReason.UnresolvedTypar
        | UnknownReasonRow.Deferred -> UnknownReason.Deferred
        | UnknownReasonRow.ArityMismatch -> UnknownReason.ArityMismatch
        | UnknownReasonRow.NoValueType -> UnknownReason.NoValueType

    // Rebuilt DIRECTLY, never through a normalising constructor (one that flattens / dedupes
    // / collapses): the interned row is already canonical, and normalising here would make
    // the table's contents unrecoverable from their own ids.
    and frozenType (TypeId i) : FrozenType =
        Materialise.get
            typeCache
            i
            (fun () ->
                match rows.Types.[i] with
                | TypeRow.Const(key, xs) -> FTConst(typeKey key, args xs)
                | TypeRow.Fun(arg, result) -> FTFun(frozenType arg, frozenType result)
                | TypeRow.Tuple items -> FTTuple(args items)
                | TypeRow.Record(key, xs) -> FTRecord(typeKey key, args xs)
                | TypeRow.Union(key, xs) -> FTUnion(typeKey key, args xs)
                | TypeRow.Class(key, xs) -> FTClass(typeKey key, args xs)
                | TypeRow.Enum key -> FTEnum(typeKey key)
                | TypeRow.Or ds -> FTOr(FTDisjuncts.OfSeq(seq { for d in ds -> frozenType d }))
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
                | TypeRow.Unknown reason -> FTUnknown(unknownReason reason)
            )

    /// The stored rows, which the codec writes; the materialised side is never serialized.
    /// Handed out DIRECTLY, no copy: `FrozenTypeRows` is immutable, so sharing the arrays a
    /// live table is reading through costs nothing.
    member _.Rows: FrozenTypeRows = rows

    /// Resolve an id to the value it identifies. The ID's TYPE chooses the table, so `t.[id]`
    /// cannot reach the wrong one and a further table adds an overload rather than a method
    /// name to learn. The inverses of the builder's five `Intern*` entry points.
    member _.Item
        with get (id: TypeId): FrozenType = frozenType id

    member _.Item
        with get (id: SymbolId): SymbolKey = symbolKey id

    member _.Item
        with get (id: TypeKeyId): TypeKey = typeKey id

    member _.Item
        with get (id: ModuleId): ModuleKey = moduleKey id

    member _.Item
        with get (id: FilePathId): AssemblyFilePath = filePath id

    static member OfRows(rows: FrozenTypeRows) : FrozenTypeTable = FrozenTypeTable(rows)

    /// The tables of a file that interned nothing, an overlay pool's for one, since it
    /// does not own a frozen file's types.
    static member Empty: FrozenTypeTable = FrozenTypeTable(FrozenTypeRows.empty)
