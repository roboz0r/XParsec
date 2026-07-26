namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Every ranged-table row enumerated as data; handle = position in the layout.
// One source of truth for row order; the writer walks it mechanically and
// forward-handle arithmetic derives from prefix sums over these lists only.

/// The method attribute sets, shared by the layout's method enumeration and
/// the `prepare` phase (so the writer's predicted-vs-actual row check compares
/// like with like).
[<AutoOpen>]
module internal MethodAttrSets =

    /// An abstract interface method (no body).
    let abstractMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Abstract
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot

    let staticFactoryAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Static
        ||| MethodAttributes.HideBySig

    let staticMethodAttrs = staticFactoryAttrs

    // Non-virtual `public hidebysig` instance method (the union/record/class is
    // sealed, so `call` dispatch is correct).
    let instanceMethodAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

    // A synthesised `Object.Equals`/`GetHashCode` override: reuse the base
    // virtual slot (no `NewSlot`) so name + signature matching makes it the
    // override.
    let overrideMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig

    // Typed `IEquatable<Self>::Equals(Self)` / `IComparable<Self>::CompareTo`
    // and interface-impl members: a *new* virtual slot (`Object` has none to
    // reuse), `Final` since sealed. The runtime binds it to the
    // `InterfaceImpl` by name + signature.
    let ifaceEqualsAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.Final

    let ctorAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.SpecialName
        ||| MethodAttributes.RTSpecialName

    let cctorAttrs =
        MethodAttributes.Private
        ||| MethodAttributes.Static
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.SpecialName
        ||| MethodAttributes.RTSpecialName

    // A closure derives from `System.Object` and *implements* the
    // `Vesper.Fun\`2::Invoke` interface slot by name + signature; `Final`
    // because a sealed closure has no further overrides.
    let invokeAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.Final

    /// A property is emitted (and referenced) as `get_<name>`; a method keeps
    /// its name.
    let memberMetaName (mem: TastAccessor.TypeMember) : string =
        match mem.Kind with
        | TMemberKind.Property -> "get_" + mem.Name
        | TMemberKind.Method -> mem.Name

/// Identity of one `TypeDefinition` row in the layout. Reuses existing
/// identities: nominal types by `SymbolKey`, closures by their
/// synthesized unique name, holders by `Emit.HolderKey`. Structural equality;
/// a collision is a bug that should fail loudly (dictionary add throws).
[<RequireQualifiedAccess>]
type internal TypeSlotKey =
    | ModulePseudo
    | Nominal of SymbolKey
    | Closure of name: string
    | Holder of Emit.HolderKey
    | Program

/// Which `Add*` recipe the writer uses for a `TypeSlot` — the only decision
/// left at write time.
[<RequireQualifiedAccess>]
type internal TypeSlotKind =
    | ModulePseudo
    | Interface
    | Union
    /// `valueKind` selects reference vs `[<Struct>]` value type (flips the
    /// `System.ValueType` base) — a record is always sealed and never byref-like.
    | Record of valueKind: ClassValueKind
    /// `isSealed` reflects `[<Sealed>]`; `valueKind` selects reference vs
    /// `[<Struct>]` value type (flips sequential layout + `Sealed` + the
    /// `ValueType` base) vs `[<IsByRefLike>]` byref-like (additionally stamps the
    /// `IsByRefLikeAttribute` custom attribute).
    | Class of isSealed: bool * valueKind: ClassValueKind
    | Closure
    /// A numeric enum: a sealed `System.Enum` subclass — no methods, a
    /// special-name `value__` instance field, and one `static literal` field per case.
    | Enum
    /// A string / mixed enum: a sealed `[<Struct>]` value type over a
    /// single field (`string`, or `obj` when `isMixed`), with a `.ctor` setting it,
    /// per-case `static initonly` fields, and a `.cctor` constructing them. `isMixed`
    /// is carried only for documentation symmetry with the writer; the base is
    /// always `System.ValueType`.
    | StructEnum of isMixed: bool
    /// A named module holder; `HasCctor` ⇔ it owns module values (drops
    /// `BeforeFieldInit`).
    | Holder of hasCctor: bool
    /// The anonymous "Program" holder (holder-less fns + `Main` + the top-level value fields). `hasCctor` ⇔ it owns leading-prefix values (drops
    /// `BeforeFieldInit`, its `.cctor` runs before `Main`).
    | Program of hasCctor: bool

/// Identity of one `Field` row in the layout — who resolves this
/// handle at `Bind` time. Structural; a collision fails loudly.
[<RequireQualifiedAccess>]
type internal FieldKey =
    | UnionTag of SymbolKey
    | UnionCaseField of SymbolKey * case: string * index: int
    | RecordField of SymbolKey * name: string
    /// A class primary-ctor parameter's backing field.
    | ClassCtorParamField of SymbolKey * name: string
    /// An explicit `val [mutable] x: T` instance field.
    | ClassInstanceField of SymbolKey * name: string
    /// An instance-`let` binder's backing field (a class-preamble `let`, stored by the
    /// primary `.ctor`). Distinct from `ClassInstanceField` — the `val` form is the
    /// user's own surface, this is compiler-generated storage — but both resolve by
    /// name at a `this.x` use site.
    | ClassLetField of SymbolKey * name: string
    /// A `static let` backing field.
    | ClassStaticField of SymbolKey * name: string
    /// A numeric enum's special-name `value__` instance field (its underlying
    /// integral storage).
    | EnumValueField of SymbolKey
    /// A numeric enum's `static literal` case field (`E::A`); also a string/mixed
    /// enum's `public static initonly` case field (`.cctor`-initialised, holding the
    /// constructed wrapper).
    | EnumCaseField of SymbolKey * case: string
    /// A string/mixed enum's single instance field (the wrapped `string` / `obj`).
    | EnumBackingField of SymbolKey
    | ClosureCapture of closure: string * index: int
    /// A non-capturing, monomorphic closure's `static readonly` singleton field —
    /// the one cached instance every construction site `ldsfld`s.
    | ClosureCached of closure: string
    /// A module-level value's `public static` holder field, keyed by its
    /// `SymbolKey` (declaring holder + emitted name) so the ONE combined field-def
    /// map stays injective across compilation units and under entry-file shadowing —
    /// a bare per-file `NodeKey` collides on both axes (`Emit.ModuleValue.SymbolKey`).
    | ModuleValue of SymbolKey

/// One `Field` row: the i-th entry of `AssemblyLayout.Fields` is table row
/// i+1. The layout stores only def-table rows; whether a *use site* routes
/// through a `MemberRef` on an open self-`TypeSpec` (generic types/closures) stays a `Bind`-time policy.
type internal FieldSlot =
    {
        Key: FieldKey
        Name: string
        Attrs: FieldAttributes
        Ty: FrozenType
        /// A *generic* closure's capture field encodes its signature inside the
        /// ambient closure-typar scope (the body's typars re-project onto the
        /// closure class's slots) — the writer brackets the `AddField` call in
        /// `EnterClosureTyparScope`/`ExitClosureTyparScope`. `ValueSome d` carries
        /// the closure's declaring-typar offset; `ValueNone` ⇒ no closure scope.
        ClosureScope: int voption
    }

/// Identity of one `MethodDef` row in the layout. Indexed cases
/// (`Member`, `SecondaryCtor`, `InterfaceMethod`) use the position in the
/// declaring type's own list so same-named overloads can't collide; a class's
/// interface-impl members continue the `Member` index past its own members.
[<RequireQualifiedAccess>]
type internal MethodKey =
    | InterfaceMethod of SymbolKey * index: int
    /// A nominal type's first `.ctor` (union nullary / record / class primary).
    | NominalCtor of SymbolKey
    /// A class's synthesised `static let` `.cctor`.
    | NominalCctor of SymbolKey
    | SecondaryCtor of SymbolKey * index: int
    | UnionFactory of SymbolKey * case: string
    /// An augmentation member — index over `members @ ifaceMembers`.
    | Member of SymbolKey * index: int
    | EqGetHashCode of SymbolKey
    | EqEqualsObj of SymbolKey
    | EqEqualsTyped of SymbolKey
    | CmpCompareToTyped of SymbolKey
    | CmpCompareToObj of SymbolKey
    /// The synthesised `IStructuralFormattable.Format(IFormatSink)` (`%A`).
    | FmtFormat of SymbolKey
    /// A synthesised capability co-slot (`CoSlot`) — the BCL members a capability's
    /// platform face inherits but its member surface never declared.
    | CapCoSlot of SymbolKey * CoSlot
    | ClosureCtor of closure: string
    | ClosureInvoke of closure: string
    /// A non-capturing, monomorphic closure's `.cctor` — `newobj`s the closure once
    /// and `stsfld`s it into `FieldKey.ClosureCached`. Present only
    /// for a cached closure; a capturing/generic closure has none.
    | ClosureCctor of closure: string
    | HolderCctor of Emit.HolderKey
    /// The anonymous "Program" holder's `.cctor` — initialises the
    /// leading-prefix top-level values; at most one per assembly.
    | ProgramCctor
    /// A top-level function lowered to a static method, keyed by its `SymbolKey`
    /// (declaring holder + emitted name) so the ONE combined method-def map stays
    /// injective across compilation units and under entry-file shadowing — a bare
    /// per-file `NodeKey` collides on both axes (`Emit.StaticFn.SymbolKey`).
    | StaticFn of SymbolKey
    | Main

/// One `MethodDef` row: the i-th entry of `AssemblyLayout.Methods` is table
/// row i+1. Carries the row identity (name + attrs); the signature / body /
/// params are bound late (`PreparedMethod`), against resolved handles.
type internal MethodRow =
    {
        Key: MethodKey
        Name: string
        Attrs: MethodAttributes
    }

/// A bound method row ready to write: signature and body built at the Bind /
/// Prepare phase against resolved handles (body-stream order is free — only
/// the `MethodDef` row order matters, and the writer takes that from
/// `AssemblyLayout.Methods`).
type internal PreparedMethod =
    {
        Signature: BlobBuilder
        /// `-1` ⇒ abstract (no body).
        BodyOffset: int
        ParamNames: string list
        /// `GenericParam` rows owned by this method (metadata names, quote
        /// already dropped), added by the writer once the real handle exists.
        MethodTypars: string list
    }

/// The Prepare-minted handles a `TypeDefinition` row needs at write time,
/// keyed by `TypeSlotKey`. Pre-minted because a generic parent's / interface's
/// `TypeSpec` encoding can depend on ambient state only live during the
/// type's Prepare window (e.g. the closure-typar scope).
type internal TypeRowExtras =
    {
        /// One `InterfaceImpl` entity handle per implemented interface.
        Interfaces: EntityHandle list
        /// The IL `TypeDefinition.BaseType` handle: `Object` for unions /
        /// records / closures and parent-less classes, the parent's resolved
        /// handle for an `inherit` clause, `System.ValueType` for a struct.
        BaseType: EntityHandle
    }

/// One `TypeDefinition` row. It carries NO row counts: a type's field / method rows
/// are the lists on its `TypeNode`, and every prefix sum is taken over those — so
/// there is nothing here that could disagree with them.
type internal TypeSlot =
    {
        Key: TypeSlotKey
        Kind: TypeSlotKind
        /// The `TypeDef` namespace column. EMPTY for a nested type — a nested type's
        /// namespace is its enclosing type's, which is the CLR rule.
        Namespace: string
        /// Metadata name, already arity-suffixed (`SymbolKeyOps.arityName`). ONE
        /// segment: the holder chain lives in `TypeNode.Enclosing` (a `NestedClass`
        /// row), never in the name.
        MetaName: string
        /// Metadata-layer typar names (leading F# quote dropped).
        Typars: string list
    }

/// One node of the emitted type HIERARCHY: a `TypeDefinition` row together with the
/// ranged-table rows it owns and the types nested inside it. The `TypeDef` table IS
/// the pre-order flattening of the roots, and `AssemblyLayout`'s `Types` / `Fields` /
/// `Methods` are `List.collect`s over that flattening — so a type's range and the rows
/// in it agree BY DERIVATION, not by a count kept in step by hand.
///
/// `Enclosing` is `ValueNone` for a root (`<Module>`, a namespace-level type, a
/// closure, a root module's holder, `Program`) and `ValueSome` for a type the CLR
/// nests — exactly the types that get a `NestedClass` row and nested visibility.
type internal TypeNode =
    {
        Slot: TypeSlot
        Enclosing: TypeSlotKey voption
        Fields: FieldSlot list
        Methods: MethodRow list
        Nested: TypeNode list
    }

/// One compilation unit's contribution to the assembly, as data — everything a
/// unit produces on its own, BEFORE the single `<Module>` pseudo-type and the
/// single Program holder are minted (both belong to the assembly, not a unit, so
/// `Layout.combine` mints them once and `Layout.buildUnit` never does). A future
/// multi-unit driver builds one of these per source unit and hands the list to
/// `combine`.
type internal UnitLayout =
    {
        /// This unit's placeable ROOT nodes — its namespace-level nominals, its
        /// closures, its root-module holders (each carrying its own nested subtree)
        /// — with the `<Module>` and Program roots deliberately absent. `combine`
        /// concatenates these across units between the one `<Module>` head and the
        /// one Program tail.
        Roots: TypeNode list
        /// This unit's contribution to the completeness check's built-key set: every
        /// nominal, closure and holder key it built, independently of how they were
        /// placed in the tree. `combine` adds the `<Module>` and Program keys and asks
        /// the set question once over the whole assembly.
        BuiltKeys: TypeSlotKey list
        Lowered: TastAccessor.DeclId list
        Plan: HolderPlan
        Closures: EmitTypes.Closure list
        ClosureByNode: Dictionary<ExprPoolId, EmitTypes.Closure>
        Partitioned: PartitionedTypeDecls
        /// This unit's source-lambda value-struct closure verdicts, on ITS OWN pool's
        /// lambda id space. It rides the unit rather than a single ctor-level table
        /// because an `ExprPoolId` is only meaningful RELATIVE to the pool that issued
        /// it: two units' pools both number from 0, so a foreign unit's id would not
        /// miss — it would silently name a DIFFERENT node. Per-unit scoping is what makes
        /// the bare id a sound key here; merging these across units would not be.
        FunVerdicts: IReadOnlyDictionary<ExprPoolId, FunVerdict>
        /// Whether this unit carries the entry point (`Main`). `buildUnit` leaves it FALSE
        /// — the OutputKind decision belongs to the whole assembly, not a file — and
        /// `combine` stamps it TRUE on the single entry unit (an executable's last file)
        /// and FALSE on all others, so `PrepareMain` fires exactly once.
        EmitEntryPoint: bool
    }

/// The planned assembly: the ranged-table rows as data, plus the lowering
/// products the plan was computed from (computed once here, consumed by the
/// emission passes — they must never re-derive them; see
/// `feedback_walkelems_order_ctor_params`).
type internal AssemblyLayout =
    {
        /// The `TypeDef` table: the PRE-ORDER flattening of the type hierarchy, so each
        /// holder is immediately followed by the types it holds. Index 0 = `<Module>`;
        /// the i-th entry is TypeDef row i+1.
        Types: TypeNode list
        /// The full `Field` table in row order — the fields of `Types`, in `Types`
        /// order. Derived, never assembled a second time.
        Fields: FieldSlot list
        /// The full `MethodDef` table in row order — the methods of `Types`, in `Types`
        /// order. Derived, never assembled a second time.
        Methods: MethodRow list
        /// The Program slot's presence is a layout decision: exe (`Main`) or
        /// holder-less fns. True iff some unit carries the entry point.
        EmitEntryPoint: bool
        /// The per-unit products this layout was combined from — one per source file.
        /// Every per-unit datum the emission passes need (lowered decls, holder plan,
        /// closures, partition, closure verdicts, the entry flag) lives here, keyed so a
        /// unit's bodies resolve their own file-local nodes; the shared registries and the
        /// one combined row space live on the Assembler.
        Units: UnitLayout list
    }

/// The resolved handle lookup derived from the layout once: `TypeSlotKey` →
/// `TypeDefinitionHandle` (position), plus each type's first-field /
/// first-method handle from prefix-summing `FieldCount`/`MethodCount`.
/// **No handle arithmetic exists outside this derivation.**
type internal LayoutHandles =
    {
        TypeDefs: Dictionary<TypeSlotKey, TypeDefinitionHandle>
        FirstFields: Dictionary<TypeSlotKey, FieldDefinitionHandle>
        FirstMethods: Dictionary<TypeSlotKey, MethodDefinitionHandle>
        MethodDefs: Dictionary<MethodKey, MethodDefinitionHandle>
        /// Total ranged-table rows the layout owns — writer-level row-count
        /// checks compare the real builder counts against these.
        TotalFields: int
        TotalMethods: int
    }

    member this.TypeDefOf(key: TypeSlotKey) : TypeDefinitionHandle = this.TypeDefs.[key]
    member this.FirstFieldOf(key: TypeSlotKey) : FieldDefinitionHandle = this.FirstFields.[key]
    member this.FirstMethodOf(key: TypeSlotKey) : MethodDefinitionHandle = this.FirstMethods.[key]
    member this.MethodDefOf(key: MethodKey) : MethodDefinitionHandle = this.MethodDefs.[key]
