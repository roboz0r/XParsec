namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Every ranged-table row enumerated as data; handle = position in the layout.

[<AutoOpen>]
module internal MethodAttrSets =

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

    // Non-virtual: the union / record / class is sealed, so `call` dispatch is correct.
    let instanceMethodAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

    // An `Object.Equals` / `GetHashCode` override: no `NewSlot`, so it reuses the
    // base virtual slot, matched by name + signature.
    let overrideMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig

    // Typed `IEquatable<Self>::Equals(Self)` / `IComparable<Self>::CompareTo` and
    // interface-impl members: a *new* virtual slot (`Object` has none to reuse), `Final`
    // since sealed. The runtime binds it to the `InterfaceImpl` by name + signature.
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

    // A closure *implements* the `Vesper.Fun\`2::Invoke` interface slot by name +
    // signature; `Final` because a sealed closure has no further overrides.
    let invokeAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.Final

    let memberMetaName (mem: TastAccessor.TypeMember) : string =
        match mem.Kind with
        | TMemberKind.Property -> "get_" + mem.Name
        | TMemberKind.Method -> mem.Name

/// Identity of one `TypeDefinition` row in the layout.
[<RequireQualifiedAccess>]
type internal TypeSlotKey =
    | ModulePseudo
    | Nominal of SymbolKey
    | Closure of name: string
    | ModuleClass of Emit.ModuleClassKey
    | Program

/// Which `Add*` recipe the writer uses for a `TypeSlot`.
[<RequireQualifiedAccess>]
type internal TypeSlotKind =
    | ModulePseudo
    | Interface
    | Union
    /// `valueKind` selects reference vs `[<Struct>]` value type (flips the
    /// `System.ValueType` base). No `isSealed`, because a record is always sealed
    /// and never byref-like.
    | Record of valueKind: ClassValueKind
    /// `isSealed` reflects `[<Sealed>]`; `valueKind` selects reference vs `[<Struct>]`
    /// value type (flips sequential layout + `Sealed` + the `ValueType` base) vs
    /// `[<IsByRefLike>]`, which additionally stamps `IsByRefLikeAttribute`.
    | Class of isSealed: bool * valueKind: ClassValueKind
    | Closure
    /// A numeric enum: a sealed `System.Enum` subclass with no methods, a
    /// special-name `value__` instance field, and one `static literal` field per case.
    | Enum
    /// A string / mixed enum: a sealed `[<Struct>]` value type over a single field
    /// (`string`, or `obj` when `isMixed`), with a `.ctor` setting it, per-case
    /// `static initonly` fields, and a `.cctor` constructing them.
    | StructEnum of isMixed: bool
    /// A named module's class; `hasCctor` ⇔ it owns module values (drops
    /// `BeforeFieldInit`).
    | ModuleClass of hasCctor: bool
    /// The anonymous "Program" class (the fns of no named module + `Main` + the top-level
    /// value fields). `hasCctor` ⇔ it owns leading-prefix values (drops `BeforeFieldInit`;
    /// its `.cctor` runs before `Main`).
    | Program of hasCctor: bool

/// Identity of one `Field` row in the layout.
[<RequireQualifiedAccess>]
type internal FieldKey =
    | UnionTag of SymbolKey
    | UnionCaseField of SymbolKey * case: string * index: int
    | RecordField of SymbolKey * name: string
    /// A class primary-ctor parameter's backing field.
    | ClassCtorParamField of SymbolKey * name: string
    /// An explicit `val [mutable] x: T` instance field.
    | ClassInstanceField of SymbolKey * name: string
    /// A class primary-ctor `let` bound variable's compiler-generated backing field, stored by the
    /// primary `.ctor`. Distinct from `ClassInstanceField` (the user's own `val`), but
    /// both resolve by name at a `this.x` use site.
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
    /// A non-capturing, monomorphic closure's `static readonly` singleton field, the
    /// one cached instance every construction site `ldsfld`s.
    | ClosureCached of closure: string
    /// A module-level value's `public static` field, keyed by `SymbolKey`
    /// (declaring module class + emitted name) so the combined field-def map stays injective
    /// across files and under entry-file shadowing.
    | ModuleValue of SymbolKey

/// One `Field` row: the i-th entry of `AssemblyLayout.Fields` is table row i+1.
type internal FieldSlot =
    {
        Key: FieldKey
        Name: string
        Attrs: FieldAttributes
        Ty: FrozenType
        /// `ValueSome d` ⇒ encode this field's signature inside the closure-typar scope
        /// at declaring-typar offset `d`, so the body's typars re-project onto the closure
        /// class's slots (a *generic* closure's captures); `ValueNone` ⇒ no such scope.
        ClosureScope: int voption
    }

/// Identity of one `MethodDef` row in the layout. Indexed cases use the position in the
/// declaring type's own list, so same-named overloads can't collide; a class's
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
    /// An augmentation member; `index` runs over `members @ ifaceMembers`.
    | Member of SymbolKey * index: int
    | EqGetHashCode of SymbolKey
    | EqEqualsObj of SymbolKey
    | EqEqualsTyped of SymbolKey
    | CmpCompareToTyped of SymbolKey
    | CmpCompareToObj of SymbolKey
    /// The synthesised `IStructuralFormattable.Format(IFormatSink)` (`%A`).
    | FmtFormat of SymbolKey
    /// A synthesised capability co-slot (`CoSlot`), one of the BCL members a capability's
    /// platform interface inherits but its member surface never declared.
    | CapCoSlot of SymbolKey * CoSlot
    | ClosureCtor of closure: string
    | ClosureInvoke of closure: string
    /// A non-capturing, monomorphic closure's `.cctor`, which `newobj`s the closure once
    /// and `stsfld`s it into `ClosureCached`. A capturing / generic closure has none.
    | ClosureCctor of closure: string
    | ModuleClassCctor of Emit.ModuleClassKey
    /// The anonymous "Program" class's `.cctor`, which initialises the
    /// leading-prefix top-level values; at most one per assembly.
    | ProgramCctor
    /// A top-level function lowered to a static method, keyed by `SymbolKey` (declaring
    /// module class + emitted name) so the combined method-def map stays injective across files
    /// and under entry-file shadowing.
    | StaticFn of SymbolKey
    | Main

/// One `MethodDef` row: the i-th entry of `AssemblyLayout.Methods` is table row i+1.
/// The signature / body / params are bound late (`PreparedMethod`).
type internal MethodRow =
    {
        Key: MethodKey
        Name: string
        Attrs: MethodAttributes
    }

/// A bound method row ready to write: signature and body built at the Bind / Prepare
/// phase against resolved handles.
type internal PreparedMethod =
    {
        Signature: BlobBuilder
        /// `-1` ⇒ abstract (no body).
        BodyOffset: int
        ParamNames: string list
        /// `GenericParam` rows owned by this method (metadata names, quote already dropped).
        MethodTypars: string list
    }

/// The Prepare-minted handles a `TypeDefinition` row needs at write time, keyed by
/// `TypeSlotKey`. Pre-minted because a generic parent's / interface's `TypeSpec` encoding
/// can depend on ambient state live only during that type's Prepare window.
type internal TypeRowExtras =
    {
        /// One `InterfaceImpl` entity handle per implemented interface.
        Interfaces: EntityHandle list
        /// The IL `TypeDefinition.BaseType` handle: `Object` for unions / records /
        /// closures and parent-less classes, the parent's resolved handle for an
        /// `inherit` clause, `System.ValueType` for a struct.
        BaseType: EntityHandle
    }

/// One `TypeDefinition` row. Its field / method rows are the lists on its `TypeNode`.
type internal TypeSlot =
    {
        Key: TypeSlotKey
        Kind: TypeSlotKind
        /// The `TypeDef` namespace column. EMPTY for a nested type, because the CLR takes a
        /// nested type's namespace from its enclosing type.
        Namespace: string
        /// Metadata name, already arity-suffixed (`Map\`2`). ONE segment: the containment chain
        /// lives in `TypeNode.Enclosing` (a `NestedClass` row), never in the name.
        MetaName: string
        /// Metadata-layer typar names (leading F# quote dropped).
        Typars: string list
    }

/// One node of the emitted type HIERARCHY: a `TypeDefinition` row together with the
/// ranged-table rows it owns and the types nested inside it. The `TypeDef` table is the
/// pre-order flattening of the roots.
type internal TypeNode =
    {
        Slot: TypeSlot
        /// `ValueNone` for a root (`<Module>`, a namespace-level type, a closure, a root
        /// module's class, `Program`); `ValueSome` for a type the CLR nests: exactly the
        /// types that get a `NestedClass` row and nested visibility.
        Enclosing: TypeSlotKey voption
        Fields: FieldSlot list
        Methods: MethodRow list
        Nested: TypeNode list
    }

/// One file's contribution to the assembly, as data: everything a file produces on its
/// own, BEFORE the single `<Module>` pseudo-type and the single Program class, which
/// belong to the assembly and are minted once when the files are combined.
type internal FileLayout =
    {
        /// This file's placeable ROOT nodes: its namespace-level nominals, its closures,
        /// its root-module classes (each carrying its own nested subtree). The `<Module>`
        /// and Program roots are deliberately absent.
        Roots: TypeNode list
        /// Every nominal, closure and module-class key this file built, independently of how they
        /// were placed in the tree. The completeness check compares these against the placed keys.
        BuiltKeys: TypeSlotKey list
        Lowered: TastAccessor.DeclId list
        Plan: ModuleClassPlan
        Closures: EmitTypes.Closure list
        ClosureByNode: Dictionary<TastAccessor.ExprId, EmitTypes.Closure>
        Partitioned: PartitionedTypeDecls
        /// This file's source-lambda value-struct closure verdicts, keyed by the lambda
        /// NODE, the id together with the pool that issued it, so an id from another
        /// file's pool misses instead of silently denoting a different node.
        FunVerdicts: IReadOnlyDictionary<TastAccessor.ExprId, FunVerdict>
        /// Whether this file carries the entry point (`Main`): TRUE on the single entry
        /// file (an executable's last), FALSE on every other.
        EmitEntryPoint: bool
    }

/// The planned assembly: the ranged-table rows as data, plus the lowering products the
/// plan was computed from. The emission passes consume these and must never re-derive
/// them.
type internal AssemblyLayout =
    {
        /// The `TypeDef` table: the PRE-ORDER flattening of the type hierarchy, so each
        /// module class is immediately followed by the types it holds. Index 0 = `<Module>`;
        /// the i-th entry is TypeDef row i+1.
        Types: TypeNode list
        /// The full `Field` table in row order: the fields of `Types`, in `Types` order.
        Fields: FieldSlot list
        /// The full `MethodDef` table in row order: the methods of `Types`, in `Types` order.
        Methods: MethodRow list
        /// The Program slot's presence is a layout decision: exe (`Main`) or
        /// Program-class fns. True iff some file carries the entry point.
        EmitEntryPoint: bool
        /// The per-file products this layout was combined from, one per source file, so a
        /// file's bodies resolve their own file-local nodes.
        Files: FileLayout list
    }

/// The resolved handle lookup derived from the layout once: `TypeSlotKey` →
/// `TypeDefinitionHandle` (position), plus each type's first-field / first-method handle
/// from prefix-summing the row lists its node owns.
type internal LayoutHandles =
    {
        TypeDefs: Dictionary<TypeSlotKey, TypeDefinitionHandle>
        FirstFields: Dictionary<TypeSlotKey, FieldDefinitionHandle>
        FirstMethods: Dictionary<TypeSlotKey, MethodDefinitionHandle>
        MethodDefs: Dictionary<MethodKey, MethodDefinitionHandle>
        /// Total ranged-table rows the layout owns; the writer checks the real builder
        /// counts against these.
        TotalFields: int
        TotalMethods: int
    }

    member this.TypeDefOf(key: TypeSlotKey) : TypeDefinitionHandle = this.TypeDefs.[key]
    member this.FirstFieldOf(key: TypeSlotKey) : FieldDefinitionHandle = this.FirstFields.[key]
    member this.FirstMethodOf(key: TypeSlotKey) : MethodDefinitionHandle = this.FirstMethods.[key]
    member this.MethodDefOf(key: MethodKey) : MethodDefinitionHandle = this.MethodDefs.[key]
