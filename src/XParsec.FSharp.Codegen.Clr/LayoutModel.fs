namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Every ranged-table row enumerated as data; handle = position in the layout.

/// How an abstract member's curried signature maps onto metadata parameters.
[<AutoOpen>]
module internal AbstractMemberShape =

    /// `FTFun('A, 'B)` ⇒ `(['A], 'B)`.
    let rec uncurry (t: FrozenType) : FrozenType list * FrozenType =
        match t with
        | FTFun(a, b) ->
            let ps, r = uncurry b
            a :: ps, r
        | _ -> [], t

    let isUnitTy t =
        match t with
        | FTUnit -> true
        | _ -> false

    /// An abstract member's metadata parameter types: `abstract M : unit -> X` is a
    /// *no-arg* method, so a sole leading `unit` argument is dropped.
    let abstractMethodParamTys (m: Frozen.TAbstractMethod) : FrozenType list =
        let paramTys, _ = uncurry m.Signature

        match paramTys with
        | [ single ] when isUnitTy single -> []
        // `abstract Invoke : 'A * 'B -> 'C` has one tupled domain but emits as 2 params,
        // as its conforming `member _.Invoke(a, b)` does; flatten so the runtime can bind
        // the impl to the slot.
        | [ FTTuple elems ] when elems.Length >= 2 -> EqArray.toList elems
        | _ -> paramTys

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

    // Non-virtual, which is what makes `call` bind: an augmentation member declares no
    // slot to dispatch through.
    let instanceMethodAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

    // A union's `get_Tag`: non-virtual, so a match arm binds it by `call`. `SpecialName`
    // marks it a property getter.
    let tagGetterAttrs = instanceMethodAttrs ||| MethodAttributes.SpecialName

    /// A method row bound to a `Property` row by `MethodSemantics` carries `SpecialName`,
    /// which is how a reflecting consumer tells an accessor from a method beside it.
    let accessorAttrs (kind: TMemberKind) (attrs: MethodAttributes) : MethodAttributes =
        match kind with
        | TMemberKind.Method -> attrs
        | TMemberKind.Property
        | TMemberKind.Accessor _ -> attrs ||| MethodAttributes.SpecialName

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

    // A new virtual slot a hierarchy union's base declares for its case types to implement.
    // The runtime binds it to the `InterfaceImpl` by name + signature, as `ifaceEqualsAttrs`
    // does; an implementor overrides it, so it drops `Final`.
    let abstractIfaceSlotAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Abstract
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot

    // `GetHashCode` re-abstracted on a hierarchy union's base: `Object`'s slot, so no
    // `NewSlot`.
    let abstractOverrideAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Abstract
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig

    /// The attrs one nominal's synthesised structural rows take, selected once per type.
    /// `Equals(object)` and `CompareTo(object)` are absent because they carry a body in
    /// every regime.
    type StructuralRowAttrs =
        {
            /// `GetHashCode`, which reuses `Object`'s slot.
            ObjectSlot: MethodAttributes
            /// The typed `Equals(Self)` / `CompareTo(Self)` and `Format`.
            InterfaceSlot: MethodAttributes
        }

    /// The nominal supplies every structural body itself.
    let concreteStructuralAttrs =
        {
            ObjectSlot = overrideMethodAttrs
            InterfaceSlot = ifaceEqualsAttrs
        }

    /// A hierarchy union's base declares the slots and its case types supply the bodies.
    let abstractStructuralAttrs =
        {
            ObjectSlot = abstractOverrideAttrs
            InterfaceSlot = abstractIfaceSlotAttrs
        }

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

    /// The `MethodDef` name a member emits under, concrete and abstract alike, derived
    /// from the member's kind: an accessor spells `get_`/`set_` over the property it
    /// accesses; a method keeps its bare name.
    let memberMetaName (name: string) (kind: TMemberKind) : string =
        match kind with
        | TMemberKind.Method -> name
        | TMemberKind.Property -> AccessorNames.getterName name
        | TMemberKind.Accessor(prop, TAccessorRole.Getter) -> AccessorNames.getterName prop
        | TMemberKind.Accessor(prop, TAccessorRole.Setter) -> AccessorNames.setterName prop

/// Identity of one `TypeDefinition` row in the layout.
[<RequireQualifiedAccess>]
type internal TypeSlotKey =
    | ModulePseudo
    | Nominal of SymbolKey
    /// A hierarchy union's per-case type, nested in the union's own `TypeDef`.
    | UnionCase of SymbolKey * case: string
    | Closure of name: string
    | ModuleClass of Emit.ModuleClassKey
    | Program

/// Which `Add*` recipe the writer uses for a `TypeSlot`.
[<RequireQualifiedAccess>]
type internal TypeSlotKind =
    | ModulePseudo
    | Interface
    /// `valueKind` selects reference vs `[<Struct>]` value type (flips the
    /// `System.ValueType` base) and stamps `IsReadOnly`. `regime` decides
    /// abstract-vs-sealed, so it takes no `isSealed`: a hierarchy regime
    /// (`UnionRegime.isHierarchy`) is abstract and the rest are sealed.
    | Union of valueKind: UnionValueKind * regime: UnionRegime
    /// One case of a hierarchy union: a sealed nested class extending the union, holding
    /// that case's payload fields.
    | UnionCase
    /// `valueKind` selects reference vs `[<Struct>]` value type. Always sealed.
    | Record of valueKind: RecordValueKind
    /// `isSealed` reflects `[<Sealed>]`; `valueKind` selects reference vs `[<Struct>]`
    /// value type (flips sequential layout + `Sealed` + the `ValueType` base) vs
    /// `[<IsByRefLike>]`, which additionally stamps `IsByRefLikeAttribute`.
    | Class of isSealed: bool * valueKind: ClassValueKind
    | Closure
    /// A numeric enum: a sealed `System.Enum` subclass with no methods, a
    /// special-name `value__` instance field, and one `static literal` field per case.
    | Enum
    /// A string / mixed enum: a sealed `[<Struct>]` value type over a single field
    /// (`string`, or `obj` for a mixed enum), with a `.ctor` setting it, per-case
    /// `static initonly` fields, and a `.cctor` constructing them.
    | StructEnum
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
    /// A union's `private initonly` discriminant, read by the union's own bodies and by
    /// its case types; every other reader goes through `MethodKey.UnionGetTag`.
    | UnionTag of SymbolKey
    /// A case's payload field: on the case's own `TypeDef` in a hierarchy regime, and
    /// co-resident with every other case's on the union itself in a flat one.
    | UnionCaseField of SymbolKey * case: string * index: int
    /// A reference union's `private static initonly` singleton for a NULLARY case, typed
    /// as the union. Constructed once by the union's `.cctor`; the case factory `ldsfld`s
    /// it.
    | UnionCaseSingleton of SymbolKey * case: string
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

/// One synthesised structural member on a hierarchy union's case type. The `Case`-typed
/// pair holds the field walk; the `Union`-typed overrides are the guards that reach it
/// through the slot the base declares abstract.
[<RequireQualifiedAccess>]
type internal UnionCaseSlot =
    /// `override bool Equals(U other)` — `isinst` this case, then `EqualsCase`.
    | EqualsUnion
    /// `bool Equals(<Case> other)` — the case's own field walk.
    | EqualsCase
    /// `override int GetHashCode()` — the case's tag as a literal, then its own fields.
    | GetHashCode
    /// `override int CompareTo(U other)` — this case ⇒ `CompareToCase`, else the ordinal
    /// difference.
    | CompareToUnion
    /// `int CompareTo(<Case> other)` — the case's own field walk.
    | CompareToCase
    /// `override void Format(IFormatSink)` — `BeginCase`, the case's own fields, `EndCase`.
    | Format

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal UnionCaseSlot =

    /// The metadata name of the method row a slot emits under.
    let metaName (slot: UnionCaseSlot) : string =
        match slot with
        | UnionCaseSlot.GetHashCode -> "GetHashCode"
        | UnionCaseSlot.EqualsUnion
        | UnionCaseSlot.EqualsCase -> "Equals"
        | UnionCaseSlot.CompareToUnion
        | UnionCaseSlot.CompareToCase -> "CompareTo"
        | UnionCaseSlot.Format -> "Format"

    /// The slots a hierarchy union's case type implements, in row order. The base declares
    /// each of these abstract.
    let required (s: StructuralMembers) : UnionCaseSlot list =
        [
            if s.Equality then
                UnionCaseSlot.GetHashCode
                UnionCaseSlot.EqualsUnion
                UnionCaseSlot.EqualsCase

            if s.Comparison then
                UnionCaseSlot.CompareToUnion
                UnionCaseSlot.CompareToCase

            if s.Format then
                UnionCaseSlot.Format
        ]

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
    /// `get_Tag`, the public accessor for a union's private `_tag`. Declared exactly where
    /// `UnionRegime.hasTag` holds.
    | UnionGetTag of SymbolKey
    /// A hierarchy union case type's `.ctor(payload…)`, which chains the union's own
    /// `.ctor`, passing this case's tag where the base declares one.
    | UnionCaseCtor of SymbolKey * case: string
    | UnionCaseStructural of SymbolKey * case: string * UnionCaseSlot
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

/// Identity of one `Property` row in the layout.
[<RequireQualifiedAccess>]
type internal PropertyKey =
    /// A union's `Tag`, whose getter is `MethodKey.UnionGetTag`.
    | UnionTag of SymbolKey
    /// A property a nominal type declares, keyed by the property's name and staticness,
    /// which both halves share. A static and an instance property of one name are two rows.
    | Declared of SymbolKey * prop: string * isStatic: bool

/// One accessor, as the grouping into properties reads it.
type internal AccessorRow =
    {
        /// The `MethodDef` row this accessor emits under.
        Method: MethodKey
        /// The member's own name; the parameterless-getter form spells its property with it.
        Name: string
        Kind: TMemberKind
        IsStatic: bool
        /// The accessor's own metadata parameters: a getter's parameters index the property;
        /// a setter's index it and carry the value last.
        ParamTys: FrozenType list
        RetTy: FrozenType
    }

/// One `Property` row, with the accessor rows a `MethodSemantics` row binds to it. At least
/// one half is always `ValueSome`.
type internal PropertySlot =
    {
        Key: PropertyKey
        Name: string
        /// `HASTHIS` on the row's signature, which must agree with the accessors' own.
        IsInstance: bool
        /// An indexed property's index parameters; empty for a plain one.
        IndexTys: FrozenType list
        ValueTy: FrozenType
        Getter: MethodKey voption
        Setter: MethodKey voption
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal PropertySlot =

    /// The `Property` rows a member list declares, in first-accessor order. Accessors of one
    /// property at one staticness share a row.
    let ofAccessors (key: SymbolKey) (accessors: AccessorRow list) : PropertySlot list =
        accessors
        |> List.choose (fun a ->
            match TMemberKind.propertyOf a.Name a.Kind with
            | ValueSome(prop, role) -> Some((prop, a.IsStatic), (a, role))
            | ValueNone -> None
        )
        |> List.groupBy fst
        |> List.map (fun ((prop, isStatic), halves) ->
            let pick (wanted: TAccessorRole) : AccessorRow voption =
                match
                    halves
                    |> List.tryPick (fun (_, (a, role)) -> if role = wanted then Some a else None)
                with
                | Some a -> ValueSome a
                | None -> ValueNone

            let getter = pick TAccessorRole.Getter
            let setter = pick TAccessorRole.Setter

            // A getter's parameters index the property and its return is the value; a
            // setter carries the value last, behind the same index parameters.
            let setterShape =
                setter
                |> ValueOption.map (fun s ->
                    match List.rev s.ParamTys with
                    | value :: revIndex -> List.rev revIndex, value
                    | [] -> failwithf "Layout: property '%s' declares a setter taking no value" prop
                )

            let indexTys, valueTy =
                match getter, setterShape with
                | ValueSome g, ValueSome(sIndex, sValue) ->
                    // Unification has conformed the halves.
                    if g.ParamTys <> sIndex || g.RetTy <> sValue then
                        failwithf "Layout: the getter and setter of property '%s' disagree on its type" prop

                    g.ParamTys, g.RetTy
                | ValueSome g, ValueNone -> g.ParamTys, g.RetTy
                | ValueNone, ValueSome shape -> shape
                | ValueNone, ValueNone -> failwithf "Layout: property '%s' groups no accessor" prop

            {
                Key = PropertyKey.Declared(key, prop, isStatic)
                Name = prop
                IsInstance = not isStatic
                IndexTys = indexTys
                ValueTy = valueTy
                Getter = getter |> ValueOption.map (fun a -> a.Method)
                Setter = setter |> ValueOption.map (fun a -> a.Method)
            }
        )

/// One `MethodDef` row: the i-th entry of `AssemblyLayout.Methods` is table row i+1.
/// The signature / body / params are bound late (`PreparedMethod`).
type internal MethodRow =
    {
        Key: MethodKey
        Name: string
        Attrs: MethodAttributes
    }

/// A prepared method's IL. `Abstract` is the slot an interface, or a hierarchy union's
/// base, declares for an implementor to supply; `WriteMethods` pairs it against the row's
/// `MethodAttributes.Abstract` bit.
[<RequireQualifiedAccess>]
type internal PreparedBody =
    | Abstract
    | At of offset: int

/// A bound method row ready to write: signature and body built at the Bind / Prepare
/// phase against resolved handles.
type internal PreparedMethod =
    {
        Signature: BlobBuilder
        Body: PreparedBody
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
        /// The `PropertyMap` row's range. Empty ⇒ the type gets no `PropertyMap` row at all.
        Properties: PropertySlot list
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
        /// The full `Property` table in row order, which each `PropertyMap` row ranges over.
        Properties: PropertySlot list
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
        FirstProperties: Dictionary<TypeSlotKey, PropertyDefinitionHandle>
        MethodDefs: Dictionary<MethodKey, MethodDefinitionHandle>
        PropertyDefs: Dictionary<PropertyKey, PropertyDefinitionHandle>
        /// Total ranged-table rows the layout owns; the writer checks the real builder
        /// counts against these.
        TotalFields: int
        TotalMethods: int
        TotalProperties: int
    }

    member this.TypeDefOf(key: TypeSlotKey) : TypeDefinitionHandle = this.TypeDefs.[key]
    member this.FirstFieldOf(key: TypeSlotKey) : FieldDefinitionHandle = this.FirstFields.[key]
    member this.FirstMethodOf(key: TypeSlotKey) : MethodDefinitionHandle = this.FirstMethods.[key]
    member this.FirstPropertyOf(key: TypeSlotKey) : PropertyDefinitionHandle = this.FirstProperties.[key]
    member this.MethodDefOf(key: MethodKey) : MethodDefinitionHandle = this.MethodDefs.[key]
    member this.PropertyDefOf(key: PropertyKey) : PropertyDefinitionHandle = this.PropertyDefs.[key]
