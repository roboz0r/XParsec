namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open EmitJsCapabilities

/// Member partitioning and nominal-`type`-decl collection for the JS backend — the
/// `WalkCtx`-free, `buildExpr`-free front half of emission. `collectTypes` reads the
/// un-lowered `type` decls (which `TastLower.lower` drops) into the emission list, the
/// lookup tables, the deferred pending-class/union records, and the free-member list;
/// `EmitJs` opens this module and builds the deferred method bodies once the full
/// `WalkCtx` exists.
module EmitJsTypes =

    /// A record type's JS shape: the emitted class `Name` and its `Fields` in
    /// *declaration* order — `RecordCons`/`RecordClone` reorder source args to match.
    type JsRecordInfo =
        {
            Name: string
            Fields: string list
            /// `ValueSome home` for an external record: its class lives in the home's
            /// module, so a construction site imports it rather than relying on a local
            /// class. `ValueNone` for a record declared in this file.
            Home: JsHome voption
        }

    /// A union type's JS shape: the emitted base-class `Name` and its cases keyed by
    /// F# case name. `UnionCons` and union patterns look up subclass + field order here.
    type JsUnionInfo =
        {
            Name: string
            Cases: System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>
            /// `ValueSome home` for an external union: its case classes live in the home's
            /// module, so a `UnionCons` site imports them rather than relying on a local
            /// class. `ValueNone` for a union declared in this file.
            Home: JsHome voption
        }

    // ---- Unions --------------------------------------------------------------

    /// A union case's declaration-order field names: named fields verbatim; a lone
    /// positional becomes `Item`; multiple positionals become `Item1`/`Item2`/….
    let synthFieldNames (fieldNames: string voption list) : string list =
        match fieldNames with
        | [ ValueSome n ] -> [ n ]
        | [ ValueNone ] -> [ "Item" ]
        | many ->
            many
            |> List.mapi (fun i nm ->
                match nm with
                | ValueSome n -> n
                | ValueNone -> "Item" + string (i + 1)
            )

    /// Build a `JsUnionInfo` for `baseName` with `(caseName, fieldNames)` in declaration
    /// order: tag = declaration index, subclass = `<baseName>_<case>`.
    let buildUnionInfo
        (home: JsHome voption)
        (baseName: string)
        (cases: (string * string voption list) list)
        : JsUnionInfo * JsUnionCaseDecl list =
        let caseDecls =
            cases
            |> List.mapi (fun tag (caseName, fieldNames) ->
                {
                    CaseName = caseName
                    ClassName = baseName + "_" + caseName
                    Tag = tag
                    Fields = synthFieldNames fieldNames
                }
            )

        let table = System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>()

        for c in caseDecls do
            table.[c.CaseName] <- c

        {
            Name = baseName
            Cases = table
            Home = home
        },
        caseDecls

    // ---- Member partition ----------------------------------------------------

    /// The split of a nominal type's members across the JS emission forms. The four
    /// non-`Free` categories all become class methods (built by `emitCapabilityMethods`);
    /// `Free` members are emitted separately as free receiver-first functions.
    type PartitionedMembers =
        {
            /// Instance methods bound to `this` (runtime dispatch slots): interface-impl
            /// / `Object`-override members → `emitAttachedMethod`.
            Attached: TastAccessor.TypeMember list
            /// Free receiver-first functions (tree-shakeable; call sites lower to these).
            Free: TastAccessor.TypeMember list
            /// Enumerable-capability `GetEnumerator` impls (`seq<'T>` / `IEnumerable<'T>`)
            /// → `[Symbol.iterator]` generators (`emitIteratorMethod`).
            Iterators: TastAccessor.TypeMember list
            /// Eq/comp/hash capability impls, paired with their registry-symbol key
            /// (`vesper.equality` / `vesper.comparison` / `vesper.hash`) → computed-key
            /// `[Symbol.for("vesper.X")]` methods (`emitProtocolMethod`).
            Protocols: (string * TastAccessor.TypeMember) list
            /// Disposable-capability `Dispose` impls → native `[Symbol.dispose]()`
            /// methods (`emitDisposeMethod`) that `use` calls.
            Disposers: TastAccessor.TypeMember list
        }

    /// A class's INSTANCE preamble (`let` / `do`) awaiting emission into the primary
    /// ctor. `ThisKey` is the class-level `this` binder the entries read their siblings
    /// through — a preamble reference to a ctor param or an earlier `let` is a
    /// `FieldGet`/`FieldSet` on `TExpr.Var(ThisKey)`, so the ctor must bind that key to
    /// JS `this`. `Entries` are in declaration order, which is load-bearing (`let a = f()`
    /// / `do g a` / `let b = h()`).
    type ClassPreamble =
        {
            ThisKey: BinderKeyG<BinderId>
            Entries: TastAccessor.PreambleEntry list
        }

    /// How a type's single JS constructor is formed. `Positional` names each
    /// declaration-order field as a parameter — a record, or a class whose primary
    /// ctor params ARE its fields. `Explicit` is the `val`-form class's own
    /// `new(args) = { f = e; … }`: its parameter list and its stores are what the
    /// source wrote, and neither has to match the field list.
    [<RequireQualifiedAccess>]
    type PendingCtor =
        | Positional of fields: string list
        | Explicit of TastAccessor.SecondaryCtor

    /// One locally-emitted class awaiting body emission: its name + `Ctor` shape, its
    /// instance `Preamble` (`ValueNone` for a record, which has none), and its
    /// partitioned members to ATTACH (bodies built later with the full `WalkCtx`,
    /// since `collectTypes` runs before the ctx exists).
    type PendingClass =
        {
            Name: string
            Ctor: PendingCtor
            Preamble: ClassPreamble voption
            /// `static let` / `static do` entries in declaration order, initialising the
            /// class's static backing fields (`ClassName.field`) at module load. Empty
            /// for a class with no static preamble.
            StaticPreamble: TastAccessor.PreambleEntry list
            Members: PartitionedMembers
        }

    /// One locally-emitted union whose interface impls became BASE-class methods
    /// (`[Symbol.iterator]` / eq-comp-hash protocols). Like `PendingClass`, the method
    /// bodies are built later in `buildProgram` once the full `WalkCtx` exists; an
    /// interface-free union needs none of this and is emitted directly in `collectTypes`.
    type PendingUnion =
        {
            Name: string
            Brand: string
            Cases: JsUnionCaseDecl list
            Members: PartitionedMembers
        }

    /// The output of `collectTypes`: the in-source-order emission list, the three
    /// nominal lookup tables, the deferred pending-class/union records (whose method
    /// bodies `buildProgram` builds once the `WalkCtx` exists), and the flattened
    /// (typeName, member) list of free receiver-first functions to emit.
    type CollectedTypes =
        {
            Decls: JsStatement list
            Records: System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>
            Unions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
            Classes: System.Collections.Generic.Dictionary<SymbolKey, string>
            /// Locally-emitted enums, keyed by enum-type `SymbolKey` → the emitted JS
            /// object-map name. A `StaticFieldGet`/`EnumCase` resolves its `E.Ci`
            /// property access here.
            Enums: System.Collections.Generic.Dictionary<SymbolKey, string>
            PendingClasses: PendingClass list
            PendingUnions: PendingUnion list
            Members: (string * TastAccessor.TypeMember) list
        }

    // The IMPLEMENTER half of the capability protocol — the dispatch slot a type that
    // *implements* a capability emits, routed off `EmitJsCapabilities.capabilityOf` (which is
    // also what the CONSUMER half routes on; the protocol as a whole is documented there).
    // A slot lands the member in one of four buckets — `Iterators` (the `*[Symbol.iterator]()`
    // generator), `Disposers` (`[Symbol.dispose]()`), `Protocols` (a registry-symbol method),
    // or `Attached` (a plain named method) — and the bucket IS the table.

    /// The head nominal key of a frozen interface type (`FTClass(key, _)`).
    let ifaceHeadKey (ty: FrozenType) : TypeKey voption =
        match ty with
        | FTClass(key, _) -> ValueSome key
        | _ -> ValueNone

    /// The non-generic `System.Collections.IEnumerable` — implemented alongside the
    /// generic `IEnumerable<'T>` on a real BCL collection, but carries no JS protocol
    /// (the native iterator is driven by the generic `[Symbol.iterator]`), so its
    /// `GetEnumerator` impl is dropped rather than emitted as a dead attached method.
    [<Literal>]
    let nonGenericEnumerableName = "System.Collections.IEnumerable"

    /// Partition a class's `Members` into the ATTACHED instance methods (runtime
    /// dispatch slots, bound to `this`), the FREE receiver-first functions
    /// (tree-shakeable; call sites already lower to these), and the enumerable
    /// `GetEnumerator` ITERATOR impls (routed to `[Symbol.iterator]`). Interface-impl
    /// members claim their name slot first — except an enumerable-capability interface
    /// (matched against `caps.Enumerable`), whose members go to `Iterators`, and the
    /// non-generic `IEnumerable`, dropped. The redundant `obj`-typed `Object.Equals`
    /// override is always dropped (the typed `IEquatable<Self>.Equals` impl holds the
    /// `.Equals` slot); every other override (`GetHashCode`, `ToString`) attaches. A
    /// member that ends up with NO emission slot — a non-`Equals` member whose name is
    /// already claimed by an interface impl — fails loudly rather than silently vanishing.
    let partitionClassMembers
        (caps: RuntimeNames.CapabilityIds)
        (typeName: string)
        (interfaces: EqArray<FrozenType * EqArray<TastAccessor.TypeMember>>)
        (members: EqArray<TastAccessor.TypeMember>)
        : PartitionedMembers =
        let attached = ResizeArray<TastAccessor.TypeMember>()
        let iterators = ResizeArray<TastAccessor.TypeMember>()
        let protocols = ResizeArray<string * TastAccessor.TypeMember>()
        let disposers = ResizeArray<TastAccessor.TypeMember>()
        let claimed = System.Collections.Generic.HashSet<string>()

        let attachNamed (ifaceMembers: EqArray<TastAccessor.TypeMember>) =
            for m in ifaceMembers do
                if claimed.Add m.Name then
                    attached.Add m

        // Interface impls claim their name slot first. A capability impl is the exception: it
        // drives its capability's dispatch slot instead — a symbol key, which claims no string
        // name — with the CURSOR capability (`enumerator<'T>`) the one that doesn't, because
        // its slot IS a pair of plain named methods (`MoveNext()` / `Current()`), the very
        // methods the consumer half calls.
        for (iface, ifaceMembers) in interfaces do
            let isNonGenericEnumerable =
                match ifaceHeadKey iface with
                | ValueSome key -> SymbolKeyOps.typeMetaName key = nonGenericEnumerableName
                | ValueNone -> false

            let capability = ifaceHeadKey iface |> ValueOption.bind (capabilityOf caps)

            if isNonGenericEnumerable then
                ()
            else
                match capability with
                | ValueSome JsCapability.Iteration ->
                    for m in ifaceMembers do
                        iterators.Add m
                | ValueSome JsCapability.Equality ->
                    for m in ifaceMembers do
                        protocols.Add(equalityRegistryKey, m)
                | ValueSome JsCapability.Comparison ->
                    for m in ifaceMembers do
                        protocols.Add(comparisonRegistryKey, m)
                | ValueSome JsCapability.Disposal ->
                    for m in ifaceMembers do
                        disposers.Add m
                | ValueSome JsCapability.Cursor
                | ValueNone -> attachNamed ifaceMembers

        let free = ResizeArray<TastAccessor.TypeMember>()

        for m in members do
            if m.IsOverride && m.Name = "Equals" then
                // `obj`-typed `Object.Equals` override is redundant on JS — the typed
                // `IEquatable<Self>.Equals` impl holds the equality dispatch slot
                // (the `[Symbol.for("vesper.equality")]` method).
                ()
            elif m.IsOverride && m.Name = "GetHashCode" then
                // The hashing protocol slot: `hashOf` looks up `x[Symbol.for("vesper.hash")]()`,
                // so the `override GetHashCode` becomes a registry-symbol method, NOT a named
                // attached method (every OTHER override — `ToString` etc. — stays string-named).
                protocols.Add(hashRegistryKey, m)
            elif m.IsOverride then
                if claimed.Add m.Name then
                    attached.Add m
                else
                    failwithf
                        "EmitJs: class '%s' override '%s' clashes with an interface-impl member of the same name (no JS dispatch slot for both)"
                        typeName
                        m.Name
            elif claimed.Contains m.Name then
                failwithf
                    "EmitJs: class '%s' member '%s' collides with an interface-impl member of the same name (a regular method cannot share an attached dispatch slot)"
                    typeName
                    m.Name
            else
                free.Add m

        {
            Attached = List.ofSeq attached
            Free = List.ofSeq free
            Iterators = List.ofSeq iterators
            Protocols = List.ofSeq protocols
            Disposers = List.ofSeq disposers
        }

    /// Collect the file's nominal `type` decls (in source order) into the emission list,
    /// the lookup tables, the deferred pending-class/union records, and the free-member
    /// list. Read off the un-lowered decls — `TastLower.lower` drops `type` decls.
    let collectTypes
        (caps: RuntimeNames.CapabilityIds)
        (exportTypes: bool)
        (decls: TastAccessor.DeclId list)
        : CollectedTypes =
        let ordered = ResizeArray<JsStatement>()
        let records = System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>()
        let unions = System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>()
        let classes = System.Collections.Generic.Dictionary<SymbolKey, string>()
        let enums = System.Collections.Generic.Dictionary<SymbolKey, string>()
        let pendingClasses = ResizeArray<PendingClass>()
        let pendingUnions = ResizeArray<PendingUnion>()
        let members = ResizeArray<string * TastAccessor.TypeMember>()

        let addMembers (typeName: string) (ms: EqArray<TastAccessor.TypeMember>) =
            for m in ms do
                members.Add(typeName, m)

        // Run the member partition for a deferred nominal (class / interface-carrying
        // record or union) and register its `Free` members as free receiver-first
        // functions in one place — the only step every deferred arm shares. The caller
        // wraps the returned partition in the appropriate pending record.
        let deferPartition
            (typeName: string)
            (interfaces: EqArray<FrozenType * EqArray<TastAccessor.TypeMember>>)
            (declMembers: EqArray<TastAccessor.TypeMember>)
            : PartitionedMembers =
            let parts = partitionClassMembers caps typeName interfaces declMembers

            for m in parts.Free do
                members.Add(typeName, m)

            parts

        for decl in decls do
            match TastAccessor.declKind decl with
            | DeclShape.Type ->
                let td = TastAccessor.declType decl

                match td.Kind with
                // The JS backend has no value-type concept — a `[<Struct>]` record
                // (`valueKind = Struct`) emits as an ordinary reference object, a
                // pre-existing documented limitation shared with struct classes.
                | TTypeKindG.Record(fields, recMembers, recInterfaces, _) ->
                    // Local record: `Home = ValueNone` — its class is emitted here.
                    let info =
                        {
                            Name = td.Name
                            Fields = [ for f in fields -> f.Name ]
                            Home = ValueNone
                        }

                    records.[td.Key] <- info

                    if recInterfaces.IsEmpty then
                        // No interface impls → a record is one plain class with no
                        // methods; emit directly (no ctx needed). Augmentation members
                        // ride as free receiver-first functions.
                        ordered.Add(JsStatement.Class(info.Name, JsCtor.positional info.Fields [], [], exportTypes))
                        addMembers td.Name recMembers
                    else
                        // The record carries interface impls. A record is a single JS
                        // class, so route its interfaces + members through the SAME
                        // partition the class path uses: enumerable → `[Symbol.iterator]`,
                        // eq/comp/hash → registry symbols, a local interface → an attached
                        // method. The method bodies need the full `WalkCtx`, so defer like
                        // a `PendingClass` (`parts.Free` carries the augmentation members
                        // that stay free functions).
                        let parts = deferPartition td.Name recInterfaces recMembers

                        pendingClasses.Add
                            {
                                Name = info.Name
                                Ctor = PendingCtor.Positional info.Fields
                                Preamble = ValueNone
                                StaticPreamble = []
                                Members = parts
                            }
                | TTypeKindG.Union(cases, unionMembers, unionInterfaces) ->
                    // Local union: `Home = ValueNone` — its case classes are emitted here.
                    let info, caseDecls =
                        buildUnionInfo
                            ValueNone
                            td.Name
                            [ for case in cases -> case.Name, [ for (nm, _) in case.Fields -> nm ] ]

                    unions.[td.Key] <- info
                    let brand = SymbolKeyOps.qualifiedName td.Key

                    if unionInterfaces.IsEmpty then
                        // No interface impls → no base methods; emit directly (no ctx
                        // needed). Brand = qualified type name — a single value across
                        // modules, so an imported case class and any same-type value
                        // agree on `$type`.
                        ordered.Add(JsStatement.Union(td.Name, brand, caseDecls, [], exportTypes))
                        addMembers td.Name unionMembers
                    else
                        // The union carries interface impls. Route them through the SAME
                        // partition the class path uses: enumerable → `[Symbol.iterator]`,
                        // eq/comp/hash → registry symbols, others → attached. These attach
                        // to the BASE class so every case subclass inherits them and
                        // dispatch lands on a case instance. The augmentation `members`
                        // split into Free (free receiver-first fns) vs the rest — but a
                        // union's augmentation members are all non-interface here, so
                        // `parts.Free` carries exactly `unionMembers` (no double emission).
                        // The base-method bodies need the full `WalkCtx`, so defer like a
                        // `PendingClass`.
                        let parts = deferPartition td.Name unionInterfaces unionMembers

                        pendingUnions.Add
                            {
                                Name = td.Name
                                Brand = brand
                                Cases = caseDecls
                                Members = parts
                            }
                | TTypeKindG.Class cls ->
                    classes.[td.Key] <- td.Name

                    // Class shapes this lowering does not model are REJECTED here, never dropped:
                    // the emitter reads only `CtorParams` / `Fields` / `Members` /
                    // `SecondaryCtors` / `InstancePreamble` / `StaticPreamble`, so admitting one
                    // would compile to a program that silently disagrees with the CLR backend on
                    // the same source.
                    //  * `inherit` — no `extends` / `super(...)` is emitted, so the base ctor
                    //    (and its `do`) never runs and the base's members are absent from the
                    //    prototype.
                    //  * more than one `new(...)` — a JS class has exactly one constructor, so
                    //    every overload but one would be unreachable, and a call at its arity
                    //    would silently land in the survivor with the wrong arguments.
                    if cls.BaseType.IsSome || cls.BaseCtorCall.IsSome then
                        failwithf
                            "EmitJs: class '%s' declares an `inherit` clause; class inheritance is not yet supported on the JS target"
                            td.Name

                    // The primary ctor's params ARE its fields, so a secondary alongside it is a
                    // second arity; on the `val`-field form the secondaries are the ONLY ctors.
                    let ctorArities = (if cls.HasPrimaryCtor then 1 else 0) + cls.SecondaryCtors.Length

                    if ctorArities > 1 then
                        failwithf
                            "EmitJs: class '%s' declares %d constructors; a JS class has exactly one, so constructor overloads are not supported on the JS target"
                            td.Name
                            ctorArities

                    // A `val`-form class's `new(args) = { f = e; … }` is emitted as written —
                    // its own params, its own stores. Everything else (a record-like primary
                    // ctor, or a field-only class with no `new` at all) is positional over the
                    // declared fields.
                    let ctor =
                        match List.ofSeq cls.SecondaryCtors with
                        | [ sc ] -> PendingCtor.Explicit sc
                        | _ ->
                            let ctorFields = [ for f in cls.CtorParams -> f.Name ]

                            PendingCtor.Positional(
                                if List.isEmpty ctorFields then
                                    [ for f in cls.Fields -> f.Name ]
                                else
                                    ctorFields
                            )

                    // Split members into attached dispatch slots, free receiver-first
                    // functions, and enumerable-capability iterator impls (interface-impl
                    // / override / capability policy in `partitionClassMembers`).
                    let parts = deferPartition td.Name cls.Interfaces cls.Members

                    // The instance preamble's initialiser bodies need the full `WalkCtx`,
                    // so they are deferred alongside the member bodies.
                    let preamble =
                        if cls.InstancePreamble.IsEmpty then
                            ValueNone
                        else
                            ValueSome
                                {
                                    ThisKey = cls.ThisKey
                                    Entries = [ for entry in cls.InstancePreamble -> entry ]
                                }

                    pendingClasses.Add
                        {
                            Name = td.Name
                            Ctor = ctor
                            Preamble = preamble
                            StaticPreamble = [ for entry in cls.StaticPreamble -> entry ]
                            Members = parts
                        }
                // JS enum repr: a module-scope frozen object map `const E =
                // Object.freeze({ C1: v1, … })` for ALL three variants (numeric /
                // string / mixed) — JS is untyped, so the mix is the same object-map
                // shape, no reverse map (v1 = equality only). Cases stay in declaration
                // order; an unresolved case (`ValueNone`, a rejected literal already
                // errored at elaboration) is dropped from the map rather than emitting
                // a bogus value. The const carries no method bodies, so it is emitted
                // directly here (like the no-interface record/union path), and the type
                // key is registered so `StaticFieldGet`/`EnumCase` resolve `E.Ci`.
                | TTypeKindG.Enum cases ->
                    enums.[td.Key] <- td.Name

                    let entries =
                        [
                            for c in cases do
                                match c.Value with
                                | ValueSome lit -> c.Name, JsEmitHelpers.enumLiteral lit
                                | ValueNone -> ()
                        ]

                    ordered.Add(JsStatement.Enum(td.Name, entries, exportTypes))
                | _ -> ()
            | _ -> ()

        {
            Decls = List.ofSeq ordered
            Records = records
            Unions = unions
            Classes = classes
            Enums = enums
            PendingClasses = List.ofSeq pendingClasses
            PendingUnions = List.ofSeq pendingUnions
            Members = List.ofSeq members
        }
