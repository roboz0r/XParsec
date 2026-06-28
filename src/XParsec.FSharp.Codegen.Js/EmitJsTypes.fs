namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// Member partitioning and nominal-`type`-decl collection for the JS backend — the
/// `WalkCtx`-free, `buildExpr`-free front half of emission. `collectTypes` reads the
/// un-lowered `type` decls (which `TastLower.lower` drops) into the emission list, the
/// lookup tables, the deferred pending-class/union records, and the free-member list;
/// `EmitJs` opens this module and builds the deferred method bodies once the full
/// `WalkCtx` exists.
module EmitJsTypes =

    /// A record type's JS shape: the emitted class `Name` and its `Fields` in
    /// *declaration* order — `RecordCons`/`RecordClone` reorder source args to match.
    type JsRecordInfo = { Name: string; Fields: string list }

    /// A union type's JS shape: the emitted base-class `Name` and its cases keyed by
    /// F# case name. `UnionCons` and union patterns look up subclass + field order here.
    type JsUnionInfo =
        {
            Name: string
            Cases: System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>
            /// `ValueSome asm` for an external union: its case classes live in `asm`'s
            /// runtime module, so a `UnionCons` site imports them rather than relying on
            /// a local class. `ValueNone` for a union declared in this file.
            Home: string voption
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
        (home: string voption)
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
            Attached: Frozen.TTypeMember list
            /// Free receiver-first functions (tree-shakeable; call sites lower to these).
            Free: Frozen.TTypeMember list
            /// Enumerable-capability `GetEnumerator` impls (`seq<'T>` / `IEnumerable<'T>`)
            /// → `[Symbol.iterator]` generators (`emitIteratorMethod`).
            Iterators: Frozen.TTypeMember list
            /// Eq/comp/hash capability impls, paired with their registry-symbol key
            /// (`vesper.equality` / `vesper.comparison` / `vesper.hash`) → computed-key
            /// `[Symbol.for("vesper.X")]` methods (`emitProtocolMethod`).
            Protocols: (string * Frozen.TTypeMember) list
            /// Disposable-capability `Dispose` impls → native `[Symbol.dispose]()`
            /// methods (`emitDisposeMethod`) that `use` calls.
            Disposers: Frozen.TTypeMember list
        }

    /// One locally-emitted class awaiting body emission: its name + ctor `fields` and
    /// its partitioned members to ATTACH (bodies built later with the full `WalkCtx`,
    /// since `collectTypes` runs before the ctx exists).
    type PendingClass =
        {
            Name: string
            Fields: string list
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
            PendingClasses: PendingClass list
            PendingUnions: PendingUnion list
            Members: (string * Frozen.TTypeMember) list
        }

    // The JS capability protocol table — the ONE place the `caps.* → JS anchor` mapping is
    // enumerated. The CLR/JS asymmetry made concrete: a CLR anchor is a TYPE
    // (`System.IDisposable`) so it stays in source; a JS anchor is a SYMBOL
    // (`Symbol.dispose`, a dispatch key) so it lives here, in the backend. Dispatch is
    // keyed on the resolved `caps.*` identity (`partitionClassMembers`/`capMatches`).
    //
    //   capability | JS anchor                       | kind            | emission
    //   -----------|---------------------------------|-----------------|---------------------------
    //   iteration  | Symbol.iterator                 | native          | *[…]() GENERATOR (adapter)
    //   disposal   | Symbol.dispose                  | native          | [...]() plain method
    //   equality   | Symbol.for("vesper.equality")   | Vesper registry | [...]() plain method
    //   comparison | Symbol.for("vesper.comparison") | Vesper registry | [...]() plain method
    //   hashing    | Symbol.for("vesper.hash")       | Vesper registry | [...]() plain method
    //
    // Iteration is the only generator (the MoveNext/Current → next/{value,done} adapter);
    // the rest are plain methods under a computed key — native well-known (`EmitJs`
    // `symbolDispose`/`nativeSymbol`) vs registry (`registrySymbol`, from the keys below).
    // The two shapes route by a direct `if/elif` — the bucket IS the table. `hashing` is
    // the `override GetHashCode`, routed separately in `partitionClassMembers`.

    /// Registry-symbol keys for the eq/comp/hash JS capability protocols.
    /// These three protocols have no native JS dispatch, so they ride a process-wide
    /// `Symbol.for("vesper.X")` the Vesper runtimes look up — collision-proof against a
    /// foreign object's same-named string method.
    [<Literal>]
    let equalityRegistryKey = "vesper.equality"

    [<Literal>]
    let comparisonRegistryKey = "vesper.comparison"

    [<Literal>]
    let hashRegistryKey = "vesper.hash"

    /// The head nominal key of a frozen interface type (`FTClass(key, _)`).
    let ifaceHeadKey (ty: FrozenType) : SymbolKey voption =
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
        (interfaces: EqArray<FrozenType * EqArray<Frozen.TTypeMember>>)
        (members: EqArray<Frozen.TTypeMember>)
        : PartitionedMembers =
        let attached = ResizeArray<Frozen.TTypeMember>()
        let iterators = ResizeArray<Frozen.TTypeMember>()
        let protocols = ResizeArray<string * Frozen.TTypeMember>()
        let disposers = ResizeArray<Frozen.TTypeMember>()
        let claimed = System.Collections.Generic.HashSet<string>()

        let capMatches (cap: RuntimeNames.CapabilityIdentity voption) (iface: FrozenType) =
            match ifaceHeadKey iface with
            | ValueSome key -> RuntimeNames.matchesKey cap key
            | ValueNone -> false

        // Interface impls claim their name slot first. The capability interfaces are the
        // exceptions: an enumerable (`seq<'T>`) impl drives a native `[Symbol.iterator]`
        // generator, and an equatable (`IEquatable<Self>`) / comparable (`IComparable<Self>`)
        // impl drives a registry-symbol `[Symbol.for("vesper.X")]` method (see the
        // capability protocol table above) — neither claims a string name slot.
        for (iface, ifaceMembers) in interfaces do
            let isEnumerable = capMatches caps.Enumerable iface
            let isEquatable = capMatches caps.Equatable iface
            let isComparable = capMatches caps.Comparable iface
            let isDisposable = capMatches caps.Disposable iface

            let isNonGenericEnumerable =
                match ifaceHeadKey iface with
                | ValueSome key -> SymbolKeyOps.qualifiedName key = nonGenericEnumerableName
                | ValueNone -> false

            if isEnumerable then
                for m in ifaceMembers do
                    iterators.Add m
            elif isEquatable then
                for m in ifaceMembers do
                    protocols.Add(equalityRegistryKey, m)
            elif isComparable then
                for m in ifaceMembers do
                    protocols.Add(comparisonRegistryKey, m)
            elif isDisposable then
                // The disposable interface's `Dispose` impl drives a native
                // `[Symbol.dispose]()` method (the slot `use`'s `obj[Symbol.dispose]()`
                // lowering calls); it claims no string name slot.
                for m in ifaceMembers do
                    disposers.Add m
            elif isNonGenericEnumerable then
                ()
            else
                for m in ifaceMembers do
                    if claimed.Add m.Name then
                        attached.Add m

        let free = ResizeArray<Frozen.TTypeMember>()

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
    let collectTypes (caps: RuntimeNames.CapabilityIds) (exportTypes: bool) (tast: Frozen.TastFile) : CollectedTypes =
        let ordered = ResizeArray<JsStatement>()
        let records = System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>()
        let unions = System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>()
        let classes = System.Collections.Generic.Dictionary<SymbolKey, string>()
        let pendingClasses = ResizeArray<PendingClass>()
        let pendingUnions = ResizeArray<PendingUnion>()
        let members = ResizeArray<string * Frozen.TTypeMember>()

        let addMembers (typeName: string) (ms: EqArray<Frozen.TTypeMember>) =
            for m in ms do
                members.Add(typeName, m)

        // Run the member partition for a deferred nominal (class / interface-carrying
        // record or union) and register its `Free` members as free receiver-first
        // functions in one place — the only step every deferred arm shares. The caller
        // wraps the returned partition in the appropriate pending record.
        let deferPartition
            (typeName: string)
            (interfaces: EqArray<FrozenType * EqArray<Frozen.TTypeMember>>)
            (declMembers: EqArray<Frozen.TTypeMember>)
            : PartitionedMembers =
            let parts = partitionClassMembers caps typeName interfaces declMembers

            for m in parts.Free do
                members.Add(typeName, m)

            parts

        for decl in tast.Decls do
            match decl with
            | TDeclG.Type td ->
                match td.Kind with
                | TTypeKindG.Record(fields, recMembers, recInterfaces) ->
                    let info =
                        {
                            Name = td.Name
                            Fields = [ for f in fields -> f.Name ]
                        }

                    records.[td.Key] <- info

                    if recInterfaces.IsEmpty then
                        // No interface impls → a record is one plain class with no
                        // methods; emit directly (no ctx needed). Augmentation members
                        // ride as free receiver-first functions.
                        ordered.Add(JsStatement.Class(info.Name, info.Fields, [], exportTypes))
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
                                Fields = info.Fields
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

                    // The class's positional ctor stores each declared field. Use
                    // `CtorParams` when present (primary-ctor parameters that become
                    // fields); fall back to `Fields` (the `val`-field form).
                    let fieldNames =
                        let ctorFields = [ for f in cls.CtorParams -> f.Name ]

                        if List.isEmpty ctorFields then
                            [ for f in cls.Fields -> f.Name ]
                        else
                            ctorFields

                    // Split members into attached dispatch slots, free receiver-first
                    // functions, and enumerable-capability iterator impls (interface-impl
                    // / override / capability policy in `partitionClassMembers`).
                    let parts = deferPartition td.Name cls.Interfaces cls.Members

                    pendingClasses.Add
                        {
                            Name = td.Name
                            Fields = fieldNames
                            Members = parts
                        }
                | _ -> ()
            | _ -> ()

        {
            Decls = List.ofSeq ordered
            Records = records
            Unions = unions
            Classes = classes
            PendingClasses = List.ofSeq pendingClasses
            PendingUnions = List.ofSeq pendingUnions
            Members = List.ofSeq members
        }
