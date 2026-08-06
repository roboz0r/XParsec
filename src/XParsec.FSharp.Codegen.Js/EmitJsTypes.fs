namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open EmitJsCapabilities

/// Member partitioning and nominal `type`-decl collection. Runs before the walk context
/// exists, so every member and preamble BODY is deferred to `EmitJs` as a pending record.
module EmitJsTypes =

    /// A record's JS class. `Fields` is in *declaration* order, which is the emitted
    /// constructor's parameter order.
    type JsRecordInfo =
        {
            Name: string
            Fields: string list
            /// `ValueSome home` → the class lives in that module, so a `new R(…)` site
            /// imports it; `ValueNone` → declared in this file.
            Home: JsHome voption
        }

    /// A union's JS classes: `Cases` keyed by F# case name → its subclass name and
    /// field order.
    type JsUnionInfo =
        {
            Name: string
            Cases: System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>
            /// `ValueSome home` → the case classes live in that module, so a `new U_C(…)`
            /// site imports them; `ValueNone` → declared in this file.
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

    /// Tag = declaration index; subclass name = `<baseName>_<case>`.
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

    /// A nominal type's members split by the JS form each is emitted in.
    type PartitionedMembers =
        {
            /// → a name-keyed class method `M(a) { … }`, called as `x.M(a)`.
            Attached: TastAccessor.TypeMember list
            /// → a top-level `<Type>__M = (this$) => (a) => …`; call sites lower to it
            /// rather than to a method, so an unused member tree-shakes away.
            Free: TastAccessor.TypeMember list
            /// `seq<'T>`/`IEnumerable<'T>` `GetEnumerator` impls → a `*[Symbol.iterator]()`
            /// generator method.
            Iterators: TastAccessor.TypeMember list
            /// Eq/comp/hash impls with their registry key → `[Symbol.for("vesper.equality")](b) { … }`
            /// and the `vesper.comparison` / `vesper.hash` twins.
            Protocols: (string * TastAccessor.TypeMember) list
            /// `Dispose` impls → a native `[Symbol.dispose]() { … }` method, which `use` calls.
            Disposers: TastAccessor.TypeMember list
        }

    /// A class's instance `let`/`do` preamble. `ThisKey` is the binder its entries read
    /// their siblings through, so the emitted ctor must alias that name to JS `this`.
    type ClassPreamble =
        {
            ThisKey: BinderKeyG<BinderId>
            Entries: TastAccessor.PreambleEntry list
        }

    /// How a type's single JS constructor is formed.
    [<RequireQualifiedAccess>]
    type PendingCtor =
        /// `constructor(f1, f2) { this.f1 = f1; … }` over the fields in declaration order.
        | Positional of fields: string list
        /// The `val`-form class's own `new(args) = { f = e; … }`: source params, source
        /// stores, neither obliged to match the field list.
        | Explicit of TastAccessor.SecondaryCtor

    /// One locally-emitted class. A record reaches here too, with `Preamble = ValueNone`.
    type PendingClass =
        {
            Name: string
            Ctor: PendingCtor
            Preamble: ClassPreamble voption
            StaticPreamble: TastAccessor.PreambleEntry list
            Members: PartitionedMembers
        }

    /// A locally-emitted union whose interface impls became BASE-class methods; an
    /// interface-free union carries no bodies and is emitted directly instead.
    type PendingUnion =
        {
            Name: string
            Brand: string
            Cases: JsUnionCaseDecl list
            Members: PartitionedMembers
        }

    /// `Decls` holds the declarations emittable without a walk context, in SOURCE order.
    type CollectedTypes =
        {
            Decls: JsStatement list
            Records: System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>
            Unions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
            Classes: System.Collections.Generic.Dictionary<SymbolKey, string>
            /// Enum type → the emitted `Object.freeze({…})` map's name, which is what
            /// makes a case reference resolve to the property read `E.Ci`.
            Enums: System.Collections.Generic.Dictionary<SymbolKey, string>
            PendingClasses: PendingClass list
            PendingUnions: PendingUnion list
            Members: (string * TastAccessor.TypeMember) list
        }

    let ifaceHeadKey (ty: FrozenType) : TypeKey voption =
        match ty with
        | FTClass(key, _) -> ValueSome key
        | _ -> ValueNone

    /// Implemented alongside the generic `IEnumerable<'T>`, whose impl already becomes
    /// `[Symbol.iterator]`, so this one's `GetEnumerator` is dropped rather than attached dead.
    [<Literal>]
    let nonGenericEnumerableName = "System.Collections.IEnumerable"

    /// Route each member to the JS form it is emitted in. An interface impl claims its
    /// name slot first, so a plain member of the same name has no slot left and faults
    /// here; a capability impl claims a symbol slot instead and claims no name.
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

        // `Cursor` (`enumerator<'T>`) is the capability that still takes a NAME slot: its
        // dispatch is the plain pair `e.MoveNext()` / `e.Current()`, not a symbol method.
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
                // Redundant on JS: the typed `IEquatable<Self>.Equals` impl already holds
                // the `[Symbol.for("vesper.equality")]` slot.
                ()
            elif m.IsOverride && m.Name = "GetHashCode" then
                // The runtime's `hashOf` reads `x[Symbol.for("vesper.hash")]()`, so this one
                // override takes the registry slot; `ToString` and the rest stay string-named.
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

    /// Collect the file's nominal `type` decls, in source order. Takes the UN-lowered
    /// decls: lowering discards every `type` decl, so nothing survives it to read.
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

        // Partition, and enrol the `Free` members as top-level functions — the one step
        // every deferred arm shares.
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
                // JS has no value types, so the `valueKind` a `[<Struct>]` record carries is
                // ignored: it emits as the same reference-object class as any other record.
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
                        // No interface impls → no method bodies to defer; emit the class now
                        // and let the augmentation members ride out as free functions.
                        ordered.Add(JsStatement.Class(info.Name, JsCtor.positional info.Fields [], [], exportTypes))
                        addMembers td.Name recMembers
                    else
                        // A record with interface impls is still ONE class, so it takes the
                        // class path unchanged: same partition, same deferral.
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
                        // No interface impls → no base-method bodies to defer; emit now. The
                        // `$type` brand is the QUALIFIED name, so an imported case class and
                        // a local value of that type agree on it.
                        ordered.Add(JsStatement.Union(td.Name, brand, caseDecls, [], exportTypes))
                        addMembers td.Name unionMembers
                    else
                        // The impls attach to the BASE class, so every case subclass inherits
                        // them. A union's augmentation members are never interface impls, so
                        // `parts.Free` is all of `unionMembers` — hence no `addMembers` here.
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

                    // Two class shapes are REJECTED rather than dropped: `inherit`, since no
                    // `extends` / `super(…)` is emitted and the base ctor would never run;
                    // and a second `new(…)`, since a JS class has exactly one constructor.
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

                    // A lone `new(args) = { f = e; … }` is emitted as written. Everything else
                    // is positional over the primary ctor's params, or over the declared
                    // fields when the class has no primary ctor.
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

                    let parts = deferPartition td.Name cls.Interfaces cls.Members

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
                // All three enum variants (numeric / string / mixed) emit the one untyped
                // shape `const E = Object.freeze({ C1: v1, … })`, no reverse map. A case with
                // no value was already a hard error at elaboration, so drop it silently.
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
