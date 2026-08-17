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

    /// A nominal type's members split by where they are emitted: `Slotted` into the class body,
    /// each with its dispatch slot; `Free` as a top-level `<Type>__M = (this$) => (a) => …` that
    /// call sites lower to instead, so an unused member tree-shakes away. Both in source order.
    type PartitionedMembers =
        {
            Slotted: (MemberSlot * TastAccessor.TypeMember) list
            Free: TastAccessor.TypeMember list
        }

    /// A class's instance `let`/`do` preamble. `ThisKey` is the bound variable its entries read
    /// their siblings through, so the emitted ctor must alias that name to JS `this`.
    type ClassPreamble =
        {
            ThisKey: BoundVarKeyG<BoundVarId>
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

    /// An `inherit Base(args)` clause, as JS takes it.
    type PendingBase =
        {
            Chain: JsPrototypeChain
            /// The `super(…)` arguments, still unwalked: they read the derived class's own
            /// ctor params, so they need the walk context the pending record defers to.
            CtorArgs: TastAccessor.ExprId list
        }

    /// One locally-emitted class. A record reaches here too, with `Preamble = ValueNone`.
    type PendingClass =
        {
            Name: string
            Base: PendingBase voption
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
            Records: System.Collections.Generic.Dictionary<TypeKey, JsRecordInfo>
            Unions: System.Collections.Generic.Dictionary<TypeKey, JsUnionInfo>
            Classes: System.Collections.Generic.Dictionary<TypeKey, string>
            /// Enum type → the emitted `Object.freeze({…})` map's name, which is what
            /// makes a case reference resolve to the property read `E.Ci`.
            Enums: System.Collections.Generic.Dictionary<TypeKey, string>
            PendingClasses: PendingClass list
            PendingUnions: PendingUnion list
            Members: (string * TastAccessor.TypeMember) list
        }

    let ifaceTyCtorKey (ty: FrozenType) : TypeKey voption =
        match ty with
        | FTClass(key, _) -> ValueSome key
        | _ -> ValueNone

    /// Route each member to the JS form it is emitted in. An interface impl claims its
    /// name slot first, so a plain member of the same name has no slot left and faults
    /// here; a capability impl claims a symbol slot instead and claims no name.
    let partitionClassMembers
        (caps: RuntimeNames.CapabilityIds)
        (typeName: string)
        (interfaces: EqArray<FrozenType * EqArray<TastAccessor.TypeMember>>)
        (members: EqArray<TastAccessor.TypeMember>)
        : PartitionedMembers =
        let slotted = ResizeArray<MemberSlot * TastAccessor.TypeMember>()
        let free = ResizeArray<TastAccessor.TypeMember>()
        let claimed = System.Collections.Generic.HashSet<string>()

        // Only a `Named` slot spends a name, so only it consults `claimed`.
        let addNamed (m: TastAccessor.TypeMember) =
            if claimed.Add m.Name then
                slotted.Add(MemberSlot.Named, m)

        // An interface with no capability implements plain named methods.
        for (iface, ifaceMembers) in interfaces do
            let slot =
                ifaceTyCtorKey iface
                |> ValueOption.bind (capabilityOf caps)
                |> ValueOption.map (fun c -> c.Slot)
                |> ValueOption.defaultValue MemberSlot.Named

            for m in ifaceMembers do
                match slot with
                | MemberSlot.Named -> addNamed m
                | _ -> slotted.Add(slot, m)

        for m in members do
            if m.IsOverride && m.Name = "Equals" then
                // Redundant on JS: the typed `IEquatable<Self>.Equals` impl already holds
                // the `[Symbol.for("vesper.equality")]` slot.
                ()
            elif m.IsOverride && m.Name = "GetHashCode" then
                // The runtime's `hashOf` reads `x[Symbol.for("vesper.hash")]()`, so this one
                // override takes the registry slot; `ToString` and the rest stay string-named.
                slotted.Add(MemberSlot.Protocol hashRegistryKey, m)
            elif m.IsOverride then
                if claimed.Add m.Name then
                    slotted.Add(MemberSlot.Named, m)
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
            Slotted = List.ofSeq slotted
            Free = List.ofSeq free
        }

    /// Collect the file's nominal `type` decls, in source order. Takes the UN-lowered
    /// decls: lowering discards every `type` decl, so nothing survives it to read.
    /// What a class's `inherit` clause lowers to. `Erased` never reaches here: the emit filter
    /// drops those declarations whole, ahead of collection.
    type BaseVerdictOf = TastAccessor.TypeDecl -> JsBaseVerdict

    let collectTypes
        (caps: RuntimeNames.CapabilityIds)
        (baseVerdictOf: BaseVerdictOf)
        (exportTypes: bool)
        (decls: TastAccessor.DeclId list)
        : CollectedTypes =
        let ordered = ResizeArray<JsStatement>()
        let records = System.Collections.Generic.Dictionary<TypeKey, JsRecordInfo>()
        let unions = System.Collections.Generic.Dictionary<TypeKey, JsUnionInfo>()
        let classes = System.Collections.Generic.Dictionary<TypeKey, string>()
        let enums = System.Collections.Generic.Dictionary<TypeKey, string>()
        let pendingClasses = ResizeArray<PendingClass>()
        let pendingUnions = ResizeArray<PendingUnion>()
        let members = ResizeArray<string * TastAccessor.TypeMember>()

        let addMembers (typeName: string) (ms: EqArray<TastAccessor.TypeMember>) =
            for m in ms do
                members.Add(typeName, m)

        // Partition, and enrol the `Free` members as top-level functions, the one step
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
                    // Local record: `Home = ValueNone` because its class is emitted here.
                    let info =
                        {
                            Name = td.Name
                            Fields = [ for f in fields -> f.Name ]
                            Home = ValueNone
                        }

                    records.[td.TypeKey] <- info

                    if recInterfaces.IsEmpty then
                        // No interface impls → no method bodies to defer; emit the class now
                        // and let the augmentation members ride out as free functions.
                        ordered.Add(
                            JsStatement.Class(info.Name, None, JsCtor.positional info.Fields [], [], exportTypes)
                        )

                        addMembers td.Name recMembers
                    else
                        // A record with interface impls is still ONE class, so it takes the
                        // class path unchanged: same partition, same deferral.
                        let parts = deferPartition td.Name recInterfaces recMembers

                        pendingClasses.Add
                            {
                                Name = info.Name
                                Base = ValueNone
                                Ctor = PendingCtor.Positional info.Fields
                                Preamble = ValueNone
                                StaticPreamble = []
                                Members = parts
                            }
                | TTypeKindG.Union(cases, unionMembers, unionInterfaces) ->
                    // Local union: `Home = ValueNone` because its case classes are emitted here.
                    let info, caseDecls =
                        buildUnionInfo
                            ValueNone
                            td.Name
                            [ for case in cases -> case.Name, [ for (nm, _) in case.Fields -> nm ] ]

                    unions.[td.TypeKey] <- info
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
                        // every one of `unionMembers` slots `Free`. Hence no `addMembers` here.
                        let parts = deferPartition td.Name unionInterfaces unionMembers

                        pendingUnions.Add
                            {
                                Name = td.Name
                                Brand = brand
                                Cases = caseDecls
                                Members = parts
                            }
                | TTypeKindG.Class cls ->
                    classes.[td.TypeKey] <- td.Name

                    let pendingBase =
                        match baseVerdictOf td with
                        | JsBaseVerdict.Standalone -> ValueNone
                        | JsBaseVerdict.Extends chain ->
                            ValueSome
                                {
                                    Chain = chain
                                    CtorArgs =
                                        match cls.BaseCtorCall with
                                        | ValueSome bc -> List.ofSeq bc.Args
                                        | ValueNone -> []
                                }
                        | JsBaseVerdict.Unsupported ->
                            failwithf
                                "EmitJs: class '%s' declares an `inherit` clause over a base with no runtime class behind it; class inheritance is not yet supported on the JS target except over an `exn` root"
                                td.Name
                        | JsBaseVerdict.Erased ->
                            failwithf
                                "EmitJs: class '%s' inherits an erased base, so its declaration should have been dropped before collection"
                                td.Name

                    if pendingBase.IsNone && cls.BaseCtorCall.IsSome then
                        failwithf
                            "EmitJs: class '%s' calls a base constructor but declares no base type to chain to"
                            td.Name

                    // A JS class has exactly one constructor, so a second `new(…)` is rejected.
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
                            Base = pendingBase
                            Ctor = ctor
                            Preamble = preamble
                            StaticPreamble = [ for entry in cls.StaticPreamble -> entry ]
                            Members = parts
                        }
                // All three enum variants (numeric / string / mixed) emit the one untyped
                // shape `const E = Object.freeze({ C1: v1, … })`, no reverse map. A case with
                // no value was already a hard error at elaboration, so drop it silently.
                | TTypeKindG.Enum cases ->
                    enums.[td.TypeKey] <- td.Name

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
