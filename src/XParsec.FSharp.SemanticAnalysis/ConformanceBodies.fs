namespace XParsec.FSharp.SemanticAnalysis

open Vesper
open System
open System.Collections.Generic
open XParsec.FSharp.Lexer

// `.fsi` ↔ `.fs` conformance of type BODIES: a signature's body for a key against the body the
// implementation would publish signatureless under the same key. Field, case and member types on
// both halves are templates over `FTTypar(Declaring, i)`, compared by structural equality.

module ConformanceBodies =

    /// The comparison key of a type member across the pair. `Folded` is the tupled parameters
    /// to the return for a method and the bare type for a value member, so `M: a -> b -> r`
    /// and `member _.M(a, b)` fold to one type.
    [<NoComparison>]
    type private MemberShape =
        {
            Name: string
            IsStatic: bool
            IsValueMember: bool
            MethodTyparArity: int<typeSlot>
            Folded: FrozenType
        }

    let private memberShape (m: ExternalMember) : MemberShape =
        {
            Name = m.Name
            IsStatic = m.IsStatic
            IsValueMember = m.IsValueMember
            MethodTyparArity = m.Signature.MethodTyparArity
            Folded =
                if m.IsValueMember then
                    m.Signature.Return
                else
                    FTFun(ExternalSignature.tupledParameters m.Signature, m.Signature.Return)
        }

    /// A union case as the static member its constructor compiles to: `Cons(head, tail)` for
    /// a fielded case, the value `Empty` for a nullary one, each returning `self`.
    let private caseShape (self: FrozenType) (c: ExternalCaseShape) : MemberShape =
        {
            Name = c.Name
            IsStatic = true
            IsValueMember = c.FrozenFieldTypes.Length = 0
            MethodTyparArity = 0<_>
            Folded =
                match c.FrozenFieldTypes.Length with
                | 0 -> self
                | _ -> FTFun(ExternalSignature.tupledParams c.FrozenFieldTypes, self)
        }

    let private describeNominal (n: FrozenNominal) : string =
        Conformance.describeType (FTClass(n.Key, n.Args))

    let private describeField (f: ExternalFieldShape) : string =
        let prefix = if f.IsMutable then "mutable " else ""
        sprintf "%s%s: %s" prefix f.Name (Conformance.describeType f.Frozen)

    let private describeCase (c: ExternalCaseShape) : string =
        match c.FieldNames.Length with
        | 0 -> c.Name
        | _ ->
            let fields =
                [
                    for i in 0 .. c.FieldNames.Length - 1 ->
                        let ty = Conformance.describeType c.FrozenFieldTypes.[i]

                        match c.FieldNames.[i] with
                        | ValueSome n -> sprintf "%s: %s" n ty
                        | ValueNone -> ty
                ]

            sprintf "%s of %s" c.Name (String.concat " * " fields)

    let private describeEnumValue (v: ExternalEnumCaseValue) : string =
        match v with
        | ExternalEnumCaseValue.IntVal v -> sprintf "%s (%A)" (IntValue.render v) (IntValue.kind v)
        | ExternalEnumCaseValue.StringVal s -> sprintf "\"%s\"" s

    let private describeMember (m: MemberShape) : string =
        match m.Name with
        | ".ctor" -> sprintf "new: %s" (Conformance.describeType m.Folded)
        | _ ->
            let prefix = if m.IsStatic then "static member " else "member "
            sprintf "%s%s: %s" prefix m.Name (Conformance.describeType m.Folded)

    /// A declaration-shape flag written on one half alone.
    let private shapeFlag
        (typeName: string)
        (flag: string)
        (declared: bool)
        (defined: bool)
        : Conformance.ConformanceError list =
        [
            if declared <> defined then
                yield Conformance.ConformanceError.ShapeFlagDiffers(typeName, flag, declared)
        ]

    /// Each declared field by name against the defined one, then the converse, then order.
    let private checkFields
        (typeName: string)
        (declared: Block<ExternalFieldShape>)
        (defined: Block<ExternalFieldShape>)
        : Conformance.ConformanceError list =
        let definedByName = Dictionary<string, ExternalFieldShape>(StringComparer.Ordinal)

        for f in defined do
            definedByName.[f.Name] <- f

        let declaredNames = declared |> Block.map (fun f -> f.Name)
        let definedNames = defined |> Block.map (fun f -> f.Name)

        let byName =
            [
                for f in declared do
                    match definedByName.TryGetValue f.Name with
                    | false, _ -> yield Conformance.ConformanceError.FieldMissingInImpl(typeName, f.Name)
                    | true, d ->
                        if f.IsMutable <> d.IsMutable || f.Frozen <> d.Frozen then
                            yield
                                Conformance.ConformanceError.FieldDiffers(
                                    typeName,
                                    f.Name,
                                    describeField f,
                                    describeField d
                                )

                for f in defined do
                    if not (Block.contains f.Name declaredNames) then
                        yield Conformance.ConformanceError.FieldMissingInSig(typeName, f.Name)
            ]

        match byName with
        | [] when declaredNames <> definedNames -> [ Conformance.ConformanceError.FieldOrderDiffers typeName ]
        | findings -> findings

    /// Cases compare positionally: a case's index is its runtime tag.
    let private checkCases
        (typeName: string)
        (declared: Block<ExternalCaseShape>)
        (defined: Block<ExternalCaseShape>)
        : Conformance.ConformanceError list =
        if declared.Length <> defined.Length then
            [
                Conformance.ConformanceError.UnionCaseCountDiffers(typeName, declared.Length, defined.Length)
            ]
        else
            [
                for i in 0 .. declared.Length - 1 do
                    let s = declared.[i]
                    let d = defined.[i]

                    if
                        s.Name <> d.Name
                        || s.FieldNames <> d.FieldNames
                        || s.FrozenFieldTypes <> d.FrozenFieldTypes
                    then
                        yield Conformance.ConformanceError.UnionCaseDiffers(typeName, i, describeCase s, describeCase d)
            ]

    let private checkEnumCases
        (typeName: string)
        (declared: Block<ExternalEnumCaseShape>)
        (defined: Block<ExternalEnumCaseShape>)
        : Conformance.ConformanceError list =
        let definedByName =
            Dictionary<string, ExternalEnumCaseShape>(StringComparer.Ordinal)

        for c in defined do
            definedByName.[c.Name] <- c

        let declaredNames = declared |> Block.map (fun c -> c.Name)

        [
            for c in declared do
                match definedByName.TryGetValue c.Name with
                | false, _ -> yield Conformance.ConformanceError.EnumCaseMissingInImpl(typeName, c.Name)
                | true, d ->
                    if c.Value <> d.Value then
                        yield
                            Conformance.ConformanceError.EnumCaseValueDiffers(
                                typeName,
                                c.Name,
                                describeEnumValue c.Value,
                                describeEnumValue d.Value
                            )

            for c in defined do
                if not (Block.contains c.Name declaredNames) then
                    yield Conformance.ConformanceError.EnumCaseMissingInSig(typeName, c.Name)
        ]

    /// Every declared member must be matched by one defined member; a defined member the
    /// signature omits is hidden.
    let private checkMembers
        (typeName: string)
        (declared: Block<ExternalMember>)
        (defined: seq<MemberShape>)
        : Conformance.ConformanceError list =
        let defined = HashSet<MemberShape>(defined, HashIdentity.Structural)

        [
            for m in declared do
                let shape = memberShape m

                if not (defined.Contains shape) then
                    yield Conformance.ConformanceError.MemberMissingInImpl(typeName, m.Name, describeMember shape)
        ]

    let private checkClassShape
        (typeName: string)
        (declared: ExternalClassShape)
        (defined: ExternalClassShape)
        : Conformance.ConformanceError list =
        let baseName (b: FrozenNominal voption) =
            match b with
            | ValueSome n -> describeNominal n
            | ValueNone -> "obj"

        let interfaceSet (s: ExternalClassShape) = EqSet.ofSeq s.FrozenInterfaces

        let interfaceNames (s: ExternalClassShape) =
            s.FrozenInterfaces |> Seq.map describeNominal |> String.concat ", "

        [
            if declared.FrozenBaseType <> defined.FrozenBaseType then
                yield
                    Conformance.ConformanceError.BaseTypeDiffers(
                        typeName,
                        baseName declared.FrozenBaseType,
                        baseName defined.FrozenBaseType
                    )

            if interfaceSet declared <> interfaceSet defined then
                yield
                    Conformance.ConformanceError.InterfacesDiffer(
                        typeName,
                        interfaceNames declared,
                        interfaceNames defined
                    )

            yield! shapeFlag typeName "sealed" declared.Flags.Declared.IsSealed defined.Flags.Declared.IsSealed
            yield! shapeFlag typeName "abstract" declared.Flags.Declared.IsAbstract defined.Flags.Declared.IsAbstract
            yield! shapeFlag typeName "struct" declared.Flags.IsValueType defined.Flags.IsValueType
        ]

    /// Findings over the bodies of the types BOTH halves publish under one family, in key
    /// order. A key one half alone publishes, or the halves file under different families,
    /// takes its verdict from `ConformanceSurface`.
    let check (published: PublishedSurface) (implemented: PublishedSurface) : Conformance.ConformanceError list =
        let implShapes = PublishedSurface.keyIndex implemented.ShapesByKey
        let declaredMembers = PublishedSurface.keyIndex published.MembersByKey
        let definedMembers = PublishedSurface.keyIndex implemented.MembersByKey

        let membersOf (table: Dictionary<TypeKey, Block<ExternalMember>>) (key: TypeKey) =
            match table.TryGetValue key with
            | true, ms -> ms
            | _ -> Block.empty

        [
            for entry in published.ShapesByKey do
                let key = entry.Key
                let typeName = SymbolKeyOps.typeMetaName key
                let declared = membersOf declaredMembers key
                let defined = membersOf definedMembers key |> Seq.map memberShape

                match implShapes.TryGetValue key with
                | false, _ -> ()
                | true, implShape ->
                    match entry.Value, implShape with
                    | ExternalTypeShape.Record s, ExternalTypeShape.Record d ->
                        yield! checkFields typeName s.Fields d.Fields
                        yield! shapeFlag typeName "struct" s.IsValueType d.IsValueType
                        yield! checkMembers typeName declared defined
                    | ExternalTypeShape.Union s, ExternalTypeShape.Union d ->
                        yield! checkCases typeName s.Cases d.Cases
                        yield! shapeFlag typeName "struct" s.IsValueType d.IsValueType

                        let self =
                            FTUnion(key, Block.init d.TyparArity (fun i -> FTTypar(TyparScope.Type key, i)))

                        yield! checkMembers typeName declared (Seq.append defined (Seq.map (caseShape self) d.Cases))
                    | ExternalTypeShape.Enum s, ExternalTypeShape.Enum d ->
                        yield! checkEnumCases typeName s.Cases d.Cases
                    | ExternalTypeShape.Abbrev s, ExternalTypeShape.Abbrev d ->
                        if s.Body <> d.Body then
                            yield
                                Conformance.ConformanceError.AbbreviationDiffers(
                                    typeName,
                                    Conformance.describeType s.Body,
                                    Conformance.describeType d.Body
                                )
                    | ExternalTypeShape.Class s, ExternalTypeShape.Class d ->
                        match s.Commitment with
                        | ClassCommitment.Opaque -> ()
                        | ClassCommitment.Class -> yield! checkClassShape typeName s d
                        | ClassCommitment.Interface -> ()

                        if s.Commitment = d.Commitment then
                            yield! checkMembers typeName declared defined
                    | _ -> ()
        ]
