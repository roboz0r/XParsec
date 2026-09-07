namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open Vesper
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// The `.ctor` resolution of one frozen attribute. `Deduped` is the deliberate skip: another
/// write already emits the key's row. `Skipped` files its reason on the artifact.
[<RequireQualifiedAccess>]
type internal AttributeCtorResolution =
    | Ctor of EntityHandle
    | Deduped
    | Skipped of SkippedAttributeRowReason

/// The generalised `CustomAttribute` rows over the frozen attribute lists: one per attribute
/// on the assembly's type declarations, their own members, and record fields. Every parent
/// handle is a layout prediction, valid before its row is written.
module internal AttributeRowPrep =

    /// One prepared row: parent, attribute `.ctor`, encoded blob.
    type Row = (struct (EntityHandle * EntityHandle * BlobBuilder))

    [<NoEquality; NoComparison>]
    type Prepared =
        {
            Rows: Row list
            Skipped: SkippedAttributeRow list
        }

    /// Resolve one frozen attribute to its `.ctor` handle: the BCL spelling for a key the
    /// `ClrAttributeNames` table maps, the local `TypeDef`'s ctor for an attribute class this
    /// assembly emits, a contract-resolved `MemberRef` otherwise.
    let private resolveAttributeCtor
        (provider: ClrProvider)
        (classes: Dictionary<TypeKey, Emit.EmittedClass>)
        (attr: TAttribute)
        : AttributeCtorResolution =
        let positionalCount =
            attr.Args |> Block.fold (fun n a -> if a.Name.IsNone then n + 1 else n) 0

        if attr.Key = RuntimeNames.isByRefLikeAttributeKey then
            // The `TypeSlotKind.Class RefStruct` write emits this row; a second one here
            // would duplicate it.
            AttributeCtorResolution.Deduped
        elif attr.Key = RuntimeNames.attributeUsageAttributeKey then
            AttributeCtorResolution.Ctor provider.AttributeUsageAttrCtor
        else
            match classes.TryGetValue attr.Key with
            | true, c when c.Typars.IsEmpty ->
                match
                    EmitResolve.localCtors c
                    |> List.filter (fun (ps, _, _) -> List.length ps = positionalCount)
                with
                | [ (_, _, handle) ] -> AttributeCtorResolution.Ctor handle
                | [] -> AttributeCtorResolution.Skipped(SkippedAttributeRowReason.NoMatchingCtor positionalCount)
                | _ -> AttributeCtorResolution.Skipped(SkippedAttributeRowReason.AmbiguousCtor positionalCount)
            | true, _ -> AttributeCtorResolution.Skipped SkippedAttributeRowReason.GenericAttributeClass
            | false, _ ->
                match provider.TryExternalAttributeCtor(attr.Key, positionalCount) with
                | ValueSome handle -> AttributeCtorResolution.Ctor handle
                | ValueNone -> AttributeCtorResolution.Skipped(SkippedAttributeRowReason.NoExternalCtor positionalCount)

    /// The rows and skips for every attribute-bearing position of `partitions`. `enums` is
    /// the assembly's own enum emissions: a named enum-typed argument's II.23.3 SerString is
    /// the BCL spelling for a `ClrAttributeNames`-mapped key, the Vesper full name for a
    /// local enum; a referenced-assembly enum is skipped (`ForeignEnumArgument`).
    let prepare
        (provider: ClrProvider)
        (classes: Dictionary<TypeKey, Emit.EmittedClass>)
        (enums: Dictionary<TypeKey, Emit.EmittedEnum>)
        (layoutHandles: LayoutHandles)
        (fieldDefHandles: Dictionary<FieldKey, FieldDefinitionHandle>)
        (partitions: PartitionedTypeDecls list)
        : Prepared =
        let tryEnumFullName (key: TypeKey) : string voption =
            match ClrAttributeNames.tryBclEnumFullName key with
            | ValueSome n -> ValueSome n
            | ValueNone ->
                if enums.ContainsKey key then
                    ValueSome(SymbolKeyOps.typeMetaName key)
                else
                    ValueNone

        let rows = ResizeArray<Row>()
        let skipped = ResizeArray<SkippedAttributeRow>()

        let addAttributeRows (parent: string) (parentHandle: EntityHandle) (attrs: TAttributes) : unit =
            let skip (attr: TAttribute) (reason: SkippedAttributeRowReason) =
                skipped.Add
                    {
                        AttributeKey = attr.Key
                        Parent = parent
                        Reason = reason
                    }

            for attr in attrs do
                match resolveAttributeCtor provider classes attr with
                | AttributeCtorResolution.Deduped -> ()
                | AttributeCtorResolution.Skipped reason -> skip attr reason
                | AttributeCtorResolution.Ctor ctor ->
                    match AttributeBlob.tryEncode tryEnumFullName attr.Args with
                    | Error AttributeBlobRejection.UnencodableValue ->
                        skip attr SkippedAttributeRowReason.UnencodableArgument
                    | Error(AttributeBlobRejection.ForeignEnum enumKey) ->
                        skip attr (SkippedAttributeRowReason.ForeignEnumArgument enumKey)
                    | Ok blob -> rows.Add(struct (parentHandle, ctor, blob))

        let typeName (td: TastAccessor.TypeDecl) : string = SymbolKeyOps.typeMetaName td.TypeKey

        let typeParent (td: TastAccessor.TypeDecl) : EntityHandle =
            toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key))

        // Index `i` is the member's position among the declaration's OWN members, the same
        // prefix `NominalEmit.register` keys `MethodKey.Member(td.Key, i)` by. A property's
        // row lands on its `get_` method, the only row the property emits.
        let memberRows (td: TastAccessor.TypeDecl) (members: TastAccessor.TypeMember list) =
            members
            |> List.iteri (fun i m ->
                addAttributeRows
                    (typeName td + "." + m.Name)
                    (toEntity (layoutHandles.MethodDefOf(MethodKey.Member(td.Key, i))))
                    m.Attributes
            )

        for p in partitions do
            for (td, _) in p.Interfaces do
                addAttributeRows (typeName td) (typeParent td) td.Attributes

            for ud in p.Unions do
                addAttributeRows (typeName ud.Decl) (typeParent ud.Decl) ud.Decl.Attributes
                memberRows ud.Decl ud.Members

            for rd in p.Records do
                addAttributeRows (typeName rd.Decl) (typeParent rd.Decl) rd.Decl.Attributes
                memberRows rd.Decl rd.Members

                for fld in rd.Fields do
                    addAttributeRows
                        (typeName rd.Decl + "." + fld.Name)
                        (toEntity fieldDefHandles.[FieldKey.RecordField(rd.Decl.Key, fld.Name)])
                        fld.Attributes

            for cd in p.Classes do
                addAttributeRows (typeName cd.Decl) (typeParent cd.Decl) cd.Decl.Attributes
                memberRows cd.Decl cd.Members

            for ed in p.Enums do
                addAttributeRows (typeName ed.Decl) (typeParent ed.Decl) ed.Decl.Attributes

            for sed in p.StructEnums do
                addAttributeRows (typeName sed.Decl) (typeParent sed.Decl) sed.Decl.Attributes

        {
            Rows = List.ofSeq rows
            Skipped = List.ofSeq skipped
        }
