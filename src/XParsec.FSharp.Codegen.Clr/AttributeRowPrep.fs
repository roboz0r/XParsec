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

    /// The `.ctor` handle for `attr`'s recorded constructor: the local `TypeDef`'s ctor of the
    /// recorded parameter types for a class this assembly emits, a referenced assembly's
    /// `MemberRef` otherwise. `[<IsByRefLike>]` is `Deduped`, its row written by the type slot.
    let private resolveAttributeCtor
        (provider: ClrProvider)
        (classes: Dictionary<TypeKey, Emit.EmittedClass>)
        (attr: TAttribute)
        : AttributeCtorResolution =
        if attr.Key = RuntimeNames.isByRefLikeAttributeKey then
            // The `TypeSlotKind.Class RefStruct` write emits this row; a second one here
            // would duplicate it.
            AttributeCtorResolution.Deduped
        elif attr.Key = RuntimeNames.attributeUsageAttributeKey then
            AttributeCtorResolution.Ctor provider.AttributeUsageAttrCtor
        else
            match classes.TryGetValue attr.Key with
            | true, c when c.Typars.IsEmpty ->
                let paramTys = Block.toList attr.Ctor.ArgSig

                match EmitResolve.localCtors c |> List.tryFind (fun (ps, _, _) -> ps = paramTys) with
                | Some(_, _, handle) -> AttributeCtorResolution.Ctor handle
                | None ->
                    failwithf
                        "Emit: attribute class '%s' emits no constructor of parameter types %A, which the front end selected"
                        c.Name
                        paramTys
            | true, _ -> AttributeCtorResolution.Skipped SkippedAttributeRowReason.GenericAttributeClass
            | false, _ ->
                match provider.TryExternalAttributeCtor attr.Ctor with
                | ValueSome handle -> AttributeCtorResolution.Ctor handle
                | ValueNone -> AttributeCtorResolution.Skipped SkippedAttributeRowReason.CtorUnresolved

    /// The rows and skips for every attribute-bearing position of `layouts`.
    let prepare
        (provider: ClrProvider)
        (classes: Dictionary<TypeKey, Emit.EmittedClass>)
        (layoutHandles: LayoutHandles)
        (fieldDefHandles: Dictionary<FieldKey, FieldDefinitionHandle>)
        (layouts: FileLayout list)
        : Prepared =
        let namer = ClrTypeNamer(provider, ClrTypeNames.localNames layouts)

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
                    match AttributeBlob.tryEncode namer.TryTypeName attr.Ctor.ArgSig attr.Args with
                    | Ok blob -> rows.Add(struct (parentHandle, ctor, blob))
                    | Error(AttributeEncodeFailure.UnspellableType _) ->
                        skip attr SkippedAttributeRowReason.UnspellableArgumentType
                    | Error(AttributeEncodeFailure.UnencodableValue ty) ->
                        failwithf
                            "AttributeRowPrep: %s on %s carries a %A argument, which the front end reports under the CLR platform facts; this tree was analysed without them"
                            (SymbolKeyOps.typeMetaName attr.Key)
                            parent
                            ty

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

        for layout in layouts do
            let p = layout.Partitioned

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
