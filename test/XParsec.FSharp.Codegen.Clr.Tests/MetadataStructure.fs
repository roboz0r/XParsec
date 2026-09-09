/// Assertions over an emitted PE's metadata, read back with `MetadataReader`: what was
/// WRITTEN, as against what the runtime makes of the assembly once loaded.
module XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

/// Widen a metadata handle to `EntityHandle`. `op_Implicit` also has a `-> Handle`
/// overload, and spelling the target explicitly stays off the implicit-conversion warning.
let inline private toEntity (h: ^T) : EntityHandle =
    (^T: (static member op_Implicit: ^T -> EntityHandle) h)

let inline private rowOf (h: ^T) : int = MetadataTokens.GetRowNumber(toEntity h)

/// A `TypeDef` row as the PE carries it: the field / method / property rows its
/// `FieldList` / `MethodList` / `PropertyMap` range claims, in row order.
type EmittedType =
    {
        /// `Ns.Name` for a top-level type, `Ns.Outer+Inner` for a nested one. This is
        /// the spelling `Assembly.GetType` binds.
        Name: string
        Row: int
        Fields: string list
        Methods: string list
        Properties: string list
    }

/// What a caller expects one type's rows to be; `Type` uses the same name spelling.
type ExpectedType =
    {
        Type: string
        Fields: string list
        Methods: string list
    }

/// The six `Nested*` values of the visibility field. Nesting REPLACES the visibility
/// rather than adding a flag, so a type is nested iff its visibility is one of these.
let private nestedVisibilities =
    set
        [
            TypeAttributes.NestedPublic
            TypeAttributes.NestedPrivate
            TypeAttributes.NestedFamily
            TypeAttributes.NestedAssembly
            TypeAttributes.NestedFamANDAssem
            TypeAttributes.NestedFamORAssem
        ]

let private isNested (attrs: TypeAttributes) =
    nestedVisibilities.Contains(attrs &&& TypeAttributes.VisibilityMask)

let private nameOf (md: MetadataReader) (h: TypeDefinitionHandle) : string =
    let rec go (h: TypeDefinitionHandle) =
        let td = md.GetTypeDefinition h
        let name = md.GetString td.Name

        if isNested td.Attributes then
            let encl = td.GetDeclaringType()

            if encl.IsNil then
                // A nested-flagged type with no `NestedClass` row is itself a defect;
                // render it plainly rather than crash, and let the assertions report it.
                name
            else
                go encl + "+" + name
        else
            let ns = md.GetString td.Namespace
            if String.IsNullOrEmpty ns then name else ns + "." + name

    go h

/// The `Ns.Name` spelling of a `TypeRef` row.
let private typeRefName (md: MetadataReader) (h: TypeReferenceHandle) : string =
    let tr = md.GetTypeReference h
    let ns = md.GetString tr.Namespace
    let n = md.GetString tr.Name
    if ns = "" then n else ns + "." + n

/// The `TypeDef` row of `typeName` by the `Ns.Outer+Inner` spelling. Raises when the
/// assembly declares no such type.
let private typeDefOf (md: MetadataReader) (typeName: string) : TypeDefinitionHandle =
    match md.TypeDefinitions |> Seq.tryFind (fun h -> nameOf md h = typeName) with
    | Some h -> h
    | None ->
        failwithf "MetadataStructure: no type '%s' among %A" typeName [ for h in md.TypeDefinitions -> nameOf md h ]

/// The `MethodDef` rows of `typeName` in row order.
let private methodDefsOf (md: MetadataReader) (typeName: string) : MethodDefinition list =
    [
        for mh in (md.GetTypeDefinition(typeDefOf md typeName)).GetMethods() -> md.GetMethodDefinition mh
    ]

/// The first `MethodDef` row named `methodName` on `typeName`. Raises when the type
/// declares no such method.
let private methodDefOf (md: MetadataReader) (typeName: string) (methodName: string) : MethodDefinition =
    let methods = methodDefsOf md typeName

    match methods |> List.tryFind (fun m -> md.GetString m.Name = methodName) with
    | Some m -> m
    | None ->
        failwithf
            "MetadataStructure: %s declares no %s among %A"
            typeName
            methodName
            [ for m in methods -> md.GetString m.Name ]

/// Every `TypeDef` row, in table order, with the field / method rows its range claims.
let readTypes (md: MetadataReader) : EmittedType list =
    [
        for h in md.TypeDefinitions do
            let td = md.GetTypeDefinition h

            {
                Name = nameOf md h
                Row = rowOf h
                Fields = [ for f in td.GetFields() -> md.GetString((md.GetFieldDefinition f).Name) ]
                Methods = [ for m in td.GetMethods() -> md.GetString((md.GetMethodDefinition m).Name) ]
                Properties =
                    [
                        for p in td.GetProperties() -> md.GetString((md.GetPropertyDefinition p).Name)
                    ]
            }
    ]

// ---- The range partition -----------------------------------------------------
// Each type's field and method ranges must be consecutive, non-overlapping and
// gap-free, together covering the `Field` / `MethodDef` / `Property` tables exactly.

/// `ranges` pairs each claiming type with the table rows its range holds, in claim order.
/// The ranges must consecutively cover rows `1..total`.
// `owner` is indexed by row id (1-based), so overlap and gap are both lookups.
let private assertTableRanges (label: string) (table: string) (total: int) (ranges: (string * int list) list) =
    let owner: string voption[] = Array.create (total + 1) ValueNone
    let mutable cursor = 1

    for typeName, rows in ranges do
        match rows with
        | [] -> ()
        | first :: _ ->
            if first <> cursor then
                failwithf
                    "%s: %s range of '%s' starts at row %d but the previous types claim through %d, so the ranges are not consecutive"
                    label
                    table
                    typeName
                    first
                    (cursor - 1)

        rows
        |> List.iteri (fun i row ->
            if row <> cursor + i then
                failwithf "%s: %s range of '%s' is not consecutive: %A" label table typeName rows

            if row < 1 || row > total then
                failwithf "%s: %s row %d claimed by '%s' is outside the table (1..%d)" label table row typeName total

            match owner.[row] with
            | ValueSome prior ->
                failwithf "%s: %s row %d is claimed by both '%s' and '%s'" label table row prior typeName
            | ValueNone -> owner.[row] <- ValueSome typeName
        )

        cursor <- cursor + List.length rows

    if cursor <> total + 1 then
        failwithf "%s: the TypeDef rows claim %d %s rows but the table has %d" label (cursor - 1) table total

    for row in 1..total do
        if owner.[row].IsNone then
            failwithf "%s: %s row %d is claimed by no TypeDef" label table row

let private assertRangePartition (label: string) (md: MetadataReader) =
    let types =
        [
            for h in md.TypeDefinitions do
                let td = md.GetTypeDefinition h

                nameOf md h, [ for f in td.GetFields() -> rowOf f ], [ for m in td.GetMethods() -> rowOf m ]
        ]

    assertTableRanges label "Field" (md.GetTableRowCount TableIndex.Field) [ for n, f, _ in types -> n, f ]

    assertTableRanges label "MethodDef" (md.GetTableRowCount TableIndex.MethodDef) [ for n, _, m in types -> n, m ]

// ---- The `Property` table and its `PropertyMap` claimants ---------------------
// A type declaring no property gets no `PropertyMap` row, so the claimants are a SUBSET
// of the TypeDef table. SRM neither sorts nor validates `PropertyMap`, which makes this
// the check that the emitter added those rows ascending by parent.

let private assertPropertyRanges (label: string) (md: MetadataReader) =
    let claimants =
        [
            for h in md.TypeDefinitions do
                match [ for p in (md.GetTypeDefinition h).GetProperties() -> rowOf p ] with
                | [] -> ()
                | rows -> nameOf md h, rows
        ]

    assertTableRanges label "Property" (md.GetTableRowCount TableIndex.Property) claimants

    let mapRows = md.GetTableRowCount TableIndex.PropertyMap

    if mapRows <> List.length claimants then
        failwithf
            "%s: the PropertyMap table has %d rows but %d types claim a property range, so a row is orphaned or duplicated"
            label
            mapRows
            (List.length claimants)

    // Every accessor a `MethodSemantics` row names must be a method of the property's own
    // declaring type, which a mis-sorted table would violate.
    for h in md.TypeDefinitions do
        let td = md.GetTypeDefinition h
        let own = set [ for m in td.GetMethods() -> rowOf m ]

        for p in td.GetProperties() do
            let accessors = (md.GetPropertyDefinition p).GetAccessors()

            for a in [ accessors.Getter; accessors.Setter ] do
                if not a.IsNil && not (own.Contains(rowOf a)) then
                    failwithf
                        "%s: property '%s' on '%s' names an accessor outside that type's own MethodDef range"
                        label
                        (md.GetString((md.GetPropertyDefinition p).Name))
                        (nameOf md h)

// ---- `<Module>` is TypeDef row 1 ---------------------------------------------

let private assertModuleRow (label: string) (md: MetadataReader) =
    match List.ofSeq md.TypeDefinitions with
    | [] -> failwithf "%s: the TypeDef table is empty, not even the <Module> pseudo-type" label
    | first :: _ ->
        let td = md.GetTypeDefinition first
        let name = md.GetString td.Name

        if name <> "<Module>" then
            failwithf "%s: TypeDef row 1 is '%s', not '<Module>'" label name

        if not (String.IsNullOrEmpty(md.GetString td.Namespace)) then
            failwithf "%s: the <Module> pseudo-type carries namespace '%s'" label (md.GetString td.Namespace)

// ---- `NestedClass` rows, and a nested type's flags ----------------------------
// The reader binary-searches `NestedClass`, so an unsorted table shows up here as a
// nested-flagged type with no enclosing type, and an orphan row in the row count.

let private assertNestedClassRows (label: string) (md: MetadataReader) =
    let mutable nestedCount = 0

    for h in md.TypeDefinitions do
        let td = md.GetTypeDefinition h

        if isNested td.Attributes then
            nestedCount <- nestedCount + 1
            let name = md.GetString td.Name
            let encl = td.GetDeclaringType()

            if encl.IsNil then
                failwithf "%s: '%s' has nested visibility but no NestedClass row (or the table is unsorted)" label name

            if encl = h then
                failwithf "%s: '%s' is its own enclosing type" label name

            // The CLI requires a nested type to follow its enclosing type in the table.
            if rowOf encl >= rowOf h then
                failwithf
                    "%s: nested type '%s' (row %d) precedes its enclosing type (row %d)"
                    label
                    name
                    (rowOf h)
                    (rowOf encl)

            // A nested type's own name is a single segment and its namespace column is
            // empty, because the namespace belongs to the OUTERMOST container.
            if not (String.IsNullOrEmpty(md.GetString td.Namespace)) then
                failwithf
                    "%s: nested type '%s' carries namespace '%s', but a nested TypeDef's namespace column is empty"
                    label
                    name
                    (md.GetString td.Namespace)

            if name.Contains "." then
                failwithf
                    "%s: nested type name '%s' is dotted, but the containment chain belongs in NestedClass, not the name"
                    label
                    name

            let visibility = td.Attributes &&& TypeAttributes.VisibilityMask

            if not (nestedVisibilities.Contains visibility) then
                failwithf "%s: nested type '%s' has visibility %A, which is not a Nested* value" label name visibility

    let rows = md.GetTableRowCount TableIndex.NestedClass

    if rows <> nestedCount then
        failwithf
            "%s: the NestedClass table has %d rows but %d TypeDefs carry nested visibility, so a nesting row is orphaned or duplicated"
            label
            rows
            nestedCount

// ---- Pre-order contiguity ----------------------------------------------------
// A type at row r that transitively encloses n types owns rows (r, r+n] and nothing else.
// Legal metadata allows a nested type anywhere after its enclosing type.

let private assertPreOrderContiguity (label: string) (md: MetadataReader) =
    let children = Dictionary<int, ResizeArray<int>>()

    for h in md.TypeDefinitions do
        let td = md.GetTypeDefinition h

        if isNested td.Attributes && not (td.GetDeclaringType().IsNil) then
            let parent = rowOf (td.GetDeclaringType())

            match children.TryGetValue parent with
            | true, xs -> xs.Add(rowOf h)
            | _ -> children.[parent] <- ResizeArray [ rowOf h ]

    let rec subtree (row: int) : int list =
        match children.TryGetValue row with
        | true, xs ->
            [
                for c in xs do
                    yield c
                    yield! subtree c
            ]
        | _ -> []

    for h in md.TypeDefinitions do
        let row = rowOf h
        let sub = subtree row |> List.sort
        let expected = [ row + 1 .. row + List.length sub ]

        if sub <> expected then
            failwithf
                "%s: '%s' (row %d) encloses %d types but they are not the contiguous block %A; got %A"
                label
                (nameOf md h)
                row
                (List.length sub)
                expected
                sub

// ---- The public assertions ---------------------------------------------------

/// Every structural invariant above, over an already-open reader, so that a hand-built
/// metadata image with no PE around it can be checked too.
let assertWellFormedMetadata (label: string) (md: MetadataReader) : unit =
    assertModuleRow label md
    assertRangePartition label md
    assertPropertyRanges label md
    assertNestedClassRows label md
    assertPreOrderContiguity label md

/// Every `TypeDef` row of an emitted PE, with the rows its ranges claim: the diagnosis
/// view, and how a caller writes an `ExpectedType` pin in the first place.
let emittedTypes (bytes: byte[]) : EmittedType list =
    use pe = openPe bytes
    readTypes (pe.GetMetadataReader())

/// One type's `Property` row names, each with the accessor method names bound to it by
/// `MethodSemantics`. Raises when the assembly declares no such type.
let propertiesOf (bytes: byte[]) (typeName: string) : (string * (string voption * string voption)) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    let nameOfAccessor (a: MethodDefinitionHandle) =
        if a.IsNil then
            ValueNone
        else
            ValueSome(md.GetString((md.GetMethodDefinition a).Name))

    [
        for p in (md.GetTypeDefinition(typeDefOf md typeName)).GetProperties() do
            let pd = md.GetPropertyDefinition p
            let accessors = pd.GetAccessors()

            md.GetString pd.Name, (nameOfAccessor accessors.Getter, nameOfAccessor accessors.Setter)
    ]

/// One type's `Field` row names in row order, by the `Ns.Outer+Inner` spelling.
/// Raises when the assembly declares no such type.
let fieldsOf (bytes: byte[]) (typeName: string) : string list =
    let types = emittedTypes bytes

    match types |> List.tryFind (fun t -> t.Name = typeName) with
    | Some t -> t.Fields
    | None -> failwithf "MetadataStructure: no type '%s' among %A" typeName [ for t in types -> t.Name ]

/// A `TypeDef` row's declaration shape: its `extends` column, its own `GenericParam`
/// rows in index order, and the attribute bits a caller pins.
type TypeDecl =
    {
        /// `TypeDef`/`TypeRef` bases print by name; a `TypeSpec` base (an `extends` over a
        /// generic instantiation) is an unnamed blob and prints `<typespec>`.
        Extends: string
        Typars: string list
        IsAbstract: bool
        IsSealed: bool
        IsNested: bool
        /// The `LayoutMask` bits: `AutoLayout`, `SequentialLayout` or `ExplicitLayout`.
        Layout: TypeAttributes
        /// The `VisibilityMask` bits.
        Visibility: TypeAttributes
    }

/// The declaration shape of the type `Assembly.GetType name` would bind.
/// `ValueNone` ⇒ the assembly declares no such type.
let typeDecl (bytes: byte[]) (name: string) : TypeDecl voption =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    let named = md.TypeDefinitions |> Seq.tryFind (fun h -> nameOf md h = name)

    match named with
    | None -> ValueNone
    | Some h ->
        let td = md.GetTypeDefinition h
        let b = td.BaseType

        let extends =
            if b.IsNil then
                "<none>"
            else
                match b.Kind with
                | HandleKind.TypeDefinition -> nameOf md (TypeDefinitionHandle.op_Explicit b: TypeDefinitionHandle)
                | HandleKind.TypeReference -> typeRefName md (TypeReferenceHandle.op_Explicit b: TypeReferenceHandle)
                | _ -> "<typespec>"

        ValueSome
            {
                Extends = extends
                Typars =
                    [
                        for gh in td.GetGenericParameters() ->
                            let gp = md.GetGenericParameter gh
                            md.GetString gp.Name
                    ]
                IsAbstract = td.Attributes.HasFlag TypeAttributes.Abstract
                IsSealed = td.Attributes.HasFlag TypeAttributes.Sealed
                IsNested = isNested td.Attributes
                Layout = td.Attributes &&& TypeAttributes.LayoutMask
                Visibility = td.Attributes &&& TypeAttributes.VisibilityMask
            }

/// One type's `FieldLayout` rows as `(field name, offset)` in field-row order, by the
/// `Ns.Outer+Inner` spelling; a field with no row is absent. Raises when the assembly
/// declares no such type.
let fieldLayoutsOf (bytes: byte[]) (typeName: string) : (string * int) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    match md.TypeDefinitions |> Seq.tryFind (fun h -> nameOf md h = typeName) with
    | None ->
        failwithf "MetadataStructure: no type '%s' among %A" typeName [ for h in md.TypeDefinitions -> nameOf md h ]
    | Some h ->
        [
            for fh in (md.GetTypeDefinition h).GetFields() do
                let fd = md.GetFieldDefinition fh
                let offset = fd.GetOffset()

                if offset >= 0 then
                    md.GetString fd.Name, offset
        ]

/// The `ClassLayout` table's row count.
let classLayoutRowCount (bytes: byte[]) : int =
    use pe = openPe bytes
    pe.GetMetadataReader().GetTableRowCount TableIndex.ClassLayout

/// Every `TypeDef` row's `Field` rows as `(type name, (field name, attributes) list)`, both
/// in table order, by the `Ns.Outer+Inner` spelling.
let allFieldAttrs (bytes: byte[]) : (string * (string * FieldAttributes) list) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    [
        for h in md.TypeDefinitions ->
            nameOf md h,
            [
                for fh in (md.GetTypeDefinition h).GetFields() ->
                    let fd = md.GetFieldDefinition fh
                    md.GetString fd.Name, fd.Attributes
            ]
    ]

/// One type's `Field` rows in row order as `(name, attributes)`, by the `Ns.Outer+Inner`
/// spelling. Raises when the assembly declares no such type.
let fieldAttrsOf (bytes: byte[]) (typeName: string) : (string * FieldAttributes) list =
    let types = allFieldAttrs bytes

    match types |> List.tryFind (fun (n, _) -> n = typeName) with
    | Some(_, fields) -> fields
    | None -> failwithf "MetadataStructure: no type '%s' among %A" typeName (List.map fst types)

/// One type's `Method` rows in row order as `(name, attributes)`, by the `Ns.Outer+Inner`
/// spelling. Raises when the assembly declares no such type.
let methodAttrsOf (bytes: byte[]) (typeName: string) : (string * MethodAttributes) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    [ for m in methodDefsOf md typeName -> md.GetString m.Name, m.Attributes ]

/// A method's `Param` row names. Sequence 0 is the return parameter's row and is skipped.
let private paramNamesOfMethod (md: MetadataReader) (m: MethodDefinition) : string list =
    [
        for ph in m.GetParameters() do
            let p = md.GetParameter ph

            if p.SequenceNumber > 0 then
                md.GetString p.Name
    ]

/// One type's `Method` rows in row order as `(name, Param row names)`, by the
/// `Ns.Outer+Inner` spelling. Raises when the assembly declares no such type.
let paramNamesOf (bytes: byte[]) (typeName: string) : (string * string list) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    [
        for m in methodDefsOf md typeName -> md.GetString m.Name, paramNamesOfMethod md m
    ]

/// The `Param` row names of the method `methodName` on `typeName`, the first row where the
/// name is overloaded. Raises when the type declares no such method.
let methodParamNamesOf (bytes: byte[]) (typeName: string) (methodName: string) : string list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()
    paramNamesOfMethod md (methodDefOf md typeName methodName)

let private genericParamsOf (md: MetadataReader) (handles: GenericParameterHandleCollection) =
    [
        for gh in handles ->
            let gp = md.GetGenericParameter gh
            md.GetString gp.Name, gp.Attributes
    ]

/// The `GenericParam` rows of `typeName` in index order as `(name, attributes)`, by the
/// `Ns.Outer+Inner` spelling. Raises when the assembly declares no such type.
let typeGenericParamsOf (bytes: byte[]) (typeName: string) : (string * GenericParameterAttributes) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()
    genericParamsOf md ((md.GetTypeDefinition(typeDefOf md typeName)).GetGenericParameters())

/// The `GenericParam` rows of the method `methodName` on `typeName` in index order as
/// `(name, attributes)`, the first row where the name is overloaded. Raises when the type
/// declares no such method.
let methodGenericParamsOf
    (bytes: byte[])
    (typeName: string)
    (methodName: string)
    : (string * GenericParameterAttributes) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()
    genericParamsOf md ((methodDefOf md typeName methodName).GetGenericParameters())

/// Renders a signature type as IL spells it: a nominal by its `Ns.Outer+Inner` name, a
/// type's typar as `!i`, a method's as `!!j`, an instantiation as `` Ns.Name`n<args> ``.
type private SignatureTypeNames(md: MetadataReader) =
    /// The IL spelling of a `TypeDef`, `TypeRef` or `TypeSpec` handle.
    member this.TypeDefOrRefName(h: EntityHandle) : string =
        match h.Kind with
        | HandleKind.TypeDefinition -> nameOf md (TypeDefinitionHandle.op_Explicit h: TypeDefinitionHandle)
        | HandleKind.TypeReference -> typeRefName md (TypeReferenceHandle.op_Explicit h: TypeReferenceHandle)
        | HandleKind.TypeSpecification ->
            let ts =
                md.GetTypeSpecification(TypeSpecificationHandle.op_Explicit h: TypeSpecificationHandle)

            ts.DecodeSignature(this, ())
        | kind -> failwithf "MetadataStructure: a TypeDefOrRef position holds a %A" kind

    interface ISignatureTypeProvider<string, unit> with
        member _.GetPrimitiveType(code) = string code
        member _.GetTypeFromDefinition(_, h, _) = nameOf md h
        member _.GetTypeFromReference(_, h, _) = typeRefName md h
        member _.GetTypeFromSpecification(_, _, h, _) = "<typespec>"
        member _.GetSZArrayType(elem) = elem + "[]"

        member _.GetArrayType(elem, shape) =
            elem + "[" + String(',', shape.Rank - 1) + "]"

        member _.GetByReferenceType(elem) = elem + "&"
        member _.GetPointerType(elem) = elem + "*"

        member _.GetGenericInstantiation(generic, args) =
            generic + "<" + String.Join(", ", args) + ">"

        member _.GetGenericTypeParameter(_, i) = sprintf "!%d" i
        member _.GetGenericMethodParameter(_, i) = sprintf "!!%d" i
        member _.GetFunctionPointerType(_) = "<fnptr>"

        member _.GetModifiedType(modifier, unmodified, isRequired) =
            unmodified + (if isRequired then " modreq(" else " modopt(") + modifier + ")"

        member _.GetPinnedType(elem) = elem + " pinned"

/// The `GenericParamConstraint` rows of each parameter in `handles`, as `(name, targets)`
/// in row order. A `TypeDef` / `TypeRef` target is its name, a `TypeSpec` target its
/// decoded signature (`` System.IComparable`1<!!0> ``).
let private genericParamConstraintsOf (md: MetadataReader) (handles: GenericParameterHandleCollection) =
    let names = SignatureTypeNames md

    [
        for gh in handles ->
            let gp = md.GetGenericParameter gh

            md.GetString gp.Name,
            [
                for ch in gp.GetConstraints() -> names.TypeDefOrRefName (md.GetGenericParameterConstraint ch).Type
            ]
    ]

/// The `GenericParamConstraint` rows of `typeName`'s parameters in index order as
/// `(name, targets)`, by the `Ns.Outer+Inner` spelling. Raises when the assembly declares
/// no such type.
let typeGenericParamConstraintsOf (bytes: byte[]) (typeName: string) : (string * string list) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()
    genericParamConstraintsOf md ((md.GetTypeDefinition(typeDefOf md typeName)).GetGenericParameters())

/// The `GenericParamConstraint` rows of the parameters of the method `methodName` on
/// `typeName` in index order as `(name, targets)`, the first row where the name is
/// overloaded. Raises when the type declares no such method.
let methodGenericParamConstraintsOf
    (bytes: byte[])
    (typeName: string)
    (methodName: string)
    : (string * string list) list =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()
    genericParamConstraintsOf md ((methodDefOf md typeName methodName).GetGenericParameters())

let genericParamConstraintRowCount (bytes: byte[]) : int =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()
    md.GetTableRowCount TableIndex.GenericParamConstraint

/// How many `MemberRef` rows carry `name`. The table is appended to rather than
/// deduplicated, so a count above one is a member ref minted more than once.
let memberRefRowCount (bytes: byte[]) (name: string) : int =
    use pe = openPe bytes
    let md = pe.GetMetadataReader()

    md.MemberReferences
    |> Seq.filter (fun mh -> md.GetString((md.GetMemberReference mh).Name) = name)
    |> Seq.length

let assertWellFormed (label: string) (bytes: byte[]) : unit =
    use pe = openPe bytes
    assertWellFormedMetadata label (pe.GetMetadataReader())

let assertWellFormedFile (label: string) (path: string) : unit =
    assertWellFormed label (IO.File.ReadAllBytes path)

/// The rows in each named type's range are EXACTLY these, in order; types not named are
/// not checked. Names are the teeth: permute the emitted field rows against the type
/// rows and every count, range and total still agrees; only the names move.
/// Raises when two `TypeDef` rows render to the same name, since a nested-flagged type
/// with no `NestedClass` row renders without its `Outer+` prefix and would otherwise
/// shadow the row the expectation targets.
let assertTypeMembersMetadata (label: string) (md: MetadataReader) (expected: ExpectedType list) : unit =
    let actual = readTypes md

    for e in expected do
        match actual |> List.filter (fun t -> t.Name = e.Type) with
        | [] -> failwithf "%s: no TypeDef named '%s'; the PE has %A" label e.Type (actual |> List.map (fun t -> t.Name))
        | _ :: _ :: _ as dupes ->
            failwithf
                "%s: TypeDef rows %A all render as '%s', so the expectation is ambiguous"
                label
                (dupes |> List.map (fun t -> t.Row))
                e.Type
        | [ t ] ->
            if t.Fields <> e.Fields then
                failwithf "%s: '%s' claims field rows %A but should claim %A" label e.Type t.Fields e.Fields

            if t.Methods <> e.Methods then
                failwithf "%s: '%s' claims method rows %A but should claim %A" label e.Type t.Methods e.Methods

let assertTypeMembers (label: string) (bytes: byte[]) (expected: ExpectedType list) : unit =
    use pe = openPe bytes
    assertTypeMembersMetadata label (pe.GetMetadataReader()) expected

/// The `(type, method)` the PE's entry-point token resolves to, by looking the
/// token's row up in the `TypeDef` method ranges. `ValueNone` for a library.
let entryPointOwner (bytes: byte[]) : (string * string) voption =
    use pe = openPe bytes
    let header = pe.PEHeaders.CorHeader

    if isNull header || header.EntryPointTokenOrRelativeVirtualAddress = 0 then
        ValueNone
    else
        let md = pe.GetMetadataReader()
        let handle = MetadataTokens.Handle header.EntryPointTokenOrRelativeVirtualAddress

        if handle.Kind <> HandleKind.MethodDefinition then
            failwithf "the entry-point token is a %A, not a MethodDefinition" handle.Kind

        let entry = MethodDefinitionHandle.op_Explicit handle
        let row = rowOf entry

        let owner =
            md.TypeDefinitions
            |> Seq.tryPick (fun h ->
                (md.GetTypeDefinition h).GetMethods()
                |> Seq.tryFind (fun m -> rowOf m = row)
                |> Option.map (fun _ -> nameOf md h)
            )

        match owner with
        | Some ty -> ValueSome(ty, md.GetString((md.GetMethodDefinition entry).Name))
        | None -> failwithf "the entry-point MethodDef row %d lies in no TypeDef's method range" row

/// The entry point is `typeName::methodName`, found by looking the PE's entry-point token
/// up in the `TypeDef` method ranges, so it fails whenever the emitted method rows and
/// the emitted type rows fall out of step.
let assertEntryPointOwner (label: string) (bytes: byte[]) (typeName: string) (methodName: string) : unit =
    match entryPointOwner bytes with
    | ValueNone -> failwithf "%s: the PE has no entry point" label
    | ValueSome(ty, m) ->
        if ty <> typeName || m <> methodName then
            failwithf "%s: the entry point is %s::%s, expected %s::%s" label ty m typeName methodName

/// A library carries no entry-point token at all.
let assertNoEntryPoint (label: string) (bytes: byte[]) : unit =
    match entryPointOwner bytes with
    | ValueNone -> ()
    | ValueSome(ty, m) -> failwithf "%s: expected a library but the PE's entry point is %s::%s" label ty m
