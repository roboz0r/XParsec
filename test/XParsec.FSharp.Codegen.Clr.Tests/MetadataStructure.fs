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

/// A `TypeDef` row as the PE carries it: the field / method rows its `FieldList` /
/// `MethodList` range claims, in row order.
type EmittedType =
    {
        /// `Ns.Name` for a top-level type, `Ns.Outer+Inner` for a nested one. This is
        /// the spelling `Assembly.GetType` binds.
        Name: string
        Row: int
        Fields: string list
        Methods: string list
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
            }
    ]

// ---- The range partition -----------------------------------------------------
// Each type's field and method ranges must be consecutive, non-overlapping and
// gap-free, together covering the `Field` / `MethodDef` tables exactly.

let private assertRangePartition (label: string) (md: MetadataReader) =
    // `owner` is indexed by row id (1-based), so overlap and gap are both lookups.
    let check (table: string) (total: int) (ranges: (string * int list) list) =
        let owner = Array.create (total + 1) ""
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
                    failwithf
                        "%s: %s row %d claimed by '%s' is outside the table (1..%d)"
                        label
                        table
                        row
                        typeName
                        total

                if owner.[row] <> "" then
                    failwithf "%s: %s row %d is claimed by both '%s' and '%s'" label table row owner.[row] typeName

                owner.[row] <- typeName
            )

            cursor <- cursor + List.length rows

        if cursor <> total + 1 then
            failwithf "%s: the TypeDef rows claim %d %s rows but the table has %d" label (cursor - 1) table total

        for row in 1..total do
            if owner.[row] = "" then
                failwithf "%s: %s row %d is claimed by no TypeDef" label table row

    let types =
        [
            for h in md.TypeDefinitions do
                let td = md.GetTypeDefinition h

                nameOf md h, [ for f in td.GetFields() -> rowOf f ], [ for m in td.GetMethods() -> rowOf m ]
        ]

    check "Field" (md.GetTableRowCount TableIndex.Field) [ for n, f, _ in types -> n, f ]
    check "MethodDef" (md.GetTableRowCount TableIndex.MethodDef) [ for n, _, m in types -> n, m ]

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
    assertNestedClassRows label md
    assertPreOrderContiguity label md

/// Every `TypeDef` row of an emitted PE, with the rows its ranges claim: the diagnosis
/// view, and how a caller writes an `ExpectedType` pin in the first place.
let emittedTypes (bytes: byte[]) : EmittedType list =
    use pe = openPe bytes
    readTypes (pe.GetMetadataReader())

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
let assertTypeMembersMetadata (label: string) (md: MetadataReader) (expected: ExpectedType list) : unit =
    let actual = readTypes md

    for e in expected do
        match actual |> List.tryFind (fun t -> t.Name = e.Type) with
        | None ->
            failwithf "%s: no TypeDef named '%s'; the PE has %A" label e.Type (actual |> List.map (fun t -> t.Name))
        | Some t ->
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
