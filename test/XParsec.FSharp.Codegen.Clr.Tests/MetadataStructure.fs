/// Assertions over the metadata of an EMITTED PE, read back with
/// `MetadataReader`. `TestHelpers.loadAssembly` answers what the *runtime* makes of
/// an assembly; this answers what we actually WROTE.
///
/// It is its own module rather than more of `TestHelpers` because it is an
/// assertion library, not a fixture: `TestHelpers` builds and runs things, these
/// functions only look and complain. It sits after `TestHelpers` in compile order
/// and reuses its `openPe`, so both views of the emitted PE (reflection, raw
/// metadata) stay one `open` apart for a test.
///
/// WHY IT EXISTS. `Layout.deriveHandles` predicts every metadata handle from a
/// slot's POSITION in `layout.Types`, prefix-summing each slot's `FieldCount` /
/// `MethodCount` before a single row exists; `Assembler` then walks the layout's
/// Fields and Methods lists and writes the rows. The emitter's own handle checks
/// compare a returned handle against a prediction derived from the same position in
/// the same list the writer is walking, so they cannot see a mis-ordering that
/// PRESERVES the counts — Types reordered while Fields is not, say. Such an assembly
/// still has a perfectly well-formed metadata *shape*: the field/method ranges are a
/// gap-free partition of the tables no matter how the rows are permuted, because
/// ECMA-335 stores only each `TypeDef`'s FIRST field/method and derives the end from
/// the next row. What breaks is only WHICH rows land in which type's range.
///
/// So the structural assertions here (`assertWellFormed`) pin the shape and catch
/// drift — an out-of-band row, a range that does not start at 1, a `TypeDef` table
/// that does not cover its field/method tables — while the two assertions with real
/// teeth against a counts-preserving mis-order are `assertTypeMembers` (the rows in
/// each type's range are the ones that BELONG to it, by name) and
/// `assertEntryPointOwner` (`Main`'s row, whose handle comes from the *methods* list,
/// lies inside the `Program` slot's range, whose bounds come from the *types* list —
/// the one place the two orders are cross-checked).
module XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// Widen a specific metadata handle to `EntityHandle` — SRTP picks the
/// `-> EntityHandle` overload of `op_Implicit` over the `-> Handle` one, and doing it
/// explicitly stays off F#'s implicit-conversion warning.
let inline private toEntity (h: ^T) : EntityHandle =
    (^T: (static member op_Implicit: ^T -> EntityHandle) h)

let inline private rowOf (h: ^T) : int = MetadataTokens.GetRowNumber(toEntity h)

/// A `TypeDef` row as the PE actually carries it: the metadata spelling of its name,
/// its table row, and the field / method rows its `FieldList` / `MethodList` range
/// claims — in row order.
type EmittedType =
    {
        /// `Ns.Name` for a top-level type; `Ns.Outer+Inner` for a nested one (the
        /// CLR's own spelling, the one `Assembly.GetType` binds).
        Name: string
        Row: int
        Fields: string list
        Methods: string list
    }

/// What a caller expects one type's rows to be. The name spelling matches
/// `EmittedType.Name`.
type ExpectedType =
    {
        Type: string
        Fields: string list
        Methods: string list
    }

/// The six `Nested*` values of the 3-bit visibility field. A `TypeDef` is nested iff
/// its visibility is one of them — nesting is a *replacement* of that field, not an
/// addition, so the flags and the `NestedClass` table must agree (asserted below).
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
                // `assertWellFormed` reports it. Render it plainly rather than crash.
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

// ---- 1. The range partition -------------------------------------------------
// Walk the `TypeDef` rows in table order and assert their field and method ranges
// are consecutive, non-overlapping, gap-free, and together cover the `Field` /
// `MethodDef` tables exactly — the direct statement of the prefix-sum assumption
// `deriveHandles` makes.

let private assertRangePartition (label: string) (md: MetadataReader) =
    // One pass per table; `claim` is indexed by row id (1-based), so a row claimed
    // twice (overlap) or never (gap) is a lookup, not a search.
    let check (table: string) (total: int) (ranges: (string * int list) list) =
        let owner = Array.create (total + 1) ""
        let mutable cursor = 1

        for typeName, rows in ranges do
            match rows with
            | [] -> ()
            | first :: _ ->
                if first <> cursor then
                    failwithf
                        "%s: %s range of '%s' starts at row %d but the previous types claim through %d — the ranges are not consecutive"
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

// ---- 3. `<Module>` is TypeDef row 1 -----------------------------------------

let private assertModuleRow (label: string) (md: MetadataReader) =
    match List.ofSeq md.TypeDefinitions with
    | [] -> failwithf "%s: the TypeDef table is empty — not even the <Module> pseudo-type" label
    | first :: _ ->
        let td = md.GetTypeDefinition first
        let name = md.GetString td.Name

        if name <> "<Module>" then
            failwithf "%s: TypeDef row 1 is '%s', not '<Module>'" label name

        if not (String.IsNullOrEmpty(md.GetString td.Namespace)) then
            failwithf "%s: the <Module> pseudo-type carries namespace '%s'" label (md.GetString td.Namespace)

// ---- 4 + 6. `NestedClass` rows, and a nested type's flags --------------------
// A type is nested iff its visibility flags say so; its enclosing type comes from
// the `NestedClass` table. The two must agree exactly — the reader finds the
// enclosing row by BINARY SEARCH, so an unsorted `NestedClass` table shows up here
// as a nested-flagged type with no enclosing type, and a duplicate or orphan row
// shows up in the row count.

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

            // A nested type must follow its enclosing type in the table: pre-order
            // (5) depends on it, and the CLI requires it.
            if rowOf encl >= rowOf h then
                failwithf
                    "%s: nested type '%s' (row %d) precedes its enclosing type (row %d)"
                    label
                    name
                    (rowOf h)
                    (rowOf encl)

            // Its namespace column belongs to its OUTERMOST container; its own name is a
            // single segment, never a dotted path.
            if not (String.IsNullOrEmpty(md.GetString td.Namespace)) then
                failwithf
                    "%s: nested type '%s' carries namespace '%s' — a nested TypeDef's namespace column is empty"
                    label
                    name
                    (md.GetString td.Namespace)

            if name.Contains "." then
                failwithf
                    "%s: nested type name '%s' is dotted — the containment chain belongs in NestedClass, not the name"
                    label
                    name

            let visibility = td.Attributes &&& TypeAttributes.VisibilityMask

            if not (nestedVisibilities.Contains visibility) then
                failwithf "%s: nested type '%s' has visibility %A, which is not a Nested* value" label name visibility

    let rows = md.GetTableRowCount TableIndex.NestedClass

    if rows <> nestedCount then
        failwithf
            "%s: the NestedClass table has %d rows but %d TypeDefs carry nested visibility — a nesting row is orphaned or duplicated"
            label
            rows
            nestedCount

// ---- 5. Pre-order contiguity -------------------------------------------------
// A type at row r that transitively encloses n types owns rows (r, r+n] and nothing
// else. This is what makes the table HIERARCHICAL rather than merely legal: a nested
// type may sit anywhere after its enclosing type and still be valid metadata, but
// only a pre-order walk puts each module class's subtree immediately after it.

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
                "%s: '%s' (row %d) encloses %d types but they are not the contiguous block %A — got %A"
                label
                (nameOf md h)
                row
                (List.length sub)
                expected
                sub

// ---- The public assertions ---------------------------------------------------

/// Every structural invariant `deriveHandles`'s prefix-sum prediction assumes, over
/// an already-open reader. Split from `assertWellFormed` so a hand-built metadata
/// image (no PE around it) can be checked too.
let assertWellFormedMetadata (label: string) (md: MetadataReader) : unit =
    assertModuleRow label md
    assertRangePartition label md
    assertNestedClassRows label md
    assertPreOrderContiguity label md

/// Every `TypeDef` row of an emitted PE, with the rows its ranges claim — the
/// diagnosis view (and how a caller writes an `ExpectedType` pin in the first place).
let emittedTypes (bytes: byte[]) : EmittedType list =
    use pe = openPe bytes
    readTypes (pe.GetMetadataReader())

/// `assertWellFormedMetadata` over emitted PE bytes.
let assertWellFormed (label: string) (bytes: byte[]) : unit =
    use pe = openPe bytes
    assertWellFormedMetadata label (pe.GetMetadataReader())

/// `assertWellFormed` over a materialised PE on disk.
let assertWellFormedFile (label: string) (path: string) : unit =
    assertWellFormed label (IO.File.ReadAllBytes path)

/// The rows in each named type's range are EXACTLY these, in order. Types not named
/// are not checked, so a test pins what it knows.
///
/// This is the assertion a counts-preserving mis-order cannot survive: the field and
/// method ROWS come from `layout.Fields` / `layout.Methods`, while the range that
/// claims them comes from prefix-summing `layout.Types` — permute one list against
/// the other and every count, every range and every total still agrees, but the names
/// inside each range are another type's.
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

/// The entry point is `typeName::methodName`. `Main`'s `MethodDef` handle is
/// predicted from its index in `layout.Methods` (it is appended GLOBALLY LAST), while
/// the range that must contain it is prefix-summed from `layout.Types` — so this
/// fails the moment the `Program` slot stops being the final type, or the two lists
/// otherwise fall out of step.
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
