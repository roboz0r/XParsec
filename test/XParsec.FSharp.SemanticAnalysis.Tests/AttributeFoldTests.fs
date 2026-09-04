module XParsec.FSharp.SemanticAnalysis.Tests.AttributeFoldTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Attributes land in the frozen tree as `TAttributes` — resolved `TypeKey` plus
// constant-folded arguments — at every declaration position: the type decl itself, its
// members, union cases, record fields and enum cases. Whole-pipeline (`freezeFor`), so the
// assertions cover NameResolution's registration, Elaborate's projection and the freeze.

let private src (lines: string list) = String.concat "\n" lines

/// The frozen `TTypeDecl` named `name`, off the unpooled tree.
let private typeDecl (pools: FrozenPools) (name: string) =
    (TastUnpool.ofPools pools).Decls
    |> EqArray.toList
    |> List.tryPick (fun d ->
        match d with
        | TDeclG.Type td when td.Name = name -> Some td
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failtestf "no frozen type decl named %s" name)

/// The one `Mark` attribute in `attrs`, as its folded argument list.
let private markArgs (attrs: TAttributes) : TAttributeArg list =
    match attrs |> EqArray.toList |> List.filter (fun a -> a.Key.Name = "MarkAttribute") with
    | [ a ] -> EqArray.toList a.Args
    | other -> failtestf "expected exactly one Mark attribute, got %d" (List.length other)

let private positional (v: TConstValue) : TAttributeArg =
    {
        Name = ValueNone
        Value = v
        EnumKey = ValueNone
    }

let private named (n: string) (v: TConstValue) : TAttributeArg =
    {
        Name = ValueSome n
        Value = v
        EnumKey = ValueNone
    }

/// A named enum-typed argument: the value plus the enum's key.
let private namedEnum (n: string) (enumKey: TypeKey) (v: TConstValue) : TAttributeArg =
    {
        Name = ValueSome n
        Value = v
        EnumKey = ValueSome enumKey
    }

let private int32 (v: int64) : TConstValue = TConstValue.Integral(IntKind.Int32, v)

/// Every position marked: `Mark` is an ordinary project-local class, `Targets` a
/// project-local enum for the enum-valued argument.
let private markedSource =
    src
        [
            "type MarkAttribute(n: int, s: string) ="
            "    member this.N = n"
            ""
            "type Targets ="
            "    | A = 1"
            "    | B = 2"
            ""
            // `=` and `|||` share F#'s comparison tier, so the named value is parenthesised,
            // exactly as fsc requires.
            "[<Mark(-3, \"hi\", Extra = (Targets.A ||| Targets.B))>]"
            "type Point = { [<Mark(1, \"f\")>] X: int }"
            ""
            "[<Mark(2, \"u\")>]"
            "type Shape = | [<Mark(3, \"c\")>] Circle of int"
            ""
            "type Palette = | [<Mark(4, \"e\")>] Red = 1"
            ""
            "type Widget() ="
            "    [<Mark(5, \"m\")>]"
            "    member this.M() = 1"
        ]

[<Tests>]
let tests =
    testList
        "Attribute fold"
        [
            test "a type decl's attribute arrives folded: negative int, string, enum-valued named setter" {
                let pools = freezeFor markedSource

                match FrozenPools.blockingErrors pools with
                | [] -> ()
                | errs -> failtestf "the marked source must analyse clean, got %A" errs

                Expect.equal
                    (markArgs (typeDecl pools "Point").Attributes)
                    [
                        positional (int32 -3L)
                        positional (TConstValue.String "hi")
                        namedEnum "Extra" (typeDecl pools "Targets").TypeKey (int32 3L)
                    ]
                    "`Targets.A ||| Targets.B` folds through the local enum's case table, keeping its key"
            }

            test "a record field's attribute arrives folded" {
                let pools = freezeFor markedSource

                let field =
                    match (typeDecl pools "Point").Kind with
                    | TTypeKindG.Record r -> r.Fields.[0]
                    | other -> failtestf "Point is not a record: %A" other

                Expect.equal
                    (markArgs field.Attributes)
                    [ positional (int32 1L); positional (TConstValue.String "f") ]
                    "field X carries its folded Mark"
            }

            test "a union decl's and its case's attributes arrive folded" {
                let pools = freezeFor markedSource
                let decl = typeDecl pools "Shape"

                Expect.equal
                    (markArgs decl.Attributes)
                    [ positional (int32 2L); positional (TConstValue.String "u") ]
                    "the union decl carries its folded Mark"

                let case =
                    match decl.Kind with
                    | TTypeKindG.Union u -> u.Cases.[0]
                    | other -> failtestf "Shape is not a union: %A" other

                Expect.equal
                    (markArgs case.Attributes)
                    [ positional (int32 3L); positional (TConstValue.String "c") ]
                    "case Circle carries its folded Mark"
            }

            test "an enum case's attribute arrives folded" {
                let pools = freezeFor markedSource

                let case =
                    match (typeDecl pools "Palette").Kind with
                    | TTypeKindG.Enum cases -> cases.[0]
                    | other -> failtestf "Palette is not an enum: %A" other

                Expect.equal
                    (markArgs case.Attributes)
                    [ positional (int32 4L); positional (TConstValue.String "e") ]
                    "case Red carries its folded Mark"
            }

            test "a member's attribute arrives folded" {
                let pools = freezeFor markedSource

                let m =
                    TTypeKindG.members (typeDecl pools "Widget").Kind
                    |> EqArray.toList
                    |> List.tryFind (fun m -> m.Name = "M")
                    |> Option.defaultWith (fun () -> failtest "no member M on Widget")

                Expect.equal
                    (markArgs m.Attributes)
                    [ positional (int32 5L); positional (TConstValue.String "m") ]
                    "member M carries its folded Mark"
            }

            test "AttributeUsage folds through the contract's AttributeTargets enum" {
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = true)>]"
                                "type MineAttribute() ="
                                "    member this.M() = 1"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "the usage source analyses clean"

                let usage =
                    (typeDecl pools "MineAttribute").Attributes
                    |> EqArray.toList
                    |> List.tryFind (fun a -> a.Key.Name = "AttributeUsageAttribute")
                    |> Option.defaultWith (fun () -> failtest "no AttributeUsage on MineAttribute")

                Expect.equal
                    (EqArray.toList usage.Args)
                    [
                        // Class = 4, Struct = 8, read off the external contract's case table.
                        {
                            Name = ValueNone
                            Value = int32 12L
                            EnumKey = ValueSome RuntimeNames.attributeTargetsKey
                        }
                        named "AllowMultiple" (TConstValue.Bool true)
                    ]
                    "`AttributeTargets.Class ||| AttributeTargets.Struct` folds to 12 with the enum's key"
            }

            test "an argument outside the constant domain is diagnosed and the attribute is omitted whole" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int, s: string) ="
                                "    member this.N = n"
                                ""
                                "[<Mark(1 + 1, \"x\")>]"
                                "type W = { Y: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "not a valid constant expression" "FS0267's wording"
                | other -> failtestf "expected exactly one error, got %A" other

                // Never a positionally-shifted argument list: the whole attribute is absent.
                Expect.isEmpty
                    (EqArray.toList (typeDecl pools "W").Attributes)
                    "the attribute with the failing argument is dropped whole"
            }

            test "a [<Literal>] value reference in an attribute argument folds: bare, qualified, composed" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "[<Literal>]"
                                "let Mask = 3"
                                ""
                                "module Deep ="
                                "    [<Literal>]"
                                "    let Bit = 8"
                                ""
                                // A literal's own RHS may reference earlier literals (F# 5+).
                                "[<Literal>]"
                                "let Combined = Mask ||| Deep.Bit"
                                ""
                                "[<Mark(Mask)>]"
                                "type A = { X: int }"
                                ""
                                "[<Mark(Deep.Bit)>]"
                                "type B = { X: int }"
                                ""
                                "[<Mark(Combined)>]"
                                "type C = { X: int }"
                                ""
                                "[<Mark(Mask ||| Deep.Bit)>]"
                                "type D = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools with
                | [] -> ()
                | errs -> failtestf "the literal source must analyse clean, got %A" errs

                Expect.equal (markArgs (typeDecl pools "A").Attributes) [ positional (int32 3L) ] "bare Mask"
                Expect.equal (markArgs (typeDecl pools "B").Attributes) [ positional (int32 8L) ] "qualified Deep.Bit"

                Expect.equal
                    (markArgs (typeDecl pools "C").Attributes)
                    [ positional (int32 11L) ]
                    "Combined folded through its own literal references"

                Expect.equal
                    (markArgs (typeDecl pools "D").Attributes)
                    [ positional (int32 11L) ]
                    "Mask ||| Deep.Bit folds in argument position"
            }

            test "a non-literal value reference in an attribute argument is FS0267" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "let notLit = 3"
                                ""
                                "[<Mark(notLit)>]"
                                "type W = { Y: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "not a valid constant expression" "FS0267's wording"
                | other -> failtestf "expected exactly one error, got %A" other

                Expect.isEmpty
                    (EqArray.toList (typeDecl pools "W").Attributes)
                    "the attribute with the failing argument is dropped whole"
            }

            test "a [<Literal>] binding whose RHS does not fold is FS0267 at the RHS" {
                let pools = freezeFor (src [ "let f () = 1"; ""; "[<Literal>]"; "let X = f ()" ])

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "not a valid constant expression" "FS0267's wording"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "a forward literal reference (use above the declaration) is FS0267" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "[<Mark(Later)>]"
                                "type F = { Y: int }"
                                ""
                                "[<Literal>]"
                                "let Later = 1"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains
                        msg
                        "not a valid constant expression"
                        "top-down scoping refuses the forward ref"
                | other -> failtestf "expected exactly one error, got %A" other

                Expect.isEmpty
                    (EqArray.toList (typeDecl pools "F").Attributes)
                    "the attribute with the forward reference is dropped whole"
            }

            test "an unresolved member-position attribute is an error" {
                let pools = freezeFor "type C2() =\n    [<FableImport>]\n    member this.M() = 1\n"

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "FableImport" "the diagnostic quotes the name as written"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "an out-of-range argument reports the width claim, not FS0267" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "[<Mark(300uy)>]"
                                "type W = { Y: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains msg "not representable at its authored width" "the OutOfRange rejection"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "an argument-bearing attribute folds beside a bracket sibling" {
                // `[<Mark(2, "u"); RequireQualifiedAccess>]` in ONE bracket: the `;` separates
                // the two attributes rather than sequencing `Mark`'s argument.
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int, s: string) ="
                                "    member this.N = n"
                                ""
                                "[<Mark(2, \"u\"); RequireQualifiedAccess>]"
                                "type Shape = | Circle of int"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "the bracket-sibling source analyses clean"
                let decl = typeDecl pools "Shape"

                Expect.equal
                    (markArgs decl.Attributes)
                    [ positional (int32 2L); positional (TConstValue.String "u") ]
                    "Mark folds beside its bracket sibling"

                Expect.isTrue decl.IsRequireQualifiedAccess "the sibling attribute lands too"
            }

            ptest "GAP: AllowMultiple is not enforced" {
                // A repeated attribute whose declaration carries `AllowMultiple = false`
                // (the default) should error; today both instances fold silently.
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "[<Mark(1)>]"
                                "[<Mark(2)>]"
                                "type W = { Y: int }"
                            ]
                    )

                Expect.isNonEmpty (FrozenPools.blockingErrors pools) "a repeated single-use attribute is an error"
            }
        ]

// The equality / comparison / qualified-access verdicts are member views over the frozen
// attribute list, computed by the same `AttributeVerdicts` decode the validation pass reads.

[<Tests>]
let verdictViewTests =
    testList
        "Attribute verdict views"
        [
            test "verdict views derive from the frozen attribute list" {
                let pick (source: string) (name: string) = typeDecl (freezeFor source) name

                let plain = pick "type Point = { X: int }" "Point"

                Expect.equal plain.EqualitySupport EqualityVerdict.Structural "an all-immutable record is structural"
                Expect.equal plain.ComparisonSupport ComparisonVerdict.NoComparison "comparison is opt-in"
                Expect.isFalse plain.IsRequireQualifiedAccess "no RQA attribute written"

                let refEq = pick "[<ReferenceEquality>]\ntype Point = { X: int }" "Point"

                Expect.equal
                    refEq.EqualitySupport
                    EqualityVerdict.Reference
                    "[<ReferenceEquality>] reaches the view through the frozen attribute list"

                let mutRecord = pick "type Point = { mutable X: int }" "Point"

                Expect.equal
                    mutRecord.EqualitySupport
                    EqualityVerdict.Structural
                    "a mutable field keeps the structural default, as fsc's does"

                let refEqStruct =
                    freezeFor "[<Struct; ReferenceEquality>]\ntype SPoint = { X: int }"

                Expect.exists
                    (FrozenPools.blockingErrors refEqStruct)
                    (fun d -> d.Message.Contains "'ReferenceEquality' attribute cannot be used on structs")
                    "[<ReferenceEquality>] on a struct record is FS0376"

                Expect.equal
                    (typeDecl refEqStruct "SPoint").EqualitySupport
                    EqualityVerdict.Structural
                    "the refused posture stamps no verdict; the struct record stays structural"

                let rqa = pick "[<RequireQualifiedAccess>]\ntype Shape = Circle of int" "Shape"

                Expect.isTrue
                    rqa.IsRequireQualifiedAccess
                    "[<RequireQualifiedAccess>] is carried on the frozen attribute list"

                Expect.equal rqa.EqualitySupport EqualityVerdict.Structural "a union is structural by default"

                let cmp = pick "[<StructuralComparison>]\ntype Point = { X: int }" "Point"

                Expect.equal
                    cmp.ComparisonSupport
                    ComparisonVerdict.Structural
                    "[<StructuralComparison>] reaches the view through the frozen attribute list"
            }
        ]

// `[<AttributeUsage>]` target enforcement (FS0842's wording, error severity): the used
// attribute declaration's mask — local registry or referenced contract — against the flags
// of the element it is written on. A declaration without a reachable AttributeUsage passes
// everywhere.

[<Tests>]
let targetTests =
    testList
        "AttributeUsage targets"
        [
            test "a Class-only local attribute passes on a class, errors on a method and on a let value" {
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Class)>]"
                                "type ClsOnlyAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<ClsOnly>]"
                                "type Good() ="
                                "    member this.X = 1"
                                ""
                                "type Bad() ="
                                "    [<ClsOnly>]"
                                "    member this.Y() = 1"
                                ""
                                "[<ClsOnly>]"
                                "let v = 1"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ onMethod; onLet ] ->
                    Expect.equal
                        onMethod
                        "This attribute cannot be applied to method, return value. Valid targets are: class"
                        "fsc's element list for a method"

                    Expect.equal
                        onLet
                        "This attribute cannot be applied to property, field, return value. Valid targets are: class"
                        "fsc's element list for a module value"
                | other -> failtestf "expected the method and let errors, got %A" other
            }

            test "[<Global>] on a type errors: the contract declares Property ||| Field" {
                let pools = freezeFor (src [ "[<Global>]"; "type G() ="; "    member this.X = 1" ])

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to class. Valid targets are: property, field"
                        "the Vesper contract's GlobalAttribute mask"
                | other -> failtestf "expected exactly the [<Global>] error, got %A" other
            }

            test "an attribute with no AttributeUsage passes on a type, a method and a let" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type FreeAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<Free>]"
                                "type T() ="
                                "    [<Free>]"
                                "    member this.Y() = 1"
                                ""
                                "[<Free>]"
                                "let v = 1"
                            ]
                    )

                Expect.isEmpty
                    (errorMessages (FrozenPools.blockingErrors pools))
                    "no AttributeUsage defaults to AttributeTargets.All"
            }

            test "a Class ||| Struct mask passes on a class and a struct record, errors on an interface" {
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct)>]"
                                "type CSAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<CS>]"
                                "type C() ="
                                "    member this.X = 1"
                                ""
                                "[<CS>]"
                                "[<Struct>]"
                                "type S = { X: int }"
                                ""
                                "[<CS>]"
                                "type I ="
                                "    abstract M: unit -> int"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to interface. Valid targets are: class, struct"
                        "the folded ||| mask admits class and struct only"
                | other -> failtestf "expected exactly the interface error, got %A" other
            }

            test "a contract-declared mask is enforced: [<Sealed>] on a method errors" {
                let pools =
                    freezeFor (src [ "type K() ="; "    [<Sealed>]"; "    member this.M() = 1" ])

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to method, return value. Valid targets are: class"
                        "SealedAttribute's contract mask is Class"
                | other -> failtestf "expected exactly the [<Sealed>] error, got %A" other
            }
        ]
