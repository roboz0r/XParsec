module XParsec.FSharp.SemanticAnalysis.Tests.AttributeFoldTests

open Vesper
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Attributes land in the frozen tree as `TAttributes` — resolved `TypeKey` plus
// constant-folded arguments — at every declaration position: the type decl itself, its
// members, union cases, record fields and enum cases. Whole-pipeline (`freezeFor`), so the
// assertions cover NameResolution's registration, Elaborate's projection and the freeze.

let private src (lines: string list) = String.concat "\n" lines

let private typeDecl = frozenTypeDecl

/// One argument by the member it sets (`ValueNone` for a constructor parameter) and what it
/// denotes. The stored expression carries its own source site, so two spellings of one value
/// have unequal trees.
let private argView (a: TAttributeArg) : string voption * TConstDenotation =
    let name =
        match a.Target with
        | TAttributeArgTarget.Parameter _ -> ValueNone
        | TAttributeArgTarget.Member m -> ValueSome m.Name

    name, TConstExpr.denotation a.Expr

/// The one `Mark` attribute in `attrs`, as its folded argument list.
let private markArgs (attrs: TAttributes) : (string voption * TConstDenotation) list =
    match attrs |> Block.toList |> List.filter (fun a -> a.Key.Name = "MarkAttribute") with
    | [ a ] -> [ for arg in a.Args -> argView arg ]
    | other -> failtestf "expected exactly one Mark attribute, got %d" (List.length other)

let private scalar (v: TConstValue) : TConstDenotation =
    {
        Result = TConstResult.Scalar v
        Ty = FTConst(TConstValue.canonKey v, Block.empty)
    }

/// An enum-typed argument: the case's underlying value at the enum's type.
let private enumScalar (enumKey: TypeKey) (v: TConstValue) : TConstDenotation =
    {
        Result = TConstResult.Scalar v
        Ty = FTEnum enumKey
    }

/// A `typeof<T>` / `typedefof<T>` argument: the reified type, at `Vesper.Type`.
let private reified (operand: FrozenType) : TConstDenotation =
    {
        Result = TConstResult.TypeVal operand
        Ty = FTConst(RuntimeNames.runtimeTypeKey, Block.empty)
    }

let private positional (v: TConstValue) = ValueNone, scalar v

let private named (n: string) (v: TConstValue) = ValueSome n, scalar v

let private namedEnum (n: string) (enumKey: TypeKey) (v: TConstValue) = ValueSome n, enumScalar enumKey v

let private int32 (v: int) : TConstValue = TConstValue.Integral(IntValue.Int32 v)

/// Every position marked: `Mark` is an ordinary project-local class, `Targets` a
/// project-local enum for the enum-valued argument, which the settable `Extra` takes.
let private markedSource =
    src
        [
            "type Targets ="
            "    | A = 1"
            "    | B = 2"
            ""
            "type MarkAttribute(n: int, s: string) ="
            "    member this.N = n"
            "    member val Extra = Targets.A with get, set"
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
                        positional (int32 -3)
                        positional (TConstValue.String "hi")
                        namedEnum "Extra" (typeDecl pools "Targets").TypeKey (int32 3)
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
                    [ positional (int32 1); positional (TConstValue.String "f") ]
                    "field X carries its folded Mark"
            }

            test "a union decl's and its case's attributes arrive folded" {
                let pools = freezeFor markedSource
                let decl = typeDecl pools "Shape"

                Expect.equal
                    (markArgs decl.Attributes)
                    [ positional (int32 2); positional (TConstValue.String "u") ]
                    "the union decl carries its folded Mark"

                let case =
                    match decl.Kind with
                    | TTypeKindG.Union u -> u.Cases.[0]
                    | other -> failtestf "Shape is not a union: %A" other

                Expect.equal
                    (markArgs case.Attributes)
                    [ positional (int32 3); positional (TConstValue.String "c") ]
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
                    [ positional (int32 4); positional (TConstValue.String "e") ]
                    "case Red carries its folded Mark"
            }

            test "a member's attribute arrives folded" {
                let pools = freezeFor markedSource

                let m =
                    TTypeKindG.members (typeDecl pools "Widget").Kind
                    |> Block.toList
                    |> List.tryFind (fun m -> m.Name = "M")
                    |> Option.defaultWith (fun () -> failtest "no member M on Widget")

                Expect.equal
                    (markArgs m.Attributes)
                    [ positional (int32 5); positional (TConstValue.String "m") ]
                    "member M carries its folded Mark"
            }

            test "reified type arguments fold and survive the codec" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(t: Type) ="
                                "    member this.T = t"
                                ""
                                "type Box<'T> = { v: 'T }"
                                ""
                                "[<Mark(typeof<int>)>]"
                                "type Instantiated = { X: int }"
                                ""
                                "[<Mark(typedefof<Box<_>>)>]"
                                "type Defined = { Y: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "the reified source analyses clean"
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)
                let boxKey = (typeDecl thawed "Box").TypeKey

                Expect.equal
                    (markArgs (typeDecl thawed "Instantiated").Attributes)
                    [ ValueNone, reified (FTConst(RuntimeNames.intKey, Block.empty)) ]
                    "typeof<int> reifies the primitive's own identity"

                Expect.equal
                    (markArgs (typeDecl thawed "Defined").Attributes)
                    [ ValueNone, reified (FTRecord(boxKey, Block.empty)) ]
                    "typedefof<Box<_>> reifies the arity-1 identity with no arguments"
            }

            test "array arguments fold and survive the codec" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type Targets5 ="
                                "    | A = 1"
                                "    | B = 2"
                                ""
                                "type MarkAttribute(xs: int[], es: Targets5[]) ="
                                "    member this.Xs = xs"
                                "    member this.Es = es"
                                ""
                                "[<Mark([| 1; 2 |], [| Targets5.A; Targets5.A ||| Targets5.B |])>]"
                                "type Arrayed = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "the array source analyses clean"
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)
                let targetsKey = (typeDecl thawed "Targets5").TypeKey

                let arrayOf (elemTy: FrozenType) (items: TConstValue list) : TConstDenotation =
                    {
                        Result = TConstResult.ArrayVal(Block.ofList [ for v in items -> TConstResult.Scalar v ])
                        Ty = ftArray elemTy
                    }

                Expect.equal
                    (markArgs (typeDecl thawed "Arrayed").Attributes)
                    [
                        ValueNone, arrayOf (FTConst(RuntimeNames.intKey, Block.empty)) [ int32 1; int32 2 ]
                        ValueNone, arrayOf (FTEnum targetsKey) [ int32 1; int32 3 ]
                    ]
                    "each array carries its items at the first item's type"
            }

            // The representation is target-neutral, so the front end keeps a `decimal` the CLR
            // blob lacks an `Elem` for. Analysis here runs without platform facts, so the gate
            // stays quiet; `AttributeRowTests` pins the CLR refusal of this same source.
            test "a decimal argument folds and survives the codec, ungated" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(d: decimal) ="
                                "    member this.D = d"
                                ""
                                "[<Mark(1.5M)>]"
                                "type Priced = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "the decimal source analyses clean"
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)

                Expect.equal
                    (markArgs (typeDecl thawed "Priced").Attributes)
                    [ positional (TConstValue.Decimal 1.5M) ]
                    "the decimal argument reaches the frozen tree"
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
                    |> Block.toList
                    |> List.tryFind (fun a -> a.Key.Name = "AttributeUsageAttribute")
                    |> Option.defaultWith (fun () -> failtest "no AttributeUsage on MineAttribute")

                Expect.equal
                    [ for a in usage.Args -> argView a ]
                    [
                        // Class = 4, Struct = 8, read off the external contract's case table.
                        ValueNone, enumScalar RuntimeNames.attributeTargetsKey (int32 12)
                        named "AllowMultiple" (TConstValue.Bool true)
                    ]
                    "`AttributeTargets.Class ||| AttributeTargets.Struct` folds to 12 with the enum's key"
            }

            // Each multi-flag `[<AttributeUsage>]` mask `Vesper.Core` publishes, against the
            // `System.AttributeTargets` combination FSharp.Core declares it with.
            test "Vesper.Core's published AttributeUsage masks are the FSharp.Core combinations" {
                let expected: (string * System.AttributeTargets) list =
                    let classOrStruct = System.AttributeTargets.Class ||| System.AttributeTargets.Struct

                    [
                        "AutoOpen", classOrStruct ||| System.AttributeTargets.Assembly
                        "StructuralEquality", classOrStruct
                        "StructuralComparison", classOrStruct
                        "CustomEquality", classOrStruct
                        "CustomComparison", classOrStruct
                        "DefaultAugmentation", classOrStruct
                        "NoEquality",
                        classOrStruct
                        ||| System.AttributeTargets.Interface
                        ||| System.AttributeTargets.Delegate
                        ||| System.AttributeTargets.Enum
                        "NoComparison",
                        classOrStruct
                        ||| System.AttributeTargets.Interface
                        ||| System.AttributeTargets.Delegate
                        ||| System.AttributeTargets.Enum
                        "AllowNullLiteral", System.AttributeTargets.Class ||| System.AttributeTargets.Interface
                        "Global", System.AttributeTargets.Property ||| System.AttributeTargets.Field
                        "Import",
                        System.AttributeTargets.Method
                        ||| System.AttributeTargets.Property
                        ||| System.AttributeTargets.Field
                        "Struct",
                        classOrStruct
                        ||| System.AttributeTargets.ReturnValue
                        ||| System.AttributeTargets.Parameter
                        "RequireQualifiedAccess", classOrStruct ||| System.AttributeTargets.Enum
                        "CompiledName",
                        classOrStruct
                        ||| System.AttributeTargets.Method
                        ||| System.AttributeTargets.Field
                        ||| System.AttributeTargets.Interface
                        ||| System.AttributeTargets.Delegate
                        ||| System.AttributeTargets.Enum
                        ||| System.AttributeTargets.Property
                        "Measure", System.AttributeTargets.GenericParameter ||| System.AttributeTargets.Class
                    ]

                let publishedMask (name: string) : (string voption * TConstDenotation) option =
                    let key = SymbolKeyOps.typeKeyOf "Vesper" (name + "Attribute")

                    realProvider.Value.TryLookupAttributes(SymbolKey.Type key)
                    |> Block.toList
                    |> List.tryFind (fun a -> a.Key = RuntimeNames.attributeUsageAttributeKey)
                    |> Option.map (fun usage -> argView usage.Args.[0])

                for (name, targets) in expected do
                    Expect.equal
                        (publishedMask name)
                        (Some(ValueNone, enumScalar RuntimeNames.attributeTargetsKey (int32 (int targets))))
                        name
            }

            test "an argument outside the constant domain is diagnosed and the attribute is omitted whole" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int, s: string) ="
                                "    member this.N = n"
                                ""
                                "[<Mark(id 1, \"x\")>]"
                                "type W = { Y: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "not a valid constant expression" "FS0267's wording"
                | other -> failtestf "expected exactly one error, got %A" other

                // Never a positionally-shifted argument list: the whole attribute is absent.
                Expect.isEmpty
                    (Block.toList (typeDecl pools "W").Attributes)
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

                Expect.equal (markArgs (typeDecl pools "A").Attributes) [ positional (int32 3) ] "bare Mask"
                Expect.equal (markArgs (typeDecl pools "B").Attributes) [ positional (int32 8) ] "qualified Deep.Bit"

                Expect.equal
                    (markArgs (typeDecl pools "C").Attributes)
                    [ positional (int32 11) ]
                    "Combined folded through its own literal references"

                Expect.equal
                    (markArgs (typeDecl pools "D").Attributes)
                    [ positional (int32 11) ]
                    "Mask ||| Deep.Bit folds in argument position"
            }

            // fsc reports FS0267 for the same source: an attribute argument applies the
            // binding its operator spelling denotes, and a `let (|||)` denotes that one.
            test "an argument whose ||| is shadowed by a local definition is FS0267" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "let (|||) (a: int) (b: int) = 999"
                                ""
                                "[<Mark(1 ||| 2)>]"
                                "type W = { Y: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "not a valid constant expression" "FS0267's wording"
                | other -> failtestf "expected exactly one error, got %A" other

                Expect.isEmpty
                    (Block.toList (typeDecl pools "W").Attributes)
                    "the attribute with the shadowed operator is dropped whole"
            }

            test "an enum-converted argument carries the named enum's type" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type Targets = | Class = 4 | Struct = 8"
                                ""
                                "type MarkAttribute(t: Targets) ="
                                "    member this.T = t"
                                ""
                                "[<Mark(enum<Targets> 12)>]"
                                "type A = { X: int }"
                                ""
                                "[<Mark(LanguagePrimitives.EnumOfValue<int, Targets> 12)>]"
                                "type B = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "no errors"
                let targetsKey = SymbolKeyOps.typeKeyOf "" "Targets"

                Expect.equal
                    (markArgs (typeDecl pools "A").Attributes)
                    [ ValueNone, enumScalar targetsKey (int32 12) ]
                    "enum<Targets> 12"

                Expect.equal
                    (markArgs (typeDecl pools "B").Attributes)
                    [ ValueNone, enumScalar targetsKey (int32 12) ]
                    "the qualified spelling denotes the same thing"
            }

            test "an argument whose ||| is NOT shadowed folds through the intrinsic" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "[<Mark(1 ||| 2)>]"
                                "type W = { Y: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "no errors"
                Expect.equal (markArgs (typeDecl pools "W").Attributes) [ positional (int32 3) ] "1 ||| 2"
            }

            test "arithmetic, shift, string and bool arguments fold through the intrinsics" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int, s: string, b: bool) ="
                                "    member this.N = n"
                                ""
                                "[<Literal>]"
                                "let Prefix = \"pre-\""
                                ""
                                "[<Mark(2 * 3 + (1 <<< 4), Prefix + \"fix\", not false)>]"
                                "type W = { Y: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "no errors"

                Expect.equal
                    (markArgs (typeDecl pools "W").Attributes)
                    [
                        positional (int32 22)
                        positional (TConstValue.String "pre-fix")
                        positional (TConstValue.Bool true)
                    ]
                    "each widened fold survives the codec"
            }

            // fsc reports FS0267 for the same source.
            test "an argument whose + is shadowed by a local definition is FS0267" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute(n: int) ="
                                "    member this.N = n"
                                ""
                                "let (+) (a: int) (b: int) = 999"
                                ""
                                "[<Mark(1 + 2)>]"
                                "type W = { Y: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "not a valid constant expression" "FS0267's wording"
                | other -> failtestf "expected exactly one error, got %A" other

                Expect.isEmpty
                    (Block.toList (typeDecl pools "W").Attributes)
                    "the attribute with the shadowed operator is dropped whole"
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
                    (Block.toList (typeDecl pools "W").Attributes)
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
                    (Block.toList (typeDecl pools "F").Attributes)
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
                    [ positional (int32 2); positional (TConstValue.String "u") ]
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
