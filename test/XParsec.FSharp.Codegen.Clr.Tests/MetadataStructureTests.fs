module XParsec.FSharp.Codegen.Clr.Tests.MetadataStructureTests

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

/// A program with one of everything the emitted `TypeDef` order groups by: a union, a
/// record, a class, a closure (`adder`'s lambda captures `k`), a module class holding a
/// NESTED type, the anonymous `Program` class's top-level value fields, and `Main`.
let private representative =
    String.concat
        "\n"
        [
            "type Shape ="
            "    | Dot"
            "    | Line of int"
            "type Point = { X: int; Y: int }"
            "type Counter(start: int) ="
            "    member c.Start = start"
            "    member c.Next() = c.Start + 1"
            "module M ="
            "    type Tally = { Hits: int }"
            "    let twice (x: int) = x + x"
            "    let adder (k: int) = fun (x: int) -> x + k"
            // `Tally` and its field labels reach the top level only through `open M`.
            "open M"
            "let bump = M.adder 3"
            "let p = { X = 1; Y = 2 }"
            "let c = Counter(4)"
            "let s = Line 5"
            "let t = { Hits = 7 }"
            "let n = M.twice (bump (p.X + p.Y + c.Next() + t.Hits))"
            "printfn \"%d\" n"
            "printfn \"%b\" (s = Line 5)"
        ]

let private representativeBytes: Lazy<byte[]> =
    lazy (compileSource "MetaStructRepresentative" representative |> Codegen.toBytes)

/// The same declarations as a LIBRARY (no `Main`, no `Program` slot at all).
let private libraryBytes: Lazy<byte[]> =
    lazy
        (let src =
            String.concat
                "\n"
                [
                    "namespace L"
                    ""
                    "type Shape ="
                    "    | Dot"
                    "    | Line of int"
                    ""
                    "type Point = { X: int; Y: int }"
                    ""
                    "type Counter(start: int) ="
                    "    member c.Start = start"
                ]

         compileSourceTo (ProjectInfo.library "MetaStructLibrary") src |> Codegen.toBytes)

// ---- The teeth: a hand-built, deliberately mis-ordered metadata image ---------
// The emitter cannot be made to emit a bad PE, so the corrupt image is built here with
// `MetadataBuilder` and fed to the same assertions.

/// `<Module>`, `N.A` (fields `a1`, `a2`) and `N.B` (field `b1`). The `FieldList` columns
/// always prefix-sum the TYPES, so handing `fieldRows` in another order gives a
/// counts-preserving mis-order; `firstField = 2` instead of 1 skews the prefix sum.
let private misorderedImage (fieldRows: string list) (firstField: int) : ImmutableArray<byte> =
    let mb = MetadataBuilder()

    mb.AddModule(0, mb.GetOrAddString "MisOrdered.dll", mb.GetOrAddGuid(Guid.NewGuid()), GuidHandle(), GuidHandle())
    |> ignore

    let fieldSig =
        let b = BlobBuilder()
        BlobEncoder(b).FieldSignature().Int32()
        mb.GetOrAddBlob b

    let methodSig =
        let b = BlobBuilder()

        BlobEncoder(b).MethodSignature().Parameters(0, (fun r -> r.Void()), (fun _ -> ()))

        mb.GetOrAddBlob b

    let typeRow (ns: string) (name: string) (field: int) (method: int) =
        mb.AddTypeDefinition(
            TypeAttributes.Public,
            mb.GetOrAddString ns,
            mb.GetOrAddString name,
            EntityHandle(),
            MetadataTokens.FieldDefinitionHandle field,
            MetadataTokens.MethodDefinitionHandle method
        )
        |> ignore

    // TypeDef rows first, since their FieldList/MethodList columns ARE the prefix sums.
    // `firstField` skews `<Module>` too: a mid-table skew is invisible, since a range's
    // END is the next row's start, so only a range not starting at 1 unclaims a row.
    typeRow "" "<Module>" firstField 1
    typeRow "N" "A" firstField 1
    typeRow "N" "B" (firstField + 2) 2

    // Then the row tables, in whatever order the caller asks for.
    for name in fieldRows do
        mb.AddFieldDefinition(FieldAttributes.Public, mb.GetOrAddString name, fieldSig)
        |> ignore

    for name in [ "aMethod"; "bMethod" ] do
        mb.AddMethodDefinition(
            MethodAttributes.Public ||| MethodAttributes.Static,
            MethodImplAttributes.IL,
            mb.GetOrAddString name,
            methodSig,
            -1,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore

    let root = MetadataRootBuilder(mb, null, true)
    let output = BlobBuilder()
    root.Serialize(output, 0, 0)
    output.ToImmutableArray()

let private readImage (image: ImmutableArray<byte>) =
    MetadataReaderProvider.FromMetadataImage image

[<Tests>]
let tests =
    testList
        "MetadataStructure"
        [
            test "the representative exe's metadata is well-formed" {
                assertWellFormed "representative" representativeBytes.Value
            }

            test "the library's metadata is well-formed and does not carry an entry point" {
                assertWellFormed "library" libraryBytes.Value
                assertNoEntryPoint "library" libraryBytes.Value
            }

            test "the entry point lies inside the Program type's method range" {
                assertEntryPointOwner "representative" representativeBytes.Value "Program" "Main"
            }

            test "a module function's Param rows carry the source names" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let twice (f: int -> int) (x: int) = f (f x)"
                            "let add (a: int, b: int) = a + b"
                            "let quoted (``my param``: int) = ``my param`` + 1"
                            "let dropped () (y: int) = y"
                            "printfn \"%d\" (twice (fun n -> n + 1) 1 + add (2, 3) + quoted 4 + dropped () 9)"
                        ]

                let bytes = compileSource "MetaStructParamNames" src |> Codegen.toBytes
                assertWellFormed "ParamNames" bytes
                let namesOf = methodParamNamesOf bytes "Program"

                Expect.equal (namesOf "twice") [ "f"; "x" ] "curried params take their source names"
                Expect.equal (namesOf "add") [ "a"; "b" ] "tupled params take their source names"
                Expect.equal (namesOf "quoted") [ "my param" ] "a double-backtick name is spelled verbatim"
                Expect.equal (namesOf "dropped") [ "arg0"; "y" ] "a unit placeholder keeps the positional mint"
            }

            test "a member, secondary ctor, closure, union factory and abstract slot carry the source names" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Adder(k: int) ="
                            "    new(a: int, b: int) = Adder(a + b)"
                            "    member this.Twice(n: int) = n + n + k"
                            "    member this.Both(left: int, right: int) = left + right + k"
                            "type Shape ="
                            "    | Line of x: int * int"
                            "type IOps ="
                            "    abstract Combine: left: int -> right: int -> int"
                            "    abstract Plain: int * int -> int"
                            // Passed first-class, so the lambda is a closure capturing `k`.
                            "let apply (f: int -> int -> int) = f 2 3"
                            "let run (k: int) = apply (fun (x: int) (y: int) -> x + y + k)"
                            "let a = Adder(1, 2)"
                            "let s = Line(4, 5)"
                            "printfn \"%d\" (a.Twice 3 + a.Both(1, 2) + run 1)"
                        ]

                let bytes = compileSource "MetaStructMemberParamNames" src |> Codegen.toBytes
                assertWellFormed "MemberParamNames" bytes
                let namesOf = methodParamNamesOf bytes

                Expect.equal (namesOf "Adder" "Twice") [ "n" ] "a member names its parameter"
                Expect.equal (namesOf "Adder" "Both") [ "left"; "right" ] "a tupled member names each parameter"

                let ctors =
                    paramNamesOf bytes "Adder"
                    |> List.filter (fun (n, _) -> n = ".ctor")
                    |> List.map snd

                Expect.equal ctors [ [ "k" ]; [ "a"; "b" ] ] "both ctors name their parameters"

                // The curried lambda lowers to one closure per level: the outer captures
                // `k` and takes `x`, the inner captures `x` and `k` and takes `y`.
                let closures =
                    [
                        for t in emittedTypes bytes do
                            if t.Name.StartsWith "<closure>$" then
                                t.Name, paramNamesOf bytes t.Name
                    ]

                Expect.equal
                    closures
                    [
                        "<closure>$0", [ ".ctor", [ "x"; "k" ]; "Invoke", [ "y" ] ]
                        "<closure>$1", [ ".ctor", [ "k" ]; "Invoke", [ "x" ] ]
                    ]
                    "a closure's ctor names its captures and Invoke its parameter"

                Expect.equal (namesOf "Shape" "Line") [ "_x"; "item2" ] "a factory takes the case's field names"

                Expect.equal (namesOf "IOps" "Combine") [ "left"; "right" ] "an abstract slot names its arguments"

                Expect.equal
                    (namesOf "IOps" "Plain")
                    [ "arg0"; "arg1" ]
                    "a bare-typed argument keeps the positional mint"
            }

            // The pin that makes the structural checks non-vacuous: the ACTUAL rows of
            // every type in a known assembly.
            test "each type's field and method ranges hold ITS OWN rows" {
                assertTypeMembers
                    "representative"
                    representativeBytes.Value
                    [
                        {
                            Type = "<Module>"
                            Fields = []
                            Methods = []
                        }
                        // Two cases with a payload, so `Shape` is `TypeTested`: an abstract
                        // base carrying the `_unique_Dot` singleton and the `.cctor` that
                        // fills it, then (pre-order) a nested type per case.
                        {
                            Type = "Shape"
                            Fields = [ "_unique_Dot" ]
                            Methods =
                                [
                                    ".ctor"
                                    ".cctor"
                                    "Dot"
                                    "Line"
                                    "GetHashCode"
                                    "Equals"
                                    "Equals"
                                    "Format"
                                ]
                        }
                        {
                            Type = "Shape+Dot"
                            Fields = []
                            Methods = [ ".ctor"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                        // A lone positional field takes FSC's `item`, on the case's own type.
                        {
                            Type = "Shape+Line"
                            Fields = [ "item" ]
                            Methods = [ ".ctor"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                        {
                            Type = "Point"
                            // A record field's storage carries the `@`-suffixed name; `X`
                            // and `Y` are the properties over it.
                            Fields = [ "X@"; "Y@" ]
                            Methods = [ ".ctor"; "get_X"; "get_Y"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                        {
                            Type = "Counter"
                            Fields = [ "start" ]
                            Methods = [ ".ctor"; "get_Start"; "Next" ]
                        }
                        // The lambda's capture class, then the singleton closure
                        // (`instance` field + `.cctor`) `adder` itself reifies to.
                        {
                            Type = "<closure>$0"
                            Fields = [ "capture0" ]
                            Methods = [ ".ctor"; "Invoke" ]
                        }
                        {
                            Type = "<closure>$1"
                            Fields = [ "instance" ]
                            Methods = [ ".ctor"; "Invoke"; ".cctor" ]
                        }
                        // The module's compiled module class: static methods, no fields,
                        // and immediately followed (pre-order) by the type it holds.
                        {
                            Type = "M"
                            Fields = []
                            Methods = [ "twice"; "adder" ]
                        }
                        {
                            Type = "M+Tally"
                            Fields = [ "Hits@" ]
                            Methods = [ ".ctor"; "get_Hits"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                        // The anonymous module class, last, so `Main` (the final method row)
                        // falls inside its range. A top-level value's field carries its
                        // SOURCE name; only a shadowed binding gets a slot-suffixed mint.
                        {
                            Type = "Program"
                            Fields = [ "p"; "c"; "s"; "t"; "n" ]
                            Methods = [ "Main" ]
                        }
                    ]
            }

            // The hierarchy's declaration shape: the base is abstract, each case a sealed
            // nested type extending it, and a generic union's case redeclares the union's
            // typars, so its `extends` is an instantiation rather than a bare `TypeDef`.
            test "a monomorphic hierarchy union's case extends the base by TypeDef" {
                let bytes = representativeBytes.Value

                match MetadataStructure.typeDecl bytes "Shape" with
                | ValueNone -> failtest "no Shape TypeDef"
                | ValueSome d ->
                    Expect.isTrue d.IsAbstract "the base is abstract"
                    Expect.isFalse d.IsSealed "an abstract base is not sealed"
                    Expect.equal d.Extends "System.Object" "the base extends Object"

                for case in [ "Shape+Dot"; "Shape+Line" ] do
                    match MetadataStructure.typeDecl bytes case with
                    | ValueNone -> failtestf "no %s TypeDef" case
                    | ValueSome d ->
                        Expect.isTrue d.IsNested (case + " is nested")
                        Expect.isTrue d.IsSealed (case + " is sealed")
                        Expect.isFalse d.IsAbstract (case + " is concrete")
                        Expect.equal d.Extends "Shape" (case + " extends the union")
                        Expect.equal d.Typars [] (case + " declares no typar")
            }

            test "a generic hierarchy union's case redeclares the union's typars" {
                let artifact =
                    compileSource
                        "GenericUnionCaseShape"
                        (String.concat
                            "\n"
                            [
                                "type Pair<'T> ="
                                "    | One of 'T"
                                "    | Two of 'T * 'T"
                                "let p = Two(1, 2)"
                                "printfn \"%b\" (p = p)"
                            ])

                let bytes = Codegen.toBytes artifact
                MetadataStructure.assertWellFormed "GenericUnionCaseShape" bytes

                match MetadataStructure.typeDecl bytes "Pair`1" with
                | ValueNone -> failtest "no Pair`1 TypeDef"
                | ValueSome d ->
                    Expect.isTrue d.IsAbstract "the base is abstract"
                    Expect.equal d.Typars [ "T" ] "the base declares its own typar"

                match MetadataStructure.typeDecl bytes "Pair`1+Two" with
                | ValueNone -> failtest "no Pair`1+Two TypeDef"
                | ValueSome d ->
                    Expect.isTrue d.IsNested "the case is nested"
                    Expect.isTrue d.IsSealed "the case is sealed"
                    // A nested type's own arity counts only the typars it ADDS, so the name
                    // carries no suffix even though the case declares one `GenericParam`.
                    Expect.equal d.Typars [ "T" ] "the case redeclares the union's typar"

                    Expect.equal d.Extends "<typespec>" "the case extends the base instantiated over its own typar"
            }

            // A generic type reaches its own members through `MemberRef`s on its open
            // self-`TypeSpec`, an append-only table: every pass that walks a case's payload
            // adds a row per field, and the byte-identity goldens miss a duplicate mint.
            test "a generic union's case payload is minted once per pass that walks it" {
                let artifact =
                    compileSource
                        "GenericUnionFieldRefRows"
                        (String.concat
                            "\n"
                            [
                                "type Pair<'T> ="
                                "    | One of 'T"
                                "    | Two of 'T * 'T"
                                "let p = Two(1, 2)"
                                "printfn \"%b\" (p = p)"
                                "printfn \"%A\" p"
                            ])

                let rows = MetadataStructure.memberRefRowCount (Codegen.toBytes artifact)

                // Two passes walk a case's fields: the `.ctor` / factory pass, and the
                // structural pass, whose equality, comparison and `%A` bodies share one walk.
                Expect.equal (rows "item1") 2 "Two's first field"
                Expect.equal (rows "item2") 2 "Two's second field"
                Expect.equal (rows "item") 2 "One's lone field"
            }

            // The flat regimes place every case's payload in slots co-resident on the
            // union, and their equality, comparison and `%A` bodies walk each case's
            // slots, so the same one-walk-per-pass rule covers `_tag` too.
            test "a generic struct union's co-resident payload is minted once per pass" {
                let artifact =
                    compileSource
                        "GenericStructUnionFieldRefRows"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type G<'T> ="
                                "    | Val of v: 'T"
                                "    | Num of n: int"
                                "let g: G<int> = Val 3"
                                "printfn \"%b\" (g = Val 3)"
                                "printfn \"%A\" g"
                            ])

                let rows = MetadataStructure.memberRefRowCount (Codegen.toBytes artifact)

                // `Payload_Val` and `Payload_Num` spell their wrapped field `_payload` too,
                // and their rows stay distinct from the union's: a row is per (declaring
                // type, member) rather than per name.
                Expect.equal (rows "_payload") 4 "the payload field, on the union and on each case view"
                Expect.equal (rows "_val0") 2 "Val's payload slot"
                Expect.equal (rows "_data") 2 "the overlay slot Num lands in"
                Expect.equal (rows "_tag") 2 "the discriminant"
            }

            // A single-case union's sole case is settled without a test, so the union
            // declares no `_tag` row and its payload takes FSC's spelling on the union
            // type itself.
            test "a single-case union carries no _tag and FSC-spells its payload" {
                let bytes =
                    compileSource
                        "SingleCaseUnionShape"
                        (String.concat
                            "\n"
                            [
                                "type Meters = M of float"
                                "let d = M 2.5"
                                "let v = match d with M x -> x"
                                "printfn \"%.1f\" v"
                                "printfn \"%b\" (d = M 2.5)"
                            ])
                    |> Codegen.toBytes

                MetadataStructure.assertWellFormed "SingleCaseUnionShape" bytes

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Meters")
                    [ "item" ]
                    "the lone positional payload, and no _tag"
            }

            // `C of tag: int` FSC-spells its payload `_tag`, the same name the
            // discriminant row would take; dropping the discriminant is what admits it
            // past Layout's field-name uniqueness check.
            test "a single-case union may declare a field named tag" {
                let bytes =
                    compileSource
                        "SingleCaseTagField"
                        (String.concat
                            "\n"
                            [
                                "type C = C of tag: int"
                                "let c = C 7"
                                "let t = match c with C tag -> tag"
                                "printfn \"%d\" t"
                            ])
                    |> Codegen.toBytes

                MetadataStructure.assertWellFormed "SingleCaseTagField" bytes

                Expect.equal (MetadataStructure.fieldsOf bytes "C") [ "_tag" ] "the payload owns the name outright"
            }

            // A `TypeTested` union settles a case by its runtime type, so its base declares
            // no discriminant and, with every case carrying a payload, no singleton: no
            // field rows at all.
            test "a type-tested union's base carries no field rows" {
                let bytes =
                    compileSource
                        "TypeTestedBaseFields"
                        (String.concat
                            "\n"
                            [
                                "type Shape ="
                                "    | Circle of r: int"
                                "    | Rect of w: int * h: int"
                                "let area (s: Shape) ="
                                "    match s with"
                                "    | Circle r -> r * r"
                                "    | Rect(w, h) -> w * h"
                                "printfn \"%d\" (area (Rect(2, 3)))"
                                "printfn \"%b\" (Circle 2 = Circle 2)"
                            ])
                    |> Codegen.toBytes

                MetadataStructure.assertWellFormed "TypeTestedBaseFields" bytes

                Expect.equal (MetadataStructure.fieldsOf bytes "Shape") [] "the base"
                Expect.equal (MetadataStructure.fieldsOf bytes "Shape+Circle") [ "_r" ] "the one-payload case"
                Expect.equal (MetadataStructure.fieldsOf bytes "Shape+Rect") [ "_w"; "_h" ] "the two-payload case"
            }

            // One case more, so the `isinst` chain gives way to `ldfld _tag` and the base
            // declares the row the type-tested one drops.
            test "a tagged union's base carries _tag" {
                let bytes =
                    compileSource
                        "TaggedBaseFields"
                        (String.concat
                            "\n"
                            [
                                "type Quad ="
                                "    | Q0 of int"
                                "    | Q1 of int"
                                "    | Q2 of int"
                                "    | Q3 of int"
                                "let v (q: Quad) ="
                                "    match q with"
                                "    | Q0 x -> x"
                                "    | Q1 x -> x + 1"
                                "    | Q2 x -> x + 2"
                                "    | Q3 x -> x + 3"
                                "printfn \"%d\" (v (Q2 5))"
                            ])
                    |> Codegen.toBytes

                MetadataStructure.assertWellFormed "TaggedBaseFields" bytes

                Expect.equal (MetadataStructure.fieldsOf bytes "Quad") [ "_tag" ] "the discriminant, and nothing else"
            }

            // A nullary case of a reference union is constructed once into `_unique_<Case>`,
            // in every regime. An enum-like union is nullary throughout, so it carries the
            // discriminant and a singleton per case.
            test "an enum-like union holds a singleton per case" {
                let bytes =
                    compileSource
                        "EnumLikeUnionSingletons"
                        (String.concat
                            "\n"
                            [
                                "type Colour ="
                                "    | Red"
                                "    | Green"
                                "    | Blue"
                                "let name (c: Colour) ="
                                "    match c with"
                                "    | Red -> \"red\""
                                "    | Green -> \"green\""
                                "    | Blue -> \"blue\""
                                "printfn \"%s\" (name Green)"
                            ])
                    |> Codegen.toBytes

                MetadataStructure.assertWellFormed "EnumLikeUnionSingletons" bytes

                Expect.equal
                    (MetadataStructure.fieldsOf bytes "Colour")
                    [ "_tag"; "_unique_Red"; "_unique_Green"; "_unique_Blue" ]
                    "the discriminant and one singleton per case"
            }

            // The same rule at the other end of the case count: one nullary case takes a
            // singleton and no discriminant.
            test "a single nullary case is a singleton and carries no _tag" {
                let bytes =
                    compileSource
                        "SingleNullaryCaseSingleton"
                        (String.concat "\n" [ "type Marker ="; "    | M"; "let m = M"; "printfn \"%b\" (m = M)" ])
                    |> Codegen.toBytes

                MetadataStructure.assertWellFormed "SingleNullaryCaseSingleton" bytes

                Expect.equal (MetadataStructure.fieldsOf bytes "Marker") [ "_unique_M" ] "the singleton alone"
            }

            // A union's storage is its own: `_tag` and each `_unique_<Case>` are private,
            // and the public surface is `get_Tag` plus the per-case factories.
            test "a union's fields are private behind get_Tag and the case factories" {
                let bytes =
                    compileSource
                        "UnionFieldVisibility"
                        (String.concat
                            "\n"
                            [
                                "type Colour ="
                                "    | Red"
                                "    | Green"
                                "    | Blue"
                                "let name (c: Colour) ="
                                "    match c with"
                                "    | Red -> \"red\""
                                "    | Green -> \"green\""
                                "    | Blue -> \"blue\""
                                "printfn \"%s\" (name Green)"
                            ])
                    |> Codegen.toBytes

                MetadataStructure.assertWellFormed "UnionFieldVisibility" bytes

                let access (a: FieldAttributes) = a &&& FieldAttributes.FieldAccessMask

                for (name, attrs) in MetadataStructure.fieldAttrsOf bytes "Colour" do
                    Expect.equal (access attrs) FieldAttributes.Private (name + " is private")
                    Expect.isTrue (attrs.HasFlag FieldAttributes.InitOnly) (name + " is initonly")

                let methods = MetadataStructure.methodAttrsOf bytes "Colour"

                let accessOf (name: string) =
                    match methods |> List.tryFind (fun (n, _) -> n = name) with
                    | Some(_, a) -> a &&& MethodAttributes.MemberAccessMask
                    | None -> failtestf "Colour declares no %s among %A" name (List.map fst methods)

                Expect.equal (accessOf "get_Tag") MethodAttributes.Public "get_Tag fronts the discriminant"

                for case in [ "Red"; "Green"; "Blue" ] do
                    Expect.equal (accessOf case) MethodAttributes.Public (case + " is the singleton's accessor")
            }

            // Each Vesper package is a library full of modules, unions, records and closures.
            for package in
                [
                    "Vesper.Core"
                    "Vesper.List"
                    "Vesper.Comparison"
                    "Vesper.Printf"
                    "Vesper.Set"
                ] do
                test (sprintf "the emitted %s package is well-formed" package) {
                    match ((buildPackage package).Value |> snd).OutputPath with
                    | Some path -> assertWellFormedFile package path
                    | None -> failwithf "buildPackage %s produced no OutputPath" package
                }

            // ---- Teeth ----------------------------------------------------------

            test "a counts-preserving field mis-order is caught" {
                // The rows are written b1, a1, a2 while the FieldList columns still say
                // A claims the first two and B the third. Structurally flawless.
                let bad = misorderedImage [ "b1"; "a1"; "a2" ] 1
                use provider = readImage bad
                let md = provider.GetMetadataReader()

                assertWellFormedMetadata "mis-ordered" md

                let expected =
                    [
                        {
                            Type = "N.A"
                            Fields = [ "a1"; "a2" ]
                            Methods = [ "aMethod" ]
                        }
                        {
                            Type = "N.B"
                            Fields = [ "b1" ]
                            Methods = [ "bMethod" ]
                        }
                    ]

                Expect.throws
                    (fun () -> assertTypeMembersMetadata "mis-ordered" md expected)
                    "a permuted Field table must be caught by the member assertion"

                // …and the same image with the rows in the right order passes.
                let good = misorderedImage [ "a1"; "a2"; "b1" ] 1
                use goodProvider = readImage good
                assertTypeMembersMetadata "ordered" (goodProvider.GetMetadataReader()) expected
            }

            test "a skewed prefix sum leaves a Field row unclaimed and is caught" {
                // Every FieldList column is off by one, so field row 1 belongs to no
                // type and the ranges no longer cover the table.
                let bad = misorderedImage [ "a1"; "a2"; "b1" ] 2
                use provider = readImage bad

                Expect.throws
                    (fun () -> assertWellFormedMetadata "skewed" (provider.GetMetadataReader()))
                    "a Field row claimed by no TypeDef must break the range partition"
            }
        ]
