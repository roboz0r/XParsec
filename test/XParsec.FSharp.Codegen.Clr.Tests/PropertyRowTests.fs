module XParsec.FSharp.Codegen.Clr.Tests.PropertyRowTests

open System
open System.IO
open System.Reflection
open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

// `Property` / `PropertyMap` / `MethodSemantics` rows: a member declared as a property
// emits the row a reflecting consumer binds it through, rather than a bare `get_`-named
// method beside it.

let private bytesOf (name: string) (lines: string list) : byte[] =
    Codegen.toBytes (compileSource name (String.concat "\n" lines))

/// The `MethodAttributes` of one type's method rows by name.
let private methodAttrsOn (bytes: byte[]) (typeName: string) : Map<string, MethodAttributes> =
    let asm = loadAssembly bytes
    let ty = asm.GetType(typeName, throwOnError = true)

    ty.GetMethods(
        BindingFlags.Public
        ||| BindingFlags.Instance
        ||| BindingFlags.Static
        ||| BindingFlags.DeclaredOnly
    )
    |> Array.map (fun m -> m.Name, m.Attributes)
    |> Map.ofArray

[<Tests>]
let tests =
    testList
        "Property rows"
        [
            test "a parameterless property emits one Property row bound to its getter" {
                let bytes =
                    bytesOf "PropGet" [ "type C(v: int) ="; "    member this.P = v"; "let c = C(1)" ]

                Expect.equal
                    (propertiesOf bytes "C")
                    [ "P", (ValueSome "get_P", ValueNone) ]
                    "the row is named for the property, not the accessor"
            }

            test "`with get, set` yields ONE row carrying both halves" {
                let bytes =
                    bytesOf
                        "PropGetSet"
                        [
                            "type C(v: int) ="
                            "    let mutable q = v"
                            "    member this.Q with get () = q and set (w: int) = q <- w"
                            "let c = C(1)"
                        ]

                Expect.equal
                    (propertiesOf bytes "C")
                    [ "Q", (ValueSome "get_Q", ValueSome "set_Q") ]
                    "both accessors bind to the one property they were declared as halves of"
            }

            test "a write-only property emits a row carrying the setter alone" {
                let bytes =
                    bytesOf
                        "PropSetOnly"
                        [
                            "type C(v: int) ="
                            "    let mutable q = v"
                            "    member this.Q with set (w: int) = q <- w"
                            "let c = C(1)"
                        ]

                Expect.equal
                    (propertiesOf bytes "C")
                    [ "Q", (ValueNone, ValueSome "set_Q") ]
                    "a property declaring no getter still gets its row"
            }

            test "an indexed property emits a row whose accessors are the indexed pair" {
                let bytes =
                    bytesOf
                        "PropIndexed"
                        [
                            "type C(v: int) ="
                            "    let mutable slot = v"
                            "    member this.Item with get (i: int) = slot + i and set (i: int) (w: int) = slot <- w + i"
                            "let c = C(1)"
                        ]

                Expect.equal
                    (propertiesOf bytes "C")
                    [ "Item", (ValueSome "get_Item", ValueSome "set_Item") ]
                    "an index does not split the property into two"
            }

            test "a static property emits its row too" {
                let bytes =
                    bytesOf "PropStatic" [ "type C() ="; "    static member P with get () = 5"; "let c = C()" ]

                Expect.equal (propertiesOf bytes "C") [ "P", (ValueSome "get_P", ValueNone) ] "static is not a bar"
            }

            // The halves group by (name, staticness), so a static and an instance property
            // sharing a name are two rows rather than a silent merge.
            test "a static and an instance property of one name are two rows" {
                let bytes =
                    bytesOf
                        "PropStaticInstance"
                        [
                            "type C(v: int) ="
                            "    member this.P = v"
                            "    static member P = 3"
                            "let c = C(1)"
                        ]

                Expect.equal
                    (propertiesOf bytes "C")
                    [ "P", (ValueSome "get_P", ValueNone); "P", (ValueSome "get_P", ValueNone) ]
                    "one instance row and one static row"
            }

            test "a union's discriminant is reachable as a Tag property" {
                let bytes =
                    bytesOf
                        "PropUnionTag"
                        [
                            "type U ="
                            "    | A of int"
                            "    | B of string"
                            "    | C"
                            "    | D"
                            "let u = A 1"
                        ]

                Expect.equal
                    (propertiesOf bytes "U")
                    [ "Tag", (ValueSome "get_Tag", ValueNone) ]
                    "the public accessor for the private `_tag` is a property"
            }

            test "a type-tested union declares no Tag property, having no tag" {
                let bytes =
                    bytesOf "PropNoTag" [ "type U ="; "    | A of int"; "    | B of string"; "let u = A 1" ]

                Expect.isEmpty (propertiesOf bytes "U") "no discriminant field means no Tag property"
            }

            test "an interface's abstract property slots emit Property rows" {
                let bytes =
                    bytesOf
                        "PropIface"
                        [
                            "type I ="
                            "    abstract P: int with get, set"
                            "    abstract R: int"
                            "    abstract M: int -> int"
                            "type C() ="
                            "    let mutable q = 0"
                            "    interface I with"
                            "        member this.P with get () = q and set (w: int) = q <- w"
                            "        member this.R = 1"
                            "        member this.M(x: int) = x"
                            "let c = C()"
                        ]

                Expect.equal
                    (propertiesOf bytes "I")
                    [
                        "P", (ValueSome "get_P", ValueSome "set_P")
                        "R", (ValueSome "get_R", ValueNone)
                    ]
                    "an abstract slot pair is one property; the abstract method is none"
            }

            test "each record field emits a Property row over its backing storage" {
                let bytes =
                    bytesOf
                        "PropRecordFields"
                        [ "type R = { X: int; mutable Y: string }"; "let r = { X = 1; Y = \"a\" }" ]

                Expect.equal
                    (propertiesOf bytes "R")
                    [
                        "X", (ValueSome "get_X", ValueNone)
                        "Y", (ValueSome "get_Y", ValueSome "set_Y")
                    ]
                    "an immutable field is get-only; a `mutable` one carries both halves"
            }

            // The property is the field's whole public surface, and the storage behind it
            // carries FSC's `@`-suffixed name. An immutable field is written by the `.ctor`
            // alone — a literal is a `newobj` and `{ r with X = v }` rebuilds through the same
            // ctor — so it takes `initonly` too.
            test "a record field's storage is private, and initonly unless the field is `mutable`" {
                let bytes =
                    bytesOf
                        "PropRecordFieldAttrs"
                        [
                            "type R = { X: int; mutable Y: string }"
                            "[<Struct>]"
                            "type S = { A: int; mutable B: int }"
                            "let r = { X = 1; Y = \"a\" }"
                            "let s = { A = 1; B = 2 }"
                        ]

                let immutable (name: string) =
                    name, FieldAttributes.Private ||| FieldAttributes.InitOnly

                let mutable' (name: string) = name, FieldAttributes.Private

                Expect.equal
                    (fieldAttrsOf bytes "R")
                    [ immutable "X@"; mutable' "Y@" ]
                    "an immutable field's storage keeps initonly; a `mutable` one drops it"

                Expect.equal
                    (fieldAttrsOf bytes "S")
                    [ immutable "A@"; mutable' "B@" ]
                    "a struct record's storage takes the same bits"
            }

            // The accessors reach the storage the record's own `.ctor` filled, so the pair
            // round-trips a value no literal wrote.
            test "reflection binds a record field's accessors and they reach the storage" {
                let bytes =
                    bytesOf "PropRecordReflect" [ "type R = { X: int; mutable Y: int }"; "let r = { X = 1; Y = 2 }" ]

                let asm = loadAssembly bytes
                let ty = asm.GetType("R", throwOnError = true)
                let x = ty.GetProperty "X"
                let y = ty.GetProperty "Y"

                Expect.isTrue x.CanRead "an immutable field reads"
                Expect.isFalse x.CanWrite "an immutable field does not write"
                Expect.isTrue y.CanWrite "a `mutable` field writes"

                let instance = Activator.CreateInstance(ty, [| box 7; box 8 |])
                Expect.equal (x.GetValue instance :?> int) 7 "the getter reads what the ctor stored"

                y.SetValue(instance, box 21)
                Expect.equal (y.GetValue instance :?> int) 21 "the setter writes the field the getter reads"
            }

            // A raw `FieldDefinition` token resolves to the wrong slot for a field at index
            // >= 1 of a generic type, so the SECOND field is the one that pins the accessor's
            // `MemberRef` on the open self-`TypeSpec`.
            test "a generic record's accessors read the right slot" {
                let bytes =
                    bytesOf
                        "PropGenericRecordReflect"
                        [
                            "type Box<'T> = { First: 'T; Second: 'T }"
                            "let b = { First = 1; Second = 2 }"
                        ]

                let asm = loadAssembly bytes

                let ty =
                    (asm.GetType("Box`1", throwOnError = true)).MakeGenericType [| typeof<int> |]

                let instance = Activator.CreateInstance(ty, [| box 7; box 8 |])

                Expect.equal ((ty.GetProperty "First").GetValue instance :?> int) 7 "the first field's getter"
                Expect.equal ((ty.GetProperty "Second").GetValue instance :?> int) 8 "the second field's getter"
            }

            test "a struct record's field getter reads through the byref this" {
                let bytes =
                    bytesOf
                        "PropStructRecordReflect"
                        [
                            "[<Struct>]"
                            "type P = { X: int; mutable Y: int }"
                            "let p = { X = 1; Y = 2 }"
                        ]

                let asm = loadAssembly bytes
                let ty = asm.GetType("P", throwOnError = true)
                Expect.isTrue ty.IsValueType "P is a struct record"

                let instance = Activator.CreateInstance(ty, [| box 7; box 8 |])
                Expect.equal ((ty.GetProperty "X").GetValue instance :?> int) 7 "the getter reads through `ldarg.0`"
            }

            test "a property with no accessors declared is no property row" {
                let bytes =
                    bytesOf "PropNone" [ "type C(v: int) ="; "    member this.M() = v"; "let c = C(1)" ]

                Expect.isEmpty (propertiesOf bytes "C") "a method is not a property"
            }

            test "accessor method rows carry SpecialName; a plain method does not" {
                let bytes =
                    bytesOf
                        "PropSpecialName"
                        [
                            "type C(v: int) ="
                            "    let mutable q = v"
                            "    member this.P = v"
                            "    member this.Q with get () = q and set (w: int) = q <- w"
                            "    member this.M() = v"
                            "let c = C(1)"
                        ]

                let attrs = methodAttrsOn bytes "C"

                let hasSpecialName n =
                    attrs.[n].HasFlag MethodAttributes.SpecialName

                Expect.isTrue (hasSpecialName "get_P") "a parameterless getter is an accessor"
                Expect.isTrue (hasSpecialName "get_Q") "a declared getter is an accessor"
                Expect.isTrue (hasSpecialName "set_Q") "a declared setter is an accessor"
                Expect.isFalse (hasSpecialName "M") "a plain method is not"
            }

            // The rows are what a runtime consumer binds through, so assert the binding and
            // not only the metadata.
            test "reflection binds the emitted rows as a readable, writable property" {
                let bytes =
                    bytesOf
                        "PropReflect"
                        [
                            "type C(v: int) ="
                            "    let mutable q = v"
                            "    member this.Q with get () = q and set (w: int) = q <- w"
                            "let c = C(1)"
                        ]

                let asm = loadAssembly bytes
                let ty = asm.GetType("C", throwOnError = true)
                let p = ty.GetProperty "Q"

                Expect.isNotNull p "reflection finds the property"
                Expect.isTrue p.CanRead "the getter is bound"
                Expect.isTrue p.CanWrite "the setter is bound"
                Expect.equal p.PropertyType typeof<int> "the row's signature carries the value type"

                Expect.equal p.SetMethod.ReturnType typeof<Void> "a setter returns void"

                let instance = Activator.CreateInstance(ty, [| box 7 |])
                Expect.equal (p.GetValue instance :?> int) 7 "reading goes through the getter"

                p.SetValue(instance, box 21)
                Expect.equal (p.GetValue instance :?> int) 21 "writing goes through the setter"
            }

            test "an indexed property reflects with its index parameter" {
                let bytes =
                    bytesOf
                        "PropReflectIndexed"
                        [
                            "type C(v: int) ="
                            "    member this.Item with get (i: int) = i + v"
                            "let c = C(1)"
                        ]

                let asm = loadAssembly bytes
                let ty = asm.GetType("C", throwOnError = true)
                let p = ty.GetProperty "Item"

                Expect.isNotNull p "reflection finds the indexed property"

                Expect.equal
                    (p.GetIndexParameters() |> Array.map (fun ip -> ip.ParameterType))
                    [| typeof<int> |]
                    "the index parameter is on the property signature, not only the accessor"

                let instance = Activator.CreateInstance(ty, [| box 10 |])
                Expect.equal (p.GetValue(instance, [| box 5 |]) :?> int) 15 "the indexed getter runs"
            }

            // A record field's storage is reached directly only by the record's own bodies.
            // Every use site elsewhere binds the accessor pair.
            test "a record field read and write outside the record call the accessors" {
                let bytes =
                    bytesOf
                        "RecordAccessorConsumers"
                        [
                            "type R = { X: int; mutable Y: int }"
                            "let getX (r: R) = r.X"
                            "let setY (r: R) (v: int) = r.Y <- v"
                            "let r = { X = 1; Y = 2 }"
                        ]

                Expect.equal (peMethodMemberOps bytes "Program" "getX") [ "call", "get_X" ] "`r.X` calls the getter"

                Expect.equal
                    (peMethodMemberOps bytes "Program" "setY")
                    [ "call", "set_Y" ]
                    "`r.Y <- v` calls the setter"
            }

            test "a record clone and a record pattern read the source through the getters" {
                let bytes =
                    bytesOf
                        "RecordAccessorCloneAndPattern"
                        [
                            "type R = { X: int; Y: int }"
                            "let withY (r: R) = { r with Y = 0 }"
                            "let sum (r: R) ="
                            "    match r with"
                            "    | { X = x; Y = y } -> x + y"
                            "let r = { X = 1; Y = 2 }"
                        ]

                Expect.equal
                    (peMethodMemberOps bytes "Program" "withY")
                    [ "call", "get_X"; "newobj", ".ctor" ]
                    "the clone reads the un-overridden field through its getter"

                // The trailing `newobj` is the match's fallthrough exception.
                Expect.equal
                    (peMethodMemberOps bytes "Program" "sum")
                    [ "call", "get_X"; "call", "get_Y"; "newobj", ".ctor" ]
                    "the pattern reads each named field through its getter"
            }

            test "a generic record's consumer calls the getter through a MemberRef on the instantiation" {
                let bytes =
                    bytesOf
                        "RecordAccessorGenericConsumer"
                        [
                            "type Box<'T> = { First: 'T; mutable Second: 'T }"
                            "let second (b: Box<int>) = b.Second"
                            "let b = { First = 1; Second = 2 }"
                        ]

                Expect.equal
                    (peMethodMemberOps bytes "Program" "second")
                    [ "call", "get_Second" ]
                    "the second field of a generic record binds its getter"

                Expect.equal (memberRefRowCount bytes "get_Second") 1 "one MemberRef row on `Box<int>`"
                Expect.equal (memberRefRowCount bytes "set_Second") 0 "a read mints no setter MemberRef"
            }

            test "a struct record's consumer calls the getter on the value's address" {
                let bytes =
                    bytesOf
                        "RecordAccessorStructConsumer"
                        [
                            "[<Struct>]"
                            "type P = { X: int; mutable Y: int }"
                            "let getX (p: P) = p.X"
                            "let withX (p: P) = { p with X = 9 }"
                            "let p = { X = 1; Y = 2 }"
                        ]

                Expect.equal (peMethodMemberOps bytes "Program" "getX") [ "call", "get_X" ] "`p.X` calls the getter"

                Expect.equal
                    (peMethodMemberOps bytes "Program" "withX")
                    [ "call", "get_Y"; "newobj", ".ctor" ]
                    "the clone reads the copied field through its getter"

                let asm = loadAssembly bytes
                let program = asm.GetType("Program", throwOnError = true)
                let ty = asm.GetType("P", throwOnError = true)
                let instance = Activator.CreateInstance(ty, [| box 7; box 8 |])
                Expect.equal (program.GetMethod("getX").Invoke(null, [| instance |]) :?> int) 7 "the getter reads X"

                let cloned = program.GetMethod("withX").Invoke(null, [| instance |])
                Expect.equal ((ty.GetProperty "X").GetValue cloned :?> int) 9 "the clone overrides X"
                Expect.equal ((ty.GetProperty "Y").GetValue cloned :?> int) 8 "the clone copies Y"
            }

            // `Vesper.Ref<'T>` is a record in a referenced package, so a promoted `let
            // mutable` reaches `contents` through the imported accessor `MemberRef`s.
            test "a referenced-package record field binds the imported accessors" {
                let bytes =
                    bytesOf
                        "RecordAccessorExternalConsumer"
                        [
                            "let useCounter (z: int) : int ="
                            "    let mutable n = z"
                            "    let bump (v: int) ="
                            "        n <- n + v"
                            "    bump 7"
                            "    n"
                        ]

                let ops = peMethodMemberOps bytes "Program" "useCounter"
                Expect.contains ops ("call", "get_contents") "the cell is read through `get_contents`"
                Expect.isFalse (List.contains ("ldfld", "contents") ops) "the cell's field is not read directly"
            }

            // A compilation referencing the emitted DLL resolves the member as a property.
            // Without the rows the reader sees only a `get_Value` method, and `x.Value`
            // does not resolve.
            test "a property round-trips through emit and MetadataSymbols" {
                let outDir = tmpDir "prop-roundtrip"
                let outPath = Path.Combine(outDir, "PropRoundTrip.dll")

                let project =
                    { ProjectInfo.library "PropRoundTrip" with
                        OutputPath = Some outPath
                    }

                let source =
                    String.concat
                        "\n"
                        [
                            "namespace PropProbe"
                            ""
                            "type Holder(v: int) ="
                            "    member this.Value = v"
                        ]

                let artifact = compileSourceTo project source
                Codegen.materialise artifact

                let provider =
                    MetadataSymbols.create (outPath :: vesperCoreDll.Value :: MetadataSymbols.runtimeAssemblyPaths ())

                match ExternalSymbols.tryMetaType provider "PropProbe.Holder" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let named = info.Members |> Block.toList |> List.filter (fun m -> m.Name = "Value")

                    match named with
                    | [ m ] -> Expect.equal m.Storage MemberStorage.Property "the reader reports it as a property"
                    | ms -> failtestf "expected exactly one member named 'Value', got %d" (List.length ms)
                | other -> failtestf "expected PropProbe.Holder as a Class shape, got %A" other
            }
        ]
