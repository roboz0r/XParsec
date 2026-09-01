module XParsec.FSharp.Codegen.Clr.Tests.CrossFileTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// A compilation is an ordered SEQUENCE of frozen files emitted into ONE assembly: two files
// compiled together and RUN. A cross-file reference that failed to re-home to a LOCAL
// definition would emit a self-`AssemblyRef`, so the loader faults and `peAssemblyRefs` sees it.

/// Compile a multi-file assembly through the production driver seam: each unit analysed against
/// the composed prior views, `views ++ external` composed, ONE PE emitted. Returns its bytes.
let private compileUnits (asmName: string) (units: AssemblyFiles.SourceUnit list) : byte[] =
    // The external surface (operators, `printfn`, the Vesper primitives) that the front end
    // resolves against and codegen threads through.
    let external = ClrSymbolProviders.buildContract defaultPackages
    let project = withCore (ProjectInfo.defaults asmName)

    // Scoping is forward-only: each file sees the earlier ones through their projected views.
    // A parse or analysis error surfaces here, anchored to its own file.
    match ClrDriver.compileWith [] external project (ClrDriver.sourcesFor project Set.empty units) with
    | Ok artifact -> Codegen.toBytes artifact
    | Error diags -> failtestf "cross-file compile failed:\n%s" (AnchoredDiagnostic.renderAll diags)

let private compileFiles (asmName: string) (sources: AssemblyFiles.SourceFile list) : byte[] =
    compileUnits asmName (sources |> List.map SourceUnit.ofImplementation)

let private compileTwoFiles (asmName: string) (file1: string) (file2: string) : byte[] =
    compileFiles asmName [ SourceFile.ofText "file1.fs" file1; SourceFile.ofText "file2.fs" file2 ]

[<Tests>]
let tests =
    testList
        "CrossFile (multi-file codegen)"
        [
            test "two files compile into one assembly and run: cross-file module fn + generic fn" {
                // File 1: a named module exporting a module fn (`add`) and a generic fn
                // (`identity`), two cross-file surfaces resolved through file 1's view.
                let file1 =
                    "\
namespace CrossFile

module Lib =
    let add (a: int) (b: int) : int = a + b

    let identity (v: 'T) : 'T = v
"

                // File 2 (entry, last): calls file 1's module fn and its generic fn, then prints.
                let file2 =
                    "\
open CrossFile.Lib

let s = add 7 5
let e = identity s
printfn \"%d\" (s + e)
"

                let asmName = "CrossFileRun"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                // s = add 7 5 = 12; e = identity 12 = 12; total = 24.
                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "24" "cross-file module fn + generic fn combine to 24"
            }

            // A unit with a `.fsi`: file 2 resolves what the SIGNATURE publishes, and the
            // `inline` template beside it still splices. Run, so the emitted PE is the proof.
            test "two files run: file 1 has a `.fsi`, and its inline template still splices" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    let hidden (x: int) : int = x * 1000

    let addBase (x: int) : int = x + 10

    let inline twice (x: int) : int = addBase (addBase x)
"

                // `hidden` is deliberately absent: what file 2 meets is this, not what the
                // implementation infers. It is still COMPILED — hiding is visibility, not
                // deletion — so the emitted assembly carries it either way.
                let file1Sig =
                    "\
namespace CrossFile

module Lib =
    val addBase: x: int -> int

    val inline twice: x: int -> int
"

                let file2 =
                    "\
open CrossFile.Lib

printfn \"%d\" (twice 11 + addBase 1)
"

                let asmName = "CrossFileSigned"

                let bytes =
                    compileUnits
                        asmName
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                            SourceUnit.ofImplementation (SourceFile.ofText "file2.fs" file2)
                        ]

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                // `twice 11` = addBase (addBase 11) = 31, plus `addBase 1` = 11 ⇒ 42.
                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the spliced template and the published call both ran"

                // Hidden from file 2, still EMITTED: a signature governs what later files
                // resolve, not what codegen lowers. Asked of the metadata, which throws when
                // the method is absent.
                Expect.isNonEmpty (peMethodIl bytes "CrossFile.Lib" "hidden") "the unpublished binding still compiled"
            }

            // A TOP-LEVEL binding (one outside any `module`) is held by the file's namespace,
            // so it has a `SymbolKey` the freeze can publish. Identity and emission stay
            // separate: the key says `addBase`, the metadata says the anonymous Program class.
            test "two files run: file 2 calls and EXPANDS file 1's top-level bindings" {
                // File 1 declares no module at all. Only a top-level FUNCTION may live in a
                // non-entry file, because a top-level VALUE is entry-file-only top-level code,
                // so both bindings here are functions.
                let file1 =
                    "\
let addBase (x: int) : int = x + 10

let inline twice (x: int) : int = addBase (addBase x)
"

                // File 2 (entry, last): resolves both by their BARE names, because a top-level
                // binding in a header-less file is keyed in the global namespace.
                let file2 = "printfn \"%d\" (twice 11 + addBase 1)\n"

                let asmName = "CrossFileTopLevel"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                // `twice 11` = addBase (addBase 11) = 31, plus `addBase 1` = 11 ⇒ 42.
                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the expanded inline template and the direct call both resolved cross-file"

                // `addBase` emits under its SOURCE name, the name its key qualifies to.
                let names = programClassMethods bytes |> Array.map (fun m -> m.Name)

                Expect.contains names "addBase" (sprintf "addBase emitted under its own name; got %A" names)
            }

            test "two files run: file 2 boxes a value into file 1's obj record field (cross-file box)" {
                // `{ V = 7 }` type-checks into file 1's `V: obj` cross-file; codegen must then
                // box it. A missing box is invalid IL that fails to load, so a clean unbox
                // round-trip proves the cross-file box fires.
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Box = { V: obj }
"

                let file2 =
                    "\
open CrossFile.Lib

let b = { V = 7 }
let n = b.V :?> int
printfn \"%d\" n
"

                let bytes = compileTwoFiles "CrossFileObjBox" file1 file2
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" (output.Replace("\r", "").Trim()))
                Expect.equal (output.Replace("\r", "").Trim()) "7" "the boxed int reads back cross-file"
            }

            test "two files run: file 2 boxes a value into file 1's obj union case field (cross-file box)" {
                // `Wrap 7` type-checks into file 1's `Wrap of obj` cross-file; codegen must
                // then box it. A missing box is invalid IL that fails to load, so a clean
                // unbox round-trip proves the cross-file box fires. Module-held, so the
                // bare case also exercises the cross-file `open`-scope resolution.
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Holder = Wrap of obj
"

                let file2 =
                    "\
open CrossFile.Lib

let w = Wrap 7

let n =
    match w with
    | Wrap v -> v :?> int

printfn \"%d\" n
"

                let bytes = compileTwoFiles "CrossFileObjUnionBox" file1 file2
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" (output.Replace("\r", "").Trim()))
                Expect.equal (output.Replace("\r", "").Trim()) "7" "the boxed int reads back cross-file"
            }

            test "two files run: file 2 reads a record FIELD declared in file 1 (ldfld re-homes local)" {
                // File 1 declares a record and a factory; file 2 reads `.X` off the result. The
                // read resolves through file 1's projected provider view (no local
                // `TypeRegistry` entry) and codegen re-homes its `recKey`, yielding a plain `ldfld`.
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type R = { X: int }

    let make () : R = { X = 42 }
"

                // File 2 (entry, last): reads file 1's record field cross-file, then prints.
                let file2 =
                    "\
open CrossFile.Lib

let r = make ()
printfn \"%d\" r.X
"

                let asmName = "CrossFileFieldRead"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "cross-file record field read prints the field value"
            }

            test "two files run: file 2 CONSTRUCTS a record declared in file 1 (newobj re-homes local)" {
                // File 2 BUILDS file 1's record with a bare field-set literal `{ X = …; Y = … }`.
                // The construction resolves through file 1's projected provider view (which
                // unions its `TryRecordsWithField` candidates), yielding a plain `newobj` after re-home.
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type R = { X: int; Y: int }
"

                // File 2 (entry, last): builds file 1's record cross-file, reads a field, prints.
                let file2 =
                    "\
open CrossFile.Lib

let r = { X = 20; Y = 22 }
printfn \"%d\" (r.X + r.Y)
"

                let asmName = "CrossFileRecordCons"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "cross-file record construction builds and reads the record"
            }

            // A module-held UNION reached across a file boundary, which is the `InModule`
            // containment an `ExternalUnionCase` keyed by compiled NAME would flatten: the case
            // resolves, but the union it types as is a different identity from the registered one.
            test "two files run: file 2 uses a UNION file 1 declared inside a module" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Shape =
        | Sq of int
        | Tri of int
"

                let file2 =
                    "\
open CrossFile.Lib

let s = Sq 42
printfn \"%d\" (match s with | Sq n -> n | Tri n -> n)
"

                let asmName = "CrossFileUnionInModule"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the case constructed and matched at the union's registered identity"
            }

            // Same-arity ctor overloads are separated by ARGUMENT TYPE, and the front end and
            // codegen must separate them the same way: an arity-only pick in either emits a
            // `newobj` of the wrong overload, which the verifier rejects.
            test "two files run: file 2 picks between file 1's SAME-ARITY ctor overloads" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Shape(x: int) =
        new(s: string) = Shape(s.Length)

        member this.Raw = x
"

                let file2 =
                    "\
open CrossFile.Lib

let a = Shape(31)
let b = Shape(\"abcdefghijk\")
printfn \"%d\" (a.Raw + b.Raw)
"

                let asmName = "CrossFileCtorOverload"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "31 from the int overload plus 11 from the string one"
            }

            test "two files run: file 2 declares the class and picks between its SAME-ARITY ctors" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    let seed (u: int) : int = u + 30
"

                let file2 =
                    "\
open CrossFile.Lib

type Shape(x: int) =
    new(s: string) = Shape(s.Length)

    member this.Raw = x

let a = Shape(seed 1)
let b = Shape(\"abcdefghijk\")
printfn \"%d\" (a.Raw + b.Raw)
"

                let bytes = compileTwoFiles "SameFileCtorOverload" file1 file2
                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the same-file pick agrees with the cross-file one"
            }

            // `inherit` reaches any of the base's constructors, not only the primary.
            test "two files run: file 2 INHERITS through a SECONDARY ctor of a class it declares" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    let seed (u: int) : int = u + 10
"

                let file2 =
                    "\
open CrossFile.Lib

type Base(x: int) =
    new(s: string) = Base(s.Length)

    member this.Raw = x

type Derived() =
    inherit Base(\"abcdefghijklmnopqrstuvwxyzabcde\")

printfn \"%d\" ((Derived()).Raw + seed 1)
"

                let bytes = compileTwoFiles "InheritSecondaryCtor" file1 file2
                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the base-ctor chain called the string overload"
            }

            // A prior file's class is published with its `.ctor` overloads, and the ctor-sugar
            // application resolves through the REGISTERED key, which keeps the `InModule`
            // containment a re-cut from `CrossFile.Lib.Shape` would flatten.
            test "two files run: file 2 CONSTRUCTS a class declared in file 1, primary and secondary" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Shape(x: int) =
        new() = Shape(11)

        member this.Raw = x
"

                let file2 =
                    "\
open CrossFile.Lib

let s = Shape(31)
let d = Shape()
printfn \"%d\" (s.Raw + d.Raw)
"

                let asmName = "CrossFileCtor"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "both ctor overloads resolved and the inherited member read back"
            }

            test "two files run: file 2 CONSTRUCTS a GENERIC class declared in file 1" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Box<'T>(v: 'T) =
        member this.Value = v
"

                let file2 =
                    "\
open CrossFile.Lib

let b = Box<int>(42)
printfn \"%d\" b.Value
"

                let asmName = "CrossFileGenericCtor"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the type argument pins the ctor parameter and the member read"
            }

            test "two files run: file 2 INHERITS a class declared in file 1 (extends re-homes local)" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Shape(x: int) =
        member this.Raw = x
"

                // File 2 (entry, last): derives from file 1's class, chains its ctor, and reads
                // the inherited member back.
                let file2 =
                    "\
open CrossFile.Lib

type Circle(r: int, t: int) =
    inherit Shape(t)

    member this.Radius = r

let c = Circle(11, 31)
printfn \"%d\" (c.Radius + c.Raw)
"

                let asmName = "CrossFileInherit"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the derived ctor chains to file 1's base and the inherited member reads back"
            }

            // Cross-file INTERFACE dispatch: the frozen `Interface` arm decurries each abstract
            // slot to an `ExternalMember`, and codegen re-homes the interface's own nominal to
            // file 1's LOCAL `TypeDef`, because `externalMemberRef` probes `userTypes` first.

            test "two files run: file 2 calls an INTERFACE member declared in file 1 (decurried slot)" {
                // File 1 declares an interface, an implementing class, and a factory returning
                // the interface; file 2 dispatches `GetVal` on the result. A missing or wrong
                // uncurry surfaces as a "no such member" miss or a bad `callvirt`.
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type IGetVal =
        abstract member GetVal: unit -> int

    type Holder(n: int) =
        interface IGetVal with
            member _.GetVal() = n

    let make (n: int) : IGetVal = Holder(n) :> IGetVal
"

                let file2 =
                    "\
open CrossFile.Lib

let g = make 21
printfn \"%d\" (g.GetVal())
"

                let asmName = "CrossFileInterfaceCall"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "21" "cross-file interface member dispatch returns the value"
            }

            test "two files run: file 2 calls a cross-file interface member through a typar bound" {
                // The typar-constrained form: file 2's generic `callIt` calls `GetVal` off the
                // `'T :> IGetVal` bound, instantiated at the cross-file interface, the SAME
                // decurried slot as above, reached through the typar-bound resolution seam.
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type IGetVal =
        abstract member GetVal: unit -> int

    type Holder(n: int) =
        interface IGetVal with
            member _.GetVal() = n

    let make (n: int) : IGetVal = Holder(n) :> IGetVal
"

                let file2 =
                    "\
open CrossFile.Lib

let callIt (x: 'T when 'T :> IGetVal) : int = x.GetVal()

let g = make 13
printfn \"%d\" (callIt g)
"

                let asmName = "CrossFileInterfaceTyparCall"
                let bytes = compileTwoFiles asmName file1 file2

                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "13" "cross-file typar-bound interface dispatch returns the value"
            }

            // Codegen composes the per-file views nearest-first, matching analysis. That
            // agreement is NOT observable by running a re-declaration: analysis refuses the
            // second file's declaration of the path (FS0248), so the shadowed definition is
            // unreachable rather than merely unpreferred. Analysis pins the ordering itself
            // (`AssemblyFilesTests`); this pins the refusal that keeps it unobservable here.
            test "a module contributed by two files is REJECTED before emission" {
                let file1 =
                    "\
namespace CrossFile

module Shared =
    let dup () : int = 1
"

                let file2 =
                    "\
namespace CrossFile

module Shared =
    let dup () : int = 2
"

                let file3 =
                    "\
open CrossFile

printfn \"%d\" (Shared.dup ())
"

                let compile () =
                    compileFiles
                        "CrossFileRedeclare"
                        [
                            SourceFile.ofText "file1.fs" file1
                            SourceFile.ofText "file2.fs" file2
                            SourceFile.ofText "file3.fs" file3
                        ]
                    |> ignore

                let failure =
                    try
                        compile ()
                        None
                    with e ->
                        Some e.Message

                match failure with
                | None -> failtest "a module split across two files must not reach emission"
                | Some m ->
                    Expect.stringContains
                        m
                        "Two modules named 'CrossFile.Shared' occur in two parts of this assembly"
                        (sprintf "analysis refuses the split module; got %A" m)
            }

            // A prior file's module VALUE. The front end resolves and types it; codegen has a
            // local re-home for a module FUNCTION only (`ClrRecipes.emitExternalCall` through
            // `env.LocalModuleFns`), so the read falls to a `MemberRef` scoped by the value's
            // own-assembly home: a self-`AssemblyRef`, and a method ref to a static field.
            test "a prior file's module VALUE reads through a local field, not a self-AssemblyRef" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    let v : int = 5
"

                let file2 =
                    "\
open CrossFile.Lib

printfn \"%d\" (v + 1 + CrossFile.Lib.v)
"

                let asmName = "CrossFileValue"
                let bytes = compileTwoFiles asmName file1 file2
                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" output)
                Expect.equal (output.Replace("\r", "").Trim()) "11" "5 + 1 + 5"
            }

            // A case qualified by its MODULE in expression position. The front end mints a
            // free TyVar for it (see `LongIdentResolutionTests`), and Elaborate freezes the
            // name as `TExpr.External` with no key, so emission finds no recipe.
            test "a module-qualified union case constructs across a file boundary" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Color =
        | Red
        | Green of int
"

                let file2 =
                    "\
open CrossFile.Lib

let a (c: Color) =
    match c with
    | Red -> 0
    | Green n -> n

printfn \"%d\" (a (CrossFile.Lib.Green 3) + a CrossFile.Lib.Red)
"

                let asmName = "CrossFileModuleCase"
                let bytes = compileTwoFiles asmName file1 file2
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" output)
                Expect.equal (output.Replace("\r", "").Trim()) "3" "Green 3 + Red"
            }

            // The type-qualified spelling of the same case is the control: it runs today.
            test "a type-qualified union case constructs and matches across a file boundary" {
                let file1 =
                    "\
namespace CrossFile

module Lib =
    type Color =
        | Red
        | Green of int
"

                let file2 =
                    "\
open CrossFile.Lib

let a (c: Color) =
    match c with
    | Color.Red -> 0
    | Color.Green n -> n

printfn \"%d\" (a (Color.Green 3) + a Color.Red)
"

                let asmName = "CrossFileTypeCase"
                let bytes = compileTwoFiles asmName file1 file2
                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" output)
                Expect.equal (output.Replace("\r", "").Trim()) "3" "Green 3 + Red"
            }
        ]
