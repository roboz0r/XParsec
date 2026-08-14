module XParsec.FSharp.Codegen.Clr.Tests.CrossFileTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// A compilation is an ordered SEQUENCE of frozen files emitted into ONE assembly: two files
// compiled together and RUN. A cross-file reference that failed to re-home to a LOCAL
// definition would emit a self-`AssemblyRef`, so the loader faults and `peAssemblyRefs` sees it.

/// Compile a multi-file assembly through the production driver seam: each file analysed against
/// the composed prior views, `views ++ external` composed, ONE PE emitted. Returns its bytes.
let private compileFiles (asmName: string) (sources: AssemblyFiles.SourceFile list) : byte[] =
    // The external surface (operators, `printfn`, the Vesper primitives) that the front end
    // resolves against and codegen threads through.
    let external = ClrSymbolProviders.buildContract defaultPackages
    let project = withCore (ProjectInfo.defaults asmName)

    // Scoping is forward-only: each file sees the earlier ones through their projected views.
    // A parse or analysis error surfaces here, anchored to its own file.
    match ClrDriver.compileAssemblyWith [] external project sources with
    | Ok artifact -> Codegen.toBytes artifact
    | Error diags -> failtestf "cross-file compile failed: %A" diags

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
            // agreement is NOT observable by running a re-declaration: layout rejects the
            // second contribution to a module first, so the shadowed definition is
            // unreachable rather than merely unpreferred. Analysis pins the ordering itself
            // (`AssemblyFilesTests`); this pins the rejection that keeps it unobservable here.
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
                        "contributed by more than one file"
                        (sprintf "layout rejects the split module; got %A" m)
            }
        ]
