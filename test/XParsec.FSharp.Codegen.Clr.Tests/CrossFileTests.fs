module XParsec.FSharp.Codegen.Clr.Tests.CrossFileTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The first end-to-end proof that a compilation is an ordered SEQUENCE of frozen files
// emitted into ONE assembly: two source files compiled together and RUN. File 1 (a named
// module) exports a module function and a generic function; file 2 (the entry file, LAST)
// references both cross-file, then prints. The pipeline is the production shape:
// `AssemblyFiles.analyseAssembly` (per-file analyse against the composed prior views) →
// `composite(views ++ external)` → `Codegen.compileFiles`.
//
// The load-and-run is itself the sharpest assertion that the cross-file `add` call resolved
// to a LOCAL `MethodDef`: had it stayed an external `MemberRef` scoped by the compilation's
// own (home-stamped) assembly name, the loader would fault on a nonexistent `AssemblyRef`.
// `peAssemblyRefs` pins that structurally too — the own name is never in the ref table.
//
// SCOPE NOTE — cross-file record FIELD READ and CONSTRUCTION are both exercised below:
// `resolveFieldStep`'s `TyRecord` arm has a provider fallback (file 2 reads a field of
// file 1's record) and `recordFieldSetVerdict` has the same provider path (file 2 BUILDS
// file 1's record via a bare field-set literal). In both cases codegen re-homes the
// object argument's / literal's cross-file `recKey` to the LOCAL `TypeDef`, emitting `ldfld` /
// `newobj`. Union-case construction (the local `CtorIndex` — `Unresolved identifier`)
// remains blocked UPSTREAM; the codegen multi-file machinery it would feed is already in
// place and shared-registry resolved.

/// Compile a two-file assembly through the shared multi-file driver seam
/// (`ClrDriver.compileAssemblyWith`), which analyses each file against the composed prior
/// views, gates on front-end errors, composes `views ++ external`, and emits ONE PE.
/// Returns the emitted PE bytes.
let private compileTwoFiles (asmName: string) (file1: string) (file2: string) : byte[] =
    // The external surface (operators, `printfn`, the Vesper primitives) both the front end
    // resolves against and codegen threads through — the SAME provider the single-file
    // `compileSource` path uses, so the files resolve `+` / `printfn` identically.
    let external = ClrSymbolProviders.buildContract defaultManifests
    let project = withCore (ProjectInfo.defaults asmName)

    // Forward-only scoping is proven by file 2 (which sees file 1) analysing clean: its
    // references to file 1's fn / generic resolve through file 1's projected view. A parse
    // or analysis error surfaces here anchored to its own file.
    match
        ClrDriver.compileAssemblyWith Pipeline.analyseFor [] external project [ "file1.fs", file1; "file2.fs", file2 ]
    with
    | Ok artifact -> Codegen.toBytes artifact
    | Error diags -> failtestf "cross-file compile failed: %A" diags

[<Tests>]
let tests =
    testList
        "CrossFile (multi-file codegen)"
        [
            test "two files compile into one assembly and run: cross-file module fn + generic fn" {
                // File 1: a named module exporting a module function (`add`) and a generic
                // function (`identity`) — the two cross-file surfaces the front end resolves
                // through a prior file's projected view today.
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

                // The cross-file call resolved LOCALLY: the compilation never references
                // ITSELF as an external assembly (a wrong resolution would emit that ref, and
                // the loader would fault on it).
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

            // A TOP-LEVEL binding — one written outside any `module` — is exportable like any
            // other: it declares no module, but it is held by the file's namespace, so it has
            // a `SymbolKey` the freeze can publish and a consumer can resolve. This is the case
            // the publish boundary used to REFUSE ("no exportable identity … move it into a
            // module"), and refusing it took the whole template with it.
            //
            // Both halves are exercised at once, because both were blocked by the same
            // missing identity: file 2 CALLS file 1's top-level `addBase` directly, and it
            // EXPANDS file 1's top-level `let inline twice`, whose published body references
            // `addBase` — a reference the freeze can only bake in as a `SymbolKey`. Emission
            // homes both on the anonymous Program holder (the CLR has no namespace-level
            // method), which is exactly why the identity and the emission are separate facts:
            // the key says `addBase`, the metadata says which type it landed on.
            test "two files run: file 2 calls and EXPANDS file 1's top-level bindings" {
                // File 1 declares no module at all. Only a top-level FUNCTION may live in a
                // non-entry file — a top-level VALUE is entry-file-only top-level code
                // (`Layout.combine`) — so both bindings here are functions.
                let file1 =
                    "\
let addBase (x: int) : int = x + 10

let inline twice (x: int) : int = addBase (addBase x)
"

                // File 2 (entry, last): resolves both by their BARE names — a top-level
                // binding in a header-less file is keyed in the global namespace, so it
                // qualifies to exactly the name written here.
                let file2 = "printfn \"%d\" (twice 11 + addBase 1)\n"

                let asmName = "CrossFileTopLevel"
                let bytes = compileTwoFiles asmName file1 file2

                // As above: a cross-file call that failed to re-home to the local `MethodDef`
                // would emit a self-`AssemblyRef` and fault the loader.
                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                // The expanded template must land on file 1's `addBase`, not on nothing:
                // `twice 11` = addBase (addBase 11) = 31, plus `addBase 1` = 11 ⇒ 42.
                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "the expanded inline template and the direct call both resolved cross-file"

                // `addBase` emits under its SOURCE name on the Program holder — the name its
                // key qualifies to — which is what let file 2's reference find it.
                let names = programHolderMethods bytes |> Array.map (fun m -> m.Name)

                Expect.contains names "addBase" (sprintf "addBase emitted under its own name; got %A" names)
            }

            test "two files run: file 2 boxes a value into file 1's obj record field (cross-file box)" {
                // The field-init coercion (`unifyArg`) type-checks `{ V = 7 }` into file 1's
                // `V: obj` cross-file; codegen must then box it (the external `recordFieldTy`
                // arm), exactly as the local path does. A missing box is invalid IL that fails
                // to load, so a clean unbox round-trip proves the cross-file box fires.
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
                // End-to-end: file 1 declares a record and a factory returning it; file 2
                // reads `.X` off the factory result and prints it. The field read resolves
                // through file 1's projected provider view (no local `TypeRegistry` entry), and
                // codegen re-homes the object argument's cross-file `recKey` to the LOCAL `TypeDef` so
                // it emits a plain `ldfld` — THIS run is where any codegen `ldfld` gap surfaces.
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

                // The record field read re-homed to a LOCAL `ldfld`: the compilation never
                // references ITSELF as an external assembly (a wrong resolution — treating the
                // field as an external member ref — would emit that ref and the loader faults).
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
                // End-to-end: file 1 declares a record; file 2 BUILDS it with a bare
                // field-set literal `{ X = …; Y = … }`, reads a field, and prints it. The
                // construction resolves through file 1's projected provider view (no local
                // `TypeRegistry` entry — `recordFieldSetVerdict` unions the provider's
                // `TryRecordsWithField` candidates), and codegen re-homes the literal's
                // cross-file `recKey` to the LOCAL `TypeDef` so it emits a plain `newobj` —
                // THIS run is where any codegen `newobj` gap would surface.
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

                // The record construction re-homed to a LOCAL `newobj`: the compilation never
                // references ITSELF as an external assembly (a wrong resolution — treating the
                // ctor as an external member ref — would emit that ref and the loader faults).
                let refs = peAssemblyRefs bytes

                Expect.isFalse
                    (refs |> List.contains asmName)
                    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

                let exitCode, output = runEntryPoint bytes
                let actual = output.Replace("\r", "").Trim()

                Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
                Expect.equal actual "42" "cross-file record construction builds and reads the record"
            }

            // Cross-file INTERFACE dispatch, end to end: `FrozenSignature`'s `Interface` arm
            // decurries each abstract slot to an `ExternalMember` (`abstractMemberOf`), the
            // consumer resolves + dispatches it cross-file, AND codegen re-homes the interface's
            // own nominal to file 1's LOCAL `TypeDef` — so, like the record tests above, the
            // emitted PE carries NO self-`AssemblyRef`. (The recover-by-signature member-ref path
            // reached `externalClassRef` directly and used to parent on an `AssemblyRef` to our
            // own assembly; `externalMemberRef` now probes `userTypes` first, the member-ref
            // analogue of the `recKey` re-home records got.) Both forms below carry the full
            // self-ref guard its siblings use.

            test "two files run: file 2 calls an INTERFACE member declared in file 1 (decurried slot)" {
                // File 1 declares an interface, a class implementing it, and a factory returning
                // the interface; file 2 dispatches `GetVal` on the interface-typed result — a
                // object argument grounded to file 1's cross-file interface. A missing/wrong uncurry
                // surfaces as a front-end "no such member" miss or a bad `callvirt`, so a clean
                // run returning the value is the proof the slot resolved cross-file.
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

                // The interface dispatch resolved against file 1's LOCAL interface `TypeDef`, not
                // an external member ref scoped by the compilation's own assembly — the same
                // structural self-ref guard the record tests carry.
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
                // The typar-constrained dispatch form (`'T :> IGetVal`) — the external-interface
                // path `InferRecordAccess`'s coercion scan resolves via `TryLookupMember` on the
                // interface key. File 2's generic `callIt` calls `GetVal` off the `'T :> IGetVal`
                // bound, instantiated at the cross-file interface — the SAME decurried slot as the
                // direct form above, reached through the distinct typar-bound resolution seam.
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
        ]
