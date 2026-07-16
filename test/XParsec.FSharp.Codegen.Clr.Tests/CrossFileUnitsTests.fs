module XParsec.FSharp.Codegen.Clr.Tests.CrossFileUnitsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyUnits
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The first end-to-end proof that a compilation is an ordered SEQUENCE of frozen units
// emitted into ONE assembly: two source files compiled together and RUN. Unit 1 (a named
// module) exports a module function and a generic function; unit 2 (the entry file, LAST)
// references both cross-file, then prints. The pipeline is the production shape:
// `AssemblyUnits.analyseAssembly` (per-file analyse against the composed prior views) →
// `composite(views ++ external)` → `Codegen.compileUnits`.
//
// The load-and-run is itself the sharpest assertion that the cross-file `add` call resolved
// to a LOCAL `MethodDef`: had it stayed an external `MemberRef` scoped by the compilation's
// own (home-stamped) assembly name, the loader would fault on a nonexistent `AssemblyRef`.
// `peAssemblyRefs` pins that structurally too — the own name is never in the ref table.
//
// SCOPE NOTE — cross-file record FIELD READ and CONSTRUCTION are both now exercised (see the
// field-read and construction tests below): R3 gave `resolveFieldStep`'s `TyRecord` arm a
// provider fallback (unit 2 reads a field of unit 1's record), and R4b-2 gave
// `recordFieldSetVerdict` the same provider path (unit 2 BUILDS unit 1's record via a bare
// field-set literal). In both cases codegen re-homes the receiver's / literal's cross-file
// `recKey` to the LOCAL `TypeDef`, emitting `ldfld` / `newobj`. Union-case construction (the
// local `CtorIndex` — `Unresolved identifier`) remains blocked UPSTREAM; the codegen N-unit
// machinery it would feed is already in place and shared-registry resolved.

/// Compile a two-file assembly through the shared multi-file driver seam
/// (`ClrDriver.compileAssemblyWith`), which analyses each file against the composed prior
/// views, gates on front-end errors, composes `views ++ external`, and emits ONE PE.
/// Returns the emitted PE bytes.
let private compileTwoUnits (asmName: string) (unit1: string) (unit2: string) : byte[] =
    // The external surface (operators, `printfn`, the Vesper primitives) both the front end
    // resolves against and codegen threads through — the SAME provider the single-file
    // `compileSource` path uses, so the units resolve `+` / `printfn` identically.
    let external = ClrSymbolProviders.buildContract defaultManifests
    let project = withCore (ProjectInfo.defaults asmName)

    // Forward-only scoping is proven by unit 2 (which sees unit 1) analysing clean: its
    // references to unit 1's fn / generic resolve through unit 1's projected view. A parse
    // or analysis error surfaces here anchored to its own unit.
    match
        ClrDriver.compileAssemblyWith
            Pipeline.analyseFor
            []
            external
            project
            [ "unit1.fs", unit1; "unit2.fs", unit2 ]
    with
    | Ok artifact -> Codegen.toBytes artifact
    | Error diags -> failtestf "cross-file compile failed: %A" diags

[<Tests>]
let tests =
    testList
        "CrossFileUnits (multi-file codegen)"
        [
            test "two units compile into one assembly and run: cross-file module fn + generic fn" {
                // Unit 1: a named module exporting a module function (`add`) and a generic
                // function (`identity`) — the two cross-file surfaces the front end resolves
                // through a prior unit's projected view today.
                let unit1 =
                    "\
namespace CrossFile

module Lib =
    let add (a: int) (b: int) : int = a + b

    let identity (v: 'T) : 'T = v
"

                // Unit 2 (entry, last): calls unit 1's module fn and its generic fn, then prints.
                let unit2 =
                    "\
open CrossFile.Lib

let s = add 7 5
let e = identity s
printfn \"%d\" (s + e)
"

                let asmName = "CrossFileRun"
                let bytes = compileTwoUnits asmName unit1 unit2

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

            test "two units run: unit 2 reads a record FIELD declared in unit 1 (ldfld re-homes local)" {
                // R3 end-to-end: unit 1 declares a record and a factory returning it; unit 2
                // reads `.X` off the factory result and prints it. The field read resolves
                // through unit 1's projected provider view (no local `TypeRegistry` entry), and
                // codegen re-homes the receiver's cross-file `recKey` to the LOCAL `TypeDef` so
                // it emits a plain `ldfld` — THIS run is where any codegen `ldfld` gap surfaces.
                let unit1 =
                    "\
namespace CrossFile

module Lib =
    type R = { X: int }

    let make () : R = { X = 42 }
"

                // Unit 2 (entry, last): reads unit 1's record field cross-file, then prints.
                let unit2 =
                    "\
open CrossFile.Lib

let r = make ()
printfn \"%d\" r.X
"

                let asmName = "CrossFileFieldRead"
                let bytes = compileTwoUnits asmName unit1 unit2

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

            test "two units run: unit 2 CONSTRUCTS a record declared in unit 1 (newobj re-homes local)" {
                // R4b-2 end-to-end: unit 1 declares a record; unit 2 BUILDS it with a bare
                // field-set literal `{ X = …; Y = … }`, reads a field, and prints it. The
                // construction resolves through unit 1's projected provider view (no local
                // `TypeRegistry` entry — `recordFieldSetVerdict` unions the provider's
                // `TryRecordsWithField` candidates), and codegen re-homes the literal's
                // cross-file `recKey` to the LOCAL `TypeDef` so it emits a plain `newobj` —
                // THIS run is where any codegen `newobj` gap would surface.
                let unit1 =
                    "\
namespace CrossFile

module Lib =
    type R = { X: int; Y: int }
"

                // Unit 2 (entry, last): builds unit 1's record cross-file, reads a field, prints.
                let unit2 =
                    "\
open CrossFile.Lib

let r = { X = 20; Y = 22 }
printfn \"%d\" (r.X + r.Y)
"

                let asmName = "CrossFileRecordCons"
                let bytes = compileTwoUnits asmName unit1 unit2

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
        ]
