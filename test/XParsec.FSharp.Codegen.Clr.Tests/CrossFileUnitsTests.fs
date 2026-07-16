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
// SCOPE NOTE — cross-file NOMINAL-TYPE use (the plan's "builds its record" surface) is NOT
// exercised here because it is blocked UPSTREAM, in the front end, independently of this
// codegen cut: the `FrozenSignature.toProvider` view a prior unit projects covers cross-file
// FUNCTION resolution, but a use site never consults the provider for a nominal's own SHAPE.
// Record construction (`InferRecordAccess.inferRecord`) and record field access
// (`resolveFieldStep`, the `TyRecord` arm) resolve only through the analysing unit's LOCAL
// `TypeRegistry`; union-case construction resolves through the local `CtorIndex`. So unit 2
// cannot build unit 1's record (`No record type matches the field set`), read its field
// (`Unknown record type`), or build its union case (`Unresolved identifier`). That front-end
// projection-coverage boundary must close before the record/union third of the proof can
// land; the codegen N-unit machinery it would feed is already in place and shared-registry
// resolved.

/// Compile a two-file assembly through the multi-unit front end + `compileUnits`, asserting
/// each unit analysed clean. Returns the emitted PE bytes.
let private compileTwoUnits (asmName: string) (unit1: string) (unit2: string) : byte[] =
    // The external surface (operators, `printfn`, the Vesper primitives) both the front end
    // resolves against and codegen threads through — the SAME provider the single-file
    // `compileSource` path uses, so the units resolve `+` / `printfn` identically.
    let external = ClrSymbolProviders.buildContract defaultManifests

    let results =
        analyseAssembly asmName external [ "unit1.fs", unit1; "unit2.fs", unit2 ]

    let units =
        results
        |> List.map (
            function
            | Ok u -> u
            | Error e -> failtestf "unit %s failed to parse: %A" e.Path e.Diagnostics
        )

    // Forward-only scoping is proven by unit 2 (which sees unit 1) analysing clean: its
    // references to unit 1's fn / generic resolve through unit 1's projected view.
    for u in units do
        let errs =
            u.Frozen.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

        Expect.isEmpty errs (sprintf "unit %s analysed with errors: %A" u.Path errs)

    // The composite codegen provider: each unit's projected view (so a cross-file call finds
    // the prior unit's exported open signature, which `emitExternalCall` re-homes to the local
    // `MethodDef`) ahead of the external stack (inline bodies + package symbols).
    let symbols =
        ExternalSymbolProviders.composite ([ for u in units -> u.View ] @ [ external ])

    let tasts = [ for u in units -> u.Frozen ]
    let project = withCore (ProjectInfo.defaults asmName)
    let artifact = Codegen.compileUnits symbols project tasts
    Codegen.toBytes artifact

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
        ]
