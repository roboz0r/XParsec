module XParsec.FSharp.SemanticAnalysis.Tests.AssemblyUnitsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyUnits
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Step B1 of multi-file compilation units: the FRONT-END multi-file assembly pipeline.
// NO codegen here. It proves cross-file NAME RESOLUTION — file N+1 resolves file N's
// symbols by name through file N's projected provider view — plus forward-only scoping,
// nearest-first shadowing, and per-unit diagnostic anchoring. Each file owns its own
// Input/Lexed, so `NodeKey` offsets are per-file and never collide across files.

let private asm = "MultiFileAsm"

/// The `Ok` units of an assembly run, or a test failure naming the first parse error.
let private units (results: Result<FrozenUnit, UnitError> list) : FrozenUnit list =
    results
    |> List.map (
        function
        | Ok u -> u
        | Error e -> failtestf "unit %s failed to parse: %A" e.Path e.Diagnostics
    )

/// A unit's unresolved-symbol error diagnostics (the front end phrases both the bare and
/// the qualified miss with an "Unresolved" message).
let private unresolvedErrors (u: FrozenUnit) : Diagnostic list =
    u.Frozen.Diagnostics
    |> List.filter (fun d -> d.Severity = Severity.Error && d.Message.Contains "Unresolved")

// --- shared file-1 export surface: a type T and a saturated function f -----------------

let private file1Qualified =
    "\
namespace Test.A

module M =
    type T = { value: int }

    let f (x: int) : int = x + 1
"

[<Tests>]
let tests =
    testList
        "AssemblyUnits (multi-file front end)"
        [
            test "file 2 resolves file 1's type + saturated function by QUALIFIED name" {
                // file 2 references file 1 fully qualified: `Test.A.M.T` / `Test.A.M.f`.
                let file2 =
                    "\
namespace Test.B

module N =
    let useT (t: Test.A.M.T) : int = t.value

    let useF () : int = Test.A.M.f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1Qualified; "file2.fs", file2 ]
                    |> units

                Expect.hasLength all 2 "both files analysed"
                let f2 = all.[1]

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf "file 2 has no unresolved-symbol errors (diagnostics: %A)" f2.Frozen.Diagnostics)
            }

            test "file 2 resolves file 1's exports through an OPEN-ed bare reference" {
                // file 2 `open`s file 1's module and references `T` / `f` bare.
                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let useT (t: T) : int = t.value

    let useF () : int = f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1Qualified; "file2.fs", file2 ]
                    |> units

                let f2 = all.[1]

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf "opened bare reference resolves through the view (diagnostics: %A)" f2.Frozen.Diagnostics)
            }

            test "FORWARD-only: file 1 cannot resolve a symbol defined only in file 2" {
                // file 1 (analysed FIRST) references `Test.B.beta`, which only file 2
                // declares — a later file's export is invisible to an earlier file.
                let file1 =
                    "\
namespace Test.A

module A =
    let usesB () : int = Test.B.beta 1
"

                let file2 =
                    "\
namespace Test.B

module B =
    let beta (x: int) : int = x
"

                let all =
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
                    |> units

                let f1 = all.[0]
                let f2 = all.[1]

                Expect.isNonEmpty (unresolvedErrors f1) "file 1's forward reference to file 2 is unresolved"
                // file 2 references nothing forward; it is clean.
                Expect.isEmpty (unresolvedErrors f2) "file 2 (which sees file 1) is clean"
            }

            test "NEAREST file wins a name clash (compose ordering)" {
                // Two files declare the SAME qualified binding `Test.Shared.dup` with
                // DIFFERENT schemes (int vs string). The compose puts the nearer file
                // first, so its symbol shadows the farther one's.
                let earlier =
                    "\
namespace Test

module Shared =
    let dup = 1
"

                let later =
                    "\
namespace Test

module Shared =
    let dup = \"hello\"
"

                let all =
                    analyseAssembly asm realProvider.Value [ "earlier.fs", earlier; "later.fs", later ]
                    |> units

                let viewEarlier = all.[0].View
                let viewLater = all.[1].View

                let name = "Test.Shared.dup"

                let symEarlier =
                    match (viewEarlier :> IExternalSymbolResolver).TryLookup name with
                    | ValueSome s -> s
                    | ValueNone -> failtest "earlier file did not export dup"

                let symLater =
                    match (viewLater :> IExternalSymbolResolver).TryLookup name with
                    | ValueSome s -> s
                    | ValueNone -> failtest "later file did not export dup"

                // Guard: the two views really disagree, so the shadowing test is meaningful.
                Expect.notEqual symEarlier.Scheme symLater.Scheme "the two files' dup schemes differ (int vs string)"

                // Nearest-first: `later` shadows `earlier`.
                let composedNearestLater =
                    ExternalSymbolProviders.composite [ viewLater; viewEarlier ] :> IExternalSymbolResolver

                match composedNearestLater.TryLookup name with
                | ValueSome s -> Expect.equal s.Scheme symLater.Scheme "nearest (later) file's dup wins"
                | ValueNone -> failtest "composed provider did not resolve dup"

                // And the reverse ordering proves it is ORDER, not identity: nearest=earlier wins.
                let composedNearestEarlier =
                    ExternalSymbolProviders.composite [ viewEarlier; viewLater ] :> IExternalSymbolResolver

                match composedNearestEarlier.TryLookup name with
                | ValueSome s ->
                    Expect.equal s.Scheme symEarlier.Scheme "nearest (earlier) file's dup wins when it is first"
                | ValueNone -> failtest "composed provider did not resolve dup"
            }

            test "diagnostics anchor to their OWN unit's path + (line, col)" {
                // file 1 is clean; file 2 has an undefined name on line 4. The anchored
                // diagnostic must carry file 2's PATH and a (line, col) from file 2's text
                // — not file 1's.
                let file1 =
                    "\
namespace Test

module A =
    let a = 1
"

                let file2 =
                    "\
namespace Test

module B =
    let b = undefinedThing
"

                let all =
                    analyseAssembly asm realProvider.Value [ "one.fs", file1; "two.fs", file2 ]
                    |> units

                let anchored = consolidatedDiagnostics all

                let hit =
                    anchored
                    |> List.tryFind (fun a -> a.Diagnostic.Message.Contains "undefinedThing")

                match hit with
                | None -> failtestf "no diagnostic mentioned undefinedThing; got %A" anchored
                | Some a ->
                    Expect.equal a.Path "two.fs" "anchored to file 2's path"
                    // Line 4 is `    let b = undefinedThing`; `undefinedThing` starts after
                    // the 4-space indent + "let b = " (col 13, 1-based).
                    Expect.equal a.Line 4 "line resolved against file 2's own text"
                    Expect.equal a.Col 13 "column resolved against file 2's own text"

                // No file-1 diagnostic bled into this: file 1 is clean.
                Expect.isEmpty
                    (anchored |> List.filter (fun a -> a.Path = "one.fs"))
                    "file 1 contributes no diagnostics"
            }

            test "file 2 reads a record FIELD declared in file 1 (cross-unit provider fallback)" {
                // R3: a record field read on a receiver whose record type is declared in a
                // PRIOR unit. Unit 1 declares `R = { X: int }` and a factory returning it;
                // unit 2 reads `.X`. Before R3 this errored "Unknown record type 'R'" because
                // `resolveFieldStep`'s `TyRecord` arm never consulted the provider on a local
                // miss — records were the one nominal kind with no provider field-read path.
                let file1 =
                    "\
namespace Test.A

module M =
    type R = { X: int }

    let make () : R = { X = 42 }
"

                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let r = make ()
    let z = r.X
"

                let all =
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
                    |> units

                let f2 = all.[1]

                // No provider-miss field-read error leaked (the pre-R3 failure mode).
                let unknownRecord =
                    f2.Frozen.Diagnostics
                    |> List.filter (fun d -> d.Severity = Severity.Error && d.Message.Contains "Unknown record")

                Expect.isEmpty
                    unknownRecord
                    (sprintf "cross-unit field read must not error 'Unknown record' (diagnostics: %A)" f2.Frozen.Diagnostics)

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf "cross-unit field read resolves clean (diagnostics: %A)" f2.Frozen.Diagnostics)

                // The read types as `int`: `z`'s exported scheme is the field's type,
                // resolved through the provider's frozen record shape.
                match (f2.View :> IExternalSymbolResolver).TryLookup "Test.B.N.z" with
                | ValueSome sym ->
                    Expect.equal sym.Scheme (FTConst(RuntimeNames.intKey, EqArray.empty)) "r.X types as int cross-unit"
                | ValueNone -> failtest "file 2 did not export z"
            }

            test "same offset-0 decl in both files does not break resolution" {
                // Both files open with `namespace` at offset 0 and a decl at identical
                // early offsets; because each unit owns its own Lexed/PassContext the keys
                // never collide, and the cross-file reference still resolves.
                let file2 =
                    "\
namespace Test.B

module N =
    let useF () : int = Test.A.M.f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1Qualified; "file2.fs", file2 ]
                    |> units

                // Both files carry a binding whose NodeKey offset is small/overlapping,
                // yet file 2 resolves file 1's `f` — the separate-unit invariant holds.
                Expect.isEmpty (unresolvedErrors all.[1]) "resolution survives colliding raw offsets"
            }
        ]
