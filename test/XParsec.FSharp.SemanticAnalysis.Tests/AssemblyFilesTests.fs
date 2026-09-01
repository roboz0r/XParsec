module XParsec.FSharp.SemanticAnalysis.Tests.AssemblyFilesTests

open Expecto
// Before the SemanticAnalysis open, so a bare `Diagnostic` stays the semantic one; opened
// here for `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Cross-file name resolution: file N+1 resolves file N's symbols through file N's
// projected provider view. Each file owns its own Input/Lexed, so `NodeKey` offsets are
// per-file.

let private asm: CompilingAssembly =
    {
        Name = AssemblyName "MultiFileAsm"
        Target = "none"
    }

/// A `.fs` with no `.fsi` beside it, so it publishes the surface it infers. Every test here
/// is in that case but the hiding ones, which pair the two halves explicitly.
let private impl (id: string) (text: string) : SourceUnit =
    SourceUnit.ofImplementation (SourceFile.ofText id text)

/// `AnalysedAssembly.analyse` over units held as TEXT, under no compilation defines. No source
/// in this suite carries a `#if`, so every file here parses one way.
let private analyseAssembly
    (assembly: CompilingAssembly)
    (external: IExternalSymbolProvider)
    (units: SourceUnit list)
    : UnitOutcome list =
    let analysed =
        AnalysedAssembly.analyse
            Pipeline.analyseFileFor
            external
            {
                Assembly = assembly
                Units = List.map (AssemblyUnit.parse Set.empty) units
            }

    analysed.Units

/// The analysed files of an assembly run, or a test failure citing the first parse error.
let private files (outcomes: UnitOutcome list) : FrozenFile list =
    outcomes
    |> List.map (
        function
        | UnitOutcome.Analysed u -> u.File
        | UnitOutcome.Failed(leading, rest) ->
            failtestf
                "unit failed to parse: %A"
                [ for e in leading :: rest -> e.Id.Name, FileFault.diagnostics e.Fault ]
    )

/// A file's unresolved-symbol errors — both the bare and the qualified miss say "Unresolved".
let private unresolvedErrors (f: FrozenFile) : Diagnostic list =
    f.Frozen.Residue.Diagnostics
    |> List.filter (fun d -> Diagnostic.isError d && d.Message.Contains "Unresolved")

/// A file's resolution-miss errors, both message families: a VALUE name misses as
/// "Unresolved …", a TYPE name in annotation or signature position as "The type '…' is
/// not defined".
let private definitionErrors (f: FrozenFile) : Diagnostic list =
    f.Frozen.Residue.Diagnostics
    |> List.filter (fun d ->
        Diagnostic.isError d
        && (d.Message.Contains "Unresolved" || d.Message.Contains "is not defined")
    )

// --- shared file-1 export surface ------------------------------------------------------

let private file1Qualified =
    "\
namespace Test.A

module M =
    type T = { value: int }

    let f (x: int) : int = x + 1
"

// --- shared file-1 hiding surface ------------------------------------------------------
// Two records and two functions, of which the signature below publishes one of each.

let private file1Signed =
    "\
namespace Test.A

module M =
    type Shown = { value: int }

    type Hidden = { other: int }

    let shown (x: int) : int = x + 1

    let hidden (x: int) : int = x + 2
"

let private file1Fsi =
    "\
namespace Test.A

module M =
    val shown: int -> int

    type Shown = { value: int }
"

/// A file 2 referencing one of file 1's types and one of its functions, both qualified.
let private usesFile1 (typeName: string) (valueName: string) : string =
    sprintf
        "\
namespace Test.B

module N =
    let useT (t: Test.A.M.%s) : int = 0

    let useF () : int = Test.A.M.%s 3
"
        typeName
        valueName

[<Tests>]
let tests =
    testList
        "AssemblyFiles (multi-file front end)"
        [
            // A file publishes the surface it froze, so refusing to freeze one that reported an
            // error would report every name it exports as missing, once per later file.
            test "an error in file 1 leaves the rest of its surface visible to file 2" {
                let file1 =
                    "\
namespace Test.A

module M =
    type T = { value: int }

    let f (x: int) : int = x + 1

    let bad = notDefinedAnywhere 1
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [ impl "file1.fs" file1; impl "file2.fs" (usesFile1 "T" "f") ]
                    |> files

                Expect.hasLength all 2 "both files analysed"

                Expect.isNonEmpty (unresolvedErrors all.[0]) "file 1 reports its own unresolved name"

                Expect.isEmpty
                    (definitionErrors all.[1])
                    (sprintf
                        "file 2 inherits none of file 1's fault (diagnostics: %A)"
                        (all.[1].Frozen.Residue.Diagnostics |> List.map (fun d -> d.Message)))
            }

            test "file 2 resolves file 1's type + saturated function by QUALIFIED name" {
                let file2 =
                    "\
namespace Test.B

module N =
    let useT (t: Test.A.M.T) : int = t.value

    let useF () : int = Test.A.M.f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1Qualified; impl "file2.fs" file2 ]
                    |> files

                Expect.hasLength all 2 "both files analysed"
                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "file 2 has no unresolved/undefined-type errors (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "file 2 resolves file 1's exports through an OPEN-ed bare reference" {
                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let useT (t: T) : int = t.value

    let useF () : int = f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1Qualified; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "opened bare reference resolves through the view (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "FORWARD-only: file 1 cannot resolve a symbol defined only in file 2" {
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
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f1 = all.[0]
                let f2 = all.[1]

                Expect.isNonEmpty (unresolvedErrors f1) "file 1's forward reference to file 2 is unresolved"
                Expect.isEmpty (unresolvedErrors f2) "file 2 (which sees file 1) is clean"
            }

            test "composite resolves first-provider-wins (ordering, not identity)" {
                // Composes the two views BY HAND, so it pins `composite` alone. What the
                // assembly front end actually hands a third file is the test below.
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
                    analyseAssembly asm realProvider.Value [ impl "earlier.fs" earlier; impl "later.fs" later ]
                    |> files

                let viewEarlier = all.[0].View
                let viewLater = all.[1].View

                let name = "Test.Shared.dup"

                let symEarlier =
                    match ScopeContents.tryValueAt viewEarlier.Scope name with
                    | ValueSome s -> s
                    | ValueNone -> failtest "earlier file did not export dup"

                let symLater =
                    match ScopeContents.tryValueAt viewLater.Scope name with
                    | ValueSome s -> s
                    | ValueNone -> failtest "later file did not export dup"

                // Without this the shadowing checks below could not tell the two apart.
                Expect.notEqual symEarlier.Scheme symLater.Scheme "the two files' dup schemes differ (int vs string)"

                // Nearest-first: `later` shadows `earlier`.
                let composedNearestLater =
                    ExternalSymbolProviders.composite [ viewLater; viewEarlier ] :> IExternalSymbolResolver

                match ScopeContents.tryValueAt composedNearestLater.Scope name with
                | ValueSome s -> Expect.equal s.Scheme symLater.Scheme "nearest (later) file's dup wins"
                | ValueNone -> failtest "composed provider did not resolve dup"

                // The reverse ordering proves it is ORDER, not identity: nearest=earlier wins.
                let composedNearestEarlier =
                    ExternalSymbolProviders.composite [ viewEarlier; viewLater ] :> IExternalSymbolResolver

                match ScopeContents.tryValueAt composedNearestEarlier.Scope name with
                | ValueSome s ->
                    Expect.equal s.Scheme symEarlier.Scheme "nearest (earlier) file's dup wins when it is first"
                | ValueNone -> failtest "composed provider did not resolve dup"
            }

            test "a THIRD file sees the NEARER of two re-declarations" {
                // The rule end-to-end, through `analyseAssembly` rather than a hand-composed
                // list: file 2 re-declares file 1's `dup` at a DIFFERENT type, and file 3 must
                // resolve the one file 2 declared.
                let file1 =
                    "\
namespace Test

module Shared =
    let dup = 1
"

                let file2 =
                    "\
namespace Test

module Shared =
    let dup = \"hello\"
"

                // Annotating `string` is itself the assertion: it type-checks only if the
                // NEARER declaration won.
                let file3 =
                    "\
namespace Test.C

module C =
    let useIt: string = Test.Shared.dup
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [ impl "file1.fs" file1; impl "file2.fs" file2; impl "file3.fs" file3 ]
                    |> files

                let name = "Test.Shared.dup"

                let schemeOf (f: FrozenFile) =
                    match ScopeContents.tryValueAt f.View.Scope name with
                    | ValueSome s -> s.Scheme
                    | ValueNone -> failtestf "file did not export %s" name

                let schemeEarlier = schemeOf all.[0]
                let schemeLater = schemeOf all.[1]

                Expect.notEqual schemeEarlier schemeLater "the two files' dup schemes differ (int vs string)"

                let errors = all.[2].Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError

                Expect.isEmpty errors "file 3 binding the nearer (string) dup to a string is clean"

                // `Scoped` is the composed provider file 3 was analysed against.
                match ScopeContents.tryValueAt all.[2].Scoped.Scope name with
                | ValueSome s ->
                    Expect.equal s.Scheme schemeLater "file 3 resolves dup to the NEARER file's declaration"
                | ValueNone -> failtest "file 3's scoped provider did not resolve dup"
            }

            // A frozen tree's nodes carry the `AssemblyFilePath` their anchors index, and the name is
            // never reopened, so one that varies with the checkout or the host OS freezes the
            // same sources to different trees.
            test "a ROOTED name is refused, on every OS rather than the host one" {
                for rooted in [ "/z.fs"; "\\z.fs"; "C:/work/z.fs"; "D:\\work\\z.fs" ] do
                    match AssemblyFileId.tryOfRelative rooted with
                    | Ok id -> failtestf "%s names a checkout, not a file within an assembly (got %s)" rooted id.Name
                    | Error why -> Expect.stringContains why "rooted" (sprintf "refused for being rooted; got %s" why)
            }

            test "one file has ONE name, however it was spelled" {
                let canonical = AssemblyFileId.ofRelative "math/z.fs"

                for spelling in [ "math\\z.fs"; "./math/z.fs"; "math/./z.fs"; "sub/../math/z.fs" ] do
                    Expect.equal
                        (AssemblyFileId.ofRelative spelling)
                        canonical
                        (sprintf "%s canonicalises to math/z.fs" spelling)
            }

            test "a name that climbs out of its assembly, or does not identify a file, is refused" {
                for bad in [ "../z.fs"; "a/../../z.fs"; ""; "   "; "."; "a/.." ] do
                    match AssemblyFileId.tryOfRelative bad with
                    | Ok id -> failtestf "'%s' does not identify a file within an assembly (got '%s')" bad id.Name
                    | Error _ -> ()
            }

            // Whether two spellings are two files is decided by the FILESYSTEM, not the
            // platform: Windows can mount a case-sensitive directory.
            test "a file is named as the DISK has it, not as the caller spelled it" {
                let dir =
                    System.IO.Path.Combine(
                        System.IO.Path.GetTempPath(),
                        "vesper-case-" + System.Guid.NewGuid().ToString("N")
                    )

                System.IO.Directory.CreateDirectory dir |> ignore

                try
                    let source = "namespace Test.A\n\nmodule M =\n    let f () : int = 1\n"
                    System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "MixedCase.fs"), source)

                    if System.IO.File.Exists(System.IO.Path.Combine(dir, "mixedcase.fs")) then
                        // Case-insensitive: both spellings open ONE file, so both must give the
                        // one name it has, or its anchors fork on how it was asked for.
                        Expect.equal
                            (SourceFile.read dir "mixedcase.fs").Id
                            (AssemblyFileId.ofRelative "MixedCase.fs")
                            "the disk's own casing is the file's identity"
                    else
                        // Case-sensitive: two genuinely different files, and `mixedcase.fs` is
                        // absent.
                        Expect.throws
                            (fun () -> SourceFile.read dir "mixedcase.fs" |> ignore)
                            "a name the disk does not have reads nothing"
                finally
                    System.IO.Directory.Delete(dir, true)
            }

            test "a `\\`-spelled file freezes to the same tree as its `/` twin" {
                let source = "namespace Test.A\n\nmodule M =\n    let f () : int = 1\n"

                let frozenAs (spelling: string) =
                    match analyseAssembly asm realProvider.Value [ impl spelling source ] with
                    | [ UnitOutcome.Analysed u ] -> u.File.Retained.Path
                    | other -> failtestf "expected one analysed file, got %A" other

                Expect.equal
                    (frozenAs "math\\z.fs")
                    (frozenAs "math/z.fs")
                    "the separator is the host OS's business, not the assembly's"
            }

            test "diagnostics anchor to their OWN file's path + (line, col)" {
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
                    analyseAssembly asm realProvider.Value [ impl "one.fs" file1; impl "two.fs" file2 ]
                    |> files

                let anchored = consolidatedDiagnostics all

                let hit =
                    anchored
                    |> List.tryFind (fun a -> a.Diagnostic.Message.Contains "undefinedThing")

                match hit with
                | None -> failtestf "no diagnostic mentioned undefinedThing; got %A" anchored
                | Some a ->
                    Expect.equal (AssemblyFileId.toStored a.Path) "two.fs" "anchored to file 2's path"
                    // `    let b = undefinedThing`: 4-space indent + "let b = " ⇒ col 13, 1-based.
                    Expect.equal a.Line 4 "line resolved against file 2's own text"
                    Expect.equal a.Col 13 "column resolved against file 2's own text"

                Expect.isEmpty
                    (anchored |> List.filter (fun a -> AssemblyFileId.toStored a.Path = "one.fs"))
                    "file 1 contributes no diagnostics"
            }

            test "file 2 reads a record FIELD declared in file 1 (cross-file provider fallback)" {
                // The record arm of field resolution must consult the provider on a local miss.
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
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                let unknownRecord =
                    f2.Frozen.Residue.Diagnostics
                    |> List.filter (fun d -> Diagnostic.isError d && d.Message.Contains "Unknown record")

                Expect.isEmpty
                    unknownRecord
                    (sprintf
                        "cross-file field read must not error 'Unknown record' (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf "cross-file field read resolves clean (diagnostics: %A)" f2.Frozen.Residue.Diagnostics)

                match ScopeContents.tryValueAt f2.View.Scope "Test.B.N.z" with
                | ValueSome sym ->
                    Expect.equal sym.Scheme (FTConst(RuntimeNames.intKey, EqArray.empty)) "r.X types as int cross-file"
                | ValueNone -> failtest "file 2 did not export z"
            }

            test "file 2 CONSTRUCTS a record declared in file 1 — bare + qualified (cross-file provider)" {
                // Bare `{ X = 1; Y = 2 }` resolves through the provider's field-set reverse
                // index; qualified `{ R.X = 3; R.Y = 4 }` through the qualified-external filter.
                let file1 =
                    "\
namespace Test.A

module M =
    type R = { X: int; Y: int }
"

                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let bare = { X = 1; Y = 2 }
    let qualified = { R.X = 3; R.Y = 4 }
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                let recordErrors =
                    f2.Frozen.Residue.Diagnostics
                    |> List.filter (fun d ->
                        Diagnostic.isError d
                        && (d.Message.Contains "Unknown record"
                            || d.Message.Contains "No record type matches"
                            || d.Message.Contains "Field set is ambiguous")
                    )

                Expect.isEmpty
                    recordErrors
                    (sprintf
                        "cross-file record construction must resolve clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf
                        "cross-file record construction has no unresolved symbols (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                // Typing as `R` and not a fresh TyVar is what proves it found file 1's record.
                let expectRecordR (name: string) =
                    match ScopeContents.tryValueAt f2.View.Scope name with
                    | ValueSome sym ->
                        match sym.Scheme with
                        | FTRecord(key, _) ->
                            let (DisplayName shown) = SymbolKeyOps.typeSimpleName key
                            Expect.equal shown "R" (sprintf "%s types as record R cross-file" name)
                        | other -> failtestf "%s expected to type as record R, got %A" name other
                    | ValueNone -> failtestf "file 2 did not export %s" name

                expectRecordR "Test.B.N.bare"
                expectRecordR "Test.B.N.qualified"
            }

            test "file 2 PATTERN-MATCHES a record declared in file 1 (cross-file provider)" {
                // Pattern position takes the same provider route: `{ X = x; Y = y }` resolves
                // to file 1's record by field set.
                let file1 =
                    "\
namespace Test.A

module M =
    type R = { X: int; Y: int }
"

                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let sum () : int =
        let r = { X = 20; Y = 22 }

        match r with
        | { X = x; Y = y } -> x + y
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                let patternErrors =
                    f2.Frozen.Residue.Diagnostics
                    |> List.filter (fun d ->
                        Diagnostic.isError d
                        && (d.Message.Contains "Unknown record"
                            || d.Message.Contains "No record type matches"
                            || d.Message.Contains "has no field")
                    )

                Expect.isEmpty
                    patternErrors
                    (sprintf
                        "cross-file record pattern must resolve clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf
                        "cross-file record pattern has no unresolved symbols (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "file 2 ANNOTATES a value + parameter with a record type declared in file 1" {
                // The annotation's dotted open-expansion (`Test.A.M.R`) resolves through the
                // frozen provider's module-containment fallback to file 1's record shape.
                let file1 =
                    "\
namespace Test.A

module M =
    type R = { X: int }
"

                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let h (r : R) : R = r
    let g (r : R) : int = r.X
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "value/parameter annotation of a cross-file record resolves clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                // A unification mismatch would slip past `definitionErrors`, which filters only
                // the not-defined / unresolved families — so require zero errors too.
                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "pure cross-file annotation raises no error (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "file 2's MEMBER signature annotates a type declared in file 1" {
                // Ctor-parameter and member-return annotations of a prior-file type, both
                // resolved through the same module-containment fallback.
                let file1 =
                    "\
namespace Test.A

module M =
    type T = { value: int }
"

                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    type Box(t: T) =
        member _.Get() : T = t
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "member-signature annotation of a cross-file type resolves clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                // Both annotate the SAME prior-file type, so one identity is used throughout.
                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "member-signature annotation raises no error (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "same offset-0 decl in both files does not break resolution" {
                // Each file owns its own Lexed/PassContext, so decls at identical early
                // offsets still mint distinct keys.
                let file2 =
                    "\
namespace Test.B

module N =
    let useF () : int = Test.A.M.f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1Qualified; impl "file2.fs" file2 ]
                    |> files

                Expect.isEmpty (unresolvedErrors all.[1]) "resolution survives colliding raw offsets"
            }

            test "cross-file MODULE-HELD type: annotation identity matches construction identity" {
                // `let r : R = { X = 1 }` unifies an annotation against a literal, so both paths
                // must pin the same registered key: construction through the record candidate's
                // type key, annotation through the key the provider returned with the shape.
                let file1 =
                    "\
namespace Test.A

module M =
    type R = { X: int }
"

                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let r : R = { X = 1 }
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "annotation + construction of a cross-file module-held record agree on identity (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "RQA record is NOT bare-constructible across files (RequireQualifiedAccess honoured)" {
                // F# requires the qualifier for an RQA record, so freeze carries the flag
                // through and the projected record is excluded from the bare field-set index.
                let file1 =
                    "\
namespace Test.A

module M =
    [<RequireQualifiedAccess>]
    type R = { X: int; Y: int }
"

                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let bare = { X = 1; Y = 2 }
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                let errs = f2.Frozen.Residue.Diagnostics |> Diagnostic.errors

                Expect.isNonEmpty
                    errs
                    (sprintf
                        "bare construction of a cross-file RQA record must be rejected (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "record in an UNOPENED namespace is NOT bare-constructible across files (ambient-scope gate)" {
                // F#'s unqualified field index holds only `open`-ed records, so the provider's
                // field-set candidates are gated by the live open scope: a record whose
                // declaring module is unreachable unqualified is excluded.
                let file1 =
                    "\
namespace Test.A

module M =
    type R = { X: int; Y: int }
"

                let file2 =
                    "\
namespace Test.B

module N =
    let bare = { X = 1; Y = 2 }
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                let errs = f2.Frozen.Residue.Diagnostics |> Diagnostic.errors

                Expect.isNonEmpty
                    errs
                    (sprintf
                        "bare construction of a cross-file record without the `open` must be rejected (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "a referenced package's record in a prelude namespace is bare-constructible (ambient scope admits)" {
                // `Vesper` resolves unqualified in every compilation, so `Vesper.Core`'s
                // `Ref<'T>` reaches the bare field-set index through the ambient tail of the
                // open scope, which is the channel a written `open` feeds for a cross-file
                // record. `dotnet fsi` types `{ contents = 1 }` as `int ref` the same way.
                let source =
                    "\
namespace Test.B

module N =
    let cell = { contents = 1 }
"

                let all = analyseAssembly asm realProvider.Value [ impl "file1.fs" source ] |> files
                let f1 = all.[0]

                Expect.isEmpty
                    (f1.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "bare construction of a package record must resolve clean (diagnostics: %A)"
                        f1.Frozen.Residue.Diagnostics)

                // Typing as the package's key, not a fresh TyVar, is what proves the
                // candidate came from the provider.
                match ScopeContents.tryValueAt f1.View.Scope "Test.B.N.cell" with
                | ValueSome sym ->
                    Expect.equal
                        sym.Scheme
                        (FTRecord(
                            RuntimeNames.vesperRefKey,
                            EqArray.singleton (FTConst(RuntimeNames.intKey, EqArray.empty))
                        ))
                        "the bare literal types as Vesper.Ref<int>"
                | ValueNone -> failtest "the file did not export cell"
            }

            test "a cross-file `member private` does NOT resolve for dispatch (member-level accessibility honoured)" {
                // A `private` member is not visible to another file, so the private dispatch
                // must error while the public one stays clean: freeze drops `Private` members
                // on the same internal-or-better threshold it applies to top-level entities.
                let file1 =
                    "\
namespace Test.A

module M =
    type Gadget() =
        member _.Pub(x: int) : int = x
        member private _.Priv(x: int) : int = x
"

                let publicCaller =
                    "\
namespace Test.B

open Test.A.M

module N =
    let call (g: Gadget) : int = g.Pub 1
"

                let privateCaller =
                    "\
namespace Test.B

open Test.A.M

module N =
    let call (g: Gadget) : int = g.Priv 1
"

                let errorsOf (caller: string) =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" caller ]
                    |> files
                    |> fun all -> all.[1].Frozen.Residue.Diagnostics
                    |> Diagnostic.errors

                let pubErrs = errorsOf publicCaller
                let privErrs = errorsOf privateCaller

                Expect.isEmpty
                    pubErrs
                    (sprintf "a cross-file PUBLIC member dispatch resolves (diagnostics: %A)" pubErrs)

                Expect.isNonEmpty
                    privErrs
                    (sprintf "a cross-file `member private` dispatch must be rejected (diagnostics: %A)" privErrs)
            }

            test "a prior file's inline template is OUTLINED, not spliced" {
                // The specialization entry is anchored in the declaring file: it keeps the
                // declaring file's positions, which a spliced body could not.
                let file1 =
                    "\
namespace Test.A

module M =
    let inline twice (x: int) : int = x + x
"

                let file2 =
                    "\
namespace Test.B

module N =
    let four = Test.A.M.twice 2
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let consumer = all.[1]

                Expect.isEmpty
                    (consumer.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf "the cross-file inline call resolves (diagnostics: %A)" consumer.Frozen.Residue.Diagnostics)

                // The served `+` inside the template mints entries of its own, so select
                // `twice`'s.
                let twiceEntries =
                    consumer.Frozen.Specializations
                    |> Array.filter (fun e -> SymbolKeyOps.simpleName e.Key.Template = DisplayName "twice")

                match List.ofArray twiceEntries with
                | [ entry ] ->
                    Expect.equal entry.Path all.[0].Retained.Path "the entry is anchored in the DECLARING file"
                | other -> failtestf "expected exactly one `twice` specialization entry, got %d" (List.length other)

                // The call-site EDGE is file 2's own node, so its origin is file 2 while the
                // entry it points at has file 1.
                let edgeOrigins =
                    [
                        for p in consumer.Frozen.ExprPayloads do
                            match p with
                            | ExprPayload.InlineCall c -> yield c.Path
                            | _ -> ()
                    ]

                match edgeOrigins |> List.filter (fun o -> o = consumer.Retained.Path) with
                | [ _ ] -> ()
                | other ->
                    failtestf
                        "expected exactly one edge in the consuming file's own material, got %d"
                        (List.length other)
            }

            test "cross-file INTRINSIC: a prior file's primitive resolves in a later file's annotation" {
                // An intrinsic-repr primitive (`type myint = (# "System.Int32" #)`) is an
                // abbrev kept OUT of `Decls`, so it resolves cross-file only because freeze
                // publishes its repr entry as a lookup-visible intrinsic type shape.
                let file1 =
                    "\
namespace Test.A

#nowarn \"42\"

type myint = (# \"System.Int32\" #)
"

                let file2 =
                    "\
namespace Test.B

module N =
    let identity (x: Test.A.myint) : Test.A.myint = x
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "a prior file's primitive resolves in a later file's annotation (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "cross-file INTERFACE MEMBER: a prior file's abstract method resolves for dispatch + conformance" {
                // file 2 both DISPATCHES on the abstract method and IMPLEMENTS it. Both need
                // freeze to decurry each abstract method into the interface's projected member
                // set — one resolves the dispatch, the other fills the conformance check's slot.
                let file1 =
                    "\
namespace Test.A

type Applier<'A, 'B> =
    abstract member Apply: arg: 'A -> 'B
"

                let file2 =
                    "\
namespace Test.B

type Twice(g: Test.A.Applier<int, int>) =
    member _.Call(x: int) : int = g.Apply(g.Apply(x))

type IdInt() =
    interface Test.A.Applier<int, int> with
        member _.Apply(x) = x
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "cross-file interface dispatch + conformance resolve the abstract method (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            // --- the enclosing-namespace rule, both directions ---------------------------
            // A file's own `namespace X.Y.Z` header implicitly opens `X.Y.Z` (F# compiler
            // `ImplicitlyOpenOwnNamespace`); a prior file contributes only its root NAME.

            test "SAME-namespace later file resolves a prior file's type by BARE name (no open)" {
                // Every `Vesper.Core` file is `namespace Vesper`, so this is how one Core file
                // reaches a prior Core file's type with no `open` written.
                let file1 =
                    "\
namespace Test.A

type Widget = { X: int }
"

                let file2 =
                    "\
namespace Test.A

module N =
    let h (w: Widget) : int = w.X
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "a same-namespace later file resolves a prior file's type bare (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "DIFFERENT-namespace later file does NOT resolve a prior file's type by bare name" {
                // The consumer's header opens only `Test.B`. Publishing file 1's declared
                // namespaces as ambient prefixes would resolve `Widget` here — an implicit
                // `open Test.A` no source line asked for.
                let file1 =
                    "\
namespace Test.A

type Widget = { X: int }
"

                let file2 =
                    "\
namespace Test.B

module N =
    let h (w: Widget) : int = w.X
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isNonEmpty
                    (definitionErrors f2)
                    (sprintf
                        "a bare prior-file type in an UNOPENED different namespace must not resolve (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "a RECOVERED file's parse diagnostics anchor to its own file" {
                let clean =
                    "\
namespace Test

module A =
    let a = 1
"

                let broken =
                    "\
namespace Test

module B =
    let b = (1 + 2
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "clean.fs" clean; impl "broken.fs" broken ]
                    |> files

                let anchored = consolidatedDiagnostics all

                // Selected on the verdict the diagnostic carries, not a rendered code, so a
                // renumbering cannot break it.
                let isUnclosed (a: AssemblyFiles.AnchoredDiagnostic) =
                    match a.Diagnostic.Kind with
                    | Kind.Parse(DiagnosticCode.UnclosedDelimiter _) -> true
                    | _ -> false

                match anchored |> List.filter isUnclosed with
                | [ a ] ->
                    Expect.equal
                        (AssemblyFileId.toStored a.Path)
                        "broken.fs"
                        "anchored to the file that needed recovery"

                    Expect.isNonEmpty a.Diagnostic.Related "the opening delimiter is labelled"
                | other -> failtestf "expected one unclosed-delimiter diagnostic, got %A" other

                Expect.isEmpty
                    (anchored |> List.filter (fun a -> AssemblyFileId.toStored a.Path = "clean.fs"))
                    "the clean file contributed none"
            }

            // --- a `.fsi` HIDES ------------------------------------------------------
            // A file with a signature publishes what the signature DECLARES, not what its
            // implementation infers, and a later file resolves only through that.

            test "file 2 resolves what file 1's `.fsi` publishes" {
                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Fsi)
                                (SourceFile.ofText "file1.fs" file1Signed)
                            impl "file2.fs" (usesFile1 "Shown" "shown")
                        ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf "a published type and val resolve (diagnostics: %A)" (consolidatedDiagnostics all))
            }

            test "file 2 constructs a published record from its FIELD NAMES alone" {
                // No type named: the field set is all the literal carries, so it resolves
                // through the field-reverse index the signature publishes, scope-gated by
                // the `open` — the same channel an unsigned file's own view publishes.
                let file2 =
                    "\
namespace Test.B

open Test.A.M

module N =
    let make () = { value = 1 }
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Fsi)
                                (SourceFile.ofText "file1.fs" file1Signed)
                            impl "file2.fs" file2
                        ]
                    |> files

                Expect.isEmpty
                    (all.[1].Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "the bare record literal resolves to the published record"
            }

            test "file 2 cannot resolve what file 1's `.fsi` OMITS" {
                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Fsi)
                                (SourceFile.ofText "file1.fs" file1Signed)
                            impl "file2.fs" (usesFile1 "Hidden" "hidden")
                        ]
                    |> files

                let f2 = all.[1]

                Expect.isNonEmpty (definitionErrors f2) "the unpublished type and val are hidden"
            }

            // A MEMBER whose signature references a type the compilation cannot resolve declares
            // nothing a consumer could call, so it is dropped rather than faulting the file.
            // The DROP is reported: what the signature promised is now absent for every later
            // file, and discovering that as an unresolved name three files on is worse.
            test "a `.fsi` member referencing an unresolvable type is dropped, and the drop WARNS" {
                let file1Sig =
                    "\
namespace Test.A

module M =
    type Holder =
        member Reachable: int -> int
        member Unreachable: NoSuchTypeAnywhere -> int
"

                let file1 =
                    "\
namespace Test.A

module M =
    type Holder =
        member this.Reachable(x: int) : int = x
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                        ]
                    |> files

                let anchored = consolidatedDiagnostics all

                let dropped =
                    anchored |> List.filter (fun a -> a.Diagnostic.Code = DiagCode.Vesper "V245")

                match dropped with
                | [ a ] ->
                    Expect.equal a.Diagnostic.Severity Severity.Warning "a gap in what the compiler models WARNS"

                    Expect.equal
                        (AssemblyFileId.toStored a.Path)
                        "file1.fsi"
                        "anchored to the signature that made the claim"

                    Expect.stringContains a.Diagnostic.Message "Unreachable" "the report names the member it dropped"
                | other -> failtestf "expected exactly one V245 for the dropped member, got %A" other

                // The refusal itself does NOT survive: the name is wrong in the contract, and
                // that is the drop's business rather than a second finding against the file.
                Expect.isEmpty
                    (anchored
                     |> List.filter (fun a ->
                         Diagnostic.isError a.Diagnostic
                         && a.Diagnostic.Message.Contains "NoSuchTypeAnywhere"
                     ))
                    "the unresolved name is reported once, as the drop"
            }

            // `extern` is the spelling a signature normally uses, but the inline-IL form
            // parses in a `.fsi` too, and both grammars must read it as the primitive BINDING
            // it is: read as a transparent alias it registers no abbreviation either, and the
            // type publishes a name with no shape behind it.
            test "a `.fsi` inline-IL abbreviation publishes the primitive, not an empty alias" {
                let file1Sig =
                    "\
namespace Test.A

type myint = (# \"System.Int32\" #)
"

                let file1 = file1Sig

                let file2 =
                    "\
namespace Test.B

module N =
    let f (x: Test.A.myint) : Test.A.myint = x
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                            impl "file2.fs" file2
                        ]
                    |> files

                Expect.isEmpty
                    (definitionErrors all.[1])
                    (sprintf
                        "the signature's `(# … #)` binding is reachable (diagnostics: %A)"
                        all.[1].Frozen.Residue.Diagnostics)
            }

            test "the SAME file 1 publishes them when it has no `.fsi`" {
                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [ impl "file1.fs" file1Signed; impl "file2.fs" (usesFile1 "Hidden" "hidden") ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "nothing is hidden without a signature to hide it (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "the `.fsi` REPLACES the inferred signature: a narrower type is what binds" {
                let file1 =
                    "\
namespace Test.A

module M =
    let same x = x
"

                let file1Sig =
                    "\
namespace Test.A

module M =
    val same: x: int -> int
"

                let file2 =
                    "\
namespace Test.B

module N =
    let useIt () : string = Test.A.M.same \"s\"
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                            impl "file2.fs" file2
                        ]
                    |> files

                // The implementation infers `'a -> 'a`, which would take the `string`. The
                // declared `int -> int` is what file 2 meets, so the argument is rejected —
                // and rejected on its TYPE, having resolved through the signature.
                Expect.isEmpty (unresolvedErrors all.[1]) "the declared binding resolves"

                Expect.isNonEmpty
                    (all.[1].Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "the declared parameter type rejects the argument"
            }

            test "a `let inline`'s template survives the `.fsi` that publishes its declaration" {
                let file1 =
                    "\
namespace Test.A

module M =
    let inline twice (x: int) : int = x + x
"

                let file1Sig =
                    "\
namespace Test.A

module M =
    val inline twice: x: int -> int
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                        ]
                    |> files

                // The signature carries no template — the implementation's is layered back on
                // under the same key, so a later file still splices rather than calling.
                match ScopeContents.tryValueAt all.[0].View.Scope "Test.A.M.twice" with
                | ValueSome s -> Expect.isTrue s.InlineBody.IsSome "the published symbol carries its body"
                | ValueNone -> failtest "twice did not publish"
            }

            test "a `val` the implementation does not satisfy is a conformance error on the `.fsi`" {
                let file1 =
                    "\
namespace Test.A

module M =
    let present (x: int) : int = x
"

                let file1Sig =
                    "\
namespace Test.A

module M =
    val present: x: int -> int

    val absent: x: int -> int
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                        ]
                    |> files

                let conformance =
                    consolidatedDiagnostics all
                    |> List.filter (fun a ->
                        match a.Diagnostic.Kind with
                        | Kind.Conformance _ -> true
                        | _ -> false
                    )

                match conformance with
                | [ a ] ->
                    Expect.equal (AssemblyFileId.toStored a.Path) "file1.fsi" "anchored to the file that made the claim"
                    Expect.stringContains a.Diagnostic.Message "absent" "refers to the unsatisfied val"
                | other -> failtestf "expected one conformance error, got %A" other
            }

            test "the two halves must agree on their leading declaration" {
                // The pairing rule a manifest-paired unit is held to as well: two files whose
                // leading `namespace` differ are not a pair, so every finding below that
                // would be about the wrong companion.
                let file1 =
                    "\
namespace Test.A

module M =
    let f (x: int) : int = x
"

                let file1Sig =
                    "\
namespace Test.Elsewhere

module M =
    val f: x: int -> int
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                        ]
                    |> files

                let mismatches =
                    consolidatedDiagnostics all
                    |> List.filter (fun a ->
                        match a.Diagnostic.Kind with
                        | Kind.Conformance(_, ConformanceVerdict.ModulePairingMismatch _) -> true
                        | _ -> false
                    )

                match mismatches with
                | [ a ] ->
                    Expect.stringContains a.Diagnostic.Message "Test.A" "names the implementation's declaration"
                    Expect.stringContains a.Diagnostic.Message "Test.Elsewhere" "names the signature's"
                | other -> failtestf "expected one pairing mismatch, got %A" other
            }

            test "a `.fsi` the parser had to RECOVER reports against the signature's own text" {
                // The signature half is as lenient as the implementation half: recovery patches
                // the tree and carries its findings, anchored to the `.fsi`, where the gate
                // then refuses them. Only a signature that yields no tree at all fails the unit.
                let file1 = "namespace Test.A\n\nmodule M =\n    let f (x: int) : int = x\n"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" "namespace Test.A\n\nmodule M =\n    val f: (((\n")
                                (SourceFile.ofText "file1.fs" file1)
                        ]
                    |> files

                let parseFindings =
                    consolidatedDiagnostics all
                    |> List.filter (fun a ->
                        match a.Diagnostic.Kind with
                        | Kind.Parse _ -> true
                        | _ -> false
                    )

                match parseFindings with
                | [] -> failtest "the malformed signature reported nothing"
                | findings ->
                    Expect.all
                        findings
                        (fun a -> AssemblyFileId.toStored a.Path = "file1.fsi")
                        "every parse finding anchors to the signature, not its companion"
            }

            test "a published symbol is homed at the IMPLEMENTATION, not the signature" {
                // A home identifies where a symbol physically lives, and what a backend emits for
                // this unit is compiled from the `.fs`. A backend that resolves the home to a
                // module path would otherwise point to a file no build writes.
                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Fsi)
                                (SourceFile.ofText "file1.fs" file1Signed)
                        ]
                    |> files

                match ScopeContents.tryValueAt all.[0].View.Scope "Test.A.M.shown" with
                | ValueSome s ->
                    match s.Origin.Home.DeclaringFile with
                    | ValueSome f ->
                        Expect.equal (AssemblyFileId.toStored f.Relative) "file1.fs" "homed at the compiled file"
                    | ValueNone -> failtest "the published symbol carries no declaring file"
                | ValueNone -> failtest "shown did not publish"
            }

            test "a `member inline`'s template survives the `.fsi` that publishes the member" {
                // The member half of the same rule the `let inline` case states: the signature
                // publishes the member, the implementation's lifted body is layered back on
                // under the member key a call site resolves through. The two keys are minted by
                // different code paths, so their agreement is what this asserts.
                let file1 =
                    "\
namespace Test.A

#nowarn \"42\"

type prim =
    (# \"System.Int32\" #)

    with

        member inline this.Poke(x: int) : int = x

    end
"

                let file1Sig =
                    "\
namespace Test.A

type prim = extern with
    member inline Poke: x: int -> int
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                        ]
                    |> files

                match ExternalSymbols.tryMetaTypeAt all.[0].View "Test.A.prim" 0 with
                | ValueSome(struct (typeKey, _)) ->
                    match all.[0].View.TryLookupMember(typeKey, "Poke") with
                    | ValueSome m -> Expect.isTrue m.InlineBody.IsSome "the published member carries its body"
                    | ValueNone -> failtest "Poke did not publish"
                | ValueNone -> failtest "prim did not publish"
            }

            test "an in-assembly `type t = extern` takes the repr its own `.fs` binds" {
                // The `.fs` pre-scan the signature extraction runs first: without it the type
                // publishes as `Unsupported <target>` and every later mention of it is an error.
                let file1 =
                    "\
namespace Test.A

type prim = (# \"System.Int32\" #)
"

                let file1Sig =
                    "\
namespace Test.A

type prim = extern
"

                let all =
                    analyseAssembly
                        asm
                        realProvider.Value
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" file1Sig)
                                (SourceFile.ofText "file1.fs" file1)
                        ]
                    |> files

                match ExternalSymbols.tryMetaType all.[0].View "Test.A.prim" with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Platform = IntrinsicPlatform.Bound platform
                                                                 }
                                                        }) ->
                    Expect.equal platform (PlatformTypeId "System.Int32") "the type id came from the sibling `.fs`"
                | other -> failtestf "expected Test.A.prim to publish its repr, got %A" other
            }

            test "file 2 resolves a type abbreviation file 1 declares with no `.fsi`" {
                let file1 =
                    "\
namespace Test.A

type myalias = int
"

                let file2 =
                    "\
namespace Test.B

module N =
    let f (x: Test.A.myalias) : Test.A.myalias = x
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf "file 2 resolves the abbreviation (diagnostics: %A)" f2.Frozen.Residue.Diagnostics)

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "file 2 leaves no unresolved TyVar behind the abbreviation"

                match ExternalSymbols.tryMetaType all.[0].View "Test.A.myalias" with
                | ValueSome(ExternalTypeShape.Abbrev(arity, _)) ->
                    Expect.equal arity 0 "`myalias` takes no type parameter"
                | other -> failtestf "expected Test.A.myalias to publish as an abbreviation, got %A" other
            }

            test "file 2 expands a GENERIC abbreviation file 1 declares with no `.fsi`" {
                let file1 =
                    "\
namespace Test.A

type pair<'T> = 'T * 'T
"

                let file2 =
                    "\
namespace Test.B

module N =
    let swap (p: Test.A.pair<int>) : int * int =
        let (a, b) = p
        b, a
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    (sprintf "file 2 expands `pair<int>` to `int * int` (diagnostics: %A)" f2.Frozen.Residue.Diagnostics)

                match ExternalSymbols.tryMetaType all.[0].View "Test.A.pair`1" with
                | ValueSome(ExternalTypeShape.Abbrev(arity, _)) ->
                    Expect.equal arity 1 "`pair` takes one type parameter"
                | other -> failtestf "expected Test.A.pair to publish as an abbreviation, got %A" other
            }

            test "a `private` type abbreviation is readable from a nested module of its declaring module" {
                let file1 =
                    "\
namespace Test.A

module Priv =
    type private myalias = int

    module Nested =
        let h (x: myalias) : int = x + 1

    let f (x: myalias) : int = x
"

                let all = analyseAssembly asm realProvider.Value [ impl "file1.fs" file1 ] |> files

                Expect.isEmpty
                    (all.[0].Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "`private` reaches the declaring module and the modules nested in it"
            }

            test "file 2 does NOT resolve a `private` type abbreviation file 1 declares" {
                let file1 =
                    "\
namespace Test.A

module Priv =
    type private myalias = int

    let f (x: myalias) : int = x
"

                let file2 =
                    "\
namespace Test.B

module N =
    let g (x: Test.A.Priv.myalias) : int = x
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                Expect.isEmpty
                    (all.[0].Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "the declaring file reads its own `private` abbreviation"

                Expect.isNonEmpty
                    (all.[1].Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "FS1092: a `private` abbreviation is out of reach from another file"
            }

            test "file 2 resolves an `internal` type abbreviation file 1 declares" {
                let file1 =
                    "\
namespace Test.A

module Intern =
    type internal myalias = int
"

                let file2 =
                    "\
namespace Test.B

module N =
    let g (x: Test.A.Intern.myalias) : int = x + 1
"

                let all =
                    analyseAssembly asm realProvider.Value [ impl "file1.fs" file1; impl "file2.fs" file2 ]
                    |> files

                Expect.isEmpty
                    (all.[1].Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    (sprintf
                        "`internal` reaches the rest of the assembly (diagnostics: %A)"
                        all.[1].Frozen.Residue.Diagnostics)
            }
        ]
