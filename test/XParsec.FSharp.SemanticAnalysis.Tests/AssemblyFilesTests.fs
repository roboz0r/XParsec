module XParsec.FSharp.SemanticAnalysis.Tests.AssemblyFilesTests

open Expecto
// Before the SemanticAnalysis open, so a bare `Diagnostic` stays the semantic one; opened
// here for `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Cross-file name resolution: file N+1 resolves file N's symbols through file N's
// projected provider view. Each file owns its own Input/Lexed, so `NodeKey` offsets are
// per-file.

let private asm = "MultiFileAsm"

/// The `Ok` files of an assembly run, or a test failure naming the first parse error.
let private files (results: Result<FrozenFile, UnparsedFile> list) : FrozenFile list =
    results
    |> List.map (
        function
        | Ok f -> f
        | Error e -> failtestf "file %s failed to parse: %A" e.Path e.Failure.Diagnostics
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

[<Tests>]
let tests =
    testList
        "AssemblyFiles (multi-file front end)"
        [
            test "file 2 resolves file 1's type + saturated function by QUALIFIED name" {
                let file2 =
                    "\
namespace Test.B

module N =
    let useT (t: Test.A.M.T) : int = t.value

    let useF () : int = Test.A.M.f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1Qualified; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1Qualified; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
                    |> files

                let f1 = all.[0]
                let f2 = all.[1]

                Expect.isNonEmpty (unresolvedErrors f1) "file 1's forward reference to file 2 is unresolved"
                Expect.isEmpty (unresolvedErrors f2) "file 2 (which sees file 1) is clean"
            }

            test "NEAREST file wins a name clash (compose ordering)" {
                // The composite's FIRST provider is the nearest file.
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
                    |> files

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

                // Without this the shadowing checks below could not tell the two apart.
                Expect.notEqual symEarlier.Scheme symLater.Scheme "the two files' dup schemes differ (int vs string)"

                // Nearest-first: `later` shadows `earlier`.
                let composedNearestLater =
                    ExternalSymbolProviders.composite [ viewLater; viewEarlier ] :> IExternalSymbolResolver

                match composedNearestLater.TryLookup name with
                | ValueSome s -> Expect.equal s.Scheme symLater.Scheme "nearest (later) file's dup wins"
                | ValueNone -> failtest "composed provider did not resolve dup"

                // The reverse ordering proves it is ORDER, not identity: nearest=earlier wins.
                let composedNearestEarlier =
                    ExternalSymbolProviders.composite [ viewEarlier; viewLater ] :> IExternalSymbolResolver

                match composedNearestEarlier.TryLookup name with
                | ValueSome s ->
                    Expect.equal s.Scheme symEarlier.Scheme "nearest (earlier) file's dup wins when it is first"
                | ValueNone -> failtest "composed provider did not resolve dup"
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
                    analyseAssembly asm realProvider.Value [ "one.fs", file1; "two.fs", file2 ]
                    |> files

                let anchored = consolidatedDiagnostics all

                let hit =
                    anchored
                    |> List.tryFind (fun a -> a.Diagnostic.Message.Contains "undefinedThing")

                match hit with
                | None -> failtestf "no diagnostic mentioned undefinedThing; got %A" anchored
                | Some a ->
                    Expect.equal a.Path "two.fs" "anchored to file 2's path"
                    // `    let b = undefinedThing`: 4-space indent + "let b = " ⇒ col 13, 1-based.
                    Expect.equal a.Line 4 "line resolved against file 2's own text"
                    Expect.equal a.Col 13 "column resolved against file 2's own text"

                Expect.isEmpty
                    (anchored |> List.filter (fun a -> a.Path = "one.fs"))
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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

                match (f2.View :> IExternalSymbolResolver).TryLookup "Test.B.N.z" with
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    match (f2.View :> IExternalSymbolResolver).TryLookup name with
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1Qualified; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
                    |> files

                let f2 = all.[1]

                let errs = f2.Frozen.Residue.Diagnostics |> Diagnostic.errors

                Expect.isNonEmpty
                    errs
                    (sprintf
                        "bare construction of a cross-file record without the `open` must be rejected (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", caller ]
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
                // producer's positions, which a spliced body could not.
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
                    |> files

                let consumer = all.[1]

                Expect.isEmpty
                    (consumer.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf "the cross-file inline call resolves (diagnostics: %A)" consumer.Frozen.Residue.Diagnostics)

                match List.ofArray consumer.Frozen.Specializations with
                | [ entry ] ->
                    Expect.equal entry.Origin all.[0].Source.File "the entry is anchored in the DECLARING file"
                | other -> failtestf "expected exactly one specialization entry, got %d" (List.length other)

                // The call-site EDGE is file 2's own node, so it names file 2 while the entry
                // it points at names file 1.
                let edgeOrigins =
                    [
                        for p in consumer.Frozen.ExprPayloads do
                            match p with
                            | ExprPayload.InlineCall c -> yield c.Origin
                            | _ -> ()
                    ]

                match edgeOrigins with
                | [ o ] -> Expect.equal o consumer.Source.File "the call site is the CONSUMING file's material"
                | other -> failtestf "expected exactly one edge in the consumer, got %d" (List.length other)
            }

            test "cross-file INTRINSIC: a prior file's primitive resolves in a later file's annotation" {
                // An intrinsic-repr primitive (`type myint = (# "System.Int32" #)`) is an
                // abbrev kept OUT of `Decls`, so it resolves cross-file only because freeze
                // publishes its repr entry as a lookup-answering intrinsic type shape.
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                // set — one answers the dispatch, the other the conformance check's slot.
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1; "file2.fs", file2 ]
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
                    analyseAssembly asm realProvider.Value [ "clean.fs", clean; "broken.fs", broken ]
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
                    Expect.equal a.Path "broken.fs" "anchored to the file that needed recovery"
                    Expect.isNonEmpty a.Diagnostic.Related "the opening delimiter is labelled"
                | other -> failtestf "expected one unclosed-delimiter diagnostic, got %A" other

                Expect.isEmpty
                    (anchored |> List.filter (fun a -> a.Path = "clean.fs"))
                    "the clean file contributed none"
            }
        ]
