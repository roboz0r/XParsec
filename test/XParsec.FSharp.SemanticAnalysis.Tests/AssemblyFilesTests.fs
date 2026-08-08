module XParsec.FSharp.SemanticAnalysis.Tests.AssemblyFilesTests

open Expecto
// Ahead of the SemanticAnalysis open so the bare `Diagnostic` stays the semantic one; this
// is here for the parser's `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The FRONT-END multi-file assembly pipeline.
// NO codegen here. It proves cross-file NAME RESOLUTION — file N+1 resolves file N's
// symbols by name through file N's projected provider view — plus forward-only scoping,
// nearest-first shadowing, and per-file diagnostic anchoring. Each file owns its own
// Input/Lexed, so `NodeKey` offsets are per-file and never collide across files.

let private asm = "MultiFileAsm"

/// The `Ok` files of an assembly run, or a test failure naming the first parse error.
let private files (results: Result<FrozenFile, UnparsedFile> list) : FrozenFile list =
    results
    |> List.map (
        function
        | Ok f -> f
        | Error e -> failtestf "file %s failed to parse: %A" e.Path e.Failure.Diagnostics
    )

/// A file's unresolved-symbol error diagnostics (the front end phrases both the bare and
/// the qualified miss with an "Unresolved" message).
let private unresolvedErrors (f: FrozenFile) : Diagnostic list =
    f.Frozen.Residue.Diagnostics
    |> List.filter (fun d -> Diagnostic.isError d && d.Message.Contains "Unresolved")

/// A file's TYPE-RESOLUTION-miss errors, both message families. An unresolved VALUE name
/// is phrased "Unresolved …"; a TYPE name that fails to resolve in annotation / signature
/// position is phrased "The type '…' is not defined" (`PassContext.Error`). A faithful
/// cross-file type-resolution check must catch BOTH — filtering only "Unresolved" let an
/// annotation-position type miss pass silently (a false green).
let private definitionErrors (f: FrozenFile) : Diagnostic list =
    f.Frozen.Residue.Diagnostics
    |> List.filter (fun d ->
        Diagnostic.isError d
        && (d.Message.Contains "Unresolved" || d.Message.Contains "is not defined")
    )

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
        "AssemblyFiles (multi-file front end)"
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
                    |> files

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "opened bare reference resolves through the view (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
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
                    |> files

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

            test "diagnostics anchor to their OWN file's path + (line, col)" {
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
                    |> files

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

            test "file 2 reads a record FIELD declared in file 1 (cross-file provider fallback)" {
                // A record field read on a receiver whose record type is declared in a PRIOR
                // file. File 1 declares `R = { X: int }` and a factory returning it; file 2
                // reads `.X`. `resolveFieldStep`'s `TyRecord` arm must consult the provider on
                // a local miss — records are otherwise the one nominal kind with no
                // provider field-read path.
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

                // No provider-miss field-read error leaked.
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

                // The read types as `int`: `z`'s exported scheme is the field's type,
                // resolved through the provider's frozen record shape.
                match (f2.View :> IExternalSymbolResolver).TryLookup "Test.B.N.z" with
                | ValueSome sym ->
                    Expect.equal sym.Scheme (FTConst(RuntimeNames.intKey, EqArray.empty)) "r.X types as int cross-file"
                | ValueNone -> failtest "file 2 did not export z"
            }

            test "file 2 CONSTRUCTS a record declared in file 1 — bare + qualified (cross-file provider)" {
                // A record LITERAL whose record type is declared in a PRIOR file.
                // File 1 declares `R = { X: int; Y: int }`; file 2 builds it two ways — bare
                // `{ X = 1; Y = 2 }` (resolved through the provider field-set reverse index)
                // and qualified `{ R.X = 3; R.Y = 4 }` (local `tryRecord` miss → the qualified
                // external filter). Both need `recordFieldSetVerdict` to consult the provider.
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

                // Both literals type as `R` (an `FTRecord` whose type key's simple name is R),
                // proving the construction resolved to file 1's record, not a fresh TyVar.
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
                // Pattern position: the record-literal arm of `inferPat` routes through
                // the SAME shared resolver, so a `{ X = x; Y = y }` pattern resolves to file 1's
                // record by field set cross-file.
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
                // Value-position + parameter-position annotation of a prior-file record:
                // `let h (r : R) = r` and `let g (r : R) : int = r.X`, `R` opened from file 1.
                // The annotated type name is name-resolved + stamped external, and its dotted
                // open-expansion (`Test.A.M.R`) resolves through the frozen provider's
                // module-containment fallback to file 1's record shape. Before that fallback the
                // dotted spelling missed the `+`-keyed identity index and the annotation errored
                // "The type 'R' is not defined".
                //
                // The annotations here are PURE (parameter type + field read): the name resolves
                // to one identity used consistently, so there is no error at all. A form that
                // also CONSTRUCTS the record (`let r : R = { X = 1 }`) does NOT belong here — see
                // the `ptest` below: construction pins the registered `InModule` identity while
                // the annotation carries the re-cut flattened one, and they disagree.
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

                // Honest guard: a pure annotation must raise NO error at all (a hidden
                // unification mismatch would slip past `definitionErrors`, which filters only
                // the not-defined / unresolved families).
                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "pure cross-file annotation raises no error (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "file 2's MEMBER signature annotates a type declared in file 1" {
                // Member-signature return + constructor-parameter annotation of a prior-file
                // type: a class in file 2 captures a `T` (ctor param annotation) and returns it
                // from a member (return annotation). Both annotated type names are prior-file type
                // names brought in by `open`, resolved through the same module-containment
                // fallback.
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

                // Ctor param + member return both annotate the SAME prior-file type, so their
                // one (flattened) identity is used consistently — no error at all.
                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "member-signature annotation raises no error (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "same offset-0 decl in both files does not break resolution" {
                // Both files open with `namespace` at offset 0 and a decl at identical
                // early offsets; because each file owns its own Lexed/PassContext the keys
                // never collide, and the cross-file reference still resolves.
                let file2 =
                    "\
namespace Test.B

module N =
    let useF () : int = Test.A.M.f 3
"

                let all =
                    analyseAssembly asm realProvider.Value [ "file1.fs", file1Qualified; "file2.fs", file2 ]
                    |> files

                // Both files carry a binding whose NodeKey offset is small/overlapping,
                // yet file 2 resolves file 1's `f` — the separate-file invariant holds.
                Expect.isEmpty (unresolvedErrors all.[1]) "resolution survives colliding raw offsets"
            }

            test "cross-file MODULE-HELD type: annotation identity matches construction identity" {
                // A module-held type's identity agrees across resolution paths:
                //   * construction / field-set (`{ X = 1 }`) pins the REGISTERED `InModule` key,
                //     carried structurally on `ExternalRecordCandidate.TypeKey`;
                //   * annotation (`r : R`) goes through NameResolution's `useSiteTypeKey`, which
                //     stamps the SAME registered key the provider resolved — the one that came
                //     back WITH the shape from `TryLookupType` — rather than re-cutting the
                //     dotted spelling into a flattened `InNamespace` holder.
                // So `let r : R = { X = 1 }` unifies the annotation against the literal with a
                // matching identity — no type mismatch.
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

            // Two cross-file resolution rules that were OVER-PERMISSIVE while records
            // landed (an INVALID program wrongly resolved — never a miscompile). Each
            // asserts the CORRECT, rejecting behaviour now that the gap is closed.

            test "RQA record is NOT bare-constructible across files (RequireQualifiedAccess honoured)" {
                // file 1 marks a record `[<RequireQualifiedAccess>]`; file 2 `open`s the
                // module and builds it with a BARE field-set literal. F# requires the
                // qualifier for an RQA record, so bare construction must NOT resolve — an
                // error. The RQA flag is threaded from the declaration's attributes through
                // freeze (`TTypeDecl.IsRequireQualifiedAccess` →
                // `ExternalRecordCandidate.IsRequireQualifiedAccess`) and honoured in the
                // bare-construction candidate filter (`InferResolve.admitsBareExternalRecord`),
                // so the projected RQA record is excluded from the bare field-set index.
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
                // file 1 declares a record; file 2 does NOT `open` its module, yet builds it
                // with a BARE field-set literal matching its fields. F#'s unqualified field
                // index (`eFieldLabels`) holds only `open`-ed records, so without the `open`
                // the bare literal must NOT resolve — an error. `recordFieldSetVerdict` gates
                // its provider `TryRecordsWithField` candidates by the live open scope
                // (`InferResolve.admitsBareExternalRecord` via `OpenScope.tryQualify`), so a
                // record whose declaring module is not reachable unqualified is excluded.
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
                // file 1 declares a class with a PUBLIC method and a `member private` one;
                // file 2 dispatches on each. A `private` member is not visible to another
                // file, so the private dispatch must NOT resolve — an error — while the
                // public one stays clean. Member accessibility rides
                // `TTypeMemberG.Accessibility` (captured from the CST `access` token) and
                // `FrozenSignature.membersOf` drops `Private` on the same internal-or-better
                // threshold `exported` applies to top-level entities, so the private member
                // never reaches the projected class shape's member set. The paired
                // public/private cases make the drop falsifiable: before this landed BOTH
                // resolved (OVER-PERMISSIVE — a leak, never a miscompile).
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
                // A view carries the file it was projected from, so a template it serves has
                // both readings available and the expansion abstracts the call into a
                // specialization entry. Anchored in file 1, which is the whole point: an entry
                // keeps the producer's positions, and a spliced body cannot.
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

                // The other half, and the one a relative pop cannot state: the EDGE is file 2's
                // own node, so it names file 2 while the entry it points at names file 1.
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
                // file 1 declares an intrinsic-repr primitive (`type x = (# "…" #)` — an
                // `ILIntrinsic` abbrev kept OUT of `Decls`); file 2 annotates a binding with
                // it. It resolves cross-file only because `FrozenSignature.toProvider`
                // publishes each `IntrinsicReprKeys` entry as an
                // `ExternalTypeShape.Intrinsic`, so `TryLookupType` answers the name — the
                // repr axes alone never did.
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
                // file 1 declares an interface with an abstract method; file 2 both
                // DISPATCHES on it (`g.Apply …`) and IMPLEMENTS it (`interface … with member
                // …`). Both resolve only because the interface arm of
                // `FrozenSignature.toProvider` now decurries each abstract method to an
                // `ExternalMember` under the interface key (the member set was `ValueNone`
                // before), so `TryLookupMembers` answers the dispatch and the conformance
                // check finds the required slot.
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
            //
            // F# makes a prior file's namespace-direct type bare-visible through an implicit
            // open of the CONSUMER's own `namespace N` header (`ImplicitlyOpenOwnNamespace`,
            // `CheckDeclarations.fs:355` — "Inside "namespace X.Y.Z" there is an implicit open
            // of "X.Y.Z""). A prior file contributes only its root NAME
            // (`AddLocalRootModuleOrNamespace`), never its contents — a producer never says
            // "open me". Vesper mints that implicit open in `CstWalk.addNamespacePrefix`.
            //
            // The pair below pins both directions, which is what makes the rule falsifiable:
            // publishing a file's declared namespaces as `AmbientOpenPrefixes` would pass the
            // SAME-namespace test while silently failing the DIFFERENT-namespace one.

            test "SAME-namespace later file resolves a prior file's type by BARE name (no open)" {
                // The consumer declares the SAME namespace as the producer, so its own header
                // implicitly opens `Test.A` and `Widget` resolves unqualified — with no `open`
                // written and nothing published by file 1's view. This is the case `Vesper.Core`
                // relies on: every Core file is `namespace Vesper`, so `compiler-attributes.fs`
                // reaches `prim-types-attr.clr.fs`'s `Attribute` this way.
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
                // The consumer declares a DIFFERENT namespace and writes no `open`, so `Test.A`
                // is not in its scope and `Widget` must not resolve. Its own header opens only
                // `Test.B`. Guards the leak that a producer-published ambient reintroduces: with
                // file 1's view publishing `Test.A` as `AmbientOpenPrefixes`, this resolved
                // clean — an implicit `open Test.A` no source line asked for.
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

            // A file that analysed only because RECOVERY patched its tree is not silent: its
            // parse diagnostics ride on the file and anchor against that file's own text,
            // exactly as the analysis residue does.
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

                // Selected on the VERDICT, not on a rendered code: the classification is what
                // the diagnostic carries, so this cannot be broken by a renumbering.
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
