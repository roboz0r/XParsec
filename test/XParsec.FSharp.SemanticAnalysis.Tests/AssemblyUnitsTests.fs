module XParsec.FSharp.SemanticAnalysis.Tests.AssemblyUnitsTests

open Expecto
// Ahead of the SemanticAnalysis open so the bare `Diagnostic` stays the semantic one; this
// is here for the parser's `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
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
        | Error e -> failtestf "unit %s failed to parse: %A" e.Path e.Failure.Diagnostics
    )

/// A unit's unresolved-symbol error diagnostics (the front end phrases both the bare and
/// the qualified miss with an "Unresolved" message).
let private unresolvedErrors (u: FrozenUnit) : Diagnostic list =
    u.Frozen.Residue.Diagnostics
    |> List.filter (fun d -> Diagnostic.isError d && d.Message.Contains "Unresolved")

/// A unit's TYPE-RESOLUTION-miss errors, both message families. An unresolved VALUE name
/// is phrased "Unresolved …"; a TYPE name that fails to resolve in annotation / signature
/// position is phrased "The type '…' is not defined" (`PassContext.Error`). A faithful
/// cross-unit type-resolution check must catch BOTH — filtering only "Unresolved" let an
/// annotation-position type miss pass silently (a false green).
let private definitionErrors (u: FrozenUnit) : Diagnostic list =
    u.Frozen.Residue.Diagnostics
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
                    |> units

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
                    f2.Frozen.Residue.Diagnostics
                    |> List.filter (fun d -> Diagnostic.isError d && d.Message.Contains "Unknown record")

                Expect.isEmpty
                    unknownRecord
                    (sprintf
                        "cross-unit field read must not error 'Unknown record' (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf "cross-unit field read resolves clean (diagnostics: %A)" f2.Frozen.Residue.Diagnostics)

                // The read types as `int`: `z`'s exported scheme is the field's type,
                // resolved through the provider's frozen record shape.
                match (f2.View :> IExternalSymbolResolver).TryLookup "Test.B.N.z" with
                | ValueSome sym ->
                    Expect.equal sym.Scheme (FTConst(RuntimeNames.intKey, EqArray.empty)) "r.X types as int cross-unit"
                | ValueNone -> failtest "file 2 did not export z"
            }

            test "file 2 CONSTRUCTS a record declared in file 1 — bare + qualified (cross-unit provider)" {
                // R4b-2: a record LITERAL whose record type is declared in a PRIOR unit.
                // Unit 1 declares `R = { X: int; Y: int }`; unit 2 builds it two ways — bare
                // `{ X = 1; Y = 2 }` (resolved through the provider field-set reverse index)
                // and qualified `{ R.X = 3; R.Y = 4 }` (local `tryRecord` miss → the qualified
                // external filter). Before R4b-2 both errored "No record type matches the field
                // set" because `recordFieldSetVerdict` never consulted the provider.
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
                    |> units

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
                        "cross-unit record construction must resolve clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf
                        "cross-unit record construction has no unresolved symbols (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                // Both literals type as `R` (an `FTRecord` whose type key's simple name is R),
                // proving the construction resolved to unit 1's record, not a fresh TyVar.
                let expectRecordR (name: string) =
                    match (f2.View :> IExternalSymbolResolver).TryLookup name with
                    | ValueSome sym ->
                        match sym.Scheme with
                        | FTRecord(key, _) ->
                            let (DisplayName shown) = SymbolKeyOps.typeSimpleName key
                            Expect.equal shown "R" (sprintf "%s types as record R cross-unit" name)
                        | other -> failtestf "%s expected to type as record R, got %A" name other
                    | ValueNone -> failtestf "file 2 did not export %s" name

                expectRecordR "Test.B.N.bare"
                expectRecordR "Test.B.N.qualified"
            }

            test "file 2 PATTERN-MATCHES a record declared in file 1 (cross-unit provider)" {
                // R4b-2 pattern position: the record-literal arm of `inferPat` routes through
                // the SAME shared resolver, so a `{ X = x; Y = y }` pattern resolves to unit 1's
                // record by field set cross-unit.
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
                    |> units

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
                        "cross-unit record pattern must resolve clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                Expect.isEmpty
                    (unresolvedErrors f2)
                    (sprintf
                        "cross-unit record pattern has no unresolved symbols (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "file 2 ANNOTATES a value + parameter with a record type declared in file 1" {
                // Value-position + parameter-position annotation of a prior-unit record:
                // `let h (r : R) = r` and `let g (r : R) : int = r.X`, `R` opened from file 1.
                // The annotation type head is name-resolved + stamped external, and its dotted
                // open-expansion (`Test.A.M.R`) resolves through the frozen provider's
                // module-containment fallback to unit 1's record shape. Before that fallback the
                // dotted spelling missed the `+`-keyed identity index and the annotation errored
                // "The type 'R' is not defined".
                //
                // The annotations here are PURE (parameter type + field read): the head resolves
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
                    |> units

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "value/parameter annotation of a cross-unit record resolves clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                // Honest guard: a pure annotation must raise NO error at all (a hidden
                // unification mismatch would slip past `definitionErrors`, which filters only
                // the not-defined / unresolved families).
                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "pure cross-unit annotation raises no error (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "file 2's MEMBER signature annotates a type declared in file 1" {
                // Member-signature return + constructor-parameter annotation of a prior-unit
                // type: a class in file 2 captures a `T` (ctor param annotation) and returns it
                // from a member (return annotation). Both annotation heads are prior-unit type
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
                    |> units

                let f2 = all.[1]

                Expect.isEmpty
                    (definitionErrors f2)
                    (sprintf
                        "member-signature annotation of a cross-unit type resolves clean (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)

                // Ctor param + member return both annotate the SAME prior-unit type, so their
                // one (flattened) identity is used consistently — no error at all.
                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "member-signature annotation raises no error (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
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

            test "cross-unit MODULE-HELD type: annotation identity matches construction identity" {
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
                    |> units

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "annotation + construction of a cross-unit module-held record agree on identity (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            // Two cross-unit resolution rules that were OVER-PERMISSIVE while records
            // landed (an INVALID program wrongly resolved — never a miscompile). Each
            // asserts the CORRECT, rejecting behaviour now that the gap is closed.

            test "RQA record is NOT bare-constructible across units (RequireQualifiedAccess honoured)" {
                // unit 1 marks a record `[<RequireQualifiedAccess>]`; unit 2 `open`s the
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
                    |> units

                let f2 = all.[1]

                let errs = f2.Frozen.Residue.Diagnostics |> Diagnostic.errors

                Expect.isNonEmpty
                    errs
                    (sprintf
                        "bare construction of a cross-unit RQA record must be rejected (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "record in an UNOPENED namespace is NOT bare-constructible across units (ambient-scope gate)" {
                // unit 1 declares a record; unit 2 does NOT `open` its module, yet builds it
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
                    |> units

                let f2 = all.[1]

                let errs = f2.Frozen.Residue.Diagnostics |> Diagnostic.errors

                Expect.isNonEmpty
                    errs
                    (sprintf
                        "bare construction of a cross-unit record without the `open` must be rejected (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "a cross-unit `member private` does NOT resolve for dispatch (member-level accessibility honoured)" {
                // unit 1 declares a class with a PUBLIC method and a `member private` one;
                // unit 2 dispatches on each. A `private` member is not visible to another
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
                    |> units
                    |> fun all -> all.[1].Frozen.Residue.Diagnostics
                    |> Diagnostic.errors

                let pubErrs = errorsOf publicCaller
                let privErrs = errorsOf privateCaller

                Expect.isEmpty
                    pubErrs
                    (sprintf "a cross-unit PUBLIC member dispatch resolves (diagnostics: %A)" pubErrs)

                Expect.isNonEmpty
                    privErrs
                    (sprintf "a cross-unit `member private` dispatch must be rejected (diagnostics: %A)" privErrs)
            }

            test "a prior unit's inline template is OUTLINED, not spliced" {
                // A view carries the file it was projected from, so a template it serves has
                // both readings available and the expansion abstracts the call into a
                // specialization entry. Anchored in unit 1, which is the whole point: an entry
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
                    |> units

                let consumer = all.[1]

                Expect.isEmpty
                    (consumer.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf "the cross-unit inline call resolves (diagnostics: %A)" consumer.Frozen.Residue.Diagnostics)

                match List.ofArray consumer.Frozen.Specializations with
                | [ entry ] ->
                    Expect.equal entry.Origin all.[0].Source.File "the entry is anchored in the DECLARING unit"
                | other -> failtestf "expected exactly one specialization entry, got %d" (List.length other)

                // The other half, and the one a relative pop cannot state: the EDGE is unit 2's
                // own node, so it names unit 2 while the entry it points at names unit 1.
                let edgeOrigins =
                    [
                        for p in consumer.Frozen.ExprPayloads do
                            match p with
                            | ExprPayload.InlineCall c -> yield c.Origin
                            | _ -> ()
                    ]

                match edgeOrigins with
                | [ o ] -> Expect.equal o consumer.Source.File "the call site is the CONSUMING unit's material"
                | other -> failtestf "expected exactly one edge in the consumer, got %d" (List.length other)
            }

            test "cross-unit INTRINSIC: a prior unit's primitive resolves in a later unit's annotation" {
                // unit 1 declares an intrinsic-repr primitive (`type x = (# "…" #)` — an
                // `ILIntrinsic` abbrev kept OUT of `Decls`); unit 2 annotates a binding with
                // it. It resolves cross-unit only because `FrozenSignature.toProvider`
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
                    |> units

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "a prior unit's primitive resolves in a later unit's annotation (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "cross-unit INTERFACE MEMBER: a prior unit's abstract method resolves for dispatch + conformance" {
                // unit 1 declares an interface with an abstract method; unit 2 both
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
                    |> units

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "cross-unit interface dispatch + conformance resolve the abstract method (diagnostics: %A)"
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
            // publishing a unit's declared namespaces as `AmbientOpenPrefixes` would pass the
            // SAME-namespace test while silently failing the DIFFERENT-namespace one.

            test "SAME-namespace later file resolves a prior unit's type by BARE name (no open)" {
                // The consumer declares the SAME namespace as the producer, so its own header
                // implicitly opens `Test.A` and `Widget` resolves unqualified — with no `open`
                // written and nothing published by unit 1's view. This is the case `Vesper.Core`
                // relies on: every Core file is `namespace Vesper`, so `compiler-attributes.fs`
                // reaches `prim-types-attr.fs`'s `Attribute` this way.
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
                    |> units

                let f2 = all.[1]

                Expect.isEmpty
                    (f2.Frozen.Residue.Diagnostics |> Diagnostic.errors)
                    (sprintf
                        "a same-namespace later file resolves a prior unit's type bare (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            test "DIFFERENT-namespace later file does NOT resolve a prior unit's type by bare name" {
                // The consumer declares a DIFFERENT namespace and writes no `open`, so `Test.A`
                // is not in its scope and `Widget` must not resolve. Its own header opens only
                // `Test.B`. Guards the leak that a producer-published ambient reintroduces: with
                // unit 1's view publishing `Test.A` as `AmbientOpenPrefixes`, this resolved
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
                    |> units

                let f2 = all.[1]

                Expect.isNonEmpty
                    (definitionErrors f2)
                    (sprintf
                        "a bare prior-unit type in an UNOPENED different namespace must not resolve (diagnostics: %A)"
                        f2.Frozen.Residue.Diagnostics)
            }

            // A unit that analysed only because RECOVERY patched its tree is not silent: its
            // parse diagnostics ride on the unit and anchor against that unit's own text,
            // exactly as the analysis residue does.
            test "a RECOVERED unit's parse diagnostics anchor to its own file" {
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
                    |> units

                let anchored = consolidatedDiagnostics all

                // Selected on the VERDICT, not on a rendered code: the classification is what
                // the diagnostic carries, so this cannot be broken by a renumbering.
                let isUnclosed (a: AssemblyUnits.AnchoredDiagnostic) =
                    match a.Diagnostic.Kind with
                    | Kind.Parse(DiagnosticCode.UnclosedDelimiter _) -> true
                    | _ -> false

                match anchored |> List.filter isUnclosed with
                | [ a ] ->
                    Expect.equal a.Path "broken.fs" "anchored to the unit that needed recovery"
                    Expect.isNonEmpty a.Diagnostic.Related "the opening delimiter is labelled"
                | other -> failtestf "expected one unclosed-delimiter diagnostic, got %A" other

                Expect.isEmpty
                    (anchored |> List.filter (fun a -> a.Path = "clean.fs"))
                    "the clean unit contributed none"
            }
        ]
