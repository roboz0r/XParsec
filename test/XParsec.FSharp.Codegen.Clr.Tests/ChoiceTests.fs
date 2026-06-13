module XParsec.FSharp.Codegen.Clr.Tests.ChoiceTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// vesper-lib-test-plan Phase 2 — the behavioral runtime suite for `Vesper.Choice`,
// the read-across from `Vesper.Result`. Choice is the *thinnest* of the pure-data
// unions: a `[<Struct>]` two-case union (`Choice1Of2` of `'T1`, `Choice2Of2` of
// `'T2`) with NO module — its sole consumer `set.fs` constructs a Choice and
// consumes it in the next `match`, never escaping (choice.fsi). So unlike Option /
// Result there is no `ChoiceModule` to reflect, and the two routes are:
//
//   * REFLECTION-INVOKE for the pure-data surface (construction + the two
//     discriminators). `buildPackage "Vesper.Choice"` emits a real
//     `Vesper.Choice.dll`; we construct `Choice1Of2`/`Choice2Of2` through the
//     union's emitted static case factories and read the discriminating `_tag`
//     field (declaration order: `Choice1Of2` = 0, `Choice2Of2` = 1) and the
//     per-case payload fields (`Choice1Of2_0` / `Choice2Of2_0`) directly — there
//     is no `isChoice1Of2` module function, so the tag IS the discriminator.
//   * DRIVER PROGRAMS for construction + `match` across the package boundary
//     (`open Vesper`), through the now-general cross-package machinery (Gap 2
//     Layers B/C). Choice has no instance members and no module, so a constructed
//     value is observed only by a `match`.
//
// Like `Result<'T,'TError>`, `Choice<'T1,'T2>` has *two* type parameters and each
// case constrains only one — `Choice1Of2 5` is `Choice<int, '_>`; F# generalises
// the free one, so every standalone value is annotated `: Choice<int, string>` to
// pin both parameters and keep each row's instantiation explicit.

/// The built `Vesper.Choice.dll` (cached). `buildPackage` loads it into its own
/// ALC and returns the loaded assembly; every type/value below is reflected from
/// *this* assembly so identities line up across `Invoke`s.
let private choiceAsm: Lazy<Assembly> =
    lazy (fst (buildPackage "Vesper.Choice").Value)

let private intTy = typeof<int>
let private strTy = typeof<string>

/// `Vesper.Choice`2` closed over <int, string> — the receiver type for the case
/// factories and field reads. (The `[<CompiledName("FSharpChoice`2")>]` on the
/// contract is a C#-interop alias the backend does not apply to the emitted type
/// name, same as `Vesper.Result`2` carries `FSharpResult`2`.)
let private choiceIntStr: Lazy<Type> =
    lazy (choiceAsm.Value.GetType("Vesper.Choice`2").MakeGenericType(intTy, strTy))

/// `Choice1Of2 (v: int) : Choice<int, string>` via the emitted static factory.
let private c1IS (v: int) : obj =
    choiceIntStr.Value.GetMethod("Choice1Of2").Invoke(null, [| box v |])

/// `Choice2Of2 (v: string) : Choice<int, string>` via the emitted static factory.
let private c2IS (v: string) : obj =
    choiceIntStr.Value.GetMethod("Choice2Of2").Invoke(null, [| box v |])

/// The discriminating `_tag` field (declaration order) off a Choice value.
let private tagOf (receiver: obj) : int =
    choiceIntStr.Value.GetField("_tag").GetValue(receiver) :?> int

/// Read a per-case payload field (`Choice1Of2_0` / `Choice2Of2_0`) off a value.
let private fieldOf (name: string) (receiver: obj) : obj =
    choiceIntStr.Value.GetField(name).GetValue(receiver)

[<Tests>]
let tests =
    testList
        "Choice"
        [
            // ---- construction round-trips through the case factories ----------
            test "Choice1Of2 / Choice2Of2 construct distinct values" {
                Expect.isNotNull (c1IS 3) "Choice1Of2 3 constructs"
                Expect.isNotNull (c2IS "boom") "Choice2Of2 \"boom\" constructs"
            }

            // ---- discriminators: the `_tag` field IS the discriminator (no
            //      module). Declaration order pins Choice1Of2 = 0, Choice2Of2 = 1.
            test "tag discriminates: Choice1Of2 -> 0, Choice2Of2 -> 1" {
                Expect.equal (tagOf (c1IS 3)) 0 "Choice1Of2 is tag 0"
                Expect.equal (tagOf (c2IS "e")) 1 "Choice2Of2 is tag 1"
            }

            // ---- payload extraction: the active case's field holds the value ---
            test "Choice1Of2 payload reads back through Choice1Of2_0" {
                Expect.equal (fieldOf "Choice1Of2_0" (c1IS 7) :?> int) 7 "Choice1Of2 7 carries 7"
            }

            test "Choice2Of2 payload reads back through Choice2Of2_0" {
                Expect.equal (fieldOf "Choice2Of2_0" (c2IS "hi") :?> string) "hi" "Choice2Of2 \"hi\" carries \"hi\""
            }
        ]

// Higher-arity `Choice<'T1, …, 'T7>` (arity-overload follow-up).
// These are *distinct* emitted types `Vesper.Choice`3`…`Vesper.Choice`7` — the
// proof that the whole pipeline (front-end type registry, codegen `userTypes` /
// `genericUnions`, the external contract provider) is keyed by `(name, arity)`,
// not the bare short name "Choice". Same reflection idiom as the arity-2 suite:
// construct each case through its emitted static factory and read the `_tag`
// discriminator + per-case payload field.

let private boolTy = typeof<bool>

/// `Vesper.Choice`3` closed over <int, string, bool>.
let private choice3: Lazy<Type> =
    lazy (choiceAsm.Value.GetType("Vesper.Choice`3").MakeGenericType(intTy, strTy, boolTy))

/// `Vesper.Choice`7` closed over seven `int`s (a uniform instantiation keeps the
/// factory calls terse — the arity, not the element types, is what's under test).
let private choice7: Lazy<Type> =
    lazy (choiceAsm.Value.GetType("Vesper.Choice`7").MakeGenericType(Array.create 7 intTy))

let private tagOfOn (ty: Type) (receiver: obj) : int =
    ty.GetField("_tag").GetValue(receiver) :?> int

let private fieldOfOn (ty: Type) (name: string) (receiver: obj) : obj = ty.GetField(name).GetValue(receiver)

[<Tests>]
let higherArity =
    testList
        "ChoiceHigherArity"
        [
            // `Vesper.Choice`3` is a genuinely distinct emitted type (not collapsed
            // onto `Choice`2`): all three case factories exist and tag in
            // declaration order 0/1/2.
            test "Choice`3 constructs all three cases; tags 0/1/2" {
                let c1 = choice3.Value.GetMethod("Choice1Of3").Invoke(null, [| box 5 |])
                let c2 = choice3.Value.GetMethod("Choice2Of3").Invoke(null, [| box "hi" |])
                let c3 = choice3.Value.GetMethod("Choice3Of3").Invoke(null, [| box true |])
                Expect.equal (tagOfOn choice3.Value c1) 0 "Choice1Of3 is tag 0"
                Expect.equal (tagOfOn choice3.Value c2) 1 "Choice2Of3 is tag 1"
                Expect.equal (tagOfOn choice3.Value c3) 2 "Choice3Of3 is tag 2"
            }

            // Each case's payload reads back through its own `<Case>_0` field — the
            // struct carries one field per arm (`Choice1Of3_0` … `Choice3Of3_0`).
            test "Choice`3 payloads read back per case" {
                let c1 = choice3.Value.GetMethod("Choice1Of3").Invoke(null, [| box 5 |])
                let c3 = choice3.Value.GetMethod("Choice3Of3").Invoke(null, [| box true |])
                Expect.equal (fieldOfOn choice3.Value "Choice1Of3_0" c1 :?> int) 5 "Choice1Of3 carries 5"
                Expect.equal (fieldOfOn choice3.Value "Choice3Of3_0" c3 :?> bool) true "Choice3Of3 carries true"
            }

            // The widest arm of the family: `Vesper.Choice`7` emits distinctly, with
            // its last case `Choice7Of7` tagging at declaration index 6.
            test "Choice`7 emits; first/last cases tag 0/6" {
                let first = choice7.Value.GetMethod("Choice1Of7").Invoke(null, [| box 1 |])
                let last = choice7.Value.GetMethod("Choice7Of7").Invoke(null, [| box 7 |])
                Expect.equal (tagOfOn choice7.Value first) 0 "Choice1Of7 is tag 0"
                Expect.equal (tagOfOn choice7.Value last) 6 "Choice7Of7 is tag 6"
                Expect.equal (fieldOfOn choice7.Value "Choice7Of7_0" last :?> int) 7 "Choice7Of7 carries 7"
            }

            // The arity-2 and arity-3 receivers are different `Type`s — pins that the
            // emitter did NOT collapse the overloaded short name to a single type.
            test "Choice`2 and Choice`3 are distinct emitted types" {
                Expect.notEqual choiceIntStr.Value.Name choice3.Value.Name "`2 and `3 have distinct metadata names"
                Expect.equal choiceIntStr.Value.Name "Choice`2" "arity-2 metadata name"
                Expect.equal choice3.Value.Name "Choice`3" "arity-3 metadata name"
            }
        ]

// Cross-package higher-arity consumption (`open Vesper`): construction + a 3-arm
// `match` on `Choice<int, string, bool>`, exercising the external contract
// provider's arity-keyed resolution end-to-end (the arity-3 case index + the
// arity-3 type shape, distinct from arity-2).
[<Tests>]
let higherArityRuntime =
    testList
        "ChoiceHigherArityRuntime"
        [
            test "match over a 3-way Choice selects each arm" {
                runsChoiceLines
                    [ "1"; "2"; "3" ]
                    ("open Vesper\n"
                     + "let pick (c: Choice<int, string, bool>) =\n"
                     + "    match c with\n    | Choice1Of3 _ -> 1\n    | Choice2Of3 _ -> 2\n    | Choice3Of3 _ -> 3\n"
                     + "printfn \"%d\" (pick (Choice1Of3 9))\n"
                     + "printfn \"%d\" (pick (Choice2Of3 \"x\"))\n"
                     + "printfn \"%d\" (pick (Choice3Of3 true))")
            }

            test "match binds the middle arm's payload of a 3-way Choice" {
                runsChoice
                    "hi"
                    ("open Vesper\n"
                     + "let msg (c: Choice<int, string, bool>) =\n"
                     + "    match c with\n    | Choice1Of3 _ -> \"one\"\n    | Choice2Of3 s -> s\n    | Choice3Of3 _ -> \"three\"\n"
                     + "printfn \"%s\" (msg (Choice2Of3 \"hi\"))")
            }
        ]

// Construction (Layer B) + pattern matching (Layer C) of `Choice`'s cases across
// the package boundary (`open Vesper`). Choice has no nullary case (both cases
// carry a field), no instance members, and no module, so the constructed value is
// read back only through a `match` — which drives each case's field extract
// (`Choice1Of2_0` at tag 0, `Choice2Of2_0` at tag 1).
[<Tests>]
let ctorAndMatchRuntime =
    testList
        "ChoiceCtorRuntime"
        [
            // Emit smoke: both case factories produce valid IL and the program runs.
            test "Choice1Of2 and Choice2Of2 construct and run (Layer B emit smoke)" {
                runsChoice
                    "ok"
                    ("open Vesper\n"
                     + "let a : Choice<int, string> = Choice1Of2 5\n"
                     + "let b : Choice<int, string> = Choice2Of2 \"boom\"\n"
                     + "printfn \"%s\" \"ok\"")
            }

            // `match` extracts the `Choice1Of2` payload (tag 0, int field) and
            // defaults on `Choice2Of2`; the case-pattern type is driven by the
            // annotated parameter.
            test "match extracts Choice1Of2 payload, defaults on Choice2Of2" {
                runsChoiceLines
                    [ "7"; "0" ]
                    ("open Vesper\n"
                     + "let describe (c: Choice<int, string>) =\n    match c with\n    | Choice1Of2 x -> x\n    | Choice2Of2 _ -> 0\n"
                     + "printfn \"%d\" (describe (Choice1Of2 7))\n"
                     + "printfn \"%d\" (describe (Choice2Of2 \"boom\"))")
            }

            // `match` binding the *Choice2Of2* field (tag 1, string field) — the
            // second case's `<Choice2Of2>_0` extract, distinct from the arm above.
            test "match binds the Choice2Of2 payload" {
                runsChoiceLines
                    [ "one"; "boom" ]
                    ("open Vesper\n"
                     + "let msg (c: Choice<int, string>) =\n    match c with\n    | Choice1Of2 _ -> \"one\"\n    | Choice2Of2 e -> e\n"
                     + "printfn \"%s\" (msg (Choice1Of2 1))\n"
                     + "printfn \"%s\" (msg (Choice2Of2 \"boom\"))")
            }

            // Discriminate without binding (`Choice1Of2 _`), driving only the `_tag`
            // compare — no field extract.
            test "match discriminates Choice1Of2 vs Choice2Of2" {
                runsChoiceLines
                    [ "true"; "false" ]
                    ("open Vesper\n"
                     + "let isFirst (c: Choice<int, string>) =\n    match c with\n    | Choice1Of2 _ -> true\n    | Choice2Of2 _ -> false\n"
                     + "printfn \"%b\" (isFirst (Choice1Of2 1))\n"
                     + "printfn \"%b\" (isFirst (Choice2Of2 \"e\"))")
            }
        ]

// Front-end regression guard (analysis only): the cross-package Choice surface
// type-checks through the contract provider's ambient open scope. The cheap A/B/C
// guard the plan calls for (Choice has no module, so no Layer D).
[<Tests>]
let frontEndTests =
    testList
        "ChoiceFrontEnd"
        [
            // Construction resolves through the reverse case index (open `Vesper`).
            test "Choice1Of2 5 types as Choice<int, string> (annotated)" {
                typeChecksChoice "let x : Choice<int, string> = Choice1Of2 5"
            }

            test "Choice2Of2 \"boom\" types as Choice<int, string> (annotated)" {
                typeChecksChoice "let x : Choice<int, string> = Choice2Of2 \"boom\""
            }

            // Qualified case forms `Choice.Choice1Of2` / `Choice.Choice2Of2`.
            test "Choice.Choice1Of2 / Choice.Choice2Of2 (qualified) type-check" {
                typeChecksChoice
                    "let x : Choice<int, string> = Choice.Choice1Of2 5\nlet y : Choice<int, string> = Choice.Choice2Of2 \"e\""
            }

            // `match` binds each case's field at the receiver's instantiation.
            test "match Choice1Of2 x binds x : int; Choice2Of2 e binds e : string" {
                typeChecksChoice
                    "let f (c: Choice<int, string>) : int =\n    match c with\n    | Choice1Of2 x -> x\n    | Choice2Of2 _ -> 0"

                typeChecksChoice
                    "let f (c: Choice<int, string>) : string =\n    match c with\n    | Choice1Of2 _ -> \"\"\n    | Choice2Of2 e -> e"
            }

            // Direct generic reference (no abbreviation) resolves as a union.
            test "Vesper.Choice<int, string> direct ref type-checks in a match" {
                typeChecksChoice
                    "let f (c: Vesper.Choice<int, string>) : int =\n    match c with\n    | Choice1Of2 x -> x\n    | Choice2Of2 _ -> 0"
            }

            // Higher-arity `Choice<'T1, 'T2, 'T3>` resolves to the *arity-3* overload
            // (a distinct type from `Choice`2`), and its cases bind at the right
            // field types — the arity-overload resolution end to end in the front end.
            test "Choice<int, string, bool> (arity 3) construction type-checks" {
                typeChecksChoice "let x : Choice<int, string, bool> = Choice2Of3 \"e\""
            }

            test "match on Choice<int, string, bool> binds each arm at its field type" {
                typeChecksChoice
                    "let f (c: Choice<int, string, bool>) : int =\n    match c with\n    | Choice1Of3 x -> x\n    | Choice2Of3 _ -> 0\n    | Choice3Of3 _ -> 1"
            }

            // A case from the wrong arity does NOT belong to the arity-3 type — the
            // `Choice2Of2` ctor types as `Choice`2`, so annotating it `Choice`3` is a
            // type error (proves the arities are genuinely distinct types).
            test "Choice1Of7 types as the arity-7 overload" {
                typeChecksChoice "let x : Choice<int, int, int, int, int, int, int> = Choice1Of7 5"
            }
        ]
