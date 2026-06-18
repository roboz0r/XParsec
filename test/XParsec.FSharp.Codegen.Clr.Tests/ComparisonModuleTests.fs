module XParsec.FSharp.Codegen.Clr.Tests.ComparisonModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `Vesper.Comparison`, the ordering family (`<` / `>` / `<=` / `>=`).
//
// Unlike Option / Result / Choice this package is *signature-only*
// (`manifest.toml` has `impl = []`): the four operators are `let inline`
// static-optimizations over inline IL, spliced at each use site, so there is no
// `Vesper.Comparison.dll` to build or reflect. The package is already in
// `defaultManifests`, so its operators resolve in every program the plain
// `runs` / `typeChecks` helpers compile — which makes the use-site runtime
// matrix below *both* Layer B (the inline bodies splice + emit) and Layer R
// (they return the right answer). No opt-in harness, no DLL.
//
// `comparison.fs` carries one CIL-emitting clause per primitive
// (`when ^T: byte/char/bool/float32/float/int64/int`) plus a structural *base*
// that routes through `Comparer<^T>.Default.Compare(x, y)`. This suite is the
// per-primitive completion of that clause table:
//
//   * `ComparisonTests.fs` already prints `<`/`>`/`<=`/`>=` on int and the bare
//     `<` on char/float — the broad cheap net. This file fills in the *missing*
//     primitive clauses (byte, bool, float32, int64) and exercises all four
//     operators per type, so a regression in any single `clt`/`cgt` clause is a
//     punctual red row.
//   * `StructuralComparisonTests.fs` covers the structural base for *user*
//     records/unions (generated `IComparable`). The base rows here cover the
//     base for a BCL type with no clause — `string` — so the
//     `Comparer<string>.Default` fallback is proven independently of codegen's
//     own `CompareTo` emission.
//
// All seven primitive clauses now run end-to-end: the `float32` and `int64`
// literal-lowering gaps that originally gated their rows are closed (`float32`'s
// `f`-suffixed literal now lowers in the front end; `int64` now has a codegen
// `Const(Int64) → ldc.i8` emit arm).
//
// Each `comparison-prim` row is one `runs` assertion named by its source, so the
// table *is* the coverage map for "which ordering opcode fires for which
// primitive".

[<Tests>]
let primitiveTests =
    testList
        "ComparisonPrimitives"
        [
            for src, expected in
                [
                    // ---- byte (uy): clt/cgt over the unsigned value (0..255 are
                    //      all positive as i4, so signed clt/cgt is correct) -------
                    """printfn "%b" (1uy < 2uy)""", "true"
                    """printfn "%b" (2uy < 1uy)""", "false"
                    """printfn "%b" (2uy > 1uy)""", "true"
                    """printfn "%b" (1uy > 2uy)""", "false"
                    """printfn "%b" (1uy <= 1uy)""", "true"
                    """printfn "%b" (2uy <= 1uy)""", "false"
                    """printfn "%b" (1uy >= 1uy)""", "true"
                    """printfn "%b" (1uy >= 2uy)""", "false"

                    // ---- char: clt/cgt over the char's int value (the bare `<` is
                    //      in ComparisonTests; here are the other three ops) -------
                    """printfn "%b" ('a' > 'b')""", "false"
                    """printfn "%b" ('b' > 'a')""", "true"
                    """printfn "%b" ('a' <= 'a')""", "true"
                    """printfn "%b" ('b' <= 'a')""", "false"
                    """printfn "%b" ('b' >= 'a')""", "true"
                    """printfn "%b" ('a' >= 'b')""", "false"

                    // ---- bool: ordered as i4 (false = 0 < true = 1) -------------
                    """printfn "%b" (false < true)""", "true"
                    """printfn "%b" (true < false)""", "false"
                    """printfn "%b" (true > false)""", "true"
                    """printfn "%b" (false > true)""", "false"
                    """printfn "%b" (false <= false)""", "true"
                    """printfn "%b" (true <= false)""", "false"
                    """printfn "%b" (true >= true)""", "true"
                    """printfn "%b" (false >= true)""", "false"

                    // ---- float32 (f): IEEE clt/cgt -------------------------------
                    """printfn "%b" (1.5f < 2.5f)""", "true"
                    """printfn "%b" (2.5f < 1.5f)""", "false"
                    """printfn "%b" (2.5f > 1.5f)""", "true"
                    """printfn "%b" (2.5f <= 2.5f)""", "true"
                    """printfn "%b" (2.5f <= 1.5f)""", "false"
                    """printfn "%b" (2.5f >= 2.5f)""", "true"
                    """printfn "%b" (1.5f >= 2.5f)""", "false"

                    // ---- float: IEEE clt/cgt (bare `<` is in ComparisonTests) ----
                    """printfn "%b" (2.5 > 1.5)""", "true"
                    """printfn "%b" (1.5 > 2.5)""", "false"
                    """printfn "%b" (2.5 <= 2.5)""", "true"
                    """printfn "%b" (2.5 <= 1.5)""", "false"
                    """printfn "%b" (2.5 >= 2.5)""", "true"
                    """printfn "%b" (1.5 >= 2.5)""", "false"

                    // ---- int64 (L): clt/cgt over 64-bit operands -----------------
                    """printfn "%b" (1L < 2L)""", "true"
                    """printfn "%b" (2L < 1L)""", "false"
                    """printfn "%b" (2L > 1L)""", "true"
                    """printfn "%b" (1L > 2L)""", "false"
                    """printfn "%b" (2L <= 2L)""", "true"
                    """printfn "%b" (2L <= 1L)""", "false"
                    """printfn "%b" (2L >= 2L)""", "true"
                    """printfn "%b" (1L >= 2L)""", "false"
                ] -> test src { runs expected src }
        ]

// The structural *base* clause: an operand type with no `when ^T:` clause falls
// through to `Comparer<^T>.Default.Compare(x, y) <op> 0`. `string` is the
// cleanest probe — a BCL type that implements `IComparable<string>` so
// `Comparer<string>.Default` dispatches to ordinal-ish `String.CompareTo`,
// exercising the fallback with *no* codegen-generated `CompareTo` involved
// (unlike the user-record rows in StructuralComparisonTests).
[<Tests>]
let baseTests =
    testList
        "ComparisonBase"
        [
            for src, expected in
                [
                    """printfn "%b" ("a" < "b")""", "true"
                    """printfn "%b" ("b" < "a")""", "false"
                    """printfn "%b" ("b" > "a")""", "true"
                    """printfn "%b" ("a" <= "a")""", "true"
                    """printfn "%b" ("b" <= "a")""", "false"
                    """printfn "%b" ("b" >= "a")""", "true"
                    """printfn "%b" ("a" >= "b")""", "false"
                ] -> test src { runs expected src }
        ]

// Front-end regression guard (analysis only): the `when 'T: comparison`
// constraint. A generically-used operator infers the
// comparison constraint and type-checks; ordering an un-annotated record — whose
// comparison support is the opt-in default `NoComparison` — is rejected with a
// `comparison` constraint diagnostic. (StructuralComparisonTests proves the
// emit/route side; this is the cheap analysis-side anchor for this package.)
[<Tests>]
let frontEndTests =
    testList
        "ComparisonFrontEnd"
        [
            // A bare generic binding generalizes with `'T: comparison` and is
            // accepted — one row per operator so each constraint path is pinned.
            test "generic `<` infers a comparison constraint and type-checks" { typeChecks "let lt a b = a < b" }
            test "generic `>` infers a comparison constraint and type-checks" { typeChecks "let gt a b = a > b" }
            test "generic `<=` infers a comparison constraint and type-checks" { typeChecks "let le a b = a <= b" }
            test "generic `>=` infers a comparison constraint and type-checks" { typeChecks "let ge a b = a >= b" }

            // Ordering on a value whose type has no comparison support is a
            // constraint error — the negative direction of `when 'T: comparison`.
            test "`<` on an un-annotated record raises a comparison constraint error" {
                failsWith
                    "comparison"
                    (String.concat
                        "\n"
                        [
                            "type Pair = { X: int }"
                            "let a = { X = 1 }"
                            "let b = { X = 2 }"
                            "let r = a < b"
                        ])
            }
        ]
