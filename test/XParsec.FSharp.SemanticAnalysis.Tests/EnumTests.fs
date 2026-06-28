module XParsec.FSharp.SemanticAnalysis.Tests.EnumTests

// Step 1b: enum case-literal resolution + numeric / string / mixed
// classification + diagnostics. The first coverage to actually drive an enum
// declaration (`type E = | C = v`) through elaboration — `tryEnumType` was
// unexercised by the 1a skeleton. Each test analyses an enum source through the
// full pass pipeline and asserts (a) the resolved case→literal table + derived
// variant via the `TastShape` renderer (`enum<variant> | C = <lit>`), and
// (b) the reported diagnostics (the mixed warning / the illegal-case error).

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

/// The single surfaced enum decl, rendered.
let private enumShape (tast: TastFile) : string =
    match tast.Decls with
    | EqList [ d ] -> TastShape.prettyDecl d
    | other -> failwithf "expected a single enum TDecl.Type, got %A" other

let private errors (tast: TastFile) =
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

let private warnings (tast: TastFile) =
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Warning)

[<Tests>]
let tests =
    testList
        "Enum"
        [
            test "numeric enum: all-int cases classify numeric, no diagnostics" {
                let tast = analyse "type Color = | Red = 0 | Green = 1 | Blue = 2"

                Expect.equal
                    (enumShape tast)
                    "type Color = enum<numeric> | Red = 0 | Green = 1 | Blue = 2"
                    "numeric variant + resolved int values"

                Expect.isEmpty tast.Diagnostics "no diagnostics for a well-formed numeric enum"
            }

            test "numeric enum preserves the authored integral width (suffix)" {
                // `1uy` → `TConstValue.Byte`, `2L` → `TConstValue.Int64`: the
                // authored width rides through on the literal's case, surfaced by
                // the renderer's suffix. Width is preserved, never defaulted here
                // (step 2/freeze maps unsuffixed `Int` → I32).
                let tast = analyse "type Widths = | A = 1uy | B = 2uy"

                Expect.equal
                    (enumShape tast)
                    "type Widths = enum<numeric> | A = 1uy | B = 2uy"
                    "byte width preserved on each case"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "string enum: all-string cases classify string, no diagnostics" {
                let tast = analyse "type Dir = | Up = \"up\" | Down = \"down\""

                Expect.equal
                    (enumShape tast)
                    "type Dir = enum<string> | Up = \"up\" | Down = \"down\""
                    "string variant + resolved string values"

                Expect.isEmpty tast.Diagnostics "no diagnostics for a well-formed string enum"
            }

            test "mixed enum: int + string cases classify mixed and WARN (accepted)" {
                let tast = analyse "type Mix = | A = 1 | B = \"x\""

                Expect.equal
                    (enumShape tast)
                    "type Mix = enum<mixed> | A = 1 | B = \"x\""
                    "mixed variant; both case values still recorded raw"

                Expect.isEmpty (errors tast) "mixed is accepted — no error"
                Expect.equal (List.length (warnings tast)) 1 "exactly one mixed warning"

                Expect.stringContains
                    (List.head (warnings tast)).Message
                    "mixes integer and string"
                    "warning explains the heterogeneity"
            }

            test "illegal case (non-literal expression) is a hard ERROR; siblings survive" {
                let tast = analyse "type Bad = | A = 0 | B = 1 + 1"

                // A resolves; B (a `1 + 1` expression) errors → `<unresolved>`.
                // The enum + the good sibling are still recorded (numeric, derived
                // from the one resolved int case).
                Expect.equal
                    (enumShape tast)
                    "type Bad = enum<numeric> | A = 0 | B = <unresolved>"
                    "bad case renders unresolved; good sibling preserved"

                Expect.equal (List.length (errors tast)) 1 "exactly one error for the bad case"
                Expect.isEmpty (warnings tast) "no warning — this is a reject, not mixed"
            }

            test "illegal case (non-int-non-string literal) is a hard ERROR" {
                // A `float` constant is neither integer nor string — rejected even
                // though it IS a literal. Only int+string heterogeneity is admitted.
                let tast = analyse "type Bad2 = | A = 1.5"

                Expect.equal
                    (enumShape tast)
                    "type Bad2 = enum<?> | A = <unresolved>"
                    "no legal case resolved → variant unknown"

                Expect.equal (List.length (errors tast)) 1 "exactly one error for the float case"
            }

            test "negative int case (`-1`) resolves to a signed integer literal, no diagnostics" {
                // `-1` parses as a unary-minus PrefixApp, not an `Expr.Const`, yet a
                // negative integral enum member is legal and common (`None = -1`).
                let tast = analyse "type E = | A = -1 | B = 0"

                Expect.equal
                    (enumShape tast)
                    "type E = enum<numeric> | A = -1 | B = 0"
                    "negative case resolves to -1; numeric variant"

                Expect.isEmpty tast.Diagnostics "no diagnostics for a negative signed enum case"
            }

            test "negative on an unsigned width (`-1uy`) is a hard ERROR" {
                // A negative value has no unsigned representation — reject the unary
                // minus on an unsigned-width literal (one error, like `1 + 1`).
                let tast = analyse "type Bad3 = | A = -1uy"

                Expect.equal (List.length (errors tast)) 1 "exactly one error for the negative unsigned case"
            }
        ]
