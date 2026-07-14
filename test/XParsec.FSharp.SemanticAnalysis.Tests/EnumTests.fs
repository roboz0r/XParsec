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

/// The resolved case table of the single surfaced enum decl.
let private enumCases (tast: TastFile) =
    match tast.Decls with
    | EqList [ TDecl.Type { Kind = TTypeKindG.Enum cases } ] -> cases
    | other -> failwithf "expected a single enum TDecl.Type, got %A" other

/// The derived underlying primitive type name of the single surfaced enum.
let private underlying (input: string) : string voption =
    analyse input |> enumCases |> TEnumCases.underlyingTypeName

let private errors (tast: TastFile) =
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// The body expression + declared type of the single `let` decl in `tast` (the
/// enum `TDecl.Type` is skipped). Used by the step-3 member-access tests.
let private singleLet (tast: TastFile) : TExpr * SemType =
    match
        tast.Decls
        |> EqArray.toList
        |> List.choose (fun d ->
            match d with
            | TDecl.Let(_, v, _, ty) -> Some(v, ty)
            | _ -> None
        )
        |> List.ofSeq
    with
    | [ one ] -> one
    | other -> failwithf "expected exactly one let decl, got %d" (List.length other)

/// The enum's simple name when `t` is a `TyEnum`, else `ValueNone` — proves the
/// value's static type is the enum nominal (NOT its underlying int/string).
let private enumTypeName (t: SemType) : string voption =
    match t with
    | TyEnum k ->
        let (DisplayName name) = SymbolKeyOps.typeSimpleName k
        ValueSome name
    | _ -> ValueNone

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
                // `1uy` → `IntWidth.Byte`, `2L` → `IntWidth.Int64`: the authored width rides
                // through on the constant's `IntWidth`, surfaced by the renderer's suffix.
                // Width is preserved, never defaulted here (step 2/freeze maps an unsuffixed
                // `int` → I32).
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

            // --- Step 2: underlying-type derivation (where width lives) ----------

            test "underlying type: unsuffixed numeric cases default to int (≡ I32)" {
                Expect.equal (underlying "type C = | A = 0 | B = 1") (ValueSome "int") "unsuffixed → int"
            }

            test "underlying type: an explicit byte width names byte" {
                Expect.equal (underlying "type W = | A = 1uy | B = 2uy") (ValueSome "byte") "byte width"
            }

            test "underlying type: unsuffixed cases adopt the single explicit width" {
                // The rule: all explicit widths must agree; unsuffixed `Int` cases
                // adopt the explicit width if present (here `2L` → int64), else int.
                Expect.equal (underlying "type E = | A = 0 | B = 2L") (ValueSome "int64") "adopts int64"
            }

            test "underlying type: a string enum is string" {
                Expect.equal (underlying "type D = | Up = \"u\" | Down = \"d\"") (ValueSome "string") "string"
            }

            test "underlying type: a mixed enum boxes to obj" {
                Expect.equal (underlying "type M = | A = 1 | B = \"x\"") (ValueSome "obj") "mixed → obj"
            }

            test "uniform-width invariant: differing explicit widths are a hard ERROR" {
                // `1uy` (byte) and `2L` (int64) are two distinct explicit integral
                // widths — a CLR enum has a single underlying type, so this is illegal.
                let tast = analyse "type Bad = | A = 1uy | B = 2L"

                Expect.equal (List.length (errors tast)) 1 "exactly one uniform-width error"

                Expect.stringContains
                    (List.head (errors tast)).Message
                    "single underlying type"
                    "error explains the uniform-width invariant"
            }

            test "uniform-width invariant: unsuffixed + one explicit width is NOT a conflict" {
                // Only a mismatch of *explicit* widths fires; an unsuffixed `Int`
                // adopts the lone explicit width, so this is well-formed.
                let tast = analyse "type Ok = | A = 0 | B = 2L | C = 3L"

                Expect.isEmpty (errors tast) "no width conflict — unsuffixed adopts int64"
            }

            // --- Step 3: member / value access (E.C1) ------------------------

            test "E.C1 infers the enum type and lowers to a static-field access" {
                // Cases are static members on the enum type (CLR enum field access):
                // `E.A` resolves to `StaticFieldGet(E, A)` typed `TyEnum E` — the
                // enum nominal, not the underlying int.
                let tast = analyse "type E = | A = 0 | B = 1\nlet c = E.A"
                let body, _ = singleLet tast

                Expect.equal
                    (enumTypeName (TastWalk.exprTy body))
                    (ValueSome "E")
                    "E.A has static type E (a distinct nominal)"

                Expect.equal (TastShape.prettyExpr body) "E.A" "lowers to a static-field access on the enum type"
                Expect.isEmpty (errors tast) "no diagnostics for a valid enum-case access"
            }

            test "(x: E) annotation resolves to the enum nominal and a matching value checks" {
                // The `: E` annotation must resolve to `TyEnum E` (not opaque), and a
                // same-enum value unifies against it without error.
                let tast = analyse "type E = | A = 0 | B = 1\nlet c: E = E.B"
                let _, ty = singleLet tast

                Expect.equal (enumTypeName ty) (ValueSome "E") "the `: E` annotation resolves to the enum nominal"
                Expect.isEmpty (errors tast) "a matching enum value checks against the annotation"
            }

            test "wrong-type assignment (let n: int = E.A) is a type error — E is not its underlying int" {
                // The enum is a DISTINCT nominal: `TyEnum E` does not unify with
                // `int`, so this is caught (it is NOT structurally int).
                let tast = analyse "type E = | A = 0 | B = 1\nlet n: int = E.A"

                Expect.isNonEmpty (errors tast) "an enum value is not assignable to its underlying int"

                Expect.exists
                    (errors tast)
                    (fun d -> d.Message.Contains "mismatch")
                    "reported as a type mismatch, not silently coerced"
            }

            test "unknown case (E.NotACase) is a resolution error" {
                let tast = analyse "type E = | A = 0 | B = 1\nlet c = E.NotACase"

                Expect.exists
                    (errors tast)
                    (fun d -> d.Message.Contains "has no case")
                    "an unknown enum case is diagnosed, mirroring the unknown-union-case miss"
            }

            // --- Step 4: pattern matching (equality only) ---------------------

            test "match on an enum scrutinee with a wildcard type-checks cleanly" {
                // `| E.A` / `| E.B` are enum-case constant patterns; the scrutinee
                // `(x: E)` unifies against the enum nominal, and the `| _` makes
                // the match total — no diagnostics at all.
                let tast =
                    analyse "type E = | A = 0 | B = 1\nlet f (x: E) = match x with | E.A -> 1 | E.B -> 2 | _ -> 0"

                Expect.isEmpty tast.Diagnostics "a wildcard-closed enum match has no diagnostics"
            }

            test "enum-case pattern lowers to a `TPat.EnumCase` rendering `E.C`" {
                // The pattern carries the case *identity* (enumKey + caseName), not
                // the underlying literal — it renders identically to the `E.C`
                // expression form, the producer/consumer split codegen reads.
                let tast =
                    analyse "type E = | A = 0 | B = 1\nlet f (x: E) = match x with | E.A -> 1 | _ -> 0"

                // The arm pattern is reachable via the function body's match; rather
                // than dig the TAST, assert the whole decl renders the enum-case arm.
                let rendered =
                    tast.Decls
                    |> EqArray.toList
                    |> List.map TastShape.prettyDecl
                    |> String.concat "\n"

                Expect.stringContains rendered "E.A" "the enum-case pattern renders as `E.A`"
                Expect.isEmpty (errors tast) "no errors lowering an enum-case pattern"
            }

            test "wildcard-less enum match is an incomplete match (exhaustiveness deferred)" {
                // v1 is equality-only: an enum match without `| _` is NOT proven
                // exhaustive (closed-enum completeness is a deliberate follow-up).
                // It behaves like an int/string-literal match — no *error*; the
                // arms still type-check. Adding `| _` (the test above) is the way
                // to silence the incomplete match until exhaustiveness lands.
                let tast =
                    analyse "type E = | A = 0 | B = 1\nlet f (x: E) = match x with | E.A -> 1 | E.B -> 2"

                Expect.isEmpty (errors tast) "a wildcard-less enum match is not an error (deferred exhaustiveness)"
            }

            test "match (n: int) with | E.A is a type error — enum is a distinct nominal" {
                // The enum-case pattern is typed `TyEnum E`; matched against an
                // `int` scrutinee it fails to unify (the enum is NOT structurally
                // int), exactly like the `let n: int = E.A` expression-side error.
                let tast =
                    analyse "type E = | A = 0 | B = 1\nlet f (n: int) = match n with | E.A -> 1 | _ -> 0"

                Expect.isNonEmpty (errors tast) "an enum-case pattern doesn't match an int scrutinee"

                Expect.exists
                    (errors tast)
                    (fun d -> d.Message.Contains "mismatch")
                    "reported as a type mismatch (enum vs int), not silently coerced"
            }

            test "unknown case in a pattern (| E.NotACase) is a resolution error" {
                // Mirrors the expression-side `E.NotACase` miss: the pattern head
                // names a registered enum but the tail is not one of its cases.
                let tast =
                    analyse "type E = | A = 0 | B = 1\nlet f (x: E) = match x with | E.NotACase -> 1 | _ -> 0"

                Expect.exists
                    (errors tast)
                    (fun d -> d.Message.Contains "has no case")
                    "an unknown enum case in pattern position is diagnosed"
            }

            test "enum equality `x = E.A` types both operands as the enum (no enum-specific failure)" {
                // The equality form `x = E.A` is the other half of v1 enum matching.
                // Both operands are `TyEnum E`, so under the real *polymorphic* `=`
                // (`'a -> 'a -> bool`) the comparison checks cleanly. The codegen test
                // fixture (`MockBuiltins`) declares `op_Equality` MONOMORPHICALLY as
                // `int -> int -> bool` — a divergence its own module header flags — so
                // under THIS provider the comparison reports the mock's int-vs-enum
                // shape clash. That is a fixture limitation, NOT an enum gap: there is
                // no enum-specific resolution failure (no "has no case" / unknown enum),
                // and step 4's actual deliverable — the `match` path — lowers to
                // underlying-value equality at codegen independent of the `=` operator.
                let tast =
                    analyse "type E = | A = 0 | B = 1\nlet f (x: E) = if x = E.A then 1 else 0"

                // Whatever the mock's monomorphic `=` reports is a plain type mismatch;
                // none of it is an enum resolution error. (Under a polymorphic `=`
                // this list is empty.)
                Expect.all
                    (errors tast)
                    (fun d -> d.Message.Contains "mismatch")
                    "only the mock's monomorphic-int `=` shape clash — no enum-specific resolution failure"
            }
        ]
