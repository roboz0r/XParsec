module XParsec.FSharp.Codegen.Js.Tests.DynamicTypeTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// TS `any` arrives F#-side as the opaque `dynamic` JS intrinsic, with no special
// unify/subsume behaviour. Its ONLY capability is the `?` operator, and there are NO
// assignability edges: a value cannot silently enter or leave `dynamic`.

let private stringT = named "string"

let private dynManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "dynlib"
        Version = None
        Exports =
            [
                Schema.Export.Class(
                    "D",
                    0,
                    [
                        // `mkObj : () -> any` — a nested object; `?`-chain + write target.
                        staticMethod' "mkObj" [ sig0 dynamic ]
                        // `useAny : (v: any) -> string` — an `any` PARAM.
                        staticMethod' "useAny" [ sig1 "v" dynamic stringT ]
                    ],
                    [],
                    Schema.ImportShape.Named,
                    []
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private dynContract = contractTs dynManifest

let private dynProvider: IExternalSymbolProvider = dynContract.Provider

/// Hand-authored JS runtime backing the `dynlib` manifest. `mkObj` returns a nested
/// object so `d?foo?bar` (computed chain) and `d?bar <- v` (write) work; `useAny`
/// stringifies its argument.
let private dynRuntimeSource =
    """export function D_mkObj() { return { foo: { bar: "chain" }, n: 7 }; }
export function D_useAny(v) { return "A:" + v; }
"""

/// The WARNING diagnostics from analysing `input`. The implicit-`dynamic`-escape sweep raises
/// `Severity.Warning`, which `analyseWith` filters out, so it needs its own accessor.
let private warningsWith (input: string) : Diagnostic list =
    let lexed, file = parseFile input

    let tast = Pipeline.analyseSem dynProvider (Hashing.originSourceOfText lexed) file

    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Warning)

let private emitWithDyn (input: string) : string =
    emitWith dynContract (Map.ofList [ "dynlib", JsPackageOutput.rootModule "dynlib.mjs" dynRuntimeSource ]) false input

[<Tests>]
let tests =
    testList
        "DynamicType"
        [
            test "`d?foo` unconstrained defaults to `dynamic`; a chain stays dynamic" {
                // `y = d?foo` fires the `default ^TResult : dynamic`; `d?a?b` re-applies
                // it, so both admit further `?`-access without error.
                let errs =
                    analyseWith
                        dynProvider
                        (String.concat
                            "\n"
                            [
                                "let d = D.mkObj()"
                                "let y = d?foo"
                                "let z = d?foo?bar"
                                "let w = y?again" // y is dynamic → `?` still valid
                                "ignore (z, w)"
                                ""
                            ])

                Expect.isEmpty errs (sprintf "unconstrained `?` should default to dynamic: %A" errs)
            }

            test "`let n : int = d?foo` pins `^TResult` to int (the default does NOT fire)" {
                // A pinned context unifies `^TResult` to `int` BEFORE defaulting, so `n : int`
                // type-checks and `n + 1` is int arithmetic.
                let errs =
                    analyseWith
                        dynProvider
                        (String.concat
                            "\n"
                            [ "let d = D.mkObj()"; "let n : int = d?n"; "let m = n + 1"; "ignore m"; "" ])

                Expect.isEmpty errs (sprintf "`?` should target-type to int: %A" errs)
            }

            test "dotted `.foo` on a dynamic value is a type ERROR (`.` is statically-known only)" {
                let errs =
                    analyseWith
                        dynProvider
                        (String.concat "\n" [ "let d = D.mkObj()"; "let a = d.foo"; "ignore a"; "" ])

                Expect.isNonEmpty errs "dotted access on a dynamic must error"
            }

            test "`let n : int = d` is a type ERROR (no assignability edge out of dynamic)" {
                let errs =
                    analyseWith
                        dynProvider
                        (String.concat "\n" [ "let d = D.mkObj()"; "let n : int = d"; "ignore n"; "" ])

                Expect.isNonEmpty errs "a bare dynamic must not assign to a concrete type"
            }

            test "`dynamic someInt` enters dynamic (emits the value unchanged) and `?`-read/write round-trip" {
                // `dynamic 7` enters via the identity `retype`; `d?foo?bar` reads the computed
                // chain `d["foo"]["bar"]`; `d?bar <- v` writes; `useAny` takes an `any` param.
                let program =
                    String.concat
                        "\n"
                        [
                            "let d = D.mkObj()"
                            "d?bar <- \"written\""
                            // The `?`-read target-types OUT to `string`, so the default does
                            // not fire; the annotation is required because no assignability
                            // edge carries a bare `dynamic` into `%s`.
                            "let chained : string = d?foo?bar"
                            "let w : string = d?bar"
                            "let entered = dynamic 7"
                            "let r = D.useAny(entered)"
                            "printfn \"%s %s %s\" chained w r"
                            ""
                        ]

                let js = emitWithDyn program

                // Computed-member CHAIN read (bracket form, not dotted). The `$N`-template
                // expander wraps each operand in parens: `d["foo"]["bar"]` → `(d)[("foo")]…`.
                Expect.stringContains js "[(\"foo\")]" "dynamic `?`-chain emits computed member access"
                // Computed-member WRITE (`$0[$1] = $2` → `…[…] = (…)`).
                Expect.stringContains js "] = (" "dynamic `?`-write emits a computed-member assignment"
                Expect.stringContains js "const entered = 7" "`dynamic` enter emits the value verbatim"

                expectNodeOutput
                    "dynamic-type"
                    [ "dynamic-type.mjs", js; "dynlib.mjs", dynRuntimeSource ]
                    "chain written A:7"
            }

            // A `?`-result pinned to a concrete type by CONTEXT (the `default : dynamic` never
            // fired) is an unchecked assertion, so it warns. Suppressed ONLY by an ascription
            // directly on the `?` expression: "name the type at the escape point."

            test "`d?foo + 1` implicitly escapes `dynamic` to int → warns" {
                let ws =
                    warningsWith (String.concat "\n" [ "let d = D.mkObj()"; "let m = d?n + 1"; "ignore m"; "" ])

                Expect.isNonEmpty ws "an implicit `dynamic`→int escape must warn"

                Expect.stringContains
                    (ws |> List.map (fun d -> d.Message) |> String.concat "\n")
                    "escape from 'dynamic'"
                    "the warning names the dynamic escape"
            }

            test "unconstrained `d?foo` stays dynamic → no warning" {
                // The default fires; nothing escaped, so nothing to warn about.
                let ws =
                    warningsWith (String.concat "\n" [ "let d = D.mkObj()"; "let y = d?foo"; "ignore y"; "" ])

                Expect.isEmpty ws (sprintf "a dynamic-valued `?` must not warn: %A" ws)
            }

            test "a `?`-chain `d?a?b` stays dynamic → no warning" {
                let ws =
                    warningsWith (String.concat "\n" [ "let d = D.mkObj()"; "let z = d?foo?bar"; "ignore z"; "" ])

                Expect.isEmpty ws (sprintf "a `?`-chain must not warn: %A" ws)
            }

            test "an ascription ON the `?` expression `(d?foo : int)` suppresses the warning" {
                let ws =
                    warningsWith (String.concat "\n" [ "let d = D.mkObj()"; "let m = (d?n : int) + 1"; "ignore m"; "" ])

                Expect.isEmpty ws (sprintf "`(d?foo : int)` names the type at the escape point → no warn: %A" ws)
            }

            test "an annotation on the BINDING `let n : int = d?foo` still warns (not on the `?` node)" {
                // The syntactic rule: only an ascription on the `?` expression itself
                // suppresses. `let n : int = …` nudges toward `let n = (d?n : int)`.
                let ws =
                    warningsWith (String.concat "\n" [ "let d = D.mkObj()"; "let n : int = d?n"; "ignore n"; "" ])

                Expect.isNonEmpty ws "a binding-level annotation does not suppress the escape warning"
            }

            test "`d?foo <- v` (the setter) is unit, never an escape → no warning" {
                let ws = warningsWith (String.concat "\n" [ "let d = D.mkObj()"; "d?bar <- 3"; "" ])

                Expect.isEmpty ws (sprintf "a dynamic write is not an escape: %A" ws)
            }

            // `retype`, the general erasing reinterpret, is public but lives in the
            // NON-auto-opened `Vesper.Unsafe`, so the unchecked cast is reachable only after an
            // explicit `open` and never ambiently in scope.

            test "`retype` is reachable through `open Vesper.Unsafe`" {
                let errs =
                    analyseWith
                        dynProvider
                        (String.concat "\n" [ "open Vesper.Unsafe"; "let s : string = retype 7"; "ignore s"; "" ])

                Expect.isEmpty errs (sprintf "`open Vesper.Unsafe` should bring `retype` into scope: %A" errs)
            }

            test "`retype` is NOT ambient — bare `retype` without the `open` does not resolve" {
                // `retype` is not in the auto-opened `DynamicOperators`, so an unqualified use
                // with no `open Vesper.Unsafe` does not resolve.
                let errs =
                    analyseWith dynProvider (String.concat "\n" [ "let s : string = retype 7"; "ignore s"; "" ])

                Expect.isNonEmpty errs "bare `retype` must not resolve without `open Vesper.Unsafe`"
            }
        ]
