module XParsec.FSharp.Codegen.Js.Tests.UndefinedIdentityTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// `undefined`, `null` and `unit` are THREE distinct type identities under the conditional
// fold's membership test, even though `unit` and `undefined` are repr-coincident on the
// backend (both emit JS `undefined`).

// ─── Schema builders ────────────────────────────────────────────────────────
let private named n = Schema.TypeRef.Named(n, [])
let private unitT = named "unit"

let private cond c e wt wf =
    Schema.TypeRef.Conditional(c, e, wt, wf)

let private param name ty : Schema.Param =
    {
        Name = name
        Type = ty
        Optional = false
        Rest = false
    }

let private methodMem name (sigs: Schema.Signature list) : Schema.Member =
    {
        Name = name
        Kind = Schema.MemberKind.Method
        Type = None
        Signatures = sigs
        Static = false
        Optional = false
    }

// A single-param method whose param type is the ground conditional
// `check extends extend ? int : bool`. The arg type at the call site witnesses the branch: an
// `int` arg admits iff the fold picked `int`, a `bool` arg iff it picked `bool`.
let private condMethod name check extend : Schema.Member =
    methodMem
        name
        [
            {
                TypeParams = 0
                TypeParamBounds = []
                Params = [ param "x" (cond check extend (named "int") (named "bool")) ]
                Returns = unitT
            }
        ]

let private idManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "idlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Id",
                    0,
                    [
                        // undefined ⊑ undefined  → int branch (identity holds)
                        condMethod "uu" (named "undefined") (named "undefined")
                        // undefined ⊑ unit  → false → bool branch
                        condMethod "uUnit" (named "undefined") unitT
                        // unit ⊑ undefined  → false → bool branch (converse)
                        condMethod "unitU" unitT (named "undefined")
                        // null ⊑ null  → int branch (identity holds)
                        condMethod "nn" (named "null") (named "null")
                        // null ⊑ unit  → false → bool branch
                        condMethod "nUnit" (named "null") unitT
                        // unit ⊑ unit  → int branch (unit's own identity intact)
                        condMethod "unitUnit" unitT unitT
                    ],
                    [],
                    []
                )
                Schema.Export.Function(
                    "makeId",
                    [
                        {
                            TypeParams = 0
                            TypeParamBounds = []
                            Params = []
                            Returns = named "Id"
                        }
                    ],
                    Schema.ImportShape.Named
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private idProvider: IExternalSymbolProvider = stackTs idManifest

let private prelude = "let e : Id = makeId()"

let private analyse (body: string) : Diagnostic list =
    let input = prelude + "\n" + body + "\n"
    let lexed, file = parseFile input

    let tast = Pipeline.analyseSem idProvider (Hashing.lexedFileOfText lexed) file

    tast.Diagnostics |> Diagnostic.errors

[<Tests>]
let tests =
    testList
        "UndefinedIdentity"
        [
            // ── undefined is its own identity, DISTINCT from unit ──
            test "undefined extends undefined folds TRUE (int branch)" {
                Expect.isEmpty (analyse "e.uu(5)") "undefined ⊑ undefined holds → int param"
                Expect.isNonEmpty (analyse "e.uu(true)") "a bool arg must not match the int branch"
            }

            test "undefined extends unit folds FALSE (bool branch) — undefined ≠ unit" {
                Expect.isEmpty (analyse "e.uUnit(true)") "undefined ⊑ unit is FALSE → bool param"
                Expect.isNonEmpty (analyse "e.uUnit(5)") "an int arg proves undefined⊑unit did NOT hold"
            }

            test "unit extends undefined folds FALSE (bool branch) — the converse too" {
                Expect.isEmpty (analyse "e.unitU(true)") "unit ⊑ undefined is FALSE → bool param"
                Expect.isNonEmpty (analyse "e.unitU(5)") "an int arg proves unit⊑undefined did NOT hold"
            }

            // ── null is its own identity, DISTINCT from unit ──
            test "null extends null folds TRUE (int branch)" {
                Expect.isEmpty (analyse "e.nn(5)") "null ⊑ null holds → int param"
                Expect.isNonEmpty (analyse "e.nn(true)") "a bool arg must not match the int branch"
            }

            test "null extends unit folds FALSE (bool branch) — null ≠ unit" {
                Expect.isEmpty (analyse "e.nUnit(true)") "null ⊑ unit is FALSE → bool param"
                Expect.isNonEmpty (analyse "e.nUnit(5)") "an int arg proves null⊑unit did NOT hold"
            }

            // ── unit's own identity is intact ──
            test "unit extends unit folds TRUE (int branch)" {
                Expect.isEmpty (analyse "e.unitUnit(5)") "unit ⊑ unit holds → int param"
                Expect.isNonEmpty (analyse "e.unitUnit(true)") "a bool arg must not match the int branch"
            }
        ]
