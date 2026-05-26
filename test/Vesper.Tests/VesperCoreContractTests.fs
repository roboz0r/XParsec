module Vesper.Tests.VesperCoreContractTests

// Migration step 1 of minimal-core-lib-plan.md: prove the Vesper.Core contract
// (`.fsi`) and implementation (`.fs`) files are consumable by XParsec.FSharp.
// Uses the golden-file machinery pointed at the real sources in
// `src/Vesper.Core/`, so each `.parsed` snapshot lands next to its source. First
// run creates the golden (test fails locally / skips on CI); review it (confirm
// the declarations come out and there are no recovery diagnostics), commit it,
// and every later run asserts the parse against it automatically.

open System.IO

open Expecto

/// Path to a `src/<package>/<fileName>` source file, relative to this test
/// file's compile-time location (repo-root `src/`).
let private vesperPath (package: string) (fileName: string) =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", package, fileName)

let private vesperCorePath (fileName: string) = vesperPath "Vesper.Core" fileName

/// Contract `.fsi` files, in `manifest.toml` compile order.
let private contractFiles =
    [
        "prim-types-min.fsi"
        "prim-types-int.fsi"
        "prim-types-float.fsi"
        "prim-types-string.fsi"
        "prim-types-object.fsi"
        "prim-types-exn.fsi"
        "prim-types-decimal.fsi"
        "prim-types-nativeint.fsi"
        "prim-types-nd-array.fsi"
        "prim-types-attr.fsi"
        "compiler-attributes.fsi"
        "core-types.fsi"
        "ops-platform.fsi"
        "ops-std.fsi"
    ]

/// Impl `.fs` files — our-backend target source, one companion per `prim-types-*`
/// contract (each binds its extern types to `(# "..." #)` intrinsics).
/// `ops-platform.fs` carries the `hash` inline body the codegen inline-body
/// loader reads (milestone M). The cons-list (`list-min.fs` / `List.fs`) moved to
/// the standalone `Vesper.List` package (package-split-plan PS1) — see
/// `vesperListContractTests` below.
let private implFiles =
    [
        "prim-types-min.fs"
        "prim-types-int.fs"
        "prim-types-float.fs"
        "prim-types-string.fs"
        "prim-types-object.fs"
        "prim-types-exn.fs"
        "prim-types-decimal.fs"
        "prim-types-nativeint.fs"
        "prim-types-nd-array.fs"
        "prim-types-attr.fs"
        "core-types.fs"
        "ops-platform.fs"
    ]

[<Tests>]
let vesperCoreContractTests =
    testList
        "VesperCoreContract"
        [
            for fileName in contractFiles do
                test $"Parsing {fileName}" { testParseSignatureFile (vesperCorePath fileName) }

            for fileName in implFiles do
                test $"Parsing {fileName}" { testParseFile (vesperCorePath fileName) }
        ]

/// Vesper.Option — standalone package carved out of Vesper.Core's core-types
/// (package-split-plan PS1). Same golden-file bar as the core contract: the
/// `.fsi` and `.fs` must parse with zero recovery diagnostics.
[<Tests>]
let vesperOptionContractTests =
    let optionPath fileName = vesperPath "Vesper.Option" fileName

    testList
        "VesperOptionContract"
        [
            test "Parsing option.fsi" { testParseSignatureFile (optionPath "option.fsi") }
            test "Parsing option.fs" { testParseFile (optionPath "option.fs") }
        ]

/// Vesper.Result — standalone package carved out of Vesper.Core's core-types
/// (package-split-plan PS1). Same golden-file bar as the core contract: the
/// `.fsi` and `.fs` must parse with zero recovery diagnostics.
[<Tests>]
let vesperResultContractTests =
    let resultPath fileName = vesperPath "Vesper.Result" fileName

    testList
        "VesperResultContract"
        [
            test "Parsing result.fsi" { testParseSignatureFile (resultPath "result.fsi") }
            test "Parsing result.fs" { testParseFile (resultPath "result.fs") }
        ]

/// Vesper.Comparison — the ordering family relocated out of Vesper.Core's
/// `ops-platform.fsi` (operators-plan.md O2). Same golden-file bar: `comparison.fsi`
/// must parse with zero recovery diagnostics.
[<Tests>]
let vesperComparisonContractTests =
    let comparisonPath fileName = vesperPath "Vesper.Comparison" fileName

    testList
        "VesperComparisonContract"
        [
            test "Parsing comparison.fsi" { testParseSignatureFile (comparisonPath "comparison.fsi") }
        ]

/// Vesper.List — standalone package carved out of Vesper.Core's core-types
/// (package-split-plan PS1). `list.fsi` is the contract; `list-min.fs` is the
/// compiled cons-list (named `Nil`/`Cons` cases) the package's `Vesper.List.dll`
/// builds from; `List.fs` is the verbatim `[]`/`::` + `module List` target. Same
/// golden-file bar: each must parse with zero recovery diagnostics.
[<Tests>]
let vesperListContractTests =
    let listPath fileName = vesperPath "Vesper.List" fileName

    testList
        "VesperListContract"
        [
            test "Parsing list.fsi" { testParseSignatureFile (listPath "list.fsi") }
            test "Parsing list-min.fs" { testParseFile (listPath "list-min.fs") }
            test "Parsing List.fs" { testParseFile (listPath "List.fs") }
        ]
