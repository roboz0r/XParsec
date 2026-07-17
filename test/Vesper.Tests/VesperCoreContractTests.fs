module Vesper.Tests.VesperCoreContractTests

// Prove the Vesper.Core contract (`.fsi`) and implementation (`.fs`) files are
// consumable by XParsec.FSharp.
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
        "prim-types-bigint.fsi"
        "prim-types-nativeint.fsi"
        "prim-types-nd-array.fsi"
        "prim-types-attr.fsi"
        "compiler-attributes.fsi"
        "core-types.fsi"
        "capabilities.fsi"
        "ops-platform.fsi"
        "ops-std.fsi"
        "int-comparison.fsi"
    ]

/// Impl `.fs` files — our-backend target source, one companion per `prim-types-*`
/// contract (each binds its extern types to `(# "..." #)` intrinsics).
/// `ops-platform.fs` carries the `hash` inline body the codegen inline-body
/// loader reads. The cons-list (`list.fs`) moved to
/// the standalone `Vesper.List` package (one package per type) — see
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
        "prim-types-bigint.fs"
        "prim-types-nativeint.fs"
        "prim-types-nd-array.fs"
        "prim-types-attr.fs"
        "core-types.fs"
        "capabilities.fs"
        "ops-platform.fs"
        "ops-std.fs"
        "int-comparison.fs"
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
/// (one package per type). Same golden-file bar as the core contract: the
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
/// (one package per type). Same golden-file bar as the core contract: the
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

/// Vesper.Choice — standalone package (one package per type) mirroring
/// Vesper.Result: the `Choice<'T1, 'T2>` struct DU consumed by `set.fs`'s
/// `partitionWith`. Same golden-file bar: the `.fsi` and `.fs` must parse with
/// zero recovery diagnostics.
[<Tests>]
let vesperChoiceContractTests =
    let choicePath fileName = vesperPath "Vesper.Choice" fileName

    testList
        "VesperChoiceContract"
        [
            test "Parsing choice.fsi" { testParseSignatureFile (choicePath "choice.fsi") }
            test "Parsing choice.fs" { testParseFile (choicePath "choice.fs") }
        ]

/// Vesper.Array — standalone package (one package per module) adding the `Array`
/// module (`fold` / `zeroCreate`) over the intrinsic `'T[]` type, consumed by
/// `set.fs`'s `toArray` / `ofArray`. Same golden-file bar: the `.fsi` and `.fs`
/// must parse with zero recovery diagnostics.
[<Tests>]
let vesperArrayContractTests =
    let arrayPath fileName = vesperPath "Vesper.Array" fileName

    testList
        "VesperArrayContract"
        [
            test "Parsing array.fsi" { testParseSignatureFile (arrayPath "array.fsi") }
            test "Parsing array.fs" { testParseFile (arrayPath "array.fs") }
        ]

/// Vesper.Seq — standalone package (one package per module) adding the `Seq`
/// module (`fold` / `reduce` / `truncate` / `toArray`) over `seq<'T>`, consumed by
/// `set.fs`'s `Set.Union` / `Set.Intersection` / line 961 `Seq.truncate`. Same
/// golden-file bar: the `.fsi` and `.fs` must parse with zero recovery diagnostics.
[<Tests>]
let vesperSeqContractTests =
    let seqPath fileName = vesperPath "Vesper.Seq" fileName

    testList
        "VesperSeqContract"
        [
            test "Parsing seq.fsi" { testParseSignatureFile (seqPath "seq.fsi") }
            test "Parsing seq.fs" { testParseFile (seqPath "seq.fs") }
            test "Parsing struct-seq.fsi" { testParseSignatureFile (seqPath "struct-seq.fsi") }
            test "Parsing struct-seq.fs" { testParseFile (seqPath "struct-seq.fs") }
        ]

/// Vesper.Comparison — the ordering family relocated out of Vesper.Core's
/// `ops-platform.fsi`. Same golden-file bar: `comparison.fsi`
/// must parse with zero recovery diagnostics; the impl `comparison.fs` (the four
/// operator bodies as static-optimization over inline IL)
/// must parse too.
[<Tests>]
let vesperComparisonContractTests =
    let comparisonPath fileName = vesperPath "Vesper.Comparison" fileName

    testList
        "VesperComparisonContract"
        [
            test "Parsing comparison.fsi" { testParseSignatureFile (comparisonPath "comparison.fsi") }
            test "Parsing comparison.fs" { testParseFile (comparisonPath "comparison.fs") }
        ]

[<Tests>]
let vesperListContractTests =
    let listPath fileName = vesperPath "Vesper.List" fileName

    testList
        "VesperListContract"
        [
            test "Parsing list.fsi" { testParseSignatureFile (listPath "list.fsi") }
            test "Parsing list.fs" { testParseFile (listPath "list.fs") }
        ]

/// Vesper.Set — standalone package (one package per type). Both `set.fsi`
/// (contract) and `set.fs` (impl) are verbatim copies of FSharp.Core's
/// `set.fsi`/`set.fs` with only the namespace patched to `Vesper.Collections`
/// and the `Microsoft.FSharp.*` opens dropped. Same golden-file bar: each must
/// parse with zero recovery diagnostics.
[<Tests>]
let vesperSetContractTests =
    let setPath fileName = vesperPath "Vesper.Set" fileName

    testList
        "VesperSetContract"
        [
            test "Parsing set.fsi" { testParseSignatureFile (setPath "set.fsi") }
            test "Parsing set.fs" { testParseFile (setPath "set.fs") }
        ]
