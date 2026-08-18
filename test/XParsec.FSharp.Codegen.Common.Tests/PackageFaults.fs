/// Ok-extraction over the package APIs' `PackageSetFault` results, for fixture setup where a
/// fault is a broken test environment rather than the assertion subject.
module XParsec.FSharp.Codegen.Common.Tests.PackageFaults

open XParsec.FSharp.SemanticAnalysis

/// The `Ok` value, else a raise carrying `label` and the described fault. Raises (never
/// `failtest`), so it is usable in module-level and `lazy` fixture initialisers.
let okOrFail (label: string) (r: Result<'T, PackageSetFault>) : 'T =
    match r with
    | Ok v -> v
    | Error e -> failwithf "%s: %s" label (PackageSetFault.describe e)
