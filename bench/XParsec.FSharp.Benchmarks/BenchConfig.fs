module XParsec.FSharp.Benchmarks.BenchConfig

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Toolchains.InProcess.Emit

/// Forces InProcessEmitToolchain for every benchmark so BDN doesn't try to spawn
/// an out-of-process runner. The generated out-of-process csproj inherits this
/// repo's CPM pin (FSharp.Core 8.0.300) which conflicts with FCS's 10.1.202
/// transitive requirement and trips NU1109 on restore.
type InProcessConfig() as this =
    inherit ManualConfig()

    do
        this.AddJob(Job.Default.WithToolchain(InProcessEmitToolchain.Instance))
        |> ignore
