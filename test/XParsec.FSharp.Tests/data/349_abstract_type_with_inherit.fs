// Abstract type (no implementation) whose last member is `inherit IDisposable`
// rather than an `abstract` member. No explicit `interface ... with` marker.
// Affects: ilread.fs
open System

type ILModuleReader =
    abstract ILModuleDef: int
    abstract ILAssemblyRefs: int list

    // ILModuleReader objects only need to be explicitly disposed if memory
    // mapping is used, i.e. reduceMemoryUsage = false
    inherit IDisposable

[<Sealed>]
type Impl() =
    interface ILModuleReader with
        member _.ILModuleDef = 0
        member _.ILAssemblyRefs = []
        member _.Dispose() = ()
