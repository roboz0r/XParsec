// FSharp.Core/assemblyinfo.fs:1
module Vesper.AssemblyInfo

// The prelude every compilation against this package is written under. Read off the CST
// before any file is analysed, so no `.fsi` half declares them.

[<assembly: AutoOpen("Vesper")>]
[<assembly: AutoOpen("Vesper.Collections")>]
do ()
