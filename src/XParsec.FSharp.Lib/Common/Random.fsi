// Transliterated from D:\roboz0r\fsharp\src\FSharp.Core\Random.fsi
// upstream commit: ff81858ea7eaa4631e1d1ad6aa4f61d7b8967139
// Near-literal copy per `feedback-fsharpcore-port-transliterate`.

namespace Microsoft.FSharp.Core

open System

[<AbstractClass; Sealed>]
type internal ThreadSafeRandom =
    static member Shared: Random
