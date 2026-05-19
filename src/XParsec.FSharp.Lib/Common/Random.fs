// Transliterated from D:\roboz0r\fsharp\src\FSharp.Core\Random.fs
// upstream commit: ff81858ea7eaa4631e1d1ad6aa4f61d7b8967139
// Near-literal copy per `feedback-fsharpcore-port-transliterate`.

// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Core

open System
open System.Runtime.CompilerServices

[<AbstractClass; Sealed>]
type internal ThreadSafeRandom() =

    [<DefaultValue>]
    [<ThreadStatic>]
    static val mutable private random: Random

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member private Create() =
        ThreadSafeRandom.random <- Random()
        ThreadSafeRandom.random

    // Don't pass the returned Random object between threads
    static member Shared =
        match ThreadSafeRandom.random with
        | null -> ThreadSafeRandom.Create()
        | random -> random
