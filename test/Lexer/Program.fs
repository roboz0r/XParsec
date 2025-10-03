module Program

open Fable.Pyxpecto
#if FABLE_COMPILER
#else
open Expecto
#endif

// This is possibly the most magic used to make this work.
// Js and ts cannot use `Async.RunSynchronously`, instead they use `Async.StartAsPromise`.
// Here we need the transpiler not to worry about the output type.
#if FABLE_COMPILER && !FABLE_COMPILER_JAVASCRIPT && !FABLE_COMPILER_TYPESCRIPT
let (!!) (any: 'a) = any
#endif
#if FABLE_COMPILER && FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Core.JsInterop
#endif

[<EntryPoint>]
let main argv =
#if FABLE_COMPILER
    let all = testList "Lexer.Tests" [ LexerTests.tests ]


    !! Pyxpecto.runTests [||] all
#else
    Tests.runTestsInAssemblyWithCLIArgs [] argv
#endif
