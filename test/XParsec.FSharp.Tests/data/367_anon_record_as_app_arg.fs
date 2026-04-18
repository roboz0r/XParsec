// Anonymous record literal `{| ... |}` used as a whitespace-application
// argument, e.g. `ignore {| A = 1 |}` or `printfn "%A" {| A = 1 |}`. Parallels
// the inline-IL fix: the atomic-dispatch already handles `{|`, but the
// application-arg recognizer must also accept it as a valid argument opener.
//
// Exercised forms:
//   1. Simple single-arg application: `ignore {| ... |}`
//   2. Prefix-operator application:    `not {| Ok = false |}.Ok`
//   3. Multi-arg whitespace app:       `f x {| K = 1 |}`

let ignoreAnon () =
    ignore {| Kind = "start"; Count = 0 |}

let negated (f: unit -> {| Ok: bool |}) =
    not (f ()).Ok

let twoArgs f x =
    f x {| Tag = "y"; Value = 1 |}
