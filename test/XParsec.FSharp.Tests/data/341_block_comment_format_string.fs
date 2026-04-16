// Block comment containing a multi-line format string with escape sequences.
// Affects: CompilerDiagnostics.fs
let f x =
    match x with
    | 1 -> ()
    (*
          Printf.bprintf os ".\n\n    state = %A\n    token = %A\n    expect %A"
              a
              b
              c
    *)
    | _ -> ()
