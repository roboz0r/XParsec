module Test

// No bar, with on same line as pattern
let a x =
    try x + 1
    with _ -> 0

// No bar, with on separate line from pattern
let b x =
    try
        x + 1
    with _ ->
        0

// Bar present
let c x =
    try
        x + 1
    with
    | _ ->
        0

// No bar, single-line
let d x =
    try x + 1 with _ -> 0
