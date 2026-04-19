module XParsec.FSharp.Benchmarks.Fixtures

open System.IO

let private fixturesDir = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")

let private read name =
    let text = File.ReadAllText(Path.Combine(fixturesDir, name))
    text.Replace("\r\n", "\n")

let small = lazy read "small.fs"
let medium = lazy read "medium.fs"
let large = lazy read "large.fs"

type FixtureSize =
    | Small = 0
    | Medium = 1
    | Large = 2

let load (size: FixtureSize) =
    match size with
    | FixtureSize.Small -> small.Value
    | FixtureSize.Medium -> medium.Value
    | FixtureSize.Large -> large.Value
    | s -> failwithf "Unknown fixture size: %A" s
