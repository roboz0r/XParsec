module CLArgsTests

open System
open System.Collections.Generic
open System.Collections.Immutable

open Expecto

open XParsec
open XParsec.CLArgs

type TestOptions =
    | Flag1
    | Flag2
    | Setting of string
    | BoolSetting of bool

let testParser options (input, expectedParsed, expectedUnparsed) =
    let parser = options |> CLParser.ofOptions

    let reader = Reader.ofArray input ()

    match parser reader with
    | Ok result ->
        "" |> Expect.equal result.Parsed.Options expectedParsed
        "" |> Expect.equal result.Parsed.Unparsed expectedUnparsed
    | Error e -> failwithf "%A" e

let testMany options inputCases =
    let test = testParser options
    inputCases |> List.iter test


let listComparer =
    { new IComparer<string list> with
        member this.Compare(x, y) =
            match x, y with
            | [], [] -> 0
            | [], _ -> 1
            | _, [] -> -1
            | x0 :: xs, y0 :: ys ->
                match String.Compare(x0, y0) with
                | 0 -> this.Compare(xs, ys)
                | cmp -> cmp
    }


[<Tests>]
let tests =
    testList
        "CLArgsParsersTests"
        [
            test "Parse Flags" {
                let cases =
                    [
                        [||], imm { }, imm { }
                        [| "--flag1" |], imm { Flag1 }, imm { }
                        [| "--flag2" |], imm { Flag2 }, imm { }

                        [| "--flag1"; "--flag2" |],
                        imm {
                            Flag1
                            Flag2
                        },
                        imm { }

                        [| "--flag3" |], imm { }, imm { "--flag3" }

                        [| "--flag1"; "--flag2"; "--flag3" |],
                        imm {
                            Flag1
                            Flag2
                        },
                        imm { "--flag3" }

                        [| "--flag1"; "--flag4"; "--flag2"; "--flag3" |],
                        imm {
                            Flag1
                            Flag2
                        },
                        imm {
                            "--flag4"
                            "--flag3"
                        }

                    ]

                testMany [ CLFlag("flag1", Flag1, None, ""); CLFlag("flag2", Flag2, None, "") ] cases
            }

            test "Parse Settings" {
                let cases =
                    [
                        [||], imm { }, imm { }
                        [| "--setting"; "value" |], imm { Setting "value" }, imm { }

                        [| "--setting"; "value"; "--setting"; "value" |],
                        imm {
                            Setting "value"
                            Setting "value"
                        },
                        imm { }

                        [| "--flag3" |], imm { }, imm { "--flag3" }

                        [| "--setting1"; "value" |],
                        imm { },
                        imm {
                            "--setting1"
                            "value"
                        }
                    ]

                testMany [ CLSetting("setting", Setting, None, false, "") ] cases
            }

            test "Parse Settings Equals Assignment" {
                let cases =
                    [
                        [||], imm { }, imm { }
                        [| "--setting=value" |], imm { Setting "value" }, imm { }

                        [| "--setting=value"; "--setting=value" |],
                        imm {
                            Setting "value"
                            Setting "value"
                        },
                        imm { }
                    ]

                testMany [ CLSetting("setting", Setting, None, true, "") ] cases
            }

            test "Parse Bool Settings" {
                let cases =
                    [
                        [||], imm { }, imm { }
                        [| "--setting"; "true" |], imm { BoolSetting true }, imm { }

                        [| "--setting"; "true"; "--setting"; "false" |],
                        imm {
                            BoolSetting true
                            BoolSetting false
                        },
                        imm { }
                        [| "--setting=true"; "--setting=false" |],
                        imm {
                            BoolSetting true
                            BoolSetting false
                        },
                        imm { }

                        [| "--setting"; "true"; "--setting"; "false"; "-b"; "true"; "-b=false" |],
                        imm {
                            BoolSetting true
                            BoolSetting false
                            BoolSetting true
                            BoolSetting false
                        },
                        imm { }
                    ]

                testMany [ CLBoolSetting("setting", BoolSetting, Some 'b', true, "") ] cases
            }

            ftest "Sort command list" {
                let cases =
                    [
                        [ "a"; "a" ]
                        [ "a"; "b"; "c" ]
                        [ "a"; "b" ]
                        [ "a" ]
                        [ "b"; "a"; "a" ]
                        [ "b"; "a"; "c" ]
                        [ "b"; "a"; "d" ]
                        [ "b"; "a" ]
                        [ "c"; "a"; "b" ]
                        [ "c"; "b"; "a" ]
                        []
                    ]

                let invertComparer =
                    { new IComparer<string list> with
                        member this.Compare(x, y) = -listComparer.Compare(x, y)
                    }

                let sorted = cases |> List.sortWith (fun x y -> listComparer.Compare(x, y))
                let sorted2 = cases |> List.sortWith (fun x y -> invertComparer.Compare(x, y))

                "" |> Expect.equal sorted cases
                "" |> Expect.equal sorted2 (List.rev cases)
            }
        ]
