module CLArgsTests

open System

open Expecto

open XParsec
open XParsec.CLArgs

type TestOptions =
    | Flag1
    | Flag2
    | Setting of string
    | BoolSetting of bool

let testParser options (input, expected) =
    let parser = options |> CLParser.ofOptions

    let reader = Reader.ofArray input ()

    match parser reader with
    | Ok result -> "" |> Expect.equal result.Parsed expected
    | Error e -> failwithf "%A" e

let testMany options inputCases =
    let test = testParser options
    inputCases |> List.iter test


[<Tests>]
let tests =
    testList
        "CLArgsParsersTests"
        [
            test "Parse Flags" {
                let cases =
                    [
                        [||], imm { }
                        [| "--flag1" |], imm { Flag1 }
                        [| "--flag2" |], imm { Flag2 }

                        [| "--flag1"; "--flag2" |],
                        imm {
                            Flag1
                            Flag2
                        }
                    ]

                testMany [ CLFlag("flag1", Flag1, None); CLFlag("flag2", Flag2, None) ] cases
            }

            test "Parse Settings" {
                let cases =
                    [
                        [||], imm { }
                        [| "--setting"; "value" |], imm { Setting "value" }

                        [| "--setting"; "value"; "--setting"; "value" |],
                        imm {
                            Setting "value"
                            Setting "value"
                        }
                    ]

                testMany [ CLSetting("setting", Setting, None, false) ] cases
            }

            test "Parse Settings Equals Assignment" {
                let cases =
                    [
                        [||], imm { }
                        [| "--setting=value" |], imm { Setting "value" }

                        [| "--setting=value"; "--setting=value" |],
                        imm {
                            Setting "value"
                            Setting "value"
                        }
                    ]

                testMany [ CLSetting("setting", Setting, None, true) ] cases
            }

            test "Parse Bool Settings" {
                let cases =
                    [
                        [||], imm { }
                        [| "--setting"; "true" |], imm { BoolSetting true }

                        [| "--setting"; "true"; "--setting"; "false" |],
                        imm {
                            BoolSetting true
                            BoolSetting false
                        }
                        [| "--setting=true"; "--setting=false" |],
                        imm {
                            BoolSetting true
                            BoolSetting false
                        }

                        [| "--setting"; "true"; "--setting"; "false"; "-b"; "true"; "-b=false" |],
                        imm {
                            BoolSetting true
                            BoolSetting false
                            BoolSetting true
                            BoolSetting false
                        }
                    ]

                testMany [ CLBoolSetting("setting", BoolSetting, Some 'b', true) ] cases
            }
        ]
