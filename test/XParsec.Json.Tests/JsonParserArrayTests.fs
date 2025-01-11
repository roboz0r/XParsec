module JsonParserArrayTests

open System.Collections.Immutable

open Expecto

open XParsec
open XParsec.JsonArray

[<Tests>]
let tests =
    testList
        "JsonParserArrayTests"
        [
            test "Parser fragments" {
                let input, expected, p =
                    "\"string\"", JsonValue.String "string", JsonParsers.pString |>> JsonValue.String

                let reader = Reader.ofArray (input.ToCharArray()) ()
                let result = p reader

                match result with
                | Ok result -> "" |> Expect.equal expected result.Parsed
                | Error e -> failwithf "%A" e
            }


            test "single values" {
                [ "true", JsonValue.True; "false", JsonValue.False; "null", JsonValue.Null ]
                |> List.iter (fun (input, expected) ->
                    let result = JsonParsers.pJson (Reader.ofArray (input.ToCharArray()) ())

                    match result with
                    | Ok result -> "" |> Expect.equal expected result.Parsed
                    | Error e -> failwithf "%A" e)
            }


            test "numbers" {
                [
                    "123", JsonValue.Number 123.0
                    "123.456", JsonValue.Number 123.456
                    "-123.456", JsonValue.Number -123.456
                    "123e4", JsonValue.Number 123e4
                    "123e-4", JsonValue.Number 123e-4
                    "123e+4", JsonValue.Number 123e4
                    "123.456e4", JsonValue.Number 123.456e4
                    "123.456e-4", JsonValue.Number 123.456e-4
                    "123.456e+4", JsonValue.Number 123.456e4
                    "-123e4", JsonValue.Number -123e4
                    "-123e-4", JsonValue.Number -123e-4
                    "-123e+4", JsonValue.Number -123e4
                    "-123.456e4", JsonValue.Number -123.456e4
                    "-123.456e-4", JsonValue.Number -123.456e-4
                    "-123.456e+4", JsonValue.Number -123.456e4
                ]
                |> List.iter (fun (input, expected) ->
                    let result = JsonParsers.pJson (Reader.ofArray (input.ToCharArray()) ())

                    match result with
                    | Ok result -> "" |> Expect.equal expected result.Parsed
                    | Error e -> failwithf "%A" e)
            }

            test "Strings" {
                [
                    "\"string\"", JsonValue.String "string"
                    "\"\"", JsonValue.String ""
                    "\" \"", JsonValue.String " "
                    "\"\\\"\"", JsonValue.String "\""
                    "\"\\\\\"", JsonValue.String "\\"
                    "\"\\/\"", JsonValue.String "/"
                    "\"\\b\"", JsonValue.String "\b"
                    "\"\\f\"", JsonValue.String "\f"
                    "\"\\n\"", JsonValue.String "\n"
                    "\"\\r\"", JsonValue.String "\r"
                    "\"\\t\"", JsonValue.String "\t"
                    "\"\\u0000\"", JsonValue.String "\u0000"
                    "\"\\u0001\"", JsonValue.String "\u0001"
                    "\"\\u0002\"", JsonValue.String "\u0002"
                    "\"\\uffff\"", JsonValue.String "\uffff"
                ]
                |> List.iter (fun (input, expected) ->
                    let result = JsonParsers.pJson (Reader.ofArray (input.ToCharArray()) ())

                    match result with
                    | Ok result -> "" |> Expect.equal expected result.Parsed
                    | Error e -> failwithf "%A" e)
            }

            test "Arrays" {
                [
                    "[]", JsonValue.Array(ImmutableArray.Empty)
                    "[ ]", JsonValue.Array(ImmutableArray.Empty)
                    "[ \t\r\n    ]", JsonValue.Array(ImmutableArray.Empty)
                    "[1, 2, 3]",
                    JsonValue.Array(
                        ImmutableArray.CreateRange [ JsonValue.Number 1.0; JsonValue.Number 2.0; JsonValue.Number 3.0 ]
                    )
                    "[true, false, null]",
                    JsonValue.Array(ImmutableArray.CreateRange [ JsonValue.True; JsonValue.False; JsonValue.Null ])
                    "[\"string\", 123, true]",
                    JsonValue.Array(
                        ImmutableArray.CreateRange [ JsonValue.String "string"; JsonValue.Number 123.0; JsonValue.True ]
                    )
                ]
                |> List.iter (fun (input, expected) ->
                    let result = JsonParsers.pJson (Reader.ofArray (input.ToCharArray()) ())

                    match result with
                    | Ok result -> "" |> Expect.equal expected result.Parsed
                    | Error e -> failwithf "%A" e)
            }

            test "Objects" {
                [
                    "{}", JsonValue.Object(ImmutableArray.Empty)
                    "{ }", JsonValue.Object(ImmutableArray.Empty)
                    "{ \t\r\n    }", JsonValue.Object(ImmutableArray.Empty)
                    "{\"key\": 123}",
                    JsonValue.Object(
                        ImmutableArray.Create
                            {
                                Name = "key"
                                Value = JsonValue.Number 123.0
                            }
                    )
                    "{\"key\": 123, \"key2\": \"value\"}",
                    JsonValue.Object(
                        ImmutableArray.CreateRange
                            [
                                {
                                    Name = "key"
                                    Value = JsonValue.Number 123.0
                                }
                                {
                                    Name = "key2"
                                    Value = JsonValue.String "value"
                                }
                            ]
                    )
                    "{\"key\": 123, \"key2\": \"value\", \"key3\": true}",
                    JsonValue.Object(
                        ImmutableArray.CreateRange
                            [
                                {
                                    Name = "key"
                                    Value = JsonValue.Number 123.0
                                }
                                {
                                    Name = "key2"
                                    Value = JsonValue.String "value"
                                }
                                {
                                    Name = "key3"
                                    Value = JsonValue.True
                                }
                            ]
                    )
                ]
                |> List.iter (fun (input, expected) ->
                    let result = JsonParsers.pJson (Reader.ofArray (input.ToCharArray()) ())

                    match result with
                    | Ok result -> "" |> Expect.equal expected result.Parsed
                    | Error e -> failwithf "%A" e)
            }

        ]
