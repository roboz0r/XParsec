module XParsec.Server.Serialization

open System
open System.Text.Json
open Thoth.Json.Core
open Thoth.Json.System.Text.Json
open XParsec.Shared

module ParseSyntax =
    let encode =
        function
        | ParseSyntax.Json -> "json"

    let decode =
        function
        | "json" -> ParseSyntax.Json
        | x -> failwithf "Unknown ParseSyntax: %s" x

module ParseRequest =
    let encode (x: ParseRequest) =
        Encode.helpers.encodeObject
            [
                "id", Encode.helpers.encodeString (x.Id.ToString())
                "input", Encode.helpers.encodeString x.Input
                "syntax", Encode.helpers.encodeString (ParseSyntax.encode x.Syntax)
            ]

    let decoder: Decoder<ParseRequest> =
        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.guid
                Input = get.Required.Field "input" Decode.string
                Syntax = get.Required.Field "syntax" (Decode.string |> Decode.map ParseSyntax.decode)
            }
        )
