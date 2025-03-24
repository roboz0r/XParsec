module XParsec.Client.Serialization

open System
open Thoth.Json
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
        Encode.object
            [
                "id", Encode.string (x.Id.ToString())
                "input", Encode.string x.Input
                "syntax", Encode.string (ParseSyntax.encode x.Syntax)
            ]

    let decoder: Decoder<ParseRequest> =
        Decode.object (fun get ->
            {
                Id = get.Required.Field "id" Decode.guid
                Input = get.Required.Field "input" Decode.string
                Syntax = get.Required.Field "syntax" (Decode.string |> Decode.map ParseSyntax.decode)
            }
        )
