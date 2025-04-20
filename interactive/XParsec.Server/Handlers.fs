module XParsec.Server.Handlers

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Logging

open Thoth.Json.System.Text.Json
open Oxpecker
open XParsec
open XParsec.Shared

open type Microsoft.AspNetCore.Http.TypedResults
open System.Text.Json
open Serialization

#nowarn FS3511 // This state machine is not statically compilable

let parse (ctx: HttpContext) : Task =
    let noInput =
        {
            RequestId = Guid.Empty
            Success = false
            Message = "No input provided"
        }

    task {
        let logger = ctx.GetLogger()

        try
            let! input = ctx.Request.ReadFromJsonAsync<JsonDocument>()

            match input with
            | null -> return! json noInput ctx
            | input ->
                let input = Decode.fromValue ParseRequest.decoder input.RootElement

                match input with
                | Error _ -> return! json noInput ctx
                | Ok input ->
                    logger.LogInformation("Parsed input: {0}", input)

                    if String.IsNullOrWhiteSpace input.Input then
                        logger.LogInformation("No input provided")
                        return! json noInput ctx
                    else
                        let success, msg =
                            match input.Syntax with
                            | ParseSyntax.Json ->
                                let reader = Reader.ofString input.Input ()
                                let pJson = XParsec.Json.JsonParsers.Parser

                                match pJson reader with
                                | Ok value ->
                                    logger.LogInformation("Parsed JSON: {0}", value)
                                    true, "Parsed JSON"
                                | Error msg ->
                                    logger.LogError("Error parsing JSON: {0}", msg)
                                    false, ErrorFormatting.formatStringError input.Input msg

                        let resp =
                            {
                                RequestId = input.Id
                                Success = success
                                Message = msg
                            }

                        return! json resp ctx
        with ex ->
            return!
                json
                    {
                        RequestId = Guid.Empty
                        Success = false
                        Message = "Error: " + ex.Message
                    }
                    ctx
    }

let notFound (ctx: HttpContext) : Task =
    let logger = ctx.GetLogger()
    logger.LogWarning("Unhandled 404 error")
    ctx.Write <| NotFound {| Error = "Resource was not found" |}

let endpoints =
    [
        // Add routes here
        route "/" <| text "Hello World!"
        route "/parse" <| parse
    ]
