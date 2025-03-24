module XParsec.Client.API

open Oxpecker.Solid
open Fable.Core
open Thoth.Fetch
open Thoth.Json

open XParsec.Shared
open Serialization

let apiBase = "http://localhost:65357"


let fetchRoot () : JS.Promise<string> =
    promise {
        let! response = Fetch.fetch $"{apiBase}/" [] // Fetch.get($"{apiBase}/", caseStrategy = CaseStrategy.CamelCase)
        return! response.text ()
    }

let private coders =
    Extra.empty |> Extra.withCustom ParseRequest.encode ParseRequest.decoder

let parseContent (input: ParseRequest) : JS.Promise<ParseResponse> =
    promise {
        let! response = Fetch.post ($"{apiBase}/parse", input, caseStrategy = CaseStrategy.CamelCase, extra = coders)
        do! Promise.sleep 1000
        return response
    }

let orders, ordersMgr = createResource (fetchRoot)
