module XParsec.Client.App

open Oxpecker.Solid

open Browser
open Browser.Types
open Fable.Core.JsInterop
open System

open XParsec.Shared

type Deferred<'T> =
    | NotStarted
    | InProgress of id: Guid
    | Success of value: 'T
    | Failure of error: string

module Deferred =
    let resolve deferred id value =
        match deferred with
        | InProgress id' when id = id' ->
            match value with
            | Ok value -> Success value
            | Error error -> Failure error
        | _ -> deferred

    let start get set processReq request (id: Guid) =
        set (InProgress id)

        promise {
            try
                let! result = processReq request
                do set (resolve (get ()) id (Ok result))
            with ex ->
                do set (resolve (get ()) id (Error ex.Message))
        }

[<SolidComponent>]
let App () : HtmlElement =
    let inputText, setInputText = createSignal ""
    let outputState, setOutputState = createSignal NotStarted

    let mutable textAreaRef = Unchecked.defaultof<HTMLTextAreaElement> // mutable reference to textarea element

    let updateOuputState value =
        let id = Guid.NewGuid()

        let value =
            {
                Id = id
                Input = value
                Syntax = ParseSyntax.Json
            }

        let set x =
            setOutputState x

            match textAreaRef, x with
            | null, _ -> ()
            | textArea, Success value -> textArea.value <- value.Message
            | textArea, Failure error -> textArea.value <- error
            | textArea, InProgress id -> textArea.value <- "Parsing..."
            | _ -> ()

        Deferred.start outputState set API.parseContent value id |> ignore

    let handleInputChange (event: Event) =
        let text: string = event.target?value
        setInputText text
        updateOuputState text


    div (class' = "flex flex-col items-center justify-center min-h-screen bg-gray-900 text-white") {
        h1 () { "XParsec Test Client" }

        div (class' = "flex flex-col justify-between mb-5") {
            input (
                type' = "text",
                placeholder = "Enter partial json to test parser errors",
                required = true,
                value = inputText (),
                onChange = handleInputChange,
                class' =
                    "w-auto flex-1 p-2.5 border-neutral-700 border rounded mr-2.5 bg-neutral-800 text-neutral-200 sm:w-96"
            )

            (textarea (
                placeholder = "Output",
                readonly = true,
                rows = 10,
                class' =
                    "w-auto flex-1 p-2.5 border-neutral-700 border rounded mr-2.5 bg-neutral-800 text-neutral-200 sm:w-96"
            ))
                .ref (fun e -> textAreaRef <- e)
        }
    }
