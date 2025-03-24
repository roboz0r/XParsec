module XParsec.Client.App

open Oxpecker.Solid

open Browser
open Browser.Types
open Fable.Core.JsInterop
open System

open XParsec.Shared

type Item = { Name: string } // sample record type

[<SolidComponent>] // this attribute is required to compile components to JSX that Solid understands
let itemList items = // regular function arguments, no props!
    let x, setX = createSignal 0 // just sample of reactive state usage

    ul () { // ul tag
        For(each = items) { // Solid's built-in For component
            yield
                fun item index -> // function called for each item
                    li () { // li tag
                        span () { index () } // index is reactive getter from Solid
                        span () { item.Name } // string field inside span tag

                        button (onClick = fun _ -> console.log ("Removing item...", item)) { // onClick event handler
                            "Remove" // button text
                        }
                    }
        }
    }

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
    let items = [| { Name = "Item 1" }; { Name = "Item 2" } |] // sample list of items
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
        // setOutputText (text + API.orders.current)
        updateOuputState text


    div (class' = "flex flex-col items-center justify-center min-h-screen bg-gray-900 text-white") {
        itemList items // render itemList component with items
        h1 () { "XParsec Test Client" }

        div (class' = "flex flex-col justify-between mb-5") {
            input (
                type' = "text",
                placeholder = "Enter a task",
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
