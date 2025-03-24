open Browser
open Oxpecker.Solid
open Oxpecker.Solid.Router
open Fable.Core.JsInterop

open XParsec.Client.App

// HMR doesn't work in Root for some reason
[<SolidComponent>]
let Root () =
    Router() {
        Route(path = "/", component' = App)
    // Route(path="/about", component'=About)
    }

render (Root, document.getElementById "root")
