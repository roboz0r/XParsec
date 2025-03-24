module XParsec.Server.ErrorRenderer

open System
open XParsec

let render errors = ParseErrors.summarize [ errors ]
