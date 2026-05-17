module DirectivesAndAbbrev

#nowarn "57"
#nowarn "0044"

open System
open System.Collections.Generic

module L = List
module M = Microsoft.FSharp.Core

val parse: input: string -> int
val format: x: int -> string
