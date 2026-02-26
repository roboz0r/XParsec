module MyModule

type Handler = delegate of obj * System.EventArgs -> unit
type Transform = delegate of int -> int
