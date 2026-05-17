module Generic

val inline id: x: 'T -> 'T
val inline fold<'T, 'State> : folder: ('State -> 'T -> 'State) -> state: 'State -> source: seq<'T> -> 'State
val inline contains: value: 'T -> source: seq<'T> -> bool when 'T: equality
