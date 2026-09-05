// `when 'a : enum<int>` instantiated at an enum whose underlying type is `int`.
type Colour =
    | Red = 1
    | Green = 2

let onlyEnum<'a when 'a: enum<int>> (x: 'a) = x

let c = onlyEnum Colour.Green
ignore c
