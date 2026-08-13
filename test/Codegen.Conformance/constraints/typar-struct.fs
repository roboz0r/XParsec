// `when 'a : struct` instantiated at primitives the CLR represents as value types.
// Nothing is printed: the constraint holding has no result, so this is an `accept` row.
let onlyStruct<'a when 'a: struct> (x: 'a) = x

let i = onlyStruct 42
let b = onlyStruct true
ignore i
ignore b
