// `when 'a : struct` instantiated at primitives the CLR represents as value types and JS
// represents as a `number` and a `boolean`.
let onlyStruct<'a when 'a: struct> (x: 'a) = x

let i = onlyStruct 42
let b = onlyStruct true
ignore i
ignore b
