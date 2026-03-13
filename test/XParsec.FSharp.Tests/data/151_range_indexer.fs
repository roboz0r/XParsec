module RangeIndexer

let rawLit =
    if len > 10 then
        input.[charPos .. charPos + 9] + "..."
    else
        input[charPos..charEnd]
