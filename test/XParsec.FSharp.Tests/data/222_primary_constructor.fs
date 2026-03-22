module Test

// Optional parameter in primary constructor
type MyClass(?label: string) =
    member _.Label = defaultArg label "default"
