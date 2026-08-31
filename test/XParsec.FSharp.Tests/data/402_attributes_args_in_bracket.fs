module Test402

// `159_attributes_targets_and_multiple.fs` covers a bracket of argument-FREE attributes. Here
// the leading attribute carries an argument, so `;` must still separate the bracket rather
// than sequence the argument.

[<CompiledName("first"); Sealed>]
type A() =
    member _.M() = ()

[<Obsolete("gone"); CompiledName("second")>]
type B() =
    member _.M() = ()

[<Foo(1); Bar(2); Baz(3)>]
type C() =
    member _.M() = ()

[<Sealed; CompiledName("trailing")>]
type D() =
    member _.M() = ()

[<assembly: Marker(1); assembly: Marker(2)>]
do ()

[<Attr "bare-string-arg"; Sealed>]
type E() =
    member _.M() = ()
