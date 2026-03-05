module M

type MyAlias =
    SomeType<
        int,
        string
    >

type MyAlias2 =
    SomeType<
        Foo<int>,
        Bar<string>
    >
