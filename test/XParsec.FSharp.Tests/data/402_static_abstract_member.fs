module MyModule

type IAddable<'T> =
    static abstract Add: 'T * 'T -> 'T
    static abstract member Zero: 'T
