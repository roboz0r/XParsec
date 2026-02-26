module MyModule

type Point =
    { X: float; Y: float }
    with
        abstract member Length: float
    end
