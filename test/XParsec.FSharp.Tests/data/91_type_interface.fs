module MyModule

type IAnimal =
    interface
        abstract member Speak: unit -> string
        abstract member Name: string
    end
