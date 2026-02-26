module MyModule

type MyClass =
    class
        let rec f x = g (x - 1)
        and g x = f (x + 1)
    end
