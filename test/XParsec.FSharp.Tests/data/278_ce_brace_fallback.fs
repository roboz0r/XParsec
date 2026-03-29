module Test

let xs =
    seq {
        System.Console.WriteLine("hello")
        yield 1
        yield! [ 2; 3 ]
    }

let ys =
    seq {
        1
        2
        3
    }
