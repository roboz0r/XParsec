namespace Test.A

[<Measure>] type m
[<Measure>] type s
[<Measure>] type v = m / s
[<Measure>] type a = m / s ^ 2
[<Measure>] type hz = / s

module M =
    val speed: float<v>
