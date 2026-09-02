/// Programs written through type abbreviations, shared by both codegen suites. Each backend
/// asserts the alias leaves no artifact in its own output and the program prints `Expected`.
module XParsec.FSharp.Codegen.Common.Tests.TypeAbbreviationFixtures

/// A single-file program and the stdout it prints.
type Program =
    {
        /// A kebab-case name, unique across the fixtures, for the test and its artifacts.
        Name: string
        Description: string
        Source: string
        Expected: string
    }

/// The alias names every fixture declares. Each is distinct from every real type, member and
/// binding name in the fixtures, so an occurrence in emitted output is the abbreviation itself.
let aliasNames = [ "PointAlias"; "ShapeAlias"; "CounterAlias"; "myint"; "intpair" ]

let programs: Program list =
    [
        {
            Name = "abbrev-record"
            Description = "a record alias annotates, constructs a literal and reads a field (prints 7)"
            Source =
                String.concat
                    "\n"
                    [
                        "type Point = { X: int; Y: int }"
                        "type PointAlias = Point"
                        "type myint = int"
                        "let sum (p: PointAlias) : myint = p.X + p.Y"
                        "let origin: PointAlias = { X = 3; Y = 4 }"
                        "printfn \"%d\" (sum origin)"
                    ]
            Expected = "7"
        }
        {
            Name = "abbrev-generic"
            Description = "a generic alias applies at the use site and an alias is a type argument (prints 40 / 6)"
            Source =
                String.concat
                    "\n"
                    [
                        "type myint = int"
                        "type pair<'T> = 'T * 'T"
                        "let swap (p: pair<myint>) : pair<myint> ="
                        "    let (a, b) = p"
                        "    (b, a)"
                        "let weighted (p: pair<myint>) : myint ="
                        "    let (u, v) = swap p"
                        "    u + v * 2"
                        "printfn \"%d\" (weighted (10, 20))"
                        "let xs: myint list = [ 1; 2; 3 ]"
                        "let total ="
                        "    match xs with"
                        "    | [ a; b; c ] -> a + b + c"
                        "    | _ -> 0"
                        "printfn \"%d\" total"
                    ]
            Expected = "40\n6"
        }
        {
            Name = "abbrev-union"
            Description = "a union case is constructed and matched through an alias of the union (prints 25 / 12)"
            Source =
                String.concat
                    "\n"
                    [
                        "type Shape ="
                        "    | Circle of int"
                        "    | Square of int"
                        "type ShapeAlias = Shape"
                        "let area (s: ShapeAlias) : int ="
                        "    match s with"
                        "    | ShapeAlias.Circle r -> 3 * r * r"
                        "    | ShapeAlias.Square w -> w * w"
                        "printfn \"%d\" (area (ShapeAlias.Square 5))"
                        "printfn \"%d\" (area (ShapeAlias.Circle 2))"
                    ]
            Expected = "25\n12"
        }
        {
            Name = "abbrev-class"
            Description = "a class constructs and its static reads through an alias of the class (prints 7)"
            Source =
                String.concat
                    "\n"
                    [
                        "type Counter(start: int) ="
                        "    member _.Value = start"
                        "    static member Zero = Counter(0)"
                        "type CounterAlias = Counter"
                        "let seven = CounterAlias(7)"
                        "let zero: CounterAlias = CounterAlias.Zero"
                        "printfn \"%d\" (seven.Value + zero.Value)"
                    ]
            Expected = "7"
        }
    ]

/// The declarations every cross-file fixture publishes from its first file, written under
/// `header` (`"namespace Abbrev"`, or a namespace and a module line) at `indent`.
let declaringFile (header: string) (indent: string) : string =
    let body =
        [
            "type Point = { X: int; Y: int }"
            ""
            "type PointAlias = Point"
            ""
            "type Shape ="
            "    | Circle of int"
            "    | Square of int"
            ""
            "type ShapeAlias = Shape"
            ""
            "type Counter(start: int) ="
            "    member _.Value = start"
            "    static member Zero = Counter(0)"
            ""
            "type CounterAlias = Counter"
            ""
            "type myint = int"
            ""
            "type intpair = int * int"
        ]
        |> List.map (fun line -> if line.Length = 0 then line else indent + line)

    String.concat "\n" (header :: "" :: body) + "\n"
