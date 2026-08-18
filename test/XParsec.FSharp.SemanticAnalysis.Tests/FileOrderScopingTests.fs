module XParsec.FSharp.SemanticAnalysis.Tests.FileOrderScopingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (Hashing.originSourceOfText lexed) file

let private errors (tast: TastFile) =
    [
        for d in tast.Diagnostics do
            if Diagnostic.isError d then
                yield d.Message
    ]

let private expectClean (source: string) =
    let es = errors (analyse source)
    Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

/// A use ABOVE the declaration it names must not resolve. Only the VERDICT is pinned, not
/// the wording: F# blames these with FS0039 ("not defined"), we word them our own way.
let private expectRejected (source: string) =
    let es = errors (analyse source)
    Expect.isNonEmpty es "expected a diagnostic: the name is used above its declaration"

let private soleModuleLetType (tast: TastFile) : SemType =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Let(ty = ty) -> yield ty
                | _ -> ()
        ]

    match found with
    | [ ty ] -> ty
    | other -> failtestf "expected exactly one module-level let, got %A" other

/// The key the declaration itself was registered under — so an assertion compares the use
/// site against the DECLARATION, never against a hand-spelled key.
let private typeDeclKey (tast: TastFile) (typeName: string) : TypeKey =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName -> yield td.TypeKey
                | _ -> ()
        ]

    match found with
    | [ k ] -> k
    | other -> failtestf "expected exactly one type declaration named '%s', got %A" typeName other

let private nominalKey (ty: SemType) : TypeKey =
    match ty with
    | SemType.TyClass(k, _)
    | SemType.TyRecord(k, _)
    | SemType.TyUnion(k, _) -> k
    | other -> failtestf "expected a nominal type, got %A" other

/// The RESULT type of the file's sole module `let` — for `let f () = e`, what `e` bound to.
let private soleModuleLetResult (tast: TastFile) : SemType =
    match soleModuleLetType tast with
    | SemType.TyFun(_, ret) -> ret
    | other -> failtestf "expected the sole module let to be a function, got %A" other

/// The ARGUMENT type of the file's sole module `let` — for `let f x = …`, what `x` bound to.
let private soleModuleLetArg (tast: TastFile) : SemType =
    match soleModuleLetType tast with
    | SemType.TyFun(arg, _) -> arg
    | other -> failtestf "expected the sole module let to be a function, got %A" other

// F# declaration scoping is file-ordered: a use sees what is written above it and nothing
// below, with the `type … and …` group (and a type's own members) as the one recursive
// exception. Every test asserts the RESOLVED IDENTITY where it can, not mere acceptance.
[<Tests>]
let tests =
    testList
        "FileOrderScoping"
        [
            test "a ctor call below the class's declaration resolves to it" {
                let tast = analyse "type Foo(n: int) =\n    member this.N = n\nlet mk () = Foo(1)"

                Expect.isEmpty (errors tast) "no diagnostics"

                Expect.equal
                    (nominalKey (soleModuleLetResult tast))
                    (typeDeclKey tast "Foo")
                    "mk () constructs the class Foo declared above it"
            }

            // The static member returns a RECORD, so the `let`'s result type is the
            // member's own type rather than degenerating to a free TyVar.
            test "a static-member access below the class's declaration resolves to the member" {
                let tast =
                    analyse
                        "type Payload = { v: int }\ntype Foo() =\n    static member Bar = { v = 1 }\nlet s () = Foo.Bar"

                Expect.isEmpty (errors tast) "no diagnostics"

                Expect.equal
                    (nominalKey (soleModuleLetResult tast))
                    (typeDeclKey tast "Payload")
                    "s () is Foo.Bar's type — the record the static member returns"
            }

            test "a member body referencing its own type resolves to it" {
                expectClean "type C(n: int) =\n    member this.N = n\n    member this.Self: C = C(this.N)"
            }

            test "an and-joined sibling declared below is visible in a member signature" {
                expectClean "type A() =\n    member _.M(b: B) = b.N\nand B(n: int) =\n    member _.N = n"
            }

            // A ctor call and a static access of an `and`-sibling — the same EXPRESSION
            // positions, inside the group that grants them visibility.
            test "an and-joined sibling is constructible from a member body" {
                expectClean
                    "type A() =\n    member _.M() = B(5).N\nand B(n: int) =\n    member _.N = n\n    static member Zero = B(0)"
            }

            // The NEGATIVES: the same surfaces written ABOVE the declaration. The name simply
            // misses at the use site, and the "unresolved" report IS that miss. F# blames the
            // ctor, the QUALIFIER (not the member) and the record LABEL, all with FS0039.
            test "a ctor call above the class's declaration does not resolve" {
                expectRejected "let mk () = Foo(1)\ntype Foo(n: int) =\n    member this.N = n"
            }

            test "a static-member access above the class's declaration does not resolve" {
                expectRejected "let s () = Foo.Bar\ntype Foo() =\n    static member Bar = 1"
            }

            test "a record literal above the record's declaration does not resolve" {
                expectRejected "let f () = { a = 1 }\ntype R = { a: int }"
            }

            // The ONE place where "does not resolve" is not an error: an ident that does not
            // resolve in PATTERN position is a variable pattern, so F# accepts this (FS0049 /
            // FS0026 warnings) and `f` is `'a -> int`, not `U -> int`. Probed with `f "s"` and `f 42`.
            test "a union case above its union's declaration is a variable pattern" {
                let tast =
                    analyse
                        "let f x =\n    match x with\n    | Alpha -> 1\n    | Beta -> 2\ntype U =\n    | Alpha\n    | Beta"

                Expect.isEmpty
                    (errors tast)
                    "no diagnostics: an unrecognised ident in pattern position is a bound variable"

                match soleModuleLetArg tast with
                | SemType.TyVar _
                | SemType.TyTypar _ -> ()
                | other -> failtestf "expected `Alpha` to bind `x` as a free variable pattern, got %A" other
            }

            // The positive control for the above: with the union declared ABOVE, the very same
            // arms are union-case patterns, so the argument is the union.
            test "a union case below its union's declaration is a case pattern" {
                let tast =
                    analyse
                        "type U =\n    | Alpha\n    | Beta\nlet f x =\n    match x with\n    | Alpha -> 1\n    | Beta -> 2"

                Expect.isEmpty (errors tast) "no diagnostics"

                Expect.equal
                    (nominalKey (soleModuleLetArg tast))
                    (typeDeclKey tast "U")
                    "the arms are cases of U, so f takes a U"
            }

            // VALUES, the same file-order rule: a module `let` is visible from where it is
            // WRITTEN, so a use above it does not resolve — whether that use is another module
            // `let` or a class member body. `module rec` / `namespace rec` is the opt-out.
            test "a module let calling a let below it does not resolve" {
                expectRejected "let f () = g ()\nlet g () = 1"
            }

            test "a class member calling a module let below the type does not resolve" {
                expectRejected "type C() =\n    member _.M() = helper ()\nlet helper () = 1"
            }

            // The control for the two rejections above: a member body reaching an EARLIER
            // module let is ordinary F#.
            test "a class member calling a module let above the type resolves" {
                expectClean "let helper () = 1\ntype C() =\n    member _.M() = helper ()"
            }

            // Both rejected programs above are accepted verbatim inside a `module rec`.
            test "module rec restores forward visibility for a module let" {
                expectClean "module rec M\n\nlet f () = g ()\nlet g () = 1"
            }

            test "module rec restores forward visibility for a class member" {
                expectClean "module rec M\n\ntype C() =\n    member _.M() = helper ()\n\nlet helper () = 1"
            }
        ]
