module XParsec.FSharp.SemanticAnalysis.Tests.ValidationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input

    let ctx, _ =
        Pipeline.analyseSemWithContext realProvider.Value (LexedFile.ofText lexed) file

    ctx

let private hasMessage (ctx: PassContext) (fragment: string) =
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains fragment)

[<Tests>]
let tests =
    testList
        "Validation"
        [
            test "assignment to an immutable binding emits a diagnostic" {
                let ctx = analyse "let x = 1\nx <- 2"
                Expect.isTrue (hasMessage ctx "assignment to immutable binding") "diagnostic emitted"
            }

            test "assignment to a mutable binding is clean" {
                let ctx = analyse "let mutable x = 1\nx <- 2"
                Expect.isFalse (hasMessage ctx "immutable") "no immutability diagnostic"
            }

            test "assignment to an unresolved name does not crash" {
                // The mutability check has no binding entry to read, so it must skip rather
                // than throw or report a second time over the Unresolved diagnostic.
                let ctx = analyse "unknownName <- 1"
                Expect.isFalse (hasMessage ctx "immutable") "no immutability diagnostic on unresolved name"
                Expect.isTrue (hasMessage ctx "Unresolved") "the original unresolved diagnostic still fires"
            }

            test "value restriction: free TyVar at end of analysis fires a diagnostic" {
                // No use pins `'a`, and Validation runs after Unification, so it sees a
                // free root.
                let ctx = analyse "let mutable id = fun x -> x"
                Expect.isTrue (hasMessage ctx "value restriction") "VR diagnostic on unconstrained mutable"
            }

            test "value restriction: pinned by use → no diagnostic" {
                // `id 1` unifies `'a -> 'a` with `int -> _`, pinning `'a` before Validation
                // runs.
                let ctx = analyse "let mutable id = fun x -> x\nlet a = id 1"
                Expect.isFalse (hasMessage ctx "value restriction") "no VR diagnostic when use pinned the var"
            }

            test "value restriction: pinned by assignment → no diagnostic" {
                // Why the check lives in Validation and not Unification: the assignment is
                // seen AFTER the binding, so let-time cannot decide.
                let ctx = analyse "let mutable r = fun x -> x\nr <- (fun (n : int) -> n + 1)"

                Expect.isFalse (hasMessage ctx "value restriction") "no VR diagnostic when assignment pinned the var"
            }

            test "value restriction: concretely-typed mutable is clean" {
                let ctx = analyse "let mutable n = 0"
                Expect.isFalse (hasMessage ctx "value restriction") "no VR diagnostic for a concretely-typed mutable"
            }

            test "value restriction: parameter-annotated mutable is clean" {
                // The inner lambda's `(n : int)` propagates through unification, so
                // `f : int -> int` has no free vars at value-restriction time.
                let ctx = analyse "let mutable f = fun (n : int) -> n + 1"
                Expect.isFalse (hasMessage ctx "value restriction") "no VR diagnostic when params are annotated"
            }

            test "assignment to immutable field diagnoses" {
                let ctx =
                    analyse "type R = { X: int; mutable Y: int }\nlet r = { X = 1; Y = 2 }\nr.X <- 5"

                Expect.isTrue (hasMessage ctx "immutable field 'X'") "X-not-mutable diagnostic emitted"
            }

            test "assignment to mutable field is clean" {
                let ctx =
                    analyse "type R = { X: int; mutable Y: int }\nlet r = { X = 1; Y = 2 }\nr.Y <- 5"

                Expect.isFalse (hasMessage ctx "immutable field") "no immutability diagnostic on mutable field"
            }

            test "unresolved field access diagnoses" {
                // Nothing uses `f`, so the object argument's TyVar stays free.
                let ctx = analyse "let f r = r.X"
                Expect.isTrue (hasMessage ctx "Cannot resolve member") "deferred-dot-access diagnostic emitted"
            }

            test "resolved-by-use field access is clean" {
                let ctx = analyse "type R = { X: int }\nlet f r = r.X\nlet u = f { X = 1 }"

                Expect.isFalse (hasMessage ctx "Cannot resolve member") "no deferred-dot diagnostic"
            }

            test "well-formed DU pipeline emits no diagnostics" {
                let ctx =
                    analyse
                        "type S =\n    | Circle of float\n    | Rectangle of float * float\n    | Point\nlet p = Point\nlet c = Circle 1.0\nlet r = Rectangle(2.0, 3.0)"

                Expect.isEmpty ctx.Diagnostics "no diagnostics on a well-formed DU pipeline"
            }

            test "value restriction passes on a DU value" {
                // `TyUnion` is ground, so the mutable binding has no free TyVar.
                let ctx = analyse "type S = | Circle of float\nlet mutable c = Circle 1.0"

                Expect.isFalse (hasMessage ctx "value restriction") "no value-restriction on mutable DU"
            }

            test "mutable generic record literal is clean once pinned" {
                // The literal pins `'a`, so the binding is `Box<int>` — no free TyVar.
                let ctx = analyse "type Box<'a> = { Value: 'a }\nlet mutable b = { Value = 1 }"

                Expect.isFalse (hasMessage ctx "value restriction") "no VR diagnostic when the literal pins the typar"
            }

            test "mutable id is still diagnosed (sanity)" {
                let ctx = analyse "let mutable id = fun x -> x"
                Expect.isTrue (hasMessage ctx "value restriction") "free-typar mutable still fires"
            }

            test "unresolved member on class object argument still diagnoses" {
                let ctx = analyse "let f p = p.NotAMember"

                Expect.isTrue (hasMessage ctx "Cannot resolve member") "deferred-dot-access diagnostic emitted"
            }

            test "resolved-by-use class member access is clean" {
                let ctx =
                    analyse "type C() =\n    member this.M () = 1\nlet f p = p.M()\nlet _ = f (new C())"

                Expect.isFalse (hasMessage ctx "Cannot resolve member") "no deferred-dot diagnostic"
            }

            // FS3200 — in a recursive declaration group, `open`s must come first in each
            // module or namespace scope.
            test "module rec: an open after a binding is rejected (FS3200)" {
                let ctx = analyse "module rec R\n\nlet a = 1\nopen Q\nlet b = 2"
                Expect.isTrue (hasMessage ctx "must come first") "interspersed open in a module rec diagnoses"
            }

            test "module rec: opens-first is clean" {
                let ctx = analyse "module rec R\n\nopen P\nopen Q\nlet a = 1\nlet b = 2"
                Expect.isFalse (hasMessage ctx "must come first") "leading opens in a module rec are fine"
            }

            test "non-recursive module: an open after a binding is fine (running accumulator)" {
                // FS3200 is rec-only; a non-rec scope is a running accumulator, so
                // an interspersed open is legal (it just isn't visible above it).
                let ctx = analyse "module M\n\nlet a = 1\nopen Q\nlet b = 2"
                Expect.isFalse (hasMessage ctx "must come first") "interspersed open in a non-rec module is legal"
            }

            test "namespace rec: a nested module's interspersed open is rejected (FS3200)" {
                // Each module under a rec group is independently an opens-first scope, so
                // the open in `module B` is misplaced even though N's own opens lead.
                let ctx =
                    analyse "namespace rec N\n\nopen A\n\nmodule B =\n    let a = 1\n    open C\n    let b = 2"

                Expect.isTrue
                    (hasMessage ctx "must come first")
                    "interspersed open in a rec-namespace submodule diagnoses"
            }

            // A fieldless `[<Struct>]` whose body is only an interface impl is valid F# — a
            // stateless struct closure. The implicit-class lookahead must admit the leading
            // `interface` rather than fall to abbreviation parsing and skip tokens.
            test "fieldless [<Struct>] with only an interface impl parses cleanly" {
                let ctx =
                    analyse
                        "type Fun<'a, 'b> =\n    abstract member Invoke: 'a -> 'b\n\n[<Struct>]\ntype AddOne =\n    interface Fun<int, int> with\n        member _.Invoke(x: int) : int = x + 1"

                Expect.isFalse (hasMessage ctx "Skipped tokens") "no parse-recovery skip on a fieldless struct closure"
            }

            test "fieldless (plain) type with only an interface impl parses cleanly" {
                // The same shape without `[<Struct>]` takes the implicit-class path too.
                let ctx =
                    analyse
                        "type Fun<'a, 'b> =\n    abstract member Invoke: 'a -> 'b\n\ntype AddOne =\n    interface Fun<int, int> with\n        member _.Invoke(x: int) : int = x + 1"

                Expect.isFalse
                    (hasMessage ctx "Skipped tokens")
                    "no parse-recovery skip on a fieldless interface-only class"
            }
        ]
