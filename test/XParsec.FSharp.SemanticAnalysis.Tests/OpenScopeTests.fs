module XParsec.FSharp.SemanticAnalysis.Tests.OpenScopeTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// the scope-preserving module walk
// (`CstWalk.walkModuleTree`) and the qualification primitive (`OpenScope.tryQualify`
// / `tryResolve`). No pass consumes the scope yet — these pin the computed scope
// directly.

/// Flatten a source string to `(label, prefixes)` per leaf element, where `label`
/// names the element and `prefixes` is the `OpenScope.Prefixes` active there.
let private walk (input: string) : (string * string list) list =
    let lexed, file = parseFile input
    let ctx = PassContext(realProvider.Value, Hashing.originSourceOfText lexed)

    let label (e: ModuleElem<SyntaxToken>) : string =
        match e with
        | ModuleElem.Import(ImportDecl.ImportDecl(longIdent = li)) ->
            "open " + (li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
        | ModuleElem.ModuleAbbrev(ModuleAbbrev.ModuleAbbrev(ident = id)) -> "abbrev " + ctx.NameOf id
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) when bs.Length > 0 ->
            match bs.[0].headPat with
            | Pat.NamedSimple t -> "let " + ctx.NameOf t
            | _ -> "let ?"
        | _ -> "other"

    CstWalk.walkModuleTree ctx.NameOf OpenScope.empty file
    |> List.map (fun (e, scope) -> label e, scope.Prefixes)

[<Tests>]
let tests =
    testList
        "OpenScope"
        [
            test "non-recursive: running accumulator, open visible only after it, inherited by nested module" {
                // `open B` sits inside M after `let x`; it must not reach `let x`
                // but must reach `let y` and the nested `Inner.let z`. The
                // enclosing `open A` (and the namespace `N`) reach everything below.
                let src =
                    "namespace N\n\nopen A\n\nmodule M =\n    let x = 1\n    open B\n    let y = 2\n\n    module Inner =\n        let z = 3\n"

                let got = walk src

                let expected =
                    [
                        "open A", [ "N" ]
                        "let x", [ "A"; "N" ]
                        "open B", [ "A"; "N" ]
                        "let y", [ "B"; "A"; "N" ]
                        "let z", [ "B"; "A"; "N" ]
                    ]

                Expect.equal got expected "per-element prefixes (most-recent-first)"
            }

            test "recursive module: constant prelude, every open applies to the whole body" {
                // `module rec`: `open Q` declared *after* `let a` is still visible to
                // `let a` (FS3200's whole-scope-prelude semantics, §3.2). Every
                // element sees the same constant scope.
                let src = "module rec R\n\nopen P\nlet a = 1\nopen Q\nlet b = 2\n"

                let got = walk src

                let expected =
                    [
                        "open P", [ "Q"; "P" ]
                        "let a", [ "Q"; "P" ]
                        "open Q", [ "Q"; "P" ]
                        "let b", [ "Q"; "P" ]
                    ]

                Expect.equal got expected "constant prelude shared by all elements"
            }

            test "module abbrev expands the head segment before probing" {
                let scope =
                    { OpenScope.empty with
                        Abbrevs = Map.ofList [ "R", "A.B.C" ]
                    }

                Expect.equal
                    (OpenScope.tryQualify scope (fun n -> n = "A.B.C.X") "R.X")
                    (ValueSome "A.B.C.X")
                    "R.X expands to A.B.C.X"
            }

            test "tryQualify: bare name first, then prefixes, head prefix shadows" {
                let scope =
                    { OpenScope.empty with
                        Prefixes = [ "B"; "A" ]
                    }

                // A name present at the root resolves bare, before any prefix.
                Expect.equal
                    (OpenScope.tryQualify scope (fun n -> n = "Foo") "Foo")
                    (ValueSome "Foo")
                    "bare name wins when present at root"

                // When both prefixes could answer, the most-recent (head) wins.
                Expect.equal
                    (OpenScope.tryQualify scope (fun n -> n = "A.x" || n = "B.x") "x")
                    (ValueSome "B.x")
                    "B (most recent) shadows A"

                Expect.equal (OpenScope.tryQualify scope (fun _ -> false) "x") ValueNone "no candidate resolves"
            }

            test "tryResolve returns the looked-up value under the resolving prefix" {
                let scope =
                    { OpenScope.empty with
                        Prefixes = [ "System.Collections.Generic" ]
                    }

                let lookup name =
                    if name = "System.Collections.Generic.EqualityComparer" then
                        ValueSome 42
                    else
                        ValueNone

                Expect.equal
                    (OpenScope.tryResolve scope lookup "EqualityComparer")
                    (ValueSome 42)
                    "resolves via the prefix"

                Expect.equal (OpenScope.tryResolve scope lookup "Nope") ValueNone "unknown misses"
            }
        ]
