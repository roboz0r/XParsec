module XParsec.FSharp.SemanticAnalysis.Tests.OpenScopeTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// Flatten a source string to `(label, open paths)` per element — the opens in force there,
/// most-recent-first.
let private walk (input: string) : (string * string list) list =
    let lexed, file = parseFile input

    let ctx = PassContext(realProvider.Value, LexedFile.ofText lexed, testCompiling)

    let label (e: ModuleElem<SyntaxToken>) : string =
        match e with
        | ModuleElem.Import(ImportDecl.ImportDecl(longIdent = li)) ->
            "open " + (li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
        | ModuleElem.ModuleAbbrev(ModuleAbbrev.ModuleAbbrev(ident = id)) -> "abbrev " + ctx.NameOf id
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) when bs.Length > 0 ->
            match bs.[0].pattern with
            | Pat.NamedSimple t -> "let " + ctx.NameOf t
            | _ -> "let ?"
        | _ -> "other"

    CstModuleTree.walkImpl ctx.NameOf OpenScope.empty file
    |> List.map (fun w ->
        let opens =
            w.Scope
            |> List.choose (
                function
                | LocalScopeDecl.Open o -> Some o.Path
                | LocalScopeDecl.Abbrev _ -> None
            )

        label w.Elem, opens
    )

[<Tests>]
let tests =
    testList
        "OpenScope"
        [
            test "non-recursive: running accumulator, open visible only after it, inherited by nested module" {
                let src =
                    "namespace N\n\nopen A\n\nmodule M =\n    let x = 1\n    open B\n    let y = 2\n\n    module Inner =\n        let z = 3\n"

                let got = walk src

                let expected =
                    [
                        "open A", []
                        "let x", [ "A" ]
                        "open B", [ "A" ]
                        "let y", [ "B"; "A" ]
                        "let z", [ "B"; "A" ]
                    ]

                Expect.equal got expected "per-element opens (most-recent-first)"
            }

            test "recursive module: constant prelude, every open applies to the whole body" {
                // The tree walk shares one open set across a `rec` body, so `open Q` below
                // `let a` reaches it. Name resolution never meets the shape: FS3200 refuses an
                // `open` that is not first in a `rec` module, and this compiler emits it.
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

        ]
