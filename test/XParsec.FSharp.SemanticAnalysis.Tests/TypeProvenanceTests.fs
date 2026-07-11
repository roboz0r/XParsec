module XParsec.FSharp.SemanticAnalysis.Tests.TypeProvenanceTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Type provenance: `PassContext.IsTypeDeclared` (a node's type was written in source)
// and `IsInferenceHole` / `HasInferenceHoleIn` (a `_`-wildcard position inside an
// otherwise-declared annotation, e.g. `Box<_>` — `Box` declared, the arg inferred).
// Read off the LIVE (pre-freeze) ctx: Elaborate zonks holes to their inferred fill.

let private analyse (input: string) : PassContext * TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSemWithContext realProvider.Value input lexed file

/// The trailing `let`'s binder NodeKey (its declared/inferred query target).
let private lastBinderKey (tast: TastFile) : NodeKey =
    match tast.Decls.[tast.Decls.Length - 1] with
    | TDecl.Let(TPat.NamedSimple(k, _, _), _, _, _) -> k
    | other -> failwithf "expected a trailing let-binder, got %A" other

/// The trailing `let f <param> = …`'s single lambda-parameter NodeKey.
let private lastParamKey (tast: TastFile) : NodeKey =
    match tast.Decls.[tast.Decls.Length - 1] with
    | TDecl.Let(_, TExpr.Lambda(TPat.NamedSimple(k, _, _), _, _, _), _, _) -> k
    | other -> failwithf "expected a trailing single-param function, got %A" other

/// The binder's LIVE (un-zonked) type — the graph `HasInferenceHoleIn` must read.
let private liveType (ctx: PassContext) (key: NodeKey) : SemType =
    match ctx.Bindings.TypeVar.TryGetValue key with
    | ValueSome tv -> TyVar tv
    | ValueNone -> failwithf "no TypeVar entry for %O" key

let private box = "type Box<'T> = { Item: 'T }\n"

[<Tests>]
let tests =
    testList
        "TypeProvenance"
        [
            test "an annotated value binding's type is declared" {
                let ctx, tast = analyse "let x : int = 1"
                Expect.isTrue (ctx.IsTypeDeclared(lastBinderKey tast)) "let x : int = 1 → x declared"
            }

            test "an unannotated value binding's type is inferred" {
                let ctx, tast = analyse "let x = 1"
                Expect.isFalse (ctx.IsTypeDeclared(lastBinderKey tast)) "let x = 1 → x inferred"
            }

            test "a typed function parameter is declared" {
                let ctx, tast = analyse "let f (x: int) = x"
                Expect.isTrue (ctx.IsTypeDeclared(lastParamKey tast)) "(x: int) → declared"
            }

            test "an untyped function parameter is inferred" {
                let ctx, tast = analyse "let f x = x + 1"
                Expect.isFalse (ctx.IsTypeDeclared(lastParamKey tast)) "x → inferred"
            }

            test "a fully-written annotation has no inference holes (Box<int>)" {
                let ctx, tast = analyse (box + "let x : Box<int> = { Item = 5 }")
                let k = lastBinderKey tast
                Expect.isTrue (ctx.IsTypeDeclared k) "Box<int> → declared"
                Expect.isFalse (ctx.HasInferenceHoleIn(liveType ctx k)) "Box<int> → no holes"
            }

            test "a wildcard argument is an inference hole inside a declared type (Box<_>)" {
                // The `Box` head is declared, the `_` argument inferred — even though
                // inference pins `_` to `int` from the `{ Item = 5 }` initialiser.
                let ctx, tast = analyse (box + "let x : Box<_> = { Item = 5 }")
                let k = lastBinderKey tast
                Expect.isTrue (ctx.IsTypeDeclared k) "Box<_> → the binding is (partly) declared"
                Expect.isTrue (ctx.HasInferenceHoleIn(liveType ctx k)) "Box<_> → the arg is an inferred hole"
            }

            test "a named typar argument is written, not a hole (Box<'a>)" {
                // `'a` is written by the programmer, so it is NOT an inference hole —
                // only the anonymous `_` is. Use a parameter to avoid value restriction.
                let ctx, tast = analyse (box + "let f (x: Box<'a>) = x")
                let k = lastParamKey tast
                Expect.isTrue (ctx.IsTypeDeclared k) "(x: Box<'a>) → declared"
                Expect.isFalse (ctx.HasInferenceHoleIn(liveType ctx k)) "Box<'a> → no holes ('a is written)"
            }

            test "a bare wildcard annotation `(x: _)` is a request to infer, so inferred" {
                // Unlike `Box<_>` (written head, inferred arg), a *whole-type* `_` writes
                // no structure — it asks inference to fill it, so the binder is inferred,
                // not declared.
                let ctx, tast = analyse "let f (x: _) = x + 1"
                Expect.isFalse (ctx.IsTypeDeclared(lastParamKey tast)) "(x: _) → inferred"
            }
        ]
