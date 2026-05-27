module XParsec.FSharp.SemanticAnalysis.Tests.GeneralisationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private analyseWithCtx (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseWithContext MockBuiltins.provider input lexed file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | [ TDecl.Let(_, _, _, ty) ] -> ty
    | other -> failwithf "expected single TDecl.Let, got %A" other

let private hasMismatch (tast: TastFile) =
    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

let private hasOccurs (tast: TastFile) =
    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Occurs check")

[<Tests>]
let tests =
    testList
        "Generalisation"
        [
            test "polymorphic identity used at two types" {
                // The headline case. `id` generalises to `∀'a. 'a -> 'a`,
                // each use at a different type mints its own variable.
                let tast = analyse "let r = let id = fun x -> x in id 1, id true"

                Expect.equal
                    (declType tast)
                    (TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ]))
                    "r : int * bool"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "polymorphic identity in arithmetic and boolean position" {
                // The example from the README's "What this is not (yet)".
                let tast =
                    analyse "let r = let id = fun x -> x in id 1 + (if id true then 0 else 1)"

                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "polymorphic constant returned by application" {
                // `k 1 true` — k : 'a -> 'b -> 'a, applied to int and bool, returns int.
                let tast = analyse "let r = let k = fun x -> fun _ -> x in k 1 true"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "function-form let generalises" {
                // `let id x = x` is sugar for `let id = fun x -> x`; same scheme.
                let tast = analyse "let r = let id x = x in id 1, id true"

                Expect.equal
                    (declType tast)
                    (TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ]))
                    "r : int * bool"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "polymorphic recursion is rejected: f used at conflicting types in its own body" {
                // Within `f`'s RHS, `f` is monomorphic. Pinning to `bool -> ?`
                // via `f true` makes `f 1` outside the binding mismatch.
                let tast = analyse "let r = let rec f x = f true in f 1"
                // f's type pins to bool -> ?, so `f 1` at int triggers a mismatch.
                Expect.isTrue (hasMismatch tast) "mismatch on f 1 after f's RHS pinned bool"
            }

            test "lambda parameter does NOT generalise" {
                // `fun id -> ...` — `id` is a parameter, monomorphic.
                // Using it as both int and bool inside the body must mismatch.
                let tast =
                    analyse "let r = (fun id -> id 1 + (if id true then 0 else 1)) (fun x -> x)"

                Expect.isTrue (hasMismatch tast) "param `id` is monomorphic — int vs bool mismatches"
            }

            test "nested let-poly: inner binding generalises inside outer body" {
                let tast = analyse "let outer () = let inner x = x in inner 1, inner true"
                let unitTy = BuiltinTypes.tyUnit
                let bodyTy = TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ])
                Expect.equal (declType tast) (TyFun(unitTy, bodyTy)) "outer : unit -> int * bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "local scheme coexists with external polymorphic provider" {
                // A custom provider with `myId : 'a -> 'a`. Local `f` aliases
                // `myId`; both schemes mint independent vars per use site.
                // (NameRes doesn't yet handle the `(|>)` operator-form path
                // from the plan's example, so we exercise the same shape via
                // a regular-name external symbol.)
                let myIdSymbol: ExternalSymbol =
                    ExternalSymbols.poly
                        "myId"
                        (fun level ->
                            let tv = TypeVar()
                            tv.Level <- level
                            TyFun(TyVar tv, TyVar tv)
                        )

                let provider: IExternalSymbolProvider =
                    { new IExternalSymbolProvider with
                        member _.TryLookup name =
                            if name = "myId" then
                                ValueSome myIdSymbol
                            else
                                MockBuiltins.provider.TryLookup name

                        member _.TryLookupType _ = ValueNone
                        member _.TryLookupMember(_, _) = ValueNone
                        member _.TryLookupMembers(_, _) = [||]
                    }

                let input = "let r = let f = myId in f 1, f true"
                let lexed, file = parseFile input
                let tast = Pipeline.analyse provider input lexed file

                Expect.equal
                    (declType tast)
                    (TyTuple(EqArray.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ]))
                    "r : int * bool"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "occurs check still fires on let rec f x = f" {
                // Generalisation runs AFTER RHS unification, so the recursive
                // self-application that creates an infinite type still bombs.
                let tast = analyse "let rec f x = f"
                Expect.isTrue (hasOccurs tast) "occurs check stops infinite type"
            }

            test "mutual recursion + generalisation: id used inside pair at two types" {
                // `let rec id x = x and pair x = id x, id x`.
                // id generalises to `∀'a. 'a -> 'a`; pair generalises to
                // `∀'b. 'b -> 'b * 'b`. Two uses of `id` inside pair share
                // pair's argument variable, not id's quantified one.
                let tast = analyse "let rec id x = x\nand pair x = id x, id x"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let pairDecl =
                    match tast.Decls with
                    | [ _; d ] -> d
                    | other -> failwithf "expected two decls, got %A" other

                match pairDecl with
                | TDecl.Let(_, _, _, TyFun(arg, TyTuple args)) when args.Length = 2 ->
                    // pair : 'b -> ('b * 'b) — both tuple elements share 'b.
                    Expect.equal args.[0] arg "first tuple element matches arg type"
                    Expect.equal args.[1] arg "second tuple element matches arg type"
                | other -> failtestf "expected `'b -> 'b * 'b`, got %A" other
            }

            test "local scheme: two uses do not share variables" {
                // `let id = fun x -> x` followed by two top-level uses at
                // different types. id's scheme is at module level; each
                // top-level binding is its own group.
                let tast = analyse "let id = fun x -> x\nlet a = id 1\nlet b = id true"

                match tast.Decls with
                | [ _; TDecl.Let(_, _, _, aTy); TDecl.Let(_, _, _, bTy) ] ->
                    Expect.equal aTy BuiltinTypes.tyInt "a : int"
                    Expect.equal bTy BuiltinTypes.tyBool "b : bool"
                | other -> failwithf "expected three decls, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "tuple-destructuring let does NOT generalise" {
                // Compound headPats skip the scheme table. `let (f, _) = ...`
                // doesn't get a scheme even if the RHS would otherwise generalise.
                // No mismatch expected — this just confirms it type-checks.
                let tast = analyse "let r = let (f, _) = (fun x -> x), 0 in f 1"
                Expect.isEmpty tast.Diagnostics "no diagnostics for monomorphic tuple-destructure"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
            }

            test "mutable binding does NOT generalise (no scheme entry)" {
                // `let mutable id = fun x -> x` — even though the RHS is a
                // syntactic value, the `mutable` keyword skips generalisation.
                let ctx, _ = analyseWithCtx "let mutable id = fun x -> x"
                let idKey = NodeKey.ofSource 12 NodeKind.PatIdent

                Expect.isTrue (ctx.Bindings.Scheme.TryGetValue idKey = ValueNone) "no scheme entry for mutable binding"
            }

            test "mutable binding is monomorphic across two use sites" {
                // First use pins the binding's TyVar. Second use at a different
                // type triggers a mismatch — mirrors the pre-generalisation
                // behaviour of lambda parameters.
                let tast = analyse "let mutable id = fun x -> x\nlet a = id 1\nlet b = id true"
                Expect.isTrue (hasMismatch tast) "second use at bool conflicts with int from first use"
            }

            test "mutable binding: assignment unifies LHS and RHS types" {
                // `let mutable r = fun x -> x` starts with `'a -> 'a`. The
                // assignment unifies it with `int -> int`, pinning the free
                // var globally. No mismatch; subsequent reads see int -> int.
                let ctx, _ =
                    analyseWithCtx "let mutable r = fun x -> x\nr <- (fun (n : int) -> n + 1)"

                let rKey = NodeKey.ofSource 12 NodeKind.PatIdent

                let rTy =
                    match ctx.Bindings.TypeVar.TryGetValue rKey with
                    | ValueSome tv -> Unification.zonk (TyVar tv)
                    | ValueNone -> failtest "no TypeVar for r"

                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal rTy intToInt "r : int -> int after assignment"
            }
        ]
