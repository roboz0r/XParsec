module XParsec.FSharp.SemanticAnalysis.Tests.GeneralisationTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyseWithCtx (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSemWithContextFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | EqList [ TDecl.Let(m, _, _) ] -> m.Ty
    | _ -> failwithf "expected single TDecl.Let, got %A" tast.Decls

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
                // `id` generalises to `∀'a. 'a -> 'a`; each use mints its own variable.
                let tast = analyseSem "let r = let id = fun x -> x in id 1, id true"

                Expect.equal
                    (declType tast)
                    (TyTuple(Block.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ]))
                    "r : int * bool"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "polymorphic identity in arithmetic and boolean position" {
                let tast =
                    analyseSem "let r = let id = fun x -> x in id 1 + (if id true then 0 else 1)"

                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "polymorphic constant returned by application" {
                // `k 1 true` — k : 'a -> 'b -> 'a, applied to int and bool, returns int.
                let tast = analyseSem "let r = let k = fun x -> fun _ -> x in k 1 true"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "function-form let generalises" {
                // `let id x = x` is sugar for `let id = fun x -> x`; same scheme.
                let tast = analyseSem "let r = let id x = x in id 1, id true"

                Expect.equal
                    (declType tast)
                    (TyTuple(Block.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ]))
                    "r : int * bool"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "polymorphic recursion is rejected: f used at conflicting types in its own body" {
                // Within `f`'s RHS, `f` is monomorphic. Pinning to `bool -> ?`
                // via `f true` makes `f 1` outside the binding mismatch.
                let tast = analyseSem "let r = let rec f x = f true in f 1"
                Expect.isTrue (hasMismatch tast) "mismatch on f 1 after f's RHS pinned bool"
            }

            test "lambda parameter does NOT generalise" {
                // `fun id -> ...` — `id` is a parameter, monomorphic.
                // Using it as both int and bool inside the body must mismatch.
                let tast =
                    analyseSem "let r = (fun id -> id 1 + (if id true then 0 else 1)) (fun x -> x)"

                Expect.isTrue (hasMismatch tast) "param `id` is monomorphic — int vs bool mismatches"
            }

            test "nested let-poly: inner binding generalises inside outer body" {
                let tast = analyseSem "let outer () = let inner x = x in inner 1, inner true"
                let unitTy = BuiltinTypes.tyUnit
                let bodyTy = TyTuple(Block.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ])
                Expect.equal (declType tast) (TyFun(unitTy, bodyTy)) "outer : unit -> int * bool"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "local scheme coexists with external polymorphic provider" {
                // `myId : 'a -> 'a` as a one-typar FrozenType scheme. Local `f`
                // aliases it; both schemes mint independent vars per use site.
                let myIdSymbol: ExternalSymbol =
                    let scope =
                        TyparScope.ModuleFunction(SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") "myId")

                    ExternalSymbols.scheme
                        (SymbolKeyOps.inNamespace "")
                        "myId"
                        (FTFun(FTTypar(scope, 0), FTTypar(scope, 0)))
                        (FunctionScheme.unconstrained 1<typeSlot>)

                // First-hit-wins: only `myId` comes from the stub; the `int`/`bool`
                // intrinsics the RHS types through fall through to `realProvider`.
                let myIdStub: IExternalSymbolProvider = providerOfValues [ myIdSymbol ]

                let provider = ExternalSymbolProviders.composite [ myIdStub; realProvider.Value ]

                let input = "let r = let f = myId in f 1, f true"
                let lexed, file = parseFile input

                let tast =
                    Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                Expect.equal
                    (declType tast)
                    (TyTuple(Block.ofList [ BuiltinTypes.tyInt; BuiltinTypes.tyBool ]))
                    "r : int * bool"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "occurs check still fires on let rec f x = f" {
                // Generalisation runs AFTER RHS unification, so the recursive
                // self-application that creates an infinite type still bombs.
                let tast = analyseSem "let rec f x = f"
                Expect.isTrue (hasOccurs tast) "occurs check stops infinite type"
            }

            test "a sibling's scheme instantiates per use: id used inside pair" {
                // id : `∀'a. 'a -> 'a`; pair : `∀'b. 'b -> 'b * 'b`. `id` generalises first and
                // each use inside `pair` instantiates it against pair's argument var. The split
                // reports V260, so only errors are asserted absent.
                let tast = analyseSem "let rec id x = x\nand pair x = id x, id x"
                Expect.isEmpty (errorMessages tast.Diagnostics) "no errors"

                let pairTy =
                    match tast.Decls with
                    | EqList [ TDecl.LetGroup(BlockTwo(_, pair), _) ] -> pair.Ty
                    | _ -> failwithf "expected one group of two members, got %A" tast.Decls

                match pairTy with
                | TyFun(arg, TyTuple args) when args.Length = 2 ->
                    // pair : 'b -> ('b * 'b) — both tuple elements share 'b.
                    Expect.equal args.[0] arg "first tuple element matches arg type"
                    Expect.equal args.[1] arg "second tuple element matches arg type"
                | other -> failtestf "expected `'b -> 'b * 'b`, got %A" other
            }

            test "a `let rec … and …` group generalises one strongly connected component at a time" {
                // `f` references neither member and `g` references `f`, so they are separate
                // components: `f` generalises before `g` is typed, and `g` may use it at
                // `int` and at `string`. `dotnet fsi` accepts this program.
                let tast = analyseSem "let rec f x = x\nand g () = (f 1, f \"a\")"

                Expect.isEmpty
                    (errorMessages tast.Diagnostics)
                    "a syntactic group is not a generalisation barrier across components"
            }

            test "a genuine cycle is one component, so polymorphic recursion inside it is still rejected" {
                // `f` references `g` and `g` references `f`, so both stay monomorphic for the
                // whole component and the two uses of `f` conflict. `dotnet fsi` reports FS0001.
                let tast = analyseSem "let rec f x = g x\nand g x = (f 1, f \"a\")"
                Expect.isTrue (hasMismatch tast) "`f 1` and `f \"a\"` conflict within the component"
            }

            test "local scheme: two uses do not share variables" {
                // id's scheme is at module level; each top-level binding below is its
                // own generalisation group, so `a` and `b` do not share variables.
                let tast = analyseSem "let id = fun x -> x\nlet a = id 1\nlet b = id true"

                match tast.Decls with
                | EqList [ _; TDecl.Let(a, _, _); TDecl.Let(b, _, _) ] ->
                    Expect.equal a.Ty BuiltinTypes.tyInt "a : int"
                    Expect.equal b.Ty BuiltinTypes.tyBool "b : bool"
                | other -> failwithf "expected three decls, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "tuple-destructuring let does NOT generalise" {
                // Compound binding patterns skip the scheme table: `let (f, _) = …`
                // gets no scheme even though the RHS would otherwise generalise.
                let tast = analyseSem "let r = let (f, _) = (fun x -> x), 0 in f 1"
                Expect.isEmpty tast.Diagnostics "no diagnostics for monomorphic tuple-destructure"
                Expect.equal (declType tast) BuiltinTypes.tyInt "r : int"
            }

            test "mutable binding does NOT generalise (no scheme entry)" {
                // `let mutable id = fun x -> x` — even though the RHS is a
                // syntactic value, the `mutable` keyword skips generalisation.
                let ctx, _ = analyseWithCtx "let mutable id = fun x -> x"
                let idKey = NodeKey.ofSource 12 NodeKind.PatIdent

                Expect.isTrue (ctx.TryScheme idKey = ValueNone) "no scheme entry for mutable binding"
            }

            test "mutable binding is monomorphic across two use sites" {
                // `id 1` pins the binding's TyVar to `int -> int`, so `id true`
                // mismatches — the same behaviour as a lambda parameter.
                let tast = analyseSem "let mutable id = fun x -> x\nlet a = id 1\nlet b = id true"
                Expect.isTrue (hasMismatch tast) "second use at bool conflicts with int from first use"
            }

            test "chained generic combinator with constrained result typar" {
                // `wrap`'s constraint `'S :> ISeq<'T, 'E>` carries a phantom typar `'E`
                // that also appears in its result, so chaining must freshen `'E` per call:
                // if `wrap s0` grounds the shared `'E` to `ArrEnum<'T>`, `wrap s1` fails.
                let src =
                    String.concat
                        "\n"
                        [
                            "type IEnum<'T> ="
                            "    abstract member Current : 'T"
                            "type ISeq<'T, 'E when 'E :> IEnum<'T>> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrEnum<'T> ="
                            "    val Cur : 'T"
                            "    new(c: 'T) = { Cur = c }"
                            "    interface IEnum<'T> with"
                            "        member this.Current : 'T = this.Cur"
                            "[<Struct>]"
                            "type ArrSeq<'T> ="
                            "    val C : 'T"
                            "    new(c: 'T) = { C = c }"
                            "    interface ISeq<'T, ArrEnum<'T>> with"
                            "        member this.GetEnumerator() : ArrEnum<'T> = ArrEnum<'T>(this.C)"
                            "[<Struct>]"
                            "type WEnum<'E, 'T when 'E :> IEnum<'T>> ="
                            "    val mutable Source : 'E"
                            "    new(source: 'E) = { Source = source }"
                            "    interface IEnum<'T> with"
                            "        member this.Current : 'T = this.Source.Current"
                            "[<Struct>]"
                            "type WSeq<'S, 'E, 'T when 'S :> ISeq<'T, 'E> and 'E :> IEnum<'T>> ="
                            "    val Source : 'S"
                            "    new(source: 'S) = { Source = source }"
                            "    interface ISeq<'T, WEnum<'E, 'T>> with"
                            "        member this.GetEnumerator() : WEnum<'E, 'T> = WEnum<'E, 'T>(this.Source.GetEnumerator())"
                            "let ofVal (x: 'T) : ArrSeq<'T> = ArrSeq<'T>(x)"
                            "let wrap (source: 'S when 'S :> ISeq<'T, 'E> and 'E :> IEnum<'T>) : WSeq<'S, 'E, 'T> = WSeq<'S, 'E, 'T>(source)"
                            "let s0 = ofVal 1"
                            "let s1 = wrap s0"
                            "let s2 = wrap s1"
                        ]

                let tast = analyseSem src
                Expect.isEmpty tast.Diagnostics (sprintf "chained wrap diagnostics: %A" tast.Diagnostics)
            }

            test "mutable binding: assignment unifies LHS and RHS types" {
                // `r` starts at `'a -> 'a`; the assignment unifies it with
                // `int -> int`, pinning the free var globally rather than mismatching.
                let ctx, _ =
                    analyseWithCtx "let mutable r = fun x -> x\nr <- (fun (n : int) -> n + 1)"

                let rKey = NodeKey.ofSource 12 NodeKind.PatIdent

                let rTy =
                    match ctx.Bindings.TypeVar.TryGetValue rKey with
                    | ValueSome tv -> Unification.zonk ctx.Store (TyVar tv)
                    | ValueNone -> failtest "no TypeVar for r"

                let intToInt = TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)
                Expect.equal rTy intToInt "r : int -> int after assignment"
            }
        ]
