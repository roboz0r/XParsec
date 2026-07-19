module XParsec.FSharp.SemanticAnalysis.Tests.UnificationOverloadsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

[<Tests>]
let tests =
    testList
        "Unification.Overloads"
        [
            test "a rejected overload trial leaves the caller TyVar free (no residue)" {
                // The FIRST candidate `M(int, int)` binds the caller-side free var to
                // `int` at position one, then FAILS at position two (`string` vs `int`);
                // its scratch substitution is dropped. `M(string, string)` then wins (its
                // own fresh scratch binds the free var to `string`). The trial must never
                // touch the shared union-find, so the free var stays free afterwards.
                let overloadCtx = overloadCtx ()
                let freeTv = overloadCtx.Store.NewTypeVar()

                let pick candidates args =
                    pickWith overloadCtx [||] candidates args

                let candidates =
                    [| overloadMember [ intFt; intFt ] 0; overloadMember [ stringFt; stringFt ] 0 |]

                let chosen = pick candidates [ TyVar freeTv; BuiltinTypes.tyString ]

                Expect.equal chosen.IsSome true "M(string, string) is applicable"

                Expect.equal
                    (chosenParams chosen.Value)
                    [ BuiltinTypes.tyString; BuiltinTypes.tyString ]
                    "the string overload wins"

                Expect.equal freeTv.Link ValueNone "the failed trial left the caller TyVar free"
            }

            test "a shared method typar must bind consistently across argument positions" {
                // `M<'T>('T, 'T)` called with `(int, string)` is NOT applicable: `'T`
                // binds `int` at position one, so `string` at position two rejects it.
                // `M(int, string)` wins. fsi confirms the shared-typar overload is rejected.
                // A LONE `M<'T>('T,'T)` would instead surface as a commit-seam type error
                // (the picker is never entered for a single name/arity candidate), so the
                // second candidate is essential.
                let overloadCtx = overloadCtx ()

                let pick candidates args =
                    pickWith overloadCtx [||] candidates args

                let shared =
                    overloadMember [ FTTypar(TyparAxis.Method, 0); FTTypar(TyparAxis.Method, 0) ] 1

                let concrete = overloadMember [ intFt; stringFt ] 0

                let chosen =
                    pick [| shared; concrete |] [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]

                Expect.equal chosen.IsSome true "M(int, string) is applicable after the shared-typar reject"

                Expect.equal
                    (chosenParams chosen.Value)
                    [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]
                    "the concrete overload wins, not the shared-typar one"
            }

            test "a non-ground argument resolves against a concrete-parameter overload" {
                // The argument type is still a free `TyVar`. It must BIND against the
                // concrete `int` parameter rather than falling to `| _ -> false` and
                // reporting "no applicable overload". Fails TODAY: `TyVar` vs `TyConst`
                // is not matched by the pre-`matchTypes` filter. The arity-2 sibling is
                // filtered out by arity, leaving `M(int)` the unique survivor.
                let overloadCtx = overloadCtx ()

                let pick candidates args =
                    pickWith overloadCtx [||] candidates args

                let candidates = [| overloadMember [ intFt ] 0; overloadMember [ intFt; intFt ] 0 |]
                let chosen = pick candidates [ TyVar(overloadCtx.Store.NewTypeVar()) ]

                Expect.equal chosen.IsSome true "the free argument binds against M(int) — the set is not killed"
                Expect.equal (chosenParams chosen.Value) [ BuiltinTypes.tyInt ] "M(int) is chosen"
            }

            test "the more derived parameter wins the applicable-tier ranking" {
                // `M(Base)` / `M(GrandBase)`, argument `Derived`: neither is an exact match, so
                // both enter the applicable tier by subsumption. `Base` is the more derived of
                // the two (`Base :> GrandBase`), so `compareTypes` ranks `M(Base)` strictly
                // above `M(GrandBase)`. fsi confirms the nearer base wins.
                let hierCtx = hierCtx ()
                let candidates = [| classMember baseTy; classMember grandBaseTy |]
                let chosen = pickWith hierCtx [||] candidates [ derivedTy ]

                Expect.equal chosen.IsSome true "a unique best exists"
                Expect.equal (chosenParams chosen.Value) [ baseTy ] "M(Base) beats M(GrandBase)"
            }

            test "an exact match beats an applicable supertype" {
                // `M(Base)` / `M(Derived)`, argument `Derived`: the exact tier finds a single
                // structural survivor (`M(Derived)`) and returns it with no betterness
                // reasoning — `M(Base)`, applicable only by subsumption, never competes. fsi
                // confirms `M(Derived)`.
                let hierCtx = hierCtx ()
                let candidates = [| classMember baseTy; classMember derivedTy |]
                let chosen = pickWith hierCtx [||] candidates [ derivedTy ]

                Expect.equal chosen.IsSome true "a unique best exists"
                Expect.equal (chosenParams chosen.Value) [ derivedTy ] "the exact M(Derived) wins"
            }

            test "the subsumption tier admits a supertype when no exact match exists" {
                // `M(Base)` / `M(int)`, argument `Derived`: no exact match, so the applicable
                // tier decides. `Derived :> Base` admits `M(Base)`; `Derived` is unrelated to
                // `int`, so `M(int)` drops out, leaving `M(Base)` the sole survivor.
                let hierCtx = hierCtx ()
                let intClassMember = classMember (TyConst(RuntimeNames.intKey, EqArray.empty))
                let candidates = [| classMember baseTy; intClassMember |]
                let chosen = pickWith hierCtx [||] candidates [ derivedTy ]

                Expect.equal chosen.IsSome true "M(Base) is applicable by subsumption"
                Expect.equal (chosenParams chosen.Value) [ baseTy ] "the supertype parameter is admitted"
            }

            test "an overload on a generic class substitutes the class typar before ranking" {
                // `Box<'a>` with `M('a)` / `M(string)`, at `Box<int>`: `openSignature`
                // substitutes the declaring typar so `M('a)` presents an `int` parameter, and
                // the exact tier then selects by the SUBSTITUTED shape. fsi confirms `M('a)`
                // for an `int` argument and `M(string)` for a `string` argument.
                let overloadCtx = overloadCtx ()

                let candidates =
                    [| boxMember (FTTypar(TyparAxis.Declaring, 0)); boxMember stringFt |]

                let typeArgs = [| BuiltinTypes.tyInt |]

                let atInt = pickWith overloadCtx typeArgs candidates [ BuiltinTypes.tyInt ]
                Expect.equal atInt.IsSome true "the int argument resolves"

                Expect.equal
                    (chosenParamsWith typeArgs atInt.Value)
                    [ BuiltinTypes.tyInt ]
                    "M('a) — the substituted typar slot — wins for int"

                let atString = pickWith overloadCtx typeArgs candidates [ BuiltinTypes.tyString ]
                Expect.equal atString.IsSome true "the string argument resolves"

                Expect.equal
                    (chosenParamsWith typeArgs atString.Value)
                    [ BuiltinTypes.tyString ]
                    "M(string) wins for string"
            }

            test "the non-generic overload is preferred over an equally-applicable generic one" {
                // `M<'a>('a)` / `M(int)`, argument `int`: both are applicable (the method typar
                // binds `int`), their arguments compare equal, so the non-generic tiebreaker
                // selects `M(int)`. A `string` argument instead makes only the generic overload
                // applicable. fsi confirms both.
                let overloadCtx = overloadCtx ()

                let pick candidates args =
                    pickWith overloadCtx [||] candidates args

                let generic = overloadMember [ FTTypar(TyparAxis.Method, 0) ] 1
                let concrete = overloadMember [ intFt ] 0
                let candidates = [| generic; concrete |]

                let atInt = pick candidates [ BuiltinTypes.tyInt ]
                Expect.equal atInt.IsSome true "the int argument resolves"
                Expect.equal atInt.Value.MethodTyparArity 0 "the non-generic M(int) wins for int"

                let atString = pick candidates [ BuiltinTypes.tyString ]
                Expect.equal atString.IsSome true "the string argument resolves"
                Expect.equal atString.Value.MethodTyparArity 1 "only the generic overload matches string"
            }

            test "a user-declared overload resolves by parameter type" {
                // `Show(int)` returns int, `Show(string)` returns bool, so the RESULT type
                // witnesses which overload each call selected. Without overload-by-parameter
                // resolution both calls pick the first `Show`. fsi confirms `Show(1) : int`,
                // `Show("hi") : bool`.
                let input =
                    "type Printer() =\n    member this.Show(x: int) = x\n    member this.Show(x: string) = true\nlet p = Printer()\nlet a = p.Show(1)\nlet b = p.Show(\"hi\")"

                let ctx = analyse input
                Expect.equal (typeOf ctx (keyOfLet input "a")) BuiltinTypes.tyInt "p.Show(1) : int — Show(int)"

                Expect.equal
                    (typeOf ctx (keyOfLet input "b"))
                    BuiltinTypes.tyBool
                    "p.Show(\"hi\") : bool — Show(string)"

                Expect.isEmpty (errors ctx) (sprintf "no errors: %A" (errors ctx))
            }

            test "a user-declared overload resolves by arity" {
                // `M()` returns int, `M(int)` returns string; the call arity selects.
                let input =
                    "type C() =\n    member this.M() = 1\n    member this.M(x: int) = \"s\"\nlet c = C()\nlet a = c.M()\nlet b = c.M(2)"

                let ctx = analyse input
                Expect.equal (typeOf ctx (keyOfLet input "a")) BuiltinTypes.tyInt "c.M() : int"
                Expect.equal (typeOf ctx (keyOfLet input "b")) BuiltinTypes.tyString "c.M(2) : string"
                Expect.isEmpty (errors ctx) (sprintf "no errors: %A" (errors ctx))
            }

            test "static and instance members of the same name do not collide" {
                // A static `M(int)` and an instance `M(string)` share a name but differ in
                // static-ness, so each call resolves to its own member and neither is a
                // duplicate.
                let input =
                    "type C() =\n    static member M(x: int) = \"s\"\n    member this.M(x: string) = 1\nlet a = C.M(1)\nlet c = C()\nlet b = c.M(\"hi\")"

                let ctx = analyse input
                Expect.equal (typeOf ctx (keyOfLet input "a")) BuiltinTypes.tyString "C.M(1) : string — static"
                Expect.equal (typeOf ctx (keyOfLet input "b")) BuiltinTypes.tyInt "c.M(\"hi\") : int — instance"
                Expect.isEmpty (errors ctx) (sprintf "no errors: %A" (errors ctx))
            }

            test "a user overload picks the exact derived parameter over a base" {
                // `M(Base)` / `M(Derived)`, argument `Derived()`: the exact tier selects
                // `M(Derived)`. Return types (int vs bool) witness the pick.
                let input =
                    "type Base() =\n    member this.B = 0\ntype Derived() =\n    inherit Base()\n    member this.D = 0\ntype C() =\n    member this.M(x: Base) = 1\n    member this.M(x: Derived) = true\nlet c = C()\nlet r = c.M(Derived())"

                let ctx = analyse input
                Expect.equal (typeOf ctx (keyOfLet input "r")) BuiltinTypes.tyBool "c.M(Derived()) : bool — M(Derived)"
                Expect.isEmpty (errors ctx) (sprintf "no errors: %A" (errors ctx))
            }

            test "a user overload prefers the more-derived parameter (obj vs Base)" {
                // `M(obj)` / `M(Base)`, argument `Derived()`: neither is exact, both are
                // applicable by subsumption, `Base` is more derived than `obj` and wins.
                let input =
                    "type Base() =\n    member this.B = 0\ntype Derived() =\n    inherit Base()\n    member this.D = 0\ntype C() =\n    member this.M(x: obj) = 1\n    member this.M(x: Base) = true\nlet c = C()\nlet r = c.M(Derived())"

                let ctx = analyse input

                Expect.equal
                    (typeOf ctx (keyOfLet input "r"))
                    BuiltinTypes.tyBool
                    "c.M(Derived()) : bool — M(Base) beats M(obj)"

                Expect.isEmpty (errors ctx) (sprintf "no errors: %A" (errors ctx))
            }

            test "a genuinely duplicate member diagnoses at declaration time" {
                // Two `M(int)` members: same name, static-ness, kind and parameter
                // signature — a duplicate (FS0438), not a legal overload.
                let input =
                    "type C() =\n    member this.M(x: int) = 1\n    member this.M(x: int) = 2"

                let ctx = analyse input

                Expect.isTrue
                    (ctx.Diagnostics
                     |> Seq.exists (fun d -> d.Message.Contains "Duplicate definition of member"))
                    (sprintf "duplicate member diagnosed: %A" (ctx.Diagnostics |> Seq.toList))
            }

            test "an ambiguous user overload diagnoses" {
                // A `Both` value implements two unrelated interfaces `IA` and `IB`; `M(IA)`
                // and `M(IB)` are both applicable by subsumption and neither is more
                // derived, so no unique best exists (FS0041-shape).
                let input =
                    "type IA =\n    abstract member A: int\ntype IB =\n    abstract member B: int\ntype Both() =\n    interface IA with\n        member this.A = 1\n    interface IB with\n        member this.B = 2\ntype C() =\n    member this.M(x: IA) = 1\n    member this.M(x: IB) = 2\nlet c = C()\nlet r = c.M(Both())"

                let ctx = analyse input

                Expect.isTrue
                    (ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Ambiguous call"))
                    (sprintf "ambiguous call diagnosed: %A" (ctx.Diagnostics |> Seq.toList))
            }
        ]
