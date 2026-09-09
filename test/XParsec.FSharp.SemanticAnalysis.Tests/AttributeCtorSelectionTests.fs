module XParsec.FSharp.SemanticAnalysis.Tests.AttributeCtorSelectionTests

open Vesper
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Constructor selection for a written attribute: the overload its arguments select, what each
// argument fills, the `null` / `[||]` / `obj` positions, and fsc's codes for the arguments no
// constructor takes. Whole-pipeline (`freezeFor`), and through the codec the target survives.

let private src (lines: string list) = String.concat "\n" lines

let private typeDecl = frozenTypeDecl

let private scalar (v: TConstValue) : TConstDenotation =
    {
        Result = TConstResult.Scalar v
        Ty = FTConst(TConstValue.canonKey v, Block.empty)
    }

/// A `typeof<T>` / `typedefof<T>` argument: the reified type, at `Vesper.Type`.
let private reified (operand: FrozenType) : TConstDenotation =
    {
        Result = TConstResult.TypeVal operand
        Ty = FTConst(RuntimeNames.runtimeTypeKey, Block.empty)
    }

let private int32 (v: int) : TConstValue = TConstValue.Integral(IntValue.Int32 v)

[<Tests>]
let tests =
    testList
        "Attribute constructor selection"
        [
            test "constructor selection records the overload the argument's own type selects" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type AAttribute(n: int) ="
                                "    member this.N = n"
                                "    new(n: int64) = AAttribute(0)"
                                "    new(s: string) = AAttribute(0)"
                                ""
                                "[<A(1)>]"
                                "type I = { X: int }"
                                ""
                                "[<A(1L)>]"
                                "type L = { X: int }"
                                ""
                                "[<A(\"s\")>]"
                                "type S = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "each literal selects one overload"
                let aKey = (typeDecl pools "AAttribute").TypeKey

                let ctorOf (key: TypeKey) (paramTy: FrozenType) =
                    SymbolKeyOps.ctorKeyOf key (Block.singleton paramTy) 0<typeSlot>

                let ctorOn (name: string) =
                    match Block.toList (typeDecl pools name).Attributes with
                    | [ a ] -> a.Ctor
                    | other -> failtestf "expected one attribute on %s, got %d" name (List.length other)

                Expect.equal
                    (ctorOn "I")
                    (ctorOf aKey (FTConst(RuntimeNames.intKey, Block.empty)))
                    "1 selects new(n: int)"

                Expect.equal
                    (ctorOn "L")
                    (ctorOf aKey (FTConst(RuntimeNames.int64Key, Block.empty)))
                    "1L selects new(n: int64)"

                Expect.equal
                    (ctorOn "S")
                    (ctorOf aKey (FTConst(RuntimeNames.stringKey, Block.empty)))
                    "\"s\" selects new(s: string)"
            }

            test "an obj overload yields to the typed one and admits what no other does" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type MAttribute(x: obj) ="
                                "    member this.X = x"
                                "    new(s: string) = MAttribute(null)"
                                ""
                                "[<M(null)>]"
                                "type N = { X: int }"
                                ""
                                "[<M(\"s\")>]"
                                "type S = { X: int }"
                                ""
                                "[<M(1)>]"
                                "type I = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "every use selects one overload"

                let argSigOn (name: string) =
                    match Block.toList (typeDecl pools name).Attributes with
                    | [ a ] -> Block.toList a.Ctor.ArgSig
                    | other -> failtestf "expected one attribute on %s, got %d" name (List.length other)

                let stringTy = FTConst(RuntimeNames.stringKey, Block.empty)
                let objTy = FTConst(RuntimeNames.objKey, Block.empty)

                Expect.equal (argSigOn "N") [ stringTy ] "null takes new(s: string) over new(x: obj)"
                Expect.equal (argSigOn "S") [ stringTy ] "\"s\" takes new(s: string) over new(x: obj)"
                Expect.equal (argSigOn "I") [ objTy ] "1 is admitted by new(x: obj) alone"

                match Block.toList (typeDecl pools "I").Attributes with
                | [ a ] ->
                    Expect.equal
                        [ for arg in a.Args -> arg.Target, TConstExpr.denotation arg.Expr ]
                        [ TAttributeArgTarget.Parameter 0, scalar (int32 1) ]
                        "the argument keeps its own type; the boxing is the position's"
                | other -> failtestf "expected one attribute on I, got %d" (List.length other)
            }

            test "null between a string and a Type overload is FS0041" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type BAttribute(x: string) ="
                                "    member this.X = x"
                                "    new(t: Type) = BAttribute(\"\")"
                                ""
                                "[<B(null)>]"
                                "type W = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "A unique overload" "FS0041 at the attribute"
                | other -> failtestf "expected exactly one error, got %A" other

                Expect.isEmpty (Block.toList (typeDecl pools "W").Attributes) "the ambiguous attribute is dropped whole"
            }

            test "a named argument fills a trailing parameter, recorded at the parameter's index" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type S3Attribute(x: int, y: int) ="
                                "    member this.X = x"
                                ""
                                "[<S3(y = 2, x = 1)>]"
                                "type Named = { X: int }"
                                ""
                                "[<S3(1, y = 5)>]"
                                "type Trailing = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "both spellings fill new(x, y)"

                let targets (name: string) =
                    match Block.toList (typeDecl pools name).Attributes with
                    | [ a ] -> [ for arg in a.Args -> arg.Target, TConstExpr.denotation arg.Expr ]
                    | other -> failtestf "expected one attribute on %s, got %d" name (List.length other)

                Expect.equal
                    (targets "Named")
                    [
                        TAttributeArgTarget.Parameter 1, scalar (int32 2)
                        TAttributeArgTarget.Parameter 0, scalar (int32 1)
                    ]
                    "each named argument records the parameter it fills, in written order"

                Expect.equal
                    (targets "Trailing")
                    [
                        TAttributeArgTarget.Parameter 0, scalar (int32 1)
                        TAttributeArgTarget.Parameter 1, scalar (int32 5)
                    ]
                    "a positional argument then a named parameter"
            }

            test "a named argument matching a parameter of one overload and a property is FS0041" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type WAttribute(x: int) ="
                                "    member this.X = x"
                                "    new(x: int, y: int) = WAttribute(x)"
                                "    member val y = 0 with get, set"
                                ""
                                "[<W(1, y = 2)>]"
                                "type A = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "A unique overload" "FS0041, as fsc reports"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "a positional count no constructor takes reports fsc's own three" {
                let errorsOf (lines: string list) =
                    FrozenPools.blockingErrors (freezeFor (src lines))
                    |> List.map (fun d -> d.Message)

                match
                    errorsOf
                        [
                            "type S2Attribute(x: int, y: int) ="
                            "    member this.X = x"
                            ""
                            "[<S2(1)>]"
                            "type A = { X: int }"
                        ]
                with
                | [ msg ] -> Expect.stringContains msg "requires 2 argument(s)" "FS0496: one constructor, too few"
                | other -> failtestf "expected exactly one error, got %A" other

                match
                    errorsOf
                        [
                            "type S2Attribute(x: int, y: int) ="
                            "    member this.X = x"
                            ""
                            "[<S2(1, 2, 3)>]"
                            "type A = { X: int }"
                        ]
                with
                | [ msg ] ->
                    Expect.stringContains
                        msg
                        "takes 2 argument(s) but is here given 3"
                        "FS0501: one constructor, too many"
                | other -> failtestf "expected exactly one error, got %A" other

                match
                    errorsOf
                        [
                            "type S3Attribute(x: int, y: int) ="
                            "    member this.X = x"
                            "    new(x: int) = S3Attribute(x, 0)"
                            ""
                            "[<S3(1, 2, 3)>]"
                            "type A = { X: int }"
                        ]
                with
                | [ msg ] -> Expect.stringContains msg "does not take 3 argument(s)" "FS0505: several constructors"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "a named argument matching no parameter or member is FS0495" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type PAttribute() ="
                                "    member this.X = 1"
                                ""
                                "[<P(Nope = 3)>]"
                                "type A = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains msg "no argument or settable return property 'Nope'" "FS0495 at the name"
                | other -> failtestf "expected exactly one error, got %A" other

                Expect.isEmpty (Block.toList (typeDecl pools "A").Attributes) "the attribute is dropped whole"
            }

            test "a constant of another type than the one constructor's parameter is FS0001 at the argument" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type NAttribute(x: int64) ="
                                "    member this.X = x"
                                ""
                                "[<N(1)>]"
                                "type A = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains
                        msg
                        "expected to have type 'int64' but here has type 'int'"
                        "a literal keeps its own type: no widening into the parameter"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "null against a parameter with no null value is FS0043" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type UAttribute(x: int) ="
                                "    member this.X = x"
                                ""
                                "[<U(null)>]"
                                "type A = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] -> Expect.stringContains msg "does not have 'null' as a proper value" "FS0043 at null"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "an unannotated constructor parameter is refused at the use" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type UAttribute(n) ="
                                "    member this.N = n"
                                ""
                                "[<U(1)>]"
                                "type A = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains msg "has no type annotation" "the parameter's declared type is required"
                | other -> failtestf "expected exactly one error, got %A" other
            }

            test "null, an empty array and obj positions take their types from the constructor, through the codec" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type FAttribute(o: obj, s: string, xs: int[], os: obj[]) ="
                                "    member this.O = o"
                                ""
                                "[<F(1, null, [||], [| 1; \"a\"; null; typeof<int> |])>]"
                                "type A = { X: int }"
                                ""
                                "[<F(typeof<int>, \"s\", null, [||])>]"
                                "type B = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "every position admits its argument"
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)

                let intTy = FTConst(RuntimeNames.intKey, Block.empty)
                let stringTy = FTConst(RuntimeNames.stringKey, Block.empty)
                let objTy = FTConst(RuntimeNames.objKey, Block.empty)

                let argsOn (name: string) =
                    match Block.toList (typeDecl thawed name).Attributes with
                    | [ a ] -> [ for arg in a.Args -> arg.Target, TConstExpr.denotation arg.Expr ]
                    | other -> failtestf "expected one attribute on %s, got %d" name (List.length other)

                Expect.equal
                    (argsOn "A")
                    [
                        TAttributeArgTarget.Parameter 0, scalar (int32 1)
                        TAttributeArgTarget.Parameter 1,
                        {
                            Result = TConstResult.Null
                            Ty = stringTy
                        }
                        TAttributeArgTarget.Parameter 2,
                        {
                            Result = TConstResult.ArrayVal Block.empty
                            Ty = ftArray intTy
                        }
                        TAttributeArgTarget.Parameter 3,
                        {
                            Result =
                                TConstResult.ArrayVal(
                                    Block.ofList
                                        [
                                            TConstResult.Scalar(int32 1)
                                            TConstResult.Scalar(TConstValue.String "a")
                                            TConstResult.Null
                                            TConstResult.TypeVal intTy
                                        ]
                                )
                            Ty = ftArray objTy
                        }
                    ]
                    "1 keeps int at obj; null is string; [||] is int[]; the obj[] items keep their own types"

                Expect.equal
                    (argsOn "B")
                    [
                        TAttributeArgTarget.Parameter 0, reified intTy
                        TAttributeArgTarget.Parameter 1, scalar (TConstValue.String "s")
                        TAttributeArgTarget.Parameter 2,
                        {
                            Result = TConstResult.Null
                            Ty = ftArray intTy
                        }
                        TAttributeArgTarget.Parameter 3,
                        {
                            Result = TConstResult.ArrayVal Block.empty
                            Ty = ftArray objTy
                        }
                    ]
                    "typeof<int> at obj; null is int[]; [||] is obj[]"
            }

            test "a named argument sets a property or a field, recorded with the member's type" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type PAttribute() ="
                                "    let mutable p: obj = null"
                                "    [<DefaultValue>] val mutable F: int"
                                "    member this.P with get () = p and set (v: obj) = p <- v"
                                ""
                                "[<P(P = \"s\", F = 3)>]"
                                "type A = { X: int }"
                            ]
                    )

                Expect.isEmpty (FrozenPools.blockingErrors pools) "both names resolve on the class"
                let intTy = FTConst(RuntimeNames.intKey, Block.empty)
                let objTy = FTConst(RuntimeNames.objKey, Block.empty)

                match Block.toList (typeDecl pools "A").Attributes with
                | [ a ] ->
                    Expect.equal
                        [ for arg in a.Args -> arg.Target, TConstExpr.denotation arg.Expr ]
                        [
                            TAttributeArgTarget.Member(TAttributeMember.Property("P", objTy)),
                            scalar (TConstValue.String "s")
                            TAttributeArgTarget.Member(TAttributeMember.Field("F", intTy)), scalar (int32 3)
                        ]
                        "the property keeps the argument's own type under obj; the field is typed as declared"
                | other -> failtestf "expected one attribute on A, got %d" (List.length other)
            }

            ptest "GAP: a referenced class's constructor parameter cannot be filled by name" {
                // fsc accepts `validOn = ...`, reading the parameter name from metadata.
                // `ExternalMember` carries no parameter names, so the name resolves against
                // the class's members alone and reports FS0495.
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(validOn = AttributeTargets.Class)>]"
                                "type MyAttribute() ="
                                "    member this.X = 1"
                            ]
                    )

                Expect.isEmpty
                    (FrozenPools.blockingErrors pools)
                    "a referenced constructor's parameter is filled by its written name"
            }

            ptest "GAP: a getter-only property is settable by name" {
                // fsc reports FS0495 for `N = 2`: `N` has no setter. Settability is recorded
                // on neither `TypeMemberInfo` nor `ExternalMember`, so any property or field
                // of the class matching the name is accepted.
                let pools =
                    freezeFor (
                        src
                            [
                                "type MarkAttribute() ="
                                "    member this.N = 1"
                                ""
                                "[<Mark(N = 2)>]"
                                "type A = { X: int }"
                            ]
                    )

                match FrozenPools.blockingErrors pools |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.stringContains msg "no argument or settable return property 'N'" "FS0495 at the name"
                | other -> failtestf "expected exactly one error, got %A" other
            }
        ]
