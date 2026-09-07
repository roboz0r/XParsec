module XParsec.FSharp.SemanticAnalysis.Tests.FrozenConstraintTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// A generalised binding's `when` clauses reach the frozen pools on the scheme's own typars,
// one `ConstraintSet` per typar, and survive the blob codec.

/// The scheme of the module binding named `name`; `None` for a binding absent from the table.
let private tryFrozenSchemeOf (pools: FrozenPools) (name: string) : FunctionScheme option =
    pools.FunctionSchemes
    |> Array.tryPick (fun (BoundVarId i, s) -> if pools.BoundVarNames.[i] = name then Some s else None)

let private frozenSchemeOf (pools: FrozenPools) (name: string) : FunctionScheme =
    tryFrozenSchemeOf pools name
    |> Option.defaultWith (fun () -> failtestf "no frozen scheme for %s" name)

/// Each typar's constraints, in stored order, of `typars`.
let private constraintsOf (typars: TyparList) : TyparConstraintKindG<FrozenType> list list =
    [ for t in typars.Types -> EqSet.toList t.Constraints.Kinds ]

/// The constraints of the module binding named `name`, per typar.
let private schemeOf (pools: FrozenPools) (name: string) : TyparConstraintKindG<FrozenType> list list =
    constraintsOf (frozenSchemeOf pools name).Typars

let private source =
    """
let eq<'a when 'a: equality> (x: 'a) = x
let cmp<'a when 'a: comparison> (x: 'a) = x
let st<'a when 'a: struct> (x: 'a) = x
let notSt<'a when 'a: not struct> (x: 'a) = x
let nul<'a when 'a: null> (x: 'a) = x
let notNul<'a when 'a: not null> (x: 'a) = x
let coerce<'e when 'e :> exn> (x: 'e) = x
let two<'a, 'b when 'a: struct and 'b: comparison> (x: 'a) (y: 'b) = y
let newable<'a when 'a: (new: unit -> 'a)> (x: 'a) = x
let unmanaged<'a when 'a: unmanaged> (x: 'a) = x
let enumOf<'a when 'a: enum<int>> (x: 'a) = x
let inferredEq x y = x = y
let inferredCmp x y = x < y
let viaCall x = inferredCmp x x
let mono (x: int) = x
"""

let private expected: (string * TyparConstraintKindG<FrozenType> list list) list =
    [
        "eq", [ [ TyparConstraintKindG.Equality ] ]
        "cmp", [ [ TyparConstraintKindG.Comparison ] ]
        "st", [ [ TyparConstraintKindG.Struct ] ]
        "notSt", [ [ TyparConstraintKindG.ReferenceType ] ]
        "nul", [ [ TyparConstraintKindG.Nullness ] ]
        "notNul", [ [ TyparConstraintKindG.NotNull ] ]
        "two", [ [ TyparConstraintKindG.Struct ]; [ TyparConstraintKindG.Comparison ] ]
        "newable", [ [ TyparConstraintKindG.DefaultConstructor ] ]
        "unmanaged", [ [ TyparConstraintKindG.Unmanaged ] ]
        "enumOf", [ [ TyparConstraintKindG.Enum RuntimeNames.intTy ] ]
        // No `delegate<_,_>` entry: `Translate` refuses the clause, so codec tag 10 is unpinned.
        // Inferred from the body rather than declared, and propagated through a call.
        "inferredEq", [ [ TyparConstraintKindG.Equality ] ]
        "inferredCmp", [ [ TyparConstraintKindG.Comparison ] ]
        "viaCall", [ [ TyparConstraintKindG.Comparison ] ]
    ]

/// A scheme over `n` positional typars, the first constrained by `kind`.
let private schemeWith (n: int) (kind: TyparConstraintKindG<FrozenType>) : FunctionScheme =
    FunctionScheme.ofTypars (
        TyparList.positionalWith
            (fun i ->
                if i = 0 then
                    ConstraintSet.ofKinds [ kind ]
                else
                    ConstraintSet.empty
            )
            n
    )

[<Tests>]
let tests =
    testList
        "FrozenConstraint"
        [
            for name, constraints in expected do
                test (sprintf "%s freezes its constraint on its own typar" name) {
                    let pools = freezeFor source
                    Expect.equal (schemeOf pools name) constraints "frozen scheme"
                }

            test "a coercion freezes its target over the method scope" {
                let pools = freezeFor source

                match schemeOf pools "coerce" with
                | [ [ TyparConstraintKindG.Coercion(FTConst(key, _)) ] ] ->
                    Expect.equal key.Name "exn" "coercion target"
                | other -> failtestf "unexpected scheme %A" other
            }

            test "every kind survives the blob codec" {
                let pools = freezeFor source
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)

                for name in "coerce" :: List.map fst expected do
                    Expect.equal (frozenSchemeOf thawed name) (frozenSchemeOf pools name) name
            }

            test "a scheme's arity is the binding's quantified typar count" {
                let pools = freezeFor source
                Expect.equal (frozenSchemeOf pools "eq").TyparArity 1 "eq"
                Expect.equal (frozenSchemeOf pools "two").TyparArity 2 "two"
                Expect.equal (frozenSchemeOf pools "inferredEq").TyparArity 1 "inferredEq"
            }

            test "a monomorphic binding has no scheme" {
                let pools = freezeFor source
                Expect.isNone (tryFrozenSchemeOf pools "mono") "mono"
            }

            test "a constraint whose type references a method typar past the arity is refused" {
                let target =
                    FTTypar(TyparScope.ModuleFunction(SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") "f"), 1)

                Expect.throws
                    (fun () -> schemeWith 1 (TyparConstraintKindG.Coercion target) |> ignore)
                    "method typar referenced past the arity"
            }

            test "a trait on a typar at or past the arity is refused" {
                let trait_: MemberTrait =
                    {
                        TyparIndices = EqArray.singleton 1
                        MemberName = "op_Addition"
                        ArgTypes = EqArray.empty
                        ReturnType = RuntimeNames.intTy
                    }

                Expect.throws
                    (fun () ->
                        FunctionScheme.create (TyparList.positional 1) (EqArray.singleton trait_)
                        |> ignore
                    )
                    "trait index past the arity"
            }
        ]

// A type declaration, a member and an abstract slot carry their own typars' constraints on the
// typars themselves, in the type scope for the type and the method scope for the rest.

let private declSource =
    """
type Box<'a when 'a: comparison> = { Item: 'a }

type Holder<'a>() =
    member _.Same (x: 'a) (y: 'a) = x = y
    member _.Pick<'b when 'b: struct> (b: 'b) = b
    member _.Less x y = x < y

type IShape =
    abstract Map<'c when 'c: not struct> : 'c -> 'c
"""

let private typeDeclOf (pools: FrozenPools) (name: string) : Pooled.TTypeDecl =
    EqArray.toList (TastUnpool.ofPools pools).Decls
    |> List.pick (
        function
        | TDeclG.Type td when td.Name = name -> Some td
        | _ -> None
    )

let private memberOf (td: Pooled.TTypeDecl) (name: string) : Pooled.TTypeMember =
    TTypeKindG.members td.Kind
    |> EqArray.toList
    |> List.find (fun m -> m.Name = name)

[<Tests>]
let declTests =
    testList
        "TyparConstraints on declarations"
        [
            test "a record's declared constraint is on the declaring typar" {
                let td = typeDeclOf (freezeFor declSource) "Box"

                Expect.equal
                    (constraintsOf td.TypeParams)
                    [ [ TyparConstraintKindG.Comparison ] ]
                    "type-level constraint"
            }

            test "a constraint inferred from a member body lands on the declaring typar" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal (constraintsOf td.TypeParams) [ [ TyparConstraintKindG.Equality ] ] "type-level constraint"

                Expect.isFalse (memberOf td "Same").MethodTypars.HasConstraints "no method-scope constraint"
            }

            test "a member's declared constraint is on its own typar" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (constraintsOf (memberOf td "Pick").MethodTypars)
                    [ [ TyparConstraintKindG.Struct ] ]
                    "method constraint"
            }

            test "a member's inferred constraint is on its own typar" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (constraintsOf (memberOf td "Less").MethodTypars)
                    [ [ TyparConstraintKindG.Comparison ] ]
                    "method constraint"
            }

            test "an abstract slot's declared constraint is on its own typar" {
                let td = typeDeclOf (freezeFor declSource) "IShape"

                match td.Kind with
                | TTypeKindG.Interface methods ->
                    Expect.equal
                        (EqArray.toList methods |> List.map (fun m -> constraintsOf m.MethodTypars))
                        [ [ [ TyparConstraintKindG.ReferenceType ] ] ]
                        "slot constraint"
                | _ -> failtest "IShape is not an interface"
            }

            test "declaration constraints survive the blob codec" {
                let pools = freezeFor declSource
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)

                for name in [ "Box"; "Holder"; "IShape" ] do
                    Expect.equal (typeDeclOf thawed name) (typeDeclOf pools name) name
            }
        ]

// Two `fsc` parity gaps in how a scheme's typars are ordered and merged, each pinned as it
// stands. `fsc` orders a binding's typars by first appearance in the source INCLUDING its
// `when` clauses; this compiler orders them by appearance in the binding's type, then the
// constraint-only ones. Two coercions on one typar to the same generic interface unify their
// arguments under FS0064 in `fsc`; this compiler keeps both.

/// The frozen type of the module binding named `name`.
let private declTypeOf (pools: FrozenPools) (name: string) : FrozenType =
    EqArray.toList (TastUnpool.ofPools pools).Decls
    |> List.pick (
        function
        | TDeclG.Let({ Pattern = TPatG.NamedSimple(BoundVarId b, _, _, _) } as m, _, _) when
            pools.BoundVarNames.[b] = name
            ->
            Some m.Ty
        | _ -> None
    )

let private paritySource =
    """
type IStructSeq<'T, 'E> =
    abstract Enumerator: unit -> 'E

type IFun<'T, 'U> =
    abstract Invoke: 'T -> 'U

type MapSeq<'S, 'E, 'TFunc, 'T, 'U> = { Source: 'S; F: 'TFunc }

let map (f: 'TFunc when 'TFunc :> IFun<'T, 'U>) (source: 'S when 'S :> IStructSeq<'T, 'E>) : MapSeq<'S, 'E, 'TFunc, 'T, 'U> =
    { Source = source; F = f }

type IBox<'t> =
    abstract Value: 't

let twice (x: 'a when 'a :> IBox<int> and 'a :> IBox<'b>) = x
"""

[<Tests>]
let fscParityTests =
    testList
        "fsc typar-order and FS0064 parity"
        [
            ptest
                "gap: fsc orders `map`'s typars TFunc, T, U, S, E by first appearance including the when clauses; this compiler gives TFunc, S, E, T, U" {
                let pools = freezeFor paritySource

                let scope =
                    TyparScope.ModuleFunction(SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") "map")

                let at (i: int) = FTTypar(scope, i)

                let mapSeq =
                    match declTypeOf pools "map" with
                    | FTFun(_, FTFun(_, result)) -> result
                    | other -> failtestf "map's type is not a two-argument function: %A" other

                // TFunc = 0, T = 1, U = 2, S = 3, E = 4.
                match mapSeq with
                | FTRecord(_, args) ->
                    Expect.equal (EqArray.toList args) [ at 3; at 4; at 0; at 1; at 2 ] "MapSeq<'S, 'E, 'TFunc, 'T, 'U>"
                | other -> failtestf "map does not return a record: %A" other
            }

            ptest "gap: fsc unifies two coercions to one generic interface under FS0064; this compiler keeps both" {
                let pools = freezeFor paritySource
                let scheme = frozenSchemeOf pools "twice"

                Expect.equal scheme.TyparArity 1 "`'b` is solved to `int`, leaving one typar"

                match constraintsOf scheme.Typars with
                | [ [ TyparConstraintKindG.Coercion(FTClass(key, args)) ] ] ->
                    Expect.equal key.Name "IBox" "one coercion remains"
                    Expect.equal (EqArray.toList args) [ RuntimeNames.intTy ] "at IBox<int>"
                | other -> failtestf "unexpected constraints %A" other
            }
        ]
