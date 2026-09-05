module XParsec.FSharp.SemanticAnalysis.Tests.FrozenConstraintTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// A generalised binding's `when` clauses reach the frozen pools as `FrozenConstraint`s over
// the method axis, one per source constraint, and survive the blob codec.

/// The frozen scheme of the module binding named `name`, in stored order.
let private schemeOf (pools: FrozenPools) (name: string) : FrozenConstraint list =
    pools.GenericFnSchemes
    |> Array.tryPick (fun (BoundVarId i, cs) ->
        if pools.BoundVarNames.[i] = name then
            Some(EqSet.toList cs)
        else
            None
    )
    |> Option.defaultWith (fun () -> failtestf "no frozen scheme for %s" name)

/// The bound `kind` on the typar at `i`.
let private at (i: int) (kind: TyparConstraintKindG<'ty>) : TyparConstraintG<'ty> = { TyparIndex = i; Kind = kind }

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
"""

let private expected: (string * FrozenConstraint list) list =
    [
        "eq", [ at 0 TyparConstraintKindG.Equality ]
        "cmp", [ at 0 TyparConstraintKindG.Comparison ]
        "st", [ at 0 TyparConstraintKindG.Struct ]
        "notSt", [ at 0 TyparConstraintKindG.ReferenceType ]
        "nul", [ at 0 TyparConstraintKindG.Nullness ]
        "notNul", [ at 0 TyparConstraintKindG.NotNull ]
        "two", [ at 0 TyparConstraintKindG.Struct; at 1 TyparConstraintKindG.Comparison ]
        "newable", [ at 0 TyparConstraintKindG.DefaultConstructor ]
        "unmanaged", [ at 0 TyparConstraintKindG.Unmanaged ]
        "enumOf", [ at 0 (TyparConstraintKindG.Enum RuntimeNames.intTy) ]
        // No `delegate<_,_>` entry: `Translate` refuses the clause, so codec tag 10 is unpinned.
        // Inferred from the body rather than declared, and propagated through a call.
        "inferredEq", [ at 0 TyparConstraintKindG.Equality ]
        "inferredCmp", [ at 0 TyparConstraintKindG.Comparison ]
        "viaCall", [ at 0 TyparConstraintKindG.Comparison ]
    ]

[<Tests>]
let tests =
    testList
        "FrozenConstraint"
        [
            for name, constraints in expected do
                test (sprintf "%s freezes its constraint over the method axis" name) {
                    let pools = freezeFor source
                    Expect.equal (schemeOf pools name) constraints "frozen scheme"
                }

            test "a coercion freezes its target over the method axis" {
                let pools = freezeFor source

                match schemeOf pools "coerce" with
                | [ {
                        TyparIndex = 0
                        Kind = TyparConstraintKindG.Coercion(FTConst(key, _))
                    } ] -> Expect.equal key.Name "exn" "coercion target"
                | other -> failtestf "unexpected scheme %A" other
            }

            test "every kind survives the blob codec" {
                let pools = freezeFor source
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)

                for name in "coerce" :: List.map fst expected do
                    Expect.equal (schemeOf thawed name) (schemeOf pools name) name
            }
        ]

// A type declaration, a member and an abstract slot carry their own typars' bounds on the
// declaration itself, on the declaring axis for the type and the method axis for the rest.

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
            test "a record's declared bound is on the declaring axis" {
                let td = typeDeclOf (freezeFor declSource) "Box"

                Expect.equal
                    (EqSet.toList td.TyparConstraints)
                    [ at 0 TyparConstraintKindG.Comparison ]
                    "type-level bound"
            }

            test "a bound inferred from a member body lands on the declaring typar" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (EqSet.toList td.TyparConstraints)
                    [ at 0 TyparConstraintKindG.Equality ]
                    "type-level bound"

                Expect.isTrue (memberOf td "Same").MethodTyparConstraints.IsEmpty "no method-axis bound"
            }

            test "a member's declared bound is on the method axis" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (EqSet.toList (memberOf td "Pick").MethodTyparConstraints)
                    [ at 0 TyparConstraintKindG.Struct ]
                    "method bound"
            }

            test "a member's inferred bound is on the method axis" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (EqSet.toList (memberOf td "Less").MethodTyparConstraints)
                    [ at 0 TyparConstraintKindG.Comparison ]
                    "method bound"
            }

            test "an abstract slot's declared bound is on the method axis" {
                let td = typeDeclOf (freezeFor declSource) "IShape"

                match td.Kind with
                | TTypeKindG.Interface methods ->
                    Expect.equal
                        (EqArray.toList methods
                         |> List.map (fun m -> EqSet.toList m.MethodTyparConstraints))
                        [ [ at 0 TyparConstraintKindG.ReferenceType ] ]
                        "slot bound"
                | _ -> failtest "IShape is not an interface"
            }

            test "declaration bounds survive the blob codec" {
                let pools = freezeFor declSource
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)

                for name in [ "Box"; "Holder"; "IShape" ] do
                    Expect.equal (typeDeclOf thawed name) (typeDeclOf pools name) name
            }
        ]
