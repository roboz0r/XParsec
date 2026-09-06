module XParsec.FSharp.SemanticAnalysis.Tests.FrozenConstraintTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// A generalised binding's `when` clauses reach the frozen pools as `FrozenConstraint`s over
// the method scope, one per source constraint, and survive the blob codec.

/// The scheme of the module binding named `name`; `None` for a binding absent from the table.
let private tryFrozenSchemeOf (pools: FrozenPools) (name: string) : GenericFnScheme option =
    pools.GenericFnSchemes
    |> Array.tryPick (fun (BoundVarId i, s) -> if pools.BoundVarNames.[i] = name then Some s else None)

let private frozenSchemeOf (pools: FrozenPools) (name: string) : GenericFnScheme =
    tryFrozenSchemeOf pools name
    |> Option.defaultWith (fun () -> failtestf "no frozen scheme for %s" name)

/// The constraints of the module binding named `name`, in stored order.
let private schemeOf (pools: FrozenPools) (name: string) : FrozenConstraint list =
    EqSet.toList (frozenSchemeOf pools name).Constraints

/// The constraint `kind` on the typar at `i`.
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
let mono (x: int) = x
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
                test (sprintf "%s freezes its constraint over the method scope" name) {
                    let pools = freezeFor source
                    Expect.equal (schemeOf pools name) constraints "frozen scheme"
                }

            test "a coercion freezes its target over the method scope" {
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

            test "a constraint on a typar at or past the arity is refused" {
                Expect.throws
                    (fun () ->
                        GenericFnScheme.create 1 (EqSet.ofSeq [ at 1 TyparConstraintKindG.Equality ])
                        |> ignore
                    )
                    "TyparIndex past the arity"
            }

            test "a constraint whose type references a method typar past the arity is refused" {
                let target =
                    FTTypar(TyparScope.ModuleFunction(SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") "f"), 1)

                Expect.throws
                    (fun () ->
                        GenericFnScheme.create 1 (EqSet.ofSeq [ at 0 (TyparConstraintKindG.Coercion target) ])
                        |> ignore
                    )
                    "method typar referenced past the arity"
            }
        ]

// A type declaration, a member and an abstract slot carry their own typars' constraints on the
// declaration itself, in the type scope for the type and the method scope for the rest.

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
            test "a record's declared constraint is in the type scope" {
                let td = typeDeclOf (freezeFor declSource) "Box"

                Expect.equal
                    (EqSet.toList td.TyparConstraints)
                    [ at 0 TyparConstraintKindG.Comparison ]
                    "type-level constraint"
            }

            test "a constraint inferred from a member body lands on the declaring typar" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (EqSet.toList td.TyparConstraints)
                    [ at 0 TyparConstraintKindG.Equality ]
                    "type-level constraint"

                Expect.isTrue (memberOf td "Same").MethodTyparConstraints.IsEmpty "no method-scope constraint"
            }

            test "a member's declared constraint is in the method scope" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (EqSet.toList (memberOf td "Pick").MethodTyparConstraints)
                    [ at 0 TyparConstraintKindG.Struct ]
                    "method constraint"
            }

            test "a member's inferred constraint is in the method scope" {
                let td = typeDeclOf (freezeFor declSource) "Holder"

                Expect.equal
                    (EqSet.toList (memberOf td "Less").MethodTyparConstraints)
                    [ at 0 TyparConstraintKindG.Comparison ]
                    "method constraint"
            }

            test "an abstract slot's declared constraint is in the method scope" {
                let td = typeDeclOf (freezeFor declSource) "IShape"

                match td.Kind with
                | TTypeKindG.Interface methods ->
                    Expect.equal
                        (EqArray.toList methods
                         |> List.map (fun m -> EqSet.toList m.MethodTyparConstraints))
                        [ [ at 0 TyparConstraintKindG.ReferenceType ] ]
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
