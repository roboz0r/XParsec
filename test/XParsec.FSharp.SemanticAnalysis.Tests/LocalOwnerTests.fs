module XParsec.FSharp.SemanticAnalysis.Tests.LocalOwnerTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `FrozenPools.LocalOwners` maps every generalised body-local `let` to the declaration whose
// body declares it. A module-level `let` has no row.

/// Every `TyparScope` a frozen type's `FTTypar` leaves carry.
let rec private scopesOf (t: FrozenType) : TyparScope list =
    match t with
    | FTTypar(scope, _) -> [ scope ]
    | t ->
        let found = ResizeArray<TyparScope>()
        FrozenType.iterChildren (fun child -> found.AddRange(scopesOf child)) t
        List.ofSeq found

/// Every scope reached from the file's expression and pattern type columns.
let private allScopes (pools: FrozenPools) : TyparScope list =
    [
        for ty in pools.ExprTys do
            yield! scopesOf pools.Types.[ty]
        for ty in pools.PatTys do
            yield! scopesOf pools.Types.[ty]
    ]

/// The id the local named `name` quantifies under, read off its binding pattern's frozen type.
let private localIdOf (pools: FrozenPools) (name: string) : LocalBindingId =
    let ids =
        [
            for i in 0 .. pools.PatPayloads.Length - 1 do
                match pools.PatPayloads.[i] with
                | PatPayload.NamedSimple(BoundVarId b, _) when pools.BoundVarNames.[b] = name ->
                    for scope in scopesOf pools.Types.[pools.PatTys.[i]] do
                        match scope with
                        | TyparScope.LocalFunction id -> id
                        | TyparScope.Type _
                        | TyparScope.Member _
                        | TyparScope.ModuleFunction _ -> ()
                | _ -> ()
        ]

    match List.distinct ids with
    | [ id ] -> id
    | other -> failtestf "expected one LocalFunction scope on '%s'; found %A" name other

let private ownerOf (pools: FrozenPools) (name: string) : LocalOwner =
    let owners = DenseTable.index pools.LocalOwners

    match owners.TryGetValue(localIdOf pools name) with
    | true, owner -> owner
    | false, _ -> failtestf "no LocalOwners row for '%s'" name

/// The key of the member named `name` on the file's one type declaration.
let private memberKeyOf (pools: FrozenPools) (name: string) : MemberKey =
    let keys =
        [
            for payload in pools.DeclPayloads do
                match payload with
                | DeclPayload.Type td ->
                    for m in TTypeKindG.members td.Kind do
                        if m.Name = name then
                            m.Key
                | DeclPayload.Let _
                | DeclPayload.LetGroup _
                | DeclPayload.Expression _ -> ()
        ]

    match keys with
    | [ key ] -> key
    | other -> failtestf "expected one member named '%s'; found %d" name other.Length

let private source =
    """
type C<'a>() =
    member _.M(y: 'a) =
        let idc z = z
        (idc 1, idc "s", y)

let f (a: int) =
    let g b = b

    let h c =
        let inner d = d
        (inner 1, inner "s", c)

    (g 1, g "s", h a)
"""

let private moduleFunction (name: string) : LocalOwner =
    LocalOwner.ModuleFunction(SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") name)

[<Tests>]
let tests =
    testList
        "LocalOwner"
        [
            test "a module function's local is owned by its BindingKey" {
                let pools = freezeFor source
                Expect.equal (ownerOf pools "g") (moduleFunction "f") "g's owner"
                Expect.equal (ownerOf pools "h") (moduleFunction "f") "h's owner"
            }

            test "a member's local is owned by the member's MemberKey" {
                let pools = freezeFor source
                Expect.equal (ownerOf pools "idc") (LocalOwner.Member(memberKeyOf pools "M")) "idc's owner"
            }

            test "a nested local is owned by the local it is declared in" {
                let pools = freezeFor source
                Expect.equal (ownerOf pools "inner") (LocalOwner.Local(localIdOf pools "h")) "inner's owner"
            }

            test "the owned ids are exactly the file's body-locals" {
                let pools = freezeFor source
                let owned = pools.LocalOwners |> Array.map fst |> Array.sort

                let locals =
                    [ "idc"; "g"; "h"; "inner" ]
                    |> List.map (localIdOf pools)
                    |> List.sort
                    |> Array.ofList

                Expect.equal owned locals "the module-level `f` is a declaration, so it takes no row"
            }

            test "LocalOwners survives the blob codec" {
                let pools = freezeFor source
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)
                Expect.equal thawed.LocalOwners pools.LocalOwners "LocalOwners"
            }

            test "a Type leaf never freezes under a module function's body" {
                let pools = freezeFor "let f (a: int) =\n    let g b = b\n    (g 1, g \"s\", a)\n"

                let typeScopes =
                    allScopes pools
                    |> List.filter (fun s ->
                        match s with
                        | TyparScope.Type _ -> true
                        | _ -> false
                    )

                Expect.isEmpty typeScopes "no Type scope stands under a module function"
            }
        ]
