module XParsec.FSharp.SemanticAnalysis.Tests.LocalOwnerTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `FrozenPools.LocalOwners` maps every generalised body-local `let` to the declaration whose
// body declares it. A module-level `let` has no row. `FrozenPools.LocalSchemes` maps a local's
// bound variable to the scope of its own typars.

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

/// The bound variable named `name`.
let private boundVarOf (pools: FrozenPools) (name: string) : BoundVarId =
    match
        [
            for i in 0 .. pools.BoundVarNames.Length - 1 do
                if pools.BoundVarNames.[i] = name then
                    BoundVarId i
        ]
    with
    | [ b ] -> b
    | other -> failtestf "expected one bound variable named '%s'; found %d" name other.Length

let private schemeOf (pools: FrozenPools) (name: string) : LocalScheme =
    let schemes = DenseTable.index pools.LocalSchemes

    match schemes.TryGetValue(boundVarOf pools name) with
    | true, scheme -> scheme
    | false, _ -> failtestf "no LocalSchemes row for '%s'" name

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

/// `Cross.Lib.served`, a splice template with one generalised body-local, and the consuming
/// file that expands it. The consumer's own copy of the local is what the assertions read.
let private servedTemplate =
    "\
namespace Cross

module Lib =
    let inline served (x: int) =
        let g y = y
        (g x, g \"a\")
"

let private servedConsumer =
    "\
namespace Cross

module Use =
    let consume () = Lib.served 7
"

/// The last unit of a two-file assembly, frozen.
let private freezeConsumer (files: (string * string) list) : FrozenPools =
    (analyseFiles "LocalOwnerCross" files |> analysedFiles |> List.last).Frozen

/// Every `FTUnknown` reason the file's expression and pattern type columns carry.
let private unknownsIn (pools: FrozenPools) : UnknownReason list =
    let rec reasons (t: FrozenType) : UnknownReason list =
        match t with
        | FTUnknown reason -> [ reason ]
        | t ->
            let found = ResizeArray<UnknownReason>()
            FrozenType.iterChildren (fun child -> found.AddRange(reasons child)) t
            List.ofSeq found

    [
        for ty in pools.ExprTys do
            yield! reasons pools.Types.[ty]
        for ty in pools.PatTys do
            yield! reasons pools.Types.[ty]
    ]

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
                // `source` covers `Member`, `ModuleFunction` and `Local`; the consumer covers
                // `Spliced`.
                for pools in
                    [
                        freezeFor source
                        freezeConsumer [ "lib.fs", servedTemplate; "use.fs", servedConsumer ]
                    ] do
                    let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)
                    Expect.equal thawed.LocalOwners pools.LocalOwners "LocalOwners"
                    Expect.equal thawed.LocalSchemes pools.LocalSchemes "LocalSchemes"
            }

            test "a local's scheme is the scope its own leaves carry" {
                let pools = freezeFor source

                for name in [ "idc"; "g"; "h"; "inner" ] do
                    Expect.equal (schemeOf pools name).Id (localIdOf pools name) (name + "'s scope")
                    Expect.equal (schemeOf pools name).TyparArity 1<typeSlot> (name + " quantifies one typar")
            }

            test "a same-file inline body's local is the same binding under each spliced copy" {
                let pools =
                    freezeFor "let inline f (x: int) =\n    let g y = y\n    (g x, g \"s\")\nlet a = f 1\nlet b = f 2\n"

                let rows =
                    pools.LocalSchemes
                    |> Array.map (fun (BoundVarId b, s) -> pools.BoundVarNames.[b], s)
                    |> Array.sortBy fst

                // Both call sites ground `f` at one instantiation, so they share one entry.
                Expect.equal (Array.length rows) 2 "the template's `g` and the specialization's copy"

                Expect.equal
                    (rows |> Array.map (fun (_, s) -> s.Id) |> Array.distinct |> Array.length)
                    1
                    "one LocalBindingId"

                Expect.equal (fst rows.[rows.Length - 1]) "g" "the template's own row is named"
                Expect.equal pools.LocalOwners.Length 1 "one owner row for the one binding"
            }

            test "a local quantifying nothing has no scheme row" {
                let pools = freezeFor "let f (a: int) =\n    let g b = b + a\n    g 1\n"
                Expect.isEmpty pools.LocalSchemes "g is monomorphic"
                Expect.equal pools.LocalOwners.Length 1 "g still has an owner row"
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

            test "a served template's local is generalised again in the consuming file" {
                let pools = freezeConsumer [ "lib.fs", servedTemplate; "use.fs", servedConsumer ]

                Expect.equal
                    (Array.length pools.LocalSchemes)
                    1
                    "the spliced `g` quantifies its own typar in the consumer"

                Expect.equal (snd pools.LocalSchemes.[0]).TyparArity 1<typeSlot> "one typar"

                Expect.equal
                    (pools.LocalOwners |> Array.map snd)
                    [|
                        LocalOwner.Spliced(SymbolKey.Binding(SymbolKeyOps.moduleBindingKey "Cross" "Lib" "served"))
                    |]
                    "the owning declaration is the template, which belongs to the declaring file"

                Expect.isEmpty
                    (unknownsIn pools |> List.filter (fun r -> r = UnknownReason.UnresolvedTypar))
                    "one cell per local typar for the whole body would leave the local's typar unquantified"
            }
        ]
