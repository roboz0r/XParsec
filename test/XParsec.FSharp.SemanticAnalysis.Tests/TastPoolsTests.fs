module XParsec.FSharp.SemanticAnalysis.Tests.TastPoolsTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The pool-build faithfulness gate: freeze a program, build the pools, then walk the
// pools from the decl roots and assert the reconstructed tree equals the DU walk —
// same shapes in the same order and the same expr/pat child fan-out at every node. No
// pool holds a DU node (all three are struct-of-arrays), so every node is checked by its
// SHAPE column and child-id columns. The DU walk here re-derives children through
// `TastAccessor` INDEPENDENTLY of the builder (which walks the same accessor), so the two
// agreeing is the cross-check, not a tautology; the reconstruction follows the id columns
// into the dense pool arrays, so a mis-wired child edge shows up as a fan-out mismatch.

/// The dense id the pool interned a DU node's own binder under. The unpooled tree already
/// names its binders in the pool's own space, so this is the node's `BinderKey` widened —
/// derived from the node rather than read off the payload under test, so a payload that
/// names its binder is checked against an independent answer instead of against itself.
let private internedBinderId (pools: FrozenPools) (b: BinderKeyG<BinderId> voption) : BinderId voption =
    b
    |> ValueOption.map (fun b ->
        let id = BinderKey.identity b
        let (BinderId i) = id

        if i < 0 || i >= pools.BinderNames.Length then
            failtestf "the pooled node's binder %O occupies no binder-pool slot" id

        id
    )

let rec private checkPat (pools: FrozenPools) (PatPoolId i) (du: Pooled.TPat) =
    let binder = internedBinderId pools (BinderKey.ofPat du)
    Expect.equal pools.PatPayloads.[i] (TastPoolShapes.patPayload binder du) "pat payload"
    let duKids = TastPoolShapes.patChildren du
    Expect.equal (ChildColumn.count pools.PatChildren i) duKids.Length "pat child fan-out"
    Array.iter2 (checkPat pools) (ChildColumn.slice pools.PatChildren i) duKids

let rec private checkExpr (pools: FrozenPools) (ExprPoolId i) (du: Pooled.TExpr) =
    let binder = internedBinderId pools (BinderKey.ofExpr du)
    Expect.equal pools.ExprPayloads.[i] (TastPoolShapes.exprPayload id binder du) "expr payload"
    let duExprKids = TastPoolShapes.exprChildren du
    let duPatKids = TastPoolShapes.exprPatChildren du
    Expect.equal (ChildColumn.count pools.ExprChildren i) duExprKids.Length "expr child fan-out"
    Expect.equal (ChildColumn.count pools.ExprPatChildren i) duPatKids.Length "expr's pat fan-out"
    Array.iter2 (checkExpr pools) (ChildColumn.slice pools.ExprChildren i) duExprKids
    Array.iter2 (checkPat pools) (ChildColumn.slice pools.ExprPatChildren i) duPatKids

/// A type declaration's body slots in traversal order, gathered by running the SAME
/// traversal the pool fill and unpool run (`TastConvert.typeDecl` at a collecting body
/// mapping). Reusing it is what keeps this check from drifting away from the set of slots
/// that are actually pooled — a newly-added body slot appears here for free.
let private bodySlots (td: TTypeDeclG<FrozenType, Anchor, 'id, 'body>) : 'body[] =
    let slots = ResizeArray<'body>()

    TastConvert.typeDecl
        {
            Ty = id
            Tok = id
            Id = BinderKey.identity
            Body =
                fun b ->
                    slots.Add b
                    b
        }
        td
    |> ignore

    slots.ToArray()

let private checkDecl (pools: FrozenPools) (DeclPoolId i) (du: Pooled.TDecl) =
    // The pooled shape is its payload's (`DeclPayload.shape`); each arm asserts the one
    // the DU case it is standing in calls for, so the correspondence is checked without
    // a second DU→shape match to keep in step.
    let shapeIs = Expect.equal (DeclPayload.shape pools.DeclPayloads.[i])

    match du with
    | TDeclG.Let(binding = binding; value = value) ->
        shapeIs DeclShape.Let "decl shape"
        Expect.equal (ChildColumn.count pools.DeclExprChildren i) 1 "let decl one value child"
        Expect.equal (ChildColumn.count pools.DeclPatChildren i) 1 "let decl one binding child"
        checkExpr pools (ChildColumn.item pools.DeclExprChildren i 0) value
        checkPat pools (ChildColumn.item pools.DeclPatChildren i 0) binding
    | TDeclG.Expression(expr = expr) ->
        shapeIs DeclShape.Expression "decl shape"
        Expect.equal (ChildColumn.count pools.DeclExprChildren i) 1 "expression decl one child"
        Expect.equal (ChildColumn.count pools.DeclPatChildren i) 0 "expression decl no pat child"
        checkExpr pools (ChildColumn.item pools.DeclExprChildren i 0) expr
    | TDeclG.Type duTd ->
        shapeIs DeclShape.Type "decl shape"
        Expect.equal (ChildColumn.count pools.DeclExprChildren i) 0 "type decl surfaces no expr child"
        Expect.equal (ChildColumn.count pools.DeclPatChildren i) 0 "type decl surfaces no pat child"

        // The member / preamble / ctor bodies are not children — they are named by id
        // INSIDE the payload's declaration shape, which is what keeps "which body fills
        // which slot" expressed by the shape. Zip the pooled ids against the DU's bodies
        // in slot order and check each pooled subtree against the one it stands for.
        match pools.DeclPayloads.[i] with
        | DeclPayload.Type td ->
            let ids = bodySlots td
            let bodies = bodySlots duTd
            Expect.equal ids.Length bodies.Length "one pooled body id per type-decl body slot"
            Array.iter2 (checkExpr pools) ids bodies
        | p -> failtestf "a Type decl's pool payload is %A, not DeclPayload.Type" p

/// A pooled lambda's `LambdaKey`. `FunVerdicts` is keyed by the lambda id space, not the
/// binder pool, and the Node that carried the key is gone — so the key is read off the
/// `ExprToks` column exactly as `ofPools` does. One home for that, so a test cannot key a
/// verdict differently from the code under test.
let private pooledLambdaKey (pools: FrozenPools) (ExprPoolId i) : LambdaKey = LambdaKey pools.ExprToks.[i]

/// The id-resolution gate: the `ExprVarBinder` column is populated EXACTLY at the `Var`
/// slots (each to an in-range `BinderId`), and each side table's source keys land on a
/// `BinderId`/`ExprPoolId`. A binder the enumeration missed shows up as an unresolved
/// reference (a `toPools` fault). That each `Var` resolves to its OWN binder key is proven
/// by the round-trip gate (which rebuilds every `Var.binding` from this column). The pooled
/// side tables must also cover the source maps 1:1 — a dropped or duplicated key would
/// desync the rebuilt map from the original — whether they keep their key (`DenseTable`) or
/// have given it up for a position (`BinderColumn`).
let private checkIdResolution (pools: FrozenPools) (frozen: Pooled.TastFile) =
    for i in 0 .. pools.ExprPayloads.Length - 1 do
        match ExprPayload.shape pools.ExprPayloads.[i], pools.ExprVarBinder.[i] with
        | ExprShape.Var, ValueSome(BinderId b) ->
            Expect.isTrue (b >= 0 && b < pools.BinderNames.Length) "Var binder id is an interned binder"
        | ExprShape.Var, ValueNone -> failtest "a Var pool entry carries no resolved binder id"
        | _, ValueSome _ -> failtest "a non-Var pool entry carries a binder id"
        | _, ValueNone -> ()

    // Each dense side table is the source map re-keyed onto its id space: same cardinality,
    // and every dense key resolves (through the given resolver) to a key the source map
    // holds. Only the KEYS are compared — a dense value need not be the source value's type
    // — and the values' faithfulness is the round-trip gate's business, not this one's.
    // `BindingValReprs` is absent: it is DERIVED off the columns rather than re-keyed from
    // a source map, and has its own gate (`checkValReprPatsAreLambdaParams`).
    let checkTable (name: string) (resolve: 'id -> 'k) (dense: ('id * 'v)[]) (sourceKeys: Set<'k>) =
        Expect.equal dense.Length sourceKeys.Count (name + " dense form covers the source map 1:1")

        for (id, _) in dense do
            Expect.isTrue (Set.contains (resolve id) sourceKeys) (name + " dense key resolves to a source key")

    // A per-binder COLUMN (`BinderColumn`) holds no key, so what is checked is the FILLED
    // SLOTS: one per source entry, each at the slot of the binder that entry named. The
    // column is also aligned to the binder pool, which a keyed table has no obligation to be.
    let checkColumn (name: string) (col: BinderColumn<'v>) (sourceKeys: Set<BinderId>) =
        Expect.equal col.Length pools.BinderNames.Length (name + " column is aligned with the binder pool")

        let filled =
            [|
                for i in 0 .. col.Length - 1 do
                    if col.[i].IsSome then
                        yield BinderId i
            |]

        Expect.equal filled.Length sourceKeys.Count (name + " column covers the source map 1:1")

        for k in filled do
            Expect.isTrue (Set.contains k sourceKeys) (name + " filled slot is a source key's binder")

    // The source keys in the address space the resolvers answer in: a binder-keyed table is
    // widened (`BinderKey.widenMap`), `FunVerdicts` is already lambda-key-shaped.
    let keysOf (m: Map<'k, 'w>) =
        m |> Map.toSeq |> Seq.map fst |> Set.ofSeq

    let binderSource (m: Map<BinderKeyG<BinderId>, 'w>) = keysOf (BinderKey.widenMap m)

    checkTable "ModuleMembers" id pools.ModuleMembers (binderSource frozen.ModuleMembers)
    checkTable "ClosureReprs" id pools.ClosureReprs (binderSource frozen.ClosureReprs)
    checkTable "FunVerdicts" (pooledLambdaKey pools) pools.FunVerdicts (keysOf frozen.FunVerdicts)
    checkTable "GenericFnSchemes" id pools.GenericFnSchemes (binderSource frozen.GenericFnSchemes)
    checkColumn "BindingTyparArities" pools.BindingTyparArities (binderSource frozen.BindingTyparArities)

/// A binding's recorded arity is READ OFF the pooled lambda chain, so a tuple group's pattern must
/// BE the lambda parameter node it was peeled from. A re-pooled copy would be structurally equal and
/// so invisible to the round-trip gate, but a different id — and identity after freeze is
/// the id. Every recorded `GTuple` must therefore name a pat that some `Lambda` bears as
/// its parameter.
///
/// Also: one entry per `NamedSimple`-headed `Let` root and no more, which is the coverage
/// the file→file signature projection relies on.
let private checkValReprPatsAreLambdaParams (pools: FrozenPools) =
    let lambdaParams = System.Collections.Generic.HashSet<PatPoolId>()

    for i in 0 .. pools.ExprPayloads.Length - 1 do
        if pools.ExprPayloads.[i] = ExprPayload.Lambda then
            lambdaParams.Add(ChildColumn.item pools.ExprPatChildren i 0) |> ignore

    for _, vr in pools.BindingValReprs do
        for g in vr.Groups do
            match g with
            | ArgGroupG.GTuple pat ->
                Expect.isTrue (lambdaParams.Contains pat) "a tuple group names a lambda's own parameter pattern"
            | ArgGroupG.GUnit _
            | ArgGroupG.GSimple _ -> ()

    let namedLetRoots =
        pools.Roots
        |> Array.filter (fun (DeclPoolId d) ->
            DeclPayload.shape pools.DeclPayloads.[d] = DeclShape.Let
            && (let (PatPoolId head) = ChildColumn.item pools.DeclPatChildren d 0

                match pools.PatPayloads.[head] with
                | PatPayload.NamedSimple _ -> true
                | _ -> false)
        )

    Expect.equal pools.BindingValReprs.Length namedLetRoots.Length "one recorded arity per simple-binder Let root"

// The binder columns' obligation, which is ONE obligation: a binder the SOURCE WRITES is
// anchored at the token that writes it and named with the text there; a binder no source
// writes has neither. Both columns are filled from a single record made where the binder's
// key was minted (`PassContext.BinderSpellings`), so the check is that they agree with each
// other and with the file's own tokens. Returns the count of written binders, so a test can
// assert non-vacuous coverage.
//
// It is deliberately NOT "a binder is anchored where its introducing NODE sits". That holds
// only until an inline body is copied onto its call site: the node then sits at the call,
// while its binders — `freshen`-minted — are written nowhere at all. Asserting the node form
// is what made deriving a name from a node's anchor look sound.
let private checkBinderSpellings (src: string) (pools: FrozenPools) : int =
    let lexed, _ = parseFile src
    let mutable written = 0

    for i in 0 .. pools.BinderNames.Length - 1 do
        match pools.BinderNames.[i], pools.BinderToks.[i].Index with
        | "", ValueNone -> ()
        | "", ValueSome t -> failtestf "binder %d is anchored at token %d but has no name" i (int t)
        | name, ValueNone -> failtestf "binder %d is named '%s' but is written nowhere" i name
        | name, ValueSome t ->
            written <- written + 1

            Expect.equal
                (lexed.GetIdentifier(t, src))
                name
                (sprintf "binder %d's name is the identifier at its own anchor" i)

    written

let private checkProgram (src: string) =
    let pools, frozen = poolsFor src
    let duDecls = EqArray.toArray frozen.Decls
    Expect.equal pools.Roots.Length duDecls.Length "one root per emittable decl"
    Array.iter2 (checkDecl pools) pools.Roots duDecls
    checkIdResolution pools frozen
    checkValReprPatsAreLambdaParams pools

    // The interconversion gate: `ofPools ∘ rePool` reconstructs a structurally-equal file.
    // Both sides speak the pool's own dense identity, so the comparison needs no widening
    // at all — and the ids the re-pool assigns must be the ids the tree already bore, since
    // the walk that assigns them is the walk that unpooled it.
    //
    // Compared DIRECTLY, DU value against DU value — the serializer is
    // no oracle here, because `FrozenCodec.flatten` itself pools the file, so flattening
    // both sides would compare `toPools (ofPools (toPools f))` with `toPools f` and prove
    // nothing about `ofPools`. `TastFileG.structurallyEqual` is the equality a whole-file
    // `=` cannot be (two fields are `IReadOnlyDictionary`, reference-equal only). The
    // rebuilt file's decl trees come from the pool ids and its side-table maps are re-keyed
    // through the binder pool (not shared from the source), so an inequality is a genuine
    // decl-tree OR key-remap divergence.
    Expect.isTrue
        (TastFileG.structurallyEqual (TastUnpool.ofPools pools) frozen)
        "ofPools (rePool f) round-trips to a structurally-equal frozen file"

// Representative programs, spanning binder shapes (lambda / let-in / for), control
// flow (if / match), and the type + value forms (record decl, record literal, field
// access) — enough distinct expr/pat shapes that a broken child edge in any of the
// common cases trips the walk.
let private programs =
    [
        "curried fn + saturated call", "let add x y = x + y\nlet answer = add 1 40\n"
        "if/then/else + unary op", "let f x = if x > 0 then x else -x\n"
        "match with const and wildcard arms", "let classify x =\n    match x with\n    | 0 -> 1\n    | _ -> 2\n"
        "nested let-in", "let h () =\n    let a = 1 in\n    let b = 2 in\n    a + b\n"
        "record decl, literal and field get",
        "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }\nlet getX (v: R) = v.X\n"
        "for-to loop with mutable accumulator",
        "let sumTo n =\n    let mutable t = 0\n    for i = 1 to n do\n        t <- t + i\n    t\n"

        // Binding heads that introduce NO single binder, or introduce one only behind a
        // wrapper the frozen tree erases. Each of these once faulted `toPools`: the
        // side tables were filed under `CstKeys.ofBinding` (the head PATTERN's key),
        // which for these shapes names a node the frozen tree does not bear — a
        // `Pat.EnclosedBlock`/`Pat.As` wrapper `translatePat` drops, or a composite /
        // wildcard pattern that binds no name at all. The producers now file under the
        // binder the head introduces (`BinderKey.ofPat`), or under nothing.
        "module-level tuple destructuring", "let p = (1, 2)\nlet (a, b) = p\nlet s = a + b\n"
        "module-level tuple destructuring without parens", "let p = (1, 2)\nlet a, b = p\nlet s = a + b\n"
        "module-level nested destructuring", "let p = ((1, 2), 3)\nlet ((a, b), c) = p\nlet s = a + b + c\n"
        "module-level record destructuring",
        "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }\nlet { X = xx; Y = yy } = r\nlet s = xx + yy\n"
        "module-level union destructuring", "type U = | A of int\nlet u = A 1\nlet (A n) = u\nlet s = n + 1\n"
        "destructuring head with an as-alias", "let p = (1, 2)\nlet (a, b) as q = p\nlet s = a + b\n"
        "module-level wildcard binding", "let _ = 5\n"
        "parenthesised simple binding head", "let (x) = 5\nlet y = x + 1\n"
        "annotated parenthesised simple binding head", "let (x: int) = 5\nlet y = x + 1\n"
        "parenthesised mutable binding head", "let mutable (m) = 1\nlet f () = m <- m + 1\n"
        "destructuring and wildcard heads in a named module",
        "module M\n\nmodule N =\n    let p = (1, 2)\n    let (a, b) = p\n    let (z) = a\n    let _ = b\n"

        // The same non-binder heads INSIDE a body, which reach the pool through
        // `ClosureReprs` (the escape snapshot) rather than the module-binding tables.
        "wildcard binding in a function body", "let f x =\n    let _ = x\n    x\n"
        "wildcard binding in a lambda body", "let f = fun x ->\n    let _ = x\n    x\n"
        "parenthesised binding head in a function body", "let f x =\n    let (y) = x\n    y\n"
        "parenthesised use binding head", "let f (d: System.IDisposable) =\n    use (x) = d\n    1\n"
        "destructuring let ahead of a use in a function body",
        "let f (d: System.IDisposable * int) =\n    let (a, b) = d\n    use x = a\n    b\n"

        // Inline bindings: an `inline` binding is in `Decls` like any other (it is
        // emitted as an ordinary module function) and, when publishable, is ALSO a
        // second independent tree under `InlineTemplates`. Both are pooled roots, so
        // the SAME source binder is reached twice by the enumeration — idempotent in
        // the key, so it lands on one `BinderId` and the side tables filed against it
        // resolve. A top-level `inline` binding is published nowhere (no top-level
        // binding is exported), so only its `Decls` copy exists.
        "top-level inline binding", "module M\nlet inline f x = x + 1\nlet y = f 2\n"
        "inline binding in a named module", "module M\n\nmodule N =\n    let inline f x = x + 1\n\nlet y = N.f 2\n"
        "nullary intrinsic alias binding", "module M\n\nmodule N =\n    let undef = (# \"undefined\" #)\n"

        // Shapes that were ALREADY sound, pinned here so a future producer change
        // cannot silently start filing them under a non-binder key.
        "destructuring let in a function body", "let f (p: int * int) =\n    let (a, b) = p\n    a + b\n"
        "module-level operator binding", "module M\nlet (+.) a b = a + b\n"
        "lambda with a tuple parameter", "let f = fun (a, b) -> a + b\n"
        "or-pattern arm", "let f x =\n    match x with\n    | 0 | 1 -> 9\n    | _ -> 0\n"
        "type-test as-pattern arm",
        "let f (x: int | string) =\n    match x with\n    | :? int as i -> i\n    | _ -> 0\n"
        "class with a ctor-param-capturing member closure",
        "type C(a: int) =\n    member this.M(x: int) =\n        let g = fun y -> y + x + a\n        g 1\n"

        // A type declaration's BODIES are pooled, and they are the only trees reached
        // through the declaration shape rather than through a decl's child columns. These
        // walk the remaining body slots — the static / instance preamble, a secondary
        // ctor's primary-chain args, a base-ctor call's args, an interface impl's members
        // — each of which also binds keys (`this`, member / ctor parameters) that no
        // pattern node introduces, so a `Var` naming one is exactly what the binder
        // enumeration must cover.
        "class with static and instance preamble entries",
        "type C(a: int) =\n    static let s = 1\n    let b = a + 1\n    do ()\n    member this.M() = b + s\n"
        "class with a secondary constructor", "type C(x: int) =\n    new() = C(0)\n    member this.X = x\n"
        "derived class with a base-ctor call",
        "type Shape(x: int) =\n    member this.Raw = x\n\ntype Circle(r: int, t: int) =\n    inherit Shape(t)\n    member this.Radius = r\n"
        "class with an interface implementation",
        "type IBox =\n    abstract member Unwrap : unit -> int\n\ntype Box(value: int) =\n    interface IBox with\n        member this.Unwrap() : int = value\n"
    ]

[<Tests>]
let tests =
    testList
        "TastPools.build mirrors the DU"
        [
            for name, src in programs do
                test name { checkProgram src }
        ]

// WHICH FILE the anchor columns index is a column-set-wide fact, so no walk of the tree can
// check it and the round-trip gate above would pass with it dropped or replaced. It is also
// the only thing that can tell this file's own material from a producer's once the front end
// is out of reach (`InlineExpand`), and it must be the identity the analysis ran under: one
// rebuilt downstream from a path is a different value that compares unequal, which reads as
// "every node of this file is foreign".
[<Tests>]
let unitOriginTests =
    testList
        "TastPools states which file the anchors index"
        [
            test "the freeze stamps the origin it was analysed under" {
                let origin, frozen = freezeWithOrigin "let a = 1\n"

                Expect.equal frozen.Origin origin.File "the pools name the file the anchors were taken from"
            }

            test "re-pooling an unpooled tree keeps it" {
                // The unpool carries no origin — the tree has no field for one — so the fill
                // has only the pool it came out of to take it from.
                let origin, frozen = freezeWithOrigin "let a = 1\n"

                Expect.equal
                    (TastPools.rePool frozen (TastUnpool.ofPools frozen)).Origin
                    origin.File
                    "the re-fill kept the file, rather than defaulting to nobody's"
            }
        ]

[<Tests>]
let binderAnchorTests =
    testList
        "TastPools names a binder where the source writes it"
        [
            // Each program checks the obligation on its own binders; the aggregate asserts
            // the set is non-vacuous, so a program mix in which the source wrote no binder
            // at all would fail loud rather than pass trivially.
            for name, src in programs do
                test name {
                    let pools, _ = poolsFor src
                    checkBinderSpellings src pools |> ignore
                }

            test "the program set exercises source-written binders" {
                let total =
                    programs
                    |> List.sumBy (fun (_, src) ->
                        let pools, _ = poolsFor src
                        checkBinderSpellings src pools
                    )

                Expect.isGreaterThan total 0 "at least one binder is named at its own anchor"
            }
        ]

// Every tree the frozen file bears is in the pools — the type declarations' member bodies,
// the inline vocabulary, and the `ValRepr` tuple-group patterns included. The round-trip
// gate above proves that faithfully, but only for the domains the program set actually
// CONTAINS: a file with no type declaration proves nothing about pooled member bodies. So
// count the three, and fail loudly if the set stops populating any of them.

/// The three formerly-opaque carriers, counted over one program's pools. A tuple group
/// names a lambda parameter node rather than a pooled copy of one, so its count is coverage of the
/// DERIVATION — that the program set produces tuple-parameter bindings at all.
let private carrierCounts (pools: FrozenPools) =
    let memberBodies =
        pools.DeclPayloads
        |> Array.sumBy (fun p ->
            match p with
            | DeclPayload.Type td -> (bodySlots td).Length
            | DeclPayload.Let _
            | DeclPayload.Expression _ -> 0
        )

    let tupleGroups =
        pools.BindingValReprs
        |> Array.sumBy (fun (_, vr) ->
            vr.Groups
            |> List.sumBy (fun g ->
                match g with
                | ArgGroupG.GTuple _ -> 1
                | ArgGroupG.GUnit _
                | ArgGroupG.GSimple _ -> 0
            )
        )

    {|
        MemberBodies = memberBodies
        InlineTemplates = pools.InlineTemplates.Length
        TupleGroups = tupleGroups
    |}

[<Tests>]
let pooledCarrierCoverageTests =
    testList
        "TastPools holds every tree the frozen file bears"
        [
            test "the program set populates all three formerly-opaque carriers" {
                let totals =
                    programs
                    |> List.map (fun (_, src) -> carrierCounts (fst (poolsFor src)))
                    |> List.fold
                        (fun (b, i, t) c -> b + c.MemberBodies, i + c.InlineTemplates, t + c.TupleGroups)
                        (0, 0, 0)

                let (memberBodies, inlineTemplates, tupleGroups) = totals
                Expect.isGreaterThan memberBodies 0 "a type declaration's member bodies are pooled"
                Expect.isGreaterThan inlineTemplates 0 "an inline template is pooled as its own root"
                Expect.isGreaterThan tupleGroups 0 "a ValRepr tuple group's pattern is pooled"
            }

            // Every id the three carriers name must resolve into the columns — the property a
            // structural round-trip can satisfy vacuously if a carrier is empty, and the one
            // that would break first if a body/template/group were pooled into the wrong space.
            test "every carrier id indexes its own pool" {
                for _, src in programs do
                    let pools, _ = poolsFor src

                    let inExprs (ExprPoolId i) =
                        Expect.isTrue (i >= 0 && i < pools.ExprPayloads.Length) "body id is an expr pool entry"

                    for p in pools.DeclPayloads do
                        match p with
                        | DeclPayload.Type td -> Array.iter inExprs (bodySlots td)
                        | DeclPayload.Let _
                        | DeclPayload.Expression _ -> ()

                    for t in pools.InlineTemplates do
                        let (DeclPoolId i) = t.Decl
                        Expect.isTrue (i >= 0 && i < pools.DeclPayloads.Length) "template id is a decl pool entry"

                    for _, vr in pools.BindingValReprs do
                        for g in vr.Groups do
                            match g with
                            | ArgGroupG.GTuple(PatPoolId i) ->
                                Expect.isTrue (i >= 0 && i < pools.PatPayloads.Length) "group id is a pat pool entry"

                                Expect.equal
                                    (PatPayload.shape pools.PatPayloads.[i])
                                    PatShape.Tuple
                                    "a GTuple names a Tuple pattern"
                            | ArgGroupG.GUnit _
                            | ArgGroupG.GSimple _ -> ()
            }
        ]

// The corpus never populates `FunVerdicts` (the value-struct / stack-closure emit path
// is not yet reachable), so the round-trip gate above never exercises the lambda id
// space. These inject a synthetic verdict keyed by a real frozen lambda's own anchor and
// drive the path directly: `FunVerdicts` is re-keyed onto the
// lambda's `ExprPoolId` (off the binder pool), `ofPools` inverts back to the original
// `LambdaKey`, and a key naming no pooled lambda faults.
//
// The ARITY of that re-key is the thing to hold: the key is one-to-many over the id space,
// so a verdict must reach every lambda its key names. The round trip cannot see a lost
// copy — it recomputes the key both ways and folds back to one entry either way — so the
// bearer set is asserted directly.

/// Every pooled `Lambda` id, grouped by the key its verdict resolves through.
let private lambdasByKey (pools: FrozenPools) : (LambdaKey * ExprPoolId list) list =
    [
        for i in 0 .. pools.ExprPayloads.Length - 1 do
            if ExprPayload.shape pools.ExprPayloads.[i] = ExprShape.Lambda then
                yield pooledLambdaKey pools (ExprPoolId i), ExprPoolId i
    ]
    |> List.groupBy fst
    |> List.map (fun (k, xs) -> k, List.map snd xs)

/// The first `Lambda` expr pool entry's key — the frozen lambda to key the verdict on.
let private firstLambdaKey (pools: FrozenPools) : LambdaKey =
    seq { 0 .. pools.ExprPayloads.Length - 1 }
    |> Seq.pick (fun i ->
        match ExprPayload.shape pools.ExprPayloads.[i] with
        | ExprShape.Lambda -> Some(pooledLambdaKey pools (ExprPoolId i))
        | _ -> None
    )

[<Tests>]
let funVerdictLambdaKeyTests =
    testList
        "TastPools re-keys FunVerdicts onto the lambda id space"
        [
            test "a lambda-keyed verdict pools by ExprPoolId and inverts to its NodeKey" {
                let src = "let f = fun x -> x + 1\n"
                let _, frozen = poolsFor src
                let rePool = rePoolFor src
                let lamKey = firstLambdaKey (rePool frozen)

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                let injected =
                    { frozen with
                        FunVerdicts = Map.ofList [ lamKey, verdict ]
                    }

                let pools = rePool injected

                // (a) the pool-form table is keyed by that lambda's `ExprPoolId`.
                Expect.equal pools.FunVerdicts.Length 1 "one pooled verdict"
                let (ExprPoolId i, v) = pools.FunVerdicts.[0]
                Expect.equal v verdict "pooled verdict value preserved"

                Expect.equal
                    (ExprPayload.shape pools.ExprPayloads.[i])
                    ExprShape.Lambda
                    "verdict id names a Lambda entry"

                Expect.equal (pooledLambdaKey pools (ExprPoolId i)) lamKey "verdict's ExprPoolId is the keyed lambda's"

                // (b) `ofPools` reconstructs the map under the ORIGINAL lambda NodeKey.
                let rebuilt = TastUnpool.ofPools pools
                Expect.equal rebuilt.FunVerdicts.Count 1 "one rebuilt verdict"

                Expect.equal
                    (Map.tryFind lamKey rebuilt.FunVerdicts)
                    (Some verdict)
                    "verdict rebuilt under its lambda key"
            }

            test "a FunVerdicts key naming no pooled lambda faults in toPools" {
                let src = "let f = fun x -> x + 1\n"
                let _, frozen = poolsFor src
                let rePool = rePoolFor src

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                // A key on a token index past the end of any lexed file — a lambda-keyed
                // entry naming no pooled lambda, the honest failure the resolver surfaces.
                let bogus = LambdaKey(Anchor.ofStored 1_000_000)

                let injected =
                    { frozen with
                        FunVerdicts = Map.ofList [ bogus, verdict ]
                    }

                Expect.throws (fun () -> rePool injected |> ignore) "unresolved lambda-keyed verdict faults"
            }

            test "a verdict reaches EVERY pooled lambda its key names" {
                // A published `inline` binding is what makes the key one-to-many: the
                // template `Freeze` publishes and the ordinary function the binding is
                // emitted as are two trees over the SAME source, so their lambdas anchor on
                // the same token. An inline SPLICE duplicates a body's lambdas the same way.
                let src = "module M\n\nmodule N =\n    let inline addOne x = x + 1\n"
                let _, frozen = poolsFor src
                let rePool = rePoolFor src

                let shared, bearers =
                    rePool frozen |> lambdasByKey |> List.find (fun (_, ids) -> List.length ids > 1)

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                let pools =
                    rePool
                        { frozen with
                            FunVerdicts = Map.ofList [ shared, verdict ]
                        }

                // Every bearer takes a row. Resolving the key to ONE id instead would leave
                // the copies codegen actually walks with no verdict, and nothing downstream
                // would report a miss — the closure would just emit as an ordinary one.
                Expect.equal
                    (pools.FunVerdicts |> Array.map fst |> Set.ofArray)
                    (Set.ofList bearers)
                    "every lambda the key names has a row"

                Expect.all pools.FunVerdicts (fun (_, v) -> v = verdict) "each row carries the keyed verdict"

                // The inverse folds the rows back onto the one key they came from.
                let rebuilt = TastUnpool.ofPools pools
                Expect.equal rebuilt.FunVerdicts.Count 1 "the unpool restores the source map's arity"

                Expect.equal
                    (Map.tryFind shared rebuilt.FunVerdicts)
                    (Some verdict)
                    "verdict rebuilt under its lambda key"
            }
        ]

// The REACHABILITY half of side-table identity: an entry keyed by a real binder whose
// declaration was then dropped, which is what a producer that prunes a decl and forgets its
// side-table entry leaves behind. `BinderKey`'s constructor is private, so this cannot be
// staged by minting a key from an arbitrary `NodeKey` — the only way in is the defect's own
// shape, and that is the shape worth pinning anyway.
//
// What the fault has to say is WHICH table still holds the entry: several producers file
// into several binder-keyed tables, and "an entry is stale" does not say whose bug it is.

/// Two module bindings that nothing references, so dropping the second takes nothing with
/// it — a surviving `Var` naming it would fault as an incomplete binder ENUMERATION, a
/// different failure under a different message, and the test would pass for the wrong reason.
let private staleEntrySrc = "module M\n\nmodule N =\n    let a = 1\n    let b = 2\n"

/// `staleEntrySrc` frozen with its LAST binding's declaration removed AND every side-table
/// entry for that binder stripped — the producer-side fix applied. Re-adding the entry to
/// exactly one table is then what makes the reported table name unambiguous.
let private lastBindingDropped () =
    let _, frozen = poolsFor staleEntrySrc
    let rePool = rePoolFor staleEntrySrc
    let decls = EqArray.toArray frozen.Decls

    let index, binder =
        seq { 0 .. decls.Length - 1 }
        |> Seq.rev
        |> Seq.pick (fun i ->
            match decls.[i] with
            | TDeclG.Let(binding = binding) ->
                match BinderKey.ofPat binding with
                | ValueSome b -> Some(i, b)
                | ValueNone -> None
            | TDeclG.Expression _
            | TDeclG.Type _ -> None
        )

    {|
        Binder = binder
        // The fill the injected trees below go through, carrying the naming column of the
        // freeze they were unpooled out of.
        RePool = rePool
        // Kept so the stale entry re-added below is the producer's own value, not a
        // fabricated one — the entry is genuine; only its declaration is gone.
        Member = Map.find binder frozen.ModuleMembers
        Pruned =
            { frozen with
                Decls = EqArray.ofArray (Array.removeAt index decls)
                ModuleMembers = Map.remove binder frozen.ModuleMembers
                ClosureReprs = Map.remove binder frozen.ClosureReprs
                GenericFnSchemes = Map.remove binder frozen.GenericFnSchemes
                BindingTyparArities = Map.remove binder frozen.BindingTyparArities
            }
    |}

[<Tests>]
let staleSideTableEntryTests =
    testList
        "TastPools faults on a side-table entry whose declaration left the tree"
        [
            // The control: the fault below is caused by the STALE ENTRY, not by the decl
            // being gone. Pruning both is the fix the fault demands, and it pools cleanly.
            test "a declaration pruned together with its entries pools cleanly" {
                let dropped = lastBindingDropped ()
                dropped.RePool dropped.Pruned |> ignore
            }

            test "a retained ModuleMembers entry faults, naming that table" {
                let dropped = lastBindingDropped ()

                let injected =
                    { dropped.Pruned with
                        ModuleMembers = Map.add dropped.Binder dropped.Member dropped.Pruned.ModuleMembers
                    }

                Expect.throwsC
                    (fun () -> dropped.RePool injected |> ignore)
                    (fun ex ->
                        Expect.stringContains
                            ex.Message
                            "ModuleMembers"
                            "the fault names the table holding the stale entry"
                    )
            }

            // The SAME dropped binder in a different table: the reported name tracks the
            // table, so it is diagnostic rather than a constant that happens to read right.
            //
            // This one also pins that giving up the STORED key does not give up the check.
            // `BindingTyparArities` reaches the pools as a `BinderColumn` — no key at all —
            // yet the producer's key still has to name a slot for the fill to have one to
            // write, so it goes through the same `binderIdOf` and faults the same way.
            test "a retained BindingTyparArities entry faults, naming that table" {
                let dropped = lastBindingDropped ()

                let injected =
                    { dropped.Pruned with
                        BindingTyparArities = Map.add dropped.Binder 0 dropped.Pruned.BindingTyparArities
                    }

                Expect.throwsC
                    (fun () -> dropped.RePool injected |> ignore)
                    (fun ex ->
                        Expect.stringContains
                            ex.Message
                            "BindingTyparArities"
                            "the fault names the table holding the stale entry"
                    )
            }

            // `ChildColumn` is CSR: `Start` and `Ids` are two independently length-prefixed
            // wire arrays that can disagree. A disagreement does not fault on read — it hands
            // back a different, in-range child list for every slot past it, silently
            // re-parenting the tree — so `ofStored` is the checked way in and nothing else
            // can build one. These gate the four conditions it enforces.
            testList
                "ChildColumn.ofStored rejects a malformed stored column"
                [
                    // A well-formed one, so the rejections below are not passing vacuously.
                    test "a well-formed column is admitted and reads back" {
                        let col = ChildColumn.ofStored [| 0; 2; 2; 3 |] [| 10; 11; 12 |]
                        Expect.equal (ChildColumn.length col) 3 "three slots for four starts"
                        Expect.equal (ChildColumn.count col 0) 2 "slot 0 fan-out"
                        Expect.equal (ChildColumn.slice col 0) [| 10; 11 |] "slot 0 children"
                        Expect.equal (ChildColumn.count col 1) 0 "an empty slot"
                        Expect.equal (ChildColumn.item col 2 0) 12 "the last slot needs no special case"
                    }

                    test "an empty start array is rejected" {
                        // Not the same as the empty COLUMN, which is `[| 0 |]` — CSR always
                        // carries n+1 entries, so zero of them is no column at all.
                        Expect.throws (fun () -> ChildColumn.ofStored [||] [||] |> ignore) "no slot-start array"
                    }

                    test "a column not starting at 0 is rejected" {
                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 1; 1 |] [||] |> ignore)
                            "starts must begin at 0"
                    }

                    test "decreasing slot starts are rejected" {
                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 0; 2; 1; 2 |] [| 10; 11 |] |> ignore)
                            "starts must be monotone"
                    }

                    test "starts that do not end at the id count are rejected" {
                        // The one a truncated or over-long `Ids` array produces, and the one
                        // that would otherwise read a valid-but-wrong child of the last slot.
                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 0; 2 |] [| 10; 11; 12 |] |> ignore)
                            "a trailing id no slot claims"

                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 0; 3 |] [| 10; 11 |] |> ignore)
                            "a slot claiming ids that are not there"
                    }
                ]
        ]
