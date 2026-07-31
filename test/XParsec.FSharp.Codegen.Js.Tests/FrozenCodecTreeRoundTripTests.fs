module XParsec.FSharp.Codegen.Js.Tests.FrozenCodecTreeRoundTripTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The wire-format gate: `thaw (flatten f)` is STRUCTURALLY equal to `f` for every
// JS-gated conformance program's whole `Frozen.TastFile` — the expr/decl/pat tree,
// the inline vocabulary, the `Map<NodeKey,_>` side tables, the diagnostics list, and
// the two `IReadOnlyDictionary<SymbolKey,_>` fields. This extends the leaf gate
// (`FrozenCodecRoundTripTests`) to the full file.
//
// `flatten`/`thaw` route through `TastPools.toPools`/`ofPools`, so this is also the
// corpus-wide gate on the pool interconversion: the columns must be sufficient to
// re-author every node, and the binder/lambda id remap must invert exactly. The
// equality predicate is `TastFileG.structurallyEqual` (a library function — the
// dictionary carve-out it encodes is a property of `TastFileG`, not of this test).

/// Corpus programs the JS backend actually compiles — the same gate the leaf test
/// and the byte-identity test use, so `frozenOfJs` never trips on a `Diagnose`
/// program's error diagnostics.
let private gated =
    programs
    |> List.filter (fun p ->
        match Map.tryFind "js" p.Obligations with
        | Some Obligation.Run
        | Some(Obligation.Fault _) -> true
        | _ -> false
    )

/// Freeze each gated program's source ONCE — the front-end pass is the expensive
/// part; the codec round-trip below is cheap.
let private frozenFiles: (string * FrozenPools) list =
    gated |> List.map (fun p -> p.Name, frozenOfJs p.Source)

/// The round-trip judge. The stored form IS the columns, so equality is taken on the DU
/// they encode: `FrozenPools` carries two `IReadOnlyDictionary` fields, which a derived
/// `=` would compare by reference, and `TastFileG.structurallyEqual` is the whole-file
/// equality that knows better.
let private survivesRoundTrip (f: FrozenPools) : bool =
    TastFileG.structurallyEqual (TastUnpool.ofPools f) (TastUnpool.ofPools (FrozenCodec.thaw (FrozenCodec.flatten f)))

/// A pools value bearing a specialization entry, an `InlineCall` edge naming it, and a
/// `CallerExpr` mark. No corpus program reaches any of the three — nothing places a deferred
/// body yet, and the pass flattens its own marks away — so the carriers are grafted onto a
/// real frozen file, leaving every other column exactly as the freeze built it. The entry is
/// keyed by a template the file genuinely publishes and its body IS that template's decl,
/// which is the entry shape: a `let` of lambdas.
let private withSpecialization () : FrozenPools =
    // A NESTED module: a top-level `let inline` has no home module and so no exportable
    // identity, and publishes nothing (`Freeze.toFrozenFile`).
    // `not` is a ONE-operand intrinsic, so the flattened tree is guaranteed to hold a node
    // with exactly one expr child and no pattern — the shape the `CallerExpr` graft needs.
    let pools =
        frozenOfJs "module M\n\nmodule N =\n    let inline f x = x + 1\n\nlet y = N.f 2\nlet z = not true\n"

    let template =
        match pools.InlineTemplates with
        | [| t |] -> t
        | ts -> failtestf "expected exactly one published template, got %d" ts.Length

    // `InlineCall` draws its args from the expr child column and owns no pattern, so a slot
    // with neither can stand in for a nullary one without disturbing the columns around it.
    let leaf =
        [ 0 .. pools.ExprPayloads.Length - 1 ]
        |> List.find (fun i ->
            ChildColumn.count pools.ExprChildren i = 0
            && ChildColumn.count pools.ExprPatChildren i = 0
        )

    // `CallerExpr` wraps exactly one expression and owns no pattern, so a slot with that shape
    // stands in for one the same way.
    let unary =
        [ 0 .. pools.ExprPayloads.Length - 1 ]
        |> List.find (fun i ->
            ChildColumn.count pools.ExprChildren i = 1
            && ChildColumn.count pools.ExprPatChildren i = 0
        )

    // The two grafted nodes are CALL-SITE material, so their domain is the consuming file and
    // deliberately NOT the entry's producer: a codec that dropped a node's own origin and
    // recovered it from the entry would still round-trip if the two agreed.
    let consumer =
        {
            Path =
                {
                    BucketName = "App"
                    Relative = "m.fs"
                    Absolute = "/app/m.fs"
                }
            Content = Hashing.hashString "module M\n"
        }

    let payloads = Array.copy pools.ExprPayloads

    payloads.[leaf] <-
        ExprPayload.InlineCall
            {|
                Spec = SpecializationId 0
                Origin = consumer
            |}

    payloads.[unary] <- ExprPayload.CallerExpr consumer

    { pools with
        ExprPayloads = payloads
        Specializations =
            [|
                {
                    Key =
                        {
                            Template = template.Key
                            TypeArgs = EqArray.ofList [ FTConst(RuntimeNames.intKey, EqArray.empty) ]
                        }
                    // The origin names a file OTHER than the one the blob is keyed by, so
                    // nothing about it is recoverable from the key and every field of it has to
                    // survive the wire — which is what the assertion on the decoded table
                    // checks. A synthetic identity suffices: no anchor is resolved here.
                    Origin =
                        {
                            Path =
                                {
                                    BucketName = "Lib"
                                    Relative = "n.fs"
                                    Absolute = "/lib/n.fs"
                                }
                            Content = Hashing.hashString "module N\n\nlet inline f x = x + 1\n"
                        }
                    Decl = template.Decl
                }
            |]
    }

/// The same graft with a SECOND entry naming the same producer file — one template grounded
/// two ways, which is what a program that reaches a package's inline vocabulary looks like.
/// The two entries differ only in the grounding, so the origin is the one thing they share.
let private withSharedOrigin () : FrozenPools =
    let pools = withSpecialization ()

    let first =
        match pools.Specializations with
        | [| s |] -> s
        | ss -> failtestf "expected the one grafted entry, got %d" ss.Length

    { pools with
        Specializations =
            [|
                first
                { first with
                    Key =
                        { first.Key with
                            TypeArgs = EqArray.ofList [ FTConst(RuntimeNames.boolKey, EqArray.empty) ]
                        }
                }
            |]
    }

[<Tests>]
let tests =
    testList
        "FrozenCodec tree round-trip"
        [
            test "thaw (flatten f) is structurally equal to f over the JS corpus" {
                for (name, f) in frozenFiles do
                    Expect.isTrue
                        (survivesRoundTrip f)
                        (sprintf "frozen file for %s did not survive flatten/thaw structurally" name)
            }

            // The gate must exercise a non-trivial corpus, else an empty run would make
            // `thaw ∘ flatten = id` vacuously true.
            test "the corpus exercises a non-trivial number of decls" {
                let totalDecls = frozenFiles |> List.sumBy (fun (_, f) -> f.Roots.Length)

                Expect.isGreaterThan (List.length frozenFiles) 0 "gated programs"
                Expect.isGreaterThan totalDecls 20 "total top-level decls across the corpus"
            }

            // The whole corpus above declares ZERO generic members, so it never populates a
            // member's `MethodTypeParams` — the field that used to ride the frozen tree as a
            // live union-find cell (`GeneralizedTypars`) and could not survive flatten/thaw
            // structurally. This case forces a non-empty carrier: it must ride `'ty` as
            // `(name, FTTypar(Method, i))` and round-trip as plain data.
            test "a generic member's MethodTypeParams rides the frozen tree as FTTypar and round-trips" {
                let f = frozenOfJs "type C() =\n    member this.Id<'T> (x: 'T) : 'T = x\n"

                let methodTypars =
                    (TastUnpool.ofPools f).Decls
                    |> EqArray.toList
                    |> List.tryPick (fun d ->
                        match d with
                        | TDeclG.Type td ->
                            TTypeKindG.members td.Kind
                            |> EqArray.toList
                            |> List.tryPick (fun (m: Pooled.TTypeMember) ->
                                if m.Name = "Id" then Some m.MethodTypeParams else None
                            )
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failtest "no member `Id` in the frozen tree")

                // Genuinely populated (not silently frozen empty), and each entry is the
                // positional method-axis marker — no `SemType` cell rides the tree.
                Expect.equal
                    (EqArray.toList methodTypars)
                    [ "'T", FTTypar(TyparAxis.Method, 0) ]
                    "member's own typar rides as (name, FTTypar(Method, 0))"

                Expect.isTrue (survivesRoundTrip f) "generic-member file survived flatten/thaw structurally"
            }

            // The conformance corpus declares no binding whose head pattern introduces no
            // binder, so it never exercised the seam where a side table is filed under a
            // key the frozen tree does not bear. These pin the shapes END-TO-END through
            // the stored wire form (`flatten` is `TastPools.toPools` then the column
            // writers): each once threw out of `toPools` — a file containing one could not
            // be cached at all — so a regression here is a hard failure, not a diff.
            // `TastPoolsTests` covers the full shape matrix; this is the serialization face.
            for name, src in
                [
                    "module-level tuple destructuring", "let p = (1, 2)\nlet (a, b) = p\nlet s = a + b\n"
                    "module-level wildcard binding", "let _ = 5\n"
                    "parenthesised simple binding head", "let (x) = 5\nlet y = x + 1\n"
                    "wildcard binding in a function body", "let f x =\n    let _ = x\n    x\n"
                    "top-level inline binding", "module M\nlet inline f x = x + 1\nlet y = f 2\n"
                    "inline binding in a named module",
                    "module M\n\nmodule N =\n    let inline f x = x + 1\n\nlet y = N.f 2\n"
                ] do
                test ("a binder-less or unpooled-binder binding head round-trips: " + name) {
                    Expect.isTrue
                        (survivesRoundTrip (frozenOfJs src))
                        (name + " did not survive flatten/thaw structurally")
                }

            // A `type` declaration's bodies are the one place the stored form names an
            // expression by pool id from INSIDE a declaration SHAPE rather than from a
            // child column, so the shape's writer and reader must stay in lockstep slot
            // for slot. The corpus declares no class preamble, secondary ctor, base-ctor
            // call or interface impl, so these drive the slots a plain `member` misses.
            for name, src in
                [
                    "static and instance preamble",
                    "type C(a: int) =\n    static let s = 1\n    let b = a + 1\n    do ()\n    member this.M() = b + s\n"
                    "secondary constructor", "type C(x: int) =\n    new() = C(0)\n    member this.X = x\n"
                    "base-ctor call",
                    "type Shape(x: int) =\n    member this.Raw = x\n\ntype Circle(r: int, t: int) =\n    inherit Shape(t)\n    member this.Radius = r\n"
                    "interface implementation",
                    "type IBox =\n    abstract member Unwrap : unit -> int\n\ntype Box(value: int) =\n    interface IBox with\n        member this.Unwrap() : int = value\n"
                ] do
                test ("a type declaration's body slots round-trip: " + name) {
                    Expect.isTrue
                        (survivesRoundTrip (frozenOfJs src))
                        (name + " did not survive flatten/thaw structurally")
                }

            // The specialization root array and the `InlineCall`/`CallerExpr` payloads are the
            // one part of the wire the corpus above leaves at zero, so it would pass with any
            // of them missing from the writer entirely. Assert on the decoded carriers
            // themselves, not only on the drained tree: an entry the writer skipped and the
            // reader defaulted to empty is invisible to a tree comparison that has no edge
            // pointing into it.
            test "a specialization entry, the InlineCall naming it and a CallerExpr survive flatten/thaw" {
                let grafted = withSpecialization ()
                let rt = FrozenCodec.thaw (FrozenCodec.flatten grafted)

                Expect.equal rt.Specializations grafted.Specializations "the specialization table survived the wire"

                Expect.sequenceEqual rt.ExprPayloads grafted.ExprPayloads "every expr payload survived the wire"

                Expect.isTrue (survivesRoundTrip grafted) "the grafted file survived flatten/thaw structurally"
            }

            // The origin is a REF into a table of its own, and one reference exercises the ref
            // but not the interning: what a realistic program has is many groundings of one
            // producer's templates plus a call site per grounding, and the four strings that
            // identify a file are then written once per FILE. Asserted on the decoded row
            // array, which is where the sharing is observable — references carrying equal
            // `OriginFile` values would decode identically either way.
            test "every reference to a file resolves to ONE origin row" {
                let grafted = withSharedOrigin ()
                let rt = FrozenCodec.thaw (FrozenCodec.flatten grafted)

                Expect.equal (rt.Specializations.Length) 2 "both entries survived the wire"
                Expect.equal rt.Specializations grafted.Specializations "…each with the origin it was written with"

                // Counted rather than assumed: two entries name the producer and the two
                // grafted nodes the consumer, on top of whatever the freeze itself anchored.
                let referenced =
                    [
                        for s in rt.Specializations do
                            yield s.Origin

                        for p in rt.ExprPayloads do
                            match p with
                            | ExprPayload.InlineCall c -> yield c.Origin
                            | ExprPayload.CallerExpr o -> yield o
                            | _ -> ()
                    ]
                    |> List.distinct

                Expect.isGreaterThan
                    referenced.Length
                    1
                    "the fixture must name more than one file, else sharing is vacuous"

                Expect.equal
                    (rt.Types.Rows.Origins.Length)
                    referenced.Length
                    "each file occupies ONE row, not one per reference that names it"
            }
        ]
