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
let private frozenFiles: (string * Frozen.TastFile) list =
    gated |> List.map (fun p -> p.Name, frozenOfJs p.Source)

[<Tests>]
let tests =
    testList
        "FrozenCodec tree round-trip"
        [
            test "thaw (flatten f) is structurally equal to f over the JS corpus" {
                for (name, f) in frozenFiles do
                    let rebuilt = FrozenCodec.thaw (FrozenCodec.flatten f)

                    Expect.isTrue
                        (TastFileG.structurallyEqual f rebuilt)
                        (sprintf "frozen file for %s did not survive flatten/thaw structurally" name)
            }

            // The gate must exercise a non-trivial corpus, else an empty run would make
            // `thaw ∘ flatten = id` vacuously true.
            test "the corpus exercises a non-trivial number of decls" {
                let totalDecls = frozenFiles |> List.sumBy (fun (_, f) -> f.Decls.Length)

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
                    f.Decls
                    |> EqArray.toList
                    |> List.tryPick (fun d ->
                        match d with
                        | Frozen.TDecl.Type td ->
                            TTypeKindG.members td.Kind
                            |> EqArray.toList
                            |> List.tryPick (fun (m: Frozen.TTypeMember) ->
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

                let rebuilt = FrozenCodec.thaw (FrozenCodec.flatten f)

                Expect.isTrue
                    (TastFileG.structurallyEqual f rebuilt)
                    "generic-member file survived flatten/thaw structurally"
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
                    let f = frozenOfJs src
                    let rebuilt = FrozenCodec.thaw (FrozenCodec.flatten f)

                    Expect.isTrue
                        (TastFileG.structurallyEqual f rebuilt)
                        (name + " did not survive flatten/thaw structurally")
                }
        ]
