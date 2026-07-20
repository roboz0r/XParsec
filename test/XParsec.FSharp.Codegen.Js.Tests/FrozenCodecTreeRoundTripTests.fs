module XParsec.FSharp.Codegen.Js.Tests.FrozenCodecTreeRoundTripTests

open System.Collections.Generic
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The tree-codec gate: `thaw (flatten f)` is STRUCTURALLY equal to `f` for every
// JS-gated conformance program's whole `Frozen.TastFile` — the expr/decl/pat tree,
// the inline vocabulary, the `Map<NodeKey,_>` side tables, the diagnostics list, and
// the two `IReadOnlyDictionary<SymbolKey,_>` fields. This extends the leaf gate
// (`FrozenCodecRoundTripTests`) to the full file.

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

/// The two `TastFileG` dictionary fields are `IReadOnlyDictionary<SymbolKey,_>`,
/// which carries only REFERENCE equality — a rebuilt `Dictionary` never `=`-matches
/// the original even with identical contents. Compare them as key→value SETS: same
/// count, and every key maps to an equal value (structural on `SymbolKey` via the
/// dictionary's default comparer). No canonical emit order is assumed (the cache key
/// hashes inputs, not the blob), so this is order-independent.
let private dictEqual (a: IReadOnlyDictionary<SymbolKey, 'v>) (b: IReadOnlyDictionary<SymbolKey, 'v>) : bool =
    a.Count = b.Count
    && a
       |> Seq.forall (fun (KeyValue(k, v)) ->
           match b.TryGetValue k with
           | true, v2 -> v = v2
           | _ -> false
       )

/// Whole-file structural equality: `=` for every field the derived structural
/// equality handles correctly (the tree, the `Map<_,_>` side tables, the
/// diagnostics list, the inline vocabulary), and `dictEqual` for the two
/// reference-equality dictionary fields.
let private structurallyEqual (a: Frozen.TastFile) (b: Frozen.TastFile) : bool =
    a.Decls = b.Decls
    && a.Diagnostics = b.Diagnostics
    && dictEqual a.IntrinsicReprKeys b.IntrinsicReprKeys
    && a.ModuleMembers = b.ModuleMembers
    && a.TopLevelNames = b.TopLevelNames
    && a.ClosureReprs = b.ClosureReprs
    && a.FunVerdicts = b.FunVerdicts
    && a.GenericFnSchemes = b.GenericFnSchemes
    && a.InlineBodies = b.InlineBodies
    && dictEqual a.Accessibility b.Accessibility
    && a.BindingValReprs = b.BindingValReprs
    && a.BindingTyparArities = b.BindingTyparArities

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
                        (structurallyEqual f rebuilt)
                        (sprintf "frozen file for %s did not survive flatten/thaw structurally" name)
            }

            // The gate must exercise a non-trivial corpus, else an empty run would make
            // `thaw ∘ flatten = id` vacuously true.
            test "the corpus exercises a non-trivial number of decls" {
                let totalDecls = frozenFiles |> List.sumBy (fun (_, f) -> f.Decls.Length)

                Expect.isGreaterThan (List.length frozenFiles) 0 "gated programs"
                Expect.isGreaterThan totalDecls 20 "total top-level decls across the corpus"
            }
        ]
