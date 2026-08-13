module XParsec.FSharp.SemanticAnalysis.Tests.UnmodelledExternalTypeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// An `ExternalTypeShape.Unmodelled` names a type the contract registered without a body, so
// no `SemType` can be built for an annotation naming it. What the annotation yields is the
// question these pin: the reference must report ONCE, naming the gap, and must do so
// identically however the name is spelled — a bare `Widget` and a dotted `Tests.Widget`
// reach that verdict through different arms of `translateType`.

/// The stub layered OVER `realProvider`, so the primitives an ordinary expression needs
/// (`int`, `string`) still resolve through the real contract stack.
let private providerFor (shape: ExternalTypeShape) : IExternalSymbolProvider =
    let stub =
        ExternalSymbolProviders.ofNamedChannels
            { ExternalSymbolProviders.NamedChannels.empty with
                TryLookupType =
                    fun n ->
                        match n with
                        | "Tests.Widget" -> ValueSome shape
                        | _ -> ValueNone
                AmbientOpenPrefixes = [ "Tests" ]
            }

    ExternalSymbolProviders.composite [ stub; realProvider.Value ]

let private analyse (shape: ExternalTypeShape) (input: string) : PassContext =
    let ctx, file = analyseNameRes (providerFor shape) input
    Unification.run ctx file
    ctx

let private messages (ctx: PassContext) : string list =
    [ for d in ctx.Diagnostics -> d.Message ]

let private delegateShape =
    ExternalTypeShape.Unmodelled(UnmodelledReason.Delegate, 0)

[<Tests>]
let tests =
    testList
        "UnmodelledExternalType"
        [
            test "an annotation naming an unmodelled body reports the gap, not an undefined name" {
                let msgs = analyse delegateShape "let f (w: Widget) = w" |> messages

                Expect.hasLength msgs 1 (sprintf "exactly one diagnostic, got %A" msgs)

                Expect.stringContains msgs.[0] "not yet supported" "the gap is reported as a feature gap"
                Expect.stringContains msgs.[0] "a delegate type" "the reason names WHICH form is missing"
                Expect.stringContains msgs.[0] "Widget" "the diagnostic names the type"

                // The name DOES resolve — reporting it as undefined would send the author
                // looking for a missing reference rather than an unimplemented declaration form.
                Expect.isFalse (msgs.[0].Contains "is not defined") "not reported as an undefined name"
            }

            // The divergence this suite exists for: the bare and dotted spellings resolve
            // through separate arms, and previously yielded different residues — an opaque
            // nominal for the bare name and a FREE TyVar for the dotted one, which unified
            // with anything. Both must now report the same thing.
            test "the dotted spelling reports identically to the bare one" {
                let bare = analyse delegateShape "let f (w: Widget) = w" |> messages
                let dotted = analyse delegateShape "let f (w: Tests.Widget) = w" |> messages

                Expect.hasLength dotted 1 (sprintf "exactly one diagnostic, got %A" dotted)
                Expect.stringContains dotted.[0] "not yet supported" "the dotted spelling reports the gap too"
                Expect.stringContains dotted.[0] "a delegate type" "with the same reason"
                Expect.equal (List.length dotted) (List.length bare) "the two spellings report the same count"
            }

            // Recovery is `errorTy`'s fresh TyVar, so uses of the annotated value still pin
            // freely — deliberately, so one unbuildable annotation raises one diagnostic
            // rather than a cascade at every use. What changed is that the gap is reported
            // AT the annotation; previously the dotted spelling pinned freely and said nothing.
            test "the gap is reported once at the annotation, and uses do not cascade" {
                let msgs =
                    analyse delegateShape "let f (w: Tests.Widget) = w\nlet g = f 1" |> messages

                Expect.hasLength msgs 1 (sprintf "the annotation reports; the use does not, got %A" msgs)
                Expect.stringContains msgs.[0] "not yet supported" "the one diagnostic is the gap"
            }

            // The other reason: the kind IS modelled, this declaration's body did not
            // translate. That is a broken contract, not an unimplemented feature, so it
            // reports as a message carrying the extractor's own reason.
            test "an extraction failure reports the extractor's reason, not a feature gap" {
                let shape =
                    ExternalTypeShape.Unmodelled(UnmodelledReason.ExtractionFailed "unnamed case", 0)

                let msgs = analyse shape "let f (w: Widget) = w" |> messages

                Expect.hasLength msgs 1 (sprintf "exactly one diagnostic, got %A" msgs)
                Expect.stringContains msgs.[0] "did not extract" "reported as a contract defect"
                Expect.stringContains msgs.[0] "unnamed case" "carries the reason the extractor recorded"
                Expect.isFalse (msgs.[0].Contains "not yet supported") "not reported as a feature gap"
            }

            // The contrast: a name NOTHING resolves keeps the undefined-name diagnostic.
            test "a name no shape answers is still reported as undefined" {
                let msgs = analyse delegateShape "let f (w: Sprocket) = w" |> messages

                Expect.hasLength msgs 1 (sprintf "exactly one diagnostic, got %A" msgs)
                Expect.stringContains msgs.[0] "is not defined" "an unresolvable name is undefined, not a gap"
            }
        ]
