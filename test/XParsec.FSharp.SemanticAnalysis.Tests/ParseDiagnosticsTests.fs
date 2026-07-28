module XParsec.FSharp.SemanticAnalysis.Tests.ParseDiagnosticsTests

open Expecto
open XParsec.FSharp.Lexer
// Ahead of the SemanticAnalysis open so the bare `Diagnostic` is the semantic one; this is
// here for the parser's `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// A file whose parse RECOVERS still has something to report: the tree came out complete
// only because the parser patched it, and every patch is a diagnostic. These assert at the
// seam itself (`Pipeline.parse`) — the one place a parse diagnostic becomes a semantic one
// — and on the two ways such a diagnostic then reaches a line and column.

/// `(1 + 2` — the paren is never closed, so the parser inserts a virtual `)` and the file
/// still parses to a complete tree.
let private unclosedParen = "let f () = (1 + 2\n"

/// `{| x = 1 }` — the close IS written, just the wrong one, and the parser consumes it as
/// the close. Nothing is inserted.
let private mismatchedClose = "let r = {| x = 1 }\n"

let private parsed (source: string) : Pipeline.ParsedUnit =
    match Pipeline.parse source with
    | Ok p -> p
    | Error f -> failtestf "expected a recovered parse, not a failure: %A" f.Diagnostics

/// The diagnostic whose VERDICT is the one asked for. Asked of the `Kind` and not of a
/// message substring or a stringly code: the classification is the thing the diagnostic
/// carries, so a consumer selecting on it cannot be broken by a reworded sentence.
let private ofKind (wanted: Kind -> bool) (what: string) (p: Pipeline.ParsedUnit) : Diagnostic =
    match p.Diagnostics |> List.tryFind (fun d -> wanted d.Kind) with
    | Some d -> d
    | None -> failtestf "no %s diagnostic; got %A" what p.Diagnostics

let private unclosedDelimiter (p: Pipeline.ParsedUnit) : Diagnostic =
    p
    |> ofKind
        (function
        | Kind.Parse(DiagnosticCode.UnclosedDelimiter _) -> true
        | _ -> false)
        "unclosed delimiter"

let private mismatchedDelimiter (p: Pipeline.ParsedUnit) : Diagnostic =
    p
    |> ofKind
        (function
        | Kind.Parse(DiagnosticCode.MismatchedDelimiter _) -> true
        | _ -> false)
        "mismatched delimiter"

/// The single label every delimiter diagnostic carries, on the delimiter left open.
let private openerLabel (d: Diagnostic) : Label =
    match d.Related with
    | [ label ] -> label
    | other -> failtestf "expected exactly one label, on the opener, got %A" other

[<Tests>]
let tests =
    testList
        "ParseDiagnostics"
        [
            test "a parse that RECOVERS reports positioned diagnostics" {
                let p = parsed unclosedParen
                Expect.isNonEmpty p.Diagnostics "recovery reported something"

                Expect.all
                    p.Diagnostics
                    (fun d -> d.Site <> Site.Nowhere)
                    (sprintf "every diagnostic names a place: %A" p.Diagnostics)
            }

            test "a parse that needs no recovery reports nothing" {
                Expect.isEmpty (parsed "let f () = 1 + 2\n").Diagnostics "nothing to recover, nothing to report"
            }

            // The two delimiter codes exist to say different things about the same shape of
            // mistake, and the ONLY observable difference is the primary site. Asserting
            // them together is what stops one collapsing back into the other.
            test "an INSERTED close blames the hole it went into" {
                let d = parsed unclosedParen |> unclosedDelimiter

                match d.Site with
                | Site.After _ -> ()
                | other -> failtestf "expected the gap the ')' belonged in, got %A" other

                match (openerLabel d).Site with
                | Site.At _ -> ()
                | other -> failtestf "expected the '(' token, got %A" other
            }

            test "a WRONG close blames the token itself" {
                let d = parsed mismatchedClose |> mismatchedDelimiter

                // Nothing was inserted — the offending token was consumed as the close — so
                // there is no gap to name and the token is the mistake.
                match d.Site with
                | Site.At _ -> ()
                | other -> failtestf "expected the wrong close token, got %A" other

                match (openerLabel d).Site with
                | Site.At _ -> ()
                | other -> failtestf "expected the '{|' token, got %A" other
            }

            // `AssemblyUnits.failureDiagnostics` resolves a failed unit's diagnostics, and
            // `unpositionedDiagnostics` FAULTS on a positioned one rather than printing a
            // plausible line — so a mistake in the `Lexed` plumbing is a crash, not a bad
            // message. Built by hand because the top-level parser is infallible: it recovers
            // to a tree rather than failing, so no source reaches the positioned branch yet.
            test "a failed unit's diagnostics resolve against its own token stream" {
                // No trailing newline, so the end of the file is a column on line 1.
                let source = "let f () = (1 + 2"

                let lexed =
                    match Lexing.lexString source with
                    | Result.Ok lexed -> lexed
                    | Result.Error e -> failtestf "lex failed: %A" e

                let recovered =
                    match Pipeline.parse source with
                    | Ok p -> p.Diagnostics
                    | Error f -> f.Diagnostics

                let anchored =
                    AssemblyUnits.failureDiagnostics
                        {
                            Path = "broken.fs"
                            Input = source
                            Failure =
                                {
                                    Lexed = ValueSome lexed
                                    Diagnostics = recovered
                                }
                        }

                Expect.isNonEmpty anchored "the failed unit's diagnostics came out"
                Expect.all anchored (fun a -> a.Path = "broken.fs") "each anchored to the failed unit"

                // The `)` belonged past the last token written, which is the end of the
                // file: line 1, one column past `2`. A resolved position, not a fault and
                // not the (1, 1) a placeless diagnostic would render at.
                Expect.all
                    anchored
                    (fun a -> (a.Line, a.Col) = (1, source.Length + 1))
                    (sprintf "resolved against the unit's own text: %A" anchored)
            }

            test "a unit with no token stream renders its diagnostics at the file head" {
                let anchored =
                    AssemblyUnits.failureDiagnostics
                        {
                            Path = "unlexable.fs"
                            Input = ""
                            Failure =
                                {
                                    Lexed = ValueNone
                                    Diagnostics = [ Diagnostic.nowhere (Kind.LexFailure "unreadable") ]
                                }
                        }

                match anchored with
                | [ a ] ->
                    Expect.equal a.Path "unlexable.fs" "anchored to the unit"
                    Expect.equal (a.Line, a.Col) (1, 1) "the file head"
                | other -> failtestf "expected one anchored diagnostic, got %A" other
            }
        ]
