module XParsec.FSharp.SemanticAnalysis.Tests.ParseDiagnosticsTests

open Expecto
open XParsec.FSharp.Lexer
// Ahead of the SemanticAnalysis open so the bare `Diagnostic` is the semantic one; this is
// here for the parser's `DiagnosticCode`, which `Kind.Parse` wraps.
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// A file whose parse RECOVERS still has something to report: the tree came out complete
// only because the parser patched it, and every patch is a diagnostic.

/// `(1 + 2` — the paren is never closed, so the parser inserts a virtual `)` and the file
/// still parses to a complete tree.
let private unclosedParen = "let f () = (1 + 2\n"

/// `{| x = 1 }` — the close IS written, just the wrong one, and the parser consumes it as
/// the close. Nothing is inserted.
let private mismatchedClose = "let r = {| x = 1 }\n"

let private parsed (source: string) : ParseChain.ParsedFile =
    match ParseChain.parse Set.empty source with
    | Ok p -> p
    | Error f -> failtestf "expected a recovered parse, not a failure: %A" f.Diagnostics

let private ofKind (wanted: Kind -> bool) (what: string) (p: ParseChain.ParsedFile) : Diagnostic =
    match p.Diagnostics |> List.tryFind (fun d -> wanted d.Kind) with
    | Some d -> d
    | None -> failtestf "no %s diagnostic; got %A" what p.Diagnostics

let private unclosedDelimiter (p: ParseChain.ParsedFile) : Diagnostic =
    p
    |> ofKind
        (function
        | Kind.Parse(DiagnosticCode.UnclosedDelimiter _) -> true
        | _ -> false)
        "unclosed delimiter"

let private mismatchedDelimiter (p: ParseChain.ParsedFile) : Diagnostic =
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

            // A `with` clause binds `get` and `set` and nothing else, so a third name is a
            // GRAMMAR mistake. The message names the property and the offending word, since
            // neither is recoverable from the position alone.
            test "a `with` clause naming neither get nor set is a parse error" {
                let p = parsed "type C() =\n    member this.P with frobnicate () = 1\n"

                match p.Diagnostics |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "Expected 'get' or 'set' after 'with' on 'P' but got 'frobnicate'"
                        "names the property and the word written"
                | other -> failtestf "expected exactly one diagnostic, got %A" other
            }

            test "a rejected `with` half recovers to the next member" {
                let p =
                    parsed "type C() =\n    member this.P with frobnicate () = 1\n    member this.Q = 2\n"

                Expect.equal p.Diagnostics.Length 1 "the good member after it parses without further complaint"
            }

            // The property ident is consumed before the `with` on this path, so it reaches
            // the diagnostic differently from the `member this.P` form above.
            test "a static property with no self-identifier is checked too" {
                let p = parsed "type C() =\n    static member P with frobnicate () = 1\n"

                match p.Diagnostics |> List.map (fun d -> d.Message) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "Expected 'get' or 'set' after 'with' on 'P' but got 'frobnicate'"
                        "names the property and the word written"
                | other -> failtestf "expected exactly one diagnostic, got %A" other
            }

            test "a well-formed `with get`/`set` clause needs no recovery" {
                let p =
                    parsed "type C() =\n    member this.P with get () = 1 and set (v: int) = ()\n"

                Expect.isEmpty p.Diagnostics "get and set are the two names the clause admits"
            }

            test "a static `with get`/`set` clause needs no recovery" {
                let p =
                    parsed "type C() =\n    static member P with get () = 1 and set (v: int) = ()\n"

                Expect.isEmpty p.Diagnostics "the no-self-identifier path admits the same two names"
            }

            // The two delimiter codes say different things about the same shape of mistake,
            // and the only observable difference is the primary site.
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

            // Built by hand: the top-level parser recovers to a tree rather than failing, so
            // no source produces a failed file. A file with no `Lexed` FAULTS on a positioned
            // diagnostic, so a mistake in the plumbing is a crash, not a plausible bad line.
            test "a failed file's diagnostics resolve against its own token stream" {
                // No trailing newline, so the end of the file is a column on line 1.
                let source = "let f () = (1 + 2"

                let lexed =
                    match Lexing.lexString source with
                    | Result.Ok lexed -> lexed
                    | Result.Error e -> failtestf "lex failed: %A" e

                let recovered =
                    match ParseChain.parse Set.empty source with
                    | Ok p -> p.Diagnostics
                    | Error f -> f.Diagnostics

                let anchored =
                    AssemblyFiles.failureDiagnostics
                        {
                            Id = AssemblyFileId.ofRelative "broken.fs"
                            Failure =
                                {
                                    Lexed = ValueSome lexed
                                    Diagnostics = recovered
                                }
                        }

                Expect.isNonEmpty anchored "the failed file's diagnostics came out"
                Expect.all anchored (fun a -> a.Path.Name = "broken.fs") "each anchored to the failed file"

                // The `)` belonged past the last token written: line 1, one column past `2`
                // — resolved against the text, not the (1, 1) of a placeless diagnostic.
                Expect.all
                    anchored
                    (fun a -> (a.Line, a.Col) = (1, source.Length + 1))
                    (sprintf "resolved against the file's own text: %A" anchored)
            }

            test "a file with no token stream renders its diagnostics at the file head" {
                let anchored =
                    AssemblyFiles.failureDiagnostics
                        {
                            Id = AssemblyFileId.ofRelative "unlexable.fs"
                            Failure =
                                {
                                    Lexed = ValueNone
                                    Diagnostics = [ Diagnostic.nowhere (Kind.LexFailure "unreadable") ]
                                }
                        }

                match anchored with
                | [ a ] ->
                    Expect.equal a.Path.Name "unlexable.fs" "anchored to the file"
                    Expect.equal (a.Line, a.Col) (1, 1) "the file head"
                | other -> failtestf "expected one anchored diagnostic, got %A" other
            }
        ]
