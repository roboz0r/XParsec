namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser

// A `type t = extern` names a primitive and says nothing about how the target spells it. The
// REPR comes from the paired implementation's `type t = (# "System.Int32" #)`, so a signature
// front end reads its companion's bindings before its own declarations.

module IntrinsicReprs =

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS:
    /// `(# "System.Int32" #)` ⇒ `"System.Int32"`.
    // TODO: raise diagnostics for the parts no repr can be read from (`Expr`, `InvalidText`).
    let ilString (nameOf: SyntaxToken -> string) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(nameOf t) |> ignore
            | StringPart.Expr _ -> ()

        sb.ToString()

    /// The intrinsic-representation bindings a parsed implementation declares, short name ⇒
    /// repr. Last binding wins.
    let ofImplementationInto
        (dest: Dictionary<string, string>)
        (nameOf: SyntaxToken -> string)
        (file: ImplementationFile<SyntaxToken>)
        : unit =
        for w in CstModuleTree.walkImpl nameOf OpenScope.empty file do
            match w.Elem with
            | ModuleElem.Type defs ->
                for td in defs do
                    match td with
                    | TypeDefn.Abbrev(typeName = TypeName(ident = li); typ = Type.ILIntrinsic(instrParts = parts)) when
                        li.Idents.Length = 1
                        ->
                        dest.[nameOf li.Idents.[0]] <- ilString nameOf parts
                    | _ -> ()
            | _ -> ()
