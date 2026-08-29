namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser

/// The target's own identifier for a type, as a type-position `(# "…" #)` binding spells it:
/// `"System.Int32"` on CLR, `"number"` on JS, `"!0[]"` for the array constructor. Never an
/// expression-position `(# … #)` opcode, which is a template, not a type.
[<Struct>]
type PlatformTypeId =
    | PlatformTypeId of string

    member this.Value =
        let (PlatformTypeId s) = this
        s

// A `type t = extern` declares a primitive and says nothing about how the target spells it. The
// platform type id comes from the paired implementation's `type t = (# "System.Int32" #)`, so a
// signature front end reads its companion's bindings before its own declarations.

module IntrinsicBindings =

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS:
    /// `(# "System.Int32" #)` ⇒ `"System.Int32"`.
    // TODO: raise diagnostics for the parts no id can be read from (`Expr`, `InvalidText`).
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

    /// The intrinsic bindings a parsed implementation declares, short name ⇒ platform type id.
    /// Last binding wins.
    let ofImplementationInto
        (dest: Dictionary<string, PlatformTypeId>)
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
                        dest.[nameOf li.Idents.[0]] <- PlatformTypeId(ilString nameOf parts)
                    | _ -> ()
            | _ -> ()
