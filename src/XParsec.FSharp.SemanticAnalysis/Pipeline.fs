namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

module Pipeline =

    let analyse
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        let ctx = PassContext(provider, input, lexed)
        Desugar.run ctx file
        NameResolution.run ctx file
        Unification.run ctx file
        Regions.run ctx file
        Validation.run ctx file
        Freeze.run ctx file
