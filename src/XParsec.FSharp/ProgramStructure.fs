namespace XParsec.FSharp.Parser

// Represents: named-module := module long-ident module-elems
type NamedModule<'T> = | NamedModule of moduleToken: 'T * longIdent: LongIdent<'T> * elements: ModuleElems<'T>

// Represents: anonymous-module := module-elems
// This is simply an alias for ModuleElems<'T> for clarity at the file level.
type AnonymousModule<'T> = ModuleElems<'T>

// Represents: named-module-signature := module long-ident module-signature-elements
type NamedModuleSignature<'T> =
    | NamedModuleSignature of moduleToken: 'T * longIdent: LongIdent<'T> * elements: ModuleSignatureElements<'T>

// Represents: anonymous-module-signature := module-signature-elements
// This is an alias for ModuleSignatureElements<'T>.
type AnonymousModuleSignature<'T> = ModuleSignatureElements<'T>

// Represents: implementation-file
type ImplementationFile<'T> =
    | Namespaces of declarations: NamespaceDeclGroup<'T> list
    | NamedModule of moduleDefn: NamedModule<'T>
    | AnonymousModule of elements: AnonymousModule<'T>

// Represents: signature-file
type SignatureFile<'T> =
    | Namespaces of declarations: NamespaceDeclGroupSignature<'T> list
    | NamedModule of moduleSig: NamedModuleSignature<'T>
    | AnonymousModule of elements: AnonymousModuleSignature<'T>

// Represents: script-file
// According to the grammar, a script file has the same structure
// as an implementation file, but may allow extra directives.
// We can represent it as a wrapper for clarity.
type ScriptFile<'T> = | Script of implementation: ImplementationFile<'T>

// Represents: script-fragment
// This is typically for code sent to F# Interactive (FSI).
type ScriptFragment<'T> = | ScriptFragment of elements: ModuleElems<'T>

type FSharpAst<'T> =
    | ImplementationFile of file: ImplementationFile<'T>
    | SignatureFile of file: SignatureFile<'T>
    | ScriptFile of file: ScriptFile<'T>
    | ScriptFragment of fragment: ScriptFragment<'T>
