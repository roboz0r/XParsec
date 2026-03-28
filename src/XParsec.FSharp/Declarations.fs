namespace XParsec.FSharp.Parser

// Represents: access := private | internal | public
[<RequireQualifiedAccess>]
type Access<'T> =
    | Private of privateToken: 'T
    | Internal of internalToken: 'T
    | Public of publicToken: 'T

// Represents: import-decl := open long-ident | open type long-ident
type ImportDecl<'T> =
    | ImportDecl of openToken: 'T * longIdent: LongIdent<'T>
    | ImportDeclType of openToken: 'T * typeToken: 'T * longIdent: LongIdent<'T>

// Represents: module-abbrev := module ident = long-ident
type ModuleAbbrev<'T> = | ModuleAbbrev of moduleToken: 'T * ident: 'T * equals: 'T * longIdent: LongIdent<'T>

// Represents: compiler-directive-decl := # ident string ... string
type CompilerDirectiveDecl<'T> = | CompilerDirectiveDecl of hash: 'T * ident: 'T * strings: 'T list

// Represents: module-function-or-value-defn
type ModuleFunctionOrValueDefn<'T> =
    | Let of attributes: Attributes<'T> voption * letToken: 'T * isRec: 'T voption * bindings: Binding<'T> list
    | Do of attributes: Attributes<'T> voption * doToken: 'T * expr: Expr<'T>

// Represents: module-elem
type ModuleElem<'T> =
    | FunctionOrValue of ModuleFunctionOrValueDefn<'T>
    | Type of TypeDefn<'T> list
    | Exception of ExceptionDefn<'T>
    | Module of ModuleDefn<'T>
    | ModuleAbbrev of ModuleAbbrev<'T>
    | Import of ImportDecl<'T>
    | CompilerDirective of CompilerDirectiveDecl<'T>
    | Expression of Expr<'T>
    | Missing
    | SkipsTokens of skippedTokens: 'T list

// Represents: module-elems := module-elem ... module-elem
and ModuleElems<'T> = ModuleElem<'T> list

// Represents: module-defn-body := begin module-elemsopt end
and ModuleDefnBody<'T> = | ModuleDefnBody of beginToken: 'T * elements: ModuleElems<'T> voption * endToken: 'T

// Represents: module-defn
and ModuleDefn<'T> =
    | ModuleDefn of
        attributes: Attributes<'T> voption *
        moduleToken: 'T *
        access: Access<'T> voption *
        isRec: 'T voption *
        ident: 'T *
        equals: 'T *
        body: ModuleDefnBody<'T>

// Represents: namespace-decl-group
type NamespaceDeclGroup<'T> =
    | Named of namespaceToken: 'T * isRec: 'T voption * longIdent: LongIdent<'T> * elements: ModuleElems<'T>
    | Global of namespaceToken: 'T * globalToken: 'T * elements: ModuleElems<'T>
