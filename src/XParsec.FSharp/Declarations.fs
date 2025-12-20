namespace XParsec.FSharp.Parser

// Represents: access := private | internal | public
type Access<'T> =
    | Private of privateToken: 'T
    | Internal of internalToken: 'T
    | Public of publicToken: 'T

// Represents: import-decl := open long-ident
type ImportDecl<'T> = | ImportDecl of openToken: 'T * longIdent: LongIdent<'T>

// Represents: module-abbrev := module ident = long-ident
type ModuleAbbrev<'T> = | ModuleAbbrev of moduleToken: 'T * ident: 'T * equals: 'T * longIdent: LongIdent<'T>

// Represents: compiler-directive-decl := # ident string ... string
type CompilerDirectiveDecl<'T> = | CompilerDirectiveDecl of hash: 'T * ident: 'T * strings: 'T list

// Represents: module-function-or-value-defn
type ModuleFunctionOrValueDefn<'T> =
    | LetFunction of attributes: Attributes<'T> option * letToken: 'T * functionDefn: FunctionDefn<'T>
    | LetValue of attributes: Attributes<'T> option * letToken: 'T * valueDefn: ValueDefn<'T>
    | LetRec of
        attributes: Attributes<'T> option *
        letToken: 'T *
        recToken: 'T option *
        defns: FunctionOrValueDefn<'T> list
    | Do of attributes: Attributes<'T> option * doToken: 'T * expr: Expr<'T>

// Represents: module-elem
type ModuleElem<'T> =
    | FunctionOrValue of ModuleFunctionOrValueDefn<'T>
    | Type of TypeDefn<'T>
    | Exception of ExceptionDefn<'T>
    | Module of ModuleDefn<'T>
    | ModuleAbbrev of ModuleAbbrev<'T>
    | Import of ImportDecl<'T>
    | CompilerDirective of CompilerDirectiveDecl<'T>

// Represents: module-elems := module-elem ... module-elem
and ModuleElems<'T> = ModuleElem<'T> list

// Represents: module-defn-body := begin module-elemsopt end
and ModuleDefnBody<'T> = | ModuleDefnBody of beginToken: 'T * elements: ModuleElems<'T> option * endToken: 'T

// Represents: module-defn
and ModuleDefn<'T> =
    | ModuleDefn of
        attributes: Attributes<'T> option *
        moduleToken: 'T *
        access: Access<'T> option *
        ident: 'T *
        equals: 'T *
        body: ModuleDefnBody<'T>

// Represents: namespace-decl-group
type NamespaceDeclGroup<'T> =
    | Named of namespaceToken: 'T * longIdent: LongIdent<'T> * elements: ModuleElems<'T>
    | Global of namespaceToken: 'T * globalToken: 'T * elements: ModuleElems<'T>
