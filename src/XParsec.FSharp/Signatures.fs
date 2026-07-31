namespace XParsec.FSharp.Parser

open XParsec.FSharp

// Represents: val [inline] [access] [mutable] ident [typars] : curried-sig [= literal-expr]
//
// `ident` is IdentOrOp so val sigs can use operator names (`val (?>=): T -> T -> bool`) or
// active-pattern names (`val (|Foo|_|): T -> T option`).
// `literalValue` captures the `= expr` tail for `[<Literal>] val Foo: string = "..."` forms.
type ValSig<'T> =
    | ValSig of
        attributes: Attributes<'T> voption *
        valToken: 'T *
        inlineToken: 'T voption *
        access: Access<'T> voption *
        mutableToken: 'T voption *
        ident: IdentOrOp<'T> *
        typars: TyparDefns<'T> voption *
        colon: 'T *
        signature: CurriedSig<'T> *
        literalValue: ('T (* '=' *) * Expr<'T>) voption

// Represents: type-signature-element, the members within a type signature
type TypeSignatureElement<'T> =
    | Constructor of
        attributes: Attributes<'T> voption *
        access: Access<'T> voption *
        newToken: 'T *
        colon: 'T *
        signature: UncurriedSig<'T>
    | Member of
        attributes: Attributes<'T> voption *
        memberToken: 'T *
        inlineToken: 'T voption *
        access: Access<'T> voption *
        signature: MemberSig<'T>
    | Abstract of
        attributes: Attributes<'T> voption *
        abstractToken: 'T *
        memberToken: 'T voption *
        access: Access<'T> voption *
        signature: MemberSig<'T>
    | Override of attributes: Attributes<'T> voption * overrideToken: 'T * signature: MemberSig<'T>
    | Default of attributes: Attributes<'T> voption * defaultToken: 'T * signature: MemberSig<'T>
    | StaticMember of
        attributes: Attributes<'T> voption *
        staticToken: 'T *
        memberToken: 'T *
        inlineToken: 'T voption *
        access: Access<'T> voption *
        signature: MemberSig<'T>
    | Interface of spec: InterfaceSpec<'T>
    // Class-field val signature (e.g. `val x : int` inside `type C = class ... end`).
    // Mirrors MemberDefn.Value on the implementation side.
    | Value of
        attributes: Attributes<'T> voption *
        staticToken: 'T voption *
        valToken: 'T *
        mutableToken: 'T voption *
        access: Access<'T> voption *
        ident: 'T *
        colon: 'T *
        typ: Type<'T>
    | Inherit of ClassInheritsDecl<'T>

// Represents the body of a type signature: begin type-elements-signature end
type TypeElementsSignature<'T> = ImArr<TypeSignatureElement<'T>>

// Represents: with type-elements-signature end
type TypeExtensionElementsSignature<'T> =
    | TypeExtensionElementsSignature of withToken: 'T * elements: TypeElementsSignature<'T> * endToken: 'T

// Represents: type-signature
type TypeSignature<'T> =
    | Abbrev of typeName: TypeName<'T> * equals: 'T * typ: Type<'T>
    | Record of
        typeName: TypeName<'T> *
        equals: 'T *
        lBrace: 'T *
        fields: RecordFields<'T> *
        rBrace: 'T *
        extensions: TypeExtensionElementsSignature<'T> voption
    | Union of
        typeName: TypeName<'T> *
        equals: 'T *
        cases: UnionTypeCases<'T> *
        extensions: TypeExtensionElementsSignature<'T> voption
    | Anon of typeName: TypeName<'T> * equals: 'T * beginToken: 'T * elements: TypeElementsSignature<'T> * endToken: 'T
    | Class of typeName: TypeName<'T> * equals: 'T * classToken: 'T * elements: TypeElementsSignature<'T> * endToken: 'T
    | Struct of
        typeName: TypeName<'T> *
        equals: 'T *
        structToken: 'T *
        elements: TypeElementsSignature<'T> *
        endToken: 'T
    | Interface of
        typeName: TypeName<'T> *
        equals: 'T *
        interfaceToken: 'T *
        elements: TypeElementsSignature<'T> *
        endToken: 'T
    | Enum of typeName: TypeName<'T> * equals: 'T * cases: EnumTypeCases<'T>
    | Delegate of typeName: TypeName<'T> * equals: 'T * signature: DelegateSig<'T>
    | TypeExtension of typeName: TypeName<'T> * elements: TypeExtensionElementsSignature<'T>
    // Opaque type signature (no `=` body), e.g. `[<Measure>] type kg` or `type T`.
    | AbstractType of typeName: TypeName<'T>
    // Intrinsic primitive signature: `type int = extern`. Declares "the target
    // provides this; there is no Vesper representation" — the impl-side pair is a
    // `TypeDefn.Abbrev` whose RHS is `Type.ILIntrinsic`. Distinct from `AbstractType`
    // (opaque/abstract) and from `Abbrev` (a real type alias). The optional trailing
    // `with member … / interface …` (same surface as `Record`'s extension) publishes
    // the capability surface a later extractor consumes; `ValueNone` is the bare form.
    | Extern of
        typeName: TypeName<'T> *
        equals: 'T *
        externToken: 'T *
        // Optional `class` / `interface` tag. `class` (`type Attribute = extern class`)
        // marks a HERITABLE external reference base (repr extracted from the paired
        // `.fs`'s `(# class "…" #)`); `interface` (`type disposable = extern interface
        // with …`) marks a capability INTERFACE (all-abstract surface, published as an
        // `IntrinsicInterface`). Both are distinct from the bare `extern` opaque value
        // capability. `ValueNone` is the bare form (`type int = extern`). The parser
        // admits `interface` only when a `with` follows it, so it never collides with an
        // `interface …` capability member (which always carries a type name).
        kindTag: ExternKind<'T> voption *
        members: TypeExtensionElementsSignature<'T> voption

// Represents: type-signatures := type-signature ... and ... type-signature
// The first item has no preceding 'and'; subsequent items each carry their 'and' token.
type TypeSignatures<'T> = | TypeSignatures of first: TypeSignature<'T> * rest: ImArr<'T (* 'and' *) * TypeSignature<'T>>

// Represents: module-signature-element
type ModuleSignatureElement<'T> =
    | Val of valSig: ValSig<'T>
    | ValLiteral of valToken: 'T * binding: Binding<'T>
    | Type of typeToken: 'T * typeSigs: TypeSignatures<'T>
    | Exception of attributes: Attributes<'T> voption * exceptionToken: 'T * sigData: UnionTypeCaseData<'T>
    | Module of moduleSig: ModuleSignature<'T>
    | ModuleAbbrev of abbrev: ModuleAbbrev<'T>
    | Import of importDecl: ImportDecl<'T>
    | CompilerDirective of CompilerDirectiveDecl<'T>
    | Missing
    | SkipsTokens of skippedTokens: ImArr<'T>

// Represents: module-signature-elements := module-signature-element ...
and ModuleSignatureElements<'T> = ImArr<ModuleSignatureElement<'T>>

// Represents: module-signature-body := begin module-signature-elements end
and ModuleSignatureBody<'T> =
    | ModuleSignatureBody of beginToken: 'T * elements: ModuleSignatureElements<'T> * endToken: 'T

// Represents: [attrs] module [access] [rec] ident = module-signature-body
and ModuleSignature<'T> =
    | ModuleSignature of
        attributes: Attributes<'T> voption *
        moduleToken: 'T *
        access: Access<'T> voption *
        isRec: 'T voption *
        ident: 'T *
        equals: 'T *
        body: ModuleSignatureBody<'T>

// Represents: namespace-decl-group-signature
type NamespaceDeclGroupSignature<'T> =
    | Named of namespaceToken: 'T * isRec: 'T voption * longIdent: LongIdent<'T> * elements: ModuleSignatureElements<'T>
    | Global of namespaceToken: 'T * globalToken: 'T * elements: ModuleSignatureElements<'T>
