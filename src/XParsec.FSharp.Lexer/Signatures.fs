namespace XParsec.FSharp.Parser

// Represents: val mutable~opt curried-sig -- value signature
type ValSig<'T> =
    | ValSig of
        attributes: Attributes<'T> option *
        valToken: 'T *
        mutableToken: 'T option *
        ident: 'T *  // The identifier for the value
        colon: 'T *
        signature: CurriedSig<'T>

// Represents: type-signature-element, the members within a type signature
type TypeSignatureElement<'T> =
    | Constructor of
        attributes: Attributes<'T> option *
        access: Access<'T> option *
        newToken: 'T *
        colon: 'T *
        signature: UncurriedSig<'T>
    | Member of
        attributes: Attributes<'T> option *
        memberToken: 'T *
        access: Access<'T> option *
        signature: MemberSig<'T>
    | Abstract of
        attributes: Attributes<'T> option *
        abstractToken: 'T *
        access: Access<'T> option *
        signature: MemberSig<'T>
    | Override of attributes: Attributes<'T> option * overrideToken: 'T * signature: MemberSig<'T>
    | Default of attributes: Attributes<'T> option * defaultToken: 'T * signature: MemberSig<'T>
    | StaticMember of
        attributes: Attributes<'T> option *
        staticToken: 'T *
        memberToken: 'T *
        access: Access<'T> option *
        signature: MemberSig<'T>
    | Interface of spec: InterfaceSpec<'T>

// Represents the body of a type signature: begin type-elements-signature end
type TypeElementsSignature<'T> = TypeSignatureElement<'T> list

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
        extensions: TypeExtensionElementsSignature<'T> option
    | Union of
        typeName: TypeName<'T> *
        equals: 'T *
        cases: UnionTypeCases<'T> *
        extensions: TypeExtensionElementsSignature<'T> option
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

// Represents: type-signatures := type-signature ... and ... type-signature
type TypeSignatures<'T> = (TypeSignature<'T> * 'T (* 'and' token *) ) list

// Represents: module-signature-element
type ModuleSignatureElement<'T> =
    | Val of valSig: ValSig<'T>
    | ValLiteral of valToken: 'T * valueDefn: ValueDefn<'T>
    | Type of typeToken: 'T * typeSigs: TypeSignatures<'T>
    | Exception of exceptionToken: 'T * sigData: UnionTypeCaseData<'T>
    | Module of moduleSig: ModuleSignature<'T>
    | ModuleAbbrev of abbrev: ModuleAbbrev<'T>
    | Import of importDecl: ImportDecl<'T>

// Represents: module-signature-elements := module-signature-element ...
and ModuleSignatureElements<'T> = ModuleSignatureElement<'T> list

// Represents: module-signature-body := begin module-signature-elements end
and ModuleSignatureBody<'T> =
    | ModuleSignatureBody of beginToken: 'T * elements: ModuleSignatureElements<'T> * endToken: 'T

// Represents: module-signature := module ident = module-signature-body
and ModuleSignature<'T> = | ModuleSignature of moduleToken: 'T * ident: 'T * equals: 'T * body: ModuleSignatureBody<'T>

// Represents: namespace-decl-group-signature
type NamespaceDeclGroupSignature<'T> =
    | NamespaceDeclGroupSignature of
        namespaceToken: 'T *
        longIdent: LongIdent<'T> *
        elements: ModuleSignatureElements<'T>
