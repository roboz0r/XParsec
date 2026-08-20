namespace XParsec.FSharp.SemanticAnalysis

// The PARTS an external declaration is made of: a symbol, a field / case / enum case, a
// signature, a member, and the class or intrinsic surface a type carries.

/// Why a registered type name carries no modelled body.
[<RequireQualifiedAccess>]
type UnmodelledReason =
    | Delegate
    | TypeExtension
    /// The kind IS modelled; this declaration's body did not translate.
    | ExtractionFailed of reason: string

    /// The phrase a diagnostic or fault refers to this by.
    member this.Description: string =
        match this with
        | Delegate -> "a delegate type"
        | TypeExtension -> "a type extension"
        | ExtractionFailed reason -> sprintf "a body that did not translate (%s)" reason

exception BodylessExternalShape of compiledName: string * reason: UnmodelledReason with
    override this.Message =
        sprintf
            "mkNominal: '%s' is %s, whose body this compiler does not model. Model its kind before a contract names it"
            this.compiledName
            this.reason.Description

/// A constraint captured on an external symbol's typar list; every `target` is a template
/// over the symbol's own declaring typars.
[<RequireQualifiedAccess>]
type ExternalConstraint =
    /// `when 'T : equality`.
    | Trait of typarIndex: int * kind: SemanticConstraintKind
    /// `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)` — `typarIndices` is the
    /// trait's LHS; `memberName` is the compiled name (`op_Addition`).
    | MemberTrait of
        typarIndices: EqArray<int> *
        memberName: string *
        argTypes: EqArray<FrozenType> *
        returnType: FrozenType
    /// `default ^T : <ty>` at generalisation. `target` is another typar
    /// (`default ^T3 : ^T1`) or a concrete shape (`default ^T1 : int`).
    | Default of typarIndex: int * target: FrozenType
    /// `when 'e :> <ty>`.
    | Coercion of typarIndex: int * target: FrozenType

/// How a symbol's home module EXPORTS it, deciding the JS `import` statement shape.
[<RequireQualifiedAccess>]
type ImportForm =
    /// `import { x } from …` — the default for every non-TS producer.
    | Named
    /// `import x from …` — a TS `export default`, which cannot be imported by name.
    | Default
    /// A TS `export =` (CommonJS `module.exports = X`). Under the esModuleInterop
    /// lowering, `export =` binds the whole `module.exports` to a DEFAULT import.
    | CommonJs
    /// `import * as ns from …` with member access `ns.x` (`import * as fs from "fs"`).
    | Namespace

/// A published inline body as the provider serves it: the declaring file's own template,
/// handed across the boundary VERBATIM for the consumer to thaw.
type InlineBody =
    {
        Decl: Wire.TDecl
        ParamAttrs: EqArray<ParamAttrs>
        /// The declaring file's anchors: its text and token table. `Decl`'s nodes carry
        /// token INDICES into that file, unreadable without it.
        File: LexedFile
    }

[<RequireQualifiedAccess>]
module InlineBody =

    let anchoredIn (file: LexedFile) (decl: Wire.TDecl) (paramAttrs: EqArray<ParamAttrs>) : InlineBody =
        {
            Decl = decl
            ParamAttrs = paramAttrs
            File = file
        }

type ExternalSymbol =
    {
        Name: string
        /// The symbol's type SCHEME over its own typars, baked as `FTTypar(Declaring,i)`.
        Scheme: FrozenType
        TyparArity: int
        /// Stamped onto the fresh TyVars when the scheme is realised; callers don't apply
        /// them separately.
        Constraints: ExternalConstraint list
        /// Where the symbol lives. `SymbolOrigin.Empty` until a resolving source fills it.
        Origin: SymbolOrigin
        /// Interned identity: declaring container + simple name, for exact identity checks
        /// ("is this `Vesper.Collections.List.fold`?") instead of suffix-matching the written name.
        Key: BindingKey
        /// The producer's SOURCE parameter grouping; `ValueNone` for anything not
        /// contract-extracted.
        ValRepr: TastAccessor.ValRepr voption
        ImportForm: ImportForm
        /// The symbol's splice TEMPLATE: a `val inline` whose home file published its body.
        InlineBody: InlineBody voption
    }

/// Per-field shape inside an `ExternalTypeShape.Record`.
type ExternalFieldShape =
    {
        Name: string
        IsMutable: bool
        /// The field type with the enclosing type's typars baked as `FTTypar(Declaring,i)`.
        Frozen: FrozenType
    }

    /// A field whose template is deferred until the registry is complete.
    static member create(name: string, isMutable: bool) : ExternalFieldShape =
        {
            Name = name
            IsMutable = isMutable
            Frozen = deferredTemplate
        }

/// Per-case shape inside an `ExternalTypeShape.Union`. A `FieldNames` entry is `ValueNone`
/// for a positional field, `ValueSome n` for `of n: int`; aligned with `FrozenFieldTypes`.
type ExternalCaseShape =
    {
        Name: string
        FieldNames: EqArray<string voption>
        /// The field types with the enclosing type's typars baked as `FTTypar(Declaring,i)`.
        FrozenFieldTypes: EqArray<FrozenType>
    }

    /// A case whose field templates are deferred until the registry is complete.
    static member create(name: string, fieldNames: EqArray<string voption>) : ExternalCaseShape =
        {
            Name = name
            FieldNames = fieldNames
            FrozenFieldTypes = fieldNames |> EqArray.map (fun _ -> deferredTemplate)
        }

/// An external enum case's compile-time value. No numeric WIDTH: a TS import has no width
/// notion, so `IntVal` is always `int64`.
[<RequireQualifiedAccess>]
type ExternalEnumCaseValue =
    | IntVal of int64
    | StringVal of string

/// Per-case shape inside an `ExternalTypeShape.Enum`: the identifier (the `C1` of `E.C1`)
/// plus its compile-time value. Case order matches the manifest / TS source.
type ExternalEnumCaseShape =
    {
        Name: string
        Value: ExternalEnumCaseValue
    }

/// Result of a reverse union-case lookup: the declaring union's identity plus the matched
/// case shape.
type ExternalUnionCase =
    {
        /// The declaring union's compiled (arity-suffixed) name (`Vesper.Option`,
        /// `Vesper.Choice`2`).
        UnionName: string
        TyparArity: int
        /// Where the union is declared. `SymbolOrigin.Empty` for providers that don't model
        /// origins.
        Origin: SymbolOrigin
        Case: ExternalCaseShape
        IsRequireQualifiedAccess: bool
    }

    /// Does a reference written with `qualifier` resolve to this case? A bare (`ValueNone`)
    /// one does not when the union is RQA, because F# requires `Color.Red`.
    member uc.ResolvesWith(qualifier: string voption) : bool =
        match qualifier with
        | ValueNone -> not uc.IsRequireQualifiedAccess
        | ValueSome q -> SymbolKeyOps.shortName uc.UnionName = q

/// One hit from the per-field reverse index: a record declaring the queried field. Field
/// TYPES are not here, because they come from the shape.
type ExternalRecordCandidate =
    {
        /// The exact `TypeKey` the declaring file minted: a module-held record's `InModule`
        /// containment chain cannot be recut from a compiled-name string, which yields the same
        /// metadata NAME under an unequal identity.
        TypeKey: TypeKey
        TyparArity: int
        /// Where the record is declared. `SymbolOrigin.Empty` for providers that don't model
        /// origins.
        Origin: SymbolOrigin
        /// EVERY declared field name, not only the queried one.
        FieldNames: EqArray<string>
        /// `[<RequireQualifiedAccess>]`: a consumer excludes such a record from bare
        /// field-set resolution: `{ X = … }` must be written `{ R.X = … }`.
        IsRequireQualifiedAccess: bool
    }

/// A member's type as `FrozenType` templates, with open typars baked as `FTTypar(Declaring,i)`
/// (the declaring type's) / `FTTypar(Method,j)` (its own).
type ExternalSignature =
    {
        DeclaringTyparArity: int
        MethodTyparArity: int
        /// One entry per `->` the source wrote, each already .NET-tupled: `M: a * b -> r` holds
        /// `[a * b]` and the curried `M: a -> b -> r` holds `[a; b]`. EMPTY for a value member,
        /// whose type is `Return` with no `->` in front of it.
        ArgGroups: EqArray<FrozenType>
        Return: FrozenType
        /// Per-method-typar UPPER BOUND (`<Key extends keyof Events>`): index `j` is the
        /// `j`-th method typar's bound, baked over the DECLARING typars (`keyof Events` at
        /// `Emitter<R>` → `TyKeyOf R`). Length `MethodTyparArity`, or EMPTY when none.
        MethodTyparBounds: EqArray<FrozenType voption>
    }

    /// The sentinel a contract-layer member carries until the finalize pass fills its groups /
    /// `Return` from the stashed signature CST. `argGroupCount` is already known there, and
    /// keeping it exact is what makes a deferred value member read as one.
    static member deferred(declaringTyparArity: int, methodTyparArity: int, argGroupCount: int) : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            ArgGroups = EqArray.ofList (List.replicate argGroupCount deferredTemplate)
            Return = deferredTemplate
            MethodTyparBounds = EqArray.empty
        }

    /// The .NET norm: ONE argument group, taking the tupled `parameters` whole, and no method
    /// bounds. Every reflection, manifest and tupled-source producer mints this shape; a
    /// bound-carrying producer builds the record explicitly instead.
    static member make
        (declaringTyparArity: int, methodTyparArity: int, parameters: FrozenType, return': FrozenType)
        : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            ArgGroups = EqArray.singleton parameters
            Return = return'
            MethodTyparBounds = EqArray.empty
        }

    /// A FIELD or PROPERTY, whose type is `return'` with no `->` in front of it. Distinct from
    /// the `unit -> r` METHOD `make` mints for a `member M: unit -> r`, which a use site must
    /// still apply.
    static member value(declaringTyparArity: int, methodTyparArity: int, return': FrozenType) : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            ArgGroups = EqArray.empty
            Return = return'
            MethodTyparBounds = EqArray.empty
        }

    /// One group per `->` the source wrote, each holding that group's own tupled domain.
    static member ofGroups
        (declaringTyparArity: int, methodTyparArity: int, argGroups: FrozenType list, return': FrozenType)
        : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            ArgGroups = EqArray.ofList argGroups
            Return = return'
            MethodTyparBounds = EqArray.empty
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExternalSignature =

    let unitFrozen: FrozenType = FTConst(RuntimeNames.unitKey, EqArray.empty)

    /// The body position an extractor could not translate. `what` must identify the construct,
    /// because it is all the use site is told: the contract is read back by a compilation with
    /// neither this declaration nor this extractor's error in hand.
    let unfreezable (what: string) : FrozenType =
        FTUnknown(UnknownReason.UnfreezableExternal what)

    /// `[a; b]` ⟶ `a * b`, `[]` ⟶ `unit`: one argument group's .NET-tupled domain.
    let tupledParams (ps: EqArray<FrozenType>) : FrozenType =
        match ps.Length with
        | 0 -> unitFrozen
        | 1 -> ps.[0]
        | _ -> FTTuple ps

    /// `a * b` ⟶ `[a; b]`, `unit` ⟶ `[]`: one group's domain back to one entry per source
    /// parameter.
    let argSigOfParameters (parameters: FrozenType) : EqArray<FrozenType> =
        match parameters with
        | FTUnit -> EqArray.empty
        | FTTuple items -> items
        | single -> EqArray.singleton single

    /// `a * b` ⟶ `2`, `unit` ⟶ `0`: the same count, without building the flattening.
    let groupWidth (parameters: FrozenType) : int =
        match parameters with
        | FTUnit -> 0
        | FTTuple items -> items.Length
        | _ -> 1

    /// The groups folded back onto `Return`, so `([a; b], r)` ⟶ `a -> b -> r` — the type a use
    /// site writes. A value member has no group and is its bare `Return`.
    let openTemplate (s: ExternalSignature) : FrozenType =
        let mutable t = s.Return

        for i in s.ArgGroups.Length - 1 .. -1 .. 0 do
            t <- FTFun(s.ArgGroups.[i], t)

        t

    /// `M: a * b -> r` ⟶ `[2]`, the curried `M: a -> b -> r` ⟶ `[1; 1]`: how many arguments
    /// each application consumes.
    let argGroupWidths (s: ExternalSignature) : EqArray<int> = s.ArgGroups |> EqArray.map groupWidth

    /// Every group flattened in source order, so `a -> b * c -> r` ⟶ `[a; b; c]`: the member's
    /// .NET parameter vector, and the `ArgSig` its key interns.
    let argSigOf (s: ExternalSignature) : EqArray<FrozenType> =
        match s.ArgGroups.Length with
        | 1 -> argSigOfParameters s.ArgGroups.[0]
        | _ ->
            let flat = ResizeArray<FrozenType>(s.ArgGroups.Length)

            for g in s.ArgGroups do
                flat.AddRange(argSigOfParameters g)

            EqArray.ofResizeArray flat

    /// The ONE .NET-tupled slot the groups compile to: `a -> b -> r` and `a * b -> r` both
    /// occupy `a * b`.
    let tupledParameters (s: ExternalSignature) : FrozenType = tupledParams (argSigOf s)

    /// A setter is dispatched from `x.[i] <- v`, never applied a group at a time, so its
    /// groups collapse to ONE .NET parameter vector with the getter's result appended:
    /// `Item: int -> 'T with set` takes `(int, 'T)`.
    let setter
        (declaringTyparArity: int)
        (methodTyparArity: int)
        (groupDomains: FrozenType list)
        (getterReturn: FrozenType)
        : ExternalSignature =
        let ps = ResizeArray<FrozenType>()

        for domain in groupDomains do
            ps.AddRange(argSigOfParameters domain)

        ps.Add getterReturn

        ExternalSignature.make (
            declaringTyparArity,
            methodTyparArity,
            tupledParams (EqArray.ofResizeArray ps),
            unitFrozen
        )

/// A resolved member (method, field or property) on an external type.
type ExternalMember =
    {
        Name: string
        IsStatic: bool
        /// `Field` / `Property` (value members) vs `Method`. `Field` vs `Property` matters
        /// only at CLR emission (`ldfld` vs `call get_X`).
        Storage: MemberStorage
        Signature: ExternalSignature
        /// The member's OWN generic parameter count (`Take<TSource>` ⇒ 1); `0` for every
        /// property and constructor.
        MethodTyparArity: int
        Origin: SymbolOrigin
        /// Interned identity: the OPEN declaring type (its `argSig` in `!0`-typars) + name
        /// + kind.
        Key: MemberKey
        /// The constant defaults of this member's TRAILING optional parameters
        /// (`Return(array, [<Optional>] clearArray = false)` ⇒ `[Bool false]`), in
        /// declaration order; a call may omit any suffix of them.
        OptionalDefaults: TConstValue list
        /// An OPTIONAL interface member (`verbose?: T`), so a structural-width admission at a
        /// foreign-call arg position treats it as not-required. `false` from every
        /// non-interface producer.
        IsOptional: bool
        /// The member's splice TEMPLATE: a `member inline`, lifted `this`-first so it
        /// splices through the same path a `let inline` value does.
        InlineBody: InlineBody voption
    }

    /// The zero every member literal is copied from; `Name` is derived from `key` and
    /// `Signature` is the `deferred` sentinel until filled.
    static member OfKey(key: MemberKey) : ExternalMember =
        {
            Name = key.Name
            IsStatic = false
            Storage = MemberStorage.Method
            Signature = ExternalSignature.deferred (0, 0, 1)
            MethodTyparArity = 0
            Origin = SymbolOrigin.Empty
            Key = key
            OptionalDefaults = []
            IsOptional = false
            InlineBody = ValueNone
        }

    /// A value member (field or property): no parameters, the value in `Return`.
    member m.IsValueMember = m.Storage.IsValueMember

    /// The canonical `.ctor` shape, keyed as a `MemberKind.Method` over `declKey`.
    static member ctor
        (declKey: TypeKey)
        (signature: ExternalSignature)
        (argSig: EqArray<FrozenType>)
        (origin: SymbolOrigin)
        (optionalDefaults: TConstValue list)
        : ExternalMember =
        { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey ".ctor" argSig 0 MemberKind.Method) with
            Signature = signature
            Origin = origin
            OptionalDefaults = optionalDefaults
        }

/// HOW an external type's instance-member CALLS lower on the JS backend.
type MemberLowering =
    /// Members live ON the object as prototype/own methods: native
    /// `x.member(args)` calls. F#/Fable's `[<AttachMembers>]`.
    | AttachedNative
    /// Members compile to FREE FUNCTIONS named `<Type>__<member>`, taking the object
    /// argument first, for tree-shaking. The DEFAULT.
    | TypePrefixed
    /// A SYNTHETIC grouping type with no runtime existence: bare module-level exports
    /// collected under one type, so `Util.format(x)` erases at JS emit to `format(x)`.
    | ErasedBare

type ExternalClassFlags =
    {
        Declared: DeclaredClassFlags
        /// `true` for a .NET value type (`struct`), so codegen emits a value-type `this`
        /// pointer (`ldloca` + `constrained.`) rather than reference `callvirt`.
        IsValueType: bool
        MemberLowering: MemberLowering
        /// A GLOBAL (ambient) type the JS runtime provides (`Map`, `Set`, `Promise`), so
        /// reachable by its BARE name with NO `import`. Rides the type's HOME, not the type.
        Global: bool
        /// The import-STATEMENT shape for this type's home-module exports.
        ImportForm: ImportForm
    }

    /// The conservative default when a `.fsi` commits only name + arity + interface-ness.
    static member Default =
        {
            Declared = DeclaredClassFlags.Default
            IsValueType = false
            MemberLowering = MemberLowering.TypePrefixed
            Global = false
            ImportForm = ImportForm.Named
        }

/// The shape of an external class or interface; every `Frozen…` template and each member's
/// `Signature` is written over the DECLARING type's typars.
type ExternalClassShape =
    {
        TyparArity: int
        IsInterface: bool
        /// All public declared methods + properties whose signature maps; one whose
        /// parameter or return type does not (a pointer) is dropped, not faked.
        Members: EqArray<ExternalMember>
        FrozenInterfaces: EqArray<FrozenNominal>
        /// The declared base type; `ValueNone` for an interface and for `System.Object`.
        FrozenBaseType: FrozenNominal voption
        Flags: ExternalClassFlags
        Origin: SymbolOrigin
    }

    /// A minimally-populated shape: name + arity + interface-ness only.
    static member basic(arity: int, isInterface: bool, origin: SymbolOrigin) : ExternalClassShape =
        {
            TyparArity = arity
            IsInterface = isInterface
            Members = EqArray.empty
            FrozenInterfaces = EqArray.empty
            FrozenBaseType = ValueNone
            Flags = ExternalClassFlags.Default
            Origin = origin
        }

/// What the compiling target does about an intrinsic: binds a representation, or identifies
/// itself as the target that binds none, because a use-site diagnostic must say WHICH.
[<RequireQualifiedAccess>]
type IntrinsicPlatform =
    /// The target's own name for the type, from the `<base>.<target>.fs` companion's
    /// `type x = (# "<repr>" #)`: `"System.Int32"` on CLR, `"number"`/`"Error"` on JS.
    | Repr of platform: string
    /// The target binds no representation, so referring to the type is an error there
    /// (`nativeint` / `nativeptr` / `voidptr` on JS).
    | Unsupported of target: string

/// The platform-invariant identity axis a scalar / heritable intrinsic carries.
type IntrinsicIdentity =
    {
        /// The qualified `.fsi` name the type was declared under (`Vesper.int`), the same
        /// whichever backend compiles. Never a BCL name; `int` ≠ `float` here.
        Canon: TypeKey
        /// Usually `0`, but the structural type constructors are intrinsics too
        /// (`type 'T [] = (# "!0[]" #)` has arity 1; `byref`, nd-array).
        TyparArity: int
        /// The per-target repr, or the target that binds none. Many-to-one, so it must never
        /// drive unification.
        Platform: IntrinsicPlatform
    }

/// The declared SUPERTYPE surface a primitive carries: what a subtype walk off it can
/// reach. Instance members (`exn.Message`) are NOT here, because they route through the
/// platform type.
type IntrinsicClassSurface =
    {
        /// `extern class` (`obj`/`exn`), whose `.fs` binds `(# class "…" #)`: a downstream
        /// file may `inherit` it. A scalar that merely declares an `interface` (`int` is an
        /// `equatable<int>`) carries a surface too, and is NOT heritable.
        Heritable: bool
        /// The declared `inherit` parent (`exn`'s is `obj`; `obj`'s is `ValueNone`).
        BaseType: FrozenNominal voption
        /// The declared `interface <ty>` impls (`'T[]`'s is `seq<'T>`), written over the
        /// declaring typars. The target supplies each one; no Vesper code implements them.
        Interfaces: EqArray<FrozenNominal>
        /// The contract `.ctor`s (`new: string -> exn`): the constructible surface both
        /// `new exn "…"` and `inherit exn(…)` check against.
        Members: EqArray<ExternalMember>
    }

/// An `extern` type whose sibling `.fs` carries `type x = (# "<repr>" #)`. NON-transparent
/// (unlike `Abbrev`): a use site resolves to `TyConst Id.Canon`, never the expanded repr.
type IntrinsicShape =
    {
        Id: IntrinsicIdentity
        /// Absent for a bare scalar; present once the primitive is heritable or declares a
        /// base, an interface or a `.ctor`. The value identity stays `TyConst` either way.
        Class: IntrinsicClassSurface voption
    }

    static member Scalar(canon: TypeKey, arity: int, platform: IntrinsicPlatform) : IntrinsicShape =
        {
            Id =
                {
                    Canon = canon
                    TyparArity = arity
                    Platform = platform
                }
            Class = ValueNone
        }

    /// `type Attribute = extern class`: heritable, and declaring nothing else. `inherit
    /// Attribute()` in a later file has only this surface to read the heritability off.
    static member HeritableClass(canon: TypeKey, arity: int, platform: IntrinsicPlatform) : IntrinsicShape =
        {
            Id =
                {
                    Canon = canon
                    TyparArity = arity
                    Platform = platform
                }
            Class =
                ValueSome
                    {
                        Heritable = true
                        BaseType = ValueNone
                        Interfaces = EqArray.empty
                        Members = EqArray.empty
                    }
        }

module IntrinsicClassSurface =

    /// Fold a farther source's surface onto a nearer one: an implementation file's view binds a
    /// representation without publishing a surface, so the nearest source alone would answer
    /// "declares nothing" ahead of the contract that prescribes it.
    let merge
        (nearer: IntrinsicClassSurface voption)
        (farther: IntrinsicClassSurface voption)
        : IntrinsicClassSurface voption =
        match nearer, farther with
        | ValueNone, s
        | s, ValueNone -> s
        | ValueSome a, ValueSome b ->
            ValueSome
                {
                    Heritable = a.Heritable || b.Heritable
                    BaseType =
                        match a.BaseType with
                        | ValueNone -> b.BaseType
                        | it -> it
                    Interfaces = EqArray.append a.Interfaces b.Interfaces |> EqArray.distinct
                    // `.ctor`s are authored in ONE place, so a non-empty set is the whole set.
                    Members = if a.Members.IsEmpty then b.Members else a.Members
                }

/// A CAPABILITY INTERFACE (`disposable`/`equatable`/`comparable`): an intrinsic whose
/// identity is a `TyClass` CONSTRAINT, not a `TyConst` value identity.
type IntrinsicInterfaceShape =
    {
        /// The platform-INVARIANT `.fsi` identity (`Vesper.disposable`): the
        /// capability-matching key, NOT the value-resolution key.
        Canon: TypeKey
        TyparArity: int
        /// The `.fs` `(# … #)` repr: `"System.IDisposable"` on the CLR, the sentinel
        /// `"!Vesper.disposable"` on a target with no interfaces. A bare `string`, not an
        /// `IntrinsicPlatform`: a capability is minted only where its `.fs` binds the repr.
        Platform: string
        /// The abstract member surface (`Dispose`).
        Members: EqArray<ExternalMember>
        /// The directly-inherited interfaces: `enumerator` inherits `disposable`. Empty for
        /// a capability that inherits none.
        Interfaces: EqArray<FrozenNominal>
        Origin: SymbolOrigin
    }
