namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

/// Why a registered type name carries no modelled body.
[<RequireQualifiedAccess>]
type UnmodelledReason =
    | Delegate
    | TypeExtension
    /// The kind IS modelled; this declaration's body did not translate.
    | ExtractionFailed of reason: string

    /// The phrase a diagnostic or fault names this by.
    member this.Description: string =
        match this with
        | Delegate -> "a delegate type"
        | TypeExtension -> "a type extension"
        | ExtractionFailed reason -> sprintf "a body that did not translate (%s)" reason

exception BodylessExternalShape of compiledName: string * reason: UnmodelledReason with
    override this.Message =
        sprintf
            "mkNominal: '%s' has no modelled body — %s. Model its kind before a contract names it"
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
    | MemberTrait of typarIndices: EqArray<int> * memberName: string * argTypes: FrozenType[] * returnType: FrozenType
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

/// A published inline body as the provider serves it: the producing file's own template,
/// handed across the boundary VERBATIM for the consumer to thaw.
type InlineBody =
    {
        Decl: Wire.TDecl
        ParamAttrs: ParamAttrs[]
        /// The producer file's anchors: its text and token table. `Decl`'s nodes carry
        /// token INDICES into that file, unreadable without it.
        Origin: OriginSource
    }

[<RequireQualifiedAccess>]
module InlineBody =

    let anchoredIn (origin: OriginSource) (decl: Wire.TDecl) (paramAttrs: ParamAttrs[]) : InlineBody =
        {
            Decl = decl
            ParamAttrs = paramAttrs
            Origin = origin
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
        /// ("is this `Vesper.Printf.printfn`?") instead of suffix-matching the written name.
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
        FieldNames: string voption[]
        /// The field types with the enclosing type's typars baked as `FTTypar(Declaring,i)`.
        FrozenFieldTypes: FrozenType[]
    }

    /// A case whose field templates are deferred until the registry is complete.
    static member create(name: string, fieldNames: string voption[]) : ExternalCaseShape =
        {
            Name = name
            FieldNames = fieldNames
            FrozenFieldTypes = fieldNames |> Array.map (fun _ -> deferredTemplate)
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
        FieldNames: string[]
        /// `[<RequireQualifiedAccess>]`: a consumer excludes such a record from bare
        /// field-set resolution: `{ X = … }` must be written `{ R.X = … }`.
        IsRequireQualifiedAccess: bool
    }

/// A member's tupled `(Parameters, Return)` as `FrozenType` templates: `Parameters` is the
/// .NET-tupled argument type (`N ≥ 2` → one `FTTuple`; 0 params → `unit`). Open typars are
/// baked as `FTTypar(Declaring,i)` (the declaring type's) / `FTTypar(Method,j)` (its own).
type ExternalSignature =
    {
        DeclaringTyparArity: int
        MethodTyparArity: int
        Parameters: FrozenType
        Return: FrozenType
        /// Per-method-typar UPPER BOUND (`<Key extends keyof Events>`): index `j` is the
        /// `j`-th method typar's bound, baked over the DECLARING typars (`keyof Events` at
        /// `Emitter<R>` → `TyKeyOf R`). Length `MethodTyparArity`, or EMPTY when none.
        MethodTyparBounds: FrozenType voption[]
    }

    /// The sentinel a contract-layer member carries until the finalize pass fills
    /// `Parameters` / `Return` from its stashed signature CST.
    static member deferred(declaringTyparArity: int, methodTyparArity: int) : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            Parameters = deferredTemplate
            Return = deferredTemplate
            MethodTyparBounds = [||]
        }

    /// Known `Parameters` / `Return` with no method bounds; a bound-carrying producer
    /// builds the record explicitly instead.
    static member make
        (declaringTyparArity: int, methodTyparArity: int, parameters: FrozenType, return': FrozenType)
        : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            Parameters = parameters
            Return = return'
            MethodTyparBounds = [||]
        }

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
            Signature = ExternalSignature.deferred (0, 0)
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
        IsSealed: bool
        IsAbstract: bool
        AllowNullLiteral: bool
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
            IsSealed = false
            IsAbstract = false
            AllowNullLiteral = false
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
        Members: ExternalMember[]
        FrozenInterfaces: FrozenType[]
        /// The declared base type; `ValueNone` for an interface and for `System.Object`.
        FrozenBaseType: FrozenType voption
        Flags: ExternalClassFlags
        Origin: SymbolOrigin
    }

    /// A minimally-populated shape: name + arity + interface-ness only.
    static member basic(arity: int, isInterface: bool, origin: SymbolOrigin) : ExternalClassShape =
        {
            TyparArity = arity
            IsInterface = isInterface
            Members = [||]
            FrozenInterfaces = [||]
            FrozenBaseType = ValueNone
            Flags = ExternalClassFlags.Default
            Origin = origin
        }

/// What the compiling target does about an intrinsic: binds a representation, or names
/// itself as the target that binds none, because a use-site diagnostic must say WHICH.
[<RequireQualifiedAccess>]
type IntrinsicPlatform =
    /// The target's own name for the type, from the `<base>.<target>.fs` companion's
    /// `type x = (# "<repr>" #)`: `"System.Int32"` on CLR, `"number"`/`"Error"` on JS.
    | Repr of platform: string
    /// The target binds no representation, so naming the type is an error there
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

/// The class surface a HERITABLE primitive (`obj`/`exn`) carries and a scalar lacks.
/// Instance members (`exn.Message`) are NOT here, because they route through the platform type.
type IntrinsicClassSurface =
    {
        /// The declared `inherit` parent (`exn`'s is `obj`; `obj`'s is `ValueNone`).
        BaseType: FrozenType voption
        /// The contract `.ctor`s (`new: string -> exn`): the constructible surface both
        /// `new exn "…"` and `inherit exn(…)` check against.
        Members: ExternalMember[]
    }

/// An `extern` type whose sibling `.fs` carries `type x = (# "<repr>" #)`. NON-transparent
/// (unlike `Abbrev`): a use site resolves to `TyConst Id.Canon`, never the expanded repr.
type IntrinsicShape =
    {
        Id: IntrinsicIdentity
        /// `ValueSome` ⇔ a heritable primitive (`obj`/`exn`): a downstream file can
        /// `inherit exn` / `new exn` while the value identity stays `TyConst`.
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

/// A CAPABILITY INTERFACE (`disposable`/`equatable`/`comparable`): an intrinsic whose
/// identity is a `TyClass` CONSTRAINT, not a `TyConst` value identity.
type IntrinsicInterfaceShape =
    {
        /// The platform-INVARIANT `.fsi` identity (`Vesper.disposable`): the
        /// capability-matching key, NOT the value-resolution key.
        Canon: TypeKey
        TyparArity: int
        /// The `.fs` `(# … #)` BCL repr (`"System.IDisposable"`). A bare `string`, not an
        /// `IntrinsicPlatform`: a capability is minted only where its `.fs` binds the repr.
        Platform: string
        /// The abstract member surface (`Dispose`).
        Members: ExternalMember[]
        /// The directly-inherited interfaces as nominal templates: `enumerator` inherits
        /// `disposable`. Empty for a leaf capability.
        Interfaces: FrozenType[]
        Origin: SymbolOrigin
    }

/// Type-declaration shape carried by a by-name or by-key type lookup.
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    /// `frozen` is the abbreviation body as a template, which a use site expands.
    | Abbrev of arity: int * frozen: FrozenType
    /// Field order matches source.
    | Record of arity: int * fields: ExternalFieldShape[] * origin: SymbolOrigin
    /// Case order matches source. `interfaces` are the union's directly-declared
    /// `interface <ty>` impls as nominal templates.
    | Union of arity: int * cases: ExternalCaseShape[] * interfaces: FrozenType[] * origin: SymbolOrigin
    /// An external enum: named constant cases in source order. No `arity`, because enums are
    /// never generic; the numeric / string / mixed variant is DERIVED from `cases`, never baked.
    | Enum of cases: ExternalEnumCaseShape[] * origin: SymbolOrigin
    | Class of shape: ExternalClassShape
    /// A referenced package's intrinsic-repr binding (`type exn = (# class
    /// "System.Exception" #)`): scalar (`int`) or heritable class (`obj`/`exn`).
    | Intrinsic of shape: IntrinsicShape
    /// A capability interface (`disposable`/`equatable`/`comparable`). CLR-only, because a JS
    /// capability is a plain canon-only interface `Class`.
    | IntrinsicInterface of shape: IntrinsicInterfaceShape
    /// NAME + ARITY are registered, the body is not modelled. `reason` says which gap, so a
    /// use site can name it rather than degrade silently.
    | Unmodelled of reason: UnmodelledReason * arity: int

    member this.TyparArity: int =
        match this with
        | Class info -> info.TyparArity
        | Intrinsic s -> s.Id.TyparArity
        | IntrinsicInterface s -> s.TyparArity
        | Enum _ -> 0 // enums are never generic
        | Record(arity = a)
        | Union(arity = a)
        | Abbrev(arity = a)
        | Unmodelled(arity = a) -> a

/// The RESOLVER view of the external-symbol contract: spelling → identity, opens-aware.
/// Downstream of name resolution, passes speak the key-addressed store view instead.
type IExternalSymbolResolver =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption
    /// Look up a `type` by canonical compiled name, returning its REGISTERED identity plus
    /// its body shape from the one hit.
    abstract TryLookupType: name: string -> struct (TypeKey * ExternalTypeShape) voption

    /// Reverse case-name lookup: a bare case name → its declaring union, so `Some 5` /
    /// `None` type without an annotation. First declaration wins on a name collision.
    abstract TryLookupUnionCase: caseName: string -> ExternalUnionCase voption

    /// A field name → every record declaring a field of that name; unqualified
    /// record-literal / record-pattern resolution intersects these sets to pin the type.
    abstract TryRecordsWithField: fieldName: string -> ExternalRecordCandidate[]

    /// The AMBIENT `[<AutoOpen>]` open prefixes this provider contributes, probed strictly
    /// BEHIND every explicit `open`; earliest wins: `["Vesper.ArithmeticOperators"; "Vesper"]`.
    abstract AmbientOpenPrefixes: string list

/// The STORE view of the external-symbol contract: identity → payload, once identity is
/// resolved. Addressed by `(namespace, arity-qualified name)` and NOTHING ELSE, so
/// same-named types in two assemblies are indistinguishable here.
type IExternalSymbolStore =
    /// A type declaration's body by resolved key; `ValueNone` for an unknown key or a body
    /// shape the provider doesn't model.
    abstract TryLookupType: key: SymbolKey -> ExternalTypeShape voption

    /// A static/instance member by the declaring type's key and the member name. When
    /// several overloads share a name this collapses to a single best-by-arity pick.
    abstract TryLookupMember: key: SymbolKey * memberName: string -> ExternalMember voption

    /// ALL overloads of a member by name: the candidate set the application-site overload
    /// resolver picks from. `[||]` from providers that don't model members.
    abstract TryLookupMembers: key: SymbolKey * memberName: string -> ExternalMember[]

    /// A member by resolved `MemberKey`: the channel a MEMBER splice site reaches an
    /// `InlineBody` through. BY KEY: a by-name lookup collapses an overload set to one pick
    /// and can hand back a different overload's entry.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption

    /// The TS index signature(s) `{ [k: K]: V }`: the seam `x.[k]` / `x.[k] <- v` goes
    /// through, `(key, value)` templates over the DECLARING typars. Both kinds may be present.
    abstract TryLookupIndexSignature: key: SymbolKey -> (FrozenType * FrozenType) list

    /// A value/free-function symbol by resolved key: the channel a VALUE splice site
    /// reaches an `InlineBody` through.
    abstract TryLookupByKey: key: SymbolKey -> ExternalSymbol voption

    /// `{ platform-repr -> [canon] }`: an incoming BCL/native runtime name
    /// (`"System.Exception"`) back to the `.fsi` identity (`"exn"`). ONE-TO-MANY, because on
    /// JS `int`, `float` and `float32` all repr to `"number"`.
    abstract IntrinsicReverseCanon: Map<string, SymbolKey list>

    /// `{ canon -> platform-repr }`: the `.fsi` short name to its `.fs` `(# … #)` repr for
    /// the compiling target. Empty from providers carrying no intrinsics.
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

/// Both views on ONE object: raw facts only, with NO capability predicates ("is this type
/// disposable?" is a language judgment the passes make). Every lookup must be thread-safe.
type IExternalSymbolProvider =
    inherit IExternalSymbolResolver
    inherit IExternalSymbolStore

/// An external module-level function as the codegen boundary sees it: the curried
/// `param -> … -> return` template with the function's own typars baked as
/// `FTTypar(Method, i)`.
type CodegenOpenSignature =
    {
        Origin: SymbolOrigin
        Signature: FrozenType
        MethodTyparArity: int
        /// The producer's SOURCE parameter grouping: the curried `Signature` alone can't
        /// tell a group `f (x,y)` from a tuple param `f (t:int*int)`.
        ValRepr: TastAccessor.ValRepr voption
        /// The symbol's `when 'a :> <ty>` bounds over the method-typar axis, letting a
        /// phantom slot be recovered from the constrained source's interface witness.
        Constraints: FrozenConstraint list
    }

/// The CODEGEN-facing view: only what emission needs to mint references, never the
/// inference surface. Every channel is key-addressed and NONE returns an overload set.
type ICodegenSymbols =
    abstract TryLookupType: key: SymbolKey -> ExternalTypeShape voption
    /// The exact member the front end resolved, by the `MemberKey` it stamped, with no
    /// overload re-pick.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption
    /// Select the `.ctor` a `new` emits: by the exact `MemberKey` the front end recorded
    /// when it has one, telling `ArgumentException(string, string)` from
    /// `(string, Exception)`; else the sole ctor of `arity` params.
    abstract TryLookupCtor: declKey: SymbolKey * chosen: SymbolKey voption * arity: int -> ExternalMember voption
    /// Rebase a capability member onto its true base declarer: `enumerator.MoveNext` is
    /// declared on the non-generic `IEnumerator`, and a member-ref parented on the derived
    /// interface would `MissingMethodException`.
    abstract TryRebaseCapabilityMember: key: SymbolKey -> SymbolKey voption
    /// The open `FrozenType` signature of a module-level function by its value key.
    abstract TryLookupOpenSignature: key: SymbolKey -> CodegenOpenSignature voption
    /// `{ canon -> platform-repr }`: a primitive canon name (`"int"`) to its `.fs`-declared
    /// repr (`"System.Int32"`).
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

module ExternalSymbols =

    /// `SymbolKey` is equatable-but-not-comparable, so the axis is a `Dictionary`, not a `Map`.
    let emptyForwardRepr: IReadOnlyDictionary<SymbolKey, string> =
        Dictionary<SymbolKey, string>() :> IReadOnlyDictionary<_, _>

    /// Invert a NAME-INDEXED leaf's qualified name back to a key. Sound only when the
    /// leaf's type keys are `InNamespace`. Arity comes from the SHAPE, never from the arity
    /// probed for.
    let nameKeyedTypeHit (name: string) (shape: ExternalTypeShape) : struct (TypeKey * ExternalTypeShape) =
        struct (SymbolKeyOps.qualifiedTypeKeyOf name shape.TyparArity, shape)

    let typeShapeOf (hit: struct (TypeKey * ExternalTypeShape) voption) : ExternalTypeShape voption =
        hit |> ValueOption.map (fun (struct (_, shape)) -> shape)

    /// The one entry of a by-NAME overload set whose identity is `key`.
    let memberByKey (key: MemberKey) (candidates: ExternalMember[]) : ExternalMember voption =
        match candidates |> Array.tryFind (fun m -> m.Key = key) with
        | Some m -> ValueSome m
        | None -> ValueNone

    /// The member surface an external nominal publishes: a `Class` or a capability
    /// `IntrinsicInterface`.
    [<return: Struct>]
    let (|ExternalMembers|_|) (shape: ExternalTypeShape) : ExternalMember[] voption =
        match shape with
        | ExternalTypeShape.Class shape -> ValueSome shape.Members
        | ExternalTypeShape.IntrinsicInterface shape -> ValueSome shape.Members
        | _ -> ValueNone

    /// The member surface of an external INTERFACE specifically. A non-interface `Class` is
    /// excluded, because a record cannot widen to a concrete class.
    [<return: Struct>]
    let (|ExternalInterfaceMembers|_|) (shape: ExternalTypeShape) : ExternalMember[] voption =
        match shape with
        | ExternalTypeShape.Class {
                                      IsInterface = true
                                      Members = members
                                  } -> ValueSome members
        | ExternalTypeShape.IntrinsicInterface shape -> ValueSome shape.Members
        | _ -> ValueNone

    /// A capability is an interface on BOTH targets, and only the carried shape differs.
    let isInterfaceShape (shape: ExternalTypeShape) : bool =
        match shape with
        | ExternalTypeShape.Class s -> s.IsInterface
        | ExternalTypeShape.IntrinsicInterface _ -> true
        | _ -> false

    /// An already-RESOLVED intrinsic canon key → its heritable-primitive surface
    /// (`obj`/`exn`). A DIRECT lookup: a short-name re-scan could hit a different entry of a
    /// composited provider than the one that minted the key.
    let tryIntrinsicClass
        (provider: IExternalSymbolStore)
        (canon: SymbolKey)
        : struct (IntrinsicIdentity * IntrinsicClassSurface) voption =
        match provider.TryLookupType canon with
        | ValueSome(ExternalTypeShape.Intrinsic { Id = id; Class = ValueSome surface }) ->
            ValueSome(struct (id, surface))
        | _ -> ValueNone

    /// Resolve a `(# "…" #)` REPR STRING to the first shape `choose` ACCEPTS; a rejected
    /// shape does not stop the scan. Never a source-written name, because this would miss
    /// the `open`s it was written under.
    let tryPickRuntimeType
        (provider: IExternalSymbolResolver)
        (choose: ExternalTypeShape -> 'a voption)
        (repr: string)
        : 'a voption =
        match provider.TryLookupType repr |> typeShapeOf |> ValueOption.bind choose with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            provider.AmbientOpenPrefixes
            |> List.tryPick (fun p ->
                match provider.TryLookupType(p + "." + repr) |> typeShapeOf |> ValueOption.bind choose with
                | ValueSome v -> Some v
                | ValueNone -> None
            )
            |> function
                | Some v -> ValueSome v
                | None -> ValueNone

    let tryRuntimeType (provider: IExternalSymbolResolver) (repr: string) : ExternalTypeShape voption =
        tryPickRuntimeType provider ValueSome repr

    /// Resolve the language-capability identities from their canonical contract names
    /// (`Vesper.disposable`). Keys are minted at arity 0, because the fqn already carries the
    /// backtick-arity suffix.
    let resolveCapabilities (provider: IExternalSymbolProvider) : RuntimeNames.CapabilityIds =
        let ofKey (key: TypeKey) : RuntimeNames.CapabilityIdentity =
            {
                RuntimeNames.CapabilityIdentity.Key = key
                RuntimeNames.CapabilityIdentity.CanonKey = ValueNone
            }

        // On CLR a capability anchor is an `IntrinsicInterface` carrying both names; on JS a
        // plain `Class` with only the canonical, so `bclName` is supplied and confirmed here.
        let shimConfirms (bcl: string) (lookup: string) : bool =
            match provider.TryLookupType bcl |> typeShapeOf with
            | ValueSome(ExternalTypeShape.Abbrev(_, FTClass(tyCtor, _))) -> SymbolKeyOps.typeMetaName tyCtor = lookup
            | _ -> false

        let resolveAnchorKey (canon: TypeKey) (bcl: TypeKey voption) : RuntimeNames.CapabilityIdentity voption =
            let lookup = SymbolKeyOps.typeMetaName canon
            let bclName = bcl |> ValueOption.map SymbolKeyOps.typeMetaName

            match provider.TryLookupType lookup |> typeShapeOf with
            | ValueSome(ExternalTypeShape.Intrinsic {
                                                        Id = {
                                                                 Platform = IntrinsicPlatform.Repr fqn
                                                             }
                                                    }) -> ValueSome(ofKey (SymbolKeyOps.qualifiedTypeKeyOf fqn 0))
            | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                ValueSome
                    {
                        RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf platform 0
                        RuntimeNames.CapabilityIdentity.CanonKey = ValueSome canon
                    }
            | ValueSome(ExternalTypeShape.Class _) ->
                let canonKey = canon

                match bclName with
                | ValueSome bcl when shimConfirms bcl lookup ->
                    // Pair the canon-only JS anchor with its BCL name, so either spelling
                    // folds to the canon.
                    ValueSome
                        {
                            RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf bcl 0
                            RuntimeNames.CapabilityIdentity.CanonKey = ValueSome canonKey
                        }
                | _ -> ValueSome(ofKey canonKey)
            | _ -> ValueNone

        {
            // The iteration capabilities carry a BCL reconciliation name so a TS pack's
            // `IEnumerable`1` reconciles to `seq` on JS (a no-op on CLR).
            Enumerable = resolveAnchorKey RuntimeNames.seqKey (ValueSome RuntimeNames.bclEnumerableKey)
            Enumerator = resolveAnchorKey RuntimeNames.enumeratorKey (ValueSome RuntimeNames.bclEnumeratorKey)
            // The leaf capabilities fold BCL spellings to the canonical at freeze time, so
            // they need no reconciliation name here.
            Disposable = resolveAnchorKey RuntimeNames.disposableKey ValueNone
            Equatable = resolveAnchorKey RuntimeNames.equatableKey ValueNone
            Comparable = resolveAnchorKey RuntimeNames.comparableKey ValueNone
        }

    /// Realise a member's `Signature` with SOME method typars PRE-BOUND (`seed`, index →
    /// type) instead of fresh; the rest freshen normally. A typar in no bare parameter
    /// position is unsolvable by unification alone.
    let instantiateSignatureWith
        (store: TypeStore)
        (seed: (int * SemType) list)
        (m: ExternalMember)
        (declaringArgs: SemType[])
        (level: int)
        : SemType =
        let cache = System.Collections.Generic.Dictionary<int, SemType>()

        for (j, ty) in seed do
            cache.[j] <- ty

        let methodVar = methodFreshener store cache level
        let decl i = declaringArgs.[i]
        let noLocal = localTyparInTemplate "ExternalSymbols.instantiateSignatureWith"
        let s = m.Signature

        if m.IsValueMember then
            instantiateWith decl methodVar noLocal s.Return
        else
            TyFun(instantiateWith decl methodVar noLocal s.Parameters, instantiateWith decl methodVar noLocal s.Return)

    /// Realise a member's `Signature` at `level`: `FTTypar(Declaring,i) →
    /// declaringArgs.[i]`, `FTTypar(Method,j) → fresh TyVar at level` (one per index, shared
    /// across `Parameters` and `Return`). `TyFun(params, ret)`, or the bare value type.
    let instantiateSignature (store: TypeStore) (m: ExternalMember) (declaringArgs: SemType[]) (level: int) : SemType =
        instantiateSignatureWith store [] m declaringArgs level

    /// The OPEN realisation of a member's `Signature`: declaring typars substituted from
    /// `declaringArgs`, the member's own method typars left as `TyTypar(Method,j)` markers.
    /// The applicability-filtering form, in which a generic method's marker stays a wildcard.
    let openSignature (m: ExternalMember) (declaringArgs: SemType[]) : SemType =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let noLocal = localTyparInTemplate "ExternalSymbols.openSignature"
        let s = m.Signature

        if m.IsValueMember then
            instantiateWith decl methodOpen noLocal s.Return
        else
            TyFun(
                instantiateWith decl methodOpen noLocal s.Parameters,
                instantiateWith decl methodOpen noLocal s.Return
            )

    /// A member's method-typar BOUNDS at a use site, one per index.
    /// `FTTypar(Declaring,i)` → `declaringArgs.[i]`; a `FTTypar(Method,j)` ref stays an
    /// inert marker. Empty when uncarried.
    let instantiateSignatureBounds (m: ExternalMember) (declaringArgs: SemType[]) : SemType voption[] =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let noLocal = localTyparInTemplate "ExternalSymbols.instantiateSignatureBounds"

        m.Signature.MethodTyparBounds
        |> Array.map (ValueOption.map (fun ft -> instantiateWith decl methodOpen noLocal ft))

    let instantiateFieldType (f: ExternalFieldShape) (declaringArgs: SemType[]) : SemType =
        instantiateDeclaring f.Frozen declaringArgs

    let instantiateCaseFieldTypes (c: ExternalCaseShape) (declaringArgs: SemType[]) : SemType[] =
        c.FrozenFieldTypes
        |> Array.map (fun ft -> instantiateDeclaring ft declaringArgs)

    /// Realise a class's `FrozenInterfaces`, or a union's declared `interface <ty>` impls,
    /// at a use site.
    let instantiateInterfacesOf (interfaces: FrozenType[]) (declaringArgs: SemType[]) : SemType[] =
        interfaces |> Array.map (fun ft -> instantiateDeclaring ft declaringArgs)

    let instantiateInterfaces (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType[] =
        instantiateInterfacesOf shape.FrozenInterfaces declaringArgs

    /// The identity + type args a realised interface denotes. A non-nominal instantiation
    /// denotes none, so it matches no capability.
    let interfaceNominal (ty: SemType) : struct (SymbolKey * EqArray<SemType>) voption =
        match ty with
        | TyClass(k, args)
        | TyUnion(k, args)
        | TyRecord(k, args) -> ValueSome(struct (SymbolKey.Type k, args))
        | TyConst(k, args) -> ValueSome(struct (k, args))
        | _ -> ValueNone

    /// The identity an interface TEMPLATE names, before instantiation.
    let frozenInterfaceKey (ft: FrozenType) : SymbolKey voption =
        match ft with
        | FTClass(k, _)
        | FTUnion(k, _)
        | FTRecord(k, _) -> ValueSome(SymbolKey.Type k)
        | FTConst(k, _) -> ValueSome k
        | _ -> ValueNone

    /// The type args of the first realised interface whose identity is `cap`, so `.IsSome`
    /// is the "does this set carry the capability" test.
    let tryCapabilityArgs
        (cap: RuntimeNames.CapabilityIdentity voption)
        (interfaces: SemType[])
        : EqArray<SemType> voption =
        match cap with
        | ValueNone -> ValueNone
        | ValueSome c ->
            let mutable found = ValueNone
            let mutable i = 0

            while found.IsNone && i < interfaces.Length do
                match interfaceNominal interfaces.[i] with
                | ValueSome(struct (k, args)) when c.Matches k -> found <- ValueSome args
                | _ -> ()

                i <- i + 1

            found

    /// Shared by a class shape and a heritable primitive's class surface.
    let instantiateBaseTypeFrozen (baseType: FrozenType voption) (declaringArgs: SemType[]) : SemType voption =
        baseType |> ValueOption.map (fun ft -> instantiateDeclaring ft declaringArgs)

    let instantiateBaseType (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType voption =
        instantiateBaseTypeFrozen shape.FrozenBaseType declaringArgs

    /// Flatten a frozen signature's .NET-tupled `Parameters` back to one entry per source
    /// parameter.
    let argSigOfParameters (parameters: FrozenType) : EqArray<FrozenType> =
        match parameters with
        | FTUnit -> EqArray.empty
        | FTTuple items -> items
        | single -> EqArray.singleton single

    let unitFrozen: FrozenType = FTConst(RuntimeNames.unitKey, EqArray.empty)

    /// Fold per-parameter frozen types into the single .NET-tupled `Parameters` form an
    /// `ExternalSignature` carries.
    let tupledParams (ps: FrozenType[]) : FrozenType =
        match ps.Length with
        | 0 -> unitFrozen
        | 1 -> ps.[0]
        | _ -> FTTuple(EqArray.ofArray ps)

    let unfreezable = FTUnknown "<unfreezable external template>"

    /// Realise a value/free-function symbol's `Scheme` at `level`: a fresh `TyVar` per
    /// declaring typar, the `Constraints` stamped onto them, then the scheme realised
    /// against that array.
    let instantiateSymbol (store: TypeStore) (sym: ExternalSymbol) (level: int) : SemType =
        let inst ft fresh =
            FrozenTypeBridge.instantiateDeclaring ft fresh

        let scheme = sym.Scheme
        let constraints = sym.Constraints

        if sym.TyparArity = 0 then
            inst scheme [||]
        else
            let freshTvs =
                Array.init
                    sym.TyparArity
                    (fun _ ->
                        let tv = store.NewTypeVar()
                        store.SetLevel(UnionFind.find store tv, level)
                        tv
                    )

            let fresh = freshTvs |> Array.map TyVar

            // External symbols carry no source-side NodeKey; stamp `Unknown`
            // so diagnostics attribute the constraint to the use site.
            for c in constraints do
                match c with
                | ExternalConstraint.Trait(i, kind) ->
                    let cstr: SemanticConstraint =
                        {
                            Kind = kind
                            DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                        }

                    store.Constraints.Prepend(UnionFind.find store freshTvs.[i], cstr)
                | _ -> ()

            // Appended, not prepended: generalisation takes the first target in list order
            // that resolves, so the list must stay in source order.
            for c in constraints do
                match c with
                | ExternalConstraint.Default(i, target) ->
                    store.Defaults.Append(UnionFind.find store freshTvs.[i], inst target fresh)
                | _ -> ()

            for c in constraints do
                match c with
                | ExternalConstraint.MemberTrait(idxs, mName, argFts, retFt) ->
                    let sig_: MemberSignature =
                        {
                            MemberName = mName
                            ArgTypes = EqArray.ofSeq (seq { for ft in argFts -> inst ft fresh })
                            ReturnType = inst retFt fresh
                        }

                    for i in idxs do
                        store.Srtp.Prepend(UnionFind.find store freshTvs.[i], sig_)
                | _ -> ()

            for c in constraints do
                match c with
                | ExternalConstraint.Coercion(i, target) ->
                    let cstr: SemanticConstraint =
                        {
                            Kind = SemanticConstraintKind.Coercion(inst target fresh)
                            DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                        }

                    store.Constraints.Prepend(UnionFind.find store freshTvs.[i], cstr)
                | _ -> ()

            inst scheme fresh

    /// The module-qualified compiled name (`Vesper.Collections.ListModule.fold`), bare for
    /// an unqualified binding.
    let private valueSymbolName (key: BindingKey) : string =
        SymbolKeyOps.qualifiedName (SymbolKey.Binding key)

    /// The zero the two builders below copy from; `Name` is derived from `key` and `Scheme`
    /// is the deferred sentinel until filled.
    let private ofBindingKey (key: BindingKey) : ExternalSymbol =
        {
            Name = valueSymbolName key
            Scheme = deferredTemplate
            TyparArity = 0
            Constraints = []
            Origin = SymbolOrigin.Empty
            Key = key
            ValRepr = ValueNone
            ImportForm = ImportForm.Named
            InlineBody = ValueNone
        }

    /// A monomorphic symbol from a closed `FrozenType` scheme. `decl` is a module chain, or
    /// the namespace itself for an unqualified binding (a flat-package extern like
    /// `printfn`).
    let monoFrozen (decl: ModuleContainer) (name: string) (scheme: FrozenType) : ExternalSymbol =
        { ofBindingKey (SymbolKeyOps.bindingKeyOf decl name) with
            Scheme = scheme
        }

    /// A symbol from a `FrozenType` scheme over `arity` declaring typars: a template
    /// freshened per use site, not a closure.
    let scheme
        (decl: ModuleContainer)
        (name: string)
        (frozen: FrozenType)
        (arity: int)
        (constraints: ExternalConstraint list)
        : ExternalSymbol =
        { ofBindingKey (SymbolKeyOps.bindingKeyOf decl name) with
            Scheme = frozen
            TyparArity = arity
            Constraints = constraints
        }
