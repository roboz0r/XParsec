namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

// The codegen (target) provider — mirror of `IExternalSymbolProvider`. The
// symbol provider knows a compiled name's *shape*; the codegen provider knows
// how to *emit a call* to it.

/// How to emit a resolved call once its arguments are on the stack. `Emit`
/// performs the call itself — a `call` / `callvirt` against a metadata handle,
/// or a bare intrinsic opcode like `add` (which has no handle). `ArgCount`
/// includes the receiver for an instance call; the walker adjusts depth by
/// `Pushes - ArgCount`.
type CallRecipe =
    {
        Emit: Il -> unit
        ArgCount: int
        Pushes: int
    }

type CtorRecipe = { Handle: EntityHandle; ArgCount: int }

/// Which member of an emitted *generic* union a `GenericUnionMemberRef` resolves
/// to. A generic union (`List<'T>`) is a real generic `TypeDefinition`, so every
/// reference to one of its members — even from inside the type's own factory
/// bodies — must go through a `MemberRef` on a `TypeSpec` of the type
/// instantiated with the use-site's arguments (`List<int>` externally, `List<!0>`
/// internally). The member-ref *signature* is written in terms of the type's own
/// generic parameters (`!0`), with the instantiation riding the parent
/// `TypeSpec`. Monomorphic unions keep using their `Def` tokens directly.
[<RequireQualifiedAccess>]
type UnionMember =
    | Ctor
    | Tag
    /// Payload field named `<Case>_<index>`.
    | Field of caseName: string * fieldIndex: int
    | Factory of caseName: string
    /// An augmentation member (`get_Head` instance property, `Single` static
    /// method) of a generic union (R2). `metaName` is the emitted method name
    /// (a property is `get_<name>`); the signature (`paramTys` / `retTy`) is in
    /// the type's declaring-typar markers, written into the member ref as `!0`
    /// with the parent `TypeSpec` supplying the instantiation — same shape as
    /// `Factory`, but the member name + signature are explicit (not read from the
    /// case table).
    | Member of metaName: string * isStatic: bool * paramTys: FrozenType list * retTy: FrozenType

/// Which member of an emitted *generic* closure a `GenericClosureMemberRef`
/// resolves to. A generic closure is a real generic `TypeDefinition` (one
/// `<closure>$n` per enclosing-method specialisation point); every reference to
/// one of its members — construction site, capture-field load inside the
/// closure's own `Invoke` — must go through a `MemberRef` on the `TypeSpec` of
/// the closure instantiated with the use-site's arguments (`<closure>$n<int>`
/// externally, `<closure>$n<!0>` from inside the closure's own `Invoke`). The
/// member-ref *signature* is written in terms of the closure's own generic
/// parameters (`!i`), with the instantiation riding the parent `TypeSpec`.
/// Monomorphic closures keep using their `Def` tokens directly
/// (function-representation-plan §Generic closures, C2).
[<RequireQualifiedAccess>]
type ClosureMember =
    /// The closure's `.ctor(capture0, capture1, …)`.
    | Ctor
    /// The capture field at index `i` (declaration order = ctor-arg order).
    | CaptureField of fieldIndex: int
    /// `instance ResultTy Invoke(ParamTy)` — the closure's `Invoke` override.
    | Invoke

/// Which member of an emitted *generic* record a `GenericRecordMemberRef`
/// resolves to. The analogue of `UnionMember`, but minus the
/// tag/factory machinery — a record has one nameless shape with one ctor
/// taking the fields in declaration order, and fields are keyed by their
/// source-level name (not by `<case>_<index>`). A monomorphic record skips
/// this entirely and uses its `Def` tokens directly.
[<RequireQualifiedAccess>]
type RecordMember =
    /// The single instance `.ctor(field0, field1, …)`.
    | Ctor
    /// The public field named `fieldName` (records preserve source field
    /// names — no positional encoding).
    | Field of fieldName: string

/// Which member of an emitted *generic* class a `UserGenericMemberRef`
/// resolves to (vesper-set-sprint-plan Phase 1 / B-1). A class is shaped
/// like a record at the metadata level — one ctor taking the primary-ctor
/// parameters in declaration order, one backing field per ctor parameter
/// (keyed by source name) — plus the augmentation `members` (B-1 adds the
/// instance / static method/property surface unions already carry). A
/// monomorphic class skips this entirely (its `Def` tokens suffice).
[<RequireQualifiedAccess>]
type ClassMember =
    /// The primary `instance void .ctor(p0, p1, …)` — parameter types are
    /// the ctor params in declaration order.
    | Ctor
    /// A secondary `instance void .ctor(p0, …)` (B-11) selected by its parameter
    /// signature — F# forbids two ctors of the same signature, so `paramTys`
    /// (the ctor params in declaration order, written in the type's declaring-typar
    /// markers) keys the overload. Distinct from `Ctor` because the param types are
    /// the ctor's own, not the type's fields.
    | SecondaryCtor of paramTys: FrozenType list
    /// The backing field named `fieldName` for a primary-ctor parameter.
    | Field of fieldName: string
    /// An augmentation member (`get_X` instance property, `M` instance method,
    /// or static counterpart). `metaName` is the emitted method name (a
    /// property is `get_<name>`); the signature (`paramTys` / `retTy`) is in
    /// the type's declaring-typar markers, written into the member ref as
    /// `!0` with the parent `TypeSpec` supplying the instantiation — same
    /// shape as `UnionMember.Member`.
    | Member of metaName: string * isStatic: bool * paramTys: FrozenType list * retTy: FrozenType

/// Discriminator across the user-emitted generic-type-member families
/// (`UserGenericMemberRef`). Each variant wraps the family's specific
/// member info, preserving the case data (a record's field name, a union
/// case's payload index, …) that a pure ordinal couldn't carry. Phase 1
/// adds `ClassMember` (vesper-set-sprint-plan §1.5) alongside the existing
/// three families; `ClrProvider.userGenericMemberRef`'s dispatch grows one
/// arm — no fourth `Abstract` sibling on `ICodegenProvider`
/// (vesper-set-sprint-plan §0.3 / M3).
[<RequireQualifiedAccess>]
type UserMemberKind =
    | UnionMember of UnionMember
    | RecordMember of RecordMember
    | ClassMember of ClassMember

/// Resolved metadata handles for lowering a `TExpr.Format` to the write-through
/// handler (`Vesper.Formatter`). A `Format` can't be a `CallRecipe` — it
/// interleaves literals and lazily-evaluated args around a ref-struct local — so
/// the walker owns the call *sequence* and the provider supplies only the handles.
type FormatHandles =
    {
        HandlerLocal: FrozenType
        CtorWriter: EntityHandle
        CtorString: EntityHandle
        AppendLiteral: EntityHandle
        Flush: EntityHandle
        ToStringAndClear: EntityHandle
        ConsoleOut: EntityHandle
        ConsoleError: EntityHandle
        /// Instantiates `<T = ty>` and picks the overload from
        /// `(hasAlignment, hasFormat)`. The handle's signature must match the
        /// push order (value, alignment, format = the C# parameter order).
        AppendFormatted: FrozenType * bool * bool -> EntityHandle
        /// Alignment is always passed (0 ⇒ no padding).
        AppendBool: EntityHandle
        AppendOctal: EntityHandle
        /// `%u`: the `int` argument's bits reinterpreted as `uint`.
        AppendUnsigned: EntityHandle
        /// `%0w.pf`: zero-pad after the sign — .NET has no float format that does this.
        AppendZeroPaddedFloat: EntityHandle
    }

/// Resolves compiled names to emission recipes for one target (the .NET
/// implementation is `ClrProvider`). Intentionally minimal: the contract
/// crystallises from the working implementation rather than up-front design.
type ICodegenProvider =
    /// `fnTy` is the head's full declared (curried) type — passed whole because a
    /// multi-typar generic call can't recover its type arguments from the
    /// application's result alone (`printfn` reads the printer = result of `fnTy`;
    /// `List.fold` reads `'T` / `'State` from the folder parameter). `key` is
    /// the resolved `SymbolKey.ValueKey` Freeze stamped onto the head
    /// (`Resolution.ExternalValue`); the provider dispatches by *identity*
    /// when available — e.g. only the canonical `Vesper.Printf.printfn`
    /// trips the cold-printf recipe, never a project-local
    /// `MyMod.printfn` (vesper-set-sprint-plan §0.1 / M1). Unkeyed call
    /// sites (test mocks / pre-key-pipeline paths) pass `ValueNone` and the
    /// provider falls back to name-based matching for backwards compat.
    abstract TryEmitCall: compiledName: string * key: SymbolKey voption * fnTy: FrozenType -> CallRecipe voption

    /// `tyArgs` are the constructed type's instantiation arguments. `argTypes` are
    /// the call-site argument types (in source order), used by the external-ctor
    /// path to disambiguate overloads — a v1 picker matches arity only, future
    /// pickers can match by parameter type. The internal `PrintfFormat` recipe
    /// ignores them.
    abstract TryEmitCtor: key: SymbolKey * tyArgs: FrozenType list * argTypes: FrozenType list -> CtorRecipe voption

    /// `tyArgs` are the union type's instantiation arguments; the field values
    /// are already on the stack in declaration order beneath the call. The list
    /// constructors are static `call`s, so a `CallRecipe` fits — no new shape.
    /// The receiver is identified by its nominal `SymbolKey`: the FSharp.Core
    /// `list` vs the Vesper cons-list are recognised by key identity
    /// (`RuntimeNames.isFsharpCoreListKey` / `isVesperListKey`), not by string name.
    abstract TryEmitUnionCons: key: SymbolKey * caseName: string * tyArgs: FrozenType list -> CallRecipe voption

    /// A `MemberRef` to one member of an emitted *generic* nominal user type
    /// (union / record / class) identified by its nominal `SymbolKey` `key`,
    /// instantiated at `args`. The type must have been registered with the
    /// matching `ClrProvider.RegisterGeneric*` call. A monomorphic instance
    /// never reaches here — its `Def` tokens are used directly. `kind`
    /// picks the family + the specific member (union case factory,
    /// record field, …); internal `ClrProvider` dispatches it to the existing
    /// per-family helpers (was keyed by a
    /// string `name`; closures, which have no `SymbolKey`, split off onto
    /// `UserClosureMemberRef`).
    abstract UserGenericMemberRef: key: SymbolKey * args: FrozenType list * kind: UserMemberKind -> EntityHandle

    /// A `MemberRef` to one member of an emitted *generic* closure `name`
    /// (a synthetic `<closure>$n` name — closures carry no `SymbolKey`, so they
    /// ride their own seam rather than `UserGenericMemberRef`), instantiated at
    /// `args` (function-representation-plan §Generic closures C2/C3).
    abstract UserClosureMemberRef: name: string * args: FrozenType list * which: ClosureMember -> EntityHandle

    /// A `MemberRef` to a *referenced-assembly* record's `.ctor`, instantiated
    /// at `tyArgs`. The mirror of `TryEmitUnionCons` for records: when
    /// `env.Records` doesn't hold the type (it lives in another package — e.g.
    /// `Vesper.Ref\`1` in `Vesper.Core.dll`), the
    /// provider looks the record up through its external symbol stack and
    /// mints a `MemberRef` on the instantiated `TypeSpec`.
    /// `ValueNone` ⇒ the type is unknown to the provider (no contract / metadata
    /// hit), in which case the caller falls back to its old hard error.
    /// `fieldNames` is the source-order field list the caller would have used
    /// to drive the ctor argument push — `TryEmitRecordCons` returns the ctor
    /// recipe and the caller is responsible for matching declaration order via
    /// the type's external field shape (today: the contract's field order is
    /// the declaration order, so the source-order initialiser drives a separate
    /// reorder if needed).
    abstract TryEmitRecordCons: key: SymbolKey * tyArgs: FrozenType list * fieldNames: string list -> CtorRecipe voption

    /// A `MemberRef` to one named field on a *referenced-assembly* record,
    /// instantiated at `tyArgs` — the sibling of `TryEmitRecordCons` for the
    /// `FieldGet` / `FieldSet` / record-pattern paths. Returns the field
    /// `MemberRef` on the instantiated `TypeSpec` plus the field's declared
    /// type after applying the record's typar substitution (`'T` ⇒ `tyArgs.[i]`),
    /// so a `FieldGet` knows the value type a subsequent encode/store expects.
    /// `ValueNone` ⇒ unknown record, or unknown field on a known record.
    abstract TryResolveExternalRecordField:
        key: SymbolKey * tyArgs: FrozenType list * fieldName: string -> (EntityHandle * FrozenType) voption

    /// The `_tag : int` discriminator field `MemberRef` on a *referenced-package*
    /// union, instantiated at `tyArgs`, plus `caseName`'s tag value (its
    /// zero-based index in declaration order). The cross-package `match` arm reads
    /// `scrut._tag` and compares it against this value (vesper-lib-test-plan Gap 2
    /// Layer C); the union emitter (`NominalEmit.fs`) fixes both the field name and
    /// the declaration-order tagging. `ValueNone` ⇒ unknown union / case.
    abstract ExternalUnionTag:
        key: SymbolKey * tyArgs: FrozenType list * caseName: string -> (EntityHandle * int) voption

    /// One `<caseName>_<fieldIndex>` field `MemberRef` on a referenced-package
    /// union, instantiated at `tyArgs`, plus that field's substituted declared
    /// type — the field-extract slot a `match … Some x` binds. The union sibling of
    /// `TryResolveExternalRecordField`. `ValueNone` ⇒ unknown union / case / field.
    abstract ExternalUnionCaseField:
        key: SymbolKey * tyArgs: FrozenType list * caseName: string * fieldIndex: int ->
            (EntityHandle * FrozenType) voption

    /// A `MethodSpec` instantiating a *generic* module-static method (`fold`) at a
    /// call site (R3). `handle` is the method's (predicted) `MethodDefinition`;
    /// `instTypes` the per-typar instantiation recovered by matching the method's
    /// declared parameter types against the call's actual argument types. A
    /// recursive self-call passes the method's own typars (encoded `!!i` via the
    /// ambient set); an external call passes concrete types.
    abstract StaticFnMethodSpec: handle: EntityHandle * instTypes: FrozenType list -> EntityHandle

    /// Apply a function *value* of type `funcTy` to one argument —
    /// `Vesper.Fun\`2::Invoke` (R1). Receiver and argument are both already on the
    /// stack (receiver beneath), so the recipe's `ArgCount` is 2.
    abstract TryEmitInvoke: funcTy: FrozenType -> CallRecipe voption

    /// Apply a value that is an FSharp.Core `FSharpFunc` (not a `Vesper.Fun`) —
    /// `FSharpFunc\`2::Invoke`. R1's one remaining caller is the cold printf
    /// printer returned by `PrintFormatLine`; the printf engine retargets it
    /// (handoff §R9).
    abstract TryEmitFSharpFuncInvoke: funcTy: FrozenType -> CallRecipe voption

    /// `EqualityComparer<'T>.Default` getter and its `GetHashCode(!0)` — the
    /// `hash x` use-site's BCL body (no IL opcode hashes, so it rides the comparer,
    /// the same `EqualityComparer<T>` family the DU triple hashes fields through).
    /// On the interface because the expression walker emits the `hash` call;
    /// `Equals`/`Add` stay on the concrete provider, reached only from Codegen.
    abstract EqualityComparerDefault: elem: FrozenType -> EntityHandle
    abstract EqualityComparerGetHashCode: elem: FrozenType -> EntityHandle

    /// Mint a `MemberRef` for a `TExpr.ExternalMember` from its interned
    /// `SymbolKey` (the P4 identity bridge). The key
    /// pins the declaring type + member (assembly/namespace/name + member name +
    /// overload `argSig`); `memberTy` is the access's *instantiated* type (a
    /// property's type, or a method's curried `arg → … → ret`), from which the
    /// declaring type's instantiation is recovered by matching it against the
    /// member's open signature. `isProperty` selects the `get_<name>` getter shape,
    /// `isStatic` the (non-)`this` signature. The walker pushes the receiver/args
    /// and emits the `call` (static) / `callvirt` (instance) around the handle.
    abstract ExternalMemberRef:
        key: SymbolKey * isProperty: bool * isStatic: bool * memberTy: FrozenType -> EntityHandle

    /// Like `ExternalMemberRef`, but the declaring type's instantiation is given
    /// explicitly via `declTy` (the resolved declaring `TyClass`, e.g.
    /// `List`1+Enumerator<int>`) instead of recovered from the member's open
    /// signature. Required for a T-free member like `MoveNext(): bool` on a generic
    /// enumerator, whose signature mentions no typar so the instantiation is
    /// unrecoverable (vesper-set-sprint-phase-4 §4.4). The parent is encoded straight
    /// from `declTy`, so a struct declaring type lands as a `VALUETYPE` parent.
    abstract ExternalMemberRefOn:
        key: SymbolKey * declTy: FrozenType * isProperty: bool * isStatic: bool * memberTy: FrozenType -> EntityHandle

    abstract FormatHandles: unit -> FormatHandles

    /// Lives on the provider because encoding a `FrozenType` needs the target's
    /// type references.
    abstract EncodeLocalSignature: locals: FrozenType list -> StandaloneSignatureHandle

    abstract ObjectType: EntityHandle

    /// A `TypeDefOrRefOrSpec` token for an arbitrary `FrozenType`, for the operand
    /// of `isinst` / `castclass` / `box` / `unbox.any` (inheritance-plan
    /// §casting). One `TypeSpec`-based path covers mono, generic, and external
    /// targets alike.
    abstract TypeToken: ty: FrozenType -> EntityHandle

    /// Whether a *referenced-assembly / referenced-package* nominal type
    /// (identified by its nominal `SymbolKey`) is a .NET value type
    /// (`struct`). The metadata layer reads it off `Type.IsValueType`; the contract
    /// layer reads it off the `.fsi` `struct … end` form. The
    /// expression walker consults this so `EmitExpr.isValueType` recognises an
    /// external struct the same way it already recognises a project-local one —
    /// driving `:>`-box / `:?>`-unbox / value-receiver dispatch. `false` for every
    /// reference type and any unresolved name.
    abstract IsExternalValueType: key: SymbolKey -> bool

    /// `System.Decimal::.ctor(int32, int32, int32, bool, uint8)` — emits a
    /// `decimal` constant the way F# / Roslyn do, from `Decimal.GetBits`.
    abstract DecimalCtor: EntityHandle

    /// `System.Exception::.ctor(string)` — the fallthrough a non-exhaustive
    /// `match` throws. BCL, not `FSharp.Core`'s `MatchFailureException`, so it
    /// pins no dependency.
    abstract ExceptionCtor: EntityHandle

    /// The distinct FSharp.Core constructs the emission referenced so far.
    /// **Empty ⇒ the emitted PE does not depend on `FSharp.Core.dll`** — the one
    /// place that decides whether `materialiseApp` copies it; a non-empty set is
    /// the list of constructs still pinning the dependency. Read after emission.
    abstract FSharpCoreDependencies: unit -> string list
