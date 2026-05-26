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
    | Member of metaName: string * isStatic: bool * paramTys: SemType list * retTy: SemType

/// Which member of an emitted *generic* record a `GenericRecordMemberRef`
/// resolves to. The records-plan §B2 analogue of `UnionMember`, but minus the
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

/// Resolved metadata handles for lowering a `TExpr.Format` to the write-through
/// handler (`Vesper.Formatter`). A `Format` can't be a `CallRecipe` — it
/// interleaves literals and lazily-evaluated args around a ref-struct local — so
/// the walker owns the call *sequence* and the provider supplies only the handles.
type FormatHandles =
    {
        HandlerLocal: SemType
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
        AppendFormatted: SemType * bool * bool -> EntityHandle
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
    /// `List.fold` reads `'T` / `'State` from the folder parameter).
    abstract TryEmitCall: compiledName: string * fnTy: SemType -> CallRecipe voption

    /// `tyArgs` are the constructed type's instantiation arguments.
    abstract TryEmitCtor: className: string * tyArgs: SemType list -> CtorRecipe voption

    /// `tyArgs` are the union type's instantiation arguments; the field values
    /// are already on the stack in declaration order beneath the call. The list
    /// constructors are static `call`s, so a `CallRecipe` fits — no new shape.
    abstract TryEmitUnionCons: typeName: string * caseName: string * tyArgs: SemType list -> CallRecipe voption

    /// A `MemberRef` to one member of an emitted *generic* union `name`,
    /// instantiated at `args`. The union must have been registered with
    /// `ClrProvider.RegisterGenericUnion`. A monomorphic union never reaches
    /// here — its `Def` tokens are used directly.
    abstract GenericUnionMemberRef: name: string * args: SemType list * which: UnionMember -> EntityHandle

    /// A `MemberRef` to one member of an emitted *generic* record `name`,
    /// instantiated at `args`. The record must have been registered with
    /// `ClrProvider.RegisterGenericRecord`. A monomorphic record never reaches
    /// here — its `Def` tokens are used directly (records-plan §B2).
    abstract GenericRecordMemberRef: name: string * args: SemType list * which: RecordMember -> EntityHandle

    /// A `MemberRef` to a *referenced-assembly* record's `.ctor`, instantiated
    /// at `tyArgs`. The mirror of `TryEmitUnionCons` for records: when
    /// `env.Records` doesn't hold the type (it lives in another package — e.g.
    /// `Vesper.Ref\`1` in `Vesper.Core.dll` after records-handoff Phase 2
    /// follow-up F2), the provider looks the record up through its external
    /// symbol stack and mints a `MemberRef` on the instantiated `TypeSpec`.
    /// `ValueNone` ⇒ the type is unknown to the provider (no contract / metadata
    /// hit), in which case the caller falls back to its old hard error.
    /// `fieldNames` is the source-order field list the caller would have used
    /// to drive the ctor argument push — `TryEmitRecordCons` returns the ctor
    /// recipe and the caller is responsible for matching declaration order via
    /// the type's external field shape (today: the contract's field order is
    /// the declaration order, so the source-order initialiser drives a separate
    /// reorder if needed).
    abstract TryEmitRecordCons: typeName: string * tyArgs: SemType list * fieldNames: string list -> CtorRecipe voption

    /// A `MemberRef` to one named field on a *referenced-assembly* record,
    /// instantiated at `tyArgs` — the sibling of `TryEmitRecordCons` for the
    /// `FieldGet` / `FieldSet` / record-pattern paths. Returns the field
    /// `MemberRef` on the instantiated `TypeSpec` plus the field's declared
    /// type after applying the record's typar substitution (`'T` ⇒ `tyArgs.[i]`),
    /// so a `FieldGet` knows the value type a subsequent encode/store expects.
    /// `ValueNone` ⇒ unknown record, or unknown field on a known record.
    abstract TryResolveExternalRecordField:
        typeName: string * tyArgs: SemType list * fieldName: string -> (EntityHandle * SemType) voption

    /// A `MethodSpec` instantiating a *generic* module-static method (`fold`) at a
    /// call site (R3). `handle` is the method's (predicted) `MethodDefinition`;
    /// `instTypes` the per-typar instantiation recovered by matching the method's
    /// declared parameter types against the call's actual argument types. A
    /// recursive self-call passes the method's own typars (encoded `!!i` via the
    /// ambient set); an external call passes concrete types.
    abstract StaticFnMethodSpec: handle: EntityHandle * instTypes: SemType list -> EntityHandle

    /// Apply a function *value* of type `funcTy` to one argument —
    /// `Vesper.Fun\`2::Invoke` (R1). Receiver and argument are both already on the
    /// stack (receiver beneath), so the recipe's `ArgCount` is 2.
    abstract TryEmitInvoke: funcTy: SemType -> CallRecipe voption

    /// Apply a value that is an FSharp.Core `FSharpFunc` (not a `Vesper.Fun`) —
    /// `FSharpFunc\`2::Invoke`. R1's one remaining caller is the cold printf
    /// printer returned by `PrintFormatLine`; the printf engine retargets it
    /// (handoff §R9).
    abstract TryEmitFSharpFuncInvoke: funcTy: SemType -> CallRecipe voption

    /// `EqualityComparer<'T>.Default` getter and its `GetHashCode(!0)` — the
    /// `hash x` use-site's BCL body (no IL opcode hashes, so it rides the comparer,
    /// the same `EqualityComparer<T>` family the DU triple hashes fields through).
    /// On the interface because the expression walker emits the `hash` call;
    /// `Equals`/`Add` stay on the concrete provider, reached only from Codegen.
    abstract EqualityComparerDefault: elem: SemType -> EntityHandle
    abstract EqualityComparerGetHashCode: elem: SemType -> EntityHandle

    /// Mint a `MemberRef` for a `TExpr.ExternalMember` from its interned
    /// `SymbolKey` (the P4 identity bridge — symbol-resolution-plan §7.2). The key
    /// pins the declaring type + member (assembly/namespace/name + member name +
    /// overload `argSig`); `memberTy` is the access's *instantiated* type (a
    /// property's type, or a method's curried `arg → … → ret`), from which the
    /// declaring type's instantiation is recovered by matching it against the
    /// member's open signature. `isProperty` selects the `get_<name>` getter shape,
    /// `isStatic` the (non-)`this` signature. The walker pushes the receiver/args
    /// and emits the `call` (static) / `callvirt` (instance) around the handle.
    abstract ExternalMemberRef: key: SymbolKey * isProperty: bool * isStatic: bool * memberTy: SemType -> EntityHandle

    abstract FormatHandles: unit -> FormatHandles

    /// Lives on the provider because encoding a `SemType` needs the target's
    /// type references.
    abstract EncodeLocalSignature: locals: SemType list -> StandaloneSignatureHandle

    abstract ObjectType: EntityHandle

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
