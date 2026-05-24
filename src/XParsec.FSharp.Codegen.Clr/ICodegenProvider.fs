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
