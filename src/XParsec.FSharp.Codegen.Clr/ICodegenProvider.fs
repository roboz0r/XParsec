namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

// The codegen (target) provider — mirror of `IExternalSymbolProvider`. The
// symbol provider knows a compiled name's *shape*; the codegen provider knows
// how to *emit a call* to it. `TExpr.External` / `TExpr.New` are the only
// nodes the walker consults it for. See
// [codegen-clr-plan](../XParsec.FSharp.SemanticAnalysis/docs/codegen-clr-plan.md)
// §"The codegen (target) provider".

/// How to emit a resolved call once its arguments are on the stack. `Emit`
/// performs the call itself — a `call` / `callvirt` against a metadata handle,
/// or a bare intrinsic opcode like `add` (which has no handle). `ArgCount` is
/// how many stack values the operation consumes (for an instance call this
/// includes the receiver) and `Pushes` the count it produces (0 or 1); the
/// walker adjusts the tracked depth by `Pushes - ArgCount`.
type CallRecipe =
    {
        Emit: Il -> unit
        ArgCount: int
        Pushes: int
    }

/// How to emit a constructor (`newobj`) once its arguments are on the stack.
type CtorRecipe = { Handle: EntityHandle; ArgCount: int }

/// Which member of an emitted *generic* union a `GenericUnionMemberRef` resolves
/// to (P3d.4). A generic union (`List<'T>`) is a real generic `TypeDefinition`,
/// so every reference to one of its members — even from inside the type's own
/// factory bodies — must go through a `MemberRef` on a `TypeSpec` of the type
/// instantiated with the use-site's arguments (`List<int>` externally, `List<!0>`
/// internally). The member-ref *signature* is written in terms of the type's own
/// generic parameters (`!0`), with the instantiation riding the parent
/// `TypeSpec`. Monomorphic unions keep using their `Def` tokens directly.
[<RequireQualifiedAccess>]
type UnionMember =
    /// The parameterless `.ctor()` a factory chains to.
    | Ctor
    /// The `int _tag` discriminant field.
    | Tag
    /// A case's payload field at `fieldIndex` (named `<Case>_<index>`).
    | Field of caseName: string * fieldIndex: int
    /// A case's static factory (`static List<!0> Cons(!0, List<!0>)`).
    | Factory of caseName: string

/// Resolved metadata handles for lowering a `TExpr.Format` to the write-through
/// handler (`Vesper.Formatter`). The walker owns the call *sequence* and the
/// per-hole argument recursion — a `Format` can't be a `CallRecipe` (it
/// interleaves literals and lazily-evaluated args around a ref-struct local).
/// The provider supplies only the resolved handles, so the target specifics
/// stay here. See [vesper-printf-plan](../XParsec.FSharp.SemanticAnalysis/docs/vesper-printf-plan.md).
type FormatHandles =
    {
        /// `SemType` of the handler local (a value type) for `Il.DeclareLocal`.
        HandlerLocal: SemType
        /// `instance void .ctor(int32, int32, TextWriter)` — write-through sink.
        CtorWriter: EntityHandle
        /// `instance void .ctor(int32, int32)` — string sink.
        CtorString: EntityHandle
        /// `instance void AppendLiteral(string)`.
        AppendLiteral: EntityHandle
        /// `instance void Flush()` — write-through flush + buffer release.
        Flush: EntityHandle
        /// `instance string ToStringAndClear()` — string-sink result + release.
        ToStringAndClear: EntityHandle
        /// `static TextWriter System.Console.get_Out()`.
        ConsoleOut: EntityHandle
        /// `static TextWriter System.Console.get_Error()`.
        ConsoleError: EntityHandle
        /// `AppendFormatted<T>(...)` for one hole: instantiates `<T = ty>` and
        /// picks the overload from `(hasAlignment, hasFormat)`. The walker pushes
        /// the value, then (if present) the alignment `int32`, then the format
        /// `string` — the C# parameter order — so the returned handle's signature
        /// must match that order.
        AppendFormatted: SemType * bool * bool -> EntityHandle
        /// `instance void AppendBool(bool, int32)` — `%b` (lowercase
        /// `true`/`false`). Alignment is always passed (0 ⇒ no padding).
        AppendBool: EntityHandle
        /// `instance void AppendOctal(int32, int32)` — `%o` (32-bit
        /// two's-complement octal). Alignment always passed.
        AppendOctal: EntityHandle
        /// `instance void AppendUnsigned(uint32, int32)` — `%u` (the `int`
        /// argument's bits reinterpreted as `uint`). Alignment always passed.
        AppendUnsigned: EntityHandle
        /// `instance void AppendZeroPaddedFloat(float64, string, int32)` — `%0w.pf`
        /// (zero-pad a float to a total field width, after the sign; .NET has no
        /// float format that does this). The walker pushes value, format, width.
        AppendZeroPaddedFloat: EntityHandle
    }

/// Resolves compiled names to emission recipes for one target. The .NET
/// implementation is `ClrProvider`. The interface is intentionally minimal in
/// v1 — per the backend posture, the contract crystallises from the working
/// implementation rather than up-front design.
type ICodegenProvider =
    /// Resolve a function/operator compiled name (as carried by
    /// `TExpr.External`) to a call recipe. `fnTy` is the head's full declared
    /// (curried) type — every recipe reads what it needs from it: `printfn`
    /// takes the printer = result of `fnTy`; `List.fold` reads `'T` / `'State`
    /// from the folder parameter. A multi-typar generic call can't recover its
    /// type arguments from the application's result alone, so the whole `fnTy`
    /// is passed rather than just the result.
    abstract TryEmitCall: compiledName: string * fnTy: SemType -> CallRecipe voption

    /// Resolve a constructor (as carried by `TExpr.New` / `TExpr.UnionCons`)
    /// to a ctor recipe. `tyArgs` are the constructed type's instantiation
    /// arguments (e.g. `PrintfFormat`'s four type parameters).
    abstract TryEmitCtor: className: string * tyArgs: SemType list -> CtorRecipe voption

    /// Resolve a union-case constructor (as carried by `TExpr.UnionCons`) to a
    /// call recipe. `tyArgs` are the union type's instantiation arguments (for
    /// `list<int>`, `[int]`); the field values are already on the stack in
    /// declaration order beneath the call. The list constructors are static
    /// `call`s (`Cons` / `get_Empty`), so a `CallRecipe` fits — no new shape.
    abstract TryEmitUnionCons: typeName: string * caseName: string * tyArgs: SemType list -> CallRecipe voption

    /// A `MemberRef` to one member of an emitted *generic* union `name`,
    /// instantiated at `args` (P3d.4). The union must have been registered with
    /// `ClrProvider.RegisterGenericUnion`. Used for every generic-union member
    /// access: the construction site (`UnionMember.Factory`), the `match`
    /// deconstruction (`Tag` / `Field`), and the factory bodies themselves
    /// (`Ctor` / `Tag` / `Field`, with `args` the type's own typar markers). A
    /// monomorphic union never reaches here — its `Def` tokens are used directly.
    abstract GenericUnionMemberRef: name: string * args: SemType list * which: UnionMember -> EntityHandle

    /// Resolve the application of a function *value* of type `funcTy`
    /// (a `TyFun(a, b)`) to one argument — `FSharpFunc\`2::Invoke`. The
    /// receiver function and the argument are both already on the stack
    /// (receiver beneath), so the recipe's `ArgCount` is 2.
    abstract TryEmitInvoke: funcTy: SemType -> CallRecipe voption

    /// Resolved handles for lowering a `TExpr.Format` (printf / interpolation
    /// happy path). Built fresh per `Format` node; the walker drives the call
    /// sequence with them.
    abstract FormatHandles: unit -> FormatHandles

    /// Encode a method body's declared locals into a standalone
    /// local-variable signature. Lives on the provider because encoding a
    /// `SemType` needs the target's type references.
    abstract EncodeLocalSignature: locals: SemType list -> StandaloneSignatureHandle

    /// The `System.Object` type reference, for emitted classes' base type.
    abstract ObjectType: EntityHandle

    /// Member ref to `System.Decimal::.ctor(int32, int32, int32, bool, uint8)`
    /// (lo / mid / hi / isNegative / scale), for emitting a `decimal` constant
    /// the way F# / Roslyn do — `Decimal.GetBits` supplies the five operands.
    abstract DecimalCtor: EntityHandle

    /// Member ref to `System.Exception::.ctor(string)` — the fallthrough a
    /// non-exhaustive `match` throws when no arm matches. BCL, not
    /// `FSharp.Core`'s `MatchFailureException`, so it pins no dependency.
    abstract ExceptionCtor: EntityHandle

    /// The distinct FSharp.Core constructs the emission referenced so far
    /// (construct-qualified names, e.g. `Microsoft.FSharp.Core.FSharpFunc\`2`).
    /// **Empty ⇒ the emitted PE does not depend on `FSharp.Core.dll`** — the one
    /// place that decides whether `materialiseApp` copies it. A non-empty set
    /// doubles as the list of constructs still pinning the dependency. Read after
    /// emission completes (every reference is minted during the body builds).
    abstract FSharpCoreDependencies: unit -> string list
