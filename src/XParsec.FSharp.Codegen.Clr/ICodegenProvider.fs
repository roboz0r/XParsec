namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// A call's argument arity — how many applied arguments it consumes, and how many CLR
/// values that flattens to. The two diverge only for a callee carrying a SOURCE grouping:
/// a tupled group is one argument but N pushed values, a lone `()` group one but zero.
[<RequireQualifiedAccess>]
type CallArity =
    /// Argument count = flat pop count: every argument pushes one value (an instance
    /// call's count includes the object argument).
    | Flat of argCount: int
    /// The callee's SOURCE grouping drives the split: the walker consumes
    /// `groups.Length` arguments and flattens each to its pushed CLR values;
    /// `flatArgCount` is the resulting flat pop count.
    | Grouped of groups: TastAccessor.ArgGroup list * flatArgCount: int

    /// The number of CLR values the `call` actually pops. The applied-argument count
    /// is this for `Flat`, but `groups.Length` for `Grouped`.
    member this.FlatArgCount =
        match this with
        | Flat n -> n
        | Grouped(_, n) -> n

/// How to emit a resolved call once its arguments are on the stack. `Emit` performs the
/// call itself: a `call` / `callvirt` against a metadata handle, or a bare intrinsic
/// opcode like `add` (no handle). The walker adjusts depth by `Pushes - Arity.FlatArgCount`.
type CallRecipe =
    {
        Emit: Il -> unit
        Arity: CallArity
        Pushes: int
    }

type CtorRecipe = { Handle: EntityHandle; ArgCount: int }

/// The resolved CLR handles for one `System.ValueTuple` instantiation: the instantiated
/// parent `TypeSpec`, its `.ctor`, and the public `Item…` field refs in element order.
type ValueTupleHandles =
    {
        TypeSpec: EntityHandle
        Ctor: EntityHandle
        ItemFields: EntityHandle[]
        /// `ValueNone` at arity 2–7, the flat `ValueTuple`n``. At arity ≥ 8 the .NET
        /// nesting `ValueTuple`8<t0…t6, TRest>` applies: `ItemFields` holds only
        /// `Item1…Item7`, `Ctor` takes 8 args (7 elements + the nested `TRest`).
        Rest: ValueTupleRest voption
    }

/// The `ValueTuple`8` `TRest` link: the `Rest` field ref (typed as the 8th
/// generic parameter) and the handles of the nested residual tuple it stores.
and ValueTupleRest =
    {
        RestField: EntityHandle
        Nested: ValueTupleHandles
    }

/// Which member of an emitted *generic* union a `MemberRef` names. The ref's signature is
/// written in the union's own generic parameters (`!0`), the instantiation riding the parent
/// `TypeSpec`: `List<int>` externally, `List<!0>` from inside the type's own factory bodies.
[<RequireQualifiedAccess>]
type UnionMember =
    | Ctor
    | Tag
    /// Payload field named `<Case>_<index>`.
    | Field of caseName: string * fieldIndex: int
    | Factory of caseName: string

/// Which member of an emitted *generic* closure (`<closure>$n`) a `MemberRef` names.
/// Signature in the closure's own generic parameters (`!i`); the parent `TypeSpec` is
/// `<closure>$n<int>` externally, `<closure>$n<!0>` from inside its own `Invoke`.
[<RequireQualifiedAccess>]
type ClosureMember =
    /// The closure's `.ctor(capture0, capture1, …)`.
    | Ctor
    /// The capture field at index `i` (declaration order = ctor-arg order).
    | CaptureField of fieldIndex: int
    /// `instance ResultTy Invoke(ParamTy)` — the closure's `Invoke` override.
    | Invoke

/// Which member of an emitted *generic* record a `MemberRef` names.
[<RequireQualifiedAccess>]
type RecordMember =
    /// The single instance `.ctor(field0, field1, …)`, fields in declaration order.
    | Ctor
    /// The public field named `fieldName`, the source field name preserved verbatim.
    | Field of fieldName: string

/// Which member of an emitted *generic* class a `MemberRef` names.
[<RequireQualifiedAccess>]
type ClassMember =
    /// The primary `instance void .ctor(p0, p1, …)` — parameter types are
    /// the ctor params in declaration order.
    | Ctor
    /// A secondary `instance void .ctor(p0, …)` keyed by its parameter signature —
    /// `paramTys` in declaration order, written in the type's declaring-typar markers.
    /// F# forbids two ctors of one signature, so the signature identifies the overload.
    | SecondaryCtor of paramTys: FrozenType list
    /// The backing field named `fieldName` for a primary-ctor parameter.
    | Field of fieldName: string

/// Which family, and which member of it, a `UserGenericMemberRef` names.
[<RequireQualifiedAccess>]
type UserMemberKind =
    | UnionMember of UnionMember
    | RecordMember of RecordMember
    | ClassMember of ClassMember
    /// An augmentation member of any emitted generic type: union, record, class and
    /// interface alike. `metaName` is the emitted name, so a property is `get_X`;
    /// `paramTys` / `retTy` ride the declaring typars (`!0`), a generic method's own `!!i`.
    | Member of
        metaName: string *
        isStatic: bool *
        methodTyparCount: int *
        paramTys: FrozenType list *
        retTy: FrozenType

/// Resolved metadata handles for lowering a `TExpr.Format` to the write-through handler
/// (`Vesper.Formatter`). The walker owns the call *sequence*: literals and lazily-evaluated
/// args interleaved around a ref-struct local. The provider supplies only the handles.
type FormatHandles =
    {
        HandlerLocal: FrozenType
        CtorWriter: EntityHandle
        /// `bprintf`: the `(int, int, StringBuilder)` ctor.
        CtorBuilder: EntityHandle
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
        /// `%08o`: zero-padded two's-complement octal, signature `(value: int, width: int)`.
        /// A dedicated handler because .NET has no octal format that zero-pads to a total width.
        AppendZeroPaddedOctal: EntityHandle
        /// `%05u`: zero-padded unsigned decimal. Signature `(value: uint, width: int)`.
        /// Overflowing digits are not truncated, matching F#.
        AppendZeroPaddedUnsigned: EntityHandle
        /// `%0w.pf`: zero-pad after the sign, which no .NET float format does.
        AppendZeroPaddedFloat: EntityHandle
        /// `%-0w.pf`: zero-pad on the RIGHT (left-align + zero-pad float). Neither a .NET
        /// float format nor field alignment fills the right with zeros, so this handler does it.
        AppendRightZeroPaddedFloat: EntityHandle
        /// `%+0w.pf`/`% 0w.pf`: forced sign, then zero-pad after it to a total field.
        /// Signature `(value: float, format: string, width: int, space: bool)`. The
        /// `"F<prec>"` body rounds half-to-even and the sign is composed in-handler.
        AppendForcedSignZeroPaddedFloat: EntityHandle
        /// `%.*f`/`%*.*f`/`%.*e`/`%.*g`: runtime precision. Signature
        /// `(value: float, typeChar: char, precision: int, alignment: int)`, because the
        /// .NET format string is built in-handler from `typeChar` + `precision`.
        AppendDynamicPrecisionFloat: EntityHandle
        /// `%+.*f`/`% .*f`/`%+*.*f`: runtime-precision forced-sign float. Signature
        /// `(value: float, typeChar: char, precision: int, alignment: int, space: bool)`.
        /// The sign is composed in-handler.
        AppendDynamicPrecisionSignedFloat: EntityHandle
        /// `%A`: instantiates the generic `AppendStructured<T = ty>` for the
        /// structural-format engine. Signature `(value: T, width: int, size: int)` —
        /// the print-width budget and the print-size budget.
        AppendStructured: FrozenType -> EntityHandle
        /// `%*d`/`%-*d` runtime width guard, `static int32 GuardTotalWidth(int32)` —
        /// throws `ArgumentOutOfRangeException("totalWidth")` on a negative width
        /// (F# `PadLeft` parity), identity otherwise.
        GuardTotalWidth: EntityHandle
        /// `%*A` runtime column-budget clamp, `static int32 ClampWidth(int32)` —
        /// negative → 0 (flat), identity otherwise. `%A` renders a negative width
        /// flat rather than throwing, so it clamps instead of guarding.
        ClampWidth: EntityHandle
        /// `%*.*f`/`%*.*e`/… two-star precision clamp, `static int32
        /// NormalizePrecision(int32)` (0..99). Applied by the emitter only when a hole
        /// has BOTH star dims (the `printf.fs:632` asymmetry).
        NormalizePrecision: EntityHandle
    }

/// The `Vesper.IFormatSink` member refs the synthesised `IStructuralFormattable.Format`
/// body `callvirt`s, all of them `instance void`. The body emits the protocol
/// `BeginRecord; (Field; Child)×n; EndRecord` / `BeginCase; Child×k; EndCase`.
type FormatSinkHandles =
    {
        /// `void Text(string)` — a literal run that never breaks.
        Text: EntityHandle
        /// `void Line()` — a soft break (" " flat / newline broken).
        Line: EntityHandle
        /// `void SoftBreak()` — a soft break with no flat alternative.
        SoftBreak: EntityHandle
        BeginGroup: EntityHandle
        EndGroup: EntityHandle
        /// `void BeginNest(int)` — open an indent scope for broken lines.
        BeginNest: EntityHandle
        EndNest: EntityHandle
        BeginRecord: EntityHandle
        /// `void Field(string)` — a record-field label marker; the value follows via `Child`.
        Field: EntityHandle
        EndRecord: EntityHandle
        /// `void BeginCase(string)` — the case name.
        BeginCase: EntityHandle
        EndCase: EntityHandle
        /// `void Child(object)` — the sole child entry for synthesised bodies (records + cases).
        Child: EntityHandle
    }

/// Resolves compiled names to emission recipes for one target.
type ICodegenProvider =
    /// `fnTy` is the applied function's full curried type: a multi-typar call can't recover its type args
    /// from the result alone (`List.fold` reads `'T`/`'State` from the folder). `key` dispatches
    /// by identity: only `Vesper.Printf.printfn` trips cold-printf, never `MyMod.printfn`.
    abstract TryEmitCall: compiledName: string * key: SymbolKey voption * fnTy: FrozenType -> CallRecipe voption

    /// `chosen` is the front-end-resolved `.ctor`'s `SymbolKey.MemberKey` when a `TExpr.New`
    /// recorded it, selecting that exact same-arity overload by identity; `ValueNone` falls
    /// back to the first arity match. `tyArgs` instantiate the constructed type.
    abstract TryEmitCtor:
        key: SymbolKey * chosen: SymbolKey voption * tyArgs: FrozenType list * argTypes: FrozenType list ->
            CtorRecipe voption

    /// `tyArgs` are the union type's instantiation arguments; the field values are already
    /// on the stack in declaration order beneath the call. The union is identified by
    /// key identity, not by name, since FSharp.Core's `list` and the Vesper cons-list
    /// are both written `list`.
    abstract TryEmitUnionCons: key: TypeKey * caseName: string * tyArgs: FrozenType list -> CallRecipe voption

    /// A `MemberRef` to one member of an emitted *generic* nominal user type (union /
    /// record / class) `key`, instantiated at `args`. A monomorphic instance never reaches
    /// here because its `Def` tokens are used directly.
    abstract UserGenericMemberRef: key: TypeKey * args: FrozenType list * kind: UserMemberKind -> EntityHandle

    /// A `MemberRef` to one member of an emitted *generic* closure `name` (a synthetic
    /// `<closure>$n`), instantiated at `args`. Closures carry no `SymbolKey`.
    abstract UserClosureMemberRef: name: string * args: FrozenType list * which: ClosureMember -> EntityHandle

    /// A `MemberRef` to a *referenced-assembly* record's `.ctor`, instantiated at `tyArgs`,
    /// for a record declared in another package (`Vesper.Ref\`1` in `Vesper.Core.dll`).
    /// `ValueNone` ⇒ the record is unknown here.
    abstract TryEmitRecordCons: key: TypeKey * tyArgs: FrozenType list * fieldNames: string list -> CtorRecipe voption

    /// A `MemberRef` to one named field on a *referenced-assembly* record, instantiated at
    /// `tyArgs`, plus the field's declared type after the record's typar substitution
    /// (`'T` ⇒ `tyArgs.[i]`). `ValueNone` ⇒ unknown record, or unknown field on a known one.
    abstract TryResolveExternalRecordField:
        key: SymbolKey * tyArgs: FrozenType list * fieldName: string -> (EntityHandle * FrozenType) voption

    /// The `_tag : int` discriminator field `MemberRef` on a *referenced-package* union,
    /// instantiated at `tyArgs`, plus `caseName`'s tag value (its zero-based index in
    /// declaration order). A cross-package `match` arm compares `scrut._tag` against it.
    abstract ExternalUnionTag: key: TypeKey * tyArgs: FrozenType list * caseName: string -> (EntityHandle * int) voption

    /// One `<caseName>_<fieldIndex>` field `MemberRef` on a referenced-package union,
    /// instantiated at `tyArgs`, plus that field's substituted declared type. The field is
    /// the slot a `match … Some x` extracts from. `ValueNone` ⇒ unknown union/case/field.
    abstract ExternalUnionCaseField:
        key: TypeKey * tyArgs: FrozenType list * caseName: string * fieldIndex: int ->
            (EntityHandle * FrozenType) voption

    /// A `MethodSpec` instantiating a *generic* module-static method (`fold`) at a call
    /// site. `instTypes` is the per-typar instantiation: a recursive self-call passes the
    /// method's own typars (`!!i` via the ambient set), an external call concrete types.
    abstract StaticFnMethodSpec: handle: EntityHandle * instTypes: FrozenType list -> EntityHandle

    /// Recover the declaring- and method-axis type arguments by structurally matching an
    /// *open* signature (carrying `FTTypar(Declaring,i)` / `FTTypar(Method,i)` markers)
    /// against its *instantiated* counterpart. Returns `(declaringArgs, methodArgs)`.
    abstract RecoverOpenTypars:
        declTyparArity: int * methodTyparArity: int * openT: FrozenType * instT: FrozenType ->
            FrozenType list * FrozenType list

    /// `Vesper.Fun\`2::Invoke` — apply a function *value* of type `funcTy` to one argument.
    /// Object argument and argument are both already on the stack (object arg beneath),
    /// so the recipe's arity is `Flat 2`.
    abstract TryEmitInvoke: funcTy: FrozenType -> CallRecipe voption

    /// `EqualityComparer<'T>.Default` getter and its `GetHashCode(!0)` — the `hash x`
    /// use-site's body, since no IL opcode hashes.
    abstract EqualityComparerDefault: elem: FrozenType -> EntityHandle
    abstract EqualityComparerGetHashCode: elem: FrozenType -> EntityHandle
    /// `EqualityComparer<'T>.Default.Equals(!0, !0)` — a boxing-free typed field compare.
    abstract EqualityComparerEquals: elem: FrozenType -> EntityHandle

    /// Mint a `MemberRef` for a `TExpr.ExternalMember`; `isProperty` selects the `get_<name>`
    /// getter shape. `memberTy` is the access's *instantiated* type (a method's curried
    /// `arg → … → ret`), matched against the open signature to recover the declaring args.
    abstract ExternalMemberRef:
        key: SymbolKey * isProperty: bool * isStatic: bool * memberTy: FrozenType -> EntityHandle

    /// Rebase a capability member onto its true BCL declarer: `enumerator<'T>` keys `MoveNext`
    /// on `IEnumerator`1`, but it is declared on non-generic `System.Collections.IEnumerator`
    /// and a ref parented on the generic one throws. `ValueNone` ⇒ no rebase needed.
    abstract TryCapabilityBaseMemberKey: key: SymbolKey -> SymbolKey voption

    /// Like `ExternalMemberRef`, but the declaring instantiation is given explicitly via
    /// `declTy` (`List`1+Enumerator<int>`), because a T-free member like `MoveNext(): bool`
    /// mentions no typar to recover it from. A struct `declTy` lands `VALUETYPE` parent.
    abstract ExternalMemberRefOn:
        key: SymbolKey * declTy: FrozenType * isProperty: bool * isStatic: bool * memberTy: FrozenType -> EntityHandle

    /// Mint a field `MemberRef` for an external public field (`String.Empty`, `ValueTuple`2.Item1`),
    /// read via `ldfld`/`ldsfld` not a `get_<name>` accessor. `declTy` pins the declaring
    /// instantiation; a static field passes `ValueNone` and recovers it from `memberTy`.
    abstract ExternalFieldRef: key: SymbolKey * declTy: FrozenType voption * memberTy: FrozenType -> EntityHandle

    abstract FormatHandles: unit -> FormatHandles

    abstract EncodeLocalSignature: locals: FrozenType list -> StandaloneSignatureHandle

    /// `MemberRef` for the parameterless `.ctor()` of a HERITABLE external base class
    /// (`type X = (# class "System.Attribute" #)`) — what a derived primary `.ctor` chains
    /// to instead of `System.Object::.ctor`. Minted off the `TypeRef`: it may be `protected`.
    abstract ExternalParameterlessBaseCtor: key: SymbolKey -> EntityHandle voption

    /// The raw external `TypeRef` for `key` (a heritable external base class), the token a
    /// derived type's `extends` column names. `ValueNone` ⇒ `key` is not an external class.
    abstract ExternalClassTypeRef: key: SymbolKey -> EntityHandle voption

    /// Resolve an intrinsic-CLASS `inherit` parent to its platform external key
    /// (`System.Exception`) plus its raw `TypeRef`, the derived type's `extends` token.
    /// Such a parent is an `FTConst` canon (`exn`) whose platform repr is a heritable
    /// BCL reference class.
    abstract IntrinsicClassBase: canon: SymbolKey -> struct (SymbolKey * EntityHandle) voption

    abstract ObjectType: EntityHandle

    /// A `TypeDefOrRefOrSpec` token for an arbitrary `FrozenType`, for the operand
    /// of `isinst` / `castclass` / `box` / `unbox.any`.
    abstract TypeToken: ty: FrozenType -> EntityHandle

    /// The resolved `System.ValueTuple`n` handles for an N-tuple over `elemTys`.
    abstract ValueTupleRefs: elemTys: FrozenType list -> ValueTupleHandles

    /// Whether a *referenced-assembly / referenced-package* nominal type is a .NET value
    /// type. The metadata layer reads it off `Type.IsValueType`, the contract layer off the
    /// `.fsi` `struct … end` form. `false` for a reference type or any unresolved name.
    abstract IsExternalValueType: key: SymbolKey -> bool

    /// `System.Decimal::.ctor(int32, int32, int32, bool, uint8)` — emits a
    /// `decimal` constant the way F# / Roslyn do, from `Decimal.GetBits`.
    abstract DecimalCtor: EntityHandle

    /// `System.Exception::.ctor(string)` — the fallthrough a non-exhaustive `match` throws.
    /// BCL, not `FSharp.Core`'s `MatchFailureException`, so it pins no dependency.
    abstract ExceptionCtor: EntityHandle

    /// The distinct FSharp.Core constructs the emission referenced so far, read after
    /// emission. Empty ⇒ the emitted PE does not depend on `FSharp.Core.dll`.
    abstract FSharpCoreDependencies: unit -> string list
