namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// A call's argument arity — how many applied arguments it consumes, and how many CLR
/// values that flattens to. The two diverge only for a callee carrying a SOURCE grouping:
/// a tupled group is one argument but N pushed values, a lone `()` group one but zero.
[<RequireQualifiedAccess>]
type CallArity =
    /// Argument count = flat pop count: every argument pushes one value (an instance
    /// call's count includes the object argument).
    | Flat of argCount: int
    /// The callee's SOURCE grouping drives the split: the walker consumes one argument per
    /// group and flattens each to its pushed CLR values.
    | Grouped of CompiledFns.FlatParams<FrozenType>

    /// The number of CLR values the `call` actually pops. The applied-argument count
    /// is this for `Flat`, but the group count for `Grouped`.
    member this.FlatArgCount =
        match this with
        | Flat n -> n
        | Grouped ps -> ps.FlatCount

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

/// Which member of an emitted *generic* union a `MemberRef` identifies. The ref's signature is
/// written in the union's own generic parameters (`!0`), the instantiation carried on the
/// parent `TypeSpec`: `List<int>` externally, `List<!0>` from inside the type's own factory bodies.
[<RequireQualifiedAccess>]
type UnionMember =
    /// The union's own `.ctor`, its parameters given by `UnionCtorShape.ofRegime`.
    | Ctor
    /// The `private _tag : int32` discriminant, declared only where `UnionRegime.hasTag`
    /// holds. Reachable from the union's own bodies and from its case types; elsewhere
    /// use `GetTag`.
    | Tag
    /// `get_Tag`, the public accessor for `Tag`.
    | GetTag
    /// A case's payload field, parented on the case's own type in a hierarchy regime and
    /// on the union itself in a flat one.
    | Field of caseName: string * fieldIndex: int
    /// A hierarchy union case type's `.ctor(payload…)`.
    | CaseCtor of caseName: string
    /// A reference union's `_unique_<Case>` singleton for a nullary case, a `static` field
    /// on the union typed as the union.
    | CaseSingleton of caseName: string
    | Factory of caseName: string

/// The read path from a union scrutinee to one case field.
[<RequireQualifiedAccess>]
type UnionCaseAccess =
    /// `ldfld` the payload field off the scrutinee, or off the case type in a hierarchy
    /// regime.
    | Field of EntityHandle
    /// `call` the `Get_<Case>_<i>` reader on the scrutinee's address. The scrutinee is a
    /// value type.
    | Getter of EntityHandle

/// A discriminant comparison: call `Getter` on the scrutinee and branch unless the result
/// equals `Tag`.
type TagTest =
    {
        /// `get_Tag`, the accessor fronting the union's private `_tag`.
        Getter: EntityHandle
        /// The case's zero-based index in declaration order.
        Tag: int
    }

/// The test one match arm emits to settle whether the scrutinee is a given case.
[<RequireQualifiedAccess>]
type UnionCaseTest =
    /// A single-case union: every value inhabits the case, so no test is emitted.
    | Irrefutable
    | TagEquals of TagTest
    /// `isinst` the case's own type: a non-null result settles the case.
    | IsInst of caseType: EntityHandle

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionCaseTest =

    /// The test a match arm emits under `regime`. `tag` is the case's zero-based index in
    /// declaration order. `tagGetter` and `caseType` mint their handles in the caller's
    /// own scope; each is forced exactly under the regime that reads it.
    let ofRegime
        (regime: UnionRegime)
        (tag: int)
        (tagGetter: unit -> EntityHandle)
        (caseType: unit -> EntityHandle)
        : UnionCaseTest =
        match regime with
        | UnionRegime.SingleCase -> UnionCaseTest.Irrefutable
        | UnionRegime.TypeTested -> UnionCaseTest.IsInst(caseType ())
        | UnionRegime.EnumLike
        | UnionRegime.StructTagged
        | UnionRegime.Tagged -> UnionCaseTest.TagEquals { Getter = tagGetter (); Tag = tag }

/// Which member of an emitted *generic* closure (`<closure>$n`) a `MemberRef` identifies.
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

/// Which member of an emitted *generic* record a `MemberRef` identifies.
[<RequireQualifiedAccess>]
type RecordMember =
    /// The single instance `.ctor(field0, field1, …)`, fields in declaration order.
    | Ctor
    /// The public field named `fieldName`, the source field name preserved verbatim.
    | Field of fieldName: string

/// Which member of an emitted *generic* class a `MemberRef` identifies.
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

/// Which family, and which member of it, a `UserGenericMemberRef` identifies.
[<RequireQualifiedAccess>]
type UserMemberKind =
    | UnionMember of UnionMember
    | RecordMember of RecordMember
    | ClassMember of ClassMember
    /// An augmentation member of any emitted generic type: union, record, class and
    /// interface alike. `metaName` is the emitted name, so a property is `get_X`;
    /// `paramTys` / `retTy` are written in the declaring typars (`!0`), a generic method's own `!!i`.
    | Member of
        metaName: string *
        isStatic: bool *
        methodTyparCount: int *
        paramTys: FrozenType list *
        retTy: FrozenType

/// A `Vesper.Formatter` append member generic in the value: one open generic method, to be
/// instantiated at the hole's type. `float32` and `decimal` format at their own type, since
/// widening either to `float` renders different digits.
[<RequireQualifiedAccess>]
type GenericAppend =
    /// `%d`, `%x`, `%s`, an interpolation hole — anything a general formatter renders.
    /// The overload is chosen by which optional parameters are present, in the C#
    /// declaration order (alignment before format).
    | Formatted of hasAlignment: bool * hasFormat: bool
    /// `%0w.pf`: zero-pad after the sign, which no .NET float format does. Signature
    /// `(value: T, format: string, width: int)`.
    | ZeroPaddedFloat
    /// `%-0w.pf`: zero-pad on the RIGHT (left-align + zero-pad float). Neither a .NET
    /// float format nor field alignment fills the right with zeros.
    | RightZeroPaddedFloat
    /// `%+0w.pf`/`% 0w.pf`: forced sign, then zero-pad after it to a total field.
    /// Signature `(value: T, format: string, width: int, space: bool)`. The `"F<prec>"`
    /// body rounds half-to-even and the sign is composed in-handler.
    | ForcedSignZeroPaddedFloat
    /// `%.*f`/`%*.*f`/`%.*e`/`%.*g`: runtime precision. Signature
    /// `(value: T, typeChar: char, precision: int, alignment: int)`, because the .NET
    /// format string is built in-handler from `typeChar` + `precision`.
    | DynamicPrecisionFloat
    /// `%+.*f`/`% .*f`/`%+*.*f`: runtime-precision forced-sign float. Signature
    /// `(value: T, typeChar: char, precision: int, alignment: int, space: bool)`.
    | DynamicPrecisionSignedFloat
    /// `%A`: the structural-format engine. Signature `(value: T, width: int, size: int)` —
    /// the print-width budget and the print-size budget.
    | Structured

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
        /// The append member instantiated at the hole's value type. The handle's signature
        /// matches the walker's push order: the value, then the member's own operands.
        AppendGeneric: GenericAppend * FrozenType -> EntityHandle
        /// Alignment is always passed (0 ⇒ no padding).
        AppendBool: EntityHandle
        /// `%o`: signature `(value: int64, alignment: int)`. The walker widens the
        /// argument to the 64-bit two's complement of its own-width bits.
        AppendOctal: EntityHandle
        /// `%u`: signature `(value: uint64, alignment: int)`. The walker widens the
        /// argument to its own-width bits, reinterpreted unsigned.
        AppendUnsigned: EntityHandle
        /// `%08o`: zero-padded two's-complement octal, signature `(value: int64, width: int)`.
        /// A dedicated handler because .NET has no octal format that zero-pads to a total width.
        AppendZeroPaddedOctal: EntityHandle
        /// `%05u`: zero-padded unsigned decimal. Signature `(value: uint64, width: int)`.
        /// Overflowing digits are not truncated, matching F#.
        AppendZeroPaddedUnsigned: EntityHandle
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

/// The BCL handles and heap strings a synthesised structural body (`Equals`,
/// `GetHashCode`, `CompareTo`, `Format`) calls. Each accessor mints its metadata row on
/// demand; resolve a handle once and share it across bodies.
type IStructuralHandles =
    /// `EqualityComparer<T>.Default` getter.
    abstract EqualityComparerDefault: elem: FrozenType -> EntityHandle
    /// `EqualityComparer<T>::Equals(T, T) : bool`.
    abstract EqualityComparerEquals: elem: FrozenType -> EntityHandle
    /// The `System.HashCode` accumulator local a hash body adds into.
    abstract HashCodeType: FrozenType
    /// `HashCode::Add<T>(T)`.
    abstract HashCodeAdd: elem: FrozenType -> EntityHandle
    /// `HashCode::ToHashCode() : int32`.
    abstract HashCodeToHashCode: EntityHandle
    /// `Comparer<T>.Default` getter.
    abstract ComparerDefault: elem: FrozenType -> EntityHandle
    /// `Comparer<T>::Compare(T, T) : int32`.
    abstract ComparerCompare: elem: FrozenType -> EntityHandle
    /// `System.ArgumentException::.ctor(string)`, which an `object`-typed `CompareTo`
    /// throws on an argument of another type.
    abstract ArgumentExceptionCtor: EntityHandle
    /// The `Vesper.IFormatSink` members `Format` `callvirt`s.
    abstract FormatSink: FormatSinkHandles
    /// The `box` target for a field type.
    abstract BoxToken: elem: FrozenType -> EntityHandle
    /// A `#US` heap string, for a case label or an exception message.
    abstract UserString: string -> UserStringHandle

/// Where a class key's metadata token comes from.
[<RequireQualifiedAccess>]
type ClassOrigin =
    /// The `TypeDef` this compilation emits, including one declared by another file of the
    /// same assembly.
    | Local of EntityHandle
    /// The referenced-assembly `TypeRef` for a class declared outside this compilation.
    | Foreign of EntityHandle
    /// The key does not resolve to a class in either domain.
    | Unresolved

/// Resolves compiled names to emission recipes for one target.
type ICodegenProvider =
    /// `fnTy` is the applied function's full curried type: a multi-typar call can't recover its type args
    /// from the result alone (`List.fold` reads `'T`/`'State` from the folder). `ValueSome` only
    /// where a referenced package publishes a signature for `key`.
    abstract TryEmitCall: key: BindingKey * fnTy: FrozenType -> CallRecipe voption

    /// `chosen` is the front-end-resolved `.ctor`'s `SymbolKey.MemberKey` when a `TExpr.New`
    /// recorded it, selecting that exact same-arity overload by identity; `ValueNone` falls
    /// back to the first arity match. `tyArgs` instantiate the constructed type.
    abstract TryEmitCtor:
        key: TypeKey * chosen: SymbolKey voption * tyArgs: FrozenType list * argTypes: FrozenType list ->
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
        key: TypeKey * tyArgs: FrozenType list * fieldName: string -> (EntityHandle * FrozenType) voption

    /// The test a match arm emits against a *referenced-package* union's scrutinee for
    /// `caseName`, instantiated at `tyArgs`. `ValueNone` ⇒ unknown union or case.
    abstract ExternalUnionCaseTest: key: TypeKey * tyArgs: FrozenType list * caseName: string -> UnionCaseTest voption

    /// The read path a `match … Some x` arm takes to field `fieldIndex` of `caseName` on a
    /// referenced-package union instantiated at `tyArgs`, plus the field's substituted
    /// declared type. `ValueNone` ⇒ unknown union/case/field.
    abstract ExternalUnionCaseField:
        key: TypeKey * tyArgs: FrozenType list * caseName: string * fieldIndex: int ->
            (UnionCaseAccess * FrozenType) voption

    /// The type token for one case of a referenced-package HIERARCHY union at `tyArgs`: the
    /// nested type its payload is declared on, which a cross-package `match` arm casts the
    /// scrutinee to. `ValueNone` ⇒ a flat regime, where the payload sits on the union.
    abstract ExternalUnionCaseType: key: TypeKey * tyArgs: FrozenType list * caseName: string -> EntityHandle voption

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

    /// `true` when the member's DECLARED codomain is `unit`, so its call must declare 0
    /// results. Read from the declaration, not the use site: `M: 'a -> 'a` at `'a = unit`
    /// still returns `!0`.
    abstract ExternalMemberReturnsVoid: key: SymbolKey -> bool

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
    abstract ExternalParameterlessBaseCtor: key: TypeKey -> EntityHandle voption

    abstract ClassOrigin: key: TypeKey -> ClassOrigin

    /// Resolve an intrinsic-CLASS `inherit` parent to its platform external key
    /// (`System.Exception`) plus its raw `TypeRef`, the derived type's `extends` token.
    /// Such a parent is an `FTConst` canon (`exn`) whose platform type id is a heritable
    /// BCL reference class.
    abstract IntrinsicClassBase: canon: TypeKey -> struct (TypeKey * EntityHandle) voption

    abstract ObjectType: EntityHandle

    /// A `TypeDefOrRefOrSpec` token for an arbitrary `FrozenType`, for the operand
    /// of `isinst` / `castclass` / `box` / `unbox.any`.
    abstract TypeToken: ty: FrozenType -> EntityHandle

    /// The resolved `System.ValueTuple`n` handles for an N-tuple over `elemTys`.
    abstract ValueTupleRefs: elemTys: FrozenType list -> ValueTupleHandles

    /// How a *referenced-assembly / referenced-package* nominal type is laid out: the metadata
    /// layer's `Type.IsValueType` first, then the `[<Struct>]` the shape's declaration carries.
    /// `Unsettled` where the referenced set has no fact, so a caller holding its own
    /// declarations can take over. It reads the store the front end typed against, so both
    /// ends classify a type identically.
    abstract ExternalLayout: key: TypeKey -> TypeLayout

    /// The RAW target facts, unmerged with any declaration.
    abstract Platform: IPlatformFacts voption

    /// `System.Decimal::.ctor(int32, int32, int32, bool, uint8)` — emits a
    /// `decimal` constant the way F# / Roslyn do, from `Decimal.GetBits`.
    abstract DecimalCtor: EntityHandle

    /// `System.Exception::.ctor(string)` — the fallthrough a non-exhaustive `match` throws.
    abstract ExceptionCtor: EntityHandle
