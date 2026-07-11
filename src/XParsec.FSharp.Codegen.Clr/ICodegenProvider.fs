namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

// The codegen (target) provider — mirror of `IExternalSymbolProvider`. The
// symbol provider knows a compiled name's *shape*; the codegen provider knows
// how to *emit a call* to it.

/// A call's argument arity — how many application-spine elements it consumes and
/// how many CLR values that flattens to. The two diverge only for a module function
/// carrying a captured SOURCE grouping (Step C): a tupled group is one spine element
/// but N pushed values, a lone `()` group one spine element but zero. Making the two
/// counts a single typed value keeps the `FlatArgCount` (the stack-model pop count)
/// and the spine split provably consistent, instead of a flat `ArgCount` plus a
/// parallel optional `Groups` the reader must reconcile.
[<RequireQualifiedAccess>]
type CallArity =
    /// Spine count = flat pop count: every leading spine element pushes one value
    /// (all-`GSimple` module functions and every non-module-function recipe — an
    /// instance call's count includes the receiver).
    | Flat of argCount: int
    /// The callee's SOURCE grouping drives the split: the walker consumes
    /// `groups.Length` spine elements and flattens each to its pushed CLR values
    /// (`CompiledFns.flattenPlan`); `flatArgCount` is the resulting flat pop count.
    | Grouped of groups: Frozen.ArgGroup list * flatArgCount: int

    /// The number of CLR values the `call` actually pops — what the IlIr stack model
    /// adjusts by (`Pushes - FlatArgCount`). The spine-element count is this for
    /// `Flat`, but `groups.Length` for `Grouped`.
    member this.FlatArgCount =
        match this with
        | Flat n -> n
        | Grouped(_, n) -> n

/// How to emit a resolved call once its arguments are on the stack. `Emit`
/// performs the call itself — a `call` / `callvirt` against a metadata handle,
/// or a bare intrinsic opcode like `add` (which has no handle). The walker
/// adjusts depth by `Pushes - Arity.FlatArgCount`.
type CallRecipe =
    {
        Emit: Il -> unit
        Arity: CallArity
        Pushes: int
    }

type CtorRecipe = { Handle: EntityHandle; ArgCount: int }

/// The resolved CLR handles for one `System.ValueTuple` instantiation: the
/// instantiated parent `TypeSpec`, its `.ctor`, and the public `Item…` field
/// refs in element order. Construction reads `Ctor`; destructuring reads
/// `ItemFields`; the type encoder needs only the `TypeSpec` shape, which
/// `encodeType` builds itself.
///
/// Arity 2–7 is the flat `ValueTuple`n` — `ItemFields` holds all `Item1…Itemn`,
/// `Rest` is `ValueNone`. Arity ≥ 8 is the standard .NET nesting `ValueTuple`8<t0
/// …t6, TRest>`: `ItemFields` holds only `Item1…Item7`, `Ctor` takes 8 args (7
/// elements + the nested `TRest`), and `Rest` carries the `Rest` field plus the
/// `Nested` handles for chasing element indices ≥ 7.
type ValueTupleHandles =
    {
        TypeSpec: EntityHandle
        Ctor: EntityHandle
        ItemFields: EntityHandle[]
        Rest: ValueTupleRest voption
    }

/// The `ValueTuple`8` `TRest` link: the `Rest` field ref (typed as the 8th
/// generic parameter) and the handles of the nested residual tuple it stores.
and ValueTupleRest =
    {
        RestField: EntityHandle
        Nested: ValueTupleHandles
    }

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

/// Which member of an emitted *generic* closure a `GenericClosureMemberRef`
/// resolves to. A generic closure is a real generic `TypeDefinition` (one
/// `<closure>$n` per enclosing-method specialisation point); every reference to
/// one of its members — construction site, capture-field load inside the
/// closure's own `Invoke` — must go through a `MemberRef` on the `TypeSpec` of
/// the closure instantiated with the use-site's arguments (`<closure>$n<int>`
/// externally, `<closure>$n<!0>` from inside the closure's own `Invoke`). The
/// member-ref *signature* is written in terms of the closure's own generic
/// parameters (`!i`), with the instantiation riding the parent `TypeSpec`.
/// Monomorphic closures keep using their `Def` tokens directly.
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
/// resolves to. A class is shaped
/// like a record at the metadata level — one ctor taking the primary-ctor
/// parameters in declaration order, one backing field per ctor parameter
/// (keyed by source name) — plus the augmentation `members` (instance / static
/// method/property surface unions already carry). A
/// monomorphic class skips this entirely (its `Def` tokens suffice).
[<RequireQualifiedAccess>]
type ClassMember =
    /// The primary `instance void .ctor(p0, p1, …)` — parameter types are
    /// the ctor params in declaration order.
    | Ctor
    /// A secondary `instance void .ctor(p0, …)` selected by its parameter
    /// signature — F# forbids two ctors of the same signature, so `paramTys`
    /// (the ctor params in declaration order, written in the type's declaring-typar
    /// markers) keys the overload. Distinct from `Ctor` because the param types are
    /// the ctor's own, not the type's fields.
    | SecondaryCtor of paramTys: FrozenType list
    /// The backing field named `fieldName` for a primary-ctor parameter.
    | Field of fieldName: string

/// Discriminator across the user-emitted generic-type-member families
/// (`UserGenericMemberRef`). The three family variants wrap the family's specific
/// member info, preserving the case data (a record's field name, a union
/// case's payload index, …) that a pure ordinal couldn't carry.
/// `ClrProvider.userGenericMemberRef`'s dispatch grows one arm — no fourth
/// `Abstract` sibling on `ICodegenProvider`.
[<RequireQualifiedAccess>]
type UserMemberKind =
    | UnionMember of UnionMember
    | RecordMember of RecordMember
    | ClassMember of ClassMember
    /// An augmentation member (`get_X` instance property, `M` instance method, or static
    /// counterpart) of any emitted generic type — union, record, class, or interface. NOT a
    /// per-family case: a member is described the same way whatever declares it (unlike a
    /// ctor / field / case payload, whose shape IS the family), and the declaring `key`
    /// already selects the parent `TypeSpec`, so a family tag here would be redundant data
    /// the caller has to invent — which is what drove `EmitMember` to label an *interface*
    /// slot a `ClassMember`.
    ///
    /// `metaName` is the emitted method name (a property is `get_<name>`); the signature
    /// (`paramTys` / `retTy`) is in the type's declaring-typar markers, written into the
    /// member ref as `!0` with the parent `TypeSpec` supplying the instantiation.
    /// `methodTyparCount` > 0 ⇒ the member is itself a *generic method* (`member s.Map<'U>
    /// …`): its own typars ride `!!i`, the member-ref carries the `GENERIC`
    /// calling-convention header, and the call site wraps the ref in a `MethodSpec`.
    | Member of
        metaName: string *
        isStatic: bool *
        methodTyparCount: int *
        paramTys: FrozenType list *
        retTy: FrozenType

/// Resolved metadata handles for lowering a `TExpr.Format` to the write-through
/// handler (`Vesper.Formatter`). A `Format` can't be a `CallRecipe` — it
/// interleaves literals and lazily-evaluated args around a ref-struct local — so
/// the walker owns the call *sequence* and the provider supplies only the handles.
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
        /// `%08o`: zero-padded two's-complement octal. Signature `(value: int, width: int)`
        /// — .NET has no octal format that zero-pads to a total width.
        AppendZeroPaddedOctal: EntityHandle
        /// `%05u`: zero-padded unsigned decimal. Signature `(value: uint, width: int)`
        /// — overflowing digits are not truncated, matching F#.
        AppendZeroPaddedUnsigned: EntityHandle
        /// `%0w.pf`: zero-pad after the sign — .NET has no float format that does this.
        AppendZeroPaddedFloat: EntityHandle
        /// `%-0w.pf`: zero-pad on the RIGHT (left-align + zero-pad float) — no .NET
        /// float format nor field alignment fills the right with zeros.
        AppendRightZeroPaddedFloat: EntityHandle
        /// `%+0w.pf`/`% 0w.pf`: forced sign, then zero-pad after it to a total field.
        /// Signature `(value: float, format: string, width: int, space: bool)` — the
        /// `"F<prec>"` body rounds half-to-even, and the sign is composed in-handler
        /// (a section format could do neither faithfully).
        AppendForcedSignZeroPaddedFloat: EntityHandle
        /// `%.*f`/`%*.*f`/`%.*e`/`%.*g`: runtime precision. Signature
        /// `(value: float, typeChar: char, precision: int, alignment: int)` — builds
        /// the .NET format string in-handler from `typeChar` + `precision`.
        AppendDynamicPrecisionFloat: EntityHandle
        /// `%+.*f`/`% .*f`/`%+*.*f`: runtime-precision forced-sign float. Signature
        /// `(value: float, typeChar: char, precision: int, alignment: int, space: bool)`
        /// — composes the sign in-handler (the section-format lowering is compile-time).
        AppendDynamicPrecisionSignedFloat: EntityHandle
        /// `%A`: instantiates the generic `AppendStructured<T = ty>` (like
        /// `AppendFormatted`) for the structural-format engine. Signature
        /// `(value: T, width: int, size: int)` — the print-width budget and the
        /// print-size (`PrintSize`) budget.
        AppendStructured: FrozenType -> EntityHandle
        /// `%*d`/`%-*d` runtime width guard, `static int32 GuardTotalWidth(int32)` —
        /// throws `ArgumentOutOfRangeException("totalWidth")` on a negative width
        /// (F# `PadLeft` parity), identity otherwise. The star-width lowering spills
        /// the guarded (then, for `-`, negated) result to a local before the value.
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
/// body `callvirt`s. One handle per layout primitive plus the semantic record/case
/// ops; the `Format` body drives the semantic protocol (`BeginRecord; (Field; Child)×n;
/// EndRecord` / `BeginCase; Child×k; EndCase`) exactly as the hand-written `Sem*` impls
/// do (`StructuralFormatTests.fs`). All are `instance void` on the `IFormatSink`
/// interface — the sink builds a `Doc` tree and lays it out, so the synthesised IL
/// stays straight-line.
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
        /// `void BeginRecord()` — open a synthesised record frame.
        BeginRecord: EntityHandle
        /// `void Field(string)` — a record-field label marker; the value follows via `Child`.
        Field: EntityHandle
        /// `void EndRecord()` — close the record frame.
        EndRecord: EntityHandle
        /// `void BeginCase(string)` — open a synthesised union-case frame.
        BeginCase: EntityHandle
        /// `void EndCase()` — close the union-case frame.
        EndCase: EntityHandle
        /// `void Child(object)` — the sole child entry for synthesised bodies (records + cases).
        Child: EntityHandle
    }

/// Resolves compiled names to emission recipes for one target (the .NET
/// implementation is `ClrProvider`). Intentionally minimal: the contract
/// crystallises from the working implementation rather than up-front design.
type ICodegenProvider =
    /// `fnTy` is the head's full declared (curried) type — passed whole because a
    /// multi-typar generic call can't recover its type arguments from the
    /// application's result alone (`printfn` reads the printer = result of `fnTy`;
    /// `List.fold` reads `'T` / `'State` from the folder parameter). `key` is
    /// the resolved `SymbolKey.ValueKey` Elaborate stamped onto the head
    /// (`Resolution.ExternalValue`); the provider dispatches by *identity*
    /// when available — e.g. only the canonical `Vesper.Printf.printfn`
    /// trips the cold-printf recipe, never a project-local
    /// `MyMod.printfn`. Unkeyed call
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
    /// `args`.
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
    /// `scrut._tag` and compares it against this value; the union emitter (`NominalEmit.fs`) fixes both the field name and
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
    /// call site. `handle` is the method's (predicted) `MethodDefinition`;
    /// `instTypes` the per-typar instantiation recovered by matching the method's
    /// declared parameter types against the call's actual argument types. A
    /// recursive self-call passes the method's own typars (encoded `!!i` via the
    /// ambient set); an external call passes concrete types.
    abstract StaticFnMethodSpec: handle: EntityHandle * instTypes: FrozenType list -> EntityHandle

    /// Recover the declaring- and method-axis type arguments by structurally
    /// matching an *open* signature (carrying `FTTypar(Declaring,i)` / `FTTypar(Method,i)`
    /// markers) against its *instantiated* counterpart. Returns
    /// `(declaringArgs, methodArgs)`. Used by the project-local generic-instance-method
    /// call site to recover the `MethodSpec` arguments from the call's actual
    /// argument/result types (the same primitive the external member-ref path uses
    /// to recover a declaring instantiation).
    abstract RecoverOpenTypars:
        declArity: int * methodArity: int * openT: FrozenType * instT: FrozenType -> FrozenType list * FrozenType list

    /// Apply a function *value* of type `funcTy` to one argument —
    /// `Vesper.Fun\`2::Invoke`. Receiver and argument are both already on the
    /// stack (receiver beneath), so the recipe's `ArgCount` is 2.
    abstract TryEmitInvoke: funcTy: FrozenType -> CallRecipe voption

    /// `EqualityComparer<'T>.Default` getter and its `GetHashCode(!0)` — the
    /// `hash x` use-site's BCL body (no IL opcode hashes, so it rides the comparer,
    /// the same `EqualityComparer<T>` family the DU triple hashes fields through).
    /// On the interface because the expression walker emits the `hash` call (and the
    /// string/mixed enum `| E.A` pattern emits the field-equality `Equals`);
    /// `Add` stays on the concrete provider, reached only from Codegen.
    abstract EqualityComparerDefault: elem: FrozenType -> EntityHandle
    abstract EqualityComparerGetHashCode: elem: FrozenType -> EntityHandle
    /// `EqualityComparer<'T>.Default.Equals(!0, !0)` — the boxing-free typed field
    /// compare the structural-equality triple uses, and the string/mixed enum
    /// pattern's per-case field equality.
    abstract EqualityComparerEquals: elem: FrozenType -> EntityHandle

    /// Mint a `MemberRef` for a `TExpr.ExternalMember` from its interned
    /// `SymbolKey`. The key
    /// pins the declaring type + member (assembly/namespace/name + member name +
    /// overload `argSig`); `memberTy` is the access's *instantiated* type (a
    /// property's type, or a method's curried `arg → … → ret`), from which the
    /// declaring type's instantiation is recovered by matching it against the
    /// member's open signature. `isProperty` selects the `get_<name>` getter shape,
    /// `isStatic` the (non-)`this` signature. The walker pushes the receiver/args
    /// and emits the `call` (static) / `callvirt` (instance) around the handle.
    abstract ExternalMemberRef:
        key: SymbolKey * isProperty: bool * isStatic: bool * memberTy: FrozenType -> EntityHandle

    /// Rebase a capability member call's declaring type onto its true BCL declarer when
    /// that declarer is a *base* of the reconciled platform face. A capability member
    /// (`enumerator<'T>.MoveNext()`) is keyed by its canonical capability
    /// (`Vesper.Collections.enumerator`), which reconciles to a platform face
    /// (`IEnumerator`1`) — but `MoveNext` is inherited from the non-generic
    /// `System.Collections.IEnumerator` base and a member-ref against the face itself
    /// faults at runtime (`MissingMethodException`). Returns the key rebased onto the base
    /// declarer, or `ValueNone` when no rebase is needed (not a capability, or the member
    /// is declared on the face). The `for … in` lowering hardcodes the same declarers; this
    /// gives the manual-call path (`e.MoveNext()`) the same declaring-type awareness.
    abstract TryCapabilityBaseMemberKey: key: SymbolKey -> SymbolKey voption

    /// Like `ExternalMemberRef`, but the declaring type's instantiation is given
    /// explicitly via `declTy` (the resolved declaring `TyClass`, e.g.
    /// `List`1+Enumerator<int>`) instead of recovered from the member's open
    /// signature. Required for a T-free member like `MoveNext(): bool` on a generic
    /// enumerator, whose signature mentions no typar so the instantiation is
    /// unrecoverable. The parent is encoded straight
    /// from `declTy`, so a struct declaring type lands as a `VALUETYPE` parent.
    abstract ExternalMemberRefOn:
        key: SymbolKey * declTy: FrozenType * isProperty: bool * isStatic: bool * memberTy: FrozenType -> EntityHandle

    /// Mint a field `MemberRef` for a genuine external public field (`String.Empty`,
    /// `ValueTuple`2<_,_>.Item1`), read via `ldfld`/`ldsfld` rather than a `get_<name>`
    /// accessor. `declTy` is the receiver's resolved type for an instance field (it pins
    /// the declaring instantiation) and `ValueNone` for a static field (whose declaring
    /// args are recovered from the open field type vs the use-site `memberTy`).
    abstract ExternalFieldRef: key: SymbolKey * declTy: FrozenType voption * memberTy: FrozenType -> EntityHandle

    abstract FormatHandles: unit -> FormatHandles

    /// Lives on the provider because encoding a `FrozenType` needs the target's
    /// type references.
    abstract EncodeLocalSignature: locals: FrozenType list -> StandaloneSignatureHandle

    /// `MemberRef` for the parameterless `.ctor()` of a HERITABLE external base
    /// class (`type X = (# class "System.Attribute" #)`, resolved to its external
    /// `TyClass`) — the chain target a derived class's primary `.ctor` calls instead
    /// of `System.Object::.ctor`. Minted directly off the external `TypeRef` (a
    /// `protected` base ctor need not be in the member harvest). `ValueNone` ⇒ `key`
    /// did not resolve to an external class.
    abstract ExternalParameterlessBaseCtor: key: SymbolKey -> EntityHandle voption

    /// The raw external `TypeRef` for `key` (a heritable external base class), the
    /// token a derived type's `extends` (base-type) column names. `ValueNone` ⇒ `key`
    /// did not resolve to an external class.
    abstract ExternalClassTypeRef: key: SymbolKey -> EntityHandle voption

    /// Resolve an intrinsic-CLASS `inherit` parent — an `FTConst` canon (`exn`)
    /// whose platform repr is a heritable BCL reference class — to its platform
    /// external key (`System.Exception`, the identity `TryEmitCtor` /
    /// `ExternalParameterlessBaseCtor` mint base-ctor `MemberRef`s against) plus
    /// its raw `TypeRef` (the derived type's `extends` token). `ValueNone` ⇒ not an
    /// intrinsic, or a value-type repr (never a heritable base).
    abstract IntrinsicClassBase: canon: SymbolKey -> struct (SymbolKey * EntityHandle) voption

    abstract ObjectType: EntityHandle

    /// A `TypeDefOrRefOrSpec` token for an arbitrary `FrozenType`, for the operand
    /// of `isinst` / `castclass` / `box` / `unbox.any`. One `TypeSpec`-based path
    /// covers mono, generic, and external targets alike.
    abstract TypeToken: ty: FrozenType -> EntityHandle

    /// The resolved `System.ValueTuple`n` handles for an N-tuple over `elemTys`:
    /// the instantiated `TypeSpec`, its `.ctor`, and
    /// the `Item1…Itemn` field refs. Construction (`newobj` the ctor) and
    /// destructuring (`ldfld` the `Item` fields) read the same source of truth.
    abstract ValueTupleRefs: elemTys: FrozenType list -> ValueTupleHandles

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
