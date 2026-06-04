namespace XParsec.FSharp.SemanticAnalysis

// TAST does NOT preserve trivia, parens, or token layout — tooling consumers
// query the CST for that. TAST exists for consumers that only care about
// semantics (codegen, target plugins).
//
// Each node carries its inferred SemType inline so target lowering doesn't
// need to re-query the side tables.
//
// The TAST term/declaration cluster is parameterized over its type field
// (`'ty`): `TExprG<'ty>` etc. (no-op parameterization). Today every consumer
// instantiates it at `SemType` through
// the central aliases at the bottom of this file (`type TExpr = TExprG<SemType>`,
// …), so this is a pure additive change — the bare names mean exactly what they
// meant before. The cutover then makes `freeze` produce `TExprG<FrozenType>`
// (the single SemType→FrozenType rebuild point) and reads it in codegen, without
// re-touching every annotation.

[<RequireQualifiedAccess>]
type TConstValue =
    | Int of int
    | Int64 of int64
    | Byte of byte
    | Float of double
    | Float32 of single
    | Bool of bool
    | Char of char
    | Decimal of decimal
    | String of string
    | Unit

/// Keeps the destructuring shape so a downstream consumer can introduce every
/// bound name without re-walking the CST.
[<RequireQualifiedAccess>]
type TPatG<'ty> =
    /// `binding` is the source-position NodeKey of the introducing pattern;
    /// references via `TExpr.Var` use the same key.
    | NamedSimple of binding: NodeKey * ty: 'ty
    /// `_` placeholder. Has a type (the matched value's type) but binds nothing.
    | Wildcard of ty: 'ty
    /// `ty` is always a `TyTuple` of the elements' types.
    | Tuple of items: EqArray<TPatG<'ty>> * ty: 'ty
    | Const of value: TConstValue * ty: 'ty
    /// `ty` is a `TyRecord`. May list a subset of the record's fields; unlisted
    /// fields are simply not bound.
    | Record of fields: EqArray<string * TPatG<'ty>> * ty: 'ty
    /// `fields` is the per-field sub-pattern list, empty for nullary cases. `ty`
    /// is always a `TyUnion`. The declaring union is recoverable via
    /// `ctx.Types.CtorIndex[caseName]` at consumption time.
    | Union of caseName: string * fields: EqArray<TPatG<'ty>> * ty: 'ty

/// `Ty` is the static type (drives `AppendFormatted<T>`, no box). `Alignment` is
/// the field width (negative ⇒ left-justify). `Kind`/`Format`/`Alignment` are
/// produced by `PrintfSpec.tryHoleFormat`.
///
/// Lifted out of the `TExpr` `and`-cluster (P2.13) — references only
/// `SemType`/`PrintfSpec.HoleKind`, so it doesn't need mutual recursion.
type HoleSpecG<'ty> =
    {
        Ty: 'ty
        Kind: PrintfSpec.HoleKind
        Format: string option
        Alignment: int option
    }

/// How an instance member access dispatches (inheritance-plan §Subtle
/// migrations). `Self` is the normal virtual dispatch (`callvirt`); `Base`
/// is a `base.M(...)` / `base.X` access, which must target the *parent's*
/// method slot non-virtually (`call`) so an `override` doesn't recurse into
/// itself. Set by Freeze when the receiver's head binding site is a class's
/// `BaseKey`; read by codegen to pick the call opcode.
[<RequireQualifiedAccess>]
type CallVia =
    | Self
    | Base

[<RequireQualifiedAccess>]
type TExprG<'ty> =
    | Const of value: TConstValue * ty: 'ty
    /// `binding` is the NodeKey of the *binding site*, not the use site.
    | Var of binding: NodeKey * ty: 'ty
    /// Symbol resolved through IExternalSymbolProvider. Carries the compiled
    /// name so target plugins can dispatch (`op_Addition` -> CIL `add` on
    /// .NET, native `+` on Rust, etc. — see [[project_inline_il_target_specific]]).
    /// `key` interns the resolved `SymbolKey` so codegen reads the binding off the
    /// node instead of re-resolving by name;
    /// `ValueNone` until Freeze stamps it (P3) — every site is name-only today.
    | External of compiledName: string * key: SymbolKey voption * ty: 'ty
    | Lambda of param: TPatG<'ty> * body: TExprG<'ty> * ty: 'ty
    | App of fn: TExprG<'ty> * arg: TExprG<'ty> * ty: 'ty
    | Let of binding: TPatG<'ty> * value: TExprG<'ty> * body: TExprG<'ty> * ty: 'ty
    /// `use x = value in body` (B-5). Same shape as `Let`; the distinction is that
    /// codegen wraps `body` in a `try … finally x.Dispose()` exception region so
    /// `x` is disposed on every exit. `ty` is the body's type — the expression's
    /// result. `dispose` selects the disposal path (vesper-set-sprint-phase-4 §4.3):
    /// `ValueNone` lowers a direct `x.Dispose()` call on the binder (the duck-typed
    /// path for *user* types — no `IDisposable` upcast, §4.1); `ValueSome key`
    /// disposes an *external* (BCL) binder through the keyed `Dispose` member that
    /// the front-end resolved (its declared `Dispose`, or `System.IDisposable`'s
    /// when the type implements it), emitted as an `ExternalMemberRef` `callvirt`.
    | Use of binding: TPatG<'ty> * value: TExprG<'ty> * body: TExprG<'ty> * dispose: SymbolKey voption * ty: 'ty
    | IfThenElse of cond: TExprG<'ty> * thenExpr: TExprG<'ty> * elseExpr: TExprG<'ty> * ty: 'ty
    /// `ty` is always a TyTuple of the elements' inferred types.
    | Tuple of items: EqArray<TExprG<'ty>> * ty: 'ty
    /// All items but the last must have unit type; `ty` is the last item's type.
    | Sequential of items: EqArray<TExprG<'ty>> * ty: 'ty
    /// `ty` is always unit; cond : bool, body : unit.
    | While of cond: TExprG<'ty> * body: TExprG<'ty> * ty: 'ty
    /// `ty` is always unit; the loop variable is bound to `var` with type int.
    /// `startExpr`, `endExpr`, `body` are int, int, unit respectively.
    | ForTo of var: NodeKey * startExpr: TExprG<'ty> * endExpr: TExprG<'ty> * body: TExprG<'ty> * ty: 'ty
    /// `ty` is always unit. `pat`'s type matches the element type of `source`
    /// — pinned to `int` for range sources, left as a free TypeVar otherwise.
    /// `body` types as unit. `enumerator` records how the source yields its
    /// enumerator (the front-end resolution): `Interface` lowers through the
    /// `IEnumerable<'T>` interface slots (§4.2, the range form too); `DuckTyped`
    /// carries a pattern-based struct/class `GetEnumerator()` (§4.4 — codegen
    /// emission deferred). Codegen can't re-derive this from the element type.
    | ForIn of pat: TPatG<'ty> * source: TExprG<'ty> * body: TExprG<'ty> * enumerator: ForInEnumeratorG<'ty> * ty: 'ty
    /// `scrutinee` and each `arms.[i].Pat` share the same type; every
    /// `arms.[i].Body` shares `ty`. `function` desugars to a Match over a
    /// synthetic parameter — same TExpr shape.
    | Match of scrutinee: TExprG<'ty> * arms: EqArray<TMatchArmG<'ty>> * ty: 'ty
    /// `try body with | pat -> arm`. `body` and every `arms.[i].Body`
    /// share `ty`; arm patterns currently bind against a fresh TypeVar
    /// (no `exn` type yet).
    | TryWith of body: TExprG<'ty> * arms: EqArray<TMatchArmG<'ty>> * ty: 'ty
    /// `try body finally cleanup`. `body` carries `ty`; `cleanup` is unit.
    | TryFinally of body: TExprG<'ty> * cleanup: TExprG<'ty> * ty: 'ty
    /// `lhs <- rhs`. Always types as unit.
    | Assignment of lhs: TExprG<'ty> * rhs: TExprG<'ty> * ty: 'ty
    /// `null` literal. `ty` is left as a free TypeVar in the tiny subset —
    /// real F# would constrain it to a reference type.
    | Null of ty: 'ty
    /// `start..stop` or `start..step..stop`. Endpoints (and step) all type
    /// as int in the tiny subset; `ty` is `seq<int>` (a TyConst placeholder
    /// — see [[BuiltinTypes.tySeqInt]]).
    | Range of startExpr: TExprG<'ty> * step: TExprG<'ty> option * stopExpr: TExprG<'ty> * ty: 'ty
    /// `{ X = e1; Y = e2 }` record literal. `ty` is a `TyRecord`; field
    /// list is in source order (the unification pass already validated
    /// that the field set matches the record's declared set).
    | RecordCons of fields: EqArray<string * TExprG<'ty>> * ty: 'ty
    /// `{ r with X = v; … }`. `source` types as the same `TyRecord` as
    /// `ty`; `overrides` is the source-order list of `(name, replacement)`
    /// for the listed fields. Unlisted fields are copied from `source` at
    /// the runtime level — not represented in the TAST.
    | RecordClone of source: TExprG<'ty> * overrides: EqArray<string * TExprG<'ty>> * ty: 'ty
    /// `r.X` — `ty` is the field's declared type. `receiver` types as a
    /// `TyRecord`.
    | FieldGet of receiver: TExprG<'ty> * fieldName: string * ty: 'ty
    /// `r.X <- v` — `ty` is unit. `receiver` types as a `TyRecord` whose
    /// field `fieldName` is mutable (Validation enforces).
    | FieldSet of receiver: TExprG<'ty> * fieldName: string * value: TExprG<'ty> * ty: 'ty
    /// Discriminated-union constructor application. `args` length matches
    /// the ctor's declared arity (0 for nullary). `ty` is a `TyUnion`.
    /// Nullary ctors (`Point`) and applied ctors (`Circle 1.0`,
    /// `Rectangle(2.0, 3.0)`) both fold to this node — the latter peels
    /// the `Expr.App` chain in Freeze.
    | UnionCons of caseName: string * args: EqArray<TExprG<'ty>> * ty: 'ty
    /// Class primary-constructor invocation. `args` is the per-parameter
    /// list — the parser's tuple wrapper (`new Point(3, 4)` parses with
    /// a `Tuple` arg) is peeled in Freeze so consumers see the ctor's
    /// declared arity directly. `ty` is a `TyClass`.
    | New of className: string * args: EqArray<TExprG<'ty>> * ty: 'ty
    /// Instance method invocation: `r.M(args)`. `args` is the
    /// per-parameter list (peeled the same way as `New`). `ty` is the
    /// method's declared return type. `key` is the resolved local
    /// `SymbolKey.MemberKey` (declaring type + member name) — codegen reads the declaring type off `key.decl` and the member
    /// name off `key.memberName` instead of re-deriving from a class-name string.
    | MethodCall of receiver: TExprG<'ty> * key: SymbolKey * via: CallVia * args: EqArray<TExprG<'ty>> * ty: 'ty
    | PropertyGet of receiver: TExprG<'ty> * key: SymbolKey * via: CallVia * ty: 'ty
    /// Same arg-peeling as `MethodCall`; no receiver. `key` is the resolved local
    /// `SymbolKey.MemberKey`.
    | StaticMethodCall of key: SymbolKey * args: EqArray<TExprG<'ty>> * ty: 'ty
    | StaticPropertyGet of key: SymbolKey * ty: 'ty
    /// Read of a class-level `static let` backing field (vesper-set-sprint-plan
    /// §1.8 / B-10). Lowered from a `static let`-bound name reference in a member
    /// body (Freeze rewrites the resolved `Var` exactly as a primary-ctor param
    /// becomes a `FieldGet`). Codegen emits `ldsfld` against the class's private
    /// static field — there is no method call (a static *property* would be a
    /// `StaticPropertyGet`). `ty` is the field's declared/inferred type. `declKey`
    /// is the declaring class's `SymbolKey.TypeKey` (NOT a `MemberKey` — a backing
    /// field is a field, resolved through the class's `StaticFields`, and
    /// `MemberKind` has no `Field` case).
    | StaticFieldGet of declKey: SymbolKey * fieldName: string * ty: 'ty
    /// Member access on an *external* type resolved through `IExternalSymbolProvider`
    /// `key` interns the resolved `SymbolKey` so
    /// codegen (P4) mints the ref off the node's identity instead of re-resolving by
    /// name — the external analogue of `TExpr.Var`'s `BindingSite`. `receiver` is
    /// `ValueNone` for a static member (`EqualityComparer<int>.Default`) and
    /// `ValueSome` for an instance member (`…Default.GetHashCode`). `isProperty`
    /// distinguishes a property get from a method value/group. `ty` is the access's
    /// result type — the property's type, or the method's *curried* function type
    /// (a `… GetHashCode 5` lands as `App(ExternalMember(…, ty = int -> int), 5)`).
    | ExternalMember of receiver: TExprG<'ty> voption * key: SymbolKey * memberName: string * isProperty: bool * ty: 'ty
    /// Lowered printf / string-interpolation (vesper-printf-plan P1, D9):
    /// `segments` is the interleaved literal / hole sequence in source order,
    /// each hole carrying its argument expression inline (codegen folds left to
    /// right, evaluating each arg at its hole). NOT a generic saturated call —
    /// the format literal rewrites the call's arity/arg-types, so the node carries
    /// printf semantics a generic call node cannot. `ty` is the call's result
    /// (`unit` for `printf`/`printfn`, `string` for `sprintf`).
    | Format of sink: FormatSinkG<'ty> * segments: EqArray<FormatSegG<'ty>> * ty: 'ty
    /// Value-level inline IL: `(# "opcode" args : retTy #)`. `opCode` is the
    /// stitched instruction mnemonic (e.g. `"ceq"`, `"add"`), `args` the operand
    /// expressions in source order, `ty` the declared result type. Codegen emits
    /// each arg then maps the mnemonic → `ILOpCode` (`Cil.tryOpCodeOfMnemonic`).
    /// The value-level sibling of the type-level `(# "..." #)` intrinsic carried
    /// in `TastFile.IntrinsicReprTypes`; operator `.fs` bodies (`(=)` → `ceq`,
    /// `(+)` → `add`, …) lower to this so codegen owns no per-operator dispatch.
    /// See docs/operators-plan.md.
    | ILIntrinsic of opCode: string * args: EqArray<TExprG<'ty>> * ty: 'ty
    /// F# library-only static optimization: a default expression plus a list of
    /// type-specialized clauses (`expr when ^T : int = … when ^T : ^T = …`).
    /// `clauses` are in source order; at `let inline` expansion the first clause
    /// whose constraints hold for the monomorphised type arguments is selected,
    /// else `defaultExpr`. Every clause body and `defaultExpr` share `ty` (an
    /// equality-family operator returns `bool` under every clause). Codegen does
    /// **not** emit this node directly — `Inline.inlineExpand` resolves it to the
    /// chosen branch once the call site pins the operand type (prereq 3). See
    /// docs/operators-plan.md.
    | StaticOptimization of clauses: EqArray<TStaticOptClauseG<'ty>> * defaultExpr: TExprG<'ty> * ty: 'ty
    /// `e :> T` static upcast (inheritance-plan §`:>`). `source`'s runtime type
    /// is a subtype of `ty` (validated by Unification's `subsumes`). Codegen
    /// erases it for ref types (the JIT treats a derived reference as the base)
    /// and emits `box` for a value-type source.
    | Upcast of source: TExprG<'ty> * ty: 'ty
    /// `e :?> T` checked downcast. `ty` is the (more-specific) target type;
    /// codegen emits `castclass` for ref types / `unbox.any` for value types,
    /// so a runtime mismatch throws `InvalidCastException`.
    | Downcast of source: TExprG<'ty> * ty: 'ty
    /// `e :? T` type test. `testTy` is the tested-against type `T` (the
    /// `isinst` operand); `ty` is always `TyConst "bool"` (the result). Codegen
    /// emits `isinst <testTy>; ldnull; cgt.un`.
    | TypeTest of source: TExprG<'ty> * testTy: 'ty * ty: 'ty

and TMatchArmG<'ty> =
    {
        Pat: TPatG<'ty>
        Guard: TExprG<'ty> option
        Body: TExprG<'ty>
    }

/// Kept abstract from CLR specifics so an alternate target (JS → template
/// literal) maps it independently. `ToWriter`/`ToBuilder` carry the explicit
/// sink expression (`fprintf` / `bprintf`); P1 produces only the first three.
and [<RequireQualifiedAccess>] FormatSinkG<'ty> =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of TExprG<'ty>
    | ToBuilder of TExprG<'ty>
    | ToString

and [<RequireQualifiedAccess>] FormatSegG<'ty> =
    | Lit of string
    | Hole of HoleSpecG<'ty> * TExprG<'ty>

/// One clause of a `TExpr.StaticOptimization`. `Constraints` is the `and`-joined
/// list (all must hold; declared in `SemanticInfo.fs` so the side table can carry
/// it); `Body` is the clause's optimized expression.
and TStaticOptClauseG<'ty> =
    {
        Constraints: EqArray<TStaticOptConstraint>
        Body: TExprG<'ty>
    }

[<RequireQualifiedAccess>]
type TDeclG<'ty> =
    /// The `value` body is retained verbatim regardless; when `isInline` is set
    /// the flag tells codegen it may expand the body per call site (via
    /// `Inline.inlineExpand`) rather than emit a single callable. See
    /// [front-end-gaps-plan](docs/front-end-gaps-plan.md) §C.
    | Let of binding: TPatG<'ty> * value: TExprG<'ty> * isInline: bool * ty: 'ty
    | Expression of expr: TExprG<'ty> * ty: 'ty
    /// Emits only the interface shape; records / unions / classes came later.
    | Type of TTypeDeclG<'ty>

and TTypeDeclG<'ty> =
    {
        /// Simple (unqualified) type name, e.g. `"Fun"`. The metadata name gets
        /// the arity suffix (`` Fun`2 ``) from `TypeParams.Length`.
        Name: string
        /// The type's stable nominal identity:
        /// the registry `info.Key` (`TypeKey(Some homeAsm, declNs, name\`arity)`),
        /// carried into the backend so the emitted-type tables key off it directly
        /// instead of re-deriving a string. Codegen branches local-vs-external on
        /// its home `asm` (= the assembly being emitted).
        Key: SymbolKey
        /// `None` for a module-level type.
        Namespace: string option
        /// Declared type parameters in source order (e.g. `["'A"; "'B"]`).
        TypeParams: EqArray<string>
        Kind: TTypeKindG<'ty>
        /// Equality posture for this type (records / unions / interfaces).
        /// Defaults to `Structural` — interfaces ignore it (no triple is ever
        /// synthesised), records / unions consume it in the codegen loops. See
        /// [`docs/records-plan.md`](docs/records-plan.md) §B4.
        EqualitySupport: EqualityVerdict
        /// Comparison posture for this type (records / unions / interfaces).
        /// Defaults to `NoComparison` — interfaces ignore it (no pair is ever
        /// synthesised), records / unions consume it in the codegen loops to
        /// decide whether to emit the `IComparable<Self>` / `IComparable`
        /// `InterfaceImpl`s and the `CompareTo(Self)` / `CompareTo(object)`
        /// pair. Per brainstorm-comparison §9 the default is **opt-in**, so an
        /// unannotated record / union skips the pair. See
        /// [`docs/records-plan.md`](docs/records-plan.md) §B6.
        ComparisonSupport: ComparisonVerdict
    }

and [<RequireQualifiedAccess>] TTypeKindG<'ty> =
    /// A nominal type whose members are all abstract and which has no base type /
    /// field.
    | Interface of methods: EqArray<TAbstractMethodG<'ty>>
    /// `cases` in declaration order (the index is the runtime tag), plus any
    /// augmentation members (`with member …` / `static member …`).
    | Union of cases: EqArray<TUnionCaseG<'ty>> * members: EqArray<TTypeMemberG<'ty>>
    /// `fields` are the record's payload in declaration order, paired with their
    /// declared types and mutability. `members` carries augmentation members
    /// (`with member …` / `static member …`) — empty for v1, where records carry
    /// only their field shape. See docs/records-plan.md §B1.
    | Record of fields: EqArray<TRecordFieldG<'ty>> * members: EqArray<TTypeMemberG<'ty>>
    /// Class type emission (vesper-set-sprint-plan Phase 1 / B-1).
    /// `fields` are mutable instance fields — empty in B-1 (the classes-plan v1
    /// cut); `ctorParams` borrows the `TRecordField` shape for the primary
    /// constructor's parameter list (name / type / mutability=false).
    /// `members` carries every instance / static method / property (the
    /// instance-vs-static split is the member's own `IsStatic`).
    /// `baseType` is `ValueNone` in B-1 (codegen defaults the IL
    /// `TypeDefinition.BaseType` to `Object`); Phase 2 (B-4) fills it from
    /// `ClassTypeInfo.BaseType`. `interfaces` is empty in B-1; Phase 5 (B-2)
    /// fills it from the interface-impl registry — each entry pairs the
    /// resolved interface type (a `TyClass`, remapped onto this class's typar
    /// markers so a generic interface arg like `IEnumerable<'T>` encodes against
    /// the declaring type's generic parameters) with its already-typed member
    /// bodies. Codegen emits one `InterfaceImpl` row per entry and one
    /// `MethodDefinition` per member (implicit impl — bound by name + signature;
    /// explicit `.override` rows are deferred with the `MethodImpl` table).
    /// `isSealed` reflects `[<Sealed>]` (B-8): when `true`, codegen flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition` — derivation
    /// is rejected at use sites (Phase 2's `subsumes` already excludes
    /// `Sealed`).
    /// `staticLets` are class-level `static let` bindings (B-10): codegen emits
    /// one private static field each and a synthesised `.cctor` running the
    /// initialisers in declaration order. Empty unless the class has `static let`s.
    /// `secondaryCtors` are `new(args) = SelfType(primaryArgs)` overloads (B-11):
    /// codegen emits each as a `.ctor` overload whose body runs the let-preamble
    /// then chains to the primary `.ctor`. Empty unless the class declares any.
    /// `baseCtorCall` is the `inherit Base(args)` invocation (Phase 2 / B-4 Step
    /// 2.5): codegen makes the primary `.ctor` chain to the parent's `.ctor` with
    /// these args before storing fields. `ValueNone` for a parent-less class (the
    /// primary `.ctor` then chains to `System.Object::.ctor`). Always present
    /// together with a `ValueSome baseType`.
    | Class of
        fields: EqArray<TRecordFieldG<'ty>> *
        ctorParams: EqArray<TRecordFieldG<'ty>> *
        members: EqArray<TTypeMemberG<'ty>> *
        baseType: 'ty voption *
        interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty>>> *
        isSealed: bool *
        staticLets: EqArray<TStaticLetG<'ty>> *
        secondaryCtors: EqArray<TSecondaryCtorG<'ty>> *
        baseCtorCall: TBaseCtorCallG<'ty> voption

/// `Fields` are the case's payload in declaration order; a field's name is
/// `ValueNone` when the source is positional (`Cons of 'T * list`). Empty
/// `Fields` ⇒ a nullary case (`Nil`).
and TUnionCaseG<'ty> =
    {
        Name: string
        Fields: EqArray<string voption * 'ty>
    }

/// One field of a `TTypeKind.Record`. `Type` carries the field's declared
/// type — with the declaring type's typar markers (`TyConst "'T"`) for a
/// generic record, exactly like `TUnionCase.Fields`. `IsMutable` is the
/// source-level `mutable` annotation; downstream consumers (the equality
/// triple's "all-immutable record" gate, C-Attr) read it from here rather
/// than re-querying `ctx.Types.Record`.
and TRecordFieldG<'ty> =
    {
        Name: string
        Type: 'ty
        IsMutable: bool
    }

and [<RequireQualifiedAccess>] TMemberKind =
    /// Invoked through `TExpr.MethodCall` / `TExpr.StaticMethodCall`.
    | Method
    /// A parameterless getter, read through `TExpr.PropertyGet` /
    /// `TExpr.StaticPropertyGet`. Emitted as a `get_<Name>` method (no
    /// `PropertyDefinition` row yet — see P3d.3).
    | Property

/// An instance member's body sees `this` (its `ThisKey`, resolved to `ldarg.0`)
/// and its parameters; a static member's body sees only its parameters.
and TTypeMemberG<'ty> =
    {
        Name: string
        IsStatic: bool
        Kind: TMemberKind
        /// Instance members only; `ValueNone` for a static member.
        ThisKey: NodeKey voption
        /// The synthetic `base` binder of the declaring class (inheritance-plan
        /// §Subtle migrations), shared across every member body. A `base.M(...)`
        /// receiver is a `TExpr.Var(BaseKey, parentTy)`; codegen maps it to the
        /// same `ldarg.0` as `this`, so this key is loaded identically — the
        /// `CallVia.Base` discriminator (not the receiver) drives non-virtual
        /// dispatch. `ValueNone` for a static member, a union member, or a class
        /// without an `inherit` clause.
        BaseKey: NodeKey voption
        /// The declaring type (a `TyUnion`) — the receiver type for an instance
        /// member's `this`.
        ThisTy: 'ty
        /// Parameter binders in declaration order (each `ldarg` after `this` for
        /// an instance method); empty for a property or a nullary method.
        Params: EqArray<NodeKey * 'ty>
        Body: TExprG<'ty>
        ReturnTy: 'ty
        /// The member's *own* generic parameters (`member this.Map<'C> …`,
        /// vesper-set-sprint-plan §1.10 / B-12) — distinct from the declaring
        /// type's `TTypeDecl.TypeParams`. Each entry pairs the source name
        /// (`"'C"`, for the `GenericParam` row) with the post-unification
        /// union-find *root* `TypeVar`. `Freeze.remapMemberTypes` uses these roots to
        /// flip the method axis in `Params` / `ReturnTy` / `Body` to
        /// `TyTypar(Method, i)`, exactly as the declaring
        /// type's typars ride `TyTypar(Declaring, i)`; codegen's encoder resolves
        /// both axes by index (`!!i` / `!i`) with no ambient window. This list still
        /// feeds the `GenericParam` rows and the `GENERIC` header arity. Empty for a
        /// non-generic member.
        MethodTypeParams: EqArray<string * TypeVar>
    }

/// A class-level `static let x = <init>` (vesper-set-sprint-plan §1.8 / B-10).
/// Codegen emits one private static field per entry and concatenates the
/// `Init` expressions into a synthesised `.cctor`; a `static let`-bound name
/// referenced in a member body lowers to `TExpr.StaticFieldGet`. Per-instantiation
/// generic-static-let is deferred (the front-end rejects `static let` on a generic
/// class), so `Init` is always emitted in a monomorphic class context.
and TStaticLetG<'ty> =
    {
        Name: string
        Type: 'ty
        Init: TExprG<'ty>
    }

/// One `let`-preamble binding inside a secondary constructor body
/// (`new(args) = let x = e in SelfType(...)`). `Binder` is the local's
/// `NodeKey` (codegen allocates a local slot and a body reference to the name
/// loads it); `Init` is the right-hand side. Only simple (single-name) binders
/// are modelled in v1.
and TCtorLetG<'ty> =
    {
        Binder: NodeKey
        Type: 'ty
        Init: TExprG<'ty>
    }

/// A secondary constructor (vesper-set-sprint-plan §1.9 / B-11). Codegen emits a
/// `.ctor` overload: `Params` are the overload's parameters (`ldarg` after
/// `this`); `Lets` run as locals in declaration order; then the body chains to
/// the primary `.ctor` with `PrimaryArgs` (`ldarg.0; <args>; call instance void
/// SelfType::.ctor`). There is no usable `this` before the chain call, so the
/// `Lets` / `PrimaryArgs` only reference the ctor params and earlier lets.
and TSecondaryCtorG<'ty> =
    {
        Params: EqArray<NodeKey * 'ty>
        Lets: EqArray<TCtorLetG<'ty>>
        PrimaryArgs: EqArray<TExprG<'ty>>
    }

/// An `inherit Base(args)` base-constructor invocation (vesper-set-sprint-plan
/// Phase 2 / B-4 Step 2.5). Codegen wires the primary `.ctor` to chain to the
/// parent's `.ctor`: `ldarg.0; <Args>; call instance void Base::.ctor(…)` before
/// storing the derived class's own fields. `CtorParams` are the *derived* class's
/// primary-ctor parameters (the `ldarg` mapping the base-ctor `Args` reference —
/// `this` isn't constructed yet, so an arg can only name a primary-ctor param or
/// a `static let`). The parent type itself rides the `Class` kind's `baseType`
/// slot, which also supplies the IL `TypeDefinition.BaseType`.
and TBaseCtorCallG<'ty> =
    {
        CtorParams: EqArray<NodeKey * 'ty>
        Args: EqArray<TExprG<'ty>>
    }

/// `Signature` is the curried function type; a type parameter of the *declaring
/// type* is carried as `TyConst "'A"` (a name marker the backend resolves to a
/// `GenericTypeParameter` index).
///
/// `MethodTypeParams` are the method's *own* generic parameters in source order
/// (e.g. `["'C"]` for `abstract Map<'C> : 'A -> 'C`), distinct from the declaring
/// type's `TTypeDecl.TypeParams`. They are also carried as `TyConst "'C"` markers
/// in `Signature`; the backend maps them to `GenericMethodParameter` indices (vs
/// the declaring type's `GenericTypeParameter`). Empty ⇒ a non-generic method.
and TAbstractMethodG<'ty> =
    {
        Name: string
        MethodTypeParams: EqArray<string>
        Signature: 'ty
    }

type TastFileG<'ty> =
    {
        /// Source order.
        Decls: EqArray<TDeclG<'ty>>
        /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
        Diagnostics: Diagnostic list
        /// Vesper type name → target IL representation string (e.g. `"int"` →
        /// `"System.Int32"`), from this file's `type x = (# "..." #)` intrinsic
        /// abbrevs. A use site resolves to `TyConst name`; the backend keys the
        /// emitted IL type off the *representation string* (so a platform author
        /// retargets a primitive by editing one `.fs` line). The backend overlays
        /// these on its built-in defaults.
        IntrinsicReprTypes: Map<string, string>
        /// A module-level binding's `NodeKey.Raw` → its named-holder placement
        /// (`module Foo`'s functions emit on a real `Foo`/`FooModule` static class,
        /// not the anonymous "Program" holder). Empty for a program with no named
        /// modules — every static method then lands on "Program" as before.
        ModuleMembers: Map<uint64, ModuleMemberInfo>
    }

// ---------------------------------------------------------------------------
// Central monomorphic SemType aliases (no-op parameterization over the type field).
// Every consumer today speaks `SemType`; these aliases let the bare TAST names
// continue to mean exactly that, so parameterizing the cluster above is a pure
// additive change ([[feedback_additive_changes_with_aliases]]). The
// cutover introduces a parallel `FrozenType` instantiation (`freeze : TExprG<SemType>
// -> TExprG<FrozenType>`) without re-touching every annotation here.
// ---------------------------------------------------------------------------

type TPat = TPatG<SemType>
type HoleSpec = HoleSpecG<SemType>
type TExpr = TExprG<SemType>
type TMatchArm = TMatchArmG<SemType>
type FormatSink = FormatSinkG<SemType>
type FormatSeg = FormatSegG<SemType>
type TStaticOptClause = TStaticOptClauseG<SemType>
type TDecl = TDeclG<SemType>
type TTypeDecl = TTypeDeclG<SemType>
type TTypeKind = TTypeKindG<SemType>
type TUnionCase = TUnionCaseG<SemType>
type TRecordField = TRecordFieldG<SemType>
type TTypeMember = TTypeMemberG<SemType>
type TStaticLet = TStaticLetG<SemType>
type TCtorLet = TCtorLetG<SemType>
type TSecondaryCtor = TSecondaryCtorG<SemType>
type TBaseCtorCall = TBaseCtorCallG<SemType>
type TAbstractMethod = TAbstractMethodG<SemType>
type TastFile = TastFileG<SemType>

// ---------------------------------------------------------------------------
// Parallel frozen aliases. The `SemType → FrozenType`
// freeze (the final pipeline step; `Pipeline.analyse`'s output) and codegen speak
// these. The bare names above STAY `SemType` (inference, the SemType-domain passes
// `Regions` / `RefCellPromotion` / `ResolvedTypes`, tests, any non-codegen API).
// `SemType` becomes codegen-irrelevant, not gone.
// ---------------------------------------------------------------------------

module Frozen =
    type TPat = TPatG<FrozenType>
    type HoleSpec = HoleSpecG<FrozenType>
    type TExpr = TExprG<FrozenType>
    type TMatchArm = TMatchArmG<FrozenType>
    type FormatSink = FormatSinkG<FrozenType>
    type FormatSeg = FormatSegG<FrozenType>
    type TStaticOptClause = TStaticOptClauseG<FrozenType>
    type TDecl = TDeclG<FrozenType>
    type TTypeDecl = TTypeDeclG<FrozenType>
    type TTypeKind = TTypeKindG<FrozenType>
    type TUnionCase = TUnionCaseG<FrozenType>
    type TRecordField = TRecordFieldG<FrozenType>
    type TTypeMember = TTypeMemberG<FrozenType>
    type TStaticLet = TStaticLetG<FrozenType>
    type TCtorLet = TCtorLetG<FrozenType>
    type TSecondaryCtor = TSecondaryCtorG<FrozenType>
    type TBaseCtorCall = TBaseCtorCallG<FrozenType>
    type TAbstractMethod = TAbstractMethodG<FrozenType>
    type TastFile = TastFileG<FrozenType>
    type ForInEnumerator = ForInEnumeratorG<FrozenType>
