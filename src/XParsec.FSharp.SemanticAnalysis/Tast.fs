namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// TAST does NOT preserve trivia, parens, or token layout — tooling consumers
// query the CST for that. TAST exists for consumers that only care about
// semantics (codegen, target plugins).
//
// Each node carries its inferred SemType inline so target lowering doesn't
// need to re-query the side tables.
//
// The TAST term/declaration cluster is parameterized over its type field
// (`'ty`): `TExprG<'ty, 'tok>` etc. (no-op parameterization). Today every consumer
// instantiates it at `SemType` through
// the central aliases at the bottom of this file (`type TExpr = TExprG<SemType>`,
// …), so this is a pure additive change — the bare names mean exactly what they
// meant before. The cutover then makes `freeze` produce `TExprG<FrozenType>`
// (the single SemType→FrozenType rebuild point) and reads it in codegen, without
// re-touching every annotation.

[<RequireQualifiedAccess>]
type TConstValue =
    | Int of int
    | UInt of uint32
    | Int64 of int64
    | Byte of byte
    | Float of double
    | Float32 of single
    | Bool of bool
    | Char of char
    | Decimal of decimal
    | String of string
    | Unit

/// Compiler-recognised attributes on a single function/`inline` parameter,
/// decoded (by short name) from the parameter's CST `[<…>]` sets in
/// `Passes.Attributes`. This is an extension point: adding a new special
/// parameter attribute (F# declares many — `[<InlineIfLambda>]`,
/// `[<CallerMemberName>]`, …) is a new flag here, a decoder arm in
/// `Passes.Attributes`, and a consumer where it is honoured. All-`false` is the
/// default for an un-attributed parameter. Today only `let inline` parameters
/// carry these through to a consumer (`Passes.InlineExpansion`); the carrier is
/// `ctx.InlineParamAttrs` (local inlines) and `ExternalSymbols.InlineBody`
/// (cross-package), positionally aligned to the inline's curried parameters.
[<Struct>]
type ParamAttrs =
    {
        /// `[<CallAtMostOnce>]`: splice the argument unevaluated at its single
        /// linear use (call-by-name for one use) instead of eager `let`-binding,
        /// so it is evaluated at most once — the mechanism behind `&&`/`||`
        /// short-circuiting without the operator being known to the compiler.
        /// Linearity (≤1 use, not under a lambda or loop) is validated at the
        /// declaration in `Elaborate`; the inliner trusts the flag.
        CallAtMostOnce: bool
    }

    static member Default = { CallAtMostOnce = false }

    /// True when any recognised attribute is set — the gate for storing a
    /// parameter's attrs in the (otherwise sparse) carriers.
    member this.IsDefault = not this.CallAtMostOnce

/// Keeps the destructuring shape so a downstream consumer can introduce every
/// bound name without re-walking the CST.
[<RequireQualifiedAccess>]
type TPatG<'ty, 'tok> =
    /// `binding` is the source-position NodeKey of the introducing pattern;
    /// references via `TExpr.Var` use the same key.
    | NamedSimple of binding: NodeKey * ty: 'ty * tok: 'tok
    /// `_` placeholder. Has a type (the matched value's type) but binds nothing.
    | Wildcard of ty: 'ty * tok: 'tok
    /// `ty` is always a `TyTuple` of the elements' types.
    | Tuple of items: EqArray<TPatG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    | Const of value: TConstValue * ty: 'ty * tok: 'tok
    /// `ty` is a `TyRecord`. May list a subset of the record's fields; unlisted
    /// fields are simply not bound.
    | Record of fields: EqArray<string * TPatG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// `fields` is the per-field sub-pattern list, empty for nullary cases. `ty`
    /// is always a `TyUnion`. The declaring union is recoverable via
    /// `ctx.Types.CtorIndex[caseName]` at consumption time.
    | Union of caseName: string * fields: EqArray<TPatG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// `:? testTy as x` type-test pattern. Refutable: codegen lowers it to an
    /// `isinst testTy` + null check (branch to the next arm on mismatch), then
    /// binds `inner` (the `as`-name, an irrefutable sub-pattern) against the
    /// cast-down value. `ty` is the scrutinee's type (the matched value — `obj`
    /// in practice); `testTy` is the tested-against type the binder sees.
    | TypeTestAs of testTy: 'ty * inner: TPatG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `null` literal pattern (`match x with null -> …`). Refutable, binds
    /// nothing: codegen lowers it to a `ldloc; brtrue nextLabel` (a non-null
    /// scrutinee skips the arm). `ty` is the scrutinee's (reference) type.
    | Null of ty: 'ty * tok: 'tok

/// A format hole's classified per-value formatting (printf-shared-core-plan.md
/// step (b)). A hole no longer stores the `(Kind, .NET-format, alignment)` triple
/// `PrintfSpec.tryHoleFormat` produced — it carries the *classified*, target-neutral
/// `HoleForm`, and the CLR triple (a runtime artifact) is projected on demand by
/// `Codegen.Clr.ClrHoleFormat.toDotNetFormat`. Two origins:
/// - `Classified` — the classification the lowering gate (`FreezeExpr`) already
///   computed via `PrintfHoleForm.tryClassify` to decide whether the hole lowers at
///   all. Covers every printf specifier (`%d`, `%A`, `%08.2f`, …) and every
///   printf-style `%d{x}` interpolation hole. Both backends read `HoleForm`
///   directly — no re-classification, and the gate's `ValueNone` (defer) verdict
///   never reaches here, so no consumer needs an unreachable fallback arm.
/// - `RawFormat` — an interpolation `{x:fmt}` custom-format clause (`{x:X}`,
///   `{x:N2}`): a raw .NET format string (or `None`) with no printf placeholder.
///   A CLR dialect, faithful only on the CLR backend, carried verbatim.
///
/// `PrintfHoleForm` compiles before this file, so the classified `HoleForm` is
/// stored here directly rather than re-derived per consumer.
[<RequireQualifiedAccess>]
type HoleSpecSource =
    | Classified of PrintfHoleForm.HoleForm
    | RawFormat of fmt: string option

/// `Ty` is the static type (drives `AppendFormatted<T>`, no box). `Source` is the
/// hole's classified formatting (printf classification or interpolation format
/// clause); `Tok` is the specifier's source token for source maps / PDBs
/// ([[project_tast_tok_migration]] — `FormatPlaceholder` itself carries no
/// position).
///
/// The legacy `(Kind, Format, Alignment)` projection that once lived here as
/// transitional members was retired in printf-shared-core-plan.md step (d): both
/// backends read `Source`'s `HoleForm` directly (the CLR backend then projects
/// `Field` holes to its .NET-format triple via
/// `Codegen.Clr.ClrHoleFormat.toDotNetFormat`).
///
/// Lifted out of the `TExpr` `and`-cluster (P2.13) — references only
/// `SemType`/`PrintfHoleForm`, so it doesn't need mutual recursion.
type HoleSpecG<'ty, 'tok> =
    {
        Ty: 'ty
        Source: HoleSpecSource
        Tok: 'tok
    }

/// How an instance member access dispatches. `Self` is the normal virtual
/// dispatch (`callvirt`); `Base` is a `base.M(...)` / `base.X` access, which
/// must target the *parent's* method slot non-virtually (`call`) so an
/// `override` doesn't recurse into itself. Set by Freeze when the receiver's
/// head binding site is a class's `BaseKey`; read by codegen to pick the call opcode.
[<RequireQualifiedAccess>]
type CallVia =
    | Self
    | Base

[<RequireQualifiedAccess>]
type TExprG<'ty, 'tok> =
    | Const of value: TConstValue * ty: 'ty * tok: 'tok
    /// `binding` is the NodeKey of the *binding site*, not the use site.
    | Var of binding: NodeKey * ty: 'ty * tok: 'tok
    /// Symbol resolved through IExternalSymbolProvider. Carries the compiled
    /// name so target plugins can dispatch (`op_Addition` -> CIL `add` on
    /// .NET, native `+` on Rust, etc. — see [[project_inline_il_target_specific]]).
    /// `key` interns the resolved `SymbolKey` so codegen reads the binding off the
    /// node instead of re-resolving by name;
    /// `ValueNone` until Freeze stamps it (P3) — every site is name-only today.
    | External of compiledName: string * key: SymbolKey voption * ty: 'ty * tok: 'tok
    | Lambda of param: TPatG<'ty, 'tok> * body: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    | App of fn: TExprG<'ty, 'tok> * arg: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    | Let of binding: TPatG<'ty, 'tok> * value: TExprG<'ty, 'tok> * body: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `use x = value in body` (B-5). Same shape as `Let`; the distinction is that
    /// codegen wraps `body` in a `try … finally x.Dispose()` exception region so
    /// `x` is disposed on every exit. `ty` is the body's type — the expression's
    /// result. `dispose` selects the disposal path:
    /// `ValueNone` lowers a direct `x.Dispose()` call on the binder (the duck-typed
    /// path for *user* types — no `IDisposable` upcast, §4.1); `ValueSome key`
    /// disposes an *external* (BCL) binder through the keyed `Dispose` member that
    /// the front-end resolved (its declared `Dispose`, or `System.IDisposable`'s
    /// when the type implements it), emitted as an `ExternalMemberRef` `callvirt`.
    | Use of
        binding: TPatG<'ty, 'tok> *
        value: TExprG<'ty, 'tok> *
        body: TExprG<'ty, 'tok> *
        dispose: SymbolKey voption *
        ty: 'ty *
        tok: 'tok
    | IfThenElse of
        cond: TExprG<'ty, 'tok> *
        thenExpr: TExprG<'ty, 'tok> *
        elseExpr: TExprG<'ty, 'tok> *
        ty: 'ty *
        tok: 'tok
    /// `ty` is always a TyTuple of the elements' inferred types.
    | Tuple of items: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// All items but the last must have unit type; `ty` is the last item's type.
    | Sequential of items: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// `ty` is always unit; cond : bool, body : unit.
    | While of cond: TExprG<'ty, 'tok> * body: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `ty` is always unit; the loop variable is bound to `var` with type int.
    /// `startExpr`, `endExpr`, `body` are int, int, unit respectively.
    | ForTo of
        var: NodeKey *
        startExpr: TExprG<'ty, 'tok> *
        endExpr: TExprG<'ty, 'tok> *
        body: TExprG<'ty, 'tok> *
        ty: 'ty *
        tok: 'tok
    /// `ty` is always unit. `pat`'s type matches the element type of `source`
    /// — pinned to `int` for range sources, left as a free TypeVar otherwise.
    /// `body` types as unit. `enumerator` records how the source yields its
    /// enumerator (the front-end resolution): `Interface` lowers through the
    /// `IEnumerable<'T>` interface slots (§4.2, the range form too); `Pattern`
    /// carries a pattern-based struct/class `GetEnumerator()` (§4.4 / Gap 2-3).
    /// Codegen can't re-derive this from the element type.
    | ForIn of
        pat: TPatG<'ty, 'tok> *
        source: TExprG<'ty, 'tok> *
        body: TExprG<'ty, 'tok> *
        enumerator: ForInEnumeratorG<'ty> *
        ty: 'ty *
        tok: 'tok
    /// `scrutinee` and each `arms.[i].Pat` share the same type; every
    /// `arms.[i].Body` shares `ty`. `function` desugars to a Match over a
    /// synthetic parameter — same TExpr shape.
    | Match of scrutinee: TExprG<'ty, 'tok> * arms: EqArray<TMatchArmG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// `try body with | pat -> arm`. `body` and every `arms.[i].Body`
    /// share `ty`; arm patterns currently bind against a fresh TypeVar
    /// (no `exn` type yet).
    | TryWith of body: TExprG<'ty, 'tok> * arms: EqArray<TMatchArmG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// `try body finally cleanup`. `body` carries `ty`; `cleanup` is unit.
    | TryFinally of body: TExprG<'ty, 'tok> * cleanup: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `lhs <- rhs`. Always types as unit.
    | Assignment of lhs: TExprG<'ty, 'tok> * rhs: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `null` literal. `ty` is left as a free TypeVar in the tiny subset —
    /// real F# would constrain it to a reference type.
    | Null of ty: 'ty * tok: 'tok
    /// `start..stop` or `start..step..stop`. Endpoints (and step) all type
    /// as int in the tiny subset; `ty` is `seq<int>` (a TyConst placeholder
    /// — see [[BuiltinTypes.tySeqInt]]).
    | Range of
        startExpr: TExprG<'ty, 'tok> *
        step: TExprG<'ty, 'tok> option *
        stopExpr: TExprG<'ty, 'tok> *
        ty: 'ty *
        tok: 'tok
    /// `{ X = e1; Y = e2 }` record literal. `ty` is a `TyRecord`; field
    /// list is in source order (the unification pass already validated
    /// that the field set matches the record's declared set).
    | RecordCons of fields: EqArray<string * TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// `{ r with X = v; … }`. `source` types as the same `TyRecord` as
    /// `ty`; `overrides` is the source-order list of `(name, replacement)`
    /// for the listed fields. Unlisted fields are copied from `source` at
    /// the runtime level — not represented in the TAST.
    | RecordClone of source: TExprG<'ty, 'tok> * overrides: EqArray<string * TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// `r.X` — `ty` is the field's declared type. `receiver` types as a
    /// `TyRecord`.
    | FieldGet of receiver: TExprG<'ty, 'tok> * fieldName: string * ty: 'ty * tok: 'tok
    /// `r.X <- v` — `ty` is unit. `receiver` types as a `TyRecord` whose
    /// field `fieldName` is mutable (Validation enforces).
    | FieldSet of receiver: TExprG<'ty, 'tok> * fieldName: string * value: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// Discriminated-union constructor application. `args` length matches
    /// the ctor's declared arity (0 for nullary). `ty` is a `TyUnion`.
    /// Nullary ctors (`Point`) and applied ctors (`Circle 1.0`,
    /// `Rectangle(2.0, 3.0)`) both fold to this node — the latter peels
    /// the `Expr.App` chain in Freeze.
    | UnionCons of caseName: string * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// Class primary-constructor invocation. `args` is the per-parameter
    /// list — the parser's tuple wrapper (`new Point(3, 4)` parses with
    /// a `Tuple` arg) is peeled in Freeze so consumers see the ctor's
    /// declared arity directly. `ty` is a `TyClass`.
    | New of className: string * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// Instance method invocation: `r.M(args)`. `args` is the
    /// per-parameter list (peeled the same way as `New`). `ty` is the
    /// method's declared return type. `key` is the resolved local
    /// `SymbolKey.MemberKey` (declaring type + member name) — codegen reads the declaring type off `key.decl` and the member
    /// name off `key.memberName` instead of re-deriving from a class-name string.
    | MethodCall of
        receiver: TExprG<'ty, 'tok> *
        key: SymbolKey *
        via: CallVia *
        args: EqArray<TExprG<'ty, 'tok>> *
        ty: 'ty *
        tok: 'tok
    | PropertyGet of receiver: TExprG<'ty, 'tok> * key: SymbolKey * via: CallVia * ty: 'ty * tok: 'tok
    /// Same arg-peeling as `MethodCall`; no receiver. `key` is the resolved local
    /// `SymbolKey.MemberKey`.
    | StaticMethodCall of key: SymbolKey * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    | StaticPropertyGet of key: SymbolKey * ty: 'ty * tok: 'tok
    /// Read of a class-level `static let` backing field (B-10). Lowered from a `static let`-bound name reference in a member
    /// body (Freeze rewrites the resolved `Var` exactly as a primary-ctor param
    /// becomes a `FieldGet`). Codegen emits `ldsfld` against the class's private
    /// static field — there is no method call (a static *property* would be a
    /// `StaticPropertyGet`). `ty` is the field's declared/inferred type. `declKey`
    /// is the declaring class's `SymbolKey.TypeKey` (NOT a `MemberKey` — a backing
    /// field is a field, resolved through the class's `StaticFields`, and
    /// `MemberKind` has no `Field` case).
    | StaticFieldGet of declKey: SymbolKey * fieldName: string * ty: 'ty * tok: 'tok
    /// Member access on an *external* type resolved through `IExternalSymbolProvider`
    /// `key` interns the resolved `SymbolKey` so
    /// codegen (P4) mints the ref off the node's identity instead of re-resolving by
    /// name — the external analogue of `TExpr.Var`'s `BindingSite`. `receiver` is
    /// `ValueNone` for a static member (`EqualityComparer<int>.Default`) and
    /// `ValueSome` for an instance member (`…Default.GetHashCode`). `isProperty`
    /// distinguishes a property get from a method value/group. `ty` is the access's
    /// result type — the property's type, or the method's *curried* function type
    /// (a `… GetHashCode 5` lands as `App(ExternalMember(…, ty = int -> int), 5)`).
    | ExternalMember of
        receiver: TExprG<'ty, 'tok> voption *
        key: SymbolKey *
        memberName: string *
        isProperty: bool *
        ty: 'ty *
        tok: 'tok
    /// Lowered printf / string-interpolation:
    /// `segments` is the interleaved literal / hole sequence in source order,
    /// each hole carrying its argument expression inline (codegen folds left to
    /// right, evaluating each arg at its hole). NOT a generic saturated call —
    /// the format literal rewrites the call's arity/arg-types, so the node carries
    /// printf semantics a generic call node cannot. `ty` is the call's result
    /// (`unit` for `printf`/`printfn`, `string` for `sprintf`).
    | Format of sink: FormatSinkG<'ty, 'tok> * segments: EqArray<FormatSegG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// Value-level inline IL: `(# "opcode" args : retTy #)`. `opCode` is the
    /// stitched instruction mnemonic (e.g. `"ceq"`, `"add"`), `args` the operand
    /// expressions in source order, `ty` the declared result type. Codegen emits
    /// each arg then maps the mnemonic → `ILOpCode` (`Cil.tryOpCodeOfMnemonic`).
    /// The value-level sibling of the type-level `(# "..." #)` intrinsic carried
    /// in `TastFile.IntrinsicReprTypes`; operator `.fs` bodies (`(=)` → `ceq`,
    /// `(+)` → `add`, …) lower to this so codegen owns no per-operator dispatch.
    ///
    /// `typeOperand` carries the single type token a tokenful array opcode needs
    /// (`newarr`/`ldelem` → the element type); `ValueNone` for the balanced
    /// stack ops that take no operand (the operator surface, and `ldlen`). The
    /// array forms are synthesised by Freeze from `arr.[i]` / `Array.zeroCreate`
    /// rather than written as `(# … #)` in source — F# treats array access as an
    /// IL intrinsic, so codegen owns one emission path for all three.
    | ILIntrinsic of opCode: string * typeOperand: 'ty voption * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// F# library-only static optimization: a default expression plus a list of
    /// type-specialized clauses (`expr when ^T : int = … when ^T : ^T = …`).
    /// `clauses` are in source order; at `let inline` expansion the first clause
    /// whose constraints hold for the monomorphised type arguments is selected,
    /// else `defaultExpr`. Every clause body and `defaultExpr` share `ty` (an
    /// equality-family operator returns `bool` under every clause). Codegen does
    /// **not** emit this node directly — `Inline.inlineExpand` resolves it to the
    /// chosen branch once the call site pins the operand type (prereq 3).
    | StaticOptimization of
        clauses: EqArray<TStaticOptClauseG<'ty, 'tok>> *
        defaultExpr: TExprG<'ty, 'tok> *
        ty: 'ty *
        tok: 'tok
    /// `e :> T` static upcast (inheritance-plan §`:>`). `source`'s runtime type
    /// is a subtype of `ty` (validated by Unification's `subsumes`). Codegen
    /// erases it for ref types (the JIT treats a derived reference as the base)
    /// and emits `box` for a value-type source.
    | Upcast of source: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `e :?> T` checked downcast. `ty` is the (more-specific) target type;
    /// codegen emits `castclass` for ref types / `unbox.any` for value types,
    /// so a runtime mismatch throws `InvalidCastException`.
    | Downcast of source: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `e :? T` type test. `testTy` is the tested-against type `T` (the
    /// `isinst` operand); `ty` is always `TyConst "bool"` (the result). Codegen
    /// emits `isinst <testTy>; ldnull; cgt.un`.
    | TypeTest of source: TExprG<'ty, 'tok> * testTy: 'ty * ty: 'ty * tok: 'tok
    /// SRTP member-trait call, the lowering of a `let inline` operator body's
    /// `when ^T : ^T = ((^T): (static member (+) : ^T * ^T -> ^T) (x, y))` static-opt
    /// clause (`ops-platform.fs`). `receiver` is the trait typar's type (`^T`);
    /// `memberName` is the resolved compiled member name (`op_Addition`). The node is
    /// transient: at `let inline` expansion `Inline.substMapper` substitutes `receiver`
    /// to the concrete operand type and, when that is a project-local nominal carrying
    /// the named static member, rewrites the whole node to a `StaticMethodCall` on it
    /// (the F# "^T is a nominal type" static-optimization condition). It is therefore
    /// resolved — or its clause discarded by static-opt selection — during
    /// `InlineExpansion` and never reaches codegen.
    | TraitCall of receiver: 'ty * memberName: string * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok

and TMatchArmG<'ty, 'tok> =
    {
        Pat: TPatG<'ty, 'tok>
        Guard: TExprG<'ty, 'tok> option
        Body: TExprG<'ty, 'tok>
    }

/// Kept abstract from CLR specifics so an alternate target (JS → template
/// literal) maps it independently. `ToWriter`/`ToBuilder` carry the explicit
/// sink expression (`fprintf` / `bprintf`); P1 produces only the first three.
and [<RequireQualifiedAccess>] FormatSinkG<'ty, 'tok> =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of TExprG<'ty, 'tok>
    | ToBuilder of TExprG<'ty, 'tok>
    | ToString

and [<RequireQualifiedAccess>] FormatSegG<'ty, 'tok> =
    | Lit of string
    | Hole of HoleSpecG<'ty, 'tok> * TExprG<'ty, 'tok>

/// One clause of a `TExpr.StaticOptimization`. `Constraints` is the `and`-joined
/// list (all must hold; declared in `SemanticInfo.fs` so the side table can carry
/// it); `Body` is the clause's optimized expression.
and TStaticOptClauseG<'ty, 'tok> =
    {
        Constraints: EqArray<TStaticOptConstraint>
        Body: TExprG<'ty, 'tok>
    }

// ----------------------------------------------------------------------------
// Compiled-form representation.
// Two derived artifacts describing a function / method: the SOURCE arity
// (`ValReprG`, the `ValReprInfo` analogue) and the flat compiled signature
// (`CompiledFormG`) derived from it. Generic over `'ty`/`'tok`; the builders
// (`peelValRepr`/`compiledOf`) live in `TastLower`, and the cross-assembly
// consumer (`ExternalSymbol`/`CompiledFns`) reads them. See the `Frozen.*`
// aliases below for the instantiated names.
// ----------------------------------------------------------------------------

/// One flattened compiled parameter. A simple binder's `Slot` is referenced by
/// the body directly; a destructuring leaf carries `Pat = Some …` and a synthetic
/// `Slot` the backend spills + `bindPattern`s.
and StaticParamG<'ty, 'tok> =
    {
        Slot: NodeKey
        Ty: 'ty
        Pat: TPatG<'ty, 'tok> option
    }

/// One curried argument group of a function's SOURCE signature — the distinction
/// the flat compiled signature loses. `GUnit` (`fun () -> …`) erases to zero
/// params when it is the sole group; `GSimple` is one non-tuple param; `GTuple`
/// (`fun (a, b, …) -> …`) carries the whole tuple pattern, which full-F#
/// flattening expands to one flat param per element.
and [<RequireQualifiedAccess>] ArgGroupG<'ty, 'tok> =
    | GUnit of ty: 'ty
    | GSimple of slot: NodeKey * ty: 'ty
    | GTuple of pat: TPatG<'ty, 'tok>

/// The SOURCE signature — the `ValReprInfo` analogue. `Groups.Length` is the
/// number of applications a saturated call consumes; `ResultTy` is the source
/// (NOT unit-erased) result type.
and ValReprG<'ty, 'tok> =
    {
        Typars: int
        Groups: ArgGroupG<'ty, 'tok> list
        ResultTy: 'ty
    }

/// The compiled return: `RVoid` is a unit result (CLR `void` / JS no-value).
and [<RequireQualifiedAccess>] CompiledReturnG<'ty> =
    | RVoid
    | RValue of 'ty

/// The flat compiled signature derived from a `ValReprG`: tuple-flattened,
/// lone-unit-erased parameters and the `void`-normalised return.
and CompiledFormG<'ty, 'tok> =
    {
        Params: StaticParamG<'ty, 'tok> list
        Return: CompiledReturnG<'ty>
    }

/// Whether a class declaration emits as a reference type, a `[<Struct>]` value
/// type, or a `[<IsByRefLike>]` byref-like value type. Collapses the former
/// `isStruct`/`isByRefLike` bool pair so the illegal `(isStruct = false,
/// isByRefLike = true)` combination is unrepresentable; `RefStruct` implies
/// value-type emission. Projected at `Freeze` from `ClassTypeInfo`
/// (`IsValueType` / `IsByRefLike`).
[<RequireQualifiedAccess>]
type ClassValueKind =
    | RefType
    | Struct
    | RefStruct

[<RequireQualifiedAccess>]
type TDeclG<'ty, 'tok> =
    /// `isInline` lets codegen expand the body per call site via `Inline.inlineExpand`
    /// rather than emit a single callable.
    | Let of binding: TPatG<'ty, 'tok> * value: TExprG<'ty, 'tok> * isInline: bool * ty: 'ty
    | Expression of expr: TExprG<'ty, 'tok> * ty: 'ty
    | Type of TTypeDeclG<'ty, 'tok>

and TTypeDeclG<'ty, 'tok> =
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
        Kind: TTypeKindG<'ty, 'tok>
        /// Equality posture for this type (records / unions / interfaces).
        /// Defaults to `Structural` — interfaces ignore it (no triple is ever
        /// synthesised), records / unions consume it in the codegen loops.
        EqualitySupport: EqualityVerdict
        /// Comparison posture for this type (records / unions / interfaces).
        /// Defaults to `NoComparison` — interfaces ignore it (no pair is ever
        /// synthesised), records / unions consume it in the codegen loops to
        /// decide whether to emit the `IComparable<Self>` / `IComparable`
        /// `InterfaceImpl`s and the `CompareTo(Self)` / `CompareTo(object)`
        /// pair. Per brainstorm-comparison §9 the default is **opt-in**, so an
        /// unannotated record / union skips the pair.
        ComparisonSupport: ComparisonVerdict
    }

and [<RequireQualifiedAccess>] TTypeKindG<'ty, 'tok> =
    /// A nominal type whose members are all abstract and which has no base type /
    /// field.
    | Interface of methods: EqArray<TAbstractMethodG<'ty>>
    /// `cases` in declaration order (the index is the runtime tag), plus any
    /// augmentation members (`with member …` / `static member …`).
    | Union of cases: EqArray<TUnionCaseG<'ty>> * members: EqArray<TTypeMemberG<'ty, 'tok>>
    /// `fields` are the record's payload in declaration order, paired with their
    /// declared types and mutability. `members` carries augmentation members
    /// (`with member …` / `static member …`) — empty for v1, where records carry
    /// only their field shape.
    | Record of fields: EqArray<TRecordFieldG<'ty>> * members: EqArray<TTypeMemberG<'ty, 'tok>>
    /// Class type emission (B-1).
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
    /// `ValueKind` (`ClassValueKind`) replaces the former `isStruct`/`isByRefLike`
    /// bool pair: `Struct` ⇒ codegen emits a `System.ValueType`-based value type
    /// (sealed, sequential layout, ctor without a base-ctor chain) instead of a
    /// reference class; `RefStruct` additionally stamps
    /// `System.Runtime.CompilerServices.IsByRefLikeAttribute` so the CLR confines
    /// the value type to the stack. `fields`
    /// (the explicit `val [mutable] x: T` instance fields) are populated for both
    /// structs and classes that declare them — each emits a `FieldDefinition` and
    /// a mutable one admits `this.x <- …`.
    | Class of TClassG<'ty, 'tok>

/// The payload of `TTypeKindG.Class` (B-1), lifted out of an 11-wide positional
/// tuple into a named record. See the `Class` case doc for per-field semantics.
and TClassG<'ty, 'tok> =
    {
        Fields: EqArray<TRecordFieldG<'ty>>
        CtorParams: EqArray<TRecordFieldG<'ty>>
        Members: EqArray<TTypeMemberG<'ty, 'tok>>
        BaseType: 'ty voption
        Interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'tok>>>
        IsSealed: bool
        StaticLets: EqArray<TStaticLetG<'ty, 'tok>>
        SecondaryCtors: EqArray<TSecondaryCtorG<'ty, 'tok>>
        BaseCtorCall: TBaseCtorCallG<'ty, 'tok> voption
        ValueKind: ClassValueKind
        /// True when the class declares a *primary* constructor (`type T(args) =`,
        /// including the parameterless `type T() =`); false for the `val`-field form
        /// (`type T = val …; new(…) = { … }`) whose only ctors are secondaries. The
        /// backend emits a synthesised primary `.ctor` only when this is true — for
        /// the val-field form the secondaries ARE the ctors, and a synthesised
        /// parameterless primary would collide with a parameterless `new()` (two
        /// identical `.ctor()` rows) and shadow it at construction.
        HasPrimaryCtor: bool
    }

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
/// triple's "all-immutable record" gate) read it from here rather
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
and TTypeMemberG<'ty, 'tok> =
    {
        Name: string
        IsStatic: bool
        Kind: TMemberKind
        /// `true` when declared with the `override`/`default` keyword — i.e. it
        /// overrides a base virtual slot. For a class with no `inherit` clause
        /// that base is `System.Object`, so an `override` `Equals`/`GetHashCode`/
        /// `ToString` reuses the Object virtual slot and must emit *virtual*
        /// (reusing the slot, no `NewSlot`); a plain `member` is non-virtual.
        /// Without this the override emits `Public HideBySig` (non-virtual), so it
        /// never replaces `Object.Equals` and — for a structural-equality interface
        /// like `IStructuralEquatable` — the type fails to satisfy its slots.
        IsOverride: bool
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
        Body: TExprG<'ty, 'tok>
        ReturnTy: 'ty
        /// The member's *own* generic parameters (`member this.Map<'C> …`, B-12) — distinct from the declaring
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

/// A class-level `static let x = <init>` (B-10).
/// Codegen emits one private static field per entry and concatenates the
/// `Init` expressions into a synthesised `.cctor`; a `static let`-bound name
/// referenced in a member body lowers to `TExpr.StaticFieldGet`. On a *generic*
/// class the field rides the open `TypeDefinition` (one per closed instantiation,
/// `.cctor`-initialised) and the read/store mint a `MemberRef` on the self-
/// `TypeSpec` at the declaring typars (G13).
and TStaticLetG<'ty, 'tok> =
    {
        Name: string
        Type: 'ty
        Init: TExprG<'ty, 'tok>
    }

/// One `let`-preamble binding inside a secondary constructor body
/// (`new(args) = let x = e in SelfType(...)`). `Binder` is the local's
/// `NodeKey` (codegen allocates a local slot and a body reference to the name
/// loads it); `Init` is the right-hand side. Only simple (single-name) binders
/// are modelled in v1.
and TCtorLetG<'ty, 'tok> =
    {
        Binder: NodeKey
        Type: 'ty
        Init: TExprG<'ty, 'tok>
    }

/// One `field = expr` initialiser of a secondary constructor's explicit
/// field-init block (`new(s) = { stack = s; started = false }`).
/// `Field` names a declared instance field (an explicit
/// `val` or a primary-ctor backing field); `Init` is the value stored into it
/// (`ldarg.0; <Init>; stfld Field`). Used only when a secondary ctor takes the
/// explicit-init form instead of chaining to the primary ctor.
and TCtorFieldInitG<'ty, 'tok> =
    {
        Field: string
        Init: TExprG<'ty, 'tok>
    }

/// A secondary constructor (B-11). Codegen emits a
/// `.ctor` overload: `Params` are the overload's parameters (`ldarg` after
/// `this`); `Lets` run as locals in declaration order. The body then takes one
/// of two shapes, never both:
/// - **Chain form** (`new(args) = SelfType(...)`): `FieldInits` is empty and the
///   body chains to the primary `.ctor` with `PrimaryArgs` (`ldarg.0; <args>;
///   call instance void SelfType::.ctor`). There is no usable `this` before the
///   chain call, so `Lets` / `PrimaryArgs` only reference the ctor params and
///   earlier lets.
/// - **Explicit field-init form** (`new(args) = { f = e; … }`):
///   `PrimaryArgs` is empty and each `FieldInits` entry stores into a
///   declared field (`ldarg.0; <Init>; stfld f`). No primary chain — the fields
///   not listed are left default-initialised. `this`'s storage is the freshly
///   allocated (zeroed) instance, so `Init` may reference ctor params and lets.
and TSecondaryCtorG<'ty, 'tok> =
    {
        Params: EqArray<NodeKey * 'ty>
        Lets: EqArray<TCtorLetG<'ty, 'tok>>
        PrimaryArgs: EqArray<TExprG<'ty, 'tok>>
        FieldInits: EqArray<TCtorFieldInitG<'ty, 'tok>>
    }

/// An `inherit Base(args)` base-constructor invocation (B-4 Step 2.5). Codegen wires the primary `.ctor` to chain to the
/// parent's `.ctor`: `ldarg.0; <Args>; call instance void Base::.ctor(…)` before
/// storing the derived class's own fields. `CtorParams` are the *derived* class's
/// primary-ctor parameters (the `ldarg` mapping the base-ctor `Args` reference —
/// `this` isn't constructed yet, so an arg can only name a primary-ctor param or
/// a `static let`). The parent type itself rides the `Class` kind's `baseType`
/// slot, which also supplies the IL `TypeDefinition.BaseType`.
and TBaseCtorCallG<'ty, 'tok> =
    {
        CtorParams: EqArray<NodeKey * 'ty>
        Args: EqArray<TExprG<'ty, 'tok>>
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

type TastFileG<'ty, 'tok> =
    {
        /// Source order.
        Decls: EqArray<TDeclG<'ty, 'tok>>
        /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
        // Qualified: this file `open`s `XParsec.FSharp.Parser`, which also declares a
        // `Diagnostic`; the bare name would bind to the parser's, mistyping the field.
        Diagnostics: XParsec.FSharp.SemanticAnalysis.Diagnostic list
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
        /// A *top-level* (implicit-"Program"-module) binding's `NodeKey.Raw` → its
        /// source name. Top-level bindings (an exe's last file, FS0222) record no
        /// `ModuleMemberInfo`; this names a top-level value lowered to a
        /// Program-holder static field. Empty for a library or a file led by a
        /// `module`/`namespace` declaration.
        TopLevelNames: Map<uint64, string>
        /// A closure binder's `NodeKey.Raw` → its stack-vs-heap verdict
        /// (the `EscapeState.LocalStack ∧ RegionRepr.StackOnlyEligible`
        /// conjunction), snapshotted from `ctx.Bindings.Escape` /
        /// `ctx.Bindings.ClosureRepr` after `Regions.run`. Read by codegen's
        /// `discoverClosures` to set `Emit.Closure.Repr`; a binder absent here
        /// (or any anonymous lambda) defaults to `Heap`. Inert today — emission
        /// still forces heap.
        ClosureReprs: Map<uint64, ClosureRepr>
    }

// Central monomorphic SemType aliases. Every consumer today speaks `SemType`;
// these aliases keep the bare TAST names stable as an additive change. The
// cutover adds a parallel `FrozenType` instantiation without re-touching annotations.

type TPat = TPatG<SemType, SyntaxToken>
type HoleSpec = HoleSpecG<SemType, SyntaxToken>
type TExpr = TExprG<SemType, SyntaxToken>
type TMatchArm = TMatchArmG<SemType, SyntaxToken>
type FormatSink = FormatSinkG<SemType, SyntaxToken>
type FormatSeg = FormatSegG<SemType, SyntaxToken>
type TStaticOptClause = TStaticOptClauseG<SemType, SyntaxToken>
type TDecl = TDeclG<SemType, SyntaxToken>
type TTypeDecl = TTypeDeclG<SemType, SyntaxToken>
type TTypeKind = TTypeKindG<SemType, SyntaxToken>
type TUnionCase = TUnionCaseG<SemType>
type TRecordField = TRecordFieldG<SemType>
type TTypeMember = TTypeMemberG<SemType, SyntaxToken>
type TStaticLet = TStaticLetG<SemType, SyntaxToken>
type TCtorLet = TCtorLetG<SemType, SyntaxToken>
type TCtorFieldInit = TCtorFieldInitG<SemType, SyntaxToken>
type TSecondaryCtor = TSecondaryCtorG<SemType, SyntaxToken>
type TBaseCtorCall = TBaseCtorCallG<SemType, SyntaxToken>
type TAbstractMethod = TAbstractMethodG<SemType>
type TastFile = TastFileG<SemType, SyntaxToken>

// Parallel frozen aliases. Codegen and the freeze step speak these; the bare names
// above STAY `SemType` (inference, Regions, tests, any non-codegen API).

module Frozen =
    type TPat = TPatG<FrozenType, SyntaxToken>
    type HoleSpec = HoleSpecG<FrozenType, SyntaxToken>
    type TExpr = TExprG<FrozenType, SyntaxToken>
    type TMatchArm = TMatchArmG<FrozenType, SyntaxToken>
    type FormatSink = FormatSinkG<FrozenType, SyntaxToken>
    type FormatSeg = FormatSegG<FrozenType, SyntaxToken>
    type TStaticOptClause = TStaticOptClauseG<FrozenType, SyntaxToken>
    type TDecl = TDeclG<FrozenType, SyntaxToken>
    type TTypeDecl = TTypeDeclG<FrozenType, SyntaxToken>
    type TTypeKind = TTypeKindG<FrozenType, SyntaxToken>
    type TUnionCase = TUnionCaseG<FrozenType>
    type TRecordField = TRecordFieldG<FrozenType>
    type TTypeMember = TTypeMemberG<FrozenType, SyntaxToken>
    type TStaticLet = TStaticLetG<FrozenType, SyntaxToken>
    type TCtorLet = TCtorLetG<FrozenType, SyntaxToken>
    type TCtorFieldInit = TCtorFieldInitG<FrozenType, SyntaxToken>
    type TSecondaryCtor = TSecondaryCtorG<FrozenType, SyntaxToken>
    type TBaseCtorCall = TBaseCtorCallG<FrozenType, SyntaxToken>
    type TAbstractMethod = TAbstractMethodG<FrozenType>
    type TastFile = TastFileG<FrozenType, SyntaxToken>
    type ForInEnumerator = ForInEnumeratorG<FrozenType>
    type StaticParam = StaticParamG<FrozenType, SyntaxToken>
    type ArgGroup = ArgGroupG<FrozenType, SyntaxToken>
    type ValRepr = ValReprG<FrozenType, SyntaxToken>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, SyntaxToken>
