namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
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

/// A compile-time constant. An integral constant carries its WIDTH as an `IntWidth` witness
/// (`XParsec.FSharp.Lexer`) beside its value, rather than being one DU case per width: the
/// width is then a value a consumer can ASK about (`IntWidth.name`, `IntWidth.isSigned`,
/// `IntWidth.render`) instead of a shape it must re-derive by enumerating ten cases.
///
/// That is what makes width a thing the compiler tracks. Every match on `TConstValue` is now
/// exhaustive over EIGHT cases with no residual `| other -> failwith` arm, and a new integral
/// width is one new `IntWidth` case — a compile error at each table that must learn about it,
/// not a silent fall-through that fails at run time.
///
/// `bits` is `IntWidth`'s normal form; see that module for the encoding.
[<RequireQualifiedAccess>]
type TConstValue =
    | Integral of width: IntWidth * bits: int64
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
    /// An enum-case constant pattern `| E.C1`. Like a `Const` pattern it is
    /// refutable, binds nothing, and (v1) lowers to *equality against the case's
    /// underlying value* — but it carries the case *identity* (`enumKey` +
    /// `caseName`), NOT the literal. The underlying literal lives on the frozen
    /// `TTypeKind.Enum` case table (the single source of truth, looked up by
    /// `enumKey` + `caseName`), so codegen (steps 5/6) resolves it there — exactly
    /// the producer/consumer split the `E.C1` *expression* form uses
    /// (`TExpr.StaticFieldGet`, same `enumKey`/`caseName` carrier). `ty` is the
    /// enum nominal (`TyEnum enumKey`), unified against the scrutinee so a
    /// `match (x: E)` checks and a `match (n: int)` is a type error. Closed-enum
    /// exhaustiveness is a deferred follow-up: a wildcard-less enum match is the
    /// usual incomplete match (defined-behaviour fallthrough), not an error.
    | EnumCase of enumKey: SymbolKey * caseName: string * ty: 'ty * tok: 'tok
    /// `p1 | p2 | … | pn` OR-pattern. Refutable: matches iff SOME alternative
    /// matches (tested left-to-right, first match wins). `alts` has ≥ 2 entries
    /// (the parser only builds an `Or` for an actual `|`); nested source `|`s are
    /// flattened into one level here. Binds nothing: name resolution
    /// (`bindingsOfPat`) drops or-pattern binders, so alternatives are pure
    /// refutability tests and no binder-correspondence handling is needed. `ty` is
    /// the shared alternative type (unified in `InferPat`).
    | Or of alts: EqArray<TPatG<'ty, 'tok>> * ty: 'ty * tok: 'tok

/// A format hole's classified per-value formatting. A hole no longer stores the `(Kind, .NET-format, alignment)` triple
/// `PrintfSpec.tryHoleFormat` produced — it carries the *classified*, target-neutral
/// `HoleForm`, and the CLR triple (a runtime artifact) is projected on demand by
/// `Codegen.Clr.ClrHoleFormat.toDotNetFormat`. Two origins:
/// - `Classified` — the classification the lowering gate (`ElaborateExpr`) already
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
/// (`FormatPlaceholder` itself carries no position).
///
/// The legacy `(Kind, Format, Alignment)` projection that once lived here as
/// transitional members was retired: both
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
/// `override` doesn't recurse into itself. Set by Elaborate when the receiver's
/// head binding site is a class's `BaseKey`; read by codegen to pick the call opcode.
/// `Interface` is a constrained dispatch on a *generic typar* receiver coerced to
/// an interface (`'T :> IFace`): the `MethodCall`'s `key` declaring
/// type is the interface, the receiver's type is the typar, and codegen emits
/// `constrained. <typar> callvirt <iface-slot>` (no box for a struct typar, a
/// reference dispatch for a class typar). Set by Elaborate when the receiver resolved
/// through `TyparInterfaceCall`; read by codegen. The carried `ifaceArgs`
/// are the interface's instantiation type arguments (`'E` in `'T :> IStructSeq<'E>`),
/// threaded from the typar's `Coercion` constraint so codegen can mint the slot on
/// the *instantiated* interface `TypeSpec`; empty for a non-generic interface.
[<RequireQualifiedAccess>]
type CallVia<'ty> =
    | Self
    | Base
    | Interface of ifaceArgs: EqArray<'ty>

/// How a `use` binder is disposed — the resolved verdict of Unification's
/// `resolveUseDispose`, read by every backend to pick its disposal call.
///
/// The two disposal paths are semantically distinct, so they are distinct cases: a binder
/// that implements the `disposable` CAPABILITY disposes through whatever slot the target
/// gives that capability (the CLR `IDisposable::Dispose` interface slot, the JS
/// `[Symbol.dispose]()` method), while the ref-struct carve-out and an external type's own
/// pattern `Dispose()` are ordinary member calls that happen to be named `Dispose`. Keeping
/// both in one `SymbolKey voption` forced each backend to re-derive which it held.
[<RequireQualifiedAccess>]
type Disposal =
    /// The binder implements the disposal capability. `slot` is the capability's resolved
    /// `Dispose` member key (`disposable::Dispose`) — used by a target that dispatches
    /// through the interface (the CLR, for an *external* binder); a target with a native
    /// disposal slot (JS's `[Symbol.dispose]()`) names that slot itself and ignores `slot`.
    | ViaCapability of slot: SymbolKey
    /// The binder does NOT implement the capability but exposes its own pattern `Dispose()`:
    /// the `[<IsByRefLike>]` ref-struct carve-out (it cannot be boxed to the interface), or
    /// an external type with an own-`Dispose` and no `IDisposable`. Call the keyed member
    /// directly — NOT the capability slot.
    | ViaOwnMember of key: SymbolKey
    /// Unification could not resolve disposal for this binder — it reported a
    /// `use`-over-non-disposable error (or the binder's type never resolved). The node
    /// exists only so an erroneous file still elaborates; codegen fails loudly on it,
    /// because a file carrying an error never reaches a backend.
    | Unresolved

[<RequireQualifiedAccess>]
type TExprG<'ty, 'tok> =
    | Const of value: TConstValue * ty: 'ty * tok: 'tok
    /// `binding` is the NodeKey of the *binding site*, not the use site.
    | Var of binding: NodeKey * ty: 'ty * tok: 'tok
    /// Symbol resolved through IExternalSymbolProvider. Carries the compiled
    /// name so target plugins can dispatch (`op_Addition` -> CIL `add` on
    /// .NET, native `+` on Rust, etc. — the inline IL is target-specific).
    /// `key` interns the resolved `SymbolKey` so codegen reads the binding off the
    /// node instead of re-resolving by name;
    /// `ValueNone` until Elaborate stamps it — every site is name-only today.
    | External of compiledName: string * key: SymbolKey voption * ty: 'ty * tok: 'tok
    | Lambda of param: TPatG<'ty, 'tok> * body: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    | App of fn: TExprG<'ty, 'tok> * arg: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    | Let of binding: TPatG<'ty, 'tok> * value: TExprG<'ty, 'tok> * body: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// `use x = value in body`. Same shape as `Let`; the distinction is that
    /// codegen wraps `body` in a `try … finally x.Dispose()` exception region so
    /// `x` is disposed on every exit. `ty` is the body's type — the expression's
    /// result. `dispose` is the resolved disposal path (see `Disposal`).
    | Use of
        binding: TPatG<'ty, 'tok> *
        value: TExprG<'ty, 'tok> *
        body: TExprG<'ty, 'tok> *
        dispose: Disposal *
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
    /// `IEnumerable<'T>` interface slots (the range form too); `Pattern`
    /// carries a pattern-based struct/class `GetEnumerator()`.
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
    /// `start..stop` or `start..step..stop`. Endpoints (and step) all type as int.
    /// This node ONLY survives elaboration for an UNSUPPORTED range (value position,
    /// a stepped range, or a non-simple for-in binder): the supported form —
    /// `for i in a..b do` over a unit step with a simple binder — is lowered to a
    /// counted `ForTo` by `Elaborate.translateForIn` and never reaches here. A surviving
    /// `Range` therefore carries a diagnostic (`ElaborateExpr`) and `ty` is `TyUnknown`
    /// (a range has no first-class value in this compiler; see `range-operators-plan.md`).
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
    /// the `Expr.App` chain in Elaborate.
    | UnionCons of caseName: string * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// Class primary-constructor invocation. `args` is the per-parameter
    /// list — the parser's tuple wrapper (`new Point(3, 4)` parses with
    /// a `Tuple` arg) is peeled in Elaborate so consumers see the ctor's
    /// declared arity directly. `ty` is a `TyClass`. `key` is the chosen
    /// EXTERNAL constructor's `SymbolKey.MemberKey` for a same-arity overload set
    /// the front end resolved by argument type (`ArgumentException(string, string)`
    /// vs `(string, Exception)`) — so codegen selects that exact `.ctor` by identity
    /// instead of re-running overload resolution with no `PassContext`. `ValueNone`
    /// for a project-local class (codegen resolves it by the result-type key + arity,
    /// which F#'s ban on duplicate ctor signatures makes total) and for a scratch
    /// class synthesised by Elaborate.
    | New of className: string * key: SymbolKey voption * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    /// Instance method invocation: `r.M(args)`. `args` is the
    /// per-parameter list (peeled the same way as `New`). `ty` is the
    /// method's declared return type. `key` is the resolved local
    /// `SymbolKey.MemberKey` (declaring type + member name) — codegen reads the declaring type off `key.decl` and the member
    /// name off `key.memberName` instead of re-deriving from a class-name string.
    | MethodCall of
        receiver: TExprG<'ty, 'tok> *
        key: SymbolKey *
        via: CallVia<'ty> *
        args: EqArray<TExprG<'ty, 'tok>> *
        ty: 'ty *
        tok: 'tok
    | PropertyGet of receiver: TExprG<'ty, 'tok> * key: SymbolKey * via: CallVia<'ty> * ty: 'ty * tok: 'tok
    /// Same arg-peeling as `MethodCall`; no receiver. `key` is the resolved local
    /// `SymbolKey.MemberKey`.
    | StaticMethodCall of key: SymbolKey * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok
    | StaticPropertyGet of key: SymbolKey * ty: 'ty * tok: 'tok
    /// Read of a class-level `static let` backing field. Lowered from a `static let`-bound name reference in a member
    /// body (Elaborate rewrites the resolved `Var` exactly as a primary-ctor param
    /// becomes a `FieldGet`). Codegen emits `ldsfld` against the class's private
    /// static field — there is no method call (a static *property* would be a
    /// `StaticPropertyGet`). `ty` is the field's declared/inferred type. `declKey`
    /// is the declaring class's `SymbolKey.TypeKey` (NOT a `MemberKey` — a backing
    /// field is a field, resolved through the class's `StaticFields`, and
    /// `MemberKind` has no `Field` case).
    | StaticFieldGet of declKey: SymbolKey * fieldName: string * ty: 'ty * tok: 'tok
    /// Store to a class-level `static let mutable` backing field. Lowered from a write
    /// (`x <- e`) to a `static let mutable`-bound name in a member body — Elaborate
    /// rewrites the resolved `Assignment(Var …)` exactly as an instance `let mutable`
    /// write becomes a `FieldSet`. Codegen emits `stsfld` against the class's private
    /// static field (the store analogue of `StaticFieldGet`'s `ldsfld`). `declKey` and
    /// `fieldName` carry the same field identity as `StaticFieldGet`. `ty` is the field's
    /// type; the store's own result type is unit.
    | StaticFieldSet of declKey: SymbolKey * fieldName: string * value: TExprG<'ty, 'tok> * ty: 'ty * tok: 'tok
    /// Member access on an *external* type resolved through `IExternalSymbolProvider`
    /// `key` interns the resolved `SymbolKey` so
    /// codegen (P4) mints the ref off the node's identity instead of re-resolving by
    /// name — the external analogue of `TExpr.Var`'s `BindingSite`. `receiver` is
    /// `ValueNone` for a static member (`EqualityComparer<int>.Default`) and
    /// `ValueSome` for an instance member (`…Default.GetHashCode`). `storage`
    /// distinguishes a value member (field/property get) from a method value/group —
    /// and, at CLR emission, a field (`ldfld`) from a property (`call get_X`). `ty` is
    /// the access's result type — the value member's type, or the method's *curried*
    /// function type (`… GetHashCode 5` ⇒ `App(ExternalMember(…, ty = int -> int), 5)`).
    | ExternalMember of
        receiver: TExprG<'ty, 'tok> voption *
        key: SymbolKey *
        memberName: string *
        storage: MemberStorage *
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
    /// in `TastFile.IntrinsicReprKeys`; operator `.fs` bodies (`(=)` → `ceq`,
    /// `(+)` → `add`, …) lower to this so codegen owns no per-operator dispatch.
    ///
    /// `typeOperand` carries the single type token a tokenful array opcode needs
    /// (`newarr`/`ldelem` → the element type); `ValueNone` for the balanced
    /// stack ops that take no operand (the operator surface, and `ldlen`). The
    /// array forms are synthesised by Elaborate from `arr.[i]` / `Array.zeroCreate`
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
    /// `e :> T` static upcast. `source`'s runtime type
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
    /// `((^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) (x, y))` static-opt BASE
    /// (`ops-platform.fs`). `receiver` is the LEFT operand's type
    /// (`^T1`) — one receiver, so the `(^T1 or ^T2)` support set is searched left-only
    /// and a right-operand-only member does not resolve; `ty` is the member's `^T3`
    /// result, which for a heterogeneous operator is neither operand's type.
    /// `memberName` is the resolved compiled member name (`op_Addition`). The node is
    /// transient: at `let inline` expansion `Inline.substMapper` substitutes `receiver`
    /// to the concrete operand type and, when that is a nominal carrying the named
    /// static member, rewrites the whole node to a `StaticMethodCall` on it (the F#
    /// "^T is a nominal type" static-optimization condition). A receiver that is NOT a
    /// nominal cannot carry the member, and `InlineExpansion` reports it as "the type
    /// 'X' does not support the operator '+'" — so the node never reaches codegen
    /// (neither backend has an arm for it).
    ///
    /// This node is ALSO how an operator used as a VALUE (`Seq.fold (+) …`) binds to a
    /// user type's own static member: `InlineExpansion` eta-reifies the operator and
    /// splices the same contract body, whose base is this trait call. There is no
    /// second, type-directed operator-value resolver — the trait call IS the
    /// type-directed decision.
    ///
    /// TODO(right-operand SRTP dispatch): the ONE receiver is why only the LEFT half of
    /// F#'s `when (^T1 or ^T2): static member (+)` rule is observable. A member declared
    /// only on the right operand (`static member (+) (i: int, v: Vector)`) does not
    /// resolve — the support set is searched left-only, and `applyDefaults`' `default
    /// ^T1: ^T3` chain then fuses what the trait bound did not pin, so `int * Vector`
    /// errors `int vs Vector` at unification rather than dispatching. The exemplar to
    /// support is fully-generic mixed-type SRTP inlining —
    /// `let inline lerp c p t = t * c + p * (GenericOne - c)` at
    /// `lerp 0.1f Vector2.Zero Vector2.One`, where `*` is `float32 * Vector2` and must
    /// resolve via `Vector2`'s `op_Multiply`. Carrying a candidate SET here (rather than
    /// one receiver) is what buys that.
    | TraitCall of receiver: 'ty * memberName: string * args: EqArray<TExprG<'ty, 'tok>> * ty: 'ty * tok: 'tok

and TMatchArmG<'ty, 'tok> =
    {
        Pat: TPatG<'ty, 'tok>
        Guard: TExprG<'ty, 'tok> option
        Body: TExprG<'ty, 'tok>
    }

/// Kept abstract from CLR specifics so an alternate target (JS → template
/// literal) maps it independently. `ToWriter`/`ToBuilder` carry the explicit
/// sink expression (`fprintf` / `bprintf`); P1 produces all five when fully
/// applied (`ToWriter` for `fprintf`/`fprintfn`, `ToBuilder` for `bprintf`).
/// `ToWriter`'s `newline` records the trailing `\n` (`fprintfn` sets it,
/// `fprintf` does not), mirroring `ToStdOut`/`ToStdErr`; `ToBuilder` carries no
/// newline (F# has no `bprintfn`).
and [<RequireQualifiedAccess>] FormatSinkG<'ty, 'tok> =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of writer: TExprG<'ty, 'tok> * newline: bool
    | ToBuilder of TExprG<'ty, 'tok>
    | ToString

and [<RequireQualifiedAccess>] FormatSegG<'ty, 'tok> =
    | Lit of string
    | Hole of HoleSpecG<'ty, 'tok> * TExprG<'ty, 'tok>
    /// A hole with one or both dimensions supplied as runtime arguments — star width
    /// (`%*d`, `%-*d`, `%*A`) and/or star precision (`%.*f`, `%*.*f`, `%.*e`, `%.*g`,
    /// `%+.*f`, `%.*A`). The curried application evaluates the dimension args *before*
    /// the value, in source order (width, then precision), so they ride here as
    /// `Width`/`Precision` (each present iff its dimension is a star; at least one is).
    /// Invariant: `Spec.Source` classifies to a form whose `Alignment.Star` /
    /// `PrintWidth.Star` (width) and `Prec.Star` / `PrintSize.Star` (precision) agree
    /// with which of `Width`/`Precision` are present — all constructed from the same
    /// placeholder.
    | DynHole of DynFormatHoleG<'ty, 'tok>
    /// A `%a` / `%t` printer-callback hole (`HoleForm.Callback`), lowered
    /// capture-first to an ordinary residue-*string* expression Elaborate synthesises:
    /// `sprintf` splices the callback's returned string (`cb unit [value]`); the
    /// writer/builder families splice `{ let s = new Scratch() in cb s [value]; s.ToString() }`.
    /// `residue` is therefore just a `string`-typed `TExpr` (the callback + value ride
    /// inside it as ordinary sub-exprs, so every traversal walks it with no special
    /// arm), and both backends emit it exactly as a `%s` hole — no sink knowledge in
    /// codegen. The segment stays distinct only to record `%a`/`%t` provenance.
    | CallbackHole of spec: HoleSpecG<'ty, 'tok> * residue: TExprG<'ty, 'tok>

/// A `FormatSegG.DynHole` payload: the hole's spec + value, plus whichever
/// dimension args the curried application supplies at runtime. `Width` is present
/// iff the width is a star (`%*…`), `Precision` iff the precision is a star
/// (`%.*…`); at least one is present (a plain hole stays `FormatSegG.Hole`). Fields
/// are named (not a wide tuple) so consumers read `Width`/`Precision` by intent.
and DynFormatHoleG<'ty, 'tok> =
    {
        Width: TExprG<'ty, 'tok> voption
        Precision: TExprG<'ty, 'tok> voption
        Spec: HoleSpecG<'ty, 'tok>
        Value: TExprG<'ty, 'tok>
    }

/// One clause of a `TExpr.StaticOptimization`. `Constraints` is the `and`-joined
/// list (all must hold; the constraint type is declared in `SemanticInfo.fs` so the
/// side table can carry it, but is `'ty`-generic like everything else here, so a
/// clause freezes/thaws WHOLE — no `SemType` rides inside a frozen clause); `Body`
/// is the clause's optimized expression.
and TStaticOptClauseG<'ty, 'tok> =
    {
        Constraints: EqArray<TStaticOptConstraintG<'ty>>
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
/// value-type emission. Projected at `Elaborate` from `ClassTypeInfo`
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
        /// the registry `info.TypeKey` (`TypeKey(Some homeAsm, declNs, name\`arity)`),
        /// carried into the backend so the emitted-type tables key off it directly
        /// instead of re-deriving a string. Codegen branches local-vs-external on
        /// its home `asm` (= the assembly being emitted).
        TypeKey: TypeKey
        /// `None` for a module-level type.
        Namespace: string option
        /// Declared type parameters in source order (e.g. `["'A"; "'B"]`).
        TypeParams: EqArray<string>
        /// `[<RequireQualifiedAccess>]` posture. Type-level so it covers records
        /// AND unions in one carrier: it is F#'s `isILOrRequiredQualifiedAccess`
        /// signal (`NameResolution.fs:1277`) projected through freeze — a cross-unit
        /// RQA record is kept OUT of the consumer's unqualified field-set index
        /// (a bare `{ X = … }` must qualify), and an RQA union's case out of the
        /// bare case index. Carried here so the frozen-tree projection
        /// (`FrozenSignature`) can honour it, mirroring the `.fsi` extractor's
        /// `RqaTypes` thread. Default `false`; interfaces / enums leave it unread.
        IsRequireQualifiedAccess: bool
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
        /// pair. The default is **opt-in**, so an
        /// unannotated record / union skips the pair.
        ComparisonSupport: ComparisonVerdict
    }

    /// The nominal identity widened for the key-kind-blind sinks a declaration still feeds
    /// (`MethodKey`/`FieldKey`/`TypeSlotKey` minting, `provider.RegisterUserType`).
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey

and [<RequireQualifiedAccess>] TTypeKindG<'ty, 'tok> =
    /// A nominal type whose members are all abstract and which has no base type /
    /// field.
    | Interface of methods: EqArray<TAbstractMethodG<'ty>>
    /// `cases` in declaration order (the index is the runtime tag), plus any
    /// augmentation members (`with member …` / `static member …`). `interfaces`
    /// mirrors `TClassG.Interfaces`: each entry pairs a resolved interface type
    /// with its already-typed member bodies (the `interface IFace with member …`
    /// blocks declared on the union). Empty for a plain union. The JS backend emits
    /// these as the union BASE class's capability protocol members (an enumerable impl
    /// → `[Symbol.iterator]`, inherited by every case subclass); the CLR backend's
    /// emission is still deferred.
    | Union of
        cases: EqArray<TUnionCaseG<'ty>> *
        members: EqArray<TTypeMemberG<'ty, 'tok>> *
        interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'tok>>>
    /// `fields` are the record's payload in declaration order, paired with their
    /// declared types and mutability. `members` carries augmentation members
    /// (`with member …` / `static member …`). `interfaces` mirrors
    /// `TClassG.Interfaces` / `Union.interfaces`: each entry pairs a resolved
    /// interface type with its already-typed member bodies (the
    /// `interface IFace with member …` blocks declared on the record). Empty for a
    /// plain record. The CLR backend emits the impl methods as `InterfaceImpl` rows;
    /// the JS backend attaches them to the record's class (local interface →
    /// attached method; capability interface → iterator / registry symbol).
    /// `valueKind` is `Struct` for a `[<Struct>]` record (emitted as a
    /// `System.ValueType`-based value type, sealed) and `RefType` otherwise;
    /// projected at `Elaborate` from `RecordTypeInfo.IsValueType`, mirroring how
    /// the class arm carries `ClassValueKind`. `RefStruct` is unreachable — a
    /// record cannot be `[<IsByRefLike>]`.
    | Record of
        fields: EqArray<TRecordFieldG<'ty>> *
        members: EqArray<TTypeMemberG<'ty, 'tok>> *
        interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'tok>>> *
        valueKind: ClassValueKind
    /// Class type emission.
    /// `fields` are mutable instance fields (currently empty);
    /// `ctorParams` borrows the `TRecordField` shape for the primary
    /// constructor's parameter list (name / type / mutability=false).
    /// `members` carries every instance / static method / property (the
    /// instance-vs-static split is the member's own `IsStatic`).
    /// `baseType` is `ValueNone` (codegen defaults the IL
    /// `TypeDefinition.BaseType` to `Object`); a later slice fills it from
    /// `ClassTypeInfo.BaseType`. `interfaces` is empty; another slice
    /// fills it from the interface-impl registry — each entry pairs the
    /// resolved interface type (a `TyClass`, remapped onto this class's typar
    /// markers so a generic interface arg like `IEnumerable<'T>` encodes against
    /// the declaring type's generic parameters) with its already-typed member
    /// bodies. Codegen emits one `InterfaceImpl` row per entry and one
    /// `MethodDefinition` per member (implicit impl — bound by name + signature;
    /// explicit `.override` rows are deferred with the `MethodImpl` table).
    /// `isSealed` reflects `[<Sealed>]`: when `true`, codegen flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition` — derivation
    /// is rejected at use sites (`subsumes` already excludes
    /// `Sealed`).
    /// `staticPreamble` / `instancePreamble` are the class's `[static] let` / `[static] do`
    /// entries: codegen emits one private field per `let` (static / instance respectively)
    /// and runs each sequence, in declaration order, in the `.cctor` / the primary ctor.
    /// `secondaryCtors` are `new(args) = SelfType(primaryArgs)` overloads:
    /// codegen emits each as a `.ctor` overload whose body runs the let-preamble
    /// then chains to the primary `.ctor`. Empty unless the class declares any.
    /// `baseCtorCall` is the `inherit Base(args)` invocation: codegen makes the primary `.ctor` chain to the parent's `.ctor` with
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
    /// `cases` in declaration order, each pairing a case identifier with its
    /// **resolved** compile-time literal (`| C = v`). An enum is `'ty`-free: a
    /// case value is an integer or string literal, never a typed term. The
    /// numeric / string / mixed variant is NOT stored here — it is **derived**
    /// from the case literals on demand (`TEnumCases.classify`), the single
    /// source of truth. See `TEnumCaseG` / `TEnumLiteral`.
    | Enum of cases: EqArray<TEnumCaseG<'tok>>

/// The payload of `TTypeKindG.Class`, lifted out of an 11-wide positional
/// tuple into a named record. See the `Class` case doc for per-field semantics.
and TClassG<'ty, 'tok> =
    {
        Fields: EqArray<TRecordFieldG<'ty>>
        CtorParams: EqArray<TRecordFieldG<'ty>>
        Members: EqArray<TTypeMemberG<'ty, 'tok>>
        BaseType: 'ty voption
        Interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'tok>>>
        IsSealed: bool
        /// `static let` / `static do`, in declaration order: the body of the
        /// synthesised `.cctor`. Empty unless the class declares any.
        StaticPreamble: EqArray<TPreambleEntryG<'ty, 'tok>>
        /// Instance `let` / `do`, in declaration order: the tail of the primary ctor,
        /// running after the base-ctor call and the ctor-param field stores. Empty
        /// unless the class declares any; a class with NO primary ctor can never have
        /// one (the front-end rejects it, F#'s FS0963).
        InstancePreamble: EqArray<TPreambleEntryG<'ty, 'tok>>
        /// The `this` binder every instance member body already carries
        /// (`TTypeMemberG.ThisKey`), lifted onto the class because the INSTANCE
        /// preamble's expressions read the class's fields through it too — a ctor-param
        /// or instance-`let` reference in an initialiser or `do` body is a
        /// `TExpr.FieldGet(TExpr.Var(ThisKey), …)`, so the backend must map this key to
        /// the primary ctor's `this` argument.
        ThisKey: NodeKey
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

/// A resolved enum-case literal — the classified result of reading
/// `EnumTypeCase.constValue` through the canonical literal reader
/// (`ElaborateLiterals.parseConst` / `foldStringParts`). Restricting the shape to
/// `Int` / `String` makes the non-int-non-string values the elaborator rejects
/// unrepresentable on the node (illegal cases never construct a `TEnumLiteral`;
/// they record `ValueNone` on `TEnumCaseG.Value`).
and [<RequireQualifiedAccess>] TEnumLiteral =
    /// An integer enum-case value: the authored integral literal exactly as `parseConst`
    /// resolved it, so its `IntWidth` is the authored width. `int` doubles as the
    /// unsuffixed default — step 2/freeze maps it to `I32`, every other width to its own
    /// CLR underlying type. Width is therefore *preserved*, not defaulted, here.
    ///
    /// Invariant: always a `TConstValue.Integral` whose width satisfies `IntWidth.isEnumBase`
    /// — the elaborator rejects every other constant, `nativeint` / `unativeint` included
    /// (no `System.Enum` may be based on a pointer-width integer).
    | Int of value: TConstValue
    /// A string enum-case value — the stitched literal text (escapes decoded).
    | String of value: string

/// One case of a `TTypeKind.Enum`, in declaration order. `'tok` is carried for
/// the case identifier's source token (diagnostics / source-maps), matching the
/// token-preserving convention of the sibling AST nodes.
and TEnumCaseG<'tok> =
    {
        /// Case identifier (`C` in `| C = v`).
        Name: string
        /// The case's resolved compile-time literal, classified `Int` / `String`
        /// (`TEnumLiteral`). `ValueNone` when `constValue` failed to resolve to a
        /// legal literal — a non-literal expression, an interpolated string, or a
        /// non-int-non-string constant — for which a hard error was reported at
        /// the case's source token. The case is still recorded so the enum's
        /// shape and its sibling cases survive a single bad case.
        Value: TEnumLiteral voption
        /// Source token of the case identifier.
        Tok: 'tok
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
    /// `PropertyDefinition` row yet).
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
        /// The synthetic `base` binder of the declaring class, shared across every
        /// member body. A `base.M(...)`
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
        /// The member's *own* generic parameters (`member this.Map<'C> …`) — distinct from the declaring
        /// type's `TTypeDecl.TypeParams`. Each entry pairs the source name
        /// (`"'C"`, for the `GenericParam` row) with the post-unification
        /// union-find *root* `TypeVar`. `Elaborate.remapMemberTypes` uses these roots to
        /// flip the method axis in `Params` / `ReturnTy` / `Body` to
        /// `TyTypar(Method, i)`, exactly as the declaring
        /// type's typars ride `TyTypar(Declaring, i)`; codegen's encoder resolves
        /// both axes by index (`!!i` / `!i`) with no ambient window. This list still
        /// feeds the `GenericParam` rows and the `GENERIC` header arity. Empty for a
        /// non-generic member. Carries the canonical ABI order correct-by-construction
        /// (`GeneralizedTypars`), flowed unbroken from the side-table `Generalized`.
        MethodTypeParams: GeneralizedTypars
    }

/// One `[static] let [mutable] x = <init>` of a class preamble.
///
/// A STATIC entry is a private static field, initialised by the synthesised `.cctor`; a
/// `static let`-bound name referenced anywhere in the class lowers to
/// `TExpr.StaticFieldGet`. On a *generic* class the field rides the open `TypeDefinition`
/// (one per closed instantiation, `.cctor`-initialised) and the read/store mint a
/// `MemberRef` on the self-`TypeSpec` at the declaring typars.
///
/// An INSTANCE entry is a private instance field, initialised by the primary ctor — the
/// same lowering a primary-ctor parameter already gets, with the value coming from `Init`
/// instead of an argument. Its references (in a member body or in a later preamble entry)
/// are therefore `TExpr.FieldGet`/`FieldSet` on `this`, never a `TExpr.Let` binder: an
/// instance `let mutable` captured by a preamble closure must stay ONE field, so it must
/// never reach `RefCellPromotion` (which would fork the storage between a promoted cell in
/// the closure and the field every member reads).
and TClassLetG<'ty, 'tok> =
    {
        Name: string
        Type: 'ty
        /// `let mutable` ⇒ the field is writable.
        IsMutable: bool
        Init: TExprG<'ty, 'tok>
    }

/// One entry of a class preamble, in DECLARATION order. Interleaving is
/// order-sensitive (`static let a = f()` / `static do g a` / `static let b = h()`), so a
/// preamble is one ordered sequence — not parallel lists of lets and dos.
and [<RequireQualifiedAccess>] TPreambleEntryG<'ty, 'tok> =
    | Let of TClassLetG<'ty, 'tok>
    | Do of TExprG<'ty, 'tok>

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

/// A secondary constructor. Codegen emits a
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

/// An `inherit Base(args)` base-constructor invocation. Codegen wires the primary `.ctor` to chain to the
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
        /// The chosen base `.ctor`'s identity for an EXTERNAL base (`inherit exn(msg)`),
        /// recorded by `Unification.fillBaseCtorCall` so codegen chains the base ctor by key
        /// rather than re-picking by arity. `ValueNone` for a project-local base (its ctor is
        /// the local class's, resolved from the emitted class table) and for an external base
        /// whose overload identity was not recorded (codegen falls back to arity).
        ChosenCtor: SymbolKey voption
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
        /// `true` for an abstract *property* (`abstract member Current : int` —
        /// an arg-less member sig). It emits as a `get_<Name>` getter slot so a
        /// property impl (`get_Current`) binds to it by name + signature; a
        /// `false` (method) slot keeps its bare name.
        IsProperty: bool
    }

/// A splice TEMPLATE: an inline binding's retained declaration plus the compiler
/// attributes on its parameters, positionally aligned to its curried parameters
/// (`[<CallAtMostOnce>]` &c.; empty for a body whose parameters carry none). The
/// inliner reads both — the attrs gate call-by-name-at-single-use splicing.
type TInlineBodyG<'ty, 'tok> =
    {
        Decl: TDeclG<'ty, 'tok>
        ParamAttrs: ParamAttrs[]
    }

/// One entry of a unit's INLINE VOCABULARY (`TastFileG.InlineBodies`): a body,
/// under the identity its home unit interns it by.
///
/// The `Key` is MINTED (from `ModuleBindingInfo`, at freeze), not recovered: an inline
/// binding is the one kind of symbol that is exported but NEVER emitted, so nothing
/// downstream would ever mint its identity as a side effect of emitting it. A consumer
/// splices by this key — the same key its use-site `TExpr.External` carries.
type TInlineValueG<'ty, 'tok> =
    {
        Key: SymbolKey
        Body: TInlineBodyG<'ty, 'tok>
    }

/// What a unit's `(# … #)` binding records about one intrinsic: the target
/// representation string, and whether the binding was `class`-tagged
/// (`(# class "System.Attribute" #)`) and so may be inherited.
///
/// ONE entry per intrinsic, carrying both facts: heritability is a property OF a
/// repr, so a heritable intrinsic with no repr is unrepresentable rather than
/// merely unexpected.
type IntrinsicReprInfo =
    {
        /// The target representation (`Vesper.int` → `"System.Int32"`).
        Platform: string
        /// `(# class "…" #)`-tagged: a derived unit may `inherit` this primitive
        /// (`obj` / `exn` / `Attribute`). A scalar primitive (`int`) is `false`.
        Heritable: bool
    }

type TastFileG<'ty, 'tok> =
    {
        /// Source order. In the FROZEN domain these are the EMITTABLE decls only:
        /// `Freeze` partitions the inline templates out into `InlineBodies` (an
        /// inline binding is vocabulary, not code). Pre-freeze they are still here —
        /// `Passes.InlineExpansion` splices a same-unit inline call off them.
        Decls: EqArray<TDeclG<'ty, 'tok>>
        /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
        // Qualified: this file `open`s `XParsec.FSharp.Parser`, which also declares a
        // `Diagnostic`; the bare name would bind to the parser's, mistyping the field.
        Diagnostics: XParsec.FSharp.SemanticAnalysis.Diagnostic list
        /// This unit's OWN intrinsics: the canon `SymbolKey` of a `type x = (# "..." #)`
        /// abbrev → its target representation string (`Vesper.int` → `"System.Int32"`).
        /// The backend keys the emitted IL type off the *representation string* (so a
        /// platform author retargets a primitive by editing one `.fs` line), and asks for
        /// it with the KEY the `FTConst` node carries — the frozen face of
        /// `TypeRegistry.IntrinsicReprKeys`, and the local half of the same forward
        /// `{ canon -> platform repr }` axis the provider's `IntrinsicForwardRepr` serves
        /// for the dependency closure. Keyed by identity, never by declared name: a name
        /// cannot say WHICH `int` it means, so a user type sharing an intrinsic's short
        /// name would otherwise pick up its repr.
        ///
        /// A HASH map, not an F# `Map`: a `SymbolKey` is an identity, so it is equatable
        /// but deliberately not ordered. Same face the provider's `IntrinsicForwardRepr`
        /// presents, so the backend's two halves of the axis read alike.
        IntrinsicReprKeys: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, IntrinsicReprInfo>
        /// A module-level binding's `NodeKey` → its named-holder placement
        /// (`module Foo`'s functions emit on a real `Foo`/`FooModule` static class,
        /// not the anonymous "Program" holder). Empty for a program with no named
        /// modules — every static method then lands on "Program" as before.
        ModuleMembers: Map<NodeKey, ModuleBindingInfo>
        /// A *top-level* (implicit-"Program"-module) binding's `NodeKey` → its
        /// source name. Top-level bindings (an exe's last file, FS0222) record no
        /// `ModuleBindingInfo`; this names a top-level value lowered to a
        /// Program-holder static field. Empty for a library or a file led by a
        /// `module`/`namespace` declaration.
        TopLevelNames: Map<NodeKey, string>
        /// A closure binder's `NodeKey` → its stack-vs-heap verdict
        /// (the `EscapeState.LocalStack ∧ RegionRepr.StackOnlyEligible`
        /// conjunction), snapshotted from `ctx.Bindings.Escape` /
        /// `ctx.Bindings.ClosureRepr` after `Regions.run`. Read by codegen's
        /// `discoverClosures` to set `Emit.Closure.Repr`; a binder absent here
        /// (or any anonymous lambda) defaults to `Heap`. Inert today — emission
        /// still forces heap.
        ClosureReprs: Map<NodeKey, ClosureRepr>
        /// A SOURCE-lambda argument's `NodeKey` → its
        /// value-struct closure verdict (`FunVerdict`: the flat `FunN` arity, plus
        /// the result-typar position for a transformer combinator). Snapshotted from
        /// `ctx.FunVerdicts`; `discoverClosures` reads `Arity` to size the closure's
        /// flat `Invoke`, and `ClosureVerdictRewrite` reads `ResultTyparPos` to lay a
        /// stored binding's `'TFunc` slot out as the `<closure>$` value-struct rather
        /// than the `Fun`2`/`Fun`3` interface. A lambda absent here is an ordinary
        /// curried closure.
        FunVerdicts: Map<NodeKey, FunVerdict>
        /// A project-local generalised binding's
        /// `NodeKey` → its frozen typar bounds (method-axis-indexed
        /// `FrozenConstraint` templates). Snapshotted at `Elaborate.run` (where the
        /// method-typar indices are minted, so the bounds' typar leaves line up with
        /// the body's), threaded `TastFile → HolderPlan → StaticFn/StaticMethodRef`
        /// exactly like `FunVerdicts`. Read by the call-site phantom-typar solve
        /// (`EmitCall`); the emitted arity is re-derived independently by
        /// `staticFnTypars`' body sweep.
        GenericFnSchemes: Map<NodeKey, FrozenConstraint list>
        /// The unit's INLINE VOCABULARY: every `let inline` binding (and every
        /// nullary-intrinsic value alias — `let undefined = (# "undefined" #)`, which
        /// the backends also splice rather than call), keyed by the identity its home
        /// unit interns it under.
        ///
        /// Published by `Freeze`, which is also what drops these decls from `Decls`.
        /// Both halves of that are deliberate and independent: an inline decl is NOT
        /// emittable (no backend has a lowering for a template), but it IS part of the
        /// unit's exported vocabulary — a consumer splices it. Dropping it from `Decls`
        /// without publishing it here would erase it from the unit's surface entirely.
        ///
        /// EMPTY pre-freeze: the SemType tree still carries the templates in `Decls`.
        InlineBodies: EqArray<TInlineValueG<'ty, 'tok>>
        /// Declared accessibility of each EXPORTED entity (type / member / module
        /// value / inline value), keyed by its `SymbolKey`. Stored HONESTLY (not
        /// pre-thresholded): the file→file projection applies internal-or-better, the
        /// `.fsi` extractor public-only, over the SAME fact. Captured by `Elaborate`
        /// from the CST `access` tokens. A key ABSENT here is `Public` (the F# default
        /// for an unmarked declaration). `SymbolKey`-keyed and `'ty`-free — carried
        /// verbatim across the freeze, modeled on `IntrinsicReprKeys`.
        Accessibility: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, Accessibility>
        /// A module binding's SOURCE `ValRepr` (curried / tupled group structure),
        /// keyed by the binding's headPat `NodeKey`. Computed UPSTREAM at FREEZE
        /// (`TastLower.peelValRepr`, while the lambda spine is intact) — backend-neutral,
        /// so the codegen boundary and the file→file signature projection read ONE
        /// grouping. A value (no lambda groups) records an empty-`Groups` entry, which
        /// the projection reads as "not a function" (`ExternalSymbol.ValRepr = ValueNone`).
        /// EMPTY pre-freeze — `Freeze.run` fills it. Lives here, not on
        /// `ModuleBindingInfo`, because `ValReprG` is defined in this file (a
        /// compile-order wall: `ModuleBindingInfo` in `SideTypes.fs` precedes it).
        BindingValReprs: Map<NodeKey, ValReprG<'ty, 'tok>>
        /// A module binding's single value/function typar-axis width, minted where the
        /// method-axis indices are minted (`Elaborate.mkMethodQuantEnv`). Keyed by the
        /// binding's headPat `NodeKey`; the projection reads it for
        /// `ExternalSymbol.TyparArity` and to size each binding's frozen `ValRepr`.
        BindingTyparArities: Map<NodeKey, int>
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
type DynFormatHole = DynFormatHoleG<SemType, SyntaxToken>
type TStaticOptClause = TStaticOptClauseG<SemType, SyntaxToken>
type TDecl = TDeclG<SemType, SyntaxToken>
type TTypeDecl = TTypeDeclG<SemType, SyntaxToken>
type TTypeKind = TTypeKindG<SemType, SyntaxToken>
type TClass = TClassG<SemType, SyntaxToken>
type TUnionCase = TUnionCaseG<SemType>
type TEnumCase = TEnumCaseG<SyntaxToken>
type TRecordField = TRecordFieldG<SemType>
type TTypeMember = TTypeMemberG<SemType, SyntaxToken>
type TClassLet = TClassLetG<SemType, SyntaxToken>
type TPreambleEntry = TPreambleEntryG<SemType, SyntaxToken>
type TCtorLet = TCtorLetG<SemType, SyntaxToken>
type TCtorFieldInit = TCtorFieldInitG<SemType, SyntaxToken>
type TSecondaryCtor = TSecondaryCtorG<SemType, SyntaxToken>
type TBaseCtorCall = TBaseCtorCallG<SemType, SyntaxToken>
type TAbstractMethod = TAbstractMethodG<SemType>
type TInlineBody = TInlineBodyG<SemType, SyntaxToken>
type TInlineValue = TInlineValueG<SemType, SyntaxToken>
type TastFile = TastFileG<SemType, SyntaxToken>

[<RequireQualifiedAccess>]
module TTypeKindG =
    /// The augmentation / instance members a type kind carries, uniform across the
    /// three member-bearing kinds (class / union / record). `Interface` (abstract,
    /// bodyless) and `Enum` (literal cases only) carry none. The single accessor for
    /// "the members of a type declaration", shared by the member-inline harvest
    /// (`SymbolProviders.collectInlineBodies`) and `ConformanceTypars.bodyMembers` —
    /// so neither hard-codes a single kind and a `(# … #)` member on any host is
    /// harvested, not silently dropped.
    let members (kind: TTypeKindG<'ty, 'tok>) : EqArray<TTypeMemberG<'ty, 'tok>> =
        match kind with
        | TTypeKindG.Class c -> c.Members
        | TTypeKindG.Union(_, members, _) -> members
        | TTypeKindG.Record(_, members, _, _) -> members
        | TTypeKindG.Interface _
        | TTypeKindG.Enum _ -> EqArray.empty

[<RequireQualifiedAccess>]
module TPreambleEntryG =
    /// The `let` binders of a class preamble, in declaration order — the entries that take a
    /// backing field (a `do` has storage nowhere, only an effect). Generic in `'ty`/`'tok`, so
    /// this ONE projection serves every consumer — inference-time TAST, frozen TAST, and both
    /// backends — rather than one copy per stage.
    let lets (entries: seq<TPreambleEntryG<'ty, 'tok>>) : TClassLetG<'ty, 'tok> list =
        [
            for e in entries do
                match e with
                | TPreambleEntryG.Let l -> yield l
                | TPreambleEntryG.Do _ -> ()
        ]

// Parallel frozen aliases. Codegen and the freeze step speak these; the bare names
// above STAY `SemType` (inference, Regions, tests, any non-codegen API).

module Frozen =
    type TPat = TPatG<FrozenType, SyntaxToken>
    type HoleSpec = HoleSpecG<FrozenType, SyntaxToken>
    type TExpr = TExprG<FrozenType, SyntaxToken>
    type TMatchArm = TMatchArmG<FrozenType, SyntaxToken>
    type FormatSink = FormatSinkG<FrozenType, SyntaxToken>
    type FormatSeg = FormatSegG<FrozenType, SyntaxToken>
    type DynFormatHole = DynFormatHoleG<FrozenType, SyntaxToken>
    type TStaticOptConstraint = TStaticOptConstraintG<FrozenType>
    type TStaticOptClause = TStaticOptClauseG<FrozenType, SyntaxToken>
    type TDecl = TDeclG<FrozenType, SyntaxToken>
    type TTypeDecl = TTypeDeclG<FrozenType, SyntaxToken>
    type TTypeKind = TTypeKindG<FrozenType, SyntaxToken>
    type TClass = TClassG<FrozenType, SyntaxToken>
    type TUnionCase = TUnionCaseG<FrozenType>
    // Enum cases are `'ty`-free, so the frozen alias is identical to the SemType one.
    type TEnumCase = TEnumCaseG<SyntaxToken>
    type TRecordField = TRecordFieldG<FrozenType>
    type TTypeMember = TTypeMemberG<FrozenType, SyntaxToken>
    type TClassLet = TClassLetG<FrozenType, SyntaxToken>
    type TPreambleEntry = TPreambleEntryG<FrozenType, SyntaxToken>
    type TCtorLet = TCtorLetG<FrozenType, SyntaxToken>
    type TCtorFieldInit = TCtorFieldInitG<FrozenType, SyntaxToken>
    type TSecondaryCtor = TSecondaryCtorG<FrozenType, SyntaxToken>
    type TBaseCtorCall = TBaseCtorCallG<FrozenType, SyntaxToken>
    type TAbstractMethod = TAbstractMethodG<FrozenType>
    type TInlineBody = TInlineBodyG<FrozenType, SyntaxToken>
    type TInlineValue = TInlineValueG<FrozenType, SyntaxToken>
    type TastFile = TastFileG<FrozenType, SyntaxToken>
    type ForInEnumerator = ForInEnumeratorG<FrozenType>
    type StaticParam = StaticParamG<FrozenType, SyntaxToken>
    type ArgGroup = ArgGroupG<FrozenType, SyntaxToken>
    type ValRepr = ValReprG<FrozenType, SyntaxToken>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, SyntaxToken>
