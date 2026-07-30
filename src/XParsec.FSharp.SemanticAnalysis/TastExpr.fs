namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// The TERM shapes of the TAST: constants, patterns, expressions, and the compiled-form
// cluster (a function's SOURCE arity and the flat signature derived from it). Declarations
// are `TastDecl.fs`, which reads these shapes and not the reverse — a declaration's bodies
// are expressions; the unit-level file shape and the monomorphic instantiations of every
// name here are `Tast.fs`.
//
// Each node carries its inferred SemType inline so target lowering doesn't
// need to re-query the side tables.

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
type TPatG<'ty, 'tok, 'id> =
    /// `binding` is the identity of the introducing pattern; references via
    /// `TExpr.Var` name the same one.
    | NamedSimple of binding: 'id * ty: 'ty * tok: 'tok
    /// `_` placeholder. Has a type (the matched value's type) but binds nothing.
    | Wildcard of ty: 'ty * tok: 'tok
    /// `ty` is always a `TyTuple` of the elements' types.
    | Tuple of items: EqArray<TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    | Const of value: TConstValue * ty: 'ty * tok: 'tok
    /// `ty` is a `TyRecord`. May list a subset of the record's fields; unlisted
    /// fields are simply not bound.
    | Record of fields: EqArray<string * TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `fields` is the per-field sub-pattern list, empty for nullary cases. `ty`
    /// is always a `TyUnion`. The declaring union is recoverable via
    /// `ctx.Types.CtorIndex[caseName]` at consumption time.
    | Union of caseName: string * fields: EqArray<TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `:? testTy as x` type-test pattern. Refutable: codegen lowers it to an
    /// `isinst testTy` + null check (branch to the next arm on mismatch), then
    /// binds `inner` (the `as`-name, an irrefutable sub-pattern) against the
    /// cast-down value. `ty` is the scrutinee's type (the matched value — `obj`
    /// in practice); `testTy` is the tested-against type the binder sees.
    | TypeTestAs of testTy: 'ty * inner: TPatG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
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
    | Or of alts: EqArray<TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok

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
type TExprG<'ty, 'tok, 'id> =
    | Const of value: TConstValue * ty: 'ty * tok: 'tok
    /// `binding` is the identity of the *binding site*, not the use site.
    | Var of binding: 'id * ty: 'ty * tok: 'tok
    /// Symbol resolved through IExternalSymbolProvider. Carries the compiled
    /// name so target plugins can dispatch (`op_Addition` -> CIL `add` on
    /// .NET, native `+` on Rust, etc. — the inline IL is target-specific).
    /// `key` interns the resolved `SymbolKey` so codegen reads the binding off the
    /// node instead of re-resolving by name;
    /// `ValueNone` until Elaborate stamps it — every site is name-only today.
    | External of compiledName: string * key: SymbolKey voption * ty: 'ty * tok: 'tok
    | Lambda of param: TPatG<'ty, 'tok, 'id> * body: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    | App of fn: TExprG<'ty, 'tok, 'id> * arg: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    | Let of
        binding: TPatG<'ty, 'tok, 'id> *
        value: TExprG<'ty, 'tok, 'id> *
        body: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// `use x = value in body`. Same shape as `Let`; the distinction is that
    /// codegen wraps `body` in a `try … finally x.Dispose()` exception region so
    /// `x` is disposed on every exit. `ty` is the body's type — the expression's
    /// result. `dispose` is the resolved disposal path (see `Disposal`).
    | Use of
        binding: TPatG<'ty, 'tok, 'id> *
        value: TExprG<'ty, 'tok, 'id> *
        body: TExprG<'ty, 'tok, 'id> *
        dispose: Disposal *
        ty: 'ty *
        tok: 'tok
    | IfThenElse of
        cond: TExprG<'ty, 'tok, 'id> *
        thenExpr: TExprG<'ty, 'tok, 'id> *
        elseExpr: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// `ty` is always a TyTuple of the elements' inferred types.
    | Tuple of items: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// All items but the last must have unit type; `ty` is the last item's type.
    | Sequential of items: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `ty` is always unit; cond : bool, body : unit.
    | While of cond: TExprG<'ty, 'tok, 'id> * body: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `ty` is always unit; the loop variable is bound to `var` with type int.
    /// `startExpr`, `endExpr`, `body` are int, int, unit respectively.
    | ForTo of
        var: 'id *
        identTok: 'tok *
        startExpr: TExprG<'ty, 'tok, 'id> *
        endExpr: TExprG<'ty, 'tok, 'id> *
        body: TExprG<'ty, 'tok, 'id> *
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
        pat: TPatG<'ty, 'tok, 'id> *
        source: TExprG<'ty, 'tok, 'id> *
        body: TExprG<'ty, 'tok, 'id> *
        enumerator: ForInEnumeratorG<'ty> *
        ty: 'ty *
        tok: 'tok
    /// `scrutinee` and each `arms.[i].Pat` share the same type; every
    /// `arms.[i].Body` shares `ty`. `function` desugars to a Match over a
    /// synthetic parameter — same TExpr shape.
    | Match of
        scrutinee: TExprG<'ty, 'tok, 'id> *
        arms: EqArray<TMatchArmG<TPatG<'ty, 'tok, 'id>, TExprG<'ty, 'tok, 'id>>> *
        ty: 'ty *
        tok: 'tok
    /// `try body with | pat -> arm`. `body` and every `arms.[i].Body`
    /// share `ty`; arm patterns currently bind against a fresh TypeVar
    /// (no `exn` type yet).
    | TryWith of
        body: TExprG<'ty, 'tok, 'id> *
        arms: EqArray<TMatchArmG<TPatG<'ty, 'tok, 'id>, TExprG<'ty, 'tok, 'id>>> *
        ty: 'ty *
        tok: 'tok
    /// `try body finally cleanup`. `body` carries `ty`; `cleanup` is unit.
    | TryFinally of body: TExprG<'ty, 'tok, 'id> * cleanup: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `lhs <- rhs`. Always types as unit.
    | Assignment of lhs: TExprG<'ty, 'tok, 'id> * rhs: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
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
        startExpr: TExprG<'ty, 'tok, 'id> *
        step: TExprG<'ty, 'tok, 'id> option *
        stopExpr: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// `{ X = e1; Y = e2 }` record literal. `ty` is a `TyRecord`; field
    /// list is in source order (the unification pass already validated
    /// that the field set matches the record's declared set).
    | RecordCons of fields: EqArray<string * TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `{ r with X = v; … }`. `source` types as the same `TyRecord` as
    /// `ty`; `overrides` is the source-order list of `(name, replacement)`
    /// for the listed fields. Unlisted fields are copied from `source` at
    /// the runtime level — not represented in the TAST.
    | RecordClone of
        source: TExprG<'ty, 'tok, 'id> *
        overrides: EqArray<string * TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    /// `r.X` — `ty` is the field's declared type. `receiver` types as a
    /// `TyRecord`.
    | FieldGet of receiver: TExprG<'ty, 'tok, 'id> * fieldName: string * ty: 'ty * tok: 'tok
    /// `r.X <- v` — `ty` is unit. `receiver` types as a `TyRecord` whose
    /// field `fieldName` is mutable (Validation enforces).
    | FieldSet of
        receiver: TExprG<'ty, 'tok, 'id> *
        fieldName: string *
        value: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// Discriminated-union constructor application. `args` length matches
    /// the ctor's declared arity (0 for nullary). `ty` is a `TyUnion`.
    /// Nullary ctors (`Point`) and applied ctors (`Circle 1.0`,
    /// `Rectangle(2.0, 3.0)`) both fold to this node — the latter peels
    /// the `Expr.App` chain in Elaborate.
    | UnionCons of caseName: string * args: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
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
    | New of className: string * key: SymbolKey voption * args: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// Instance method invocation: `r.M(args)`. `args` is the
    /// per-parameter list (peeled the same way as `New`). `ty` is the
    /// method's declared return type. `key` is the resolved local
    /// `SymbolKey.MemberKey` (declaring type + member name) — codegen reads the declaring type off `key.decl` and the member
    /// name off `key.memberName` instead of re-deriving from a class-name string.
    | MethodCall of
        receiver: TExprG<'ty, 'tok, 'id> *
        key: SymbolKey *
        via: CallVia<'ty> *
        args: EqArray<TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    | PropertyGet of receiver: TExprG<'ty, 'tok, 'id> * key: SymbolKey * via: CallVia<'ty> * ty: 'ty * tok: 'tok
    /// Same arg-peeling as `MethodCall`; no receiver. `key` is the resolved local
    /// `SymbolKey.MemberKey`.
    | StaticMethodCall of key: SymbolKey * args: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
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
    | StaticFieldSet of declKey: SymbolKey * fieldName: string * value: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
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
        receiver: TExprG<'ty, 'tok, 'id> voption *
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
    | Format of
        sink: FormatSinkG<TExprG<'ty, 'tok, 'id>> *
        segments: EqArray<FormatSegG<'ty, 'tok, TExprG<'ty, 'tok, 'id>>> *
        ty: 'ty *
        tok: 'tok
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
    | ILIntrinsic of
        opCode: string *
        typeOperand: 'ty voption *
        args: EqArray<TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    /// F# library-only static optimization: a default expression plus a list of
    /// type-specialized clauses (`expr when ^T : int = … when ^T : ^T = …`).
    /// `clauses` are in source order; at `let inline` expansion the first clause
    /// whose constraints hold for the monomorphised type arguments is selected,
    /// else `defaultExpr`. Every clause body and `defaultExpr` share `ty` (an
    /// equality-family operator returns `bool` under every clause). Codegen does
    /// **not** emit this node directly — `Inline.inlineExpand` resolves it to the
    /// chosen branch once the call site pins the operand type (prereq 3).
    | StaticOptimization of
        clauses: EqArray<TStaticOptClauseG<'ty, 'tok, 'id>> *
        defaultExpr: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// `e :> T` static upcast. `source`'s runtime type
    /// is a subtype of `ty` (validated by Unification's `subsumes`). Codegen
    /// erases it for ref types (the JIT treats a derived reference as the base)
    /// and emits `box` for a value-type source.
    | Upcast of source: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `e :?> T` checked downcast. `ty` is the (more-specific) target type;
    /// codegen emits `castclass` for ref types / `unbox.any` for value types,
    /// so a runtime mismatch throws `InvalidCastException`.
    | Downcast of source: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `e :? T` type test. `testTy` is the tested-against type `T` (the
    /// `isinst` operand); `ty` is always `TyConst "bool"` (the result). Codegen
    /// emits `isinst <testTy>; ldnull; cgt.un`.
    | TypeTest of source: TExprG<'ty, 'tok, 'id> * testTy: 'ty * ty: 'ty * tok: 'tok
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
    | TraitCall of receiver: 'ty * memberName: string * args: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// A call to a RESOLVED INLINE SPECIALIZATION: the `spec`-th entry of the file's
    /// specialization table (`TastFileG.Specializations`), applied to `args`. The body is
    /// NOT here — it stays in the table — so a body reached from N call sites is one entry
    /// and N edges, and the provenance of each edge is its own.
    ///
    /// `tok` is the CALL SITE, in the file this node belongs to; the entry's nodes are
    /// anchored where the body was WRITTEN. Keeping the two apart is the whole point of the
    /// node: a physical splice maps every node of the body onto the call-site token, after
    /// which nothing can say which file a node came from.
    ///
    /// `args` are positional against the entry's SURVIVING parameters — an entry is a
    /// `TDecl.Let` of lambdas, and a parameter that resolution fused into the body is not
    /// one of them, so the count agrees by construction rather than by a stored arity.
    | InlineCall of spec: SpecializationId * args: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// A CONTEXT POP — the dual of `InlineCall`'s push, and the only thing that makes a
    /// specialization entry honest about the material a reduction FUSED into it.
    ///
    /// Descending through an `InlineCall` means an `Anchor` indexes the entry's `OriginFile`.
    /// Everything under a `CallerExpr` is instead anchored in the file of whatever called
    /// that entry — one frame out. So `a && b` outlines as `if a then ⟨CallerExpr b⟩ else
    /// false`: the `if`/`then`/`else` were written in the operator's own file, the `b`
    /// subtree at the call site.
    ///
    /// It pops ONE frame RELATIVELY and names no file. Origins live on entries and never on
    /// nodes — the premise the whole table rests on — and popping relatively is also what
    /// makes nesting free: a fused argument that is itself an inline call reads push → pop →
    /// push with nothing to reconcile.
    ///
    /// "The caller" is unambiguous only because an entry holding fused material has exactly
    /// ONE call edge; a SHAREABLE (closed) entry must therefore never contain this node.
    /// That is what licenses the node rather than a property it happens to have, and
    /// `Passes.InlineExpansion` checks it on every entry it interns.
    ///
    /// SEMANTICALLY TRANSPARENT: it evaluates to its body and nothing else, and the emit-time
    /// expansion unwraps it once the frame it popped back to is the frame its parent sits in.
    /// It is not a way to defer an argument — the fusion it marks already IS the
    /// deferral, which is why a fused argument cannot instead ride on the edge's eager `args`
    /// (`&&` must not evaluate `b` unless `a` is true).
    ///
    /// `ty` and `tok` are its BODY's, always (`TastWalk.callerExpr` is the only constructor).
    /// The pop applies at this node, so its position reads in the caller's domain exactly as
    /// its body's does, and there is no position of its own to get wrong.
    | CallerExpr of body: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok

/// One arm of a `Match` / `TryWith`. `'pat`/`'e` abstract over how the arm's pattern and
/// its guard/body expressions are carried, exactly as `'body` does for a type
/// declaration's member bodies: either the trees themselves or the handles naming them
/// in a pool. An arm is a COMPOSITE CARRIER with no node identity of its own — the pool
/// flattens its pieces into the child columns — so the re-nesting that puts them back
/// (`ExprPayload.arms`) is written once, against this one shape, rather than once per
/// domain.
and TMatchArmG<'pat, 'e> =
    {
        Pat: 'pat
        Guard: 'e voption
        Body: 'e
    }

/// Kept abstract from CLR specifics so an alternate target (JS → template
/// literal) maps it independently. `ToWriter`/`ToBuilder` carry the explicit
/// sink expression (`fprintf` / `bprintf`); P1 produces all five when fully
/// applied (`ToWriter` for `fprintf`/`fprintfn`, `ToBuilder` for `bprintf`).
/// `ToWriter`'s `newline` records the trailing `\n` (`fprintfn` sets it,
/// `fprintf` does not), mirroring `ToStdOut`/`ToStdErr`; `ToBuilder` carries no
/// newline (F# has no `bprintfn`). `'e` abstracts over how the sink's own
/// sub-expression is carried — see `TMatchArmG`, which the format cluster mirrors: a
/// sink and its segments are composite carriers the pool flattens, re-nested once by
/// `ExprPayload.format`.
and [<RequireQualifiedAccess>] FormatSinkG<'e> =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of writer: 'e * newline: bool
    | ToBuilder of 'e
    | ToString

and [<RequireQualifiedAccess>] FormatSegG<'ty, 'tok, 'e> =
    | Lit of string
    | Hole of HoleSpecG<'ty, 'tok> * 'e
    /// A hole with one or both dimensions supplied as runtime arguments — star width
    /// (`%*d`, `%-*d`, `%*A`) and/or star precision (`%.*f`, `%*.*f`, `%.*e`, `%.*g`,
    /// `%+.*f`, `%.*A`). The curried application evaluates the dimension args *before*
    /// the value, in source order (width, then precision), so they ride here as
    /// `Width`/`Precision` (each present iff its dimension is a star; at least one is).
    /// Invariant: `Spec.Source` classifies to a form whose `Alignment.Star` /
    /// `PrintWidth.Star` (width) and `Prec.Star` / `PrintSize.Star` (precision) agree
    /// with which of `Width`/`Precision` are present — all constructed from the same
    /// placeholder.
    | DynHole of DynFormatHoleG<'ty, 'tok, 'e>
    /// A `%a` / `%t` printer-callback hole (`HoleForm.Callback`), lowered
    /// capture-first to an ordinary residue-*string* expression Elaborate synthesises:
    /// `sprintf` splices the callback's returned string (`cb unit [value]`); the
    /// writer/builder families splice `{ let s = new Scratch() in cb s [value]; s.ToString() }`.
    /// `residue` is therefore just a `string`-typed `TExpr` (the callback + value ride
    /// inside it as ordinary sub-exprs, so every traversal walks it with no special
    /// arm), and both backends emit it exactly as a `%s` hole — no sink knowledge in
    /// codegen. The segment stays distinct only to record `%a`/`%t` provenance.
    | CallbackHole of spec: HoleSpecG<'ty, 'tok> * residue: 'e

/// A `FormatSegG.DynHole` payload: the hole's spec + value, plus whichever
/// dimension args the curried application supplies at runtime. `Width` is present
/// iff the width is a star (`%*…`), `Precision` iff the precision is a star
/// (`%.*…`); at least one is present (a plain hole stays `FormatSegG.Hole`). Fields
/// are named (not a wide tuple) so consumers read `Width`/`Precision` by intent.
and DynFormatHoleG<'ty, 'tok, 'e> =
    {
        Width: 'e voption
        Precision: 'e voption
        Spec: HoleSpecG<'ty, 'tok>
        Value: 'e
    }

/// One clause of a `TExpr.StaticOptimization`. `Constraints` is the `and`-joined
/// list (all must hold; the constraint type is declared in `SemanticInfo.fs` so the
/// side table can carry it, but is `'ty`-generic like everything else here, so a
/// clause freezes/thaws WHOLE — no `SemType` rides inside a frozen clause); `Body`
/// is the clause's optimized expression.
and TStaticOptClauseG<'ty, 'tok, 'id> =
    {
        Constraints: EqArray<TStaticOptConstraintG<'ty>>
        Body: TExprG<'ty, 'tok, 'id>
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
///
/// `'pat` abstracts over how the destructuring pattern is carried, and `'id` over the
/// identity the slot is named in — both exactly as `ArgGroupG` does, this being the
/// flattening of one.
and StaticParamG<'ty, 'pat, 'id> =
    { Slot: 'id; Ty: 'ty; Pat: 'pat option }

/// One curried argument group of a function's SOURCE signature — the distinction
/// the flat compiled signature loses. `GUnit` (`fun () -> …`) erases to zero
/// params when it is the sole group; `GSimple` is one non-tuple param; `GTuple`
/// (`fun (a, b, …) -> …`) carries the whole tuple pattern, which full-F#
/// flattening expands to one flat param per element.
///
/// `'pat` abstracts over how the tuple pattern is carried, exactly as `'body` does
/// for a type declaration's member bodies: either the pattern tree itself
/// (`TPatG<'ty,'tok>`) or a dense id naming it in a pool. The two instantiations are
/// NOT interchangeable and the split is deliberate — a FILE's own `ValRepr` is pooled
/// alongside the tree it was peeled from, whereas an EXTERNAL symbol's is minted from
/// an `.fsi` contract (`TastLower.externalValRepr`) and belongs to no file, so it has
/// no pool to index into and stays at the tree.
///
/// `GSimple`'s slot rides the tree's identity axis for the same reason every other
/// reference does: a file's own arity is DERIVED from its pooled spine, so the parameter
/// it names is the `BinderId` that spine already addresses, while a contract-minted arity
/// belongs to no pool and stays in the node space.
and [<RequireQualifiedAccess>] ArgGroupG<'ty, 'pat, 'id> =
    | GUnit of ty: 'ty
    | GSimple of slot: 'id * ty: 'ty
    | GTuple of pat: 'pat

/// The SOURCE signature — the `ValReprInfo` analogue. `Groups.Length` is the
/// number of applications a saturated call consumes; `ResultTy` is the source
/// (NOT unit-erased) result type.
and ValReprG<'ty, 'pat, 'id> =
    {
        Typars: int
        Groups: ArgGroupG<'ty, 'pat, 'id> list
        ResultTy: 'ty
    }

/// The compiled return: `RVoid` is a unit result (CLR `void` / JS no-value).
and [<RequireQualifiedAccess>] CompiledReturnG<'ty> =
    | RVoid
    | RValue of 'ty

/// The flat compiled signature derived from a `ValReprG`: tuple-flattened,
/// lone-unit-erased parameters and the `void`-normalised return.
and CompiledFormG<'ty, 'pat, 'id> =
    {
        Params: StaticParamG<'ty, 'pat, 'id> list
        Return: CompiledReturnG<'ty>
    }
