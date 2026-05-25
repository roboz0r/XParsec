namespace XParsec.FSharp.SemanticAnalysis

// TAST does NOT preserve trivia, parens, or token layout — tooling consumers
// query the CST for that. TAST exists for consumers that only care about
// semantics (codegen, target plugins).
//
// Each node carries its inferred SemType inline so target lowering doesn't
// need to re-query the side tables.

[<RequireQualifiedAccess>]
type TConstValue =
    | Int of int
    | Int64 of int64
    | Byte of byte
    | Float of double
    | Bool of bool
    | Char of char
    | Decimal of decimal
    | String of string
    | Unit

/// Keeps the destructuring shape so a downstream consumer can introduce every
/// bound name without re-walking the CST.
[<RequireQualifiedAccess>]
type TPat =
    /// `binding` is the source-position NodeKey of the introducing pattern;
    /// references via `TExpr.Var` use the same key.
    | NamedSimple of binding: NodeKey * ty: SemType
    /// `_` placeholder. Has a type (the matched value's type) but binds nothing.
    | Wildcard of ty: SemType
    /// `ty` is always a `TyTuple` of the elements' types.
    | Tuple of items: TPat list * ty: SemType
    | Const of value: TConstValue * ty: SemType
    /// `ty` is a `TyRecord`. May list a subset of the record's fields; unlisted
    /// fields are simply not bound.
    | Record of fields: (string * TPat) list * ty: SemType
    /// `fields` is the per-field sub-pattern list, empty for nullary cases. `ty`
    /// is always a `TyUnion`. The declaring union is recoverable via
    /// `ctx.CtorIndex[caseName]` at consumption time.
    | Union of caseName: string * fields: TPat list * ty: SemType

[<RequireQualifiedAccess>]
type TExpr =
    | Const of value: TConstValue * ty: SemType
    /// `binding` is the NodeKey of the *binding site*, not the use site.
    | Var of binding: NodeKey * ty: SemType
    /// Symbol resolved through IExternalSymbolProvider. Carries the compiled
    /// name so target plugins can dispatch (`op_Addition` -> CIL `add` on
    /// .NET, native `+` on Rust, etc. — see [[project_inline_il_target_specific]]).
    | External of compiledName: string * ty: SemType
    | Lambda of param: TPat * body: TExpr * ty: SemType
    | App of fn: TExpr * arg: TExpr * ty: SemType
    | Let of binding: TPat * value: TExpr * body: TExpr * ty: SemType
    | IfThenElse of cond: TExpr * thenExpr: TExpr * elseExpr: TExpr * ty: SemType
    /// `ty` is always a TyTuple of the elements' inferred types.
    | Tuple of items: TExpr list * ty: SemType
    /// All items but the last must have unit type; `ty` is the last item's type.
    | Sequential of items: TExpr list * ty: SemType
    /// `ty` is always unit; cond : bool, body : unit.
    | While of cond: TExpr * body: TExpr * ty: SemType
    /// `ty` is always unit; the loop variable is bound to `var` with type int.
    /// `startExpr`, `endExpr`, `body` are int, int, unit respectively.
    | ForTo of var: NodeKey * startExpr: TExpr * endExpr: TExpr * body: TExpr * ty: SemType
    /// `ty` is always unit. `pat`'s type matches the element type of `source`
    /// — pinned to `int` for range sources, left as a free TypeVar otherwise.
    /// `body` types as unit.
    | ForIn of pat: TPat * source: TExpr * body: TExpr * ty: SemType
    /// `scrutinee` and each `arms.[i].Pat` share the same type; every
    /// `arms.[i].Body` shares `ty`. `function` desugars to a Match over a
    /// synthetic parameter — same TExpr shape.
    | Match of scrutinee: TExpr * arms: TMatchArm list * ty: SemType
    /// `try body with | pat -> arm`. `body` and every `arms.[i].Body`
    /// share `ty`; arm patterns currently bind against a fresh TypeVar
    /// (no `exn` type yet).
    | TryWith of body: TExpr * arms: TMatchArm list * ty: SemType
    /// `try body finally cleanup`. `body` carries `ty`; `cleanup` is unit.
    | TryFinally of body: TExpr * cleanup: TExpr * ty: SemType
    /// `lhs <- rhs`. Always types as unit.
    | Assignment of lhs: TExpr * rhs: TExpr * ty: SemType
    /// `null` literal. `ty` is left as a free TypeVar in the tiny subset —
    /// real F# would constrain it to a reference type.
    | Null of ty: SemType
    /// `start..stop` or `start..step..stop`. Endpoints (and step) all type
    /// as int in the tiny subset; `ty` is `seq<int>` (a TyConst placeholder
    /// — see [[MockBuiltins.tySeqInt]]).
    | Range of startExpr: TExpr * step: TExpr option * stopExpr: TExpr * ty: SemType
    /// `{ X = e1; Y = e2 }` record literal. `ty` is a `TyRecord`; field
    /// list is in source order (the unification pass already validated
    /// that the field set matches the record's declared set).
    | RecordCons of fields: (string * TExpr) list * ty: SemType
    /// `{ r with X = v; … }`. `source` types as the same `TyRecord` as
    /// `ty`; `overrides` is the source-order list of `(name, replacement)`
    /// for the listed fields. Unlisted fields are copied from `source` at
    /// the runtime level — not represented in the TAST.
    | RecordClone of source: TExpr * overrides: (string * TExpr) list * ty: SemType
    /// `r.X` — `ty` is the field's declared type. `receiver` types as a
    /// `TyRecord`.
    | FieldGet of receiver: TExpr * fieldName: string * ty: SemType
    /// `r.X <- v` — `ty` is unit. `receiver` types as a `TyRecord` whose
    /// field `fieldName` is mutable (Validation enforces).
    | FieldSet of receiver: TExpr * fieldName: string * value: TExpr * ty: SemType
    /// Discriminated-union constructor application. `args` length matches
    /// the ctor's declared arity (0 for nullary). `ty` is a `TyUnion`.
    /// Nullary ctors (`Point`) and applied ctors (`Circle 1.0`,
    /// `Rectangle(2.0, 3.0)`) both fold to this node — the latter peels
    /// the `Expr.App` chain in Freeze.
    | UnionCons of caseName: string * args: TExpr list * ty: SemType
    /// Class primary-constructor invocation. `args` is the per-parameter
    /// list — the parser's tuple wrapper (`new Point(3, 4)` parses with
    /// a `Tuple` arg) is peeled in Freeze so consumers see the ctor's
    /// declared arity directly. `ty` is a `TyClass`.
    | New of className: string * args: TExpr list * ty: SemType
    /// Instance method invocation: `r.M(args)`. `args` is the
    /// per-parameter list (peeled the same way as `New`). `ty` is the
    /// method's declared return type.
    | MethodCall of receiver: TExpr * methodName: string * args: TExpr list * ty: SemType
    | PropertyGet of receiver: TExpr * propertyName: string * ty: SemType
    /// Same arg-peeling as `MethodCall`; no receiver.
    | StaticMethodCall of className: string * methodName: string * args: TExpr list * ty: SemType
    | StaticPropertyGet of className: string * propertyName: string * ty: SemType
    /// Lowered printf / string-interpolation (vesper-printf-plan P1, D9):
    /// `segments` is the interleaved literal / hole sequence in source order,
    /// each hole carrying its argument expression inline (codegen folds left to
    /// right, evaluating each arg at its hole). NOT a generic saturated call —
    /// the format literal rewrites the call's arity/arg-types, so the node carries
    /// printf semantics a generic call node cannot. `ty` is the call's result
    /// (`unit` for `printf`/`printfn`, `string` for `sprintf`).
    | Format of sink: FormatSink * segments: EqArray<FormatSeg> * ty: SemType
    /// Value-level inline IL: `(# "opcode" args : retTy #)`. `opCode` is the
    /// stitched instruction mnemonic (e.g. `"ceq"`, `"add"`), `args` the operand
    /// expressions in source order, `ty` the declared result type. Codegen emits
    /// each arg then maps the mnemonic → `ILOpCode` (`Cil.tryOpCodeOfMnemonic`).
    /// The value-level sibling of the type-level `(# "..." #)` intrinsic carried
    /// in `TastFile.IntrinsicReprTypes`; operator `.fs` bodies (`(=)` → `ceq`,
    /// `(+)` → `add`, …) lower to this so codegen owns no per-operator dispatch.
    /// See docs/core-operators-handoff.md.
    | ILIntrinsic of opCode: string * args: TExpr list * ty: SemType
    /// F# library-only static optimization: a default expression plus a list of
    /// type-specialized clauses (`expr when ^T : int = … when ^T : ^T = …`).
    /// `clauses` are in source order; at `let inline` expansion the first clause
    /// whose constraints hold for the monomorphised type arguments is selected,
    /// else `defaultExpr`. Every clause body and `defaultExpr` share `ty` (an
    /// equality-family operator returns `bool` under every clause). Codegen does
    /// **not** emit this node directly — `Inline.inlineExpand` resolves it to the
    /// chosen branch once the call site pins the operand type (prereq 3). See
    /// docs/core-operators-handoff.md.
    | StaticOptimization of clauses: TStaticOptClause list * defaultExpr: TExpr * ty: SemType

and TMatchArm =
    {
        Pat: TPat
        Guard: TExpr option
        Body: TExpr
    }

/// Kept abstract from CLR specifics so an alternate target (JS → template
/// literal) maps it independently. `ToWriter`/`ToBuilder` carry the explicit
/// sink expression (`fprintf` / `bprintf`); P1 produces only the first three.
and [<RequireQualifiedAccess>] FormatSink =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of TExpr
    | ToBuilder of TExpr
    | ToString

/// `Ty` is the static type (drives `AppendFormatted<T>`, no box). `Alignment` is
/// the field width (negative ⇒ left-justify). `Kind`/`Format`/`Alignment` are
/// produced by `PrintfSpec.tryHoleFormat`.
and HoleSpec =
    {
        Ty: SemType
        Kind: PrintfSpec.HoleKind
        Format: string option
        Alignment: int option
    }

and [<RequireQualifiedAccess>] FormatSeg =
    | Lit of string
    | Hole of HoleSpec * TExpr

/// One clause of a `TExpr.StaticOptimization`. `Constraints` is the `and`-joined
/// list (all must hold; declared in `SemanticInfo.fs` so the side table can carry
/// it); `Body` is the clause's optimized expression.
and TStaticOptClause =
    {
        Constraints: TStaticOptConstraint list
        Body: TExpr
    }

[<RequireQualifiedAccess>]
type TDecl =
    /// The `value` body is retained verbatim regardless; when `isInline` is set
    /// the flag tells codegen it may expand the body per call site (via
    /// `Inline.inlineExpand`) rather than emit a single callable. See
    /// [front-end-gaps-plan](docs/front-end-gaps-plan.md) §C.
    | Let of binding: TPat * value: TExpr * isInline: bool * ty: SemType
    | Expression of expr: TExpr * ty: SemType
    /// Rung 1 (self-host) emits only the interface shape. Records / unions /
    /// classes are later rungs. See docs/self-host-rung1-plan.md.
    | Type of TTypeDecl

and TTypeDecl =
    {
        /// Simple (unqualified) type name, e.g. `"Fun"`. The metadata name gets
        /// the arity suffix (`` Fun`2 ``) from `TypeParams.Length`.
        Name: string
        /// `None` for a module-level type.
        Namespace: string option
        /// Declared type parameters in source order (e.g. `["'A"; "'B"]`).
        TypeParams: string list
        Kind: TTypeKind
    }

and [<RequireQualifiedAccess>] TTypeKind =
    /// A nominal type whose members are all abstract and which has no base type /
    /// field. Rung 1's only kind.
    | Interface of methods: TAbstractMethod list
    /// `cases` in declaration order (the index is the runtime tag), plus any
    /// augmentation members (`with member …` / `static member …`). See
    /// docs/self-host-rung2-plan.md.
    | Union of cases: TUnionCase list * members: TTypeMember list

/// `Fields` are the case's payload in declaration order; a field's name is
/// `ValueNone` when the source is positional (`Cons of 'T * list`). Empty
/// `Fields` ⇒ a nullary case (`Nil`).
and TUnionCase =
    {
        Name: string
        Fields: (string voption * SemType) list
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
and TTypeMember =
    {
        Name: string
        IsStatic: bool
        Kind: TMemberKind
        /// Instance members only; `ValueNone` for a static member.
        ThisKey: NodeKey voption
        /// The declaring type (a `TyUnion`) — the receiver type for an instance
        /// member's `this`.
        ThisTy: SemType
        /// Parameter binders in declaration order (each `ldarg` after `this` for
        /// an instance method); empty for a property or a nullary method.
        Params: (NodeKey * SemType) list
        Body: TExpr
        ReturnTy: SemType
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
and TAbstractMethod =
    {
        Name: string
        MethodTypeParams: string list
        Signature: SemType
    }

type TastFile =
    {
        /// Source order.
        Decls: TDecl list
        /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
        Diagnostics: Diagnostic list
        /// Vesper type name → target IL representation string (e.g. `"int"` →
        /// `"System.Int32"`), from this file's `type x = (# "..." #)` intrinsic
        /// abbrevs. A use site resolves to `TyConst name`; the backend keys the
        /// emitted IL type off the *representation string* (so a platform author
        /// retargets a primitive by editing one `.fs` line). The backend overlays
        /// these on its built-in defaults. See docs/selfhost-handoff.md (G7).
        IntrinsicReprTypes: Map<string, string>
        /// A module-level binding's `NodeKey.Raw` → its named-holder placement
        /// (`module Foo`'s functions emit on a real `Foo`/`FooModule` static class,
        /// not the anonymous "Program" holder). Empty for a program with no named
        /// modules — every static method then lands on "Program" as before.
        ModuleMembers: Map<uint64, ModuleMemberInfo>
    }
