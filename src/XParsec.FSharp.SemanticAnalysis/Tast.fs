namespace XParsec.FSharp.SemanticAnalysis

// TAST does NOT preserve trivia, parens, or token layout — tooling consumers
// query the CST for that. TAST exists for consumers that only care about
// semantics (codegen, target plugins).
//
// Each node carries its inferred SemType inline so target lowering doesn't
// need to re-query the side tables.

/// Literal-value payload. TODO: extend with Char as the supported subset grows.
[<RequireQualifiedAccess>]
type TConstValue =
    | Int of int
    | Int64 of int64
    | Byte of byte
    | Float of double
    | Bool of bool
    | String of string
    | Unit

/// Binding-side pattern in the TAST. Strips trivia / parens from `Pat<T>`
/// but keeps the destructuring shape so a downstream consumer can introduce
/// every bound name without re-walking the CST.
[<RequireQualifiedAccess>]
type TPat =
    /// Single named binding. `binding` is the source-position NodeKey of the
    /// introducing pattern; references via `TExpr.Var` use the same key.
    | NamedSimple of binding: NodeKey * ty: SemType
    /// `_` placeholder. Has a type (the matched value's type) but binds
    /// nothing.
    | Wildcard of ty: SemType
    /// `(a, b, …)`. `ty` is always a `TyTuple` of the elements' types.
    | Tuple of items: TPat list * ty: SemType
    /// Literal pattern (match arms): `| 0 -> …`, `| true -> …`.
    | Const of value: TConstValue * ty: SemType
    /// `{ X = px; Y = py }` — destructures by field name. `ty` is a
    /// `TyRecord`. May list a subset of the record's fields; unlisted
    /// fields are simply not bound.
    | Record of fields: (string * TPat) list * ty: SemType
    /// Discriminated-union ctor pattern. `caseName` is the ctor name
    /// (e.g. `"Circle"`); `fields` is the per-field sub-pattern list,
    /// empty for nullary cases. `ty` is always a `TyUnion`. The
    /// declaring union is recoverable via `ctx.CtorIndex[caseName]` at
    /// consumption time.
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
    /// `param` is the lambda's parameter pattern (with full destructure).
    | Lambda of param: TPat * body: TExpr * ty: SemType
    /// Curried; multi-arg applications nest.
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
    /// Instance property read: `r.X` where `X` is a class property.
    /// `ty` is the property's declared type.
    | PropertyGet of receiver: TExpr * propertyName: string * ty: SemType
    /// Static method invocation: `ClassName.M(args)`. Same arg-peeling
    /// as `MethodCall`; no receiver.
    | StaticMethodCall of className: string * methodName: string * args: TExpr list * ty: SemType
    /// Static property read: `ClassName.X`.
    | StaticPropertyGet of className: string * propertyName: string * ty: SemType

and TMatchArm =
    {
        Pat: TPat
        Guard: TExpr option
        Body: TExpr
    }

[<RequireQualifiedAccess>]
type TDecl =
    /// Top-level `let` / `let rec` binding. `isInline` mirrors the source
    /// `inline` keyword. The `value` body is retained verbatim regardless;
    /// when `isInline` is set the flag tells codegen it may expand the body
    /// per call site (via `Inline.inlineExpand`, substituting the caller's
    /// concrete types for the binding's quantified typars) rather than emit
    /// a single callable. See [front-end-gaps-plan](docs/front-end-gaps-plan.md) §C.
    | Let of binding: TPat * value: TExpr * isInline: bool * ty: SemType
    /// Top-level expression (script fragments parse as a module with one
    /// Expression element).
    | Expression of expr: TExpr * ty: SemType

type TastFile =
    {
        /// Source order.
        Decls: TDecl list
        /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
        Diagnostics: Diagnostic list
    }
