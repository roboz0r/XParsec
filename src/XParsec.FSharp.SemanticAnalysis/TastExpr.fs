namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// The TERM shapes of the TAST: constants, patterns, expressions, and the compiled-form
// cluster (a function's SOURCE arity and the flat signature derived from it). Every node
// carries its inferred type inline.

/// A compile-time constant. `Integral` carries its kind beside its value, and `bits` is that
/// value extended to 64 bits at the kind's signedness.
[<RequireQualifiedAccess>]
type TConstValue =
    | Integral of kind: IntKind * bits: int64
    | Float of double
    | Float32 of single
    | Bool of bool
    | Char of char
    | Decimal of decimal
    | String of string
    | Unit

/// What a call supplies for a trailing optional parameter it omitted.
[<RequireQualifiedAccess>]
type OptionalDefault =
    /// The declaration states a constant default.
    | Const of TConstValue
    /// The declaration states no default, so the slot is filled with the target's
    /// missing-argument value.
    | Omitted

[<Struct>]
type ParamAttrs =
    {
        /// `[<CallAtMostOnce>]`: splice the argument unevaluated at its single use instead
        /// of eager `let`-binding, which is how `&&`/`||` short-circuit.
        CallAtMostOnce: bool
    }

    static member Default = { CallAtMostOnce = false }

    member this.IsDefault = this = ParamAttrs.Default

[<RequireQualifiedAccess>]
type TPatG<'ty, 'tok, 'id> =
    /// `boundVar` is the definition site a `TExpr.Var` identifies.
    | NamedSimple of boundVar: 'id * ty: 'ty * tok: 'tok
    /// `_` placeholder. Has a type (the matched value's type) but binds nothing.
    | Wildcard of ty: 'ty * tok: 'tok
    /// `ty` is always a `TyTuple` of the elements' types.
    | Tuple of items: EqArray<TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    | Const of value: TConstValue * ty: 'ty * tok: 'tok
    /// `ty` is a `TyRecord`. May list a subset of the record's fields; unlisted
    /// fields are simply not bound.
    | Record of fields: EqArray<string * TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `fields` is the per-field sub-pattern list, empty for nullary cases. `ty` is always
    /// a `TyUnion`.
    | Union of caseName: string * fields: EqArray<TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `:? testTy as x`. Refutable; binds `inner` (the `as`-name, irrefutable) against the
    /// value cast down to `testTy`. `ty` is the scrutinee's type.
    | TypeTestAs of testTy: 'ty * inner: TPatG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `null` literal pattern (`match x with null -> …`). Refutable, binds nothing.
    /// `ty` is the scrutinee's (reference) type.
    | Null of ty: 'ty * tok: 'tok
    /// `| E.C1`. Refutable, binds nothing. Carries the case IDENTITY (`enumKey` +
    /// `caseName`), not the literal, which lives on the frozen enum's case table. `ty` is
    /// the enum nominal, unified with the scrutinee.
    | EnumCase of enumKey: TypeKey * caseName: string * ty: 'ty * tok: 'tok
    /// `p1 | p2 | … | pn`, `alts` having ≥ 2 entries, nested source `|`s flattened into one
    /// level here. Matches iff some alternative does (left-to-right, first wins), and binds
    /// nothing, because an alternative that binds is rejected at elaboration.
    | Or of alts: EqArray<TPatG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok

[<RequireQualifiedAccess>]
type HoleSpecSource =
    /// Every printf specifier (`%d`, `%A`, `%08.2f`, …) and every printf-style `%d{x}`
    /// interpolation hole, classified once at elaboration.
    | Classified of PrintfHoleForm.HoleForm
    /// An interpolation `{x:fmt}` custom-format clause (`{x:X}`, `{x:N2}`): a raw .NET
    /// format string carried verbatim, so it is faithful only on the CLR backend.
    | RawFormat of fmt: string option

/// `Ty` is the hole's static type, which picks the `AppendFormatted<T>` overload, so the
/// append is unboxed. `Tok` is the specifier's own source token, for source maps / PDBs.
type HoleSpecG<'ty, 'tok> =
    {
        Ty: 'ty
        Source: HoleSpecSource
        Tok: 'tok
    }

/// How an instance member access dispatches.
[<RequireQualifiedAccess>]
type CallVia<'ty> =
    /// Ordinary virtual dispatch (`callvirt`).
    | Self
    /// `base.M(…)` / `base.X`: targets the PARENT's slot non-virtually (`call`), so an
    /// `override` doesn't recurse into itself.
    | Base
    /// Constrained dispatch on a typar object argument coerced to an interface (`'T :> IFace`):
    /// `constrained. <typar> callvirt <iface-slot>`. `ifaceArgs` is the interface's own
    /// instantiation (`'U,'E` in `'T :> IStructSeq<'U,'E>`); empty for a non-generic interface.
    | Interface of ifaceArgs: EqArray<'ty>

[<RequireQualifiedAccess>]
type Disposal =
    /// The bound variable implements the disposal capability, disposed through whatever slot the
    /// target gives it: `slot` (`disposable::Dispose`) where dispatch goes through the
    /// interface, ignored where the target has a native slot (JS `[Symbol.dispose]()`).
    | ViaCapability of slot: SymbolKey
    /// No capability, but an own pattern `Dispose()`: the `[<IsByRefLike>]` ref-struct
    /// carve-out (unboxable to the interface), or an external type with no `IDisposable`.
    /// Call the keyed member directly, NOT the capability slot.
    | ViaOwnMember of key: SymbolKey
    /// Disposal never resolved: a `use`-over-non-disposable error was reported, or the
    /// bound variable's type never resolved. Exists so an erroneous file still elaborates; both
    /// backends `failwith` on it.
    | Unresolved

[<RequireQualifiedAccess>]
type TExprG<'ty, 'tok, 'id> =
    | Const of value: TConstValue * ty: 'ty * tok: 'tok
    /// `boundVar` is the identity of the *definition site*, not the use site.
    | Var of boundVar: 'id * ty: 'ty * tok: 'tok
    /// An externally-provided symbol, carrying the compiled name a target dispatches on
    /// (`op_Addition` → CIL `add`). `key` is `ValueNone` where the resolution stamped none.
    | External of compiledName: string * key: SymbolKey voption * ty: 'ty * tok: 'tok
    | Lambda of param: TPatG<'ty, 'tok, 'id> * body: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    | App of fn: TExprG<'ty, 'tok, 'id> * arg: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    | Let of
        pattern: TPatG<'ty, 'tok, 'id> *
        value: TExprG<'ty, 'tok, 'id> *
        body: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// `use x = value in body` — `Let`'s shape plus disposal: `body` runs inside a
    /// `try … finally`. `ty` is the body's type.
    | Use of
        pattern: TPatG<'ty, 'tok, 'id> *
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
    /// `[| e1; …; en |]`, elements in source order. `ty` is the rank-1 array of their
    /// common element type, which unification drove them all to.
    | ArrayLit of elems: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
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
    /// `ty` is always unit; `body` types as unit and `pat` as the element type of `source`
    /// (pinned to `int` for a range source, a free TypeVar otherwise). `enumerator` records
    /// how the source yields one.
    | ForIn of
        pat: TPatG<'ty, 'tok, 'id> *
        source: TExprG<'ty, 'tok, 'id> *
        body: TExprG<'ty, 'tok, 'id> *
        enumerator: ForInEnumeratorG<'ty> *
        ty: 'ty *
        tok: 'tok
    /// `scrutinee` and each `arms.[i].Pat` share the same type; every `arms.[i].Body`
    /// shares `ty`. `function` desugars to a `Match` over a synthetic parameter.
    | Match of
        scrutinee: TExprG<'ty, 'tok, 'id> *
        arms: EqArray<TMatchArmG<TPatG<'ty, 'tok, 'id>, TExprG<'ty, 'tok, 'id>>> *
        ty: 'ty *
        tok: 'tok
    /// `try body with | pat -> arm`. `body` and every `arms.[i].Body` share `ty`; arm
    /// patterns bind against a placeholder `exn` nominal, not a fresh TypeVar.
    | TryWith of
        body: TExprG<'ty, 'tok, 'id> *
        arms: EqArray<TMatchArmG<TPatG<'ty, 'tok, 'id>, TExprG<'ty, 'tok, 'id>>> *
        ty: 'ty *
        tok: 'tok
    /// `try body finally cleanup`. `body` carries `ty`; `cleanup` is unit.
    | TryFinally of body: TExprG<'ty, 'tok, 'id> * cleanup: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `lhs <- rhs`. Always types as unit.
    | Assignment of lhs: TExprG<'ty, 'tok, 'id> * rhs: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `null` literal. `ty` is left a free TypeVar, not constrained to a reference type.
    | Null of ty: 'ty * tok: 'tok
    /// `start..stop` or `start..step..stop`, endpoints and step all `int`. `for i in a..b do`
    /// lowers to a counted `ForTo`, so a surviving `Range` is an unsupported one and always
    /// carries a diagnostic.
    | Range of
        startExpr: TExprG<'ty, 'tok, 'id> *
        step: TExprG<'ty, 'tok, 'id> option *
        stopExpr: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// `{ X = e1; Y = e2 }` record literal. `ty` is a `TyRecord`; `fields` is in source
    /// order and matches the record's declared set, which unification checked.
    | RecordCons of fields: EqArray<string * TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `{ r with X = v; … }`. `source` types as the same `TyRecord` as `ty`; `overrides` is
    /// the source-order `(name, replacement)` list. Copying the unlisted fields from
    /// `source` is the backend's job, so no node here represents it.
    | RecordClone of
        source: TExprG<'ty, 'tok, 'id> *
        overrides: EqArray<string * TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    /// `r.X` — `objArg` types as a `TyRecord`, `ty` as the field's declared type.
    | FieldGet of objArg: TExprG<'ty, 'tok, 'id> * fieldName: string * ty: 'ty * tok: 'tok
    /// `r.X <- v` — `ty` is unit. `objArg` types as a `TyRecord` whose field `fieldName`
    /// is mutable; assigning an immutable one is a reported error, not a shape.
    | FieldSet of
        objArg: TExprG<'ty, 'tok, 'id> *
        fieldName: string *
        value: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// Union constructor application. `args` matches the ctor's declared arity, so nullary
    /// (`Point`) and applied (`Rectangle(2.0, 3.0)`) ctors both fold here, because the latter
    /// has its `App` chain peeled at elaboration. `ty` is a `TyUnion`.
    | UnionCons of caseName: string * args: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// Class primary-constructor invocation; `args` is the per-parameter list, the parser's
    /// tuple wrapper (`new Point(3, 4)`) already peeled. `key` identifies the chosen EXTERNAL
    /// `.ctor` of an overload set; `ValueNone` for a project-local or synthesised class.
    | New of className: string * key: SymbolKey voption * args: EqArray<TExprG<'ty, 'tok, 'id>> * ty: 'ty * tok: 'tok
    /// `r.M(args)`. `args` is the per-parameter list (peeled as for `New`); `ty` is the
    /// method's declared return type; `key` is a `SymbolKey.Member`.
    | MethodCall of
        objArg: TExprG<'ty, 'tok, 'id> *
        key: SymbolKey *
        via: CallVia<'ty> *
        args: EqArray<TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    | PropertyGet of objArg: TExprG<'ty, 'tok, 'id> * key: SymbolKey * via: CallVia<'ty> * ty: 'ty * tok: 'tok
    /// Same arg-peeling as `MethodCall`; no object argument. `declArgs` is the declaring
    /// type's instantiation at this call site (`Box<int>.M` carries `[int]`), empty for a
    /// non-generic declaring type.
    | StaticMethodCall of
        key: SymbolKey *
        declArgs: EqArray<'ty> *
        args: EqArray<TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    | StaticPropertyGet of key: SymbolKey * declArgs: EqArray<'ty> * ty: 'ty * tok: 'tok
    /// Read of a class-level `static let` backing field: `ldsfld` against the class's
    /// private static field, no method call (a static PROPERTY is a `StaticPropertyGet`).
    /// `declKey` is the declaring class's `TypeKey`, because `MemberKind` has no `Field` case.
    | StaticFieldGet of declKey: TypeKey * fieldName: string * ty: 'ty * tok: 'tok
    /// Store (`x <- e`) to a class-level `static let mutable` backing field: `stsfld`,
    /// the store analogue of `StaticFieldGet`. `ty` is the FIELD's type; the store itself
    /// results in unit.
    | StaticFieldSet of declKey: TypeKey * fieldName: string * value: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// Member access on an EXTERNAL type. `objArg` is `ValueNone` for a static member
    /// (`EqualityComparer<int>.Default`), `ValueSome` for an instance one; `storage` splits
    /// field (`ldfld`) from property (`call get_X`) from method. `M(a, b)` widths `[2]`, `M a b` `[1; 1]`.
    | ExternalMember of
        objArg: TExprG<'ty, 'tok, 'id> voption *
        key: SymbolKey *
        memberName: string *
        storage: MemberStorage *
        argGroupWidths: EqArray<int> *
        ty: 'ty *
        tok: 'tok
    /// Lowered printf / string interpolation. `segments` is the interleaved literal / hole
    /// sequence in source order, each hole carrying its argument inline, so evaluation
    /// order is left to right. `ty` is the call's result.
    | Format of
        sink: FormatSinkG<TExprG<'ty, 'tok, 'id>> *
        segments: EqArray<FormatSegG<'ty, 'tok, TExprG<'ty, 'tok, 'id>>> *
        ty: 'ty *
        tok: 'tok
    /// Value-level inline IL `(# "opcode" args : retTy #)`: `opCode` is the mnemonic
    /// (`"ceq"`, `"add"`), `args` the operands in source order. `typeOperand` is the one type
    /// token a tokenful opcode needs (`newarr`/`ldelem` → element type), else `ValueNone`.
    | ILIntrinsic of
        opCode: string *
        typeOperand: 'ty voption *
        args: EqArray<TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    /// Static optimization: a default expression plus type-specialized clauses
    /// (`expr when ^T : int = … when ^T : ^T = …`), in source order. At `let inline` expansion
    /// the first clause whose constraints hold wins, else `defaultExpr`; all bodies share `ty`.
    | StaticOptimization of
        clauses: EqArray<TStaticOptClauseG<'ty, 'tok, 'id>> *
        defaultExpr: TExprG<'ty, 'tok, 'id> *
        ty: 'ty *
        tok: 'tok
    /// `e :> T` static upcast; `source`'s type is a subtype of `ty`, checked at unification.
    | Upcast of source: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `e :?> T` checked downcast. `ty` is the (more-specific) target type.
    | Downcast of source: TExprG<'ty, 'tok, 'id> * ty: 'ty * tok: 'tok
    /// `e :? T` type test. `testTy` is the tested-against type `T`; `ty` is always `bool`.
    | TypeTest of source: TExprG<'ty, 'tok, 'id> * testTy: 'ty * ty: 'ty * tok: 'tok
    /// SRTP member-trait call `((^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) (x, y))`.
    /// `supportTys` is the candidate support set: the distinct operand types in argument order,
    /// or the node's own `ty` when there are no arguments. Inline expansion rewrites this to a
    /// `StaticMethodCall` on the one nominal in the set carrying the member.
    | TraitCall of
        supportTys: EqArray<'ty> *
        memberName: string *
        args: EqArray<TExprG<'ty, 'tok, 'id>> *
        ty: 'ty *
        tok: 'tok
    /// A call to the `spec`-th entry of the file's specialization table, applied to `args`.
    /// The body stays in the table, so N call sites are one entry and N edges. `path`/`tok`
    /// are the CALL SITE's; `args` are positional against the entry's SURVIVING parameters.
    | InlineCall of
        spec: SpecializationId *
        args: EqArray<TExprG<'ty, 'tok, 'id>> *
        path: AssemblyFilePath *
        ty: 'ty *
        tok: 'tok
    /// Marks a subtree a reduction FUSED into a specialization entry from a call site: its
    /// nodes index `path`, not the file the entry's body was written in: `a && b` outlines
    /// as `if a then ⟨CallerExpr b⟩ else false`. `ty`/`tok` are its body's; expansion unwraps it.
    | CallerExpr of body: TExprG<'ty, 'tok, 'id> * path: AssemblyFilePath * ty: 'ty * tok: 'tok

/// One arm of a `Match` / `TryWith`. `'pat`/`'e` abstract over how the pattern and the
/// guard/body expressions are carried: either the trees themselves, or handles identifying them
/// in a pool.
and TMatchArmG<'pat, 'e> =
    {
        Pat: 'pat
        Guard: 'e voption
        Body: 'e
    }

/// Where a format expression writes. `ToWriter`/`ToBuilder` carry the explicit sink
/// (`fprintf` / `bprintf`); `newline` records the trailing `\n` an `…fn` spelling adds,
/// though `ToBuilder` has none, F# having no `bprintfn`.
and [<RequireQualifiedAccess>] FormatSinkG<'e> =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of writer: 'e * newline: bool
    | ToBuilder of 'e
    | ToString

and [<RequireQualifiedAccess>] FormatSegG<'ty, 'tok, 'e> =
    | Lit of string
    | Hole of HoleSpecG<'ty, 'tok> * 'e
    /// A hole with one or both dimensions supplied at runtime: star width (`%*d`, `%-*d`)
    /// and/or star precision (`%.*f`, `%*.*f`). The curried application evaluates the
    /// dimension args BEFORE the value, in source order: width, then precision.
    | DynHole of DynFormatHoleG<'ty, 'tok, 'e>
    /// A `%a` / `%t` printer-callback hole, lowered capture-first to `residue`: an ordinary
    /// `string`-typed expression (`sprintf` splices the callback's result, the writer/builder
    /// families a scratch builder) that both backends emit exactly as a `%s` hole.
    | CallbackHole of spec: HoleSpecG<'ty, 'tok> * residue: 'e

/// `Width` is present iff the width is a star (`%*…`), `Precision` iff the precision is a
/// star (`%.*…`); at least one is, since a plain hole stays a `Hole` segment.
and DynFormatHoleG<'ty, 'tok, 'e> =
    {
        Width: 'e voption
        Precision: 'e voption
        Spec: HoleSpecG<'ty, 'tok>
        Value: 'e
    }

/// One clause of a static optimization: `Constraints` is the `and`-joined list, all of
/// which must hold for `Body` to be selected.
and TStaticOptClauseG<'ty, 'tok, 'id> =
    {
        Constraints: EqArray<TStaticOptConstraintG<'ty>>
        Body: TExprG<'ty, 'tok, 'id>
    }

/// One flattened compiled parameter. A simple bound variable's `Slot` is referenced by the body
/// directly; a destructuring parameter carries `Pat = Some …` and a synthetic `Slot` the
/// backend spills and then binds the pattern against.
and StaticParamG<'ty, 'pat, 'id> =
    { Slot: 'id; Ty: 'ty; Pat: 'pat option }

/// One curried argument group of a function's SOURCE signature: the distinction the flat
/// compiled signature loses. `GUnit` (`fun () -> …`) erases to zero params when it is the
/// sole group; `GTuple` (`fun (a, b, …) -> …`) flattens to one param per element.
and [<RequireQualifiedAccess>] ArgGroupG<'ty, 'pat, 'id> =
    | GUnit of ty: 'ty
    | GSimple of slot: 'id * ty: 'ty
    /// `'pat` is a pool entry for a FILE's own arity, pooled alongside the lambda chain it
    /// was peeled from, and the pattern tree itself for an `.fsi`-minted EXTERNAL one.
    | GTuple of pat: 'pat

/// The SOURCE signature. `Groups.Length` is the number of applications a saturated call
/// consumes; `ResultTy` is the source (NOT unit-erased) result type.
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

[<RequireQualifiedAccess>]
module TExprG =

    /// The template text of a body that is EXACTLY one zero-operand intrinsic
    /// (`(# "undefined" #)`), else `ValueNone`. With no operand to substitute, that text IS
    /// what a reference emits.
    let nullaryIntrinsicText (e: TExprG<'ty, 'tok, 'id>) : string voption =
        match e with
        | TExprG.ILIntrinsic(opCode = opCode; args = args) when args.Length = 0 -> ValueSome opCode
        | _ -> ValueNone
