namespace XParsec.FSharp.Codegen.Js

/// A 0-based source position — V3 source-map coordinates (`Line`, then `Column`
/// counted in UTF-16 code units).
[<Struct>]
type JsLoc = { Line: int; Column: int }

/// One case of an emitted union. `ClassName` is the emitted subclass name
/// (`<Union>_<Case>`); `Tag` is the declaration-order integer the base-class `tag`
/// field carries; `Fields` are the case's declaration-order field names (positional
/// fields synthesised as `Item` / `Item1` / `Item2` / …, named fields verbatim).
type JsUnionCaseDecl =
    {
        CaseName: string
        ClassName: string
        Tag: int
        Fields: string list
    }

/// `Literal` payload. `Number` and `BigInt` carry the *already-formatted* numeric
/// text (round-trippable), so the printer never re-formats and can't drift from
/// the value the walker resolved.
[<RequireQualifiedAccess>]
type JsLiteral =
    | String of string
    /// A `number` literal — the formatted text of an int32 / byte / float / float32 `Const`.
    | Number of raw: string
    /// A `bigint` literal — the digits of an int64 `Const`, printed with the trailing `n`.
    | BigInt of digits: string
    | Boolean of bool

/// One piece of a `Raw` template expansion: a verbatim chunk of the `$N` template
/// string, or a substituted operand expression (which the printer wraps in
/// parentheses for the precedence-by-construction guarantee).
[<RequireQualifiedAccess>]
type JsRawSeg =
    | Verbatim of string
    | Hole of JsExpr

and [<RequireQualifiedAccess>] JsExpr =
    | Identifier of name: string * loc: JsLoc voption
    | Literal of JsLiteral * loc: JsLoc voption
    /// `object.property` when `computed = false`, `object[property]` when `computed = true`.
    | Member of object: JsExpr * property: JsExpr * computed: bool * loc: JsLoc voption
    | Call of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
    | New of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
    /// `test ? consequent : alternate`. The printer parenthesises the whole node.
    | Conditional of test: JsExpr * consequent: JsExpr * alternate: JsExpr * loc: JsLoc voption
    /// `(a, b, …)`: evaluate each in order, yield the last.
    | Sequence of expressions: JsExpr list * loc: JsLoc voption
    /// `[a, b, …]`. A tuple is a JS array; a tuple pattern indexes it positionally.
    | Array of elements: JsExpr list * loc: JsLoc voption
    /// A `$N`-template `ILIntrinsic` expanded to verbatim text + parenthesised operand
    /// holes; the whole wrapped in parentheses by the printer. Not an ESTree node.
    | Raw of segments: JsRawSeg list * loc: JsLoc voption
    /// `(p0, …) => body`. F# functions emit as chains of *unary* arrows so currying
    /// and partial application fall out for free.
    | Arrow of parameters: string list * body: JsFnBody * loc: JsLoc voption
    /// `left <op> right` — the whole parenthesised (universal-parenthesization), so no
    /// precedence table is needed.
    | Binary of operator: string * left: JsExpr * right: JsExpr * loc: JsLoc voption
    /// `left <op> right` for the short-circuiting `&&` / `||`. Kept distinct from
    /// `Binary` for ESTree exactness; the printer treats them identically.
    | Logical of operator: string * left: JsExpr * right: JsExpr * loc: JsLoc voption
    /// `(target = value)` — an assignment *expression* (unit-typed in F#, so its
    /// yielded value is unused). `target` is a mutable-local `Identifier` (`x <- v`)
    /// or a computed `Member` (`arr.[i] <- v`). Distinct from the trampoline's
    /// `JsStatement.Assign`, whose target is a bare name only.
    | Assign of target: JsExpr * value: JsExpr * loc: JsLoc voption

/// An arrow function's body: a concise expression (`=> e`) or a brace-delimited
/// statement block (`=> { … }`) — the latter is the self-tail-call loop form.
and [<RequireQualifiedAccess>] JsFnBody =
    | Expr of JsExpr
    | Block of JsStatement list

and [<RequireQualifiedAccess>] JsStatement =
    /// An expression evaluated for effect.
    | Expression of JsExpr
    | Const of name: string * init: JsExpr
    /// `let <name> = <init>;` — a *reassignable* local binding for a `let mutable`
    /// (a binder the body mutates via `Assignment`); an immutable binder stays `Const`.
    | Let of name: string * init: JsExpr
    /// `export const <name> = <init>;` — top-level binding in library compile mode.
    | Export of name: string * init: JsExpr
    /// `import <default>, { <specifiers> } from "<source>";`. `defaultBinding` is the
    /// local name a TS DEFAULT export binds to (`None` for a named-only import); each
    /// `specifiers` entry is a `name as $alias` named binding. At least one of the two
    /// is non-empty.
    | Import of defaultBinding: string option * specifiers: string list * source: string
    /// `if (test) { … } else { … }`. An empty alternate prints without the `else`.
    | If of test: JsExpr * consequent: JsStatement list * alternate: JsStatement list
    /// `while (test) { … }` — the self-tail-call trampoline.
    | While of test: JsExpr * body: JsStatement list
    /// `for (let <var> = <init>; <var> <= <limit>; <var>++) { … }` — the F#
    /// `for i = a to b do` counted loop. `<limit>` is a value the emitter has
    /// already hoisted into a binding (F# evaluates `b` once), so re-reading it
    /// per iteration is side-effect-free.
    | For of var: string * init: JsExpr * limit: JsExpr * body: JsStatement list
    /// `for (const <binder> of <source>) { … }` — the F# `for x in source do` loop
    /// over an enumerable. JS drives the source's own `Symbol.iterator` at runtime, so
    /// no enumerator/MoveNext/Current plumbing is emitted (unlike the IL backend).
    | ForOf of binder: string * source: JsExpr * body: JsStatement list
    | Return of JsExpr
    | Continue
    /// `target = value;` — param-shadow mutation in a self-tail-call.
    | Assign of target: string * value: JsExpr
    /// A record's or class's emitted JS class: one positional constructor storing
    /// each declaration-order field into the like-named property, plus any
    /// `methods` attached as instance methods on the class (a record passes `[]`).
    /// Attached methods carry the runtime dispatch slots of a custom-equality /
    /// custom-comparison class (`Equals`/`CompareTo`/`GetHashCode`), bodied with
    /// the receiver bound to JS `this`. `export` is set in library mode so a
    /// consumer can `import` the class rather than re-emit it.
    | Class of name: string * fields: string list * methods: JsClassMethod list * export: bool
    /// A union's emitted JS classes: a `baseName` base class (`tag` + `cases()` + a
    /// non-enumerable `$type` brand getter returning `brand`, the type's qualified name)
    /// plus one `extends`-subclass per case carrying its named fields after `super(tag)`.
    /// No shared runtime base: default equality/comparison/hashing is structural (the
    /// `Vesper.Core`/`Vesper.Comparison` runtimes dispatch on `$type` + own-keys); a
    /// type with custom equality/comparison emits its own `Equals`/`CompareTo` and the
    /// runtimes pick it up by method presence. `export` (library mode) exports every
    /// class so consumers import them. `baseMethods` are the union's capability
    /// protocol members (`[Symbol.iterator]`, eq/comp/hash) — they attach to the BASE
    /// class so every case subclass inherits them and dispatch lands on a case instance.
    | Union of
        baseName: string *
        brand: string *
        cases: JsUnionCaseDecl list *
        baseMethods: JsClassMethod list *
        export: bool
    /// An enum's emitted JS repr: a module-scope frozen object map
    /// `[export ]const Name = Object.freeze({ C1: v1, … });`. The step-6 repr for ALL
    /// three variants (numeric / string / mixed) — JS is untyped, so a number, a
    /// string, or a mix of them is the same object-map shape; `E.Ci` is a property
    /// read, and there is NO reverse map (v1 needs equality only). `cases` are in
    /// declaration order with their already-formatted literal value; `export` is set
    /// in library mode so a consumer can `import` the enum object.
    | Enum of name: string * cases: (string * JsLiteral) list * export: bool
    /// A bare lexical block `{ … }` — scopes a match arm's pattern bindings so two
    /// arms binding the same name don't collide as sibling `const`s.
    | Block of body: JsStatement list
    | Throw of JsExpr
    /// `try { … } finally { … }` — the lowering of an F# `use` binding. `tryBody`
    /// runs the `use` body; `finallyBody` disposes the binder on every exit
    /// (a null-guarded `x.Dispose()` call), matching F#'s null-safe RAII semantics.
    | TryFinally of tryBody: JsStatement list * finallyBody: JsStatement list
    /// `yield <e>;` — emitted only inside a generator method body (`Generator = true`
    /// on the enclosing `JsClassMethod`). The iteration adapter's `[Symbol.iterator]`
    /// generator yields each enumerator element, so `yield x` auto-produces the
    /// `{ value: x, done: false }` iterator result (and end-of-body the `{ done: true }`).
    | Yield of JsExpr

/// An instance method attached to an emitted JS class — `Name(params) { body }`.
/// Distinct from a free, receiver-first member function: an attached method binds
/// the receiver to JS `this`, so the runtime can dispatch on method presence
/// (`a.Equals(b)`, `a.CompareTo(b)`, `x.GetHashCode()`).
///
/// A class method's name slot. `Named n` prints a plain identifier header
/// (`Equals(params)`); `Computed e` prints a computed-key header `[<e>](params)`
/// where `e` is the symbol-KEY EXPRESSION (`Symbol.iterator`, a member access, or
/// `Symbol.for("vesper.equality")`, a call). The two are mutually exclusive, so a
/// computed method can't also carry a never-printed descriptive name.
and JsMethodKey =
    | Named of string
    | Computed of JsExpr

/// One instance method of an emitted class / union base class. `Generator` prefixes
/// the header with `*` (`*[Symbol.iterator]()`) so the body may `yield` — the
/// iteration adapter's form.
and JsClassMethod =
    {
        Key: JsMethodKey
        Params: string list
        Body: JsStatement list
        Generator: bool
    }

/// `Program` with `sourceType: "module"` (ESM output).
type JsProgram = { Body: JsStatement list }
