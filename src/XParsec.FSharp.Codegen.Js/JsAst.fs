namespace XParsec.FSharp.Codegen.Js

/// A 0-based V3 source-map position: `Source` indexes the map's `sources[]` (a node copied out
/// of an inline specialization belongs to the PRODUCER's file), `Column` counts UTF-16 units.
[<Struct>]
type JsLoc = { Source: int; Line: int; Column: int }

/// One entry of a V3 map's index-aligned `sources[]` / `sourcesContent[]` arrays: the name a
/// debugger is given for a file, and the text embedded for it.
type JsMapSource = { Path: string; Content: string }

/// One case of an emitted union:
/// `class <ClassName> extends <Union> { constructor(<Fields>) { super(<Tag>); … } }`.
type JsUnionCaseDecl =
    {
        /// The F# case name, verbatim — the string the base class's `cases()` reports.
        CaseName: string
        /// The emitted subclass name, `<Union>_<Case>`.
        ClassName: string
        /// Declaration index, passed as `super(<Tag>)` and read back off the base's `tag`.
        Tag: int
        /// Ctor parameters in declaration order, each stored to `this.<field>`.
        Fields: string list
    }

[<RequireQualifiedAccess>]
type JsLiteral =
    /// `"a\nb"` — the DECODED text; the printer quotes it and re-escapes every control char.
    | String of string
    /// A `number` literal — the formatted text of an int32 / byte / float / float32 `Const`.
    | Number of raw: string
    /// A `bigint` literal — the digits of an int64 `Const`, printed with the trailing `n`.
    | BigInt of digits: string
    /// `true` / `false`.
    | Boolean of bool

/// One piece of a `Raw` template expansion: a verbatim chunk of the `$N` template
/// string, or a substituted operand expression.
[<RequireQualifiedAccess>]
type JsRawSeg =
    /// Template text spliced in as written (`" + "`, `".charCodeAt("`).
    | Verbatim of string
    /// A substituted operand; prints as `(<e>)`, so no operator precedence can straddle it.
    | Hole of JsExpr

and [<RequireQualifiedAccess>] JsExpr =
    /// A bare name, `x`. Also how the keywords `null` / `undefined` and any JS global reach
    /// the output — there is no separate case for them.
    | Identifier of name: string * loc: JsLoc voption
    /// `42`, `"s"`, `9n`, `true`.
    | Literal of JsLiteral * loc: JsLoc voption
    /// `object.property` when `computed = false`, `object[property]` when `computed = true`.
    | Member of object: JsExpr * property: JsExpr * computed: bool * loc: JsLoc voption
    /// `callee(a, b, …)`. An arrow callee is parenthesised first — `((x) => …)(v)`.
    | Call of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
    /// `new callee(a, b, …)`.
    | New of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
    /// `test ? consequent : alternate`.
    | Conditional of test: JsExpr * consequent: JsExpr * alternate: JsExpr * loc: JsLoc voption
    /// `(a, b, …)`: evaluate each in order, yield the last.
    | Sequence of expressions: JsExpr list * loc: JsLoc voption
    /// `[a, b, …]`. A tuple is a JS array; a tuple pattern indexes it positionally.
    | Array of elements: JsExpr list * loc: JsLoc voption
    /// A `$N`-template `ILIntrinsic` expanded to verbatim text + operand holes. Not an
    /// ESTree node.
    | Raw of segments: JsRawSeg list * loc: JsLoc voption
    /// `(p0, …) => body`. Arity is whatever `parameters` holds: a curried F# lambda is a
    /// CHAIN of unary arrows, a flat module function one n-ary arrow.
    | Arrow of parameters: string list * body: JsFnBody * loc: JsLoc voption
    /// `left <op> right` — `operator` is verbatim JS text (`===`, `>>>`), not the F# spelling.
    | Binary of operator: string * left: JsExpr * right: JsExpr * loc: JsLoc voption
    /// `left <op> right` for the short-circuiting `&&` / `||`. Kept distinct from
    /// `Binary` for ESTree exactness; the printer treats them identically.
    | Logical of operator: string * left: JsExpr * right: JsExpr * loc: JsLoc voption
    /// `(target = value)` — an assignment *expression*, unit-typed in F#. `target` is an
    /// `Identifier` (`x <- v`) or a `Member`, computed (`arr[i] = v`) or not (`r.X = v`).
    | Assign of target: JsExpr * value: JsExpr * loc: JsLoc voption

/// An arrow function's body.
and [<RequireQualifiedAccess>] JsFnBody =
    /// A concise body — `=> e`, no braces and no `return`.
    | Expr of JsExpr
    /// `=> { … }` — a statement block, and the form a self-tail-call loop needs.
    | Block of JsStatement list

and [<RequireQualifiedAccess>] JsStatement =
    /// `<e>;` — an expression evaluated for effect, its value discarded.
    | Expression of JsExpr
    /// `const <name> = <init>;` — the default for an F# bound variable the body never assigns.
    | Const of name: string * init: JsExpr
    /// `let <name> = <init>;` — a *reassignable* local for a `let mutable`; an immutable
    /// bound variable stays `Const`.
    | Let of name: string * init: JsExpr
    /// `export const <name> = <init>;` — a top-level binding in library compile mode;
    /// `reassignable` selects `export let` for a bound variable the module later mutates.
    | Export of name: string * init: JsExpr * reassignable: bool
    /// `import <default>, { <name> as <alias>, … } from "<source>";`. `defaultBinding` is the
    /// local name a TS DEFAULT export binds to; each `named` entry is an `(exportName, alias)`
    /// pair. Both empty prints the invalid `import  from "…";`, so one must be non-empty.
    | Import of defaultBinding: string option * named: (string * string) list * source: string
    /// `import * as <binding> from "<source>";` — its own statement, since a namespace clause
    /// cannot ride the braces of a `{ named }` import for the same source.
    | ImportNamespace of binding: string * source: string
    /// `if (test) { … } else { … }`. An empty alternate prints without the `else`.
    | If of test: JsExpr * consequent: JsStatement list * alternate: JsStatement list
    /// `while (test) { … }` — the self-tail-call trampoline.
    | While of test: JsExpr * body: JsStatement list
    /// `for (let <var> = <init>; <var> <= <limit>; <var>++) { … }` — the F# `for i = a to b do`
    /// counted loop. `<limit>` is re-read each iteration, so it must be a binding, not a call.
    | For of var: string * init: JsExpr * limit: JsExpr * body: JsStatement list
    /// `for (const <bound variable> of <source>) { … }` — the F# `for x in source do`. JS drives the
    /// source's own `Symbol.iterator`, so no MoveNext/Current plumbing is emitted.
    | ForOf of boundVar: string * source: JsExpr * body: JsStatement list
    /// `return <e>;`
    | Return of JsExpr
    /// `continue;` — re-enters the `While` trampoline after the parameter write-back.
    | Continue
    /// `target = value;` — param-shadow mutation in a self-tail-call.
    | Assign of target: string * value: JsExpr
    /// `this.<field> = <value>;` — a constructor's field store.
    | FieldStore of field: string * value: JsExpr
    /// `[export ]class <name> { constructor(…) { … } <methods…> }` — a record's or class's
    /// emitted class. `methods` are attached instance methods, receiver bound to JS `this`
    /// (a record passes `[]`).
    | Class of name: string * ctor: JsCtor * methods: JsClassMethod list * export: bool
    /// `[export ]class <baseName>` carrying `tag`, `cases()` and a `$type` getter returning
    /// `brand` (the type's qualified name), plus one `extends`-subclass per case whose fields
    /// follow `super(tag)`. `baseMethods` attach to the BASE: every subclass inherits them.
    | Union of
        baseName: string *
        brand: string *
        cases: JsUnionCaseDecl list *
        baseMethods: JsClassMethod list *
        export: bool
    /// `[export ]const <name> = Object.freeze({ C1: v1, … });` — one shape for numeric, string
    /// and mixed enums alike. `E.Ci` is a property read; there is NO reverse value→name map.
    | Enum of name: string * cases: (string * JsLiteral) list * export: bool
    /// A bare lexical block `{ … }` — scopes a match arm's pattern bindings so two
    /// arms binding the same name don't collide as sibling `const`s.
    | Block of body: JsStatement list
    /// `throw <e>;`
    | Throw of JsExpr
    /// `try { … } finally { … }` — an F# `try…finally`, or a `use` binding whose
    /// `finallyBody` disposes the bound variable.
    | TryFinally of tryBody: JsStatement list * finallyBody: JsStatement list
    /// `yield <e>;` — valid only inside a generator method body (`Generator = true` on the
    /// enclosing `JsClassMethod`).
    | Yield of JsExpr

/// A class method's name slot.
and JsMethodKey =
    /// A plain identifier header — `Named "Equals"` prints `Equals(params)`.
    | Named of string
    /// A computed-key header, `[<e>](params)`: `[Symbol.iterator]`, `[Symbol.for("vesper.hash")]`.
    | Computed of JsExpr

/// One instance method of an emitted class / union base class:
/// `[*]<Key>(<Params>) { <Body> }`.
and JsClassMethod =
    {
        /// `Equals(…)` or `[Symbol.iterator](…)`.
        Key: JsMethodKey
        /// Parameter names, printed `(p0, p1, …)`.
        Params: string list
        /// The statements between the header's braces.
        Body: JsStatement list
        /// Prefixes the header with `*`, so `Body` may `yield`.
        Generator: bool
    }

/// The one JS constructor an emitted class gets: `constructor(<Params>) { <Body> }`.
and JsCtor =
    {
        /// The ctor's OWN parameter names, not the field list — `new(args) = { f = e }`
        /// need not take one parameter per field.
        Params: string list
        /// The stores and preamble; a union subclass's `super(<tag>);` is printed before it.
        Body: JsStatement list
    }

/// The `JsCtor` shapes an emitted class is built from.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsCtor =

    /// Each declaration-order field becomes a like-named parameter stored into `this.<field>`;
    /// `tail` runs after the stores (a class's instance preamble).
    let positional (fields: string list) (tail: JsStatement list) : JsCtor =
        {
            Params = fields
            Body =
                [
                    for f in fields -> JsStatement.FieldStore(f, JsExpr.Identifier(f, ValueNone))
                ]
                @ tail
        }

/// `Program` with `sourceType: "module"` (ESM output).
type JsProgram = { Body: JsStatement list }
