/// Node / `ts.*` interop primitives shared by the extractor's modules: the
/// filesystem + path bindings, symbol/declaration accessors, and the
/// runtime-structural type predicates. The predicates classify by FIELD PRESENCE
/// or the binding's NAME CONSTANTS, never raw `TypeFlags`/`ObjectFlags` numerics
/// — the standing producer rule that the vendored flag VALUES can drift from the
/// installed TypeScript's while the field names stay stable.
module Vesper.Ts.Extractor.TsInterop

open Fable.Core
open Fable.Core.JsInterop

open TypeScript

[<Import("writeFileSync", "node:fs")>]
let writeFileSync (path: string) (contents: string) : unit = jsNative

[<Import("existsSync", "node:fs")>]
let existsSync (path: string) : bool = jsNative

[<Import("readFileSync", "node:fs")>]
let readFileSyncUtf8 (path: string) (encoding: string) : string = jsNative

[<Import("unlinkSync", "node:fs")>]
let unlinkSync (path: string) : unit = jsNative

[<Import("join", "node:path")>]
let pathJoin (a: string) (b: string) : string = jsNative

[<Import("dirname", "node:path")>]
let pathDirname (p: string) : string = jsNative

[<Import("relative", "node:path")>]
let pathRelative (from: string) (to_: string) : string = jsNative

[<Import("basename", "node:path")>]
let pathBasename (p: string) : string = jsNative

/// Read a `package.json`'s `version` field, defensively: the value is only a usable
/// stamp when it is genuinely a string (a malformed manifest can carry any JSON),
/// so classify by runtime `typeof` (the producer rule — never trust the shape) and
/// surface `undefined` → `None` for anything else.
[<Emit("(typeof $0.version === 'string') ? $0.version : undefined")>]
let jsonVersionField (parsed: obj) : string option = jsNative

/// Read a `package.json`'s `name` field, defensively (same discipline as
/// `jsonVersionField`): a foreign ref's HOME is the referenced package's name, so
/// walk-up-and-read the nearest `package.json`'s `name`, surfacing anything that is
/// not genuinely a string as `None`.
[<Emit("(typeof $0.name === 'string') ? $0.name : undefined")>]
let jsonNameField (parsed: obj) : string option = jsNative

/// Windows-vs-POSIX path portability: the manifest and TS's program tables both
/// speak forward slashes.
let normalizeSlashes (p: string) : string = p.Replace("\\", "/")

let inline hasFlag (flags: Ts.SymbolFlags) (test: Ts.SymbolFlags) = int flags &&& int test <> 0

/// A node to anchor `getTypeOfSymbolAtLocation` at — the symbol's declaration,
/// or `None` for a genuinely declaration-less symbol.
let tryDeclOf (s: Ts.Symbol) : Ts.Node option =
    match s.valueDeclaration with
    | Some d -> Some(unbox d)
    | None ->
        match s.declarations with
        | Some ds when ds.Count > 0 -> Some(unbox ds.[0])
        | _ -> None

/// `tryDeclOf`, required: the partial sibling for call sites where a
/// declaration-less symbol is a producer bug.
let declOf (s: Ts.Symbol) : Ts.Node =
    match tryDeclOf s with
    | Some d -> d
    | None -> failwithf "symbol '%s' has no declaration" (s.getName ())

/// The reserved name the checker mints for an ANONYMOUS type literal — an
/// object/function type with no declared name (`Ts.InternalSymbolName.Type`,
/// compiled `__type`). Read from the binding's name constant, per the producer
/// discipline. It is a valid-word-char string, so a bare nominal-name predicate
/// would misclassify it — every nominal-name gate must exclude it explicitly.
let isAnonymousTypeName (name: string) : bool =
    name = unbox<string> Ts.InternalSymbolName.Type

[<Emit("$0 === $1")>]
let inline jsRefEq (a: obj) (b: obj) : bool = jsNative

/// `== null` — TRUE for both `null` and `undefined` (a node's `parent` is
/// `undefined` above the source file, a JS falsy the F# option layer does not model).
[<Emit("$0 == null")>]
let inline jsIsNullOrUndef (o: obj) : bool = jsNative

/// The unquoted name of a `declare module "…"` node (`fs`, `node:fs`), or `None` for
/// any OTHER node — a non-module node, or an identifier-named `namespace`/`module`
/// (`namespace NS`, whose `.name` is an `Identifier`, not a `StringLiteral`). The
/// string-literal name's `.text` carries no quotes — they are syntax, absent from
/// `.text` — so the result needs no de-quoting. Classified by the
/// `isModuleDeclaration`/`isStringLiteral` runtime predicates, never raw `SyntaxKind`
/// numerics, per the producer discipline.
let quotedModuleNameOf (node: Ts.Node) : string option =
    if ts.isModuleDeclaration node then
        let nameNode = unbox<Ts.Node> (unbox<Ts.ModuleDeclaration> node).name

        if ts.isStringLiteral nameNode then
            Some (unbox<Ts.LiteralLikeNode> nameNode).text
        else
            None
    else
        None

/// The name of the innermost QUOTED ambient module (`declare module "fs" { … }`)
/// enclosing `node`, or `None` when the node sits in no quoted module (a true global,
/// a default-lib type). Walks the parent chain, STEPPING OVER identifier-named
/// `namespace`/`module` blocks (a `namespace NS` nested inside `declare module "fs"`
/// is still homed to `"fs"` — only the QUOTED wrapper counts, and `quotedModuleNameOf`
/// returns `None` for the identifier-named ones) and stopping at the source file.
let enclosingQuotedModuleName (node: Ts.Node) : string option =
    let rec walk (n: Ts.Node) : string option =
        if jsIsNullOrUndef (box n) || ts.isSourceFile n then
            None
        else
            match quotedModuleNameOf n with
            | Some name -> Some name
            | None -> walk n.parent

    walk node

// A `TypeReference` (`ObjectFlags.Reference`) — the runtime shape of an instantiated
// generic (`Array<string>`, `Box<number>`) — is the only `Type` carrying a `target`
// back-pointer to its generic definition. The binding exposes no runtime
// `isTypeReference()` predicate (unlike `isUnion`/`isArrayType`), so detect it
// structurally by the PRESENCE of `target`.
[<Emit("$0.target !== undefined && $0.target !== null")>]
let inline hasTargetRef (t: Ts.Type) : bool = jsNative

// `keyof T` (`IndexType`) has the runtime `isIndexType()` predicate, but `T[K]`
// (`IndexedAccessType`) and a conditional type (`ConditionalType`) have NONE — so
// detect them the same way `hasTargetRef` does: by the PRESENCE of their
// distinguishing fields. `objectType`/`indexType` are unique to an indexed-access
// type; `root` (the `ConditionalRoot` back-pointer) together with
// `checkType`/`extendsType` is unique to a conditional type.
[<Emit("$0.objectType !== undefined && $0.indexType !== undefined")>]
let inline isIndexedAccessType (t: Ts.Type) : bool = jsNative

[<Emit("$0.root !== undefined && $0.checkType !== undefined && $0.extendsType !== undefined")>]
let inline isConditionalType (t: Ts.Type) : bool = jsNative

// Object-ness of a `Type` — is it a `TypeFlags.Object` (an interface/class instance,
// an anonymous object literal, a tuple/array reference, …) as opposed to a
// primitive/union/intersection/type-parameter. Read the flag from the LIVE
// `ts.TypeFlags.Object` at runtime (passed as `$1`) rather than an F#-side enum
// constant: the vendored `TypeFlags` numeric VALUES drift between TypeScript releases
// (TS 6 renumbered `Object`), so a Fable-inlined constant silently mis-tests against
// the installed compiler. Reading the flag through the imported `ts` module keeps the
// test correct whatever TypeScript is resolved — the standing producer rule that flag
// VALUES drift while the field/enum NAMES stay stable.
[<Emit("($0.flags & $1.TypeFlags.Object) !== 0")>]
let inline isObjectTypeFlag (t: Ts.Type) (tsExports: Ts.IExports) : bool = jsNative
