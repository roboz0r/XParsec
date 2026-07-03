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
