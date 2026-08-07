/// Node / `ts.*` interop primitives: filesystem + path bindings,
/// symbol/declaration accessors, and runtime type predicates.
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

/// A `package.json`'s `version` field, or `None` when it is absent or not a string.
[<Emit("(typeof $0.version === 'string') ? $0.version : undefined")>]
let jsonVersionField (parsed: obj) : string option = jsNative

/// A `package.json`'s `name` field, or `None` when it is absent or not a string.
[<Emit("(typeof $0.name === 'string') ? $0.name : undefined")>]
let jsonNameField (parsed: obj) : string option = jsNative

/// The manifest and TS's program tables both speak forward slashes, on Windows too.
let normalizeSlashes (p: string) : string = p.Replace("\\", "/")

let inline hasFlag (flags: Ts.SymbolFlags) (test: Ts.SymbolFlags) = int flags &&& int test <> 0

let tryDeclOf (s: Ts.Symbol) : Ts.Node option =
    match s.valueDeclaration with
    | Some d -> Some(unbox d)
    | None ->
        match s.declarations with
        | Some ds when ds.Count > 0 -> Some(unbox ds.[0])
        | _ -> None

let declOf (s: Ts.Symbol) : Ts.Node =
    match tryDeclOf s with
    | Some d -> d
    | None -> failwithf "symbol '%s' has no declaration" (s.getName ())

/// The name `__type`, which the checker mints for an object/function type literal
/// with no declared name (`{ x: number }`, `(e: T) => void`).
let isAnonymousTypeName (name: string) : bool =
    name = unbox<string> Ts.InternalSymbolName.Type

[<Emit("$0 === $1")>]
let inline jsRefEq (a: obj) (b: obj) : bool = jsNative

/// `== null` — TRUE for both `null` and `undefined` (a node's `parent` is
/// `undefined` above the source file, a JS falsy the F# option layer does not model).
[<Emit("$0 == null")>]
let inline jsIsNullOrUndef (o: obj) : bool = jsNative

/// `declare module "node:fs"` → `Some "node:fs"` (`.text` excludes the quotes).
/// `None` for a non-module node and for an identifier-named `namespace NS`, whose
/// `.name` is an `Identifier` rather than a `StringLiteral`.
let quotedModuleNameOf (node: Ts.Node) : string option =
    if ts.isModuleDeclaration node then
        let nameNode = unbox<Ts.Node> (unbox<Ts.ModuleDeclaration> node).name

        if ts.isStringLiteral nameNode then
            Some (unbox<Ts.LiteralLikeNode> nameNode).text
        else
            None
    else
        None

/// The innermost enclosing `declare module "fs" { … }` name, or `None` for a node in
/// no quoted module (a true global, a default-lib type). An intervening `namespace NS`
/// is stepped over, so a declaration inside one is still homed to `"fs"`.
let enclosingQuotedModuleName (node: Ts.Node) : string option =
    let rec walk (n: Ts.Node) : string option =
        if jsIsNullOrUndef (box n) || ts.isSourceFile n then
            None
        else
            match quotedModuleNameOf n with
            | Some name -> Some name
            | None -> walk n.parent

    walk node

// An instantiated generic (`Array<string>`) is the only `Type` carrying a `target`
// back-pointer to its definition, and the binding has no `isTypeReference()` member,
// so detect it by the presence of the field.
[<Emit("$0.target !== undefined && $0.target !== null")>]
let inline hasTargetRef (t: Ts.Type) : bool = jsNative

// `T[K]` and `T extends U ? … : …` also have no predicate member, so go by their
// distinguishing fields: `objectType`/`indexType` for an indexed access, `root` plus
// `checkType`/`extendsType` for a conditional.
[<Emit("$0.objectType !== undefined && $0.indexType !== undefined")>]
let inline isIndexedAccessType (t: Ts.Type) : bool = jsNative

[<Emit("$0.root !== undefined && $0.checkType !== undefined && $0.extendsType !== undefined")>]
let inline isConditionalType (t: Ts.Type) : bool = jsNative

// True for an interface/class instance, object literal or tuple/array reference; false
// for a primitive/union/intersection/type-parameter. The bit comes from the installed
// compiler's `ts.TypeFlags.Object` (`$1`) — the vendored numeric values drift.
[<Emit("($0.flags & $1.TypeFlags.Object) !== 0")>]
let inline isObjectTypeFlag (t: Ts.Type) (tsExports: Ts.IExports) : bool = jsNative
