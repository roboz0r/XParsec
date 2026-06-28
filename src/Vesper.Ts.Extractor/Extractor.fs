/// TS → manifest extractor (Fable-compiled to JS, run under Node). The PRODUCER
/// end of the slice: drives the TypeScript compiler API over a `.d.ts` surface
/// and emits a `Vesper.Ts.Manifest` JSON file the F# `TsManifestProvider`
/// consumes. The .NET analog is `MetadataSymbols` reading assemblies through
/// `MetadataLoadContext`; here the oracle is `ts.TypeChecker`.
///
/// Wired against the vendored bindings (`vendor/TypeScript.fs`, from Glutinum,
/// MIT). MVP scope: non-generic `Interface` members (primitive-typed) + free
/// `Function`s over a single local `.d.ts`. Types are mapped via `typeToString`
/// (pragmatic, primitives only); unions/generics/structurals + construct
/// signatures + import-shape detection are the deferred classifier work.
module Vesper.Ts.Extractor.Extractor

open Fable.Core
open Fable.Core.JsInterop

open TypeScript
open Vesper.Ts.Manifest

[<Import("writeFileSync", "node:fs")>]
let private writeFileSync (path: string) (contents: string) : unit = jsNative

// Filesystem + path bindings for the package path (item 18): the synthetic entry
// file is written/removed on disk, and the package version is read off the nearest
// `package.json` when the module resolver does not supply a `packageId` (the local /
// relative case). Imported from node's builtins, mirroring `writeFileSync` above.
[<Import("existsSync", "node:fs")>]
let private existsSync (path: string) : bool = jsNative

[<Import("readFileSync", "node:fs")>]
let private readFileSyncUtf8 (path: string) (encoding: string) : string = jsNative

[<Import("unlinkSync", "node:fs")>]
let private unlinkSync (path: string) : unit = jsNative

[<Import("join", "node:path")>]
let private pathJoin (a: string) (b: string) : string = jsNative

[<Import("dirname", "node:path")>]
let private pathDirname (p: string) : string = jsNative

/// Read a `package.json`'s `version` field, defensively: the value is only a usable
/// stamp when it is genuinely a string (a malformed manifest can carry any JSON),
/// so classify by runtime `typeof` (the producer rule — never trust the shape) and
/// surface `undefined` → `None` for anything else.
[<Emit("(typeof $0.version === 'string') ? $0.version : undefined")>]
let private jsonVersionField (parsed: obj) : string option = jsNative

let inline private hasFlag (flags: Ts.SymbolFlags) (test: Ts.SymbolFlags) = int flags &&& int test <> 0

/// A node to anchor `getTypeOfSymbolAtLocation` at — the symbol's declaration.
let private declOf (s: Ts.Symbol) : Ts.Node =
    match s.valueDeclaration with
    | Some d -> unbox d
    | None ->
        match s.declarations with
        | Some ds when ds.Count > 0 -> unbox ds.[0]
        | _ -> failwithf "symbol '%s' has no declaration" (s.getName ())

// ─── declaring-axis typar environment + generic-instantiation detection ─────
//
// A reference to a type parameter (`T` in `interface Box<T> { value: T }`) must
// map to `Schema.TypeRef.Typar index` — its position in the ENCLOSING declaration's
// (class / interface / alias / free-function signature) type-parameter list. `mapType`
// is otherwise stateless, so we thread a typar ENVIRONMENT: the ordered list of the
// in-scope type parameters' SYMBOLS. A typar is resolved by SYMBOL identity (reference
// equality), NOT by name — a method-axis `<U>` that shadows a declaring `<T>` must not
// alias to it. Symbols (unlike the `Type` objects, which can be re-synthesised per
// resolution entry point) are interned once per declaration and stable across the
// declared type, a body reference, and a signature, so they are the robust key.

[<Emit("$0 === $1")>]
let inline private jsRefEq (a: obj) (b: obj) : bool = jsNative

// A `TypeReference` (`ObjectFlags.Reference`) — the runtime shape of an instantiated
// generic (`Array<string>`, `Box<number>`) — is the only `Type` carrying a `target`
// back-pointer to its generic definition. The binding exposes no runtime
// `isTypeReference()` predicate (unlike `isUnion`/`isArrayType`), so detect it
// structurally by the PRESENCE of `target`, read dynamically — avoiding a raw
// `ObjectFlags`/`TypeFlags` numeric test (the standing producer rule that the
// vendored flag values can drift from the installed TypeScript's).
[<Emit("$0.target !== undefined && $0.target !== null")>]
let inline private hasTargetRef (t: Ts.Type) : bool = jsNative

let private typarSymbols (tps: Ts.Type seq) : Ts.Symbol list =
    tps
    |> Seq.map (fun tp ->
        match tp.getSymbol () with
        | Some s -> s
        | None -> failwith "type parameter has no symbol"
    )
    |> List.ofSeq

/// Declaring-axis typar symbols of a class/interface DECLARED type (an
/// `InterfaceType` at runtime, whose `typeParameters` carry them in declaration order).
let private declaredTypars (declared: Ts.Type) : Ts.Symbol list =
    match (unbox<Ts.InterfaceType> declared).typeParameters with
    | Some tps -> typarSymbols (tps |> Seq.map unbox)
    | None -> []

/// Declaring-axis typar symbols read off a DECLARATION node's effective
/// type-parameter list — used for `type` aliases, whose declared target type is not
/// an `InterfaceType` and so exposes no `typeParameters`. The effective declarations
/// are in source order, fixing the same index space the references resolve against.
let private declTyparsOf (checker: Ts.TypeChecker) (decl: Ts.Node) : Ts.Symbol list =
    ts.getEffectiveTypeParameterDeclarations (unbox decl)
    |> Seq.map (fun tpd -> checker.getTypeAtLocation (unbox tpd))
    |> typarSymbols

/// The OWN type parameters of a call signature (the method/function axis) — the env
/// a FREE FUNCTION's own typars resolve against. A free function has no declaring
/// type, so its own params occupy the single `Typar` index space unambiguously (the
/// declaring-vs-method ambiguity only bites a generic method on a generic TYPE).
let private sigTypars (sg: Ts.Signature) : Ts.Symbol list =
    match sg.getTypeParameters () with
    | Some tps -> typarSymbols (tps |> Seq.map unbox)
    | None -> []

let private lookupTypar (env: Ts.Symbol list) (t: Ts.Type) : int option =
    match t.getSymbol () with
    | Some s -> env |> List.tryFindIndex (fun e -> jsRefEq e s)
    | None -> None

/// A generic instantiation (`Array<string>`, `Box<number>`) as its target name +
/// raw type arguments, or `None` for a non-reference type. The target's symbol names
/// the generic definition (`Array`, `Box`); `getTypeArguments` yields the substituted
/// arguments (for `string[]` the element type), which the caller recurses `mapType`
/// over — so the manifest carries `Named(name, [args])`, not the printed-form blob.
let private asGenericInstantiation (checker: Ts.TypeChecker) (t: Ts.Type) : (string * Ts.Type list) option =
    if hasTargetRef t then
        let tr = unbox<Ts.TypeReference> t

        match (unbox<Ts.Type> tr.target).getSymbol () with
        | Some sym -> Some(sym.getName (), checker.getTypeArguments tr |> List.ofSeq)
        | None -> None
    else
        None

/// A printed form is a BARE NOMINAL name — a (possibly dotted) identifier — when it
/// is safe to keep as `Named(printed, [])` in the `mapType` fallthrough: a non-generic
/// class/interface/alias/enum reference, plus the deliberately un-remapped `unknown`.
/// Anything richer (a `{x:number}` STRUCTURAL object — item 14, DEFERRED; a function
/// type; a residual generic blob) is NOT nominal and the fallthrough throws on it
/// rather than degrading to a printed blob — the cross-cutting forcing function.
let private looksNominal (printed: string) : bool =
    printed.Length > 0
    && (System.Char.IsLetter printed.[0] || printed.[0] = '_' || printed.[0] = '$')
    && printed
       |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_' || c = '$' || c = '.')

// ─── ts.Type → Schema.TypeRef ──────────────────────────────────────────────

let rec mapType (checker: Ts.TypeChecker) (env: Ts.Symbol list) (t: Ts.Type) : Schema.TypeRef =
    let printed = checker.typeToString t

    // A type-parameter REFERENCE (item 11) resolves to its declaring-axis index in
    // `env`. Checked BEFORE the printed-name match: a typar prints as its bare name
    // (`T`), which would otherwise be mistaken for a nominal type. A typar NOT in
    // `env` is a METHOD-axis typar (a generic member's own `<U>`) — the schema's
    // single-axis `Typar` cannot distinguish it from the declaring axis without a
    // contract bump, so THROW rather than silently mis-map it to the declaring axis.
    if t.isTypeParameter () then
        match lookupTypar env t with
        | Some i -> Schema.TypeRef.Typar i
        | None ->
            failwithf
                "mapType: type parameter '%s' is method-axis (a generic member's own type parameter) and not in the declaring-axis env; faithfully representing a method-axis typar REFERENCE needs a second axis tag on Schema.TypeRef.Typar (a deliberate contract bump) — see ts-extraction-plan item 11"
                printed
    else

        // Literal types erase to their base, kept in ONE place (the future hook for
        // nominal string-enum lowering, currently deferred) rather than split between
        // a predicate prologue and the named-type match below. Erasure runs before the
        // union recursion so `"GET" | "POST"` collapses to `string`, not a union of
        // singletons. NB: prefer the runtime predicates / `typeToString`, never raw
        // `TypeFlags` numerics — the vendored binding's flag values can drift from the
        // installed TypeScript's. String/number literals MUST be caught here: their
        // printed form is the literal text (`"GET"`, `42`), unmatchable below. Boolean
        // literals have no predicate in the binding, but print as `true`/`false`.
        let literalBase =
            if t.isStringLiteral () then
                Some(Schema.TypeRef.Named("string", []))
            elif t.isNumberLiteral () then
                Some(Schema.TypeRef.Named("float", []))
            elif printed = "true" || printed = "false" then
                Some(Schema.TypeRef.Named("bool", []))
            else
                None

        match literalBase with
        | Some baseTy -> baseTy
        | None ->
            match printed with
            | "string" -> Schema.TypeRef.Named("string", [])
            | "number" -> Schema.TypeRef.Named("float", []) // TS number → Vesper float (policy)
            | "boolean" -> Schema.TypeRef.Named("bool", [])
            | "void" -> Schema.TypeRef.Named("unit", [])
            | "null" -> Schema.TypeRef.Named("null", [])
            | "undefined" -> Schema.TypeRef.Named("undefined", [])
            // `any` → Dynamic (item 12). TS exposes no public `type.isAny()`, so classify
            // by printed form like the other primitives (never raw `TypeFlags`, per the
            // standing rule). Only `any` is in scope — `unknown` is deliberately NOT
            // remapped and still flows through the Named fallthrough.
            | "any" -> Schema.TypeRef.Dynamic
            | _ when t.isUnion () ->
                // Anonymous union → TyOr. null/undefined ride in as their own members
                // (resolved fork: NOT folded to unit). Erased literal members can
                // collapse to one base (`"GET" | "POST"` → string), so dedup and
                // unwrap a singleton.
                let members =
                    (unbox<Ts.UnionType> t).types
                    |> Seq.map (mapType checker env)
                    |> List.ofSeq
                    |> List.distinct

                match members with
                | [ single ] -> single
                | many -> Schema.TypeRef.Union many
            | _ when t.isIntersection () ->
                // Intersection `A & B` (item 15): erase to `obj`, the universal supertype,
                // for v1. Detected via the runtime `isIntersection` predicate (sibling of
                // the `isUnion` arm above), never raw `TypeFlags`. Object-intersections
                // graduate onto item 14's structural content-hash path once it exists — do
                // not build that mechanism here.
                Schema.TypeRef.Named("obj", [])
            | _ ->
                // Generic instantiation (`Array<string>`, `Box<number>`): a `TypeReference`
                // → `Named(target name, mapped args)` (item 11), recursing `mapType` over the
                // type arguments rather than emitting the printed-form blob (`Named("string[]")`).
                match asGenericInstantiation checker t with
                | Some(name, args) -> Schema.TypeRef.Named(name, args |> List.map (mapType checker env))
                | None ->
                    // Tightened fallthrough (cross-cutting producer discipline): now that
                    // generics are handled, keep only a BARE NOMINAL name as `Named`; THROW on a
                    // structural/anonymous object type (item 14, DEFERRED — the intended forcing
                    // function) or any other genuinely-unknown printed form (function types,
                    // exotic primitives) rather than silently degrading to `Named(printed)`.
                    if looksNominal printed then
                        Schema.TypeRef.Named(printed, [])
                    else
                        failwithf
                            "mapType: unhandled type '%s' — structural/anonymous object types are deferred (item 14), and other non-nominal forms (function types, exotic primitives) are not yet mapped; extract a sharper representation before admitting it"
                            printed

let private mapParam (checker: Ts.TypeChecker) (env: Ts.Symbol list) (p: Ts.Symbol) : Schema.Param =
    // A parameter symbol's declaration is the `ParameterDeclaration` node carrying
    // the syntactic optional/rest markers. Classify by TOKEN PRESENCE on the node
    // (runtime-structural, per the producer discipline), never raw numeric flags:
    //   optional ⇐ `x?: T` (questionToken) OR `x: T = default` (initializer);
    //   rest     ⇐ `...x: T[]` (dotDotDotToken).
    let decl = declOf p
    let paramDecl = unbox<Ts.ParameterDeclaration> decl

    {
        Name = p.getName ()
        Type = mapType checker env (checker.getTypeOfSymbolAtLocation (p, decl))
        Optional = paramDecl.questionToken.IsSome || paramDecl.initializer.IsSome
        Rest = paramDecl.dotDotDotToken.IsSome
    }

/// `env` is the typar scope the parameter / return types resolve against — the
/// DECLARING type's typars for a member, or the function's OWN typars for a free
/// function (item 11). `TypeParams` is the signature's own generic-parameter count
/// (the method axis) regardless of `env`; for a free function it coincides with `env`.
let private mapSignature (checker: Ts.TypeChecker) (env: Ts.Symbol list) (sg: Ts.Signature) : Schema.Signature =
    {
        TypeParams =
            sg.getTypeParameters ()
            |> Option.map (fun a -> a.Count)
            |> Option.defaultValue 0
        Params = sg.getParameters () |> Seq.map (mapParam checker env) |> List.ofSeq
        Returns = mapType checker env (sg.getReturnType ())
    }

/// Guard against asymmetric get/set accessors (TS 4.3 `get x(): string` / `set
/// x(v: number)`): a pure TS-ism with no analog on either backend (CLR properties
/// are type-symmetric, JS is untyped), so we `failwith` rather than pay for a
/// second schema type field speculatively. Only an accessor carrying BOTH a getter
/// and a setter can diverge — a get-only or set-only accessor is trivially
/// symmetric. We reach the two halves through the symbol's `declarations` (an
/// accessor symbol holds both the `GetAccessorDeclaration` and the
/// `SetAccessorDeclaration`), classified by the runtime `isGetAccessor` /
/// `isSetAccessor` predicates (never raw `SyntaxKind` numerics, per producer
/// discipline). The getter's RETURN type and the setter's lone PARAMETER type are
/// each resolved through `getSignatureFromDeclaration` and compared at the mapped
/// `TypeRef` level so the comparison sees what the manifest would actually carry.
let private checkAccessorSymmetry (checker: Ts.TypeChecker) (env: Ts.Symbol list) (prop: Ts.Symbol) : unit =
    let flags = prop.getFlags ()

    if
        hasFlag flags Ts.SymbolFlags.GetAccessor
        && hasFlag flags Ts.SymbolFlags.SetAccessor
    then
        let decls =
            match prop.declarations with
            | Some ds -> List.ofSeq ds
            | None -> []

        let getter = decls |> List.tryFind (fun d -> ts.isGetAccessor (unbox d))
        let setter = decls |> List.tryFind (fun d -> ts.isSetAccessor (unbox d))

        match getter, setter with
        | Some g, Some s ->
            let getReturn =
                match checker.getSignatureFromDeclaration (unbox g) with
                | Some sg -> mapType checker env (sg.getReturnType ())
                | None -> failwithf "accessor '%s' getter has no resolvable signature" (prop.getName ())

            let setParam =
                match checker.getSignatureFromDeclaration (unbox s) with
                | Some sg ->
                    let ps = sg.getParameters ()

                    if ps.Count <> 1 then
                        failwithf
                            "accessor '%s' setter must take exactly one parameter (got %d)"
                            (prop.getName ())
                            ps.Count

                    mapType checker env (checker.getTypeOfSymbolAtLocation (ps.[0], unbox s))
                | None -> failwithf "accessor '%s' setter has no resolvable signature" (prop.getName ())

            if getReturn <> setParam then
                failwithf
                    "accessor '%s' has asymmetric get/set types (get returns %A, set accepts %A); a TS-only construct with no backend analog — extract a sharper schema before admitting it"
                    (prop.getName ())
                    getReturn
                    setParam
        | _ -> ()

/// `isStatic` is supplied by the caller, not read off the symbol: instance members
/// are walked off the class's DECLARED (instance) type and the static side off the
/// constructor-function type, so the side is known by WHICH walk produced `prop`
/// rather than re-derived per symbol (item 3 — the flag used to be hardcoded false).
///
/// A get/set ACCESSOR (item 10) carries no `Method` flag and its type-at-location is
/// the resolved property type (not a call signature), so it falls through to the
/// `Property` branch alongside data properties — exactly the interim mapping (both
/// lower to `x.foo` on JS). The only extra work is the asymmetric-type guard.
///
/// `env` is the declaring type's typar scope (item 11): a member typed `T` resolves
/// to its declaring-axis `Typar` index. The member's OWN method-axis typars are
/// deliberately NOT added — a generic member (`map<U>(…)`) referencing its own `U`
/// throws in `mapType` (method-axis references need a schema-axis contract bump),
/// while the additive method `TypeParams` COUNT is still emitted per signature.
let private mapMember
    (checker: Ts.TypeChecker)
    (env: Ts.Symbol list)
    (isStatic: bool)
    (prop: Ts.Symbol)
    : Schema.Member =
    checkAccessorSymmetry checker env prop

    let t = checker.getTypeOfSymbolAtLocation (prop, declOf prop)
    let callSigs = t.getCallSignatures ()

    let isMethod =
        callSigs.Count > 0 && hasFlag (prop.getFlags ()) Ts.SymbolFlags.Method

    if isMethod then
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Method
            Type = None
            Signatures = callSigs |> Seq.map (mapSignature checker env) |> List.ofSeq
            Static = isStatic
            Optional = false
        }
    else
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Property
            Type = Some(mapType checker env t)
            Signatures = []
            Static = isStatic
            Optional = false
        }

/// Construct signatures (`getConstructSignatures()` on a class's constructor-function
/// type, or on an interface carrying a `new(): T` signature) collapse to ONE `.ctor`
/// member whose `Signatures` list holds every overload. The provider expands that one
/// member into N `ExternalMember.ctor`s (one per signature, each keyed by its argSig),
/// so the wire form stays a single `Member` and the seam convention lives consumer-side.
/// `Static = false`: a constructor is an instance-producing member, per the seam.
///
/// A construct signature's RETURN type references the declaring typars (`new
/// Container<T>()` → `Container<T>`), resolved against `env` (declaring axis) → `Typar
/// i`. TS models a generic class's construct signatures as generic OVER the class
/// typars, so `getTypeParameters()` reports them — but per the seam a constructor
/// carries NO method-axis typars (the class typars are the DECLARING axis, already
/// counted in the type's `typeParams`). So force each ctor signature's `TypeParams`
/// to 0, matching the seam's "MethodArity = 0 for every constructor" convention.
let private ctorMemberOf
    (checker: Ts.TypeChecker)
    (env: Ts.Symbol list)
    (ctorSigs: ResizeArray<Ts.Signature>)
    : Schema.Member option =
    if ctorSigs.Count = 0 then
        None
    else
        Some
            {
                Name = ".ctor"
                Kind = Schema.MemberKind.Method
                Type = None
                Signatures =
                    ctorSigs
                    |> Seq.map (fun sg ->
                        { mapSignature checker env sg with
                            TypeParams = 0
                        }
                    )
                    |> List.ofSeq
                Static = false
                Optional = false
            }

/// Classify a top-level export's import shape — the wire field that selects the
/// import intrinsic at lowering. The export-TABLE entry's ESCAPED NAME carries the
/// `export default` / `export =` brand (TS stores them under the reserved internal
/// names `InternalSymbolName.Default` / `ExportEquals`); a module/namespace symbol
/// surfaces as a `Namespace` import. Per the producer discipline we classify via
/// the binding's NAME CONSTANTS and `SymbolFlags` predicates, never raw numeric
/// flag literals (the vendored flag values can drift from the installed TS).
/// `escaped` comes from the alias/export entry; `resolved` is the followed-through
/// underlying symbol whose flags name the namespace case.
let private importShapeOf (resolved: Ts.Symbol) (escaped: string) : Schema.ImportShape =
    if escaped = unbox<string> Ts.InternalSymbolName.Default then
        Schema.ImportShape.Default
    elif escaped = unbox<string> Ts.InternalSymbolName.ExportEquals then
        Schema.ImportShape.CommonJsExport
    elif hasFlag (resolved.getFlags ()) Ts.SymbolFlags.Module then
        Schema.ImportShape.Namespace
    else
        Schema.ImportShape.Named

/// `extends` bases of a declared class/interface type, via `checker.getBaseTypes`
/// (item 16). For an `interface` this is EVERY extended interface (interfaces extend
/// many); for a `class` it is the single base CLASS — TS deliberately keeps a class's
/// `implements` interfaces OUT of `getBaseTypes`, listing only the base class there.
/// Each base maps through the existing `mapType` (its printed nominal name → `Named`).
/// The declared type of a class/interface symbol IS an `InterfaceType` at runtime; the
/// `unbox` is a Fable no-op cast satisfying the binding's parameter type.
let private extendsBases (checker: Ts.TypeChecker) (env: Ts.Symbol list) (declared: Ts.Type) : Schema.TypeRef list =
    checker.getBaseTypes (unbox<Ts.InterfaceType> declared)
    |> Seq.map (fun bt -> mapType checker env (unbox<Ts.Type> bt))
    |> List.ofSeq

/// A class's `implements` interfaces — the half `getBaseTypes` omits (see `extendsBases`).
/// Walk the class declaration's heritage clauses, keep only the `implements` clause
/// (classified by the `SyntaxKind.ImplementsKeyword` CONSTANT, never a raw numeric, per
/// producer discipline), and resolve each entry's referenced interface SYMBOL to its
/// declared type. We resolve through the symbol (`getSymbolAtLocation` on the clause
/// expression, following an `Alias`) rather than `getTypeAtLocation` on the expression,
/// because a type-only interface has no value meaning at that expression position — the
/// symbol's declared type carries the nominal identity `mapType` needs. Throw on an
/// unresolvable entry rather than silently dropping a declared interface.
let private classImplements
    (checker: Ts.TypeChecker)
    (env: Ts.Symbol list)
    (resolved: Ts.Symbol)
    : Schema.TypeRef list =
    match resolved.declarations with
    | None -> []
    | Some ds ->
        ds
        |> Seq.collect (fun d ->
            match (unbox<Ts.ClassLikeDeclarationBase> d).heritageClauses with
            | Some clauses ->
                clauses
                |> Seq.filter (fun c -> int c.token = int Ts.SyntaxKind.ImplementsKeyword)
                |> Seq.collect (fun c -> c.types)
                |> Seq.map (fun e ->
                    match checker.getSymbolAtLocation (unbox e.expression) with
                    | Some s ->
                        let target =
                            if hasFlag (s.getFlags ()) Ts.SymbolFlags.Alias then
                                checker.getAliasedSymbol s
                            else
                                s

                        mapType checker env (checker.getDeclaredTypeOfSymbol target)
                    | None -> failwithf "class implements clause entry has no resolvable interface symbol"
                )
            | None -> Seq.empty
        )
        |> List.ofSeq

let rec private mapExport (checker: Ts.TypeChecker) (sym: Ts.Symbol) : Schema.Export option =
    // Follow re-export aliases (`export { x } from …`, `export default <named>`,
    // `export = <named>`) so flags/type/name are read off the REAL underlying
    // symbol, not the alias stub. The export-table entry's escaped name still
    // carries the import-shape brand, so capture it BEFORE resolving.
    let escaped: string = unbox<string> (sym.getEscapedName ())

    let resolved =
        if hasFlag (sym.getFlags ()) Ts.SymbolFlags.Alias then
            checker.getAliasedSymbol sym
        else
            sym

    let flags = resolved.getFlags ()
    let import = importShapeOf resolved escaped

    // `export default function greet` stores the symbol under the reserved name
    // "default" (it is NOT an alias, so flag-resolution above leaves it as-is);
    // recover the authored declaration name so the binding isn't literally named
    // "default". Re-export aliases (`export = legacy`) already resolve to a real
    // named symbol, so this only fires for the inline-default case; an anonymous
    // default keeps the "default" sentinel.
    let name =
        let raw = resolved.getName ()

        if raw = unbox<string> Ts.InternalSymbolName.Default then
            match resolved.valueDeclaration with
            | Some d ->
                match (unbox<Ts.NamedDeclaration> d).name with
                | Some n -> unbox<string> (unbox<Ts.Identifier> n).escapedText
                | None -> raw
            | None -> raw
        else
            raw

    if hasFlag flags Ts.SymbolFlags.Interface then
        let declared = checker.getDeclaredTypeOfSymbol resolved
        // Declaring-axis typar scope (item 11): a member typed `T` resolves to its
        // index here; `typeParams` is this list's length (was hardcoded 0).
        let env = declaredTypars declared

        let members =
            checker.getPropertiesOfType declared
            |> Seq.map (mapMember checker env false)
            |> List.ofSeq

        // An interface can carry a `new(): T` construct signature (the
        // constructor-interface idiom, `interface FooCtor { new(): Foo }`); it lands
        // on the DECLARED type itself. Append it as a `.ctor` member like a class.
        let ctorMember =
            ctorMemberOf checker env (declared.getConstructSignatures ()) |> Option.toList

        // Heritage (item 16): an interface's heritage is its `extends` interfaces only
        // (an interface cannot have a base class), so `getBaseTypes` alone is faithful.
        Some(Schema.Export.Interface(name, List.length env, members @ ctorMember, extendsBases checker env declared))
    elif hasFlag flags Ts.SymbolFlags.Class then
        // Two distinct walks keep the static/instance split honest (item 3): the
        // DECLARED type yields the instance members; the symbol's TYPE-AT-LOCATION is
        // the constructor-function (static) type whose `getProperties` are the static
        // members and whose `getConstructSignatures` are the constructors. The static
        // side also surfaces the synthetic `prototype` slot — filter it (it is not an
        // authored member). A class may itself be `export default class`, so it reuses
        // the resolved `name`/`import` computed above.
        let instanceTy = checker.getDeclaredTypeOfSymbol resolved
        let staticTy = checker.getTypeOfSymbolAtLocation (resolved, declOf resolved)
        // Declaring-axis typar scope (item 11): the class's own type parameters, read
        // off the instance (declared) type. Statics cannot reference them in TS, so the
        // env is inert there; passing it uniformly is harmless and keeps one path.
        let env = declaredTypars instanceTy

        let instanceMembers =
            checker.getPropertiesOfType instanceTy
            |> Seq.map (mapMember checker env false)
            |> List.ofSeq

        let staticMembers =
            staticTy.getProperties ()
            |> Seq.filter (fun p -> p.getName () <> "prototype")
            |> Seq.map (mapMember checker env true)
            |> List.ofSeq

        let ctorMember =
            ctorMemberOf checker env (staticTy.getConstructSignatures ()) |> Option.toList

        // Heritage (item 16): a class's `extends` base CLASS comes from `getBaseTypes`
        // on the instance type, its `implements` interfaces from the heritage clauses
        // (`getBaseTypes` omits them). Emitted as ONE flat list (extends first); the
        // provider disambiguates base-class vs interface by name-resolving each entry
        // against the manifest's type table (the schema carries no base/interface bit).
        let heritage =
            extendsBases checker env instanceTy @ classImplements checker env resolved

        Some(Schema.Export.Class(name, List.length env, instanceMembers @ staticMembers @ ctorMember, heritage, import))
    elif hasFlag flags Ts.SymbolFlags.Function then
        let t = checker.getTypeOfSymbolAtLocation (resolved, declOf resolved)

        // A free function's OWN type parameters (item 11): each call signature is mapped
        // against its own typars (no declaring type, so the single `Typar` index space is
        // unambiguous). `identity<T>(x: T): T` → `TypeParams = 1`, params/return `Typar 0`.
        let sigs =
            t.getCallSignatures ()
            |> Seq.map (fun sg -> mapSignature checker (sigTypars sg) sg)
            |> List.ofSeq

        Some(Schema.Export.Function(name, sigs, import))
    elif hasFlag flags Ts.SymbolFlags.Variable then
        // `export const`/`let`/`var` and ambient `declare const` — a singleton VALUE
        // (not an arrow). Its type rides `getTypeOfSymbolAtLocation`. Const-ness comes
        // from the binding's COMBINED node flags: the `const`/`let` keyword lives on the
        // enclosing `VariableDeclarationList`, not the `VariableDeclaration`, so
        // `getCombinedNodeFlags` walks up to surface it. Per the producer discipline we
        // classify via the `NodeFlags.Const` CONSTANT, never a raw numeric literal.
        let decl = declOf resolved
        let varTy = checker.getTypeOfSymbolAtLocation (resolved, decl)

        let isConst = int (ts.getCombinedNodeFlags decl) &&& int Ts.NodeFlags.Const <> 0

        Some(Schema.Export.Variable(name, mapType checker [] varTy, isConst, import))
    elif hasFlag flags Ts.SymbolFlags.Enum then
        // `enum` AND `const enum` (`SymbolFlags.Enum` ORs `RegularEnum | ConstEnum`).
        // Read members straight off the `EnumDeclaration.members` node list — NOT
        // `getPropertiesOfType` on the declared type, which returns the underlying
        // `number`/`string` PROTOTYPE members (an enum's apparent type is its primitive
        // base), not the authored cases. Each member's value comes from
        // `checker.getConstantValue` on the member node: a STRING member yields
        // `U2.Case1 s` (kept verbatim), a NUMERIC member `U2.Case2 n` (stringified — the
        // schema stores `string option`); a computed member with no constant value
        // yields `None`. The member's name rides its declaration symbol.
        let enumDecl = unbox<Ts.EnumDeclaration> (declOf resolved)

        let members =
            enumDecl.members
            |> Seq.map (fun em ->
                let memberName =
                    match checker.getSymbolAtLocation (unbox em.name) with
                    | Some s -> s.getName ()
                    | None -> failwithf "enum '%s' has a member with no resolvable name symbol" name

                let value =
                    match checker.getConstantValue (unbox em) with
                    | Some(U2.Case1 s) -> Some s
                    | Some(U2.Case2 n) -> Some(string n)
                    | None -> None

                memberName, value
            )
            |> List.ofSeq

        Some(Schema.Export.Enum(name, members))
    elif hasFlag flags Ts.SymbolFlags.TypeAlias then
        // `type X = …`. Emit the RESOLVED target: `getDeclaredTypeOfSymbol` on a type
        // alias yields the aliased type, so alias-to-union / -primitive / -structural all
        // flow through the same `mapType` the members use. Declaring-axis typars (item 11):
        // an alias's target is not an `InterfaceType`, so read the typar scope off the
        // declaration's effective type-parameter list; `Pair<A,B> = A | B` → `typeParams =
        // 2`, target `Union [Typar 0; Typar 1]`. The authorial `aliasTypeArguments` capture
        // (authored vs resolved form) is still deferred.
        let target = checker.getDeclaredTypeOfSymbol resolved
        let env = declTyparsOf checker (declOf resolved)
        Some(Schema.Export.TypeAlias(name, List.length env, mapType checker env target))
    elif hasFlag flags Ts.SymbolFlags.Module then
        // `namespace NS { … }` / `module NS { … }` (item 17). `SymbolFlags.Module`
        // is the named constant ORing `ValueModule | NamespaceModule` — the SAME
        // classification Tier-0's `importShapeOf` uses to brand a namespace IMPORT,
        // reused here to detect the namespace EXPORT. Recurse the namespace's
        // exported members through `mapExport` (so a nested namespace flows through
        // this very arm) and collect into the nested `Export list`.
        // `getExportsOfModule` returns the members in a stable declaration order,
        // preserved here so the canonical-form golden stays deterministic.
        //
        // DECLARATION MERGING: a namespace symbol can ALSO carry a class/function/
        // interface flag (`class C {}; namespace C {}`). Those flags are tested
        // ABOVE this arm, so a merged symbol emits its DOMINANT declaration and
        // never reaches here — the namespace half is dropped for v1. A pure
        // namespace (Module with no other handled flag) is the only thing that
        // lands here.
        // TODO(merge): a merged class+namespace loses its namespace exports; emit
        // both halves once the seam models a type carrying a static namespace.
        let nested =
            checker.getExportsOfModule resolved
            |> List.ofSeq
            |> List.choose (mapExport checker)

        Some(Schema.Export.Namespace(name, nested))
    else
        None

// ─── drive + emit ──────────────────────────────────────────────────────────

/// Walk a MODULE symbol's exports into the manifest's `Export` list. Shared by the
/// single-file path (`extractFile`) and the package-entry path (`extractPackage`):
/// both resolve a module symbol — one for a local `.d.ts`, one for the package entry
/// the synthetic-entry program pulled in — and from there the export surface is the
/// same. `getExportsOfModule` deliberately omits the `export =` entry (a CommonJS
/// `export = X` is not a named member of the module — `tryGetMemberInModuleExports`
/// filters it out too), so read it straight from the symbol's export table under its
/// reserved internal name and prepend it. The entry is an alias; `mapExport` follows it.
let private extractModuleExports (checker: Ts.TypeChecker) (moduleSym: Ts.Symbol) : Schema.Export list =
    let exportSyms =
        let named = checker.getExportsOfModule moduleSym |> List.ofSeq
        let exportEqKey: Ts.__String = U2.Case2 Ts.InternalSymbolName.ExportEquals

        match moduleSym.exports with
        | Some tbl when tbl.has exportEqKey -> tbl.get exportEqKey :: named
        | _ -> named

    exportSyms |> List.choose (mapExport checker)

let private moduleSymbolOf (checker: Ts.TypeChecker) (sf: Ts.SourceFile) (label: string) : Ts.Symbol =
    match checker.getSymbolAtLocation (unbox sf) with
    | None -> failwithf "'%s' is not a module (no exports found)" label
    | Some moduleSym -> moduleSym

// Compiler options shared by both paths. `strict` keeps `T | null` from collapsing
// to `T` (strictNullChecks); `skipLibCheck`/`noEmit` keep the run lib-agnostic and
// side-effect-free. The package path additionally needs module resolution wired (it
// resolves a bare specifier through node's algorithm), but adding those options to
// the single-file program would not change its exports — so for safety the
// single-file builder is left byte-for-byte as before and the package builder layers
// the resolution options on top.
let private baseOptions () : Ts.CompilerOptions =
    jsOptions<Ts.CompilerOptions> (fun o ->
        o.strict <- Some true
        o.skipLibCheck <- Some true
        o.noEmit <- Some true
    )

let extractFile (dtsPath: string) (packageName: string) : Schema.PackageManifest =
    let options = baseOptions ()
    let program = ts.createProgram (ResizeArray [ dtsPath ], options)
    let checker = program.getTypeChecker ()

    match program.getSourceFile dtsPath with
    | None -> failwithf "could not load source file '%s'" dtsPath
    | Some sf ->
        let moduleSym = moduleSymbolOf checker sf dtsPath

        {
            SchemaVersion = Schema.SchemaVersion
            Package = packageName
            // A single local `.d.ts` carries no package version (no resolving
            // `package.json`), so the stamp stays `null` — preserved exactly.
            Version = None
            Exports = extractModuleExports checker moduleSym
        }

/// The package version stamp (item 18). Preference order:
///   1. the resolver's `packageId.version` — populated when the entry was resolved
///      out of `node_modules` (an installed `@types/*` package);
///   2. the nearest `package.json`'s `version` walking up from the resolved entry —
///      the local / relative case the resolver leaves `packageId`-less.
/// Returns `None` only when neither yields a string (the stamp is genuinely absent).
let private packageVersionOf (resolvedModule: Ts.ResolvedModuleFull) (resolvedFileName: string) : string option =
    let fromPackageId =
        match resolvedModule.packageId with
        | Some pid when not (System.String.IsNullOrEmpty pid.version) -> Some pid.version
        | _ -> None

    match fromPackageId with
    | Some _ -> fromPackageId
    | None ->
        // Walk up from the entry file's directory to the filesystem root, taking the
        // FIRST `package.json` found — the package's own manifest sits closest, so it
        // wins over any ancestor (a monorepo root, the repo itself).
        let rec walk (dir: string) : string option =
            let pj = pathJoin dir "package.json"

            if existsSync pj then
                jsonVersionField (JS.JSON.parse (readFileSyncUtf8 pj "utf8"))
            else
                let parent = pathDirname dir

                if parent = dir then None else walk parent

        walk (pathDirname resolvedFileName)

/// Pull a package's full `.d.ts` module-graph closure via the synthetic-entry-file
/// approach (item 18). A throwaway entry module that `export *`-s the requested
/// `specifier` is written into `resolveFromDir` (so BOTH relative specifiers and
/// `node_modules` resolution anchor there); `ts.createProgram` over it pulls the entry
/// plus everything it re-exports/imports across files. We then resolve the specifier to
/// the package ENTRY source file and walk ITS exports (following its cross-file
/// re-exports) — not the synthetic entry's — and stamp the package version.
let extractPackage (specifier: string) (resolveFromDir: string) (packageName: string) : Schema.PackageManifest =
    let options = baseOptions ()
    // Module resolution must be wired for the bare/relative specifier to resolve and
    // for the closure to be pulled. Node10 is the classic node algorithm (honours a
    // package's `types`/`typings` and `index.d.ts`); ESNext module keeps `export *`
    // an ES re-export. Set via the typed enum constants, never raw numerics.
    options.moduleResolution <- Some Ts.ModuleResolutionKind.Node10
    options.``module`` <- Some Ts.ModuleKind.ESNext

    let host = ts.createCompilerHost options
    // The synthetic entry lives in `resolveFromDir` under a reserved name; a `.ts`
    // (not `.d.ts`) so its `export *` is an ordinary module re-export. Removed in the
    // `finally` so a fixture directory is never left polluted, even on a throw.
    let entryPath = pathJoin resolveFromDir "__vesper_synthetic_entry__.ts"
    writeFileSync entryPath (sprintf "export * from \"%s\";\n" specifier)

    try
        let program = ts.createProgram (ResizeArray [ entryPath ], options, host)
        let checker = program.getTypeChecker ()

        // Resolve the specifier to the package's entry `.d.ts` (+ version) through the
        // SAME host/options the program used, so the resolved path matches a program
        // source file. `host` is a `CompilerHost`, a subtype of the `ModuleResolutionHost`
        // the resolver wants.
        let resolution = ts.resolveModuleName (specifier, entryPath, options, host)

        match resolution.resolvedModule with
        | None -> failwithf "could not resolve package '%s' from '%s'" specifier resolveFromDir
        | Some resolvedModule ->
            let resolvedFileName = resolvedModule.resolvedFileName

            // The program loaded the closure keyed by TS's normalised file names
            // (forward slashes). `getSourceFile` keys on the same normalisation, but the
            // resolver's path can differ by slash direction on Windows — so fall back to
            // a slash-normalised scan of the program's source files before giving up.
            let sf =
                match program.getSourceFile resolvedFileName with
                | Some sf -> sf
                | None ->
                    let norm (p: string) = p.Replace("\\", "/")
                    let target = norm resolvedFileName

                    match program.getSourceFiles () |> Seq.tryFind (fun f -> norm f.fileName = target) with
                    | Some sf -> sf
                    | None -> failwithf "resolved entry '%s' is not in the program" resolvedFileName

            let moduleSym = moduleSymbolOf checker sf resolvedFileName

            {
                SchemaVersion = Schema.SchemaVersion
                Package = packageName
                Version = packageVersionOf resolvedModule resolvedFileName
                Exports = extractModuleExports checker moduleSym
            }
    finally
        if existsSync entryPath then
            unlinkSync entryPath

let run (dtsPath: string) (packageName: string) (outPath: string) : unit =
    let manifest = extractFile dtsPath packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length

let runPackage (specifier: string) (resolveFromDir: string) (packageName: string) (outPath: string) : unit =
    let manifest = extractPackage specifier resolveFromDir packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length
