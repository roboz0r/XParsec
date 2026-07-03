/// `ts.Symbol` → `Schema.Export`/`Schema.Member` — the export/member-walking half
/// of the extractor. Each export arm seeds the `MapCtx` typar axes and delegates
/// every type encoding to `TypeMap`.
module Vesper.Ts.Extractor.ExportMap

open Fable.Core
open Fable.Core.JsInterop

open TypeScript
open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.TsInterop
open Vesper.Ts.Extractor.Diagnostics
open Vesper.Ts.Extractor.TypeMap

/// Guard against asymmetric get/set accessors (TS 4.3 `get x(): string` / `set
/// x(v: number)`): a pure TS-ism with no analog on either backend (CLR properties
/// are type-symmetric, JS is untyped). Only an accessor carrying BOTH a getter
/// and a setter can diverge — a get-only or set-only accessor is trivially
/// symmetric. We reach the two halves through the symbol's `declarations` (an
/// accessor symbol holds both the `GetAccessorDeclaration` and the
/// `SetAccessorDeclaration`), classified by the runtime `isGetAccessor` /
/// `isSetAccessor` predicates (never raw `SyntaxKind` numerics, per producer
/// discipline). The getter's RETURN type and the setter's lone PARAMETER type are
/// each resolved through `getSignatureFromDeclaration` and compared at the mapped
/// `TypeRef` level so the comparison sees what the manifest would actually carry.
let private checkAccessorSymmetry (ctx: MapCtx) (prop: Ts.Symbol) : unit =
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
            // Resolve the getter's return + setter's param as `TypeRef option`; a
            // structural obstacle (no resolvable signature, a setter that isn't
            // exactly-one-param) DEGRADES to `None` + a diagnostic rather than aborting
            // the whole class — the symmetry check simply cannot run and the property
            // still lowers to its `getTypeOfSymbolAtLocation` type in `mapMember`.
            let getReturn =
                match ctx.Checker.getSignatureFromDeclaration (unbox g) with
                | Some sg -> Some(mapType ctx (sg.getReturnType ()))
                | None -> None

            let setParam =
                match ctx.Checker.getSignatureFromDeclaration (unbox s) with
                | Some sg ->
                    let ps = sg.getParameters ()

                    if ps.Count <> 1 then
                        None
                    else
                        Some(mapType ctx (ctx.Checker.getTypeOfSymbolAtLocation (ps.[0], unbox s)))
                | None -> None

            match getReturn, setParam with
            | None, _
            | _, None ->
                emitWarning
                    ctx
                    Schema.DiagCode.AccessorSignatureUnresolved
                    (prop.getName ())
                    (Some(spanOfNode (unbox g)))
                    (sprintf
                        "accessor '%s' has an unresolvable get/set signature (or a non-unary setter); the get/set symmetry check was skipped and the property kept its resolved type"
                        (prop.getName ()))
            | Some getReturn, Some setParam when getReturn <> setParam ->
                // Asymmetric get/set types: a TS-only construct with no backend analog.
                // DEGRADE by NARROWING to the getter's type — the property already lowers
                // to `getTypeOfSymbolAtLocation` (the getter's return) in `mapMember`, so
                // recording the warning here is enough; the read side is the one kept.
                emitWarning
                    ctx
                    Schema.DiagCode.AsymmetricAccessorNarrowed
                    (prop.getName ())
                    (Some(spanOfNode (unbox g)))
                    (sprintf
                        "accessor '%s' has asymmetric get/set types (get returns %A, set accepts %A); narrowed to the getter's type"
                        (prop.getName ())
                        getReturn
                        setParam)
            // Symmetric (get type = set type): nothing to degrade.
            | Some _, Some _ -> ()
        | _ -> ()

/// `isStatic` is supplied by the caller, not read off the symbol: instance members
/// are walked off the class's DECLARED (instance) type and the static side off the
/// constructor-function type, so the side is known by WHICH walk produced `prop`
/// rather than re-derived per symbol (item 3).
///
/// A get/set ACCESSOR (item 10) carries no `Method` flag and its type-at-location is
/// the resolved property type (not a call signature), so it falls through to the
/// `Property` branch alongside data properties — exactly the interim mapping (both
/// lower to `x.foo` on JS). The only extra work is the asymmetric-type guard.
///
/// `ctx.DeclaringEnv` is the declaring type's typar scope (item 11): a member typed
/// `T` resolves to its declaring-axis `Typar` index. A generic METHOD's OWN typars
/// (`map<U>(…)`) become the method-axis env inside `mapSignature SigAxis.MemberMethod`,
/// so a reference to `U` maps to `MethodTypar i` — faithful, no longer erased. A
/// PROPERTY carries no method typars (`ctx` arrives with an empty `MethodEnv`).
let private mapMember (ctx: MapCtx) (isStatic: bool) (prop: Ts.Symbol) : Schema.Member =
    checkAccessorSymmetry ctx prop

    let t = ctx.Checker.getTypeOfSymbolAtLocation (prop, declOf prop)
    let callSigs = t.getCallSignatures ()

    let isMethod =
        callSigs.Count > 0 && hasFlag (prop.getFlags ()) Ts.SymbolFlags.Method

    if isMethod then
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Method
            Type = None
            Signatures = callSigs |> Seq.map (mapSignature ctx SigAxis.MemberMethod) |> List.ofSeq
            Static = isStatic
            Optional = false
        }
    else
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Property
            Type = Some(mapType ctx t)
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
/// The no-method-axis rule lives in `SigAxis.Ctor`.
let private ctorMemberOf (ctx: MapCtx) (ctorSigs: ResizeArray<Ts.Signature>) : Schema.Member option =
    if ctorSigs.Count = 0 then
        None
    else
        Some
            {
                Name = ".ctor"
                Kind = Schema.MemberKind.Method
                Type = None
                Signatures = ctorSigs |> Seq.map (mapSignature ctx SigAxis.Ctor) |> List.ofSeq
                Static = false
                Optional = false
            }

/// Classify a top-level export's import shape — the wire field that selects the
/// import intrinsic at lowering. The export-TABLE entry's ESCAPED NAME carries the
/// `export default` / `export =` brand (TS stores them under the reserved internal
/// names `InternalSymbolName.Default` / `ExportEquals`); a module/namespace symbol
/// surfaces as a `Namespace` import. Per the producer discipline we classify via
/// the binding's NAME CONSTANTS and `SymbolFlags` predicates, never raw numeric
/// flag literals. `escaped` comes from the alias/export entry; `resolved` is the
/// followed-through underlying symbol whose flags name the namespace case.
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
let private extendsBases (ctx: MapCtx) (declared: Ts.Type) : Schema.TypeRef list =
    ctx.Checker.getBaseTypes (unbox<Ts.InterfaceType> declared)
    |> Seq.map (fun bt -> mapType ctx (unbox<Ts.Type> bt))
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
let private classImplements (ctx: MapCtx) (resolved: Ts.Symbol) : Schema.TypeRef list =
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
                // An entry whose interface symbol cannot be resolved DEGRADES to `None`
                // (dropped from the heritage list) + a diagnostic, rather than aborting the
                // whole class — a lib-scale resilience over the former hard throw.
                |> Seq.choose (fun e ->
                    match ctx.Checker.getSymbolAtLocation (unbox e.expression) with
                    | Some s ->
                        let target =
                            if hasFlag (s.getFlags ()) Ts.SymbolFlags.Alias then
                                ctx.Checker.getAliasedSymbol s
                            else
                                s

                        Some(mapType ctx (ctx.Checker.getDeclaredTypeOfSymbol target))
                    | None ->
                        emitWarning
                            ctx
                            Schema.DiagCode.HeritageEntryUnresolved
                            (resolved.getName ())
                            (Some(spanOfNode (unbox e)))
                            (sprintf
                                "class '%s' has an implements-clause entry with no resolvable interface symbol; the entry was dropped"
                                (resolved.getName ()))

                        None
                )
            | None -> Seq.empty
        )
        |> List.ofSeq

/// The shared class-like extraction that BOTH the `Class` export arm AND the
/// fused-global path use (a global `interface Map<K,V>` + `declare var Map:
/// MapConstructor` merge into ONE symbol Vesper needs as ONE `Export.Class`). Two
/// distinct walks keep the static/instance split honest (item 3): the DECLARED type
/// yields the instance members and the declaring-axis typars; the symbol's
/// TYPE-AT-LOCATION is the constructor-function (static) type whose `getProperties`
/// are the static members and whose `getConstructSignatures` are the constructors.
/// The static side surfaces the synthetic `prototype` slot — filtered (not an
/// authored member). `name`/`import` are supplied by the caller: a `class` may be
/// `export default class` (reusing its resolved brand); a fused GLOBAL is always a
/// plain `Named` global. Statics cannot reference the class typars in TS, so the env
/// is inert on the static walk; passing it uniformly keeps one path.
///
/// Heritage (item 16): a class's `extends` base CLASS comes from `getBaseTypes` on
/// the instance type, its `implements` interfaces from the heritage clauses
/// (`getBaseTypes` omits them). Emitted as ONE flat list (extends first); the
/// provider disambiguates base-class vs interface by name-resolving each entry
/// against the manifest's type table (the schema carries no base/interface bit).
let private classLikeExport
    (ctx0: MapCtx)
    (resolved: Ts.Symbol)
    (name: string)
    (import: Schema.ImportShape)
    : Schema.Export =
    let checker = ctx0.Checker
    let instanceTy = checker.getDeclaredTypeOfSymbol resolved
    let staticTy = checker.getTypeOfSymbolAtLocation (resolved, declOf resolved)
    let env = declaredTypars instanceTy

    let ctx = { ctx0 with DeclaringEnv = env }

    let instanceMembers =
        checker.getPropertiesOfType instanceTy
        |> Seq.map (mapMember ctx false)
        |> List.ofSeq

    let staticMembers =
        staticTy.getProperties ()
        |> Seq.filter (fun p -> p.getName () <> "prototype")
        |> Seq.map (mapMember ctx true)
        |> List.ofSeq

    let ctorMember =
        ctorMemberOf ctx (staticTy.getConstructSignatures ()) |> Option.toList

    let heritage = extendsBases ctx instanceTy @ classImplements ctx resolved

    Schema.Export.Class(name, List.length env, instanceMembers @ staticMembers @ ctorMember, heritage, import)

/// `ctx0` is the walk ROOT (empty typar axes); each arm seeds `DeclaringEnv` with
/// its declaration's own typar scope.
let rec private mapExport (ctx0: MapCtx) (sym: Ts.Symbol) : Schema.Export option =
    let checker = ctx0.Checker
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

    // DECLARATION MERGING (item 17, deferred): a namespace (Module) symbol can ALSO
    // carry a dominant type/value flag (`class C {}; namespace C {}`). A dominant arm
    // below wins and the namespace half is DROPPED for v1 — record the drop rather
    // than silently losing it. A PURE namespace carries ONLY the Module flag and is
    // faithfully emitted by the Module arm, so it must NOT diagnose. Span anchors on
    // the dropped `ModuleDeclaration` node when present.
    if
        hasFlag flags Ts.SymbolFlags.Module
        && (hasFlag flags Ts.SymbolFlags.Class
            || hasFlag flags Ts.SymbolFlags.Interface
            || hasFlag flags Ts.SymbolFlags.Function
            || hasFlag flags Ts.SymbolFlags.Enum
            || hasFlag flags Ts.SymbolFlags.Variable
            || hasFlag flags Ts.SymbolFlags.TypeAlias)
    then
        let nsSpan =
            match resolved.declarations with
            | Some ds ->
                ds
                |> Seq.tryFind (fun d -> ts.isModuleDeclaration (unbox d))
                |> Option.map (fun d -> spanOfNode (unbox d))
            | None -> None

        emitWarning
            ctx0
            Schema.DiagCode.MergedNamespaceDropped
            name
            nsSpan
            (sprintf
                "declaration-merged namespace '%s' dropped; the dominant declaration is kept (the seam does not yet model a type carrying a static namespace)"
                name)

    if hasFlag flags Ts.SymbolFlags.Interface then
        let declared = checker.getDeclaredTypeOfSymbol resolved
        // Declaring-axis typar scope (item 11): a member typed `T` resolves to its
        // index here; `typeParams` is this list's length.
        let env = declaredTypars declared

        let ctx = { ctx0 with DeclaringEnv = env }

        let members =
            checker.getPropertiesOfType declared
            |> Seq.map (mapMember ctx false)
            |> List.ofSeq

        // An interface can carry a `new(): T` construct signature (the
        // constructor-interface idiom, `interface FooCtor { new(): Foo }`); it lands
        // on the DECLARED type itself. Append it as a `.ctor` member like a class.
        let ctorMember =
            ctorMemberOf ctx (declared.getConstructSignatures ()) |> Option.toList

        // Heritage (item 16): an interface's heritage is its `extends` interfaces only
        // (an interface cannot have a base class), so `getBaseTypes` alone is faithful.
        Some(Schema.Export.Interface(name, List.length env, members @ ctorMember, extendsBases ctx declared))
    elif hasFlag flags Ts.SymbolFlags.Class then
        // The instance/static two-walk lives in the shared `classLikeExport` (the
        // fused-global path calls it too). A class may itself be `export default class`,
        // so it reuses the resolved `name`/`import` computed above.
        Some(classLikeExport ctx0 resolved name import)
    elif hasFlag flags Ts.SymbolFlags.Function then
        let t = checker.getTypeOfSymbolAtLocation (resolved, declOf resolved)

        // A free function's OWN type parameters (item 11): each call signature is mapped
        // against its own typars (`SigAxis.FreeFunction` seeds them as the declaring
        // axis). `identity<T>(x: T): T` → `TypeParams = 1`, params/return `Typar 0`.
        let sigs =
            t.getCallSignatures ()
            |> Seq.map (mapSignature ctx0 SigAxis.FreeFunction)
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

        Some(Schema.Export.Variable(name, mapType ctx0 varTy, isConst, import))
    elif hasFlag flags Ts.SymbolFlags.Enum then
        // `enum` AND `const enum` (`SymbolFlags.Enum` ORs `RegularEnum | ConstEnum`).
        // Read members straight off the `EnumDeclaration.members` node list — NOT
        // `getPropertiesOfType` on the declared type, which returns the underlying
        // `number`/`string` PROTOTYPE members (an enum's apparent type is its primitive
        // base), not the authored cases. Each member's value comes from
        // `checker.getConstantValue` on the member node: a STRING member yields
        // `U2.Case1 s` (kept verbatim as `StringVal`), a NUMERIC member `U2.Case2 n`
        // (a JS float — type-tagged as `IntVal` after the integer-subset check below);
        // a computed member with no constant value yields `None`. The member's name
        // rides its declaration symbol.
        let enumDecl = unbox<Ts.EnumDeclaration> (declOf resolved)

        let members =
            enumDecl.members
            |> Seq.map (fun em ->
                let memberName =
                    match checker.getSymbolAtLocation (unbox em.name) with
                    | Some s -> s.getName ()
                    // A member with no resolvable name symbol degrades to a reserved
                    // sentinel (kept as a member so the enum arity survives) + a
                    // diagnostic, rather than aborting the extraction.
                    | None ->
                        emitWarning
                            ctx0
                            Schema.DiagCode.EnumMemberDegraded
                            name
                            (Some(spanOfNode (unbox em)))
                            (sprintf
                                "enum '%s' has a member with no resolvable name symbol; kept as '__unresolved__'"
                                name)

                        "__unresolved__"

                let value =
                    match checker.getConstantValue (unbox em) with
                    | Some(U2.Case1 s) -> Some(Schema.LiteralValue.StringVal s)
                    | Some(U2.Case2 n) ->
                        // Integer-subset discipline: TS technically permits non-integer
                        // numeric enum members, but the wire only carries `IntVal of
                        // int64`. At lib scale, DEGRADE a non-integer to `None` (a computed
                        // member) + a diagnostic rather than widen/round or abort.
                        if System.Math.Floor n <> n || System.Double.IsInfinity n then
                            emitWarning
                                ctx0
                                Schema.DiagCode.EnumMemberDegraded
                                (sprintf "%s.%s" name memberName)
                                (Some(spanOfNode (unbox em)))
                                (sprintf
                                    "enum '%s' member '%s' has a non-integer numeric value (%g); the value was dropped (no int64 wire form)"
                                    name
                                    memberName
                                    n)

                            None
                        else
                            Some(Schema.LiteralValue.IntVal(int64 n))
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

        let ctx = { ctx0 with DeclaringEnv = env }

        Some(Schema.Export.TypeAlias(name, List.length env, mapType ctx target))
    elif hasFlag flags Ts.SymbolFlags.Module then
        // `namespace NS { … }` / `module NS { … }` (item 17). `SymbolFlags.Module`
        // is the named constant ORing `ValueModule | NamespaceModule` — the SAME
        // classification `importShapeOf` uses to brand a namespace IMPORT, reused
        // here to detect the namespace EXPORT. Recurse the namespace's exported
        // members through `mapExport` (so a nested namespace flows through this
        // very arm) and collect into the nested `Export list`.
        // `getExportsOfModule` returns the members in a stable declaration order,
        // preserved here so the canonical-form golden stays deterministic.
        //
        // DECLARATION MERGING: a merged symbol emits its DOMINANT declaration (the
        // flags above are tested first) and never reaches here — the namespace half
        // is dropped for v1 with the diagnostic above. TODO(merge): emit both halves
        // once the seam models a type carrying a static namespace.
        let nested =
            checker.getExportsOfModule resolved
            |> List.ofSeq
            |> List.choose (mapExport ctx0)

        Some(Schema.Export.Namespace(name, nested))
    else
        None

/// Walk a MODULE symbol's exports into the manifest's `Export` list. Shared by the
/// single-file path (`extractFile`) and the package-entry path (`extractPackage`):
/// both resolve a module symbol — one for a local `.d.ts`, one for the package entry
/// the synthetic-entry program pulled in — and from there the export surface is the
/// same. `getExportsOfModule` deliberately omits the `export =` entry (a CommonJS
/// `export = X` is not a named member of the module — `tryGetMemberInModuleExports`
/// filters it out too), so read it straight from the symbol's export table under its
/// reserved internal name and prepend it. The entry is an alias; `mapExport` follows it.
let extractModuleExports
    (checker: Ts.TypeChecker)
    (program: Ts.Program)
    (diags: ResizeArray<Schema.Diagnostic>)
    (refs: ResizeArray<string * Schema.RefEntry>)
    (moduleSym: Ts.Symbol)
    : Schema.Export list =
    let exportSyms =
        let named = checker.getExportsOfModule moduleSym |> List.ofSeq
        let exportEqKey: Ts.__String = U2.Case2 Ts.InternalSymbolName.ExportEquals

        match moduleSym.exports with
        | Some tbl when tbl.has exportEqKey -> tbl.get exportEqKey :: named
        | _ -> named

    exportSyms |> List.choose (mapExport (MapCtx.Root checker program diags refs))

// ─── ambient-global dispatch (the `extractGlobals` entry mode) ─────────────────
//
// A global-scope (script) `.d.ts` is enumerated by SYMBOL, not per-file statement, so
// cross-file interface MERGES are one symbol. Each enumerated global routes through
// `mapGlobalSymbol`, which PREFERS the fused class-like path (`interface Map<K,V>` +
// `declare var Map: MapConstructor` merged into one `new`-able symbol) and otherwise
// delegates to the very same `mapExport` arms the module paths use — so a pure global
// interface / free function / `declare var` / `type` alias / `enum` is byte-identical
// to its module-entry form.

/// The fused class-like predicate. A symbol carrying BOTH the Interface (type) meaning
/// AND a Value meaning is a `new`-able global class (the merged `interface Map<K,V>` +
/// `declare var Map: MapConstructor` pair) — the exact predicate `classifyKind` uses to
/// mint an `FTClass` for the refs table. A pure type-only interface lacks the Value
/// meaning and stays an `Export.Interface`.
let isFusedClassLike (sym: Ts.Symbol) : bool =
    let flags = sym.getFlags ()

    hasFlag flags Ts.SymbolFlags.Interface && hasFlag flags Ts.SymbolFlags.Value

/// The constructor-INTERFACE (`MapConstructor`) a fused symbol's value side resolves
/// to. It is CONSUMED into the fused `Export.Class` (its construct signatures → ctors,
/// its other members → statics), so the global enumerator must NOT also emit it as a
/// standalone `Export.Interface`. `None` when the value type carries no naming symbol.
let fusedCarrierSymbol (checker: Ts.TypeChecker) (sym: Ts.Symbol) : Ts.Symbol option =
    (checker.getTypeOfSymbolAtLocation (sym, declOf sym)).getSymbol ()

/// Route ONE enumerated global symbol to its export. The fused class-like path wins
/// when both the type and value meanings are present; everything else delegates to the
/// shared `mapExport` arms unchanged (so the module goldens stay byte-identical).
let mapGlobalSymbol (ctx0: MapCtx) (sym: Ts.Symbol) : Schema.Export option =
    let resolved =
        if hasFlag (sym.getFlags ()) Ts.SymbolFlags.Alias then
            ctx0.Checker.getAliasedSymbol sym
        else
            sym

    if isFusedClassLike resolved then
        Some(classLikeExport ctx0 resolved (resolved.getName ()) Schema.ImportShape.Named)
    else
        mapExport ctx0 sym
