/// `ts.Symbol` → `Schema.Export`/`Schema.Member`. Each export arm seeds the `MapCtx`
/// typar axes and delegates every type encoding to `TypeMap`.
module Vesper.Ts.Extractor.ExportMap

open Fable.Core
open Fable.Core.JsInterop

open TypeScript
open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.TsInterop
open Vesper.Ts.Extractor.Diagnostics
open Vesper.Ts.Extractor.TypeMap

/// TS allows `get x(): string` alongside `set x(v: number)`; neither backend does
/// (CLR properties are type-symmetric, JS is untyped). Compares the getter's return
/// with the setter's lone parameter, both as mapped `TypeRef`s.
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
                // Narrowing needs no code: the property's type-at-location IS the
                // getter's return, so the warning is the whole effect.
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
            | Some _, Some _ -> ()
        | _ -> ()

/// `isStatic` is supplied by the caller: instance members are walked off the DECLARED
/// type and statics off the constructor-function type. A get/set accessor carries no
/// `Method` flag, so it maps as a property.
let private mapMember (ctx: MapCtx) (isStatic: bool) (prop: Ts.Symbol) : Schema.Member =
    checkAccessorSymmetry ctx prop

    let t = ctx.Checker.getTypeOfSymbolAtLocation (prop, declOf prop)
    let callSigs = t.getCallSignatures ()

    let isMethod =
        callSigs.Count > 0 && hasFlag (prop.getFlags ()) Ts.SymbolFlags.Method

    // Optionality (`foo?: T`) is a SYMBOL flag, not a type: tsc resolves `Partial<T>` to
    // an object whose property TYPE stays `T[P]` while the symbol carries `Optional`.
    let optional = hasFlag (prop.getFlags ()) Ts.SymbolFlags.Optional

    if isMethod then
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Method
            Type = None
            Signatures = callSigs |> Seq.map (mapSignature ctx SigAxis.MemberMethod) |> List.ofSeq
            Static = isStatic
            Optional = optional
        }
    else
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Property
            Type = Some(mapType ctx t)
            Signatures = []
            Static = isStatic
            Optional = optional
        }

/// Every construct signature collapses to ONE `.ctor` member holding all overloads in
/// `Signatures`; the provider expands it back into one external ctor per signature.
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

/// The wire field that selects the import intrinsic at lowering. TS stores
/// `export default` / `export =` under reserved escaped names, so the brand rides
/// `escaped` (the export-table entry) while `resolved` supplies only the module case.
let private importShapeOf (resolved: Ts.Symbol) (escaped: string) : Schema.ImportShape =
    if escaped = unbox<string> Ts.InternalSymbolName.Default then
        Schema.ImportShape.Default
    elif escaped = unbox<string> Ts.InternalSymbolName.ExportEquals then
        Schema.ImportShape.CommonJsExport
    elif hasFlag (resolved.getFlags ()) Ts.SymbolFlags.Module then
        Schema.ImportShape.Namespace
    else
        Schema.ImportShape.Named

/// For an `interface` this is EVERY extended interface; for a `class` it is the single
/// base CLASS — TS keeps a class's `implements` interfaces OUT of `getBaseTypes`.
let private extendsBases (ctx: MapCtx) (declared: Ts.Type) : Schema.TypeRef list =
    ctx.Checker.getBaseTypes (unbox<Ts.InterfaceType> declared)
    |> Seq.map (fun bt -> mapType ctx (unbox<Ts.Type> bt))
    |> List.ofSeq

/// A class's `implements` interfaces — the half `getBaseTypes` omits. Each heritage
/// entry resolves through its SYMBOL, not `getTypeAtLocation` on the expression: a
/// type-only interface has no value meaning at that expression position.
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

/// Shared by the `Class` arm and the fused-global path (`interface Map<K,V>` +
/// `declare var Map: MapConstructor` are ONE symbol). The DECLARED type yields the
/// instance members and typars; the type-at-location is the ctor-function type.
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

    Schema.Export.Class(
        name,
        List.length env,
        instanceMembers @ staticMembers @ ctorMember,
        heritage,
        import,
        mapIndexInfo ctx instanceTy
    )

/// `ctx0` is the walk ROOT (empty typar axes); each arm seeds `DeclaringEnv` with
/// its declaration's own typar scope.
let rec private mapExport (ctx0: MapCtx) (sym: Ts.Symbol) : Schema.Export option =
    let checker = ctx0.Checker
    // The export-table entry's escaped name carries the import-shape brand, so capture
    // it BEFORE following a re-export alias to the real underlying symbol.
    let escaped: string = unbox<string> (sym.getEscapedName ())

    let resolved =
        if hasFlag (sym.getFlags ()) Ts.SymbolFlags.Alias then
            checker.getAliasedSymbol sym
        else
            sym

    let flags = resolved.getFlags ()
    let import = importShapeOf resolved escaped

    // `export default function greet` stores the symbol under the reserved name
    // "default" and is NOT an alias, so recover `greet` from the declaration; an
    // anonymous default keeps the sentinel.
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

    // `class C {}; namespace C {}` merge into one symbol carrying both flags: a dominant
    // arm below wins and the namespace half is dropped, so record it. A PURE namespace
    // carries only `Module`, is emitted faithfully, and must NOT match here.
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
        let env = declaredTypars declared

        let ctx = { ctx0 with DeclaringEnv = env }

        let members =
            checker.getPropertiesOfType declared
            |> Seq.map (mapMember ctx false)
            |> List.ofSeq

        // `interface FooCtor { new(): Foo }` — an interface's construct signatures land
        // on the DECLARED type itself, and become a `.ctor` member as for a class.
        let ctorMember =
            ctorMemberOf ctx (declared.getConstructSignatures ()) |> Option.toList

        Some(
            Schema.Export.Interface(
                name,
                List.length env,
                members @ ctorMember,
                extendsBases ctx declared,
                mapIndexInfo ctx declared
            )
        )
    elif hasFlag flags Ts.SymbolFlags.Class then
        Some(classLikeExport ctx0 resolved name import)
    elif hasFlag flags Ts.SymbolFlags.Function then
        let t = checker.getTypeOfSymbolAtLocation (resolved, declOf resolved)

        // Each call signature is mapped against its OWN typars as the declaring axis:
        // `identity<T>(x: T): T` → `TypeParams = 1`, params/return `Typar 0`.
        let sigs =
            t.getCallSignatures ()
            |> Seq.map (mapSignature ctx0 SigAxis.FreeFunction)
            |> List.ofSeq

        Some(Schema.Export.Function(name, sigs, import))
    elif hasFlag flags Ts.SymbolFlags.Variable then
        // `export const`/`let`/`var` and ambient `declare const` — a singleton VALUE.
        // The `const` keyword lives on the enclosing `VariableDeclarationList`, not the
        // `VariableDeclaration`, so const-ness needs `getCombinedNodeFlags` to walk up.
        let decl = declOf resolved
        let varTy = checker.getTypeOfSymbolAtLocation (resolved, decl)

        let isConst = int (ts.getCombinedNodeFlags decl) &&& int Ts.NodeFlags.Const <> 0

        Some(Schema.Export.Variable(name, mapType ctx0 varTy, isConst, import))
    elif hasFlag flags Ts.SymbolFlags.Enum then
        // The authored cases are the `EnumDeclaration.members` nodes, NOT
        // `getPropertiesOfType`: an enum's apparent type is its primitive base, so that
        // returns the `number`/`string` prototype members instead.
        let enumDecl = unbox<Ts.EnumDeclaration> (declOf resolved)

        let members =
            enumDecl.members
            |> Seq.map (fun em ->
                let memberName =
                    match checker.getSymbolAtLocation (unbox em.name) with
                    | Some s -> s.getName ()
                    // Kept as a member so the enum's arity survives.
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
                        // TS permits non-integer numeric enum members; the wire carries
                        // only `IntVal of int64`, so drop the value rather than round it.
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
        // `getDeclaredTypeOfSymbol` on a `type X = …` yields the RESOLVED target, which
        // is not an `InterfaceType` — so the typars come off the declaration instead:
        // `type Pair<A,B> = A | B` → `typeParams = 2`, target `Union [Typar 0; Typar 1]`.
        let target = checker.getDeclaredTypeOfSymbol resolved
        let env = declTyparsOf checker (declOf resolved)

        let ctx = { ctx0 with DeclaringEnv = env }

        Some(Schema.Export.TypeAlias(name, List.length env, mapType ctx target))
    elif hasFlag flags Ts.SymbolFlags.Module then
        // `namespace NS { … }` / `module NS { … }`: recursing the namespace's own
        // exports means a nested namespace flows back through this arm.
        // `getExportsOfModule` is in declaration order, so the golden stays stable.
        let nested =
            checker.getExportsOfModule resolved
            |> List.ofSeq
            |> List.choose (mapExport ctx0)

        Some(Schema.Export.Namespace(name, nested))
    else
        None

/// One export whose walk THROWS is diagnosed and dropped, so an exotic symbol cannot
/// cost the whole module its manifest. The in-place degrades keep their symbol; this
/// catches only what they did not anticipate.
let mapExportResilient (ctx0: MapCtx) (sym: Ts.Symbol) : Schema.Export option =
    try
        mapExport ctx0 sym
    with ex ->
        let span = tryDeclOf sym |> Option.map spanOfNode

        emitWarning
            ctx0
            Schema.DiagCode.SymbolWalkFailed
            (sym.getName ())
            span
            (sprintf "export symbol '%s' could not be extracted and was dropped: %s" (sym.getName ()) ex.Message)

        None

/// Walk a MODULE symbol's exports into the manifest's `Export` list. A CommonJS
/// `export = X` is not a named member, so `getExportsOfModule` omits it: it is read
/// from the export table under `InternalSymbolName.ExportEquals` and prepended.
let extractModuleExports
    (checker: Ts.TypeChecker)
    (program: Ts.Program)
    (diags: ResizeArray<Schema.Diagnostic>)
    (refs: ResizeArray<string * Schema.RefEntry>)
    (moduleHome: Ts.Symbol -> string option)
    (moduleSym: Ts.Symbol)
    : Schema.Export list =
    let exportSyms =
        let named = checker.getExportsOfModule moduleSym |> List.ofSeq
        let exportEqKey: Ts.__String = U2.Case2 Ts.InternalSymbolName.ExportEquals

        match moduleSym.exports with
        | Some tbl when tbl.has exportEqKey -> tbl.get exportEqKey :: named
        | _ -> named

    let ctx0 =
        { MapCtx.Root checker program diags refs with
            ModuleHome = moduleHome
        }

    exportSyms |> List.choose (mapExportResilient ctx0)

// ─── ambient-global dispatch (the `extractGlobals` entry mode) ─────────────────
// A global-scope (script) `.d.ts` is enumerated by SYMBOL, not per-file statement, so
// cross-file interface MERGES arrive as one symbol.

/// A global carrying BOTH the Interface (type) and a Value meaning is a `new`-able
/// class: the merged `interface Map<K,V>` + `declare var Map: MapConstructor` pair. A
/// type-only interface lacks the Value meaning and stays an `Export.Interface`.
let isFusedClassLike (sym: Ts.Symbol) : bool =
    let flags = sym.getFlags ()

    hasFlag flags Ts.SymbolFlags.Interface && hasFlag flags Ts.SymbolFlags.Value

/// The constructor-INTERFACE (`MapConstructor`) a fused symbol's value side resolves
/// to. It is CONSUMED into the fused class, so the global enumerator must NOT also emit
/// it standalone. `None` when the value type carries no naming symbol.
let fusedCarrierSymbol (checker: Ts.TypeChecker) (sym: Ts.Symbol) : Ts.Symbol option =
    (checker.getTypeOfSymbolAtLocation (sym, declOf sym)).getSymbol ()

/// Route ONE enumerated global symbol to its export: fused class-like if both meanings
/// are present, otherwise the same walk the module paths use.
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
