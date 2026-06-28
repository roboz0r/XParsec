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

let inline private hasFlag (flags: Ts.SymbolFlags) (test: Ts.SymbolFlags) = int flags &&& int test <> 0

/// A node to anchor `getTypeOfSymbolAtLocation` at — the symbol's declaration.
let private declOf (s: Ts.Symbol) : Ts.Node =
    match s.valueDeclaration with
    | Some d -> unbox d
    | None ->
        match s.declarations with
        | Some ds when ds.Count > 0 -> unbox ds.[0]
        | _ -> failwithf "symbol '%s' has no declaration" (s.getName ())

// ─── ts.Type → Schema.TypeRef (MVP: printed primitive names) ───────────────

let rec mapType (checker: Ts.TypeChecker) (t: Ts.Type) : Schema.TypeRef =
    let printed = checker.typeToString t

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
        | _ when t.isUnion () ->
            // Anonymous union → TyOr. null/undefined ride in as their own members
            // (resolved fork: NOT folded to unit). Erased literal members can
            // collapse to one base (`"GET" | "POST"` → string), so dedup and
            // unwrap a singleton.
            let members =
                (unbox<Ts.UnionType> t).types
                |> Seq.map (mapType checker)
                |> List.ofSeq
                |> List.distinct

            match members with
            | [ single ] -> single
            | many -> Schema.TypeRef.Union many
        | other -> Schema.TypeRef.Named(other, []) // TODO: generics, structurals, intersection

let private mapParam (checker: Ts.TypeChecker) (p: Ts.Symbol) : Schema.Param =
    {
        Name = p.getName ()
        Type = mapType checker (checker.getTypeOfSymbolAtLocation (p, declOf p))
        Optional = false // TODO: questionToken / initializer
        Rest = false
    } // TODO: dotDotDotToken

let private mapSignature (checker: Ts.TypeChecker) (sg: Ts.Signature) : Schema.Signature =
    {
        TypeParams = 0 // TODO: sg.getTypeParameters().Count
        Params = sg.getParameters () |> Seq.map (mapParam checker) |> List.ofSeq
        Returns = mapType checker (sg.getReturnType ())
    }

/// `isStatic` is supplied by the caller, not read off the symbol: instance members
/// are walked off the class's DECLARED (instance) type and the static side off the
/// constructor-function type, so the side is known by WHICH walk produced `prop`
/// rather than re-derived per symbol (item 3 — the flag used to be hardcoded false).
let private mapMember (checker: Ts.TypeChecker) (isStatic: bool) (prop: Ts.Symbol) : Schema.Member =
    let t = checker.getTypeOfSymbolAtLocation (prop, declOf prop)
    let callSigs = t.getCallSignatures ()

    let isMethod =
        callSigs.Count > 0 && hasFlag (prop.getFlags ()) Ts.SymbolFlags.Method

    if isMethod then
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Method
            Type = None
            Signatures = callSigs |> Seq.map (mapSignature checker) |> List.ofSeq
            Static = isStatic
            Optional = false
        }
    else
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Property
            Type = Some(mapType checker t)
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
let private ctorMemberOf (checker: Ts.TypeChecker) (ctorSigs: ResizeArray<Ts.Signature>) : Schema.Member option =
    if ctorSigs.Count = 0 then
        None
    else
        Some
            {
                Name = ".ctor"
                Kind = Schema.MemberKind.Method
                Type = None
                Signatures = ctorSigs |> Seq.map (mapSignature checker) |> List.ofSeq
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

let private mapExport (checker: Ts.TypeChecker) (sym: Ts.Symbol) : Schema.Export option =
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

        let members =
            checker.getPropertiesOfType declared
            |> Seq.map (mapMember checker false)
            |> List.ofSeq

        // An interface can carry a `new(): T` construct signature (the
        // constructor-interface idiom, `interface FooCtor { new(): Foo }`); it lands
        // on the DECLARED type itself. Append it as a `.ctor` member like a class.
        let ctorMember =
            ctorMemberOf checker (declared.getConstructSignatures ()) |> Option.toList

        Some(Schema.Export.Interface(name, 0, members @ ctorMember, []))
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

        let instanceMembers =
            checker.getPropertiesOfType instanceTy
            |> Seq.map (mapMember checker false)
            |> List.ofSeq

        let staticMembers =
            staticTy.getProperties ()
            |> Seq.filter (fun p -> p.getName () <> "prototype")
            |> Seq.map (mapMember checker true)
            |> List.ofSeq

        let ctorMember =
            ctorMemberOf checker (staticTy.getConstructSignatures ()) |> Option.toList

        Some(Schema.Export.Class(name, 0, instanceMembers @ staticMembers @ ctorMember, [], import))
    elif hasFlag flags Ts.SymbolFlags.Function then
        let t = checker.getTypeOfSymbolAtLocation (resolved, declOf resolved)

        let sigs = t.getCallSignatures () |> Seq.map (mapSignature checker) |> List.ofSeq

        Some(Schema.Export.Function(name, sigs, import))
    else
        None // TODO: TypeAlias / Enum / Variable / Namespace (each stamps `import`)

// ─── drive + emit ──────────────────────────────────────────────────────────

let extractFile (dtsPath: string) (packageName: string) : Schema.PackageManifest =
    let options =
        jsOptions<Ts.CompilerOptions> (fun o ->
            o.strict <- Some true // strictNullChecks on: keep `T | null` from collapsing to `T`
            o.skipLibCheck <- Some true
            o.noEmit <- Some true
        )

    let program = ts.createProgram (ResizeArray [ dtsPath ], options)
    let checker = program.getTypeChecker ()

    match program.getSourceFile dtsPath with
    | None -> failwithf "could not load source file '%s'" dtsPath
    | Some sf ->
        match checker.getSymbolAtLocation (unbox sf) with
        | None -> failwithf "'%s' is not a module (no exports found)" dtsPath
        | Some moduleSym ->
            // `getExportsOfModule` deliberately omits the `export =` entry (a
            // CommonJS `export = X` is not a named member of the module — and
            // `tryGetMemberInModuleExports` filters it out too), so read it straight
            // from the symbol's export table under its reserved internal name and
            // prepend it. The entry is an alias; `mapExport` follows it through.
            let exportSyms =
                let named = checker.getExportsOfModule moduleSym |> List.ofSeq
                let exportEqKey: Ts.__String = U2.Case2 Ts.InternalSymbolName.ExportEquals

                match moduleSym.exports with
                | Some tbl when tbl.has exportEqKey -> tbl.get exportEqKey :: named
                | _ -> named

            let exports = exportSyms |> List.choose (mapExport checker)

            {
                SchemaVersion = Schema.SchemaVersion
                Package = packageName
                Version = None
                Exports = exports
            }

let run (dtsPath: string) (packageName: string) (outPath: string) : unit =
    let manifest = extractFile dtsPath packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length
