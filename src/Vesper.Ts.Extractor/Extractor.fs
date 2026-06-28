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

let private mapMember (checker: Ts.TypeChecker) (prop: Ts.Symbol) : Schema.Member =
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
            Static = false
            Optional = false
        }
    else
        {
            Name = prop.getName ()
            Kind = Schema.MemberKind.Property
            Type = Some(mapType checker t)
            Signatures = []
            Static = false
            Optional = false
        }

let private mapExport (checker: Ts.TypeChecker) (sym: Ts.Symbol) : Schema.Export option =
    let flags = sym.getFlags ()
    let name = sym.getName ()

    if hasFlag flags Ts.SymbolFlags.Interface then
        let declared = checker.getDeclaredTypeOfSymbol sym

        let members =
            checker.getPropertiesOfType declared
            |> Seq.map (mapMember checker)
            |> List.ofSeq

        Some(Schema.Export.Interface(name, 0, members, []))
    elif hasFlag flags Ts.SymbolFlags.Function then
        let t = checker.getTypeOfSymbolAtLocation (sym, declOf sym)

        let sigs = t.getCallSignatures () |> Seq.map (mapSignature checker) |> List.ofSeq

        Some(Schema.Export.Function(name, sigs, Schema.ImportShape.Named)) // TODO: detect import shape
    else
        None // TODO: Class / TypeAlias / Enum / Variable / Namespace

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
            let exports =
                checker.getExportsOfModule moduleSym
                |> Seq.choose (mapExport checker)
                |> List.ofSeq

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
