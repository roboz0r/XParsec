/// `ts.Type` → `Schema.TypeRef`, plus param and signature mapping. Literal types,
/// `keyof T`, `T[K]` and conditionals are recorded verbatim, never evaluated here.
/// What cannot be represented degrades (`obj`, opaque `Structural`) and warns.
module Vesper.Ts.Extractor.TypeMap

open Fable.Core.JsInterop

open TypeScript
open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.TsInterop
open Vesper.Ts.Extractor.Diagnostics

// ─── typar environments ──────────────────────────────────────────────────────

let typarSymbols (tps: Ts.Type seq) : Ts.Symbol list =
    tps
    |> Seq.map (fun tp ->
        match tp.getSymbol () with
        | Some s -> s
        | None -> failwith "type parameter has no symbol"
    )
    |> List.ofSeq

/// Declaring-axis typars in declaration order — a class/interface DECLARED type is an
/// `InterfaceType` at runtime, which is where `typeParameters` lives.
let declaredTypars (declared: Ts.Type) : Ts.Symbol list =
    match (unbox<Ts.InterfaceType> declared).typeParameters with
    | Some tps -> typarSymbols (tps |> Seq.map unbox)
    | None -> []

/// The same, in source order, for a `type` alias — whose declared target is not an
/// `InterfaceType` and so has no `typeParameters`.
let declTyparsOf (checker: Ts.TypeChecker) (decl: Ts.Node) : Ts.Symbol list =
    ts.getEffectiveTypeParameterDeclarations (unbox decl)
    |> Seq.map (fun tpd -> checker.getTypeAtLocation (unbox tpd))
    |> typarSymbols

let lookupTypar (env: Ts.Symbol list) (t: Ts.Type) : int option =
    match t.getSymbol () with
    | Some s -> env |> List.tryFindIndex (fun e -> jsRefEq e s)
    | None -> None

// ─── structural classification ───────────────────────────────────────────────

/// A generic instantiation as its target name, the target's symbol and the substituted
/// type arguments: `string[]` → `("Array", …, [string])`. `None` for anything else.
let asGenericInstantiation (checker: Ts.TypeChecker) (t: Ts.Type) : (string * Ts.Symbol * Ts.Type list) option =
    if hasTargetRef t then
        let tr = unbox<Ts.TypeReference> t

        match (unbox<Ts.Type> tr.target).getSymbol () with
        // `Handler<T> = (event: T) => void` used as `Handler<Events[Key]>` is a
        // `TypeReference` onto the reserved `__type` name, not a nominal generic:
        // `None` here sends it to the structural classification, keeping the `Fun` shape.
        | Some sym when isAnonymousTypeName (sym.getName ()) -> None
        | Some sym -> Some(sym.getName (), sym, checker.getTypeArguments tr |> List.ofSeq)
        | None -> None
    else
        None

/// A printed form is a bare (possibly dotted) identifier — `Emitter`, `NS.Foo` — and so
/// keepable as `Named(printed, [])`. `{ x: number }`, `(e: T) => void` and a residual
/// generic blob are not. The reserved `__type` name is all word chars, hence excluded.
let looksNominal (printed: string) : bool =
    printed.Length > 0
    && not (isAnonymousTypeName printed)
    && (System.Char.IsLetter printed.[0] || printed.[0] = '_' || printed.[0] = '$')
    && printed
       |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_' || c = '$' || c = '.')

/// TRUE for `(args) => ret` and nothing else: a callable object that also carries
/// members, and a constructor type `new () => T`, both stay structural. Callers gate
/// additionally on a non-nominal printed form to keep named callable interfaces named.
let isFunctionType (t: Ts.Type) : bool =
    // EXACTLY one: `Fun` carries one shape, so an overloaded anonymous function type
    // (`{ (): void; (x: string): void }`) must stay structural rather than lose sigs 1..n.
    (t.getCallSignatures ()).Count = 1
    && (t.getConstructSignatures ()).Count = 0
    && (t.getProperties ()).Count = 0

/// Can a `Structural` carry the WHOLE of this type? Needs a property or index signature
/// to carry, and no call or construct signature, which a field list would drop silently.
let carriesFaithfullyAsFields (checker: Ts.TypeChecker) (t: Ts.Type) : bool =
    ((t.getProperties ()).Count > 0 || (checker.getIndexInfosOfType t).Count > 0)
    && (t.getCallSignatures ()).Count = 0
    && (t.getConstructSignatures ()).Count = 0

/// An object whose entire content maps losslessly to `Structural` fields. Tuples and
/// arrays are excluded: a tuple's `getProperties()` yields `"0"`, `"1"`, `"length"`.
let isPureRecordObject (checker: Ts.TypeChecker) (t: Ts.Type) : bool =
    isObjectTypeFlag t ts
    && not (checker.isTupleType t)
    && not (checker.isArrayType t)
    && carriesFaithfullyAsFields checker t

// ─── ts.Type → Schema.TypeRef ──────────────────────────────────────────────

/// Real lib types nest a few levels; a self-recursive conditional (`Awaited<T>`)
/// recurses unbounded and blows the JS stack at ~thousands of frames. This sits between
/// the two, so tripping it degrades the subtree to `obj` instead of dying.
let maxMapTypeDepth = 200

let rec mapType (ctx: MapCtx) (t: Ts.Type) : Schema.TypeRef =
    ctx.Depth.Value <- ctx.Depth.Value + 1

    try
        if ctx.Depth.Value > maxMapTypeDepth then
            let span = t.getSymbol () |> Option.bind tryDeclOf |> Option.map spanOfNode

            emitWarning
                ctx
                Schema.DiagCode.RecursionDepthExceeded
                (ctx.Checker.typeToString t)
                span
                (sprintf
                    "type mapping exceeded depth %d (a self-recursive conditional such as Awaited<T>); the subtree was degraded to obj"
                    maxMapTypeDepth)

            Schema.TypeRef.Named("obj", [])
        else
            mapTypeInner ctx t
    finally
        ctx.Depth.Value <- ctx.Depth.Value - 1

/// Carry an object as `Structural(printed, …)`: its own members as fields when
/// `faithful`, else warn and carry empty. Merged and anonymous members are synthetic —
/// no declaration node — so their types come from `getTypeOfSymbol`.
and private carryStructural (ctx: MapCtx) (t: Ts.Type) (printed: string) (faithful: bool) : Schema.TypeRef =
    if faithful then
        let fields =
            t.getProperties ()
            |> Seq.map (fun p -> p.getName (), structuralFieldType ctx p)
            |> List.ofSeq

        Schema.TypeRef.Structural(printed, fields, mapIndexInfo ctx t)
    else
        emitWarning
            ctx
            Schema.DiagCode.StructuralObjectStubbed
            printed
            (t.getSymbol () |> Option.bind tryDeclOf |> Option.map spanOfNode)
            (sprintf "structural type '%s' has no faithful representation; carried as an opaque Structural" printed)

        Schema.TypeRef.Structural(printed, [], [])

/// A `Structural` field is a bare `(name, TypeRef)` with no optional channel, so `foo?: T`
/// carries as `Union [T; undefined]`. Optionality lives on the SYMBOL: tsc pre-evaluates
/// `Partial<T>` to properties still typed `T[P]`, with only the flag set.
and private structuralFieldType (ctx: MapCtx) (p: Ts.Symbol) : Schema.TypeRef =
    let ty = mapType ctx (ctx.Checker.getTypeOfSymbol p)

    if hasFlag (p.getFlags ()) Ts.SymbolFlags.Optional then
        let undef = Schema.TypeRef.Named("undefined", [])

        match ty with
        // Don't double-add `undefined` if the resolved type already carries it.
        | Schema.TypeRef.Union ds when List.contains undef ds -> ty
        | Schema.TypeRef.Union ds -> Schema.TypeRef.Union(ds @ [ undef ])
        | _ -> Schema.TypeRef.Union [ ty; undef ]
    else
        ty

/// The `(key, value)` pairs of `t`'s index signatures `{ [k: K]: V }`. `getIndexInfosOfType`
/// flattens inherited ones, so `ProcessEnv extends Dict<T>` needs no heritage walk here.
and mapIndexInfo (ctx: MapCtx) (t: Ts.Type) : (Schema.TypeRef * Schema.TypeRef) list =
    ctx.Checker.getIndexInfosOfType t
    |> Seq.map (fun info -> mapType ctx info.keyType, mapType ctx info.``type``)
    |> List.ofSeq

and private mapTypeInner (ctx: MapCtx) (t: Ts.Type) : Schema.TypeRef =
    let checker = ctx.Checker
    let printed = checker.typeToString t

    // Before the printed-name match below: a typar prints as its bare name (`T`), which
    // would otherwise look nominal. Declaring axis first, matching F# scoping — a name in
    // both axes binds to the declaring slot.
    if t.isTypeParameter () then
        match lookupTypar ctx.DeclaringEnv t with
        | Some i -> Schema.TypeRef.Typar i
        | None ->
            match lookupTypar ctx.MethodEnv t with
            | Some i -> Schema.TypeRef.MethodTypar i
            | None ->
                let span = t.getSymbol () |> Option.bind tryDeclOf |> Option.map spanOfNode

                emitWarning
                    ctx
                    Schema.DiagCode.MethodAxisTyparErased
                    printed
                    span
                    (sprintf
                        "type parameter '%s' is bound by neither the declaring nor the method axis; erased to obj"
                        printed)

                Schema.TypeRef.Named("obj", [])
    else

        // Caught before the printed-name match: `"GET"`, `keyof Events`, `Events[Key]`
        // and `… ? … : …` are unmatchable there. Two degrades are deliberate and silent:
        // a boolean literal erases to `bool`, a non-integer numeric to `float`.
        let special =
            if t.isStringLiteral () then
                Some(Schema.TypeRef.Literal(Schema.LiteralValue.StringVal (unbox<Ts.StringLiteralType> t).value))
            elif t.isNumberLiteral () then
                let v = (unbox<Ts.NumberLiteralType> t).value

                if System.Math.Floor v = v && not (System.Double.IsInfinity v) then
                    Some(Schema.TypeRef.Literal(Schema.LiteralValue.IntVal(int64 v)))
                else
                    Some(Schema.TypeRef.Named("float", []))
            elif printed = "true" || printed = "false" then
                Some(Schema.TypeRef.Named("bool", []))
            elif t.isIndexType () then
                let inner = unbox<Ts.Type> (unbox<Ts.IndexType> t).``type``
                Some(Schema.TypeRef.KeyOf(mapType ctx inner))
            elif isIndexedAccessType t then
                let iat = unbox<Ts.IndexedAccessType> t
                Some(Schema.TypeRef.IndexedAccess(mapType ctx iat.objectType, mapType ctx iat.indexType))
            elif isConditionalType t then
                let ct = unbox<Ts.ConditionalType> t

                // Both sources give the AUTHORED branch, never the evaluated pick.
                // `resolvedTrue/FalseType` are `None` for an uninstantiated conditional
                // (a still-open `T`), so fall back to the node's own branch `TypeNode`.
                let branch (resolved: Ts.Type option) (node: Ts.TypeNode) =
                    match resolved with
                    | Some r -> r
                    | None -> checker.getTypeFromTypeNode node

                Some(
                    Schema.TypeRef.Conditional(
                        mapType ctx ct.checkType,
                        mapType ctx ct.extendsType,
                        mapType ctx (branch ct.resolvedTrueType ct.root.node.trueType),
                        mapType ctx (branch ct.resolvedFalseType ct.root.node.falseType)
                    )
                )
            elif checker.isTupleType t then
                // Gated to the F#-expressible shape: a 0-/1-tuple has no F# form, and
                // `[K, V?]` / `[K, ...V[]]` are not carriable positionally, so both take
                // the opaque-`Structural` degrade instead. `readonly` and labels drop.
                let target = (unbox<Ts.TupleTypeReference> t).target
                let elems = checker.getTypeArguments (unbox<Ts.TypeReference> t)

                let allRequired =
                    target.elementFlags |> Seq.forall (fun f -> f = Ts.ElementFlags.Required)

                if elems.Count >= 2 && allRequired then
                    Some(Schema.TypeRef.Tuple(elems |> Seq.map (mapType ctx) |> List.ofSeq))
                else
                    None
            else
                None

        match special with
        | Some ty -> ty
        | None ->
            match printed with
            | "string" -> Schema.TypeRef.Named("string", [])
            | "number" -> Schema.TypeRef.Named("number", []) // kept as `number`: wider than any one Vesper numeric type
            | "boolean" -> Schema.TypeRef.Named("bool", [])
            | "void" -> Schema.TypeRef.Named("unit", [])
            | "null" -> Schema.TypeRef.Named("null", [])
            | "undefined" -> Schema.TypeRef.Named("undefined", [])
            // Designed, not a degradation, hence no diagnostic. TS exposes no `type.isAny()`,
            // so it goes by printed form. `unknown` is deliberately not remapped.
            | "any" -> Schema.TypeRef.Dynamic
            | _ when t.isUnion () ->
                // `null`/`undefined` ride in as their own disjuncts, never folded to `unit`.
                // Literal disjuncts stay distinct (`"GET" | "POST"` → two `Literal`s), so the
                // dedup only collapses genuine duplicates.
                let disjuncts =
                    (unbox<Ts.UnionType> t).types
                    |> Seq.map (mapType ctx)
                    |> List.ofSeq
                    |> List.distinct

                match disjuncts with
                | [ single ] -> single
                | many -> Schema.TypeRef.Union many
            | _ when t.isIntersection () ->
                // Object-only (`Named & Aged`) means every constituent is a
                // `TypeFlags.Object`; the checker has merged them into one member set.
                // `string & Brand` and a generic `T & U` are not, and are a real loss.
                let constituents = (unbox<Ts.IntersectionType> t).types

                let objectOnly =
                    constituents.Count > 0
                    && constituents |> Seq.forall (fun c -> isObjectTypeFlag (unbox<Ts.Type> c) ts)

                if objectOnly then
                    // An intersection is not itself a `TypeFlags.Object`, so faithfulness is
                    // judged on the merged member set rather than by `isPureRecordObject`.
                    carryStructural ctx t printed (carriesFaithfullyAsFields checker t)
                else
                    emitWarning
                        ctx
                        Schema.DiagCode.IntersectionErased
                        printed
                        (t.getSymbol () |> Option.bind tryDeclOf |> Option.map spanOfNode)
                        (sprintf
                            "intersection type '%s' erased to obj (a non-object intersection has no structural form)"
                            printed)

                    Schema.TypeRef.Named("obj", [])
            | _ ->
                // `string[]` → `Named("Array", [Named "string"])`, recursing over the type
                // arguments rather than emitting the printed blob `Named("string[]")`.
                match asGenericInstantiation checker t with
                | Some(name, targetSym, args) ->
                    // Only the target is homed here; each argument recurses and homes itself.
                    recordForeignRef ctx name targetSym
                    Schema.TypeRef.Named(name, args |> List.map (mapType ctx))
                | None when isFunctionType t && not (looksNominal printed) ->
                    let callSig = (t.getCallSignatures ()).[0]

                    let paramTypes =
                        callSig.getParameters ()
                        |> Seq.map (fun p -> mapType ctx (checker.getTypeOfSymbolAtLocation (p, declOf p)))
                        |> List.ofSeq

                    let ret = mapType ctx (callSig.getReturnType ())
                    Schema.TypeRef.Fun(paramTypes, ret)
                | None ->
                    if looksNominal printed then
                        // An intrinsic without a symbol (`symbol`, `never`, `unknown`) is left
                        // unhomed: it is a Vesper primitive, not a foreign type.
                        (match t.getSymbol () with
                         | Some sym -> recordForeignRef ctx printed sym
                         | None -> ())

                        Schema.TypeRef.Named(printed, [])
                    else
                        // An anonymous `{ x: number }`, keyed on its printed form as a stable
                        // content hash. Anything but a pure record stays opaque: partial members
                        // read as a complete type, and `string | symbol` drags in `toString`/….
                        carryStructural ctx t printed (isPureRecordObject checker t)

let mapParam (ctx: MapCtx) (p: Ts.Symbol) : Schema.Param =
    // Optional and rest are read as TOKEN PRESENCE on the `ParameterDeclaration`, never
    // as numeric flags: `x?: T` and `x: T = default` are both optional.
    let decl = declOf p
    let paramDecl = unbox<Ts.ParameterDeclaration> decl

    {
        Name = p.getName ()
        Type = mapType ctx (ctx.Checker.getTypeOfSymbolAtLocation (p, decl))
        Optional = paramDecl.questionToken.IsSome || paramDecl.initializer.IsSome
        Rest = paramDecl.dotDotDotToken.IsSome
    }

/// WHICH kind of signature is being mapped — which axis its own typars occupy, and
/// whether their bounds are extracted.
[<RequireQualifiedAccess>]
type SigAxis =
    /// Own typars ride the METHOD axis, and their authored constraints
    /// (`<Key extends keyof Events>`) are carried onto `TypeParamBounds` unevaluated.
    | MemberMethod
    /// No declaring type, so own typars take the DECLARING index space; no bounds.
    | FreeFunction
    /// Own typars split by ORIGIN: TS makes a real generic class's construct signatures
    /// generic over the CLASS typars, already on the declaring axis, while `interface
    /// FooCtor { new <T>(v: T): Foo<T> }` introduces fresh ones for the method axis.
    | Ctor

let mapSignature (ctx: MapCtx) (axis: SigAxis) (sg: Ts.Signature) : Schema.Signature =
    let ownTypars =
        sg.getTypeParameters () |> Option.map List.ofSeq |> Option.defaultValue []

    let ownTyparSyms () =
        typarSymbols (ownTypars |> Seq.map unbox)

    /// The authored constraint of one own-typar, read off the DECLARATION node — NOT
    /// `getConstraint()`, which resolves `keyof Events` to `string | number | symbol`,
    /// losing the `Events` to fold against. The node keeps it symbolic.
    let boundOf (bodyCtx: MapCtx) (tp: Ts.Type) : Schema.TypeRef option =
        match (unbox<Ts.Type> tp).getSymbol () with
        | Some s ->
            match s.declarations with
            | Some ds when ds.Count > 0 ->
                match ts.getEffectiveConstraintOfTypeParameter (unbox ds.[0]) with
                | Some node -> Some(mapType bodyCtx (ctx.Checker.getTypeFromTypeNode node))
                | None -> None
            | _ -> None
        | None -> None

    let bodyCtx, typeParams, bounds =
        match axis with
        | SigAxis.MemberMethod ->
            let bodyCtx = { ctx with MethodEnv = ownTyparSyms () }

            bodyCtx, List.length ownTypars, ownTypars |> List.map (boundOf bodyCtx)
        | SigAxis.FreeFunction ->
            let bodyCtx =
                { ctx with
                    DeclaringEnv = ownTyparSyms ()
                    MethodEnv = []
                }

            bodyCtx, List.length ownTypars, ownTypars |> List.map (fun _ -> None)
        | SigAxis.Ctor ->
            // Seeding the method env lets a FRESH construct-sig typar resolve to
            // `MethodTypar i`, while declaring-first resolution keeps a real class's on
            // `Typar`. Method arity counts only typars the declaring axis does not bind.
            let ownSyms = ownTyparSyms ()

            let freshCount =
                ownSyms
                |> List.filter (fun s -> not (ctx.DeclaringEnv |> List.exists (fun e -> jsRefEq e s)))
                |> List.length

            { ctx with MethodEnv = ownSyms }, freshCount, List.replicate freshCount None

    {
        TypeParams = typeParams
        TypeParamBounds = bounds
        Params = sg.getParameters () |> Seq.map (mapParam bodyCtx) |> List.ofSeq
        Returns = mapType bodyCtx (sg.getReturnType ())
    }
