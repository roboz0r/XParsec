/// `ts.Type` → `Schema.TypeRef` (+ param / signature mapping) — the type-encoding
/// half of the extractor. `mapType` is one recursive encoder over `MapCtx`; the
/// FAITHFUL arms (literal types, `keyof`, indexed access, conditional) record the
/// construct verbatim and NEVER evaluate it (the front end owns the fold); the
/// deliberate degrades (structural stub, intersection, bound-less typar) warn
/// through the `Diagnostics` channel.
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

/// Declaring-axis typar symbols of a class/interface DECLARED type (an
/// `InterfaceType` at runtime, whose `typeParameters` carry them in declaration order).
let declaredTypars (declared: Ts.Type) : Ts.Symbol list =
    match (unbox<Ts.InterfaceType> declared).typeParameters with
    | Some tps -> typarSymbols (tps |> Seq.map unbox)
    | None -> []

/// Declaring-axis typar symbols read off a DECLARATION node's effective
/// type-parameter list — used for `type` aliases, whose declared target type is not
/// an `InterfaceType` and so exposes no `typeParameters`. The effective declarations
/// are in source order, fixing the same index space the references resolve against.
let declTyparsOf (checker: Ts.TypeChecker) (decl: Ts.Node) : Ts.Symbol list =
    ts.getEffectiveTypeParameterDeclarations (unbox decl)
    |> Seq.map (fun tpd -> checker.getTypeAtLocation (unbox tpd))
    |> typarSymbols

let lookupTypar (env: Ts.Symbol list) (t: Ts.Type) : int option =
    match t.getSymbol () with
    | Some s -> env |> List.tryFindIndex (fun e -> jsRefEq e s)
    | None -> None

// ─── structural classification ───────────────────────────────────────────────

/// A generic instantiation (`Array<string>`, `Box<number>`) as its target name +
/// raw type arguments, or `None` for a non-reference type. The target's symbol names
/// the generic definition (`Array`, `Box`); `getTypeArguments` yields the substituted
/// arguments (for `string[]` the element type), which the caller recurses `mapType`
/// over — so the manifest carries `Named(name, [args])`, not the printed-form blob.
let asGenericInstantiation (checker: Ts.TypeChecker) (t: Ts.Type) : (string * Ts.Type list) option =
    if hasTargetRef t then
        let tr = unbox<Ts.TypeReference> t

        match (unbox<Ts.Type> tr.target).getSymbol () with
        // An ANONYMOUS type literal instantiated with type arguments (a generic type
        // ALIAS whose body is an anonymous object/function type — `Handler<T> = (event:
        // T) => void` referenced as `Handler<Events[Key]>`) surfaces as a
        // `TypeReference` whose target symbol is the reserved `__type` name. That is
        // NOT a named nominal generic — treating it as `Named("__type", …)` drops the
        // faithful function shape — so reject it here and let `mapType`'s fallthrough
        // classify the (instantiated) type by structure (`isFunctionType` → `Fun`,
        // else the structural stub). The call signature's parameter/return types are
        // already substituted, so the faithful `Fun` carries the resolved `Events[Key]`
        // indexed access.
        | Some sym when isAnonymousTypeName (sym.getName ()) -> None
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
///
/// The reserved anonymous-type name (`isAnonymousTypeName`) is a valid-word-char
/// string, so the raw predicate below would MISCLASSIFY it as nominal (collapsing an
/// alias application like `Handler<Events[Key]>`, which the checker resolves to an
/// anonymous `(event: Events[Key]) => void`, to a bogus nominal and dropping the
/// faithful function shape). Excluding it routes the anonymous type to the `mapType`
/// fallthrough where `isFunctionType` maps it to `Fun` (or the structural stub for a
/// non-function anonymous type).
let looksNominal (printed: string) : bool =
    printed.Length > 0
    && not (isAnonymousTypeName printed)
    && (System.Char.IsLetter printed.[0] || printed.[0] = '_' || printed.[0] = '$')
    && printed
       |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_' || c = '$' || c = '.')

/// A PURE function value type — the shape `(args) => ret`: at least one call
/// signature, NO construct signatures, and NO own data properties. A callable object
/// that ALSO carries members is not a bare arrow (it stays structural); a constructor
/// type (`new () => T`) is excluded by the no-construct-signature clause. Lets
/// `mapType` map a function-typed alias/parameter faithfully to `TypeRef.Fun` instead
/// of degrading it to a structural stub. The CALLER additionally gates on the printed
/// form being non-nominal, so a NAMED interface that merely declares a call signature
/// keeps its nominal identity rather than collapsing to an anonymous arrow.
let isFunctionType (t: Ts.Type) : bool =
    // EXACTLY one call signature: `TypeRef.Fun` can carry only one shape, so an
    // OVERLOADED anonymous function type (`{ (): void; (x: string): void }`) must
    // NOT enter the fast path (it would silently drop signatures 1..n) — it falls
    // to the structural stub instead, which emits its degradation warning.
    (t.getCallSignatures ()).Count = 1
    && (t.getConstructSignatures ()).Count = 0
    && (t.getProperties ()).Count = 0

// ─── ts.Type → Schema.TypeRef ──────────────────────────────────────────────

let rec mapType (ctx: MapCtx) (t: Ts.Type) : Schema.TypeRef =
    let checker = ctx.Checker
    let printed = checker.typeToString t

    // A type-parameter REFERENCE (item 11) resolves against TWO axes, declaring first
    // (matching F# scoping: a member's `<U>` shadowing a declaring `<T>` is a distinct
    // typar, but a name in BOTH binds to the declaring slot). Checked BEFORE the
    // printed-name match: a typar prints as its bare name (`T`), which would otherwise
    // be mistaken for a nominal type.
    //   - found in the DECLARING env → `Typar i` (the enclosing type/alias/free-fn axis);
    //   - else found in the METHOD env → `MethodTypar i` (a generic member's own `<U>`);
    //   - else genuinely unbound → erase to `obj` + a Warning.
    // The third case should now be unreachable for an authored member typar (it rides
    // `MethodEnv`); it survives as a defensive degrade for a typar from neither axis.
    if t.isTypeParameter () then
        match lookupTypar ctx.DeclaringEnv t with
        | Some i -> Schema.TypeRef.Typar i
        | None ->
            match lookupTypar ctx.MethodEnv t with
            | Some i -> Schema.TypeRef.MethodTypar i
            | None ->
                // Span anchored on the typar's own declaration node when it has one.
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

        // FAITHFUL structural arms for the constructs the front end ground-EVALUATES
        // (design §"Literal types stay structural" + "keyof … ride on top"): a
        // string/number literal TYPE, `keyof T`, `T[K]`, and a conditional type each
        // map to their OWN schema arm instead of degrading. The extractor NEVER
        // evaluates them (the freeze / backend-knowledge separation) — it records the
        // CONSTRUCT verbatim and the front end owns the fold. Caught here before the
        // printed-name match: their printed forms (`"GET"`, `keyof Events`,
        // `Events[Key]`, `… ? … : …`) are unmatchable there. Two deliberate SILENT
        // degrades stay (no faithful arm, no diagnostic — design decisions, mirrored
        // in `DiagCode`'s doc): a BOOLEAN literal (design §"string first; skip bool")
        // and a non-integer numeric literal (no `int64` wire form) erase to their base.
        // `keyof` rides the runtime `isIndexType()` predicate; indexed-access /
        // conditional have no predicate, so classify by FIELD PRESENCE — see
        // `isIndexedAccessType`/`isConditionalType`.
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

                // Read each branch's AUTHORED type, never the evaluated pick: the
                // `resolvedTrue/FalseType` are populated for a resolved conditional, but
                // an UNINSTANTIATED one (mitt's — `Key` is still open) leaves them
                // `None`, so fall back to the conditional NODE's own branch `TypeNode`s.
                // Either source is the authored branch, not an evaluation of the test.
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
            else
                None

        match special with
        | Some ty -> ty
        | None ->
            match printed with
            | "string" -> Schema.TypeRef.Named("string", [])
            | "number" -> Schema.TypeRef.Named("float", []) // TS number → Vesper float (policy)
            | "boolean" -> Schema.TypeRef.Named("bool", [])
            | "void" -> Schema.TypeRef.Named("unit", [])
            | "null" -> Schema.TypeRef.Named("null", [])
            | "undefined" -> Schema.TypeRef.Named("undefined", [])
            // `any` → Dynamic (item 12) — the DESIGNED mapping, not a degradation (no
            // diagnostic). TS exposes no public `type.isAny()`, so classify by printed
            // form like the other primitives. Only `any` is in scope — `unknown` is
            // deliberately NOT remapped and still flows through the Named fallthrough.
            | "any" -> Schema.TypeRef.Dynamic
            | _ when t.isUnion () ->
                // Anonymous union → TyOr. null/undefined ride in as their own members
                // (resolved fork: NOT folded to unit). Members are deduped and a
                // singleton unwrapped; literal members stay FAITHFUL
                // (`"GET" | "POST"` → `Union [Literal "GET"; Literal "POST"]`), so the
                // dedup only collapses genuine structural duplicates.
                let members =
                    (unbox<Ts.UnionType> t).types
                    |> Seq.map (mapType ctx)
                    |> List.ofSeq
                    |> List.distinct

                match members with
                | [ single ] -> single
                | many -> Schema.TypeRef.Union many
            | _ when t.isIntersection () ->
                // Intersection `A & B` (item 15): erase to `obj`, the universal
                // supertype, for v1 — a REAL fidelity loss, so it warns (unlike the
                // silent-by-design literal widenings). Object-intersections graduate
                // onto item 14's structural content-hash path once it exists.
                emitWarning
                    ctx
                    Schema.DiagCode.IntersectionErased
                    printed
                    (t.getSymbol () |> Option.bind tryDeclOf |> Option.map spanOfNode)
                    (sprintf "intersection type '%s' erased to obj (structural intersections are deferred)" printed)

                Schema.TypeRef.Named("obj", [])
            | _ ->
                // Generic instantiation (`Array<string>`, `Box<number>`): a `TypeReference`
                // → `Named(target name, mapped args)` (item 11), recursing `mapType` over the
                // type arguments rather than emitting the printed-form blob (`Named("string[]")`).
                match asGenericInstantiation checker t with
                | Some(name, args) -> Schema.TypeRef.Named(name, args |> List.map (mapType ctx))
                | None when isFunctionType t && not (looksNominal printed) ->
                    // A pure FUNCTION type (`(event: T) => void`, `Handler<T>`): map it
                    // FAITHFULLY to `TypeRef.Fun(curried param types, return)` — the
                    // provider rehydrates `Fun → FTFun` — rather than degrading to a stub.
                    // `isFunctionType` guarantees exactly one call signature and excludes
                    // callable objects with members, constructor types, and (via the
                    // `looksNominal` gate) named interfaces declaring a call signature.
                    let callSig = (t.getCallSignatures ()).[0]

                    let paramTypes =
                        callSig.getParameters ()
                        |> Seq.map (fun p -> mapType ctx (checker.getTypeOfSymbolAtLocation (p, declOf p)))
                        |> List.ofSeq

                    let ret = mapType ctx (callSig.getReturnType ())
                    Schema.TypeRef.Fun(paramTypes, ret)
                | None ->
                    // Tightened fallthrough (cross-cutting producer discipline): now that
                    // generics are handled, keep only a BARE NOMINAL name as `Named`; THROW on
                    // any other genuinely-unknown printed form (exotic primitives) rather than
                    // silently degrading to `Named(printed)`.
                    if looksNominal printed then
                        Schema.TypeRef.Named(printed, [])
                    else
                        // A STRUCTURAL/anonymous form (`{ x: number }`, item 14, DEFERRED) that
                        // surfaced as non-nominal — `keyof` / indexed-access / conditional have
                        // faithful arms above and never reach here. DEGRADE to the `Structural`
                        // stub the provider rehydrates (→ `FTUnknown`), keyed on the printed form
                        // as a stable content hash.
                        //
                        // Harvest fields ONLY for a genuine anonymous OBJECT type (`TypeFlags.Object`):
                        // there, `getProperties` is the type's OWN declared members. For any other
                        // non-object structural form the "properties" would be the APPARENT (inherited
                        // prototype) members of a primitive/union base — harvesting them recurses into
                        // `string | symbol`'s `toString`/`valueOf`/… and explodes into a noise cascade
                        // — so emit an EMPTY field set for those. (Flag CONSTANT, not a raw numeric,
                        // per producer discipline — vendored flag values can drift.)
                        let isObjectType = int (t.flags) &&& int Ts.TypeFlags.Object <> 0

                        let fields =
                            if isObjectType then
                                t.getProperties ()
                                |> Seq.map (fun p ->
                                    p.getName (), mapType ctx (checker.getTypeOfSymbolAtLocation (p, declOf p))
                                )
                                |> List.ofSeq
                            else
                                []

                        emitWarning
                            ctx
                            Schema.DiagCode.StructuralObjectStubbed
                            printed
                            (t.getSymbol () |> Option.bind tryDeclOf |> Option.map spanOfNode)
                            (sprintf
                                "anonymous/structural type '%s' stubbed as a content-hashed Structural (item 14 deferred); the provider rehydrates it as an opaque type"
                                printed)

                        Schema.TypeRef.Structural(printed, fields)

let mapParam (ctx: MapCtx) (p: Ts.Symbol) : Schema.Param =
    // A parameter symbol's declaration is the `ParameterDeclaration` node carrying
    // the syntactic optional/rest markers. Classify by TOKEN PRESENCE on the node
    // (runtime-structural, per the producer discipline), never raw numeric flags:
    //   optional ⇐ `x?: T` (questionToken) OR `x: T = default` (initializer);
    //   rest     ⇐ `...x: T[]` (dotDotDotToken).
    let decl = declOf p
    let paramDecl = unbox<Ts.ParameterDeclaration> decl

    {
        Name = p.getName ()
        Type = mapType ctx (ctx.Checker.getTypeOfSymbolAtLocation (p, decl))
        Optional = paramDecl.questionToken.IsSome || paramDecl.initializer.IsSome
        Rest = paramDecl.dotDotDotToken.IsSome
    }

/// WHICH kind of signature is being mapped — the single axis decision, made once
/// here instead of by hand at each call site (the caller-side `sigTypars`
/// placement + `emitBounds` flag + ctor record-patch this replaces).
[<RequireQualifiedAccess>]
type SigAxis =
    /// A member METHOD: its own typars are the METHOD axis, and their authored
    /// constraints (`<Key extends keyof Events>`) are harvested onto
    /// `TypeParamBounds` — carried, never evaluated (design §"keyof …
    /// ground-EVALUATED"); the front end reads them at grounding.
    | MemberMethod
    /// A FREE FUNCTION: no declaring type, so its own typars occupy the single
    /// DECLARING index space unambiguously and the provider's `scheme` freshens
    /// them; bounds are not needed downstream.
    | FreeFunction
    /// A CONSTRUCTOR: carries NO method axis at all — TS models a generic class's
    /// construct signatures as generic over the CLASS typars, but per the seam
    /// those are the declaring axis (already counted in the type's `typeParams`),
    /// so `TypeParams`/`TypeParamBounds` are forced empty ("MethodArity = 0 for
    /// every constructor").
    | Ctor

let mapSignature (ctx: MapCtx) (axis: SigAxis) (sg: Ts.Signature) : Schema.Signature =
    let ownTypars =
        sg.getTypeParameters () |> Option.map List.ofSeq |> Option.defaultValue []

    let ownTyparSyms () =
        typarSymbols (ownTypars |> Seq.map unbox)

    /// The authored constraint of one own-typar, read off the DECLARATION node
    /// (its constraint `TypeNode`), NOT `getConstraint()` on the type — the latter
    /// RESOLVES `keyof Events` to the constraint's evaluated key union (`string |
    /// number | symbol`), which VIOLATES the no-evaluation trap AND loses the
    /// `Events` the front end must fold against. `getTypeFromTypeNode` on the
    /// authored node keeps the SYMBOLIC `keyof Events` (Events stays a typar).
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
        | SigAxis.Ctor -> { ctx with MethodEnv = [] }, 0, []

    {
        TypeParams = typeParams
        TypeParamBounds = bounds
        Params = sg.getParameters () |> Seq.map (mapParam bodyCtx) |> List.ofSeq
        Returns = mapType bodyCtx (sg.getReturnType ())
    }
