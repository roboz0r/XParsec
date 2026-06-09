namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// Per-generic-closure registry entry (function-representation-plan §Generic closures, C2): the typar
/// *count* inherited from the enclosing static method, the capture-field types in declaration order,
/// the `Invoke` parameter / result types, and the closure's predicted `TypeDefinition` handle. All
/// `FrozenType` fields embed the enclosing method's `FTTypar(Method, i)`; `ClrEnv.ClosureTyparMode`
/// re-projects them onto the closure class's `!i` during the closure's own emission.
type internal GenericClosureShape =
    {
        TyparCount: int
        CaptureSigs: FrozenType list
        ParamTy: FrozenType
        ResultTy: FrozenType
        DefHandle: EntityHandle
    }

/// Shared `ClrProvider` substrate: the `MetadataContext`, the resolution inputs
/// (`reprs` / `references` / `symbols`), every lazily-minted assembly/type/member reference, the
/// per-emission registries (user types, generic shapes, FSharp.Core deps), the ambient generic-typar
/// state, and the env-only helpers (external *type* refs, typar leaves). The encoders and
/// recipe builders are layered on top of this in their own files.
///
/// Reference identities are resolved by *simple name*: `references` (read off a file, R4) wins;
/// `FSharp.Core` / `Vesper.Printf` fall back to the host-loaded copy; `Vesper.Core` / `Vesper.List`
/// are required (forcing one without its reference is a hard error). Every ref is `lazy` (G6) so the
/// metadata row is added only when first forced during emission — constructing the provider emits
/// nothing, and a PE whose IL never touches an assembly carries no `AssemblyRef` for it.
type internal ClrEnv
    (
        ctx: MetadataContext,
        reprs: Map<string, string>,
        references: Map<string, AssemblyName>,
        symbols: ICodegenSymbols,
        assemblyName: string
    ) =

    /// The home assembly of the unit being emitted (`None` only on the no-emit
    /// scaffold path). A nominal `SymbolKey` is project-local iff its `keyAsm`
    /// equals this — the codegen local/external branch (asm-discrimination).
    let envAsm = SymbolKeyOps.asmOf assemblyName

    let refOrHost (simpleName: string) (hostFallback: unit -> AssemblyName) : AssemblyName =
        match references.TryFind simpleName with
        | Some an -> an
        | None -> hostFallback ()

    let refRequired (simpleName: string) (need: string) : AssemblyName =
        match references.TryFind simpleName with
        | Some an -> an
        | None ->
            failwithf
                "ClrProvider: %s, but no %s assembly is referenced (add its path to ProjectInfo.References)."
                need
                simpleName

    let fsCoreRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(
                    refOrHost "FSharp.Core" (fun () -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.GetName())
                )
            ))

    let coreRef =
        lazy (toEntity (ctx.AssemblyRef(typeof<System.Object>.Assembly.GetName())))

    let vesperRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(refOrHost "Vesper.Printf" (fun () -> typeof<Vesper.PrintfRuntime>.Assembly.GetName()))
            ))

    let consoleRef =
        lazy (toEntity (ctx.AssemblyRef(typeof<System.Console>.Assembly.GetName())))

    let eUnit =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "Unit")))

    // The BCL-only `unit`: the zero-field `System.ValueTuple` struct (the repr
    // `prim-types-min.fs` binds `unit` to). The general `unit` type/value encodes
    // off this; `eUnit` (`FSharp.Core.Unit`) survives only on the cold-printf
    // interop island, where the `PrintfFormat` signatures name it explicitly.
    let eValueTuple =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ValueTuple")))

    // The arity-≥2 tuple family: the open generic structs `System.ValueTuple`2..`7`,
    // cached by arity (tuple-representation-plan Step 1). The nullary `unit` case
    // stays on `eValueTuple` above; an N-tuple value is simply the arity-N member
    // of the same struct family — one coherent representation, no special-casing.
    // Only the bare generic `TypeRef` lives here; the per-call-site `.ctor` /
    // `Item` field refs (which need the element-type instantiation) are minted on
    // top of this by `ClrEncoder.ValueTupleRefs`. Arity ≥ 8 (`TRest` nesting) is
    // deferred (tuple-representation-plan Risks), so the resolver rejects it.
    let valueTupleEntities = Dictionary<int, EntityHandle>()

    let eValueTupleN (arity: int) : EntityHandle =
        if arity < 2 || arity > 7 then
            failwithf
                "ClrProvider: ValueTuple arity %d is out of range — only 2–7 are emitted (≥8 `TRest` nesting is deferred, tuple-representation-plan Risks)."
                arity

        match valueTupleEntities.TryGetValue arity with
        | true, h -> h
        | _ ->
            let h =
                toEntity (ctx.TypeRef(coreRef.Value, "System", sprintf "ValueTuple`%d" arity))

            valueTupleEntities.[arity] <- h
            h

    let ePrintfFormat4 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "PrintfFormat`4")))

    let ePrintfModule =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "PrintfModule")))

    let eFSharpFunc2 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "FSharpFunc`2")))

    // Forcing `eFun2` without a `Vesper.Core` reference is a hard error — `Fun` lives in Vesper.Core,
    // not this assembly and not FSharp.Core.
    let vesperCoreRef =
        lazy (toEntity (ctx.AssemblyRef(refRequired "Vesper.Core" "a function value needs Vesper.Fun")))

    let eFun2 = lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "Fun`2")))

    // Deliberately NO fallback to `vesperCoreRef`: that would re-merge the list into Core's ref
    // surface and mint a wrong `Vesper.Core::List`1` while every test still passed.
    let vesperListRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(refRequired "Vesper.List" "a list literal / List.fold needs Vesper.Collections.List")
            ))

    let eFSharpList1 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Collections", "FSharpList`1")))

    let eVesperList1 =
        lazy (toEntity (ctx.TypeRef(vesperListRef.Value, "Vesper.Collections", "List`1")))

    let eListModule =
        lazy (toEntity (ctx.TypeRef(vesperListRef.Value, "Vesper.Collections", "ListModule")))

    let eObject = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Object")))

    // `System.ValueType` — the IL base type of every `[<Struct>]` value type
    let eValueType = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ValueType")))

    let eTextWriter =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.IO", "TextWriter")))

    let eConsole = lazy (toEntity (ctx.TypeRef(consoleRef.Value, "System", "Console")))

    let eFormatter =
        lazy (toEntity (ctx.TypeRef(vesperRef.Value, "Vesper", "Formatter")))

    let eDecimal = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Decimal")))

    let eException = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Exception")))

    let eEqualityComparer1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Collections.Generic", "EqualityComparer`1")))

    let eHashCode = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "HashCode")))

    let eEquatable1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IEquatable`1")))

    let eComparer1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Collections.Generic", "Comparer`1")))

    let eComparable1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IComparable`1")))

    let eComparable =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IComparable")))

    let eArgumentException =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ArgumentException")))

    let eObjectCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eObject.Value, ".ctor", s)))

    let eExceptionCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     1,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
                 )

             toEntity (ctx.MemberRef(eException.Value, ".ctor", s)))

    let eArgumentExceptionCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     1,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
                 )

             toEntity (ctx.MemberRef(eArgumentException.Value, ".ctor", s)))

    let eDecimalCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     5,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) ->
                         pars.AddParameter().Type().Int32()
                         pars.AddParameter().Type().Int32()
                         pars.AddParameter().Type().Int32()
                         pars.AddParameter().Type().Boolean()
                         pars.AddParameter().Type().Byte()
                     )
                 )

             toEntity (ctx.MemberRef(eDecimal.Value, ".ctor", s)))

    let eHashCodeToHashCode =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     0,
                     (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                     (fun (_: ParametersEncoder) -> ())
                 )

             toEntity (ctx.MemberRef(eHashCode.Value, "ToHashCode", s)))

    let formatterTypeName = "Vesper.Formatter"

    /// Each distinct FSharp.Core construct the emission references — lets a build tell *positively*
    /// whether the PE depends on `FSharp.Core.dll` and *what* pins it. Every FSharp.Core ref is minted
    /// through this provider, so marking each use-site captures the whole dependency surface.
    let fsharpCoreDeps = HashSet<string>()
    let markFSharpCoreDep (construct: string) : unit = fsharpCoreDeps.Add construct |> ignore

    /// User types emitted into *this* assembly, by their nominal `SymbolKey` →
    /// predicted `TypeDefinition` handle, so a field / factory / local signature
    /// can reference the type before its row is added (was string-keyed by
    /// simple/arity name).
    let userTypes = Dictionary<SymbolKey, EntityHandle>()

    /// Project-local `[<Struct>]` value-type keys.
    /// `encodeType` reads this to emit a user struct as `ELEMENT_TYPE_VALUETYPE`
    /// rather than `ELEMENT_TYPE_CLASS` in every signature.
    let userValueTypes = System.Collections.Generic.HashSet<SymbolKey>()

    /// Generic user unions by `SymbolKey` → (typar names, cases); a case is
    /// `(caseName, [(fieldMetaName, declTy)])` with `declTy` carrying declaring-typar markers
    /// (`TyConst "'T"`). Holds the shape needed to mint `MemberRef`s on the type's `TypeSpec`.
    /// Monomorphic unions are not registered (their `Def` tokens suffice).
    let genericUnions =
        Dictionary<SymbolKey, string list * (string * (string * FrozenType) list) list>()

    let genericRecords =
        Dictionary<SymbolKey, string list * (string * FrozenType) list>()

    let genericClasses =
        Dictionary<SymbolKey, string list * (string * FrozenType) list>()
    // Closures have no `SymbolKey` (synthetic names), so they stay string-keyed —
    // the "closures wrinkle" (Phase 6D); the nominal seam is key-based, closures
    // ride their own provider methods.
    let genericClosures = Dictionary<string, GenericClosureShape>()

    let arityOfMetaName (name: string) : int =
        match name.LastIndexOf '`' with
        | i when i >= 0 ->
            match System.Int32.TryParse(name.Substring(i + 1)) with
            | true, n -> n
            | _ -> 0
        | _ -> 0

    let externalAsmRef (asm: string option) : EntityHandle =
        match asm with
        | None ->
            failwith
                "ClrProvider: an external symbol carries no home assembly (project-local symbols are resolved before the provider)."
        | Some simpleName ->
            let an =
                match references.TryFind simpleName with
                | Some an -> an
                | None ->
                    match
                        System.AppDomain.CurrentDomain.GetAssemblies()
                        |> Array.tryFind (fun a -> a.GetName().Name = simpleName)
                    with
                    | Some a -> a.GetName()
                    | None -> AssemblyName(simpleName)

            toEntity (ctx.AssemblyRef an)

    // The single key→string funnel for *type-shape* lookups. A provider may key a
    // generic type bare (`Vesper.Option`, contract layer) or arity-suffixed
    // (`Vesper.Option`1`, metadata layer). A `SymbolKey`'s `qualifiedName` is the
    // already-well-formed compiled name (arity suffix retained, nested `+` segments
    // intact), so this probes that form and then its bare fallback — the *one* place
    // the two registration conventions are reconciled, replacing the per-lookup
    // `bareName` dual probe that used to leak into every shape consumer.
    // `bareName` strips at the first
    // backtick, so the qual-first order is what keeps a nested `List`1+Enumerator`
    // resolvable (its bare form would mangle to `List`).
    let lookupTypeByKey (key: SymbolKey) : ExternalTypeShape voption =
        let qual = SymbolKeyOps.qualifiedName key

        match symbols.TryLookupType qual with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            let bare = SymbolKeyOps.bareName qual

            if bare = qual then
                ValueNone
            else
                symbols.TryLookupType bare

    let lookupClassShape (key: SymbolKey) : ExternalClassShape voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Class info) -> ValueSome info
        | _ -> ValueNone

    let externalClassRef (key: SymbolKey) : EntityHandle voption =
        match lookupClassShape key with
        | ValueSome info ->
            let ns = info.Origin.Namespace
            let simple = SymbolOrigin.StripNamespace ns (SymbolKeyOps.qualifiedName key)
            let asm = externalAsmRef info.Origin.Assembly

            // A nested type's `TypeRef` (`List`1+Enumerator`, the duck-typed struct
            // enumerator) must chain through the enclosing type's `TypeRef` as its
            // ResolutionScope with the *bare* nested name + empty namespace — a flat
            // `Outer+Inner` name with the AssemblyRef scope fails to bind
            // (`TypeLoadException`). The `+` is a reflection display convention, not a
            // metadata name. Top-level (`+`-free) names take the single-segment path
            // unchanged. The generic args ride the innermost nested TypeRef, so the
            // encoder needs no further nesting awareness.
            match simple.Split('+') with
            | [| flat |] -> ValueSome(toEntity (ctx.TypeRef(asm, ns, flat)))
            | parts ->
                let mutable scope = toEntity (ctx.TypeRef(asm, ns, parts.[0]))

                for i in 1 .. parts.Length - 1 do
                    scope <- toEntity (ctx.TypeRef(scope, "", parts.[i]))

                ValueSome scope
        | _ -> ValueNone

    /// Whether a referenced-assembly type is a .NET value type (`struct`) — `false`
    /// for every reference type and for any name the provider can't resolve as a
    /// class. Drives the `VALUETYPE` vs `CLASS` element tag in `encodeType` and the
    /// value-receiver dispatch for the duck-typed struct enumerator
    /// (`List`1+Enumerator`, vesper-set-sprint-phase-4 §4.4).
    let externalIsValueType (key: SymbolKey) : bool =
        match lookupClassShape key with
        | ValueSome info -> info.Flags.IsValueType
        | _ -> false

    /// Referenced-assembly record shape by `SymbolKey` + arity. The bare-vs-arity-
    /// suffixed registration split (contract layer keys `Vesper.Ref`, metadata layer
    /// `Vesper.Ref`1`) is reconciled once inside `lookupTypeByKey`.
    let externalRecordShape (key: SymbolKey) (arity: int) : (ExternalFieldShape[] * SymbolOrigin) voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Record(a, fields, origin)) when a = arity && origin.Assembly.IsSome ->
            ValueSome(fields, origin)
        | _ -> ValueNone

    let externalRecordRef (key: SymbolKey) (arity: int) : (EntityHandle * ExternalFieldShape[]) voption =
        match externalRecordShape key arity with
        | ValueNone -> ValueNone
        | ValueSome(fields, origin) ->
            let ns = origin.Namespace
            let bareSimple = SymbolOrigin.StripNamespace ns (SymbolKeyOps.qualifiedName key)

            // Metadata `TypeRef` simple names carry the `` `n `` arity suffix; the contract-layer key
            // (`Vesper.Ref`) lacks it, the metadata-layer key (`Vesper.Ref`1`) has it. Add when absent.
            let simple = SymbolKeyOps.arityName bareSimple arity

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Assembly, ns, simple)), fields)

    /// Referenced-assembly union shape by `SymbolKey` + arity — the mirror of
    /// `externalRecordShape` for cross-package case construction (`Some` / `None`,
    /// vesper-lib-test-plan Gap 2 Layer B). The bare-vs-arity-suffixed registration
    /// split is reconciled once inside `lookupTypeByKey`.
    let externalUnionShape (key: SymbolKey) (arity: int) : (ExternalCaseShape[] * SymbolOrigin) voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Union(a, cases, origin)) when a = arity && origin.Assembly.IsSome ->
            ValueSome(cases, origin)
        | _ -> ValueNone

    let externalUnionRef (key: SymbolKey) (arity: int) : (EntityHandle * ExternalCaseShape[]) voption =
        match externalUnionShape key arity with
        | ValueNone -> ValueNone
        | ValueSome(cases, origin) ->
            let ns = origin.Namespace
            let bareSimple = SymbolOrigin.StripNamespace ns (SymbolKeyOps.qualifiedName key)

            let simple = SymbolKeyOps.arityName bareSimple arity

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Assembly, ns, simple)), cases)

    // While encoding a closure's own members (Invoke / .ctor /
    // capture fields / its TypeSpec from inside its body), the enclosing method's
    // `TyTypar(Method, i)` are the closure *class*'s generic parameters, so they
    // encode as `GenericTypeParameter i` rather than `GenericMethodTypeParameter i`.
    let mutable closureTyparMode = false

    let rec decurryTy (t: FrozenType) : FrozenType list * FrozenType =
        match t with
        | FTFun(a, b) ->
            let ps, r = decurryTy b
            a :: ps, r
        | other -> [], other

    member _.Ctx = ctx
    member _.Reprs = reprs
    member _.References = references
    member _.Symbols: ICodegenSymbols = symbols

    member _.FsCoreRef = fsCoreRef
    member _.CoreRef = coreRef
    member _.VesperRef = vesperRef
    member _.ConsoleRef = consoleRef
    member _.EUnit = eUnit
    member _.EValueTuple = eValueTuple
    member _.EValueTupleN arity = eValueTupleN arity
    member _.EPrintfFormat4 = ePrintfFormat4
    member _.EPrintfModule = ePrintfModule
    member _.EFSharpFunc2 = eFSharpFunc2
    member _.VesperCoreRef = vesperCoreRef
    member _.EFun2 = eFun2
    member _.VesperListRef = vesperListRef
    member _.EFSharpList1 = eFSharpList1
    member _.EVesperList1 = eVesperList1
    member _.EListModule = eListModule
    member _.EObject = eObject
    member _.EValueType = eValueType
    member _.ETextWriter = eTextWriter
    member _.EConsole = eConsole
    member _.EFormatter = eFormatter
    member _.EDecimal = eDecimal
    member _.EException = eException
    member _.EEqualityComparer1 = eEqualityComparer1
    member _.EHashCode = eHashCode
    member _.EEquatable1 = eEquatable1
    member _.EComparer1 = eComparer1
    member _.EComparable1 = eComparable1
    member _.EComparable = eComparable
    member _.EArgumentException = eArgumentException
    member _.EObjectCtor = eObjectCtor
    member _.EExceptionCtor = eExceptionCtor
    member _.EArgumentExceptionCtor = eArgumentExceptionCtor
    member _.EDecimalCtor = eDecimalCtor
    member _.EHashCodeToHashCode = eHashCodeToHashCode

    member _.FormatterTypeName = formatterTypeName

    member _.EnvAsm = envAsm

    member _.UserTypes = userTypes
    member _.UserValueTypes = userValueTypes
    member _.GenericUnions = genericUnions
    member _.GenericRecords = genericRecords
    member _.GenericClasses = genericClasses
    member _.GenericClosures = genericClosures

    member _.MarkFSharpCoreDep construct = markFSharpCoreDep construct

    member _.FSharpCoreDependencies() =
        fsharpCoreDeps |> List.ofSeq |> List.sort

    member _.ClosureTyparMode
        with get () = closureTyparMode
        and set v = closureTyparMode <- v

    member _.ArityOfMetaName name = arityOfMetaName name
    member _.DecurryTy t = decurryTy t

    member _.ExternalAsmRef asm = externalAsmRef asm
    member _.ExternalClassRef key = externalClassRef key
    member _.ExternalIsValueType key = externalIsValueType key
    member _.ExternalRecordShape(key, arity) = externalRecordShape key arity
    member _.ExternalRecordRef(key, arity) = externalRecordRef key arity
    member _.ExternalUnionShape(key, arity) = externalUnionShape key arity
    member _.ExternalUnionRef(key, arity) = externalUnionRef key arity
