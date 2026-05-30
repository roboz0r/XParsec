namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// Per-generic-closure registry entry (function-representation-plan §Generic closures, C2): the typar
/// union-find roots inherited from the enclosing static method, the capture-field types in
/// declaration order, the `Invoke` parameter / result types, and the closure's predicted
/// `TypeDefinition` handle. All `SemType` fields embed the typar roots verbatim;
/// `closureTyparLeaf` maps them to `!i` during the closure's own emission.
type internal GenericClosureShape =
    {
        TyparRoots: TypeVar list
        CaptureSigs: SemType list
        ParamTy: SemType
        ResultTy: SemType
        DefHandle: EntityHandle
    }

/// Shared `ClrProvider` substrate: the `MetadataContext`, the resolution inputs
/// (`reprs` / `references` / `symbols`), every lazily-minted assembly/type/member reference, the
/// per-emission registries (user types, generic shapes, FSharp.Core deps), the ambient generic-typar
/// state, and the env-only helpers (`zonk`, external *type* refs, typar leaves). The encoders and
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
        symbols: IExternalSymbolProvider
    ) =

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

    let listTypeName = "Microsoft.FSharp.Collections.list"
    let vesperListName = "Vesper.Collections.List"

    /// The cons-list's *abbreviation* name (lowercase). `Vesper.List` types `List.fold`'s
    /// `'T list` parameter with this, whereas the self-host `'T list = List<'T>` path expands to the
    /// union name — both denote the one cons-list, so recognition accepts either.
    let vesperListAbbrevName = "Vesper.Collections.list"

    let isVesperListName (name: string) =
        name = vesperListName || name = vesperListAbbrevName

    let eObject = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Object")))

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

    /// User types emitted into *this* assembly, by simple name → predicted `TypeDefinition` handle, so
    /// a field / factory / local signature can reference the type before its row is added.
    let userTypes = Dictionary<string, EntityHandle>()

    /// Generic user unions by name → (typar names, cases); a case is
    /// `(caseName, [(fieldMetaName, declTy)])` with `declTy` carrying declaring-typar markers
    /// (`TyConst "'T"`). Holds the shape needed to mint `MemberRef`s on the type's `TypeSpec`.
    /// Monomorphic unions are not registered (their `Def` tokens suffice).
    let genericUnions =
        Dictionary<string, string list * (string * (string * SemType) list) list>()

    let genericRecords = Dictionary<string, string list * (string * SemType) list>()
    let genericClasses = Dictionary<string, string list * (string * SemType) list>()
    let genericClosures = Dictionary<string, GenericClosureShape>()

    /// Resolve a `SemType` to its concrete representative, chasing union-find links. After
    /// `ResolvedTypes` no *free* TyVar survives, so the only job is dereferencing linked ones.
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> zonk target
            | ValueNone -> t
        | TyFun(a, b) -> TyFun(zonk a, zonk b)
        | TyTuple items -> TyTuple(EqArray.map zonk items)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map zonk args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map zonk args)
        | TyClass(n, args) -> TyClass(n, EqArray.map zonk args)
        | TyConst _ -> t

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

    let externalClassRef (fullName: string) : EntityHandle voption =
        match symbols.TryLookupType fullName with
        | ValueSome(ExternalTypeShape.Class info) ->
            let ns = info.Origin.Namespace
            let simple = SymbolOrigin.StripNamespace ns fullName
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
    let externalIsValueType (fullName: string) : bool =
        match symbols.TryLookupType fullName with
        | ValueSome(ExternalTypeShape.Class info) -> info.Flags.IsValueType
        | _ -> false

    /// Referenced-assembly record shape by name + arity. The contract layer keys generic records by
    /// the bare compiled name (`Vesper.Ref`), the metadata layer by the arity-suffixed key
    /// (`Vesper.Ref`1`); both forms are probed.
    let externalRecordShape (fullName: string) (arity: int) : (ExternalFieldShape[] * SymbolOrigin) voption =
        let probe (key: string) =
            match symbols.TryLookupType key with
            | ValueSome(ExternalTypeShape.Record(a, fields, origin)) when a = arity && origin.Assembly.IsSome ->
                ValueSome(fields, origin)
            | _ -> ValueNone

        let suffixed =
            if arity > 0 then
                sprintf "%s`%d" fullName arity
            else
                fullName

        match probe fullName with
        | ValueSome v -> ValueSome v
        | ValueNone -> probe suffixed

    let externalRecordRef (fullName: string) (arity: int) : (EntityHandle * ExternalFieldShape[]) voption =
        match externalRecordShape fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(fields, origin) ->
            let ns = origin.Namespace
            let bareSimple = SymbolOrigin.StripNamespace ns fullName

            // Metadata `TypeRef` simple names carry the `` `n `` arity suffix; the contract-layer key
            // (`Vesper.Ref`) lacks it, the metadata-layer key (`Vesper.Ref`1`) has it. Add when absent.
            let simple =
                if arity > 0 && not (bareSimple.Contains '`') then
                    sprintf "%s`%d" bareSimple arity
                else
                    bareSimple

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Assembly, ns, simple)), fields)

    // ---- Ambient generic-typar context ----
    //
    // Three disjoint sets drive how a free/marker typar leaf is encoded:
    //   methodTyparRoots  → generic *method* typar `!!i` (by union-find root; R3)
    //   typeTyparIx       → generic *type* typar `!0` (by TyConst marker name; S4)
    //   closureTyparRoots → generic *closure* typar `!i` (by union-find root; C2)
    // Invariant: at most one is installed (non-empty) during any single signature encoding — the
    // leaves are disjoint by construction, so the OR-chain in `ambientTyparLeaf` is safe.
    let mutable methodTyparRoots: TypeVar list = []
    let mutable typeTyparIx: Map<string, int> = Map.empty
    let mutable closureTyparRoots: TypeVar list = []

    let methodTyparLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
        match methodTyparRoots with
        | [] -> false
        | roots ->
            match zt with
            | TyVar tv ->
                let root = UnionFind.find tv

                match roots |> List.tryFindIndex (fun r -> System.Object.ReferenceEquals(r, root)) with
                | Some i ->
                    te.GenericMethodTypeParameter i
                    true
                | None -> false
            | _ -> false

    let typeTyparLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
        if Map.isEmpty typeTyparIx then
            false
        else
            match zt with
            | TyConst name ->
                match Map.tryFind name typeTyparIx with
                | Some i ->
                    te.GenericTypeParameter i
                    true
                | None -> false
            | _ -> false

    let closureTyparLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
        match closureTyparRoots with
        | [] -> false
        | roots ->
            match zt with
            | TyVar tv ->
                let root = UnionFind.find tv

                match roots |> List.tryFindIndex (fun r -> System.Object.ReferenceEquals(r, root)) with
                | Some i ->
                    te.GenericTypeParameter i
                    true
                | None -> false
            | _ -> false

    let ambientTyparLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
        methodTyparLeaf te zt || typeTyparLeaf te zt || closureTyparLeaf te zt

    let arityOfMetaName (name: string) : int =
        match name.LastIndexOf '`' with
        | i when i >= 0 ->
            match System.Int32.TryParse(name.Substring(i + 1)) with
            | true, n -> n
            | _ -> 0
        | _ -> 0

    let rec decurryTy (t: SemType) : SemType list * SemType =
        match zonk t with
        | TyFun(a, b) ->
            let ps, r = decurryTy b
            a :: ps, r
        | other -> [], other

    let typarIx (typars: string list) : Map<string, int> =
        typars |> List.mapi (fun i n -> n, i) |> Map.ofList

    member _.Ctx = ctx
    member _.Reprs = reprs
    member _.References = references
    member _.Symbols = symbols

    member _.FsCoreRef = fsCoreRef
    member _.CoreRef = coreRef
    member _.VesperRef = vesperRef
    member _.ConsoleRef = consoleRef
    member _.EUnit = eUnit
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

    member _.ListTypeName = listTypeName
    member _.VesperListName = vesperListName
    member _.VesperListAbbrevName = vesperListAbbrevName
    member _.FormatterTypeName = formatterTypeName
    member _.IsVesperListName name = isVesperListName name

    member _.UserTypes = userTypes
    member _.GenericUnions = genericUnions
    member _.GenericRecords = genericRecords
    member _.GenericClasses = genericClasses
    member _.GenericClosures = genericClosures

    member _.MarkFSharpCoreDep construct = markFSharpCoreDep construct

    member _.FSharpCoreDependencies() =
        fsharpCoreDeps |> List.ofSeq |> List.sort

    member _.MethodTyparRoots
        with get () = methodTyparRoots
        and set v = methodTyparRoots <- v

    member _.TypeTyparIx
        with get () = typeTyparIx
        and set v = typeTyparIx <- v

    member _.ClosureTyparRoots
        with get () = closureTyparRoots
        and set v = closureTyparRoots <- v

    member _.MethodTyparLeaf = methodTyparLeaf
    member _.TypeTyparLeaf = typeTyparLeaf
    member _.ClosureTyparLeaf = closureTyparLeaf
    member _.AmbientTyparLeaf = ambientTyparLeaf

    member _.Zonk t = zonk t
    member _.ArityOfMetaName name = arityOfMetaName name
    member _.DecurryTy t = decurryTy t
    member _.TyparIx typars = typarIx typars

    member _.ExternalAsmRef asm = externalAsmRef asm
    member _.ExternalClassRef fullName = externalClassRef fullName
    member _.ExternalIsValueType fullName = externalIsValueType fullName
    member _.ExternalRecordShape(fullName, arity) = externalRecordShape fullName arity
    member _.ExternalRecordRef(fullName, arity) = externalRecordRef fullName arity
