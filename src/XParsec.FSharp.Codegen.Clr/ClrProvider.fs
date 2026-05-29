namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The .NET implementation of `ICodegenProvider`. v1 references a real
// `FSharp.Core.dll` (+ the core BCL) at runtime; assembly identities are read
// from the assemblies *loaded in the codegen host*, so the emitted references
// match exactly what an in-process loader will bind against.
//
// Slice 1 covers `printfn "hi"`: the `PrintfFormat\`4` constructor and the
// generic `PrintfModule.PrintFormatLine` call. Coverage grows with the slices.

/// Per-generic-closure registry entry (function-representation-plan §Generic closures, C2): the typar union-find
/// roots inherited from the enclosing static method (`Closure.Typars`, C1), the
/// capture-field types in declaration order, the `Invoke` parameter / result
/// types, and the closure's predicted `TypeDefinition` handle (the parent for
/// `MemberRef`s minted off this closure's instantiated `TypeSpec`). All
/// `SemType` fields embed the typar roots verbatim; `closureTyparLeaf` maps
/// them to the closure type's `GenericTypeParameter` (`!i`) during the closure's
/// own emission.
type internal GenericClosureShape =
    {
        TyparRoots: TypeVar list
        CaptureSigs: SemType list
        ParamTy: SemType
        ResultTy: SemType
        DefHandle: EntityHandle
    }

/// `ICodegenProvider` over the BCL + the referenced assemblies. `reprs` is the
/// Vesper-primitive-name → IL-representation map (`IntrinsicRepr.merge` of a
/// file's intrinsic bindings over the built-in defaults); `encodeType` keys the
/// emitted IL type off the representation string (G7). `references` maps an
/// assembly's *simple name* to the identity read off its file (`ProjectInfo.References`),
/// so an emitted `AssemblyRef` matches that exact artifact, not whatever the host
/// loaded (R4). The provider looks up a type's owning assembly by name:
/// `Vesper.Core` for `Vesper.Fun\`2` (R1 / D3) and `Vesper.List` for the cons-list
/// (PS2) are *required* (a `TyFun` / list with no such reference fails to encode —
/// correct when compiling that package itself, or a program that forms neither);
/// `FSharp.Core` (the R9 cold-printf island) and `Vesper.Printf` (the happy-path
/// formatter) fall back to the host-loaded copy when not referenced explicitly.
///
/// `symbols` is the front end's resolution provider (symbol-resolution-plan §3,
/// P4): codegen reads it to mint refs for a `TExpr.ExternalMember` from the node's
/// interned `SymbolKey` (`ExternalMemberRef`) — the member's open signature comes
/// from `symbols.TryLookupMember`, which the key already pinned (no re-running of
/// name resolution). Pass `ExternalSymbols.nullProvider` on paths that emit no
/// external member access (the hand-written-body seam).
type ClrProvider
    (
        ctx: MetadataContext,
        reprs: Map<string, string>,
        references: Map<string, AssemblyName>,
        symbols: IExternalSymbolProvider
    ) =

    // Resolve an assembly's identity by simple name: a `ProjectInfo.References`
    // entry (read off the file — R4) wins; otherwise a host-loaded fallback for the
    // assemblies the provider allows to default (FSharp.Core / Vesper.Printf).
    let refOrHost (simpleName: string) (hostFallback: unit -> AssemblyName) : AssemblyName =
        match references.TryFind simpleName with
        | Some an -> an
        | None -> hostFallback ()

    // Resolve a *required* reference's identity by simple name — a `Vesper.*`
    // library with no host fallback. Forced lazily, so the failure surfaces only
    // when the missing type is actually needed.
    let refRequired (simpleName: string) (need: string) : AssemblyName =
        match references.TryFind simpleName with
        | Some an -> an
        | None ->
            failwithf
                "ClrProvider: %s, but no %s assembly is referenced (add its path to ProjectInfo.References)."
                need
                simpleName

    // Reference identities, by simple name (version-proof). Every `AssemblyRef` /
    // `TypeRef` / `MemberRef` below is `lazy` (G6): the row is added — through
    // `ctx`, which caches it — only when a ref is first *forced* (`.Value`) during
    // emission, not at construction. So merely constructing the provider emits no
    // metadata. An executable whose IL never touches FSharp.Core carries no
    // FSharp.Core `AssemblyRef`, and the library path can construct a provider for a
    // typar-only interface without pinning any assembly at all (which is what lets
    // `assembleLibrary` reuse `encodeType` — G5 — instead of its old provider-free
    // encoder).
    let fsCoreRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(
                    refOrHost "FSharp.Core" (fun () -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.GetName())
                )
            ))

    let coreRef =
        lazy (toEntity (ctx.AssemblyRef(typeof<System.Object>.Assembly.GetName())))

    // The bootstrap printf runtime (Vesper.Printf.dll) hosting `Formatter`; and
    // the assembly that owns `System.Console` (its own ref assembly, not
    // CoreLib — type-forwarded at runtime). `Vesper.Printf` takes the same
    // reference-or-host resolution as FSharp.Core; `System.Console` is a BCL ref
    // that resolves from the shared framework, so it always reads live.
    let vesperRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(refOrHost "Vesper.Printf" (fun () -> typeof<Vesper.PrintfRuntime>.Assembly.GetName()))
            ))

    let consoleRef =
        lazy (toEntity (ctx.AssemblyRef(typeof<System.Console>.Assembly.GetName())))

    // FSharp.Core type references.
    let eUnit =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "Unit")))

    let ePrintfFormat4 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "PrintfFormat`4")))

    let ePrintfModule =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "PrintfModule")))

    let eFSharpFunc2 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Core", "FSharpFunc`2")))

    // The compiled `Vesper.Core.dll`'s identity + its `Fun\`2` interface. Like
    // every other ref these are `lazy` (G6), so a program forming no function
    // value pins no `Vesper.Core` `AssemblyRef`. Forcing `eFun2` without a
    // `Vesper.Core` reference is a hard error — there is nowhere for the function
    // value's `Fun` to come from (R1: `Fun` lives in `Vesper.Core`, not this
    // assembly, and not FSharp.Core).
    let vesperCoreRef =
        lazy (toEntity (ctx.AssemblyRef(refRequired "Vesper.Core" "a function value needs Vesper.Fun")))

    let eFun2 = lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "Fun`2")))

    // The compiled `Vesper.List.dll`'s identity (its own package now —
    // package-split-plan PS2 — no longer part of `Vesper.Core.dll`). Mirrors
    // `vesperCoreRef`: `lazy` (a program touching no list pins no `Vesper.List`
    // ref), and forcing it without a `Vesper.List` reference is a hard error —
    // there is nowhere for the list type to come from. Deliberately NO fallback to
    // `vesperCoreRef`: that would re-merge the list into Core's ref surface and
    // mint a wrong `Vesper.Core::List\`1` while every test still passed.
    let vesperListRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(refRequired "Vesper.List" "a list literal / List.fold needs Vesper.Collections.List")
            ))

    let eFSharpList1 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Collections", "FSharpList`1")))

    // (FSharp.Core's `ListModule.Fold` ref is gone: `List.fold` is emitted inline
    // over the Vesper list now — R3 — so nothing references it.)

    /// `Vesper.Collections.List\`1` — the cons-list compiled into its own
    /// `Vesper.List.dll` (package-split-plan PS2). A bare program's list literal +
    /// `List.fold` retarget onto it; it references no FSharp.Core, so it pins no
    /// FSharp.Core dependency (a `Vesper.List` `AssemblyRef` instead).
    let eVesperList1 =
        lazy (toEntity (ctx.TypeRef(vesperListRef.Value, "Vesper.Collections", "List`1")))

    /// `Vesper.Collections.ListModule` in `Vesper.List.dll` (R3 deferred): the
    /// compiled holder type for the `List` module's functions (the `ModuleSuffix`
    /// representation gives it the `ListModule` name). `emitFold` mints a
    /// `MemberRef` + `MethodSpec` on it — `fold` is compiled into the DLL now, not
    /// inlined.
    let eListModule =
        lazy (toEntity (ctx.TypeRef(vesperListRef.Value, "Vesper.Collections", "ListModule")))

    /// The abbreviation name the list-literal freeze hard-codes
    /// ([front-end-gaps-plan](../XParsec.FSharp.SemanticAnalysis/docs/front-end-gaps-plan.md)
    /// §A). Both `encodeType` and `TryEmitUnionCons` key on it; when the
    /// extractor mints `FSharpList` properly this becomes a registered
    /// `TyUnion` and the match keys on that instead — additive.
    let listTypeName = "Microsoft.FSharp.Collections.list"

    /// The Vesper cons-list's `SemType` name (R3) — the front-end retargets a
    /// bare program's list literal / `List.fold` onto it (`Unification`,
    /// `Freeze`); `encodeType` / `TryEmitUnionCons` / `emitFold` key on it to mint
    /// references into the compiled `Vesper.Collections.List\`1`.
    let vesperListName = "Vesper.Collections.List"

    /// The cons-list's *abbreviation* name (`Vesper.Collections.list`, lowercase).
    /// The `Vesper.List` contract types `List.fold`'s `'T list` parameter — and so
    /// every literal driven by it — with this abbreviation (the same convention
    /// FSharp.Core uses, where the literal is typed `Microsoft.FSharp.Collections.list`),
    /// whereas the self-host `'T list = List<'T>` path expands eagerly to the union
    /// name. Both denote the one cons-list, so every list recognition point accepts
    /// either (`isVesperListName`).
    let vesperListAbbrevName = "Vesper.Collections.list"

    let isVesperListName (name: string) =
        name = vesperListName || name = vesperListAbbrevName

    // BCL type references.
    let eObject = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Object")))

    let eTextWriter =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.IO", "TextWriter")))

    let eConsole = lazy (toEntity (ctx.TypeRef(consoleRef.Value, "System", "Console")))

    let eFormatter =
        lazy (toEntity (ctx.TypeRef(vesperRef.Value, "Vesper", "Formatter")))

    let eDecimal = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Decimal")))

    let eException = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Exception")))

    // BCL refs for synthesised structural equality / hashing (C-Eq1). Both live
    // in `System.Private.CoreLib` (`coreRef`), so a union's generated
    // `Equals`/`GetHashCode` pins only the BCL — no FSharp.Core, no Vesper.Core.
    let eEqualityComparer1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Collections.Generic", "EqualityComparer`1")))

    let eHashCode = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "HashCode")))

    // `System.IEquatable\`1` — the interface a monomorphic union implements so its
    // typed `Equals(Self)` is the boxing-free path `EqualityComparer<Self>.Default`
    // (a `GenericEqualityComparer`) reaches (C-Eq1). BCL (`coreRef`), so it pins no
    // FSharp.Core / Vesper dependency.
    let eEquatable1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IEquatable`1")))

    // BCL refs for synthesised structural comparison (records-plan §B6).
    // `Comparer\`1` is the comparison analogue of `EqualityComparer\`1`; the two
    // `IComparable` shapes mirror the equality side's `IEquatable\`1`. All live in
    // `System.Private.CoreLib`, so a record / union's generated `CompareTo` pair
    // pins only the BCL — no FSharp.Core, no Vesper.Core.
    let eComparer1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Collections.Generic", "Comparer`1")))

    /// `System.IComparable\`1` — the generic interface the typed `CompareTo(Self)`
    /// implements (the boxing-free path `Comparer<Self>.Default` reaches once the
    /// type declares it).
    let eComparable1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IComparable`1")))

    /// `System.IComparable` — the non-generic interface `CompareTo(object)`
    /// implements (the path `Comparer<obj>.Default` and BCL non-generic sort APIs
    /// reach when the type lacks `IComparable<T>` for the specific argument).
    let eComparable =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IComparable")))

    /// `System.ArgumentException` — the exception type `CompareTo(object)` throws
    /// when the argument is not of `Self`, matching F#'s emission.
    let eArgumentException =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ArgumentException")))

    /// `instance void System.Object::.ctor()` — the base ctor a union's own
    /// parameterless `.ctor` chains to.
    let eObjectCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eObject.Value, ".ctor", s)))

    /// `instance void System.Exception::.ctor(string)` — the constructor a
    /// non-exhaustive `match` fallthrough throws.
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

    /// `instance void System.ArgumentException::.ctor(string)` — the constructor
    /// the generated `CompareTo(object)` throws when the argument is not of the
    /// declaring type (records-plan §B6).
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

    /// `instance void System.Decimal::.ctor(int32, int32, int32, bool, uint8)` —
    /// the lo/mid/hi/sign/scale constructor used to materialise a `decimal`
    /// constant from its `Decimal.GetBits` representation.
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

    /// The `SemType` standing for the `Vesper.Formatter` ref-struct handler
    /// local. `encodeType` maps it to the value-type signature; the printf
    /// special-form declares a local of this type per `Format` node.
    let formatterTypeName = "Vesper.Formatter"

    /// Each distinct FSharp.Core construct the emission actually references.
    /// Lets a build tell — *positively* — whether the produced PE depends on
    /// `FSharp.Core.dll` at all (empty ⇒ no dependency, so `materialiseApp`
    /// skips copying it) and, when it does, *what* pins the dependency (a
    /// ready-made target list for the eventual FSharp.Core cut, handoff §D3).
    /// Every FSharp.Core reference is minted through this provider — `fsCoreRef`
    /// and its typerefs are private here — so marking each use-site below
    /// captures the whole dependency surface. A `HashSet` so repeats collapse;
    /// the tag names the construct, not just the type, to be useful as a target
    /// list. Populated during emission; read after via `FSharpCoreDependencies`.
    let fsharpCoreDeps = HashSet<string>()
    let markFSharpCoreDep (construct: string) : unit = fsharpCoreDeps.Add construct |> ignore

    /// User types emitted into *this* assembly (rung 2: unions), by simple name →
    /// their `TypeDefinition` handle. `encodeType` resolves a `TyUnion name` slot
    /// to this handle so a field / factory / local signature can reference the
    /// type before its `TypeDefinition` row is even added (the handle is predicted
    /// from the row order — see `Codegen.assembleProgram`). Populated up front via
    /// `RegisterUserType`.
    let userTypes = Dictionary<string, EntityHandle>()

    /// Generic user unions emitted into this assembly (rung 2 P3d.4), by simple
    /// name → (typar names, cases). A case is `(caseName, [(fieldMetaName, declTy)])`
    /// where `declTy` carries the declaring-typar markers (`TyConst "'T"`). The
    /// `TypeDefinition` handle itself lives in `userTypes`; this holds the extra
    /// shape needed to mint `MemberRef`s on the type's `TypeSpec` (the member-ref
    /// signatures are written in terms of the type's own generic parameters, so
    /// the declared types — with their typar markers — are the source of truth).
    /// Monomorphic unions are *not* registered here (their `Def` tokens suffice).
    let genericUnions =
        Dictionary<string, string list * (string * (string * SemType) list) list>()

    /// Generic user records emitted into this assembly (records-plan §B2), by
    /// simple name → (typar names, fields). A field is `(name, declTy)` where
    /// `declTy` carries the declaring-typar markers (`TyConst "'T"`). The
    /// records-plan analogue of `genericUnions`: same role (parent `TypeSpec` +
    /// member-ref signatures written in the type's own typars), simpler shape
    /// (no cases — a record is one nameless "case", every field on every
    /// instance). Monomorphic records are *not* registered here.
    let genericRecords = Dictionary<string, string list * (string * SemType) list>()

    /// Generic user classes emitted into this assembly (vesper-set-sprint-plan
    /// Phase 1 / B-1), by simple name → (typar names, fields). A class's
    /// fields mirror a record's at the metadata level (one public field per
    /// primary-ctor parameter, keyed by source name; the `Member` arm of
    /// `ClassMember` carries augmentation-member signatures explicitly). The
    /// `genericRecords` analogue, sharing the same `encodeUnionType` encoder.
    /// Monomorphic classes are *not* registered here.
    let genericClasses = Dictionary<string, string list * (string * SemType) list>()

    /// Generic closures emitted into this assembly (function-representation-plan §Generic closures, C2), by
    /// closure name → its shape. Unlike generic unions / records, the typars
    /// here are not source-level names but the *enclosing static method's*
    /// `TypeVar` roots (`Closure.Typars`, C1); signature encoding identifies
    /// them by root identity rather than marker name. The capture/param/result
    /// `SemType`s embed those same roots wherever a typar appears —
    /// `closureTyparLeaf` maps them to `!i` during the closure's own emission.
    /// Monomorphic closures are *not* registered here (their `Def` tokens
    /// suffice, same as monomorphic unions / records).
    let genericClosures = Dictionary<string, GenericClosureShape>()

    /// Resolve a `SemType` to its concrete representative, chasing union-find
    /// links. After `ResolvedTypes` no *free* TyVar survives, so the only job
    /// here is dereferencing linked ones.
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

    // ---- External (referenced-assembly) reference minting (P4) ----
    //
    // The identity bridge (symbol-resolution-plan §3/§7.2): a resolved external
    // symbol's `Origin`/`SymbolKey` → an `AssemblyRef`/`TypeRef`/`TypeSpec`/
    // `MemberRef`, with no per-member hand-coding. The host fallback for an
    // un-referenced assembly reads the live copy's identity (version + PKT), the
    // same posture `coreRef` takes — `Origin.Assembly` for the BCL is currently the
    // *implementation* name (`System.Private.CoreLib`), not the target ref (the
    // §6/§9 ref-pack TODO).

    /// An `AssemblyRef` for an external symbol's home assembly by simple name: a
    /// `ProjectInfo.References` entry (read off the file — R4) wins, else the
    /// host-loaded copy's identity.
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

    /// A `TypeRef` for a referenced-assembly type by its full metadata name
    /// (`` System.Collections.Generic.EqualityComparer`1 ``), resolved through the
    /// symbol provider's `Origin` (assembly + namespace). `ValueNone` if the
    /// provider doesn't resolve it as a class/interface.
    let externalClassRef (fullName: string) : EntityHandle voption =
        match symbols.TryLookupType fullName with
        | ValueSome(ExternalTypeShape.Class info) ->
            let ns = info.Origin.Namespace
            let simple = SymbolOrigin.StripNamespace ns fullName
            ValueSome(toEntity (ctx.TypeRef(externalAsmRef info.Origin.Assembly, ns, simple)))
        | _ -> ValueNone

    /// Look up a *referenced-assembly* record's shape by `TyRecord`-carried name
    /// + arity (records-plan §B7): returns the field shapes
    /// (declaration order) and the `Origin` (assembly + namespace) so the
    /// caller can mint a `TypeRef`. The contract layer keys generic records by
    /// the bare compiled name (`Vesper.Ref`), the metadata layer by the
    /// arity-suffixed key (`` Vesper.Ref`1 ``); both forms are probed.
    /// `ValueNone` ⇒ unknown record or one whose origin is `Empty` (no assembly
    /// to mint a `TypeRef` against).
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

    /// A `TypeRef` for a referenced-assembly record (records-plan §B7). Sibling
    /// of `externalClassRef`: a generic record published in another package
    /// (`Vesper.Core.dll`'s `Ref<'T>`) reaches its `TypeDefinition` through
    /// this path, so the captured-mutable promotion's `RecordCons` can mint a
    /// member ref onto the instantiated `TypeSpec` instead of declaring a
    /// local copy.
    let externalRecordRef (fullName: string) (arity: int) : (EntityHandle * ExternalFieldShape[]) voption =
        match externalRecordShape fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(fields, origin) ->
            let ns = origin.Namespace
            let bareSimple = SymbolOrigin.StripNamespace ns fullName

            // Metadata `TypeRef` simple names carry the `` `n `` arity suffix for
            // a generic type. The contract-layer key (`Vesper.Ref`) lacks it; the
            // metadata-layer key (`Vesper.Ref`1`) already has it. Add when absent.
            let simple =
                if arity > 0 && not (bareSimple.Contains '`') then
                    sprintf "%s`%d" bareSimple arity
                else
                    bareSimple

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Assembly, ns, simple)), fields)

    /// `FSharpList\`1<X>` where `X` is encoded by `inner`. The one place that
    /// knows the list type's metadata shape, shared by `encodeType`'s list case
    /// (elem encoded recursively) and the cons/nil recipe signatures (where the
    /// element is the declaring type's generic parameter `!0`).
    let encodeListOf (te: SignatureTypeEncoder) (inner: SignatureTypeEncoder -> unit) : unit =
        markFSharpCoreDep "Microsoft.FSharp.Collections.FSharpList`1"
        let g = te.GenericInstantiation(eFSharpList1.Value, 1, false)
        inner (g.AddArgument())

    // ---- Generic module-static-method context (R3) ----
    //
    // A generic top-level function (`module List.fold`) is emitted as a *generic
    // static method*: its type parameters are the free `TypeVar`s of its
    // signature, encoded as `GenericMethodTypeParameter` (`!!i`). The set is
    // ambient — `SetMethodTypars` installs it (by union-find root) around the
    // method's signature / locals / body emission and `ClearMethodTypars` removes
    // it, so `encodeType` (and the generic-union arg encoder, for a `List<!!i>`
    // member ref *inside* such a body) map those `TypeVar`s to `!!i`. Empty
    // outside a generic static method, so every other emission is unchanged.
    let mutable methodTyparRoots: TypeVar list = []

    /// First crack at a zonked leaf: a `TypeVar` that is one of the current
    /// method's type parameters encodes to `GenericMethodTypeParameter` (`!!i`).
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

    // ---- Ambient *type*-typar context (S4: generic-union equality triple) ----
    //
    // A generic union's synthesised `Equals`/`GetHashCode`/`IEquatable` triple is
    // written in the type's own generic parameters (`!0`): a typar-typed field
    // (`Head : 'T`) reaches `EqualityComparer<!0>` / `HashCode.Add<!0>`, and the
    // self refs (`isinst`, the `other` local, the typed-`Equals` param, the
    // interface arg) are the type's `TypeSpec` (`List<!0>`). `SetTypeTypars`
    // installs a name → index map (`'T` ⇒ `!0`) around the triple emission so the
    // shared equality helpers — which encode through `encodeType` — map a marker
    // leaf to the type's `GenericTypeParameter`; `ClearTypeTypars` removes it.
    // Empty everywhere else (parallels `methodTyparRoots`'s `!!i`), so every other
    // emission, monomorphic and generic-member alike, is unchanged.
    let mutable typeTyparIx: Map<string, int> = Map.empty

    /// A zonked leaf that is a declaring-type typar marker (`TyConst "'T"`)
    /// encodes to that type's `GenericTypeParameter` (`!0`) while the ambient set
    /// is installed. (Markers carry the F# leading quote, as registered in
    /// `genericUnions`.)
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

    // ---- Ambient *closure*-typar context (function-representation-plan §Generic closures, C2) ----
    //
    // A generic closure's `TypeDefinition` carries its enclosing static method's
    // typars (`Closure.Typars`, by union-find root) — encoded as
    // `GenericTypeParameter` (`!i`) on the type, *not* `GenericMethodTypeParameter`
    // (`!!i`). `SetClosureTypars` installs the set (by root) around the closure's
    // own ctor / Invoke / field signatures (and locals, body when added in C3);
    // `ClearClosureTypars` removes it. While installed, `encodeType` maps a free
    // `TyVar` whose root is in the set to that closure type's
    // `GenericTypeParameter`. Empty everywhere else (parallels `methodTyparRoots`'s
    // `!!i`, `typeTyparIx`'s `!0` marker case), so every other emission —
    // monomorphic closures, static methods, union/record bodies — is unchanged.
    //
    // Invariant: at most one of `methodTyparRoots`, `typeTyparIx`, `closureTyparRoots`
    // is installed (non-empty) at any given moment during a single signature
    // encoding. The three contexts emit `!!i`, `!i`-by-name (TyConst marker), and
    // `!i`-by-root (TyVar) respectively; the leaves are disjoint by construction
    // (each is empty unless its `Set…` is active), so the OR-chain below is safe.
    let mutable closureTyparRoots: TypeVar list = []

    /// A zonked-leaf hook: a free `TypeVar` whose root is one of the current
    /// closure's typars encodes to that closure type's `GenericTypeParameter`
    /// (`!i`). Empty `closureTyparRoots` ⇒ no-op (every other phase).
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

    /// The executable-path leaf: a generic *method* typar (`!!i`, R3) first, then
    /// a generic *type* typar (`!0` by name, S4), then a generic *closure* typar
    /// (`!i` by root, function-representation-plan §Generic closures, C2). All three sets are empty on the
    /// common path, so this is the prior no-op leaf unless one is installed; at
    /// most one is installed at a time (disjoint by phase).
    let ambientTyparLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
        methodTyparLeaf te zt || typeTyparLeaf te zt || closureTyparLeaf te zt

    /// Encode a (zonked) `SemType` into a metadata signature type slot.
    /// `tryLeaf` gets first crack at each zonked node before the structural
    /// match: when it encodes the node (returning `true`) recursion stops there.
    /// The executable path passes a no-op (`fun _ _ -> false`); the library path
    /// (`EncodeAbstractType`) passes a resolver for typar markers (which have no
    /// `SemType`-level index, only a positional one), so an abstract signature
    /// can still reach the concrete arms here — `unit`, a nested function type, a
    /// primitive — that the old provider-free library encoder couldn't (G5).
    let rec encodeTypeCore
        (tryLeaf: SignatureTypeEncoder -> SemType -> bool)
        (te: SignatureTypeEncoder)
        (t: SemType)
        : unit =
        let zt = zonk t

        if tryLeaf te zt then
            ()
        else
            match zt with
            // `unit` is name-keyed to `FSharp.Core.Unit` ahead of the representation
            // rekey below — NOT its `prim-types-min` `System.ValueTuple` binding — and
            // this is forced, not a preference, while FSharp.Core is still in the loop:
            //   1. The FSharp.Core surfaces are typed in `Unit`. The `%A` cold path
            //      instantiates FSharp.Core's `PrintfFormat\`4<…, Unit, Unit>` (see
            //      `encodeFormatParam`) and a `a -> unit` closure overrides
            //      `FSharpFunc\`2<a, Unit>::Invoke` — any other repr is a metadata
            //      mismatch against the real members.
            //   2. The unit *value* is emitted as `ldnull` (`()` is `Unit`'s null; see
            //      `Cil.emitLdnull`), which is not a valid `System.ValueTuple` (a
            //      zero-field struct needs `initobj`, not a null reference).
            // So flipping `unit` to its declared repr is entangled with the FSharp.Core
            // cut (handoff §D3 / P2), not a one-liner; until then a file's `unit`
            // binding (and the two already-resolved BCL/runtime type names) can't hijack
            // it. `TextWriter` / `Formatter` precede the rekey for the same can't-hijack
            // reason.
            | TyConst "unit" ->
                markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
                te.Type(eUnit.Value, false)
            | TyConst "System.IO.TextWriter" -> te.Type(eTextWriter.Value, false)
            | TyConst "Vesper.Formatter" -> te.Type(eFormatter.Value, true)
            // The `System.HashCode` accumulator local of a union's generated
            // `GetHashCode` (a value type, like `Formatter`); name-keyed because
            // it has no Vesper-primitive representation.
            | TyConst "System.HashCode" -> te.Type(eHashCode.Value, true)
            | TyConst name when reprs.ContainsKey name ->
                // Primitive binding: key the IL type off the representation string the
                // name maps to (`"int"` → `"System.Int32"` → `i4`), not the Vesper
                // name (G7). The provider-only `System.Decimal` `TypeRef` is emitted
                // here; the reference-free value types are shared with the library
                // encoder via `IntrinsicRepr.tryEncodeValueType`.
                let repr = reprs.[name]

                if IntrinsicRepr.tryEncodeValueType te repr then
                    ()
                elif repr = "System.Decimal" then
                    te.Type(eDecimal.Value, true)
                else
                    failwithf "ClrProvider: no IL encoding for intrinsic representation %s (type %s)" repr name
            | TyFun(a, b) ->
                // `a -> b` is `Vesper.Fun\`2<a, b>` at the metadata level (R1 / D3),
                // read from the compiled `Vesper.Core.dll` — no FSharp.Core.
                let g = te.GenericInstantiation(eFun2.Value, 2, false)
                encodeTypeCore tryLeaf (g.AddArgument()) a
                encodeTypeCore tryLeaf (g.AddArgument()) b
            | TyClass(name, args) when name = PrintfSpec.printfFormatName ->
                markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
                let g = te.GenericInstantiation(ePrintfFormat4.Value, args.Length, false)

                for a in args do
                    encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(name, args) when name = listTypeName && args.Length = 1 ->
                // `list<elem>` ≡ `FSharpList\`1<elem>`. Serves every list-typed
                // slot: the `%A` printer's `FSharpFunc` arg, the `PrintfFormat`
                // ctor / `PrintFormatLine` / `Invoke` instantiations, and any
                // list-typed local signature.
                let elem = args.[0]
                encodeListOf te (fun arg -> encodeTypeCore tryLeaf arg elem)
            | TyRecord(name, args) when isVesperListName name && args.Length = 1 ->
                // The Vesper cons-list (R3) ≡ `Vesper.Collections.List\`1<elem>` in
                // the compiled `Vesper.Core.dll`. No FSharp.Core dep — it lives next
                // to `Fun`. The abbreviation name (`…list`) reaches here from the
                // contract-typed literal, the union name (`…List`) from the self-host
                // path; both map to the same `List\`1`.
                let elem = args.[0]
                let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
                encodeTypeCore tryLeaf (g.AddArgument()) elem
            | TyUnion(name, args) when userTypes.ContainsKey name ->
                // A user union emitted into this assembly (rung 2). Monomorphic
                // (`TyUnion(name, [])`): reference its `TypeDefinition` directly.
                // Generic (`TyUnion("List", [int])`): a `TypeSpec` instantiation
                // `List\`1<int>` over the predicted `TypeDefinition` handle, each
                // argument encoded recursively (a typar argument is intercepted by
                // `tryLeaf` — `'T` ⇒ `!0` — so this serves both a concrete `[int]`
                // use site and the type's own `[!0]` self-reference) (P3d.4).
                if args.IsEmpty then
                    te.Type(userTypes.[name], false)
                else
                    let g = te.GenericInstantiation(userTypes.[name], args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(name, args) when userTypes.ContainsKey name ->
                // A user *record* emitted into this assembly (records-plan §B2)
                // — same encoding as a user union, keyed by `TyRecord`. The list
                // special-cases above (`isVesperListName` / `listTypeName`) win
                // by precedence so a `list<elem>` slot still maps to `FSharpList\`1`
                // / `Vesper.Collections.List\`1`; only *user* records reach here.
                if args.IsEmpty then
                    te.Type(userTypes.[name], false)
                else
                    let g = te.GenericInstantiation(userTypes.[name], args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyClass(name, args) when userTypes.ContainsKey name ->
                // A user *class* emitted into this assembly (vesper-set-sprint-plan
                // Phase 1 / B-1) — same encoding as a user union/record, keyed by
                // `TyClass`. Checked *before* the external-class arm so a project-
                // local class wins over an accidental same-named external one.
                if args.IsEmpty then
                    te.Type(userTypes.[name], false)
                else
                    let g = te.GenericInstantiation(userTypes.[name], args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyClass(name, args) when (externalClassRef name).IsSome ->
                // A referenced-assembly type resolved through the symbol provider
                // (P4): its `TypeRef`, instantiated over each argument when generic.
                // Reached for a member-ref parent (via `externalTypeSpec`) and any
                // standalone slot typed as an external class. A nested marker
                // argument is intercepted by `tryLeaf` (`!i`), exactly like the user
                // union arm above.
                let tref = (externalClassRef name).Value

                if args.IsEmpty then
                    te.Type(tref, false)
                else
                    let g = te.GenericInstantiation(tref, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(name, args) when (externalRecordRef name args.Length).IsSome ->
                // A referenced-assembly *record* (records-plan §B7): its
                // `TypeRef`, instantiated over each argument when generic.
                // Lower priority than the user-record arm above (`userTypes` is
                // checked first), so a same-named user record still wins.
                let tref, _ = (externalRecordRef name args.Length).Value

                if args.IsEmpty then
                    te.Type(tref, false)
                else
                    let g = te.GenericInstantiation(tref, args.Length, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | other -> failwithf "ClrProvider: cannot encode SemType: %A" other

    /// Encode a (zonked) `SemType` for the executable path. The only leaf hook is
    /// the ambient generic-method-typar resolver (`!!i`), which is empty except
    /// while a generic static method (`List.fold`) is being emitted (R3) — so
    /// every monomorphic emission is unchanged.
    and encodeType (te: SignatureTypeEncoder) (t: SemType) : unit = encodeTypeCore ambientTyparLeaf te t

    /// Encode a `SemType` mapping each function arrow to FSharp.Core's
    /// `FSharpFunc\`2` (curried, nested), not `Vesper.Fun`. For the FSharp.Core
    /// *interop islands* R1 deliberately leaves on the old representation: the cold
    /// printf printer, which is an `FSharpFunc` produced by `PrintfModule` (retargeted
    /// by the printf engine, handoff §R9). Non-function leaves delegate to
    /// `encodeType`, so `unit` / primitives / lists encode identically.
    let rec encodeFSharpFunc (te: SignatureTypeEncoder) (t: SemType) : unit =
        match zonk t with
        | TyFun(a, b) ->
            markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2"
            let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
            encodeFSharpFunc (g.AddArgument()) a
            encodeFSharpFunc (g.AddArgument()) b
        | other -> encodeType te other

    // ---- External member-ref minting (P4) ----

    /// The open-generic arity off a metadata type name's backtick suffix
    /// (`` EqualityComparer`1 `` → 1; no suffix → 0) — the count of the declaring
    /// type's own generic parameters, hence the marker / instantiation length.
    let arityOfMetaName (name: string) : int =
        match name.LastIndexOf '`' with
        | i when i >= 0 ->
            match System.Int32.TryParse(name.Substring(i + 1)) with
            | true, n -> n
            | _ -> 0
        | _ -> 0

    /// Decurry a (zonked) curried function type into (parameter types, return).
    let rec decurryTy (t: SemType) : SemType list * SemType =
        match zonk t with
        | TyFun(a, b) ->
            let ps, r = decurryTy b
            a :: ps, r
        | other -> [], other

    /// Encode a `SemType` written in the declaring type's *open* typars: a marker
    /// `TypeVar` (one of `markerRoots`) maps to its `GenericTypeParameter` index;
    /// every other leaf delegates to the structural encoder (so a primitive, an
    /// external class, a nested generic over the marker all encode). The member-ref
    /// signature is written in these terms (`!0`), the instantiation riding the
    /// parent `TypeSpec`.
    let encodeOpen (markerRoots: TypeVar list) (te: SignatureTypeEncoder) (t: SemType) : unit =
        let tryLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
            match zt with
            | TyVar tv ->
                let r = UnionFind.find tv

                match markerRoots |> List.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                | Some i ->
                    te.GenericTypeParameter i
                    true
                | None -> false
            | _ -> false

        encodeTypeCore tryLeaf te t

    /// Recover the declaring type's instantiation by structurally matching the
    /// member's *open* signature (carrying the marker `TypeVar`s) against its
    /// *instantiated* type at the use site — the same shape `Emit.matchInstantiation`
    /// uses for generic static methods. First occurrence wins; an unmatched marker
    /// is a bug (the open form came from the same member the instantiated type did).
    let recoverTypeArgs (markerRoots: TypeVar list) (openT: SemType) (instT: SemType) : SemType list =
        let result = Array.create (List.length markerRoots) ValueNone

        let rec go (d: SemType) (a: SemType) =
            match zonk d, zonk a with
            | TyVar tv, act ->
                let r = UnionFind.find tv

                match markerRoots |> List.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                | Some i ->
                    if result.[i].IsNone then
                        result.[i] <- ValueSome act
                | None -> ()
            | TyFun(a1, r1), TyFun(a2, r2) ->
                go a1 a2
                go r1 r2
            | TyTuple xs, TyTuple ys when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyRecord(_, xs), TyRecord(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyUnion(_, xs), TyUnion(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyClass(_, xs), TyClass(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | _ -> ()

        go openT instT

        [
            for i in 0 .. result.Length - 1 ->
                match result.[i] with
                | ValueSome t -> t
                | ValueNone ->
                    failwithf "ClrProvider: could not recover external type argument %d (open %A vs %A)" i openT instT
        ]

    /// The member-ref parent: the declaring `TypeRef`, wrapped in a `TypeSpec`
    /// instantiation when generic (`` EqualityComparer`1<int> ``).
    let externalTypeSpec (tref: EntityHandle) (instArgs: SemType list) : EntityHandle =
        match instArgs with
        | [] -> tref
        | _ ->
            let tsB = BlobBuilder()
            let te = BlobEncoder(tsB).TypeSpecificationSignature()
            let g = te.GenericInstantiation(tref, List.length instArgs, false)

            for a in instArgs do
                encodeType (g.AddArgument()) (zonk a)

            toEntity (ctx.TypeSpec tsB)

    /// `SymbolKey` (+ instantiation) → minted `MemberRef`, so a member is reified
    /// once across a compilation (mechanism B's codegen memo, §7.2).
    let externalMemberCache = Dictionary<string, EntityHandle>()

    /// Mint the `MemberRef` for a `TExpr.ExternalMember` (P4). The key pins the
    /// declaring type + member name; `memberTy` is the access's instantiated type.
    /// The member's *open* signature is read from the (key-pinned, provider-cached)
    /// `TryLookupMember` over fresh marker typars; the use-site instantiation is
    /// recovered by matching that open form against `memberTy`. A property is a
    /// parameterless `get_<name>` getter; a method decurries its open signature
    /// (`unit → ret`, the zero-arg reading, collapses to no parameters).
    let externalMemberRef (key: SymbolKey) (isProperty: bool) (isStatic: bool) (memberTy: SemType) : EntityHandle =
        let declKey, memberName, argSig =
            match key with
            | SymbolKey.MemberKey(d, m, a, _) -> d, m, a
            | other -> failwithf "ClrProvider: ExternalMember key is not a MemberKey: %A" other

        let asm, ns, name =
            match declKey with
            | SymbolKey.TypeKey(asm, ns, name) -> asm, ns, name
            | other -> failwithf "ClrProvider: ExternalMember declaring key is not a TypeKey: %A" other

        let instTy = zonk memberTy
        let memoKey = sprintf "%A|%b|%b|%A" key isProperty isStatic instTy

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = if ns = "" then name else ns + "." + name

            let markers = [ for _ in 1 .. arityOfMetaName name -> TypeVar() ]
            let markerRoots = markers |> List.map UnionFind.find
            let markerTys = markers |> List.map TyVar |> List.toArray

            // Build the open signature from the *exact* overload the front end
            // committed (its key, incl. `argSig`, matches `key`) — NOT a singular
            // re-pick, which would re-collapse a resolved overload back to the
            // most-params one and disagree with the node's `memberTy` (type-args-bug.md
            // Layer 2). The singular `TryLookupMember` is the fallback for providers
            // that expose only that surface (a member with a single overload).
            let openSig =
                let chosen =
                    match
                        symbols.TryLookupMembers(declFullName, memberName)
                        |> Array.tryFind (fun m -> m.Key = key)
                    with
                    | Some m -> ValueSome m
                    | None -> symbols.TryLookupMember(declFullName, memberName)

                match chosen with
                | ValueSome m -> m.BuildSignature markerTys
                | ValueNone ->
                    failwithf "ClrProvider: external member '%s.%s' did not resolve at emit" declFullName memberName

            let instArgs = recoverTypeArgs markerRoots openSig instTy

            let tref =
                match externalClassRef declFullName with
                | ValueSome t -> t
                | ValueNone ->
                    failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

            let parent = externalTypeSpec tref (List.map zonk instArgs)
            let metaName = if isProperty then "get_" + memberName else memberName
            let s = BlobBuilder()

            if isProperty then
                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = not isStatic)
                    .Parameters(
                        0,
                        (fun (ret: ReturnTypeEncoder) -> encodeOpen markerRoots (ret.Type()) openSig),
                        (fun (_: ParametersEncoder) -> ())
                    )
            else
                let rawParams, retTy = decurryTy openSig

                // A .NET method of arity ≥ 2 is modelled tupled (`(p1*…*pN) → ret`,
                // type-args-bug.md Layer 1), so the lone decurried "parameter" is the
                // argument `TyTuple` — flatten it back to N separate parameters for
                // the member-ref blob, driven by the chosen key's `argSig` length
                // (authoritative: a genuine single `(int*int)` param has argSig
                // length 1 and must stay one parameter). Arity ≤ 1 is unchanged.
                let paramTys =
                    match rawParams with
                    | [ TyConst "unit" ] -> []
                    | [ TyTuple elems ] when argSig.Length >= 2 && elems.Length = argSig.Length -> EqArray.toList elems
                    | ps -> ps

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = not isStatic)
                    .Parameters(
                        List.length paramTys,
                        (fun (ret: ReturnTypeEncoder) -> encodeOpen markerRoots (ret.Type()) retTy),
                        (fun (pars: ParametersEncoder) ->
                            for p in paramTys do
                                encodeOpen markerRoots (pars.AddParameter().Type()) p
                        )
                    )

            let handle = toEntity (ctx.MemberRef(parent, metaName, s))
            externalMemberCache.[memoKey] <- handle
            handle

    // ---- Generic union emission (rung 2 P3d.4) ----

    /// `typar name → positional index` for a generic union's own type parameters.
    let typarIx (typars: string list) : Map<string, int> =
        typars |> List.mapi (fun i n -> n, i) |> Map.ofList

    /// Encode a `SemType` declared *within* a generic union (a field type, a
    /// factory parameter/return) — its own typar markers (`TyConst "'T"`) resolve
    /// to `GenericTypeParameter` indices, everything else delegates to `encodeType`.
    /// The member-ref / type-def signatures are all written in these terms (`!0`),
    /// with the instantiation supplied by the parent `TypeSpec`.
    let encodeUnionType (typeIx: Map<string, int>) (te: SignatureTypeEncoder) (t: SemType) : unit =
        let tryLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
            match zt with
            | TyConst name when typeIx.ContainsKey name ->
                te.GenericTypeParameter(typeIx.[name])
                true
            // A `List<!!i>` member-ref instantiation arg *inside* a generic static
            // method body resolves the method's typar `TypeVar`s to `!!i` (R3);
            // empty otherwise, so a union's own factory/member emission (whose args
            // are the union's `TyConst "'T"` markers above, or concrete) is unchanged.
            | _ -> methodTyparLeaf te zt

        encodeTypeCore tryLeaf te t

    /// `name\`n<args>` as a member-ref parent `TypeSpec`. `args` is the use-site
    /// instantiation — concrete (`[int]`) at an external site, the type's own
    /// typar markers (`[TyConst "'T"]` ⇒ `!0`) inside a factory body. Either way
    /// each argument is encoded through `encodeUnionType`, so a typar marker maps
    /// to `!i` and a concrete leaf to its IL type.
    let genericUnionTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let typars, _ = genericUnions.[name]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[name], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    /// A `MemberRef` to one member of generic union `name` instantiated at `args`.
    /// The parent is the `TypeSpec` above; the signature is in terms of the type's
    /// own generic parameters (the runtime substitutes the parent's args).
    let genericUnionMemberRef (name: string) (args: SemType list) (which: UnionMember) : EntityHandle =
        let typars, cases = genericUnions.[name]
        let typeIx = typarIx typars
        let parent = genericUnionTypeSpec name args

        let caseFields cn =
            cases |> List.find (fun (n, _) -> n = cn) |> snd

        match which with
        | UnionMember.Ctor ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | UnionMember.Tag ->
            let s = BlobBuilder()
            BlobEncoder(s).FieldSignature().Int32()
            toEntity (ctx.MemberRef(parent, "_tag", s))
        | UnionMember.Field(caseName, idx) ->
            let metaName, declTy = (caseFields caseName).[idx]
            let s = BlobBuilder()
            encodeUnionType typeIx (BlobEncoder(s).FieldSignature()) declTy
            toEntity (ctx.MemberRef(parent, metaName, s))
        | UnionMember.Factory caseName ->
            let paramTys = caseFields caseName |> List.map snd
            let retTy = TyUnion(name, EqArray.ofSeq (seq { for t in typars -> TyConst t }))
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, caseName, s))
        | UnionMember.Member(metaName, isStatic, paramTys, retTy) ->
            // An augmentation member (`get_Head` / a static method): the signature
            // is written in the type's own `!0` (the marker types carried on the
            // `TTypeMember` post-Freeze), the instantiation rides the parent
            // `TypeSpec`. Instance members carry an implicit `this` (encoded by
            // `isInstanceMethod = true`); the explicit params follow.
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, metaName, s))

    // ---- Generic record emission (records-plan §B2) ----

    /// `name\`n<args>` for a generic *record* as a member-ref parent `TypeSpec`,
    /// instantiated at `args`. Sibling of `genericUnionTypeSpec`; the same
    /// `encodeUnionType` encoder is reused for the instantiation arguments
    /// (typar markers map to `!i`, concrete leaves to their IL types).
    let genericRecordTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let typars, _ = genericRecords.[name]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[name], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    /// A `MemberRef` to one member of generic record `name` instantiated at
    /// `args`. The parent is `genericRecordTypeSpec`; the signature is written
    /// in the type's own typars (`!0`), with the parent supplying the runtime
    /// instantiation. The records-plan §B2 mirror of `genericUnionMemberRef` —
    /// fewer cases (a record has no tag / factories / aug members in v1; just
    /// the ctor and the named fields).
    let genericRecordMemberRef (name: string) (args: SemType list) (which: RecordMember) : EntityHandle =
        let typars, fields = genericRecords.[name]
        let typeIx = typarIx typars
        let parent = genericRecordTypeSpec name args

        match which with
        | RecordMember.Ctor ->
            // `instance void .ctor(field0, field1, …)` — the ctor a `RecordCons`
            // calls. Parameter types are the field types (declaration order),
            // written in the type's own typars.
            let paramTys = fields |> List.map snd
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | RecordMember.Field fieldName ->
            match fields |> List.tryFind (fun (n, _) -> n = fieldName) with
            | Some(_, declTy) ->
                let s = BlobBuilder()
                encodeUnionType typeIx (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | None -> failwithf "ClrProvider: generic record '%s' has no field '%s'" name fieldName

    // ---- Generic class emission (vesper-set-sprint-plan Phase 1 / B-1) ----
    //
    // A generic class is shaped like a generic record at the metadata level:
    // one public field per primary-ctor parameter (keyed by source name), one
    // `.ctor` taking those fields in declaration order. Augmentation members
    // (instance / static methods + properties) ride the same `Member` arm
    // `UnionMember` uses for union augmentation members, but no tag / case /
    // factory machinery is involved.

    /// `name\`n<args>` as a member-ref parent `TypeSpec` for a generic class.
    /// Mirror of `genericRecordTypeSpec` / `genericUnionTypeSpec`: the same
    /// `encodeUnionType` encoder is reused for the instantiation arguments
    /// (typar markers map to `!i`, concrete leaves to their IL types).
    let genericClassTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let typars, _ = genericClasses.[name]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[name], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    /// A `MemberRef` to one member of generic class `name` instantiated at
    /// `args`. The parent is `genericClassTypeSpec`; the signature is in the
    /// type's own typars (`!0`). The `Ctor` / `Field` arms mirror the
    /// `RecordMember` shape; `Member` mirrors `UnionMember.Member` (a method
    /// / property emitted as a `MemberDef` on the class, reached at a use
    /// site through a `MemberRef` on the instantiated `TypeSpec`).
    let genericClassMemberRef (name: string) (args: SemType list) (which: ClassMember) : EntityHandle =
        let typars, fields = genericClasses.[name]
        let typeIx = typarIx typars
        let parent = genericClassTypeSpec name args

        match which with
        | ClassMember.Ctor ->
            // `instance void .ctor(field0, field1, …)` — parameter types are
            // the backing-field types in declaration order, written in the
            // class's own typars.
            let paramTys = fields |> List.map snd
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | ClassMember.Field fieldName ->
            match fields |> List.tryFind (fun (n, _) -> n = fieldName) with
            | Some(_, declTy) ->
                let s = BlobBuilder()
                encodeUnionType typeIx (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | None -> failwithf "ClrProvider: generic class '%s' has no field '%s'" name fieldName
        | ClassMember.Member(metaName, isStatic, paramTys, retTy) ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, metaName, s))

    // ---- Generic closure emission (function-representation-plan §Generic closures, C2) ----
    //
    // The closure analogue of `genericUnionTypeSpec` / `genericUnionMemberRef`.
    // A generic closure is a real generic `TypeDefinition` whose typars mirror
    // the enclosing static method's; references to its members go through a
    // `MemberRef` on the closure's instantiated `TypeSpec`, signatures written
    // in the closure's own `!i` markers (`closureTyparLeaf` ambient).
    //
    // The `args` at a construction site are typically the enclosing static
    // method's typars (encoded `!!i` via `methodTyparLeaf` — `closureTyparRoots`
    // is *not* installed there, so there is no `!i`/`!!i` ambiguity); inside the
    // closure's own emission (capture fields, ctor, Invoke), `closureTyparRoots`
    // *is* installed and the args resolve to `!i` instead.

    /// `<closure>$n<args>` as a member-ref parent `TypeSpec`. `args` is the
    /// use-site instantiation; each argument is encoded via `encodeType` (the
    /// ambient leaf chain — `methodTyparLeaf` at the construction site,
    /// `closureTyparLeaf` from inside the closure's own emission).
    let genericClosureTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let shape = genericClosures.[name]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()

        let g =
            te.GenericInstantiation(shape.DefHandle, List.length shape.TyparRoots, false)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    /// A `MemberRef` to one member of generic closure `name` instantiated at
    /// `args`. The parent is the `TypeSpec` above; the signature is written in
    /// the closure's own typars (`!i`), with the parent supplying the runtime
    /// instantiation. Signature encoding installs `closureTyparRoots` for the
    /// duration so a free `TyVar` whose root is one of the closure's typars
    /// resolves to `!i` via `closureTyparLeaf`. The C3 mirror of
    /// `genericUnionMemberRef` — fewer cases (no tag / factories / aug
    /// members; just the ctor, the indexed capture fields, and `Invoke`).
    let genericClosureMemberRef (name: string) (args: SemType list) (which: ClosureMember) : EntityHandle =
        let shape = genericClosures.[name]
        let parent = genericClosureTypeSpec name args

        // Install the ambient typars around the signature emission only. The
        // `parent` TypeSpec above was minted under the *caller's* ambient
        // (methodTyparRoots at a construction site, or another closureTyparRoots
        // for an inner-closure self-construction); the member-ref signature
        // below speaks the closure's own typars.
        let savedClosure = closureTyparRoots
        let savedMethod = methodTyparRoots
        closureTyparRoots <- shape.TyparRoots
        methodTyparRoots <- []

        let handle =
            match which with
            | ClosureMember.Ctor ->
                // `instance void .ctor(capture0, capture1, …)` — one parameter
                // per captured value, in declaration order (field order =
                // ctor-arg order = construction-site push order).
                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = true)
                    .Parameters(
                        List.length shape.CaptureSigs,
                        (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                        (fun (pars: ParametersEncoder) ->
                            for c in shape.CaptureSigs do
                                encodeType (pars.AddParameter().Type()) c
                        )
                    )

                toEntity (ctx.MemberRef(parent, ".ctor", s))
            | ClosureMember.CaptureField idx ->
                if idx < 0 || idx >= List.length shape.CaptureSigs then
                    failwithf
                        "ClrProvider: generic closure '%s' has %d capture fields, asked for index %d"
                        name
                        (List.length shape.CaptureSigs)
                        idx

                let captureTy = shape.CaptureSigs.[idx]
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) captureTy
                toEntity (ctx.MemberRef(parent, sprintf "capture%d" idx, s))
            | ClosureMember.Invoke ->
                // `instance ResultTy Invoke(ParamTy)` — the closure's `Invoke`
                // override implementing `Vesper.Fun\`2::Invoke` at the type's
                // own typars.
                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = true)
                    .Parameters(
                        1,
                        (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) shape.ResultTy),
                        (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) shape.ParamTy)
                    )

                toEntity (ctx.MemberRef(parent, "Invoke", s))

        closureTyparRoots <- savedClosure
        methodTyparRoots <- savedMethod
        handle

    // ---- Cross-package record emission (records-plan §B7) ----
    //
    // A record published in another package — `Vesper.Ref<'T>` in
    // `Vesper.Core.dll`, after the captured-mutable promotion writes a
    // `TyRecord("Vesper.Ref", _)` — is *not* in `genericRecords` / `userTypes`
    // (those only track records emitted into the current assembly). The
    // member-ref parent is its `TypeSpec` over its external `TypeRef`, exactly
    // the shape `externalMemberRef` already mints for an `EqualityComparer<>::Default`
    // — minus the open-typars machinery, because a cross-package record's field
    // signatures are read directly from the contract layer's
    // `ExternalFieldShape.BuildType` over marker typars.

    /// Mint the `MemberRef` for a referenced-assembly record's `.ctor`,
    /// instantiated at `args`. Parameter types are the record's declared fields
    /// (in declaration order), each in its *open* typar form — encoded against
    /// the same `markerRoots` an external-class member-ref uses.
    let externalRecordCtor (fullName: string) (args: SemType list) : EntityHandle voption =
        let arity = List.length args

        match externalRecordRef fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            let parent = externalTypeSpec tref (List.map zonk args)

            // Encode field-type slots in the type's own marker typars (`!0`),
            // same convention as `externalMemberRef`'s open signatures.
            let markers = [ for _ in 1..arity -> TypeVar() ]
            let markerRoots = markers |> List.map UnionFind.find
            let markerTys = markers |> List.map TyVar |> List.toArray
            let paramTys = [ for f in fields -> f.BuildType markerTys ]

            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeOpen markerRoots (pars.AddParameter().Type()) p
                    )
                )

            ValueSome(toEntity (ctx.MemberRef(parent, ".ctor", s)))

    /// Mint the `MemberRef` for one of a referenced-assembly class's
    /// constructors, instantiated at `tyArgs` and picked by call-site arity
    /// (`argTypes.Length`). Parameter types are read off the chosen ctor's
    /// `BuildSignature(markerTys)` so the signature is written in the type's
    /// own open typars (`!0`), with the instantiation riding the parent
    /// `TypeSpec` — same convention `externalMemberRef` uses for instance methods.
    ///
    /// Overload disambiguation is **arity only** in v1: a multi-overload class
    /// where two ctors share an arity (rare in the BCL but legal) picks the
    /// first one. The front end has already narrowed the candidate set via
    /// `pickStaticOverload`, but the chosen `SymbolKey` isn't carried on
    /// `TExpr.New` today, so codegen re-picks by arity here.
    let externalCtor (fullName: string) (tyArgs: SemType list) (argTypes: SemType list) : CtorRecipe voption =
        let candidates = symbols.TryLookupMembers(fullName, ".ctor")
        let arity = List.length argTypes

        let applicable =
            candidates
            |> Array.filter (fun m ->
                match m.Key with
                | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length = arity
                | _ -> false
            )

        match applicable with
        | [||] -> ValueNone
        | _ ->
            let chosen = applicable.[0]

            let argSigLen =
                match chosen.Key with
                | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length
                | _ -> 0

            match externalClassRef fullName with
            | ValueNone -> ValueNone
            | ValueSome tref ->
                let parent = externalTypeSpec tref (List.map zonk tyArgs)
                let typeArity = arityOfMetaName fullName
                let markers = [ for _ in 1..typeArity -> TypeVar() ]
                let markerRoots = markers |> List.map UnionFind.find
                let markerTys = markers |> List.map TyVar |> List.toArray
                let openSig = chosen.BuildSignature markerTys
                let rawParams, _ = decurryTy openSig

                let paramTys =
                    match rawParams with
                    | [ TyConst "unit" ] -> []
                    | [ TyTuple elems ] when argSigLen >= 2 && elems.Length = argSigLen -> EqArray.toList elems
                    | ps -> ps

                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = true)
                    .Parameters(
                        List.length paramTys,
                        (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                        (fun (pars: ParametersEncoder) ->
                            for p in paramTys do
                                encodeOpen markerRoots (pars.AddParameter().Type()) p
                        )
                    )

                let handle = toEntity (ctx.MemberRef(parent, ".ctor", s))

                ValueSome
                    {
                        Handle = handle
                        ArgCount = List.length paramTys
                    }

    /// Mint the `MemberRef` for one named field on a referenced-assembly
    /// record, instantiated at `args`. Returns the field handle plus its
    /// *substituted* declared type — `'T` substituted to the matching `args.[i]`
    /// — so a `FieldGet` knows the value type a subsequent encode expects.
    let externalRecordField
        (fullName: string)
        (args: SemType list)
        (fieldName: string)
        : (EntityHandle * SemType) voption =
        let arity = List.length args

        match externalRecordRef fullName arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            match fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | None -> ValueNone
            | Some field ->
                let parent = externalTypeSpec tref (List.map zonk args)
                let markers = [ for _ in 1..arity -> TypeVar() ]
                let markerRoots = markers |> List.map UnionFind.find
                let markerTys = markers |> List.map TyVar |> List.toArray
                let openFieldTy = field.BuildType markerTys

                let s = BlobBuilder()
                encodeOpen markerRoots (BlobEncoder(s).FieldSignature()) openFieldTy
                let handle = toEntity (ctx.MemberRef(parent, fieldName, s))

                // The substituted field type for the caller (used for the
                // subsequent local / encode step in a `FieldGet`).
                let substitutedTy = field.BuildType(List.toArray args)
                ValueSome(handle, substitutedTy)

    /// `MethodSpec` instantiating a generic static method (R3) — a call site
    /// (`fold<int,int>`) or a recursive self-call (`fold<!!0,!!1>`, the ambient set
    /// mapping its own typars). Each instantiation type encodes via `encodeType`.
    let staticFnMethodSpec (handle: EntityHandle) (instTypes: SemType list) : EntityHandle =
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(List.length instTypes)

        for t in instTypes do
            encodeType (specEnc.AddArgument()) (zonk t)

        toEntity (ctx.MethodSpec(handle, inst))

    let lastSegment (name: string) : string =
        let i = name.LastIndexOf '.'
        if i < 0 then name else name.Substring(i + 1)

    /// `PrintfFormat<!!0, TextWriter, Unit, Unit>` — the parameter type of the
    /// generic `PrintFormatLine<T>`, where the printer slot is method type
    /// parameter 0.
    let encodeFormatParam (te: SignatureTypeEncoder) : unit =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
        markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
        let g = te.GenericInstantiation(ePrintfFormat4.Value, 4, false)
        g.AddArgument().GenericMethodTypeParameter(0)
        g.AddArgument().Type(eTextWriter.Value, false)
        g.AddArgument().Type(eUnit.Value, false)
        g.AddArgument().Type(eUnit.Value, false)

    /// `printfn` → `call PrintfModule::PrintFormatLine<printer>(format)`. The
    /// `printer` (e.g. `int -> unit` for `"%d"`) is the *result* of the head's
    /// curried type `fnTy = PrintfFormat<…> -> printer`. The format object is
    /// already on the stack; the call pops it and pushes the `T` result.
    let emitPrintfn (fnTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine"

        let resultTy =
            match zonk fnTy with
            | TyFun(_, printer) -> printer
            | other -> failwithf "ClrProvider: printfn has non-function type %A" other

        // Member reference to the generic method definition:
        //   T PrintFormatLine<T>(PrintfFormat<T, TextWriter, Unit, Unit>)
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(genericParameterCount = 1, isInstanceMethod = false)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericMethodTypeParameter(0)),
                (fun (pars: ParametersEncoder) -> encodeFormatParam (pars.AddParameter().Type()))
            )

        let memberRef = ctx.MemberRef(ePrintfModule.Value, "PrintFormatLine", msig)

        // Method specification instantiating T = resultTy. The printer is an
        // FSharp.Core `FSharpFunc` (PrintFormatLine builds it), so its arrow
        // encodes to `FSharpFunc`, not `Vesper.Fun` — this cold path is FSharp.Core
        // interop until the printf engine lands (R9).
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
        encodeFSharpFunc (specEnc.AddArgument()) resultTy
        let spec = toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            Emit = fun il -> il.Encoder.Call spec
            ArgCount = 1
            Pushes = 1
        }

    /// `Vesper.Fun\`2<a, b>::Invoke(a) : b` for applying a function value of
    /// type `a -> b` to one argument (R1 / D3). The member ref is minted against
    /// the instantiated `TypeSpec`, with the signature written in terms of the
    /// parent's generic type parameters (`instance !1 Invoke(!0)`). `Fun` is an
    /// interface, so the dispatch stays `callvirt`.
    /// `Vesper.Fun\`2<a,b>::Invoke(!0) : !1` as a `MemberRef` token — applying a
    /// function value. Shared by `emitInvoke` (the general apply path) and
    /// `emitFold`'s inline folder application (R3).
    let funInvokeRef (a: SemType) (b: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFun2.Value, 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        let typeSpec = ctx.TypeSpec tsB

        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericTypeParameter(1)),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericTypeParameter(0))
            )

        toEntity (ctx.MemberRef(toEntity typeSpec, "Invoke", msig))

    let emitInvoke (funcTy: SemType) : CallRecipe =
        match funcTy with
        | TyFun(a, b) ->
            let invokeRef = funInvokeRef a b

            {
                // `callvirt`: SRM has no helper, so emit the opcode + token.
                Emit =
                    fun il ->
                        il.Encoder.OpCode ILOpCode.Callvirt
                        il.Encoder.Token invokeRef
                // Consumes the receiver func *and* the applied arg; pushes the
                // result. So depth adjusts by 1 - 2 = -1.
                ArgCount = 2
                Pushes = 1
            }
        | other -> failwithf "ClrProvider: cannot invoke non-function type: %A" other

    /// `FSharpFunc\`2<a, b>::Invoke(a) : b` — applying a value that is an
    /// FSharp.Core `FSharpFunc`, not a `Vesper.Fun`. R1 left exactly one such
    /// island: the cold printf printer returned by `PrintFormatLine` (the printf
    /// engine retargets it, handoff §R9). Same shape as `emitInvoke` but over
    /// `FSharpFunc`2`.
    let emitFSharpFuncInvoke (funcTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2.Invoke"

        match funcTy with
        | TyFun(a, b) ->
            let tsB = BlobBuilder()
            let te = BlobEncoder(tsB).TypeSpecificationSignature()
            let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
            encodeFSharpFunc (g.AddArgument()) a
            encodeFSharpFunc (g.AddArgument()) b
            let typeSpec = ctx.TypeSpec tsB

            let msig = BlobBuilder()

            BlobEncoder(msig)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    1,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericTypeParameter(1)),
                    (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericTypeParameter(0))
                )

            let invokeRef = toEntity (ctx.MemberRef(toEntity typeSpec, "Invoke", msig))

            {
                Emit =
                    fun il ->
                        il.Encoder.OpCode ILOpCode.Callvirt
                        il.Encoder.Token invokeRef
                ArgCount = 2
                Pushes = 1
            }
        | other -> failwithf "ClrProvider: cannot invoke non-function type: %A" other

    /// `new PrintfFormat<tyArgs>(string)`.
    let emitPrintfFormatCtor (tyArgs: SemType list) : CtorRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4 (.ctor)"
        // TypeSpec for the instantiated generic, used as the member-ref parent.
        // The first type arg is the printer — an FSharp.Core `FSharpFunc` (this is
        // the cold path: it flows straight into `PrintFormatLine`), so its arrows
        // encode to `FSharpFunc`, not `Vesper.Fun` (R1; printf engine = R9).
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(ePrintfFormat4.Value, List.length tyArgs, false)

        for a in tyArgs do
            encodeFSharpFunc (g.AddArgument()) a

        let typeSpec = ctx.TypeSpec tsB

        // Constructor signature: instance void (string).
        let ctorSig = BlobBuilder()

        BlobEncoder(ctorSig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
            )

        let ctorRef = ctx.MemberRef(toEntity typeSpec, ".ctor", ctorSig)

        {
            Handle = toEntity ctorRef
            ArgCount = 1
        }

    /// `FSharpList\`1<elem>` as a member-ref parent `TypeSpec` — the declaring
    /// type both list constructors hang off.
    let listTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeListOf te (fun arg -> encodeType arg elem)
        toEntity (ctx.TypeSpec tsB)

    /// `FSharpList\`1<elem>::Cons(!0, FSharpList\`1<!0>) : FSharpList\`1<!0>` —
    /// the public static cons-cell constructor (what the F# compiler itself
    /// emits for `1 :: rest`). The signature is written in terms of the
    /// declaring type's generic parameter `!0`; the instantiation rides the
    /// parent `TypeSpec`. Head + tail are already on the stack beneath.
    let emitListCons (elem: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Collections.FSharpList`1.Cons"
        let typeSpec = listTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> encodeListOf (ret.Type()) (fun a -> a.GenericTypeParameter(0))),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    encodeListOf (pars.AddParameter().Type()) (fun a -> a.GenericTypeParameter(0))
                )
            )

        let consRef = toEntity (ctx.MemberRef(typeSpec, "Cons", msig))

        {
            Emit = fun il -> il.Encoder.Call consRef
            ArgCount = 2
            Pushes = 1
        }

    /// `FSharpList\`1<elem>::get_Empty() : FSharpList\`1<!0>` — the empty
    /// singleton getter (`[]` / `Nil`; the parameterless `.ctor` is internal,
    /// so this is the only public empty path).
    let emitListNil (elem: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Collections.FSharpList`1.get_Empty"
        let typeSpec = listTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                0,
                (fun (ret: ReturnTypeEncoder) -> encodeListOf (ret.Type()) (fun a -> a.GenericTypeParameter(0))),
                (fun (_: ParametersEncoder) -> ())
            )

        let emptyRef = toEntity (ctx.MemberRef(typeSpec, "get_Empty", msig))

        {
            Emit = fun il -> il.Encoder.Call emptyRef
            ArgCount = 0
            Pushes = 1
        }

    // ---- Vesper cons-list recipes (R3) ----
    //
    // The Vesper list mirrors FSharpList's *use*, but targets
    // `Vesper.Collections.List\`1` in the compiled `Vesper.Core.dll` (no
    // FSharp.Core), and its empty case is the static factory `Nil()` rather than
    // FSharpList's `get_Empty` property. All member refs hang off a `List\`1<elem>`
    // `TypeSpec`, their signatures written in the type's own `!0`.

    /// `Vesper.Collections.List\`1<elem>` as a member-ref parent `TypeSpec`.
    let vesperListTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
        encodeType (g.AddArgument()) elem
        toEntity (ctx.TypeSpec tsB)

    /// Encode `List\`1<!0>` (the declaring type's own typar) into a signature slot —
    /// the return type of `Cons` / `Nil` / `get_Tail`.
    let encodeVesperListOfTypar (te: SignatureTypeEncoder) : unit =
        let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
        g.AddArgument().GenericTypeParameter(0)

    /// `List\`1<elem>::Cons(!0, List\`1<!0>) : List\`1<!0>` — the static cons
    /// factory our union emission produces (head + tail already on the stack).
    let emitVesperListCons (elem: SemType) : CallRecipe =
        let typeSpec = vesperListTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> encodeVesperListOfTypar (ret.Type())),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    encodeVesperListOfTypar (pars.AddParameter().Type())
                )
            )

        let consRef = toEntity (ctx.MemberRef(typeSpec, "Cons", msig))

        {
            Emit = fun il -> il.Encoder.Call consRef
            ArgCount = 2
            Pushes = 1
        }

    /// `List\`1<elem>::Nil() : List\`1<!0>` — the static empty factory.
    let emitVesperListNil (elem: SemType) : CallRecipe =
        let typeSpec = vesperListTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> encodeVesperListOfTypar (ret.Type())), (fun _ -> ()))

        let nilRef = toEntity (ctx.MemberRef(typeSpec, "Nil", msig))

        {
            Emit = fun il -> il.Encoder.Call nilRef
            ArgCount = 0
            Pushes = 1
        }

    // ---- Closure synthesis support (slice 5 / R1) ----

    /// `Vesper.Fun\`2<a, b>` as a `TypeSpec` `EntityHandle` — the interface a
    /// synthesised closure *implements* (R1 / D3). Same encoding as `encodeType`'s
    /// `TyFun` case; used both for the closure's `InterfaceImpl` row and as the
    /// `MemberRef` parent for an explicit `Invoke` override should one be needed.
    /// A closure now derives from `System.Object` (`ObjectType` / `ObjectCtorRef`),
    /// not `FSharpFunc`.
    let funInterfaceSpec (a: SemType) (b: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFun2.Value, 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        toEntity (ctx.TypeSpec tsB)

    /// `instance b Invoke(a)` — the closure's concrete `Invoke` override
    /// signature (the closure type is closed, so no `TypeSpec`-relative typars).
    let invokeSignature (a: SemType) (b: SemType) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) b),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) a)
            )

        msig

    /// `instance void .ctor(captures…)` — the closure ctor signature, one
    /// concrete parameter per captured value (in field order).
    let closureCtorSignature (captures: SemType list) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length captures,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for c in captures do
                        encodeType (pars.AddParameter().Type()) c
                )
            )

        msig

    /// A capture field's signature (`encodeType` of its stored type).
    let fieldSignature (ty: SemType) : BlobBuilder =
        let blob = BlobBuilder()
        let te = BlobEncoder(blob).FieldSignature()
        encodeType te ty
        blob

    /// `List.fold folder state xs` over the *Vesper* list — a `call` to the `fold`
    /// compiled into `Vesper.List.dll` (R3 deferred: public module-function
    /// compilation; it was emitted inline before this DLL hosted `fold`). Builds
    /// the external generic-static-method ref
    ///   `Vesper.Collections.ListModule::fold<!!0,!!1>
    ///        (Fun<!!0, Fun<!!1, !!0>>, !!0, List<!!1>) : !!0`
    /// then a `MethodSpec` instantiating `<'State, 'T>` recovered from the head type
    /// `fnTy = ('State -> 'T -> 'State) -> 'State -> Vesper.List<'T> -> 'State`.
    /// Folder, state, list are already on the stack (ArgCount = 3); the call leaves
    /// the `'State` result. The folder's arrow stays `Vesper.Fun` and the list its
    /// `Vesper.Collections.List`, so the recipe pins `Vesper.List` (+ `Vesper.Core`
    /// via the `Fun` instantiation), no FSharp.Core.
    let emitFold (fnTy: SemType) : CallRecipe =
        let elemTy, stateTy =
            match zonk fnTy with
            | TyFun(TyFun(state, TyFun(t, _)), _) -> t, state
            | other -> failwithf "ClrProvider: List.fold has unexpected type %A" other

        // The generic `fold` signature is encoded with two fresh ambient typars
        // (`'State` ⇒ `!!0`, `'T` ⇒ `!!1`): `encodeType` maps them — and the `Fun`
        // / `List` generic instances built over them — through `methodTyparLeaf`,
        // exactly as the producer side emits the method's own signature.
        let stateTv = TypeVar()
        let tTv = TypeVar()
        let sT = TyVar stateTv
        let eT = TyVar tTv
        let folderT = TyFun(sT, TyFun(eT, sT))
        let listT = TyRecord(vesperListName, EqArray.singleton eT)

        let foldSig =
            let saved = methodTyparRoots
            methodTyparRoots <- [ UnionFind.find stateTv; UnionFind.find tTv ]
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = 2, isInstanceMethod = false)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) sT),
                    (fun (pars: ParametersEncoder) ->
                        encodeType (pars.AddParameter().Type()) folderT
                        encodeType (pars.AddParameter().Type()) sT
                        encodeType (pars.AddParameter().Type()) listT
                    )
                )

            methodTyparRoots <- saved
            s

        let foldRef = ctx.MemberRef(eListModule.Value, "fold", foldSig)

        // `MethodSpec` instantiating the call site's concrete `<'State, 'T>`.
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(2)
        encodeType (specEnc.AddArgument()) stateTy
        encodeType (specEnc.AddArgument()) elemTy
        let foldSpec = toEntity (ctx.MethodSpec(toEntity foldRef, inst))

        {
            Emit = fun il -> il.Encoder.Call foldSpec
            ArgCount = 3
            Pushes = 1
        }

    /// Encode a method body's locals into a standalone local-variable
    /// signature, reusing `encodeType` for each slot.
    let encodeLocalSignature (locals: SemType list) : StandaloneSignatureHandle =
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeType (enc.AddVariable().Type()) (zonk t)

        ctx.AddStandaloneSignature blob

    /// Member refs + the `AppendFormatted<T>` factory for lowering a
    /// `TExpr.Format` to the `Vesper.Formatter` write-through handler. All four
    /// member kinds hang off the non-generic `Formatter` value type, so the
    /// parent is a plain `TypeRef` (no `TypeSpec`); the generic `AppendFormatted`
    /// is a member ref to the open generic method + a `MethodSpec` per hole.
    let buildFormatHandles () : FormatHandles =
        let ctorWriter =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Type(eTextWriter.Value, false)
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, ".ctor", s))

        let ctorString =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    2,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, ".ctor", s))

        let appendLiteral =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    1,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendLiteral", s))

        let flush =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            toEntity (ctx.MemberRef(eFormatter.Value, "Flush", s))

        let toStringAndClear =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().String()),
                    (fun (_: ParametersEncoder) -> ())
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "ToStringAndClear", s))

        // `instance void AppendBool(bool, int32)` / `AppendOctal(int32, int32)`
        // / `AppendUnsigned(uint32, int32)` — the dedicated handler members for
        // `%b` / `%o` / `%u` (no `AppendFormatted<T>(…, fmt)` shape). Each takes
        // (value, alignment); the walker always pushes the alignment (0 ⇒ none).
        let appendMember (name: string) (param0: SignatureTypeEncoder -> unit) : EntityHandle =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    2,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        param0 (pars.AddParameter().Type())
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, name, s))

        let appendBool = appendMember "AppendBool" (fun te -> te.Boolean())
        let appendOctal = appendMember "AppendOctal" (fun te -> te.Int32())
        let appendUnsigned = appendMember "AppendUnsigned" (fun te -> te.UInt32())

        // `instance void AppendZeroPaddedFloat(float64, string, int32)` — `%0w.pf`
        // (value, "F<prec>" body, field width). Distinct arity from `appendMember`
        // (three params, not (value, alignment)), so its signature is built here.
        let appendZeroPaddedFloat =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Double()
                        pars.AddParameter().Type().String()
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendZeroPaddedFloat", s))

        let consoleGetter (name: string) : EntityHandle =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().Type(eTextWriter.Value, false)),
                    (fun (_: ParametersEncoder) -> ())
                )

            toEntity (ctx.MemberRef(eConsole.Value, name, s))

        // `AppendFormatted<T>(value [, int alignment] [, string format])` — the
        // overload is selected by which optional params are present (alignment
        // before format, matching the C# declaration), then `<T>` is bound.
        let appendFormatted (ty: SemType, hasAlignment: bool, hasFormat: bool) : EntityHandle =
            let paramCount = 1 + (if hasAlignment then 1 else 0) + (if hasFormat then 1 else 0)
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = 1, isInstanceMethod = true)
                .Parameters(
                    paramCount,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().GenericMethodTypeParameter(0)

                        if hasAlignment then
                            pars.AddParameter().Type().Int32()

                        if hasFormat then
                            pars.AddParameter().Type().String()
                    )
                )

            let memberRef = ctx.MemberRef(eFormatter.Value, "AppendFormatted", s)
            let inst = BlobBuilder()
            let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
            encodeType (specEnc.AddArgument()) (zonk ty)
            toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            HandlerLocal = TyConst formatterTypeName
            CtorWriter = ctorWriter
            CtorString = ctorString
            AppendLiteral = appendLiteral
            Flush = flush
            ToStringAndClear = toStringAndClear
            ConsoleOut = consoleGetter "get_Out"
            ConsoleError = consoleGetter "get_Error"
            AppendFormatted = appendFormatted
            AppendBool = appendBool
            AppendOctal = appendOctal
            AppendUnsigned = appendUnsigned
            AppendZeroPaddedFloat = appendZeroPaddedFloat
        }

    // ---- Structural equality / hashing helpers (C-Eq1) ----

    /// `EqualityComparer\`1<elem>` as a member-ref parent `TypeSpec`.
    let equalityComparerTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eEqualityComparer1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk elem)
        toEntity (ctx.TypeSpec tsB)

    /// `static EqualityComparer\`1<!0> get_Default()` on `EqualityComparer\`1<elem>`.
    let equalityComparerDefault (elem: SemType) : EntityHandle =
        let parent = equalityComparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                0,
                (fun (ret: ReturnTypeEncoder) ->
                    let g = ret.Type().GenericInstantiation(eEqualityComparer1.Value, 1, false)
                    g.AddArgument().GenericTypeParameter(0)
                ),
                (fun (_: ParametersEncoder) -> ())
            )

        toEntity (ctx.MemberRef(parent, "get_Default", s))

    /// `instance bool Equals(!0, !0)` on `EqualityComparer\`1<elem>` — the
    /// abstract method `EqualityComparer<T>` declares (reached via `callvirt`).
    let equalityComparerEquals (elem: SemType) : EntityHandle =
        let parent = equalityComparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Boolean()),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    pars.AddParameter().Type().GenericTypeParameter(0)
                )
            )

        toEntity (ctx.MemberRef(parent, "Equals", s))

    /// `instance int32 GetHashCode(!0)` on `EqualityComparer\`1<elem>` — the
    /// `hash x` use-site's body (the same comparer family the DU triple hashes
    /// fields through, so `hash` and `=` agree by construction), reached via
    /// `callvirt`. There is no IL opcode for a structural hash, so unlike `=`/`+`
    /// this is a BCL call, not an `ILIntrinsic` (docs/operators-plan.md).
    let equalityComparerGetHashCode (elem: SemType) : EntityHandle =
        let parent = equalityComparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericTypeParameter(0))
            )

        toEntity (ctx.MemberRef(parent, "GetHashCode", s))

    /// `instance void System.HashCode::Add<T>(!!0)` as a `MethodSpec` over `<elem>`.
    let hashCodeAdd (elem: SemType) : EntityHandle =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = 1, isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericMethodTypeParameter(0))
            )

        let memberRef = ctx.MemberRef(eHashCode.Value, "Add", s)
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
        encodeType (specEnc.AddArgument()) (zonk elem)
        toEntity (ctx.MethodSpec(toEntity memberRef, inst))

    /// `instance int32 System.HashCode::ToHashCode()`.
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

    /// `System.IEquatable\`1<self>` as a `TypeSpec` `EntityHandle` — the union's
    /// `InterfaceImpl` row (C-Eq1). Same shape as `funInterfaceSpec`, one argument:
    /// the implementing union's own (monomorphic) `SemType`.
    let equatableInterfaceSpec (selfTy: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eEquatable1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk selfTy)
        toEntity (ctx.TypeSpec tsB)

    // ---- Structural comparison helpers (records-plan §B6) ----
    //
    // Sibling of the equality helpers above. `Comparer\`1<T>` is the comparison
    // analogue of `EqualityComparer\`1<T>`: `static Comparer<T> get_Default()`
    // returns the well-known dispatcher, and `instance int32 Compare(T, T)` is
    // the abstract method that routes to `IComparable<T>` when the type
    // implements it.

    /// `Comparer\`1<elem>` as a member-ref parent `TypeSpec`.
    let comparerTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eComparer1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk elem)
        toEntity (ctx.TypeSpec tsB)

    /// `static Comparer\`1<!0> get_Default()` on `Comparer\`1<elem>`.
    let comparerDefault (elem: SemType) : EntityHandle =
        let parent = comparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                0,
                (fun (ret: ReturnTypeEncoder) ->
                    let g = ret.Type().GenericInstantiation(eComparer1.Value, 1, false)
                    g.AddArgument().GenericTypeParameter(0)
                ),
                (fun (_: ParametersEncoder) -> ())
            )

        toEntity (ctx.MemberRef(parent, "get_Default", s))

    /// `instance int32 Compare(!0, !0)` on `Comparer\`1<elem>` — the abstract
    /// method `Comparer<T>` declares (reached via `callvirt`). Routes through
    /// `IComparable<T>` if the type implements it.
    let comparerCompare (elem: SemType) : EntityHandle =
        let parent = comparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    pars.AddParameter().Type().GenericTypeParameter(0)
                )
            )

        toEntity (ctx.MemberRef(parent, "Compare", s))

    /// `System.IComparable\`1<self>` as a `TypeSpec` `EntityHandle` — the
    /// type's `InterfaceImpl` row (records-plan §B6). Mirror of
    /// `equatableInterfaceSpec`.
    let comparableInterfaceSpec (selfTy: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eComparable1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk selfTy)
        toEntity (ctx.TypeSpec tsB)

    member _.ObjectType: EntityHandle = eObject.Value

    /// Member ref to `System.Object::.ctor()` for a union's base-ctor chain.
    member _.ObjectCtorRef: EntityHandle = eObjectCtor.Value

    /// Register a user type emitted into this assembly so `encodeType` can
    /// reference it (by its predicted `TypeDefinition` handle) before its row is
    /// added. See `userTypes`.
    member _.RegisterUserType(name: string, handle: EntityHandle) : unit = userTypes.[name] <- handle

    /// Register a *generic* union's shape (typar names + cases) so
    /// `GenericUnionMemberRef` can mint `MemberRef`s on its `TypeSpec` (P3d.4).
    /// `cases` is `(caseName, [(fieldMetaName, declTy)])`; call after
    /// `RegisterUserType` has recorded the type's predicted handle. A no-op for a
    /// monomorphic union (none is registered here — its `Def` tokens are used).
    member _.RegisterGenericUnion
        (name: string, typars: string list, cases: (string * (string * SemType) list) list)
        : unit =
        genericUnions.[name] <- (typars, cases)

    /// Register a *generic* record's shape (typar names + fields) so
    /// `GenericRecordMemberRef` can mint `MemberRef`s on its `TypeSpec`
    /// (records-plan §B2). `fields` is `(name, declTy)` in declaration order;
    /// call after `RegisterUserType`. A no-op for a monomorphic record.
    member _.RegisterGenericRecord(name: string, typars: string list, fields: (string * SemType) list) : unit =
        genericRecords.[name] <- (typars, fields)

    /// Register a *generic* class's shape (typar names + backing fields per
    /// primary-ctor parameter) so `UserGenericMemberRef(_, _, ClassMember _)`
    /// can mint `MemberRef`s on its `TypeSpec` (vesper-set-sprint-plan
    /// Phase 1 / B-1). `fields` is `(name, declTy)` in declaration order;
    /// call after `RegisterUserType`. A no-op for a monomorphic class.
    member _.RegisterGenericClass(name: string, typars: string list, fields: (string * SemType) list) : unit =
        genericClasses.[name] <- (typars, fields)

    /// `<field-type>` field signature for a generic union's case field, encoded
    /// in terms of the type's own generic parameters (`Head : 'T` ⇒ `!0`).
    member _.GenericFieldSignature(typars: string list, declTy: SemType) : BlobBuilder =
        let blob = BlobBuilder()
        encodeUnionType (typarIx typars) (BlobEncoder(blob).FieldSignature()) declTy
        blob

    /// `static <ret> <name>(<params…>)` — a generic union case's factory
    /// signature, encoded in terms of the type's own generic parameters
    /// (`static List<!0> Cons(!0, List<!0>)`).
    member _.GenericStaticMethodSignature(typars: string list, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let typeIx = typarIx typars
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeUnionType typeIx (pars.AddParameter().Type()) p
                )
            )

        s

    /// `instance void .ctor(field0, field1, …)` for a *generic* record's
    /// declared ctor (records-plan §B2). Parameters are the field types in
    /// declaration order, written in the record's own typars (`Box::.ctor(!0)`).
    /// Monomorphic records use `ClosureCtorSignature` — the parameters are
    /// already concrete, no typar map needed.
    member _.GenericRecordCtorSignature(typars: string list, paramTys: SemType list) : BlobBuilder =
        let typeIx = typarIx typars
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeUnionType typeIx (pars.AddParameter().Type()) p
                )
            )

        s

    /// `instance <ret> <name>(<params…>)` — a *generic* union augmentation
    /// member's signature (R2), encoded in terms of the type's own generic
    /// parameters (`instance !0 get_Head()` / `instance List<!0> get_Tail()`).
    /// The implicit `this` is the type's `!0` self (encoded by `isInstanceMethod`).
    member _.GenericInstanceMethodSignature(typars: string list, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let typeIx = typarIx typars
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeUnionType typeIx (pars.AddParameter().Type()) p
                )
            )

        s

    /// Encode a *generic* union member body's locals — a local typed in the
    /// declaring type's typar (`h : 'T`) encodes to that type's
    /// `GenericTypeParameter` (`!0`), since the member shares the type's generic
    /// context; concrete locals delegate to `encodeType` (R2). The executable
    /// `EncodeLocalSignature` (no typar leaf) is used for every monomorphic body.
    member _.EncodeGenericLocalSignature(typars: string list, locals: SemType list) : StandaloneSignatureHandle =
        let typeIx = typarIx typars
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeUnionType typeIx (enc.AddVariable().Type()) (zonk t)

        ctx.AddStandaloneSignature blob

    /// `instance void .ctor()` — a union's parameterless constructor signature.
    member _.NullaryCtorSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

        s

    /// `static void .cctor()` — the type-initialiser signature (no `this`, no
    /// params, void return). Used for a class's synthesised `static let` seeder
    /// (vesper-set-sprint-plan §1.8 / B-10).
    member _.CctorSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

        s

    /// Install the ambient generic-method-typar set (by union-find root) for the
    /// generic static method about to be emitted (R3), so `encodeType` / the
    /// generic-union arg encoder map those `TypeVar`s to `!!i`. `ClearMethodTypars`
    /// resets it (empty ⇒ monomorphic emission, the default everywhere else).
    member _.SetMethodTypars(typars: TypeVar list) : unit =
        methodTyparRoots <- typars |> List.map UnionFind.find

    member _.ClearMethodTypars() : unit = methodTyparRoots <- []

    /// Install the ambient *type*-typar set for a generic union's equality triple
    /// (S4), so `encodeType` maps a declaring-typar marker (`'T`) to that type's
    /// `GenericTypeParameter` (`!0`). The names carry the F# leading quote (as in
    /// `genericUnions`). `ClearTypeTypars` resets it (empty ⇒ unchanged emission).
    /// A monomorphic union passes `[]` here — a no-op map.
    member _.SetTypeTypars(typars: string list) : unit =
        typeTyparIx <- typars |> List.mapi (fun i n -> n, i) |> Map.ofList

    member _.ClearTypeTypars() : unit = typeTyparIx <- Map.empty

    /// A generic union's own instantiation `TypeSpec` over its declaring typars
    /// (`List\`1<!0>`) — the `isinst` target / `other`-local / typed-`Equals` self
    /// for its synthesised equality triple (S4). Registered in `genericUnions`.
    member _.GenericUnionSelfSpec(name: string) : EntityHandle =
        let typars, _ = genericUnions.[name]
        genericUnionTypeSpec name [ for t in typars -> TyConst t ]

    /// A generic record's own instantiation `TypeSpec` over its declaring typars
    /// (`Box\`1<!0>`) — the `isinst` target / `other`-local / typed-`Equals` self
    /// for its synthesised equality triple (records-plan §B4). Same role as
    /// `GenericUnionSelfSpec`.
    member _.GenericRecordSelfSpec(name: string) : EntityHandle =
        let typars, _ = genericRecords.[name]
        genericRecordTypeSpec name [ for t in typars -> TyConst t ]

    /// `<ret> <name><`n>(<params…>)` — a *generic* module-static-method signature
    /// (R3): the method declares `typarCount` generic parameters, and every typar
    /// `TypeVar` in `paramTys` / `retTy` encodes to `!!i` through the ambient set
    /// installed by `SetMethodTypars` (`fold<!!0,!!1>`).
    member _.GenericStaticFnSignature(typarCount: int, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = typarCount, isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    /// `static <ret> <name>(<params…>)` — a union case's factory signature
    /// (`static Lst Cons(int, Lst)`); each slot encoded via `encodeType`.
    member _.StaticMethodSignature(paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    /// `instance <ret> <name>(<params…>)` — a union augmentation member's
    /// signature (P3d.3); the implicit `this` is encoded by
    /// `isInstanceMethod = true`. A property getter is a parameterless instance
    /// method (`instance bool get_IsEmpty()`).
    member _.InstanceMethodSignature(paramTys: SemType list, retTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        s

    /// `Vesper.Fun\`2<a,b>` `TypeSpec` — the interface a closure implements (R1).
    /// Its `InterfaceImpl` row; the closure base is `ObjectType`.
    member _.FunInterfaceSpec(a: SemType, b: SemType) : EntityHandle = funInterfaceSpec a b

    /// `instance b Invoke(a)` signature for the closure's `Invoke` override.
    member _.InvokeSignature(a: SemType, b: SemType) : BlobBuilder = invokeSignature a b

    /// `instance void .ctor(captures…)` signature for the closure ctor.
    member _.ClosureCtorSignature(captures: SemType list) : BlobBuilder = closureCtorSignature captures

    /// Field signature for a captured value of type `ty`.
    member _.FieldSignature(ty: SemType) : BlobBuilder = fieldSignature ty

    // ---- Generic closure surface (function-representation-plan §Generic closures, C2) ----

    /// Register a *generic* closure's shape (typar roots, capture-field types,
    /// Invoke param + result types, predicted `TypeDefinition` handle) so
    /// `GenericClosureMemberRef` can mint `MemberRef`s on its `TypeSpec`. Called
    /// once per generic closure as its `TypeDefinition` is laid out (C3, in
    /// `Codegen.fs`'s closure loop), before any member ref against it. A
    /// monomorphic closure (`Closure.Typars = []`) is *not* registered here —
    /// its `Def` tokens are used directly, same as monomorphic unions / records.
    member _.RegisterClosure
        (
            name: string,
            typars: TypeVar list,
            captureSigs: SemType list,
            paramTy: SemType,
            resultTy: SemType,
            defHandle: EntityHandle
        ) : unit =
        genericClosures.[name] <-
            {
                TyparRoots = typars |> List.map UnionFind.find
                CaptureSigs = captureSigs
                ParamTy = paramTy
                ResultTy = resultTy
                DefHandle = defHandle
            }

    /// `<closure>$n<args>` `TypeSpec` for a generic closure
    /// (function-representation-plan §Generic closures, C2). The closure must
    /// have been registered with `RegisterClosure`.
    /// Each `args` arg is encoded through `encodeType` against the current
    /// ambient — `methodTyparLeaf` at a construction site (so a method typar
    /// resolves to `!!i`), `closureTyparLeaf` from inside the closure's own
    /// emission (`!i`).
    member _.GenericClosureTypeSpec(name: string, args: SemType list) : EntityHandle =
        genericClosureTypeSpec name (List.map zonk args)

    /// A `MemberRef` to one member (`Ctor`, `CaptureField i`, `Invoke`) of
    /// generic closure `name` instantiated at `args`. The signature is written
    /// in the closure's own typars (`!i`); the parent `TypeSpec` supplies the
    /// instantiation. C3 uses this for the construction-site `Newobj` and for
    /// the in-`Invoke` capture-field loads.
    member _.GenericClosureMemberRef(name: string, args: SemType list, which: ClosureMember) : EntityHandle =
        genericClosureMemberRef name (List.map zonk args) which

    /// A capture-field signature for a *generic* closure, encoded in the
    /// closure's own typars (`!i`). The mirror of `FieldSignature` for the
    /// monomorphic case; `closureTypars` are the closure's typar roots
    /// (`Closure.Typars`), installed for the duration of one `encodeType` call
    /// so a free `TyVar` in `ty` whose root is one of them resolves to that
    /// closure type's `GenericTypeParameter` via `closureTyparLeaf`.
    member _.GenericCaptureFieldSignature(closureTypars: TypeVar list, ty: SemType) : BlobBuilder =
        let savedClosure = closureTyparRoots
        let savedMethod = methodTyparRoots
        closureTyparRoots <- closureTypars |> List.map UnionFind.find
        methodTyparRoots <- []
        let blob = BlobBuilder()
        encodeType (BlobEncoder(blob).FieldSignature()) ty
        closureTyparRoots <- savedClosure
        methodTyparRoots <- savedMethod
        blob

    /// Install the ambient closure-typar set (by union-find root) around a
    /// generic closure's own ctor / Invoke / field-signature / locals emission
    /// (function-representation-plan §Generic closures, C2). While installed, `encodeType` maps a free `TyVar`
    /// whose root is in the set to the closure type's `GenericTypeParameter`
    /// (`!i`) via `closureTyparLeaf`. `ClearClosureTypars` resets it (empty ⇒
    /// the default everywhere outside a generic closure's emission).
    ///
    /// Invariant: at most one of `SetMethodTypars` / `SetTypeTypars` /
    /// `SetClosureTypars` may be active at a time (the three contexts are
    /// disjoint by phase — see `closureTyparRoots`'s notes).
    member _.SetClosureTypars(typars: TypeVar list) : unit =
        closureTyparRoots <- typars |> List.map UnionFind.find

    member _.ClearClosureTypars() : unit = closureTyparRoots <- []

    /// Encode an abstract interface-method signature leaf (the library path, G5).
    /// A typar marker (`TyConst "'A"`) resolves to a positional generic parameter —
    /// the method's own typars (`methodIx`) shadow the declaring type's
    /// (`typeIx`), so they are tried first — and every other (concrete) leaf is
    /// delegated to `encodeType`. So an abstract signature can reference `unit`, a
    /// nested function type, a primitive, etc.; concrete leaves that pin
    /// FSharp.Core mark the dependency, so the library path's
    /// `FSharpCoreDependencies` is accurate. Recurses through structural types
    /// (a higher-order `('A -> 'B) -> 'C` param) via `encodeTypeCore`, intercepting
    /// typars at every depth — not just at the top decurried params.
    member _.EncodeAbstractType
        (typeIx: Map<string, int>, methodIx: Map<string, int>, te: SignatureTypeEncoder, t: SemType)
        : unit =
        let tryLeaf (te: SignatureTypeEncoder) (zt: SemType) : bool =
            match zt with
            | TyConst name when methodIx.ContainsKey name ->
                te.GenericMethodTypeParameter(methodIx.[name])
                true
            | TyConst name when typeIx.ContainsKey name ->
                te.GenericTypeParameter(typeIx.[name])
                true
            | _ -> false

        encodeTypeCore tryLeaf te t

    // ---- Structural equality / hashing surface (C-Eq1) ----

    /// The `System.HashCode` accumulator local type for a union's `GetHashCode`.
    member _.HashCodeType: SemType = TyConst "System.HashCode"

    /// `EqualityComparer<T>.Default` getter for a field type `T`.
    member _.EqualityComparerDefault(elem: SemType) : EntityHandle = equalityComparerDefault elem

    /// `EqualityComparer<T>::Equals(T, T) : bool` for a field type `T`.
    member _.EqualityComparerEquals(elem: SemType) : EntityHandle = equalityComparerEquals elem

    /// `EqualityComparer<T>::GetHashCode(T) : int` for the `hash` use-site.
    member _.EqualityComparerGetHashCode(elem: SemType) : EntityHandle = equalityComparerGetHashCode elem

    /// `System.HashCode::Add<T>(T)` for a field/tag type `T`.
    member _.HashCodeAdd(elem: SemType) : EntityHandle = hashCodeAdd elem

    /// `System.HashCode::ToHashCode() : int`.
    member _.HashCodeToHashCode: EntityHandle = eHashCodeToHashCode.Value

    /// A registered user type's (predicted) `TypeDefinition` handle — the
    /// `isinst` / cast target for a monomorphic union's generated `Equals`.
    member _.UserTypeHandle(name: string) : EntityHandle = userTypes.[name]

    /// `override bool Equals(object)` signature. The parameter is the compact
    /// `ELEMENT_TYPE_OBJECT` encoding (`.Object()`), not `class System.Object` —
    /// `System.Object::Equals(object)` uses the compact form, and implicit
    /// override binding is by signature *blob* match, so the encodings must agree
    /// (else the method lands in a new vtable slot instead of overriding).
    member _.EqualsOverrideSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Boolean()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Object())
            )

        s

    /// `override int GetHashCode()` signature.
    member _.GetHashCodeOverrideSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()), (fun (_: ParametersEncoder) -> ()))

        s

    /// `System.IEquatable\`1<self>` `TypeSpec` for the union's `InterfaceImpl` row.
    member _.EquatableInterfaceSpec(selfTy: SemType) : EntityHandle = equatableInterfaceSpec selfTy

    /// `instance bool Equals(Self)` — the typed `IEquatable<Self>::Equals`
    /// signature. The parameter is the union's own `TypeDefinition` (via
    /// `encodeType`); implicit interface binding matches it to the instantiated
    /// `IEquatable<Self>::Equals(!0)`.
    member _.EqualsTypedSignature(selfTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Boolean()),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) (zonk selfTy))
            )

        s

    // ---- Structural comparison surface (records-plan §B6) ----

    /// `Comparer<T>.Default` getter for a field type `T`.
    member _.ComparerDefault(elem: SemType) : EntityHandle = comparerDefault elem

    /// `Comparer<T>::Compare(T, T) : int` for a field type `T`.
    member _.ComparerCompare(elem: SemType) : EntityHandle = comparerCompare elem

    /// `System.IComparable\`1<self>` `TypeSpec` for the type's `InterfaceImpl` row.
    member _.ComparableInterfaceSpec(selfTy: SemType) : EntityHandle = comparableInterfaceSpec selfTy

    /// `System.IComparable` (non-generic) `EntityHandle` for the type's second
    /// `InterfaceImpl` row. No instantiation needed — `IComparable` is plain.
    member _.IComparableType: EntityHandle = eComparable.Value

    /// `System.ArgumentException::.ctor(string)` — the `CompareTo(object)` body
    /// throws this when the argument is not of `Self`.
    member _.ArgumentExceptionCtor: EntityHandle = eArgumentExceptionCtor.Value

    /// `override int32 CompareTo(object)` signature. The parameter uses the
    /// compact `ELEMENT_TYPE_OBJECT` encoding (`.Object()`) to match how
    /// `IComparable::CompareTo(object)` is declared — implicit interface binding
    /// is signature-blob match, so the encodings must agree.
    member _.CompareToOverrideSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Object())
            )

        s

    /// `instance int32 CompareTo(Self)` — the typed
    /// `IComparable<Self>::CompareTo` signature. The parameter is the type's
    /// own `TypeDefinition` (via `encodeType`); implicit interface binding
    /// matches it to the instantiated `IComparable<Self>::CompareTo(!0)`.
    member _.CompareToTypedSignature(selfTy: SemType) : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) (zonk selfTy))
            )

        s

    interface ICodegenProvider with
        member _.ObjectType = eObject.Value
        member _.DecimalCtor = eDecimalCtor.Value
        member _.ExceptionCtor = eExceptionCtor.Value

        // The `hash x` use-site emits these from the expression walker (`Emit`),
        // which sees only the interface — unlike the DU-triple helpers, which
        // Codegen reaches on the concrete provider.
        member _.EqualityComparerDefault(elem) = equalityComparerDefault elem
        member _.EqualityComparerGetHashCode(elem) = equalityComparerGetHashCode elem

        // The identity bridge (P4): a frozen `TExpr.ExternalMember`'s `SymbolKey` →
        // its `MemberRef`, minted from the provider-resolved signature.
        member _.ExternalMemberRef(key, isProperty, isStatic, memberTy) =
            externalMemberRef key isProperty isStatic memberTy

        // Sorted for a deterministic, diff-friendly dependency list.
        member _.FSharpCoreDependencies() =
            fsharpCoreDeps |> List.ofSeq |> List.sort

        member _.TryEmitCall(compiledName, key, fnTy) =
            if compiledName = "List.fold" then
                ValueSome(emitFold (zonk fnTy))
            else
                // Dispatch by SymbolKey identity when Freeze stamped one
                // (vesper-set-sprint-plan §0.1 / M1): only the canonical
                // `Vesper.Printf.printfn` trips the cold-printf recipe, so a
                // user `module MyMod = let printfn x = x` followed by
                // `MyMod.printfn 1` falls through to the normal external-call
                // path (which then resolves `MyMod.printfn` as a regular
                // project-local function). The name-based fallback only fires
                // on bare `"printfn"` from unkeyed call sites (test mocks
                // resolving against `MockBuiltins`); a *qualified*
                // `MyMod.printfn` doesn't match it.
                let isCanonicalPrintfn =
                    match key with
                    | ValueSome k when PrintfSpec.isCanonicalPrintfn k -> true
                    | _ -> compiledName = "printfn"

                if isCanonicalPrintfn then
                    ValueSome(emitPrintfn (zonk fnTy))
                else
                    // Arithmetic / equality / comparison operators no longer reach here:
                    // `Emit.lower` expands them to `TExpr.ILIntrinsic` from their inline-IL
                    // bodies before emission (docs/operators-plan.md, C-Eq1).
                    ValueNone

        member _.TryEmitCtor(className, tyArgs, argTypes) =
            if className = PrintfSpec.printfFormatName then
                ValueSome(emitPrintfFormatCtor (List.map zonk tyArgs))
            else
                // External class ctor (`new System.Exception(msg)` from the
                // `failwith` inline body, …). The MetadataSymbols provider surfaces
                // `.ctor` overloads under that name; `externalCtor` picks one by
                // call-site arity and mints the `newobj` MemberRef.
                externalCtor className (List.map zonk tyArgs) (List.map zonk argTypes)

        member _.TryEmitUnionCons(typeName, caseName, tyArgs) =
            let elem () =
                match List.map zonk tyArgs with
                | [ e ] -> e
                | other -> failwithf "ClrProvider: list type expects one type argument, got %A" other

            if typeName = listTypeName then
                match caseName with
                | "Cons" -> ValueSome(emitListCons (elem ()))
                | "Nil" -> ValueSome(emitListNil (elem ()))
                | _ -> ValueNone
            elif isVesperListName typeName then
                // The Vesper cons-list (R3) — its `Cons` / `Nil` static factories in
                // the compiled `Vesper.Core.dll`; no FSharp.Core.
                match caseName with
                | "Cons" -> ValueSome(emitVesperListCons (elem ()))
                | "Nil" -> ValueSome(emitVesperListNil (elem ()))
                | _ -> ValueNone
            else
                // User-defined DU constructors flow through here too; minting
                // their types + ctors from the provider is a later slice.
                ValueNone

        // Single seam over the per-family helpers (vesper-set-sprint-plan
        // §0.3 / M3): dispatch by `kind`, hand off to the existing functions.
        // Phase 1 (B-1) added the `ClassMember` arm; the per-family helpers
        // (`genericClassMemberRef`) keep records, unions, closures and classes
        // independent.
        member _.UserGenericMemberRef(name, args, kind) =
            let zonkedArgs = List.map zonk args

            match kind with
            | UserMemberKind.UnionMember which -> genericUnionMemberRef name zonkedArgs which
            | UserMemberKind.RecordMember which -> genericRecordMemberRef name zonkedArgs which
            | UserMemberKind.ClosureMember which -> genericClosureMemberRef name zonkedArgs which
            | UserMemberKind.ClassMember which -> genericClassMemberRef name zonkedArgs which

        member _.TryEmitRecordCons(typeName, tyArgs, _fieldNames) =
            let zonkedArgs = List.map zonk tyArgs

            match externalRecordCtor typeName zonkedArgs with
            | ValueNone -> ValueNone
            | ValueSome handle ->
                let argCount =
                    match externalRecordShape typeName (List.length zonkedArgs) with
                    | ValueSome(fields, _) -> fields.Length
                    | ValueNone -> 0

                ValueSome { Handle = handle; ArgCount = argCount }

        member _.TryResolveExternalRecordField(typeName, tyArgs, fieldName) =
            externalRecordField typeName (List.map zonk tyArgs) fieldName

        member _.StaticFnMethodSpec(handle, instTypes) = staticFnMethodSpec handle instTypes

        member _.TryEmitInvoke(funcTy) =
            match zonk funcTy with
            | TyFun _ as ft -> ValueSome(emitInvoke ft)
            | _ -> ValueNone

        member _.TryEmitFSharpFuncInvoke(funcTy) =
            match zonk funcTy with
            | TyFun _ as ft -> ValueSome(emitFSharpFuncInvoke ft)
            | _ -> ValueNone

        member _.FormatHandles() = buildFormatHandles ()

        member _.EncodeLocalSignature(locals) = encodeLocalSignature locals
