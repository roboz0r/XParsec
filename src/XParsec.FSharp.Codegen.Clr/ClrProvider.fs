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
type ClrProvider(ctx: MetadataContext, reprs: Map<string, string>, references: Map<string, AssemblyName>) =

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

    // BCL type references.
    let eObject = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Object")))

    let eTextWriter =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.IO", "TextWriter")))

    let eConsole = lazy (toEntity (ctx.TypeRef(consoleRef.Value, "System", "Console")))

    let eFormatter =
        lazy (toEntity (ctx.TypeRef(vesperRef.Value, "Vesper", "Formatter")))

    let eDecimal = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Decimal")))

    let eException = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Exception")))

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
        | TyTuple items -> TyTuple(List.map zonk items)
        | TyRecord(n, args) -> TyRecord(n, List.map zonk args)
        | TyUnion(n, args) -> TyUnion(n, List.map zonk args)
        | TyClass(n, args) -> TyClass(n, List.map zonk args)
        | TyConst _ -> t

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
                let g = te.GenericInstantiation(ePrintfFormat4.Value, List.length args, false)

                for a in args do
                    encodeTypeCore tryLeaf (g.AddArgument()) a
            | TyRecord(name, [ elem ]) when name = listTypeName ->
                // `list<elem>` ≡ `FSharpList\`1<elem>`. Serves every list-typed
                // slot: the `%A` printer's `FSharpFunc` arg, the `PrintfFormat`
                // ctor / `PrintFormatLine` / `Invoke` instantiations, and any
                // list-typed local signature.
                encodeListOf te (fun arg -> encodeTypeCore tryLeaf arg elem)
            | TyRecord(name, [ elem ]) when name = vesperListName ->
                // The Vesper cons-list (R3) ≡ `Vesper.Collections.List\`1<elem>` in
                // the compiled `Vesper.Core.dll`. No FSharp.Core dep — it lives next
                // to `Fun`.
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
                match args with
                | [] -> te.Type(userTypes.[name], false)
                | _ ->
                    let g = te.GenericInstantiation(userTypes.[name], List.length args, false)

                    for a in args do
                        encodeTypeCore tryLeaf (g.AddArgument()) a
            | other -> failwithf "ClrProvider: cannot encode SemType: %A" other

    /// Encode a (zonked) `SemType` for the executable path. The only leaf hook is
    /// the ambient generic-method-typar resolver (`!!i`), which is empty except
    /// while a generic static method (`List.fold`) is being emitted (R3) — so
    /// every monomorphic emission is unchanged.
    and encodeType (te: SignatureTypeEncoder) (t: SemType) : unit = encodeTypeCore methodTyparLeaf te t

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
            let retTy = TyUnion(name, [ for t in typars -> TyConst t ])
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

    /// Bare CIL arithmetic intrinsic (`add` / `sub` / `mul`): pops two
    /// operands, pushes one result, references no metadata.
    let arithmetic (opCode: ILOpCode) : CallRecipe =
        {
            Emit = fun il -> il.Encoder.OpCode opCode
            ArgCount = 2
            Pushes = 1
        }

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
        let listT = TyRecord(vesperListName, [ eT ])

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

    /// Install the ambient generic-method-typar set (by union-find root) for the
    /// generic static method about to be emitted (R3), so `encodeType` / the
    /// generic-union arg encoder map those `TypeVar`s to `!!i`. `ClearMethodTypars`
    /// resets it (empty ⇒ monomorphic emission, the default everywhere else).
    member _.SetMethodTypars(typars: TypeVar list) : unit =
        methodTyparRoots <- typars |> List.map UnionFind.find

    member _.ClearMethodTypars() : unit = methodTyparRoots <- []

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

    interface ICodegenProvider with
        member _.ObjectType = eObject.Value
        member _.DecimalCtor = eDecimalCtor.Value
        member _.ExceptionCtor = eExceptionCtor.Value

        // Sorted for a deterministic, diff-friendly dependency list.
        member _.FSharpCoreDependencies() =
            fsharpCoreDeps |> List.ofSeq |> List.sort

        member _.TryEmitCall(compiledName, fnTy) =
            if compiledName = "List.fold" then
                ValueSome(emitFold (zonk fnTy))
            else
                match lastSegment compiledName with
                | "printfn" -> ValueSome(emitPrintfn (zonk fnTy))
                | "op_Addition" -> ValueSome(arithmetic ILOpCode.Add)
                | "op_Subtraction" -> ValueSome(arithmetic ILOpCode.Sub)
                | "op_Multiply" -> ValueSome(arithmetic ILOpCode.Mul)
                | _ -> ValueNone

        member _.TryEmitCtor(className, tyArgs) =
            if className = PrintfSpec.printfFormatName then
                ValueSome(emitPrintfFormatCtor (List.map zonk tyArgs))
            else
                ValueNone

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
            elif typeName = vesperListName then
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

        member _.GenericUnionMemberRef(name, args, which) =
            genericUnionMemberRef name (List.map zonk args) which

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
