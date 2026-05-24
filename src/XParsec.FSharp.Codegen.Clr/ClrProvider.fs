namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
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

/// `ICodegenProvider` over the BCL + the loaded `FSharp.Core.dll`. `reprs` is
/// the Vesper-primitive-name → IL-representation map (`IntrinsicRepr.merge` of a
/// file's intrinsic bindings over the built-in defaults); `encodeType` keys the
/// emitted IL type off the representation string (G7).
type ClrProvider(ctx: MetadataContext, reprs: Map<string, string>) =

    // Reference identities from the live assemblies (version-proof). Every
    // `AssemblyRef` / `TypeRef` / `MemberRef` below is `lazy` (G6): the row is
    // added — through `ctx`, which caches it — only when a ref is first *forced*
    // (`.Value`) during emission, not at construction. So merely constructing the
    // provider emits no metadata. An executable whose IL never touches FSharp.Core
    // carries no FSharp.Core `AssemblyRef`, and the library path can construct a
    // provider for a typar-only interface without pinning any assembly at all
    // (which is what lets `assembleLibrary` reuse `encodeType` — G5 — instead of
    // its old provider-free encoder).
    let fsCoreRef =
        lazy (toEntity (ctx.AssemblyRef(typeof<Microsoft.FSharp.Core.Unit>.Assembly.GetName())))

    let coreRef =
        lazy (toEntity (ctx.AssemblyRef(typeof<System.Object>.Assembly.GetName())))

    // The bootstrap printf runtime (Vesper.Printf.dll) hosting `Formatter`; and
    // the assembly that owns `System.Console` (its own ref assembly, not
    // CoreLib — type-forwarded at runtime). Both read live, like FSharp.Core.
    let vesperRef =
        lazy (toEntity (ctx.AssemblyRef(typeof<Vesper.PrintfRuntime>.Assembly.GetName())))

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

    let eFSharpList1 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Collections", "FSharpList`1")))

    let eListModule =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Collections", "ListModule")))

    /// The abbreviation name the list-literal freeze hard-codes
    /// ([front-end-gaps-plan](../XParsec.FSharp.SemanticAnalysis/docs/front-end-gaps-plan.md)
    /// §A). Both `encodeType` and `TryEmitUnionCons` key on it; when the
    /// extractor mints `FSharpList` properly this becomes a registered
    /// `TyUnion` and the match keys on that instead — additive.
    let listTypeName = "Microsoft.FSharp.Collections.list"

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
                // `a -> b` is `FSharpFunc\`2<a, b>` at the metadata level.
                markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2"
                let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
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

    /// Encode a (zonked) `SemType` for the executable path — no typar markers, so
    /// the leaf hook is a no-op.
    and encodeType (te: SignatureTypeEncoder) (t: SemType) : unit = encodeTypeCore (fun _ _ -> false) te t

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
            | _ -> false

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

        // Method specification instantiating T = resultTy.
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
        encodeType (specEnc.AddArgument()) resultTy
        let spec = toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            Emit = fun il -> il.Encoder.Call spec
            ArgCount = 1
            Pushes = 1
        }

    /// `FSharpFunc\`2<a, b>::Invoke(a) : b` for applying a function value of
    /// type `a -> b` to one argument. The member ref is minted against the
    /// instantiated `TypeSpec`, with the signature written in terms of the
    /// parent's generic type parameters (`instance !1 Invoke(!0)`).
    let emitInvoke (funcTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2.Invoke"

        match funcTy with
        | TyFun(a, b) ->
            let tsB = BlobBuilder()
            let te = BlobEncoder(tsB).TypeSpecificationSignature()
            let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
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

            let invokeRef = toEntity (ctx.MemberRef(toEntity typeSpec, "Invoke", msig))

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
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(ePrintfFormat4.Value, List.length tyArgs, false)

        for a in tyArgs do
            encodeType (g.AddArgument()) a

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

    // ---- Closure synthesis support (slice 5) ----

    /// `FSharpFunc\`2<a, b>` as a `TypeSpec` `EntityHandle` — a synthesised
    /// closure's base type. Same encoding as `encodeType`'s `TyFun` case.
    let fsharpFunc2Spec (a: SemType) (b: SemType) : EntityHandle =
        markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2 (closure base)"
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        toEntity (ctx.TypeSpec tsB)

    /// Member ref to the protected parameterless `FSharpFunc\`2<a,b>::.ctor()`
    /// a closure's own ctor chains to.
    let fsharpFuncCtorRef (a: SemType) (b: SemType) : EntityHandle =
        let parent = fsharpFunc2Spec a b
        let ctorSig = BlobBuilder()

        BlobEncoder(ctorSig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

        toEntity (ctx.MemberRef(parent, ".ctor", ctorSig))

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

    /// `List.fold (+) state xs` → `call ListModule::Fold<'T,'State>(folder,
    /// state, list)`. The 2-typar generic method's `'T` / `'State` are read off
    /// the folder parameter of the head's type `fnTy = ('State -> 'T -> 'State)
    /// -> 'State -> 'T list -> 'State`; the member ref is the generic def and a
    /// `MethodSpec` instantiates it. Folder, state, list are already on the
    /// stack beneath; the call pushes the `'State` result.
    let emitFold (fnTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Collections.ListModule.Fold"

        let elemTy, stateTy =
            match zonk fnTy with
            | TyFun(TyFun(state, TyFun(t, _)), _) -> t, state
            | other -> failwithf "ClrProvider: List.fold has unexpected type %A" other

        // Generic method def signature:
        //   !!1 Fold<'T,'State>(FSharpFunc`2<!!1, FSharpFunc`2<!!0,!!1>>, !!1,
        //                       FSharpList`1<!!0>)   (!!0 = 'T, !!1 = 'State)
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(genericParameterCount = 2, isInstanceMethod = false)
            .Parameters(
                3,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericMethodTypeParameter(1)),
                (fun (pars: ParametersEncoder) ->
                    let folder = pars.AddParameter().Type()
                    let g = folder.GenericInstantiation(eFSharpFunc2.Value, 2, false)
                    g.AddArgument().GenericMethodTypeParameter(1)
                    let inner = g.AddArgument().GenericInstantiation(eFSharpFunc2.Value, 2, false)
                    inner.AddArgument().GenericMethodTypeParameter(0)
                    inner.AddArgument().GenericMethodTypeParameter(1)
                    pars.AddParameter().Type().GenericMethodTypeParameter(1)
                    encodeListOf (pars.AddParameter().Type()) (fun a -> a.GenericMethodTypeParameter(0))
                )
            )

        let memberRef = ctx.MemberRef(eListModule.Value, "Fold", msig)

        // MethodSpec instantiating <'T = elemTy, 'State = stateTy>.
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(2)
        encodeType (specEnc.AddArgument()) elemTy
        encodeType (specEnc.AddArgument()) stateTy
        let spec = toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            Emit = fun il -> il.Encoder.Call spec
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

    /// `instance void .ctor()` — a union's parameterless constructor signature.
    member _.NullaryCtorSignature() : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

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

    /// `FSharpFunc\`2<a,b>` `TypeSpec` for a closure's base type.
    member _.ClosureBaseSpec(a: SemType, b: SemType) : EntityHandle = fsharpFunc2Spec a b

    /// Member ref to `FSharpFunc\`2<a,b>::.ctor()` for the closure ctor chain.
    member _.FSharpFuncCtorRef(a: SemType, b: SemType) : EntityHandle = fsharpFuncCtorRef a b

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
            if typeName = listTypeName then
                let elem =
                    match List.map zonk tyArgs with
                    | [ e ] -> e
                    | other -> failwithf "ClrProvider: list type expects one type argument, got %A" other

                match caseName with
                | "Cons" -> ValueSome(emitListCons elem)
                | "Nil" -> ValueSome(emitListNil elem)
                | _ -> ValueNone
            else
                // User-defined DU constructors flow through here too; minting
                // their types + ctors from the provider is a later slice.
                ValueNone

        member _.GenericUnionMemberRef(name, args, which) =
            genericUnionMemberRef name (List.map zonk args) which

        member _.TryEmitInvoke(funcTy) =
            match zonk funcTy with
            | TyFun _ as ft -> ValueSome(emitInvoke ft)
            | _ -> ValueNone

        member _.FormatHandles() = buildFormatHandles ()

        member _.EncodeLocalSignature(locals) = encodeLocalSignature locals
