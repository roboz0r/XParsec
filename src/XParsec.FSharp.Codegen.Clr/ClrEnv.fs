namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// Per-generic-closure registry entry: the typar *count* inherited from the enclosing static method,
/// the capture-field types in declaration order, the `Invoke` parameter / result types, and the
/// closure's predicted `TypeDefinition` handle. All `FrozenType` fields embed the enclosing method's
/// `FTTypar(Method, i)`; `ClrEnv.ClosureTyparMode` re-projects them onto the closure class's `!i`
/// during the closure's own emission.
type internal GenericClosureShape =
    {
        TyparCount: int
        /// The closure's declaring-typar offset (its first `DeclaringTypars`
        /// slots are the enclosing class's typars). `0` for a static-fn closure.
        /// Sets the `ClosureTyparScope` offset while encoding this closure's own
        /// member-ref signatures.
        DeclaringTypars: int
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
/// Reference identities are resolved by *simple name*: `references` (read off a file) wins;
/// `FSharp.Core` falls back to the host-loaded copy; `Vesper.Core` / `Vesper.List` / `Vesper.Printf`
/// are required (forcing one without its reference is a hard error). Every ref is `lazy` so the
/// metadata row is added only when first forced during emission — constructing the provider emits
/// nothing, and a PE whose IL never touches an assembly carries no `AssemblyRef` for it.
type internal ClrEnv
    (
        ctx: MetadataContext,
        reprs: Map<string, string>,
        references: Map<string, System.Reflection.AssemblyName>,
        symbols: ICodegenSymbols
    ) =

    let refOrHost
        (simpleName: string)
        (hostFallback: unit -> System.Reflection.AssemblyName)
        : System.Reflection.AssemblyName =
        match references.TryFind simpleName with
        | Some an -> an
        | None -> hostFallback ()

    let refRequired (simpleName: string) (need: string) : System.Reflection.AssemblyName =
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

    // The bootstrap BCL identity every emitted assembly needs (its module/holder
    // types extend `System.Object`; `ValueTuple` / `ValueType` / `Enum` /
    // `Exception` / `HashCode` ride it too). Sourced from the referenced
    // `System.Runtime` (a ref pack, threaded in as the driver's BCL surface) so the
    // `AssemblyRef` is the reference identity, not the host `System.Private.CoreLib`;
    // absent a reference (the host-TPA test conveniences), it falls back to the host
    // corlib. The ref assembly type-forwards these to the impl at run time.
    let coreRef =
        lazy
            (toEntity (ctx.AssemblyRef(refOrHost "System.Runtime" (fun () -> typeof<System.Object>.Assembly.GetName()))))

    // `Vesper.Printf` is a referenced package like `Vesper.Core` / `Vesper.List`: its
    // identity comes from the `ProjectInfo.References` path the caller wires (the
    // Vesper-compiled `Vesper.Printf.dll`). `lazy`, so only a happy-path `printf` / `%A`
    // program — one that forces `eFormatter` (`Vesper.Formatter`) — requires it; a
    // printf-free program adds no `Vesper.Printf` `AssemblyRef`. The former host-loaded
    // C# fallback (`typeof<Vesper.PrintfRuntime>`) is gone: the C# `Vesper.Printf.dll`
    // is off the backend's TPA, so a printf program with no `Vesper.Printf` reference
    // now fails to encode rather than silently binding the C# handler off the host.
    let vesperRef =
        lazy
            (toEntity (ctx.AssemblyRef(refRequired "Vesper.Printf" "a printf / %A format call needs Vesper.Formatter")))

    let consoleRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(refOrHost "System.Console" (fun () -> typeof<System.Console>.Assembly.GetName()))
            ))

    // The BCL-only `unit`: the zero-field `System.ValueTuple` struct (the repr
    // `prim-types-min.fs` binds `unit` to). The general `unit` type/value encodes
    // off this — no `FSharp.Core.Unit` is referenced anywhere in the backend.
    let eValueTuple =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ValueTuple")))

    // The open generic tuple structs `System.ValueTuple`1..`8`, cached by arity
    // (just the bare `TypeRef`; the per-call-site `.ctor` / `Item` refs are minted
    // on top by `ClrEncoder.ValueTupleRefs`). `unit` stays on `eValueTuple` above.
    // The family bottoms at `1` because `ValueTuple`1` arises as the `TRest` tail
    // of a ≥ 8 nesting; a *user-level* 1-tuple is still rejected by `ValueTupleRefs`.
    let valueTupleEntities = Dictionary<int, EntityHandle>()

    let eValueTupleN (arity: int) : EntityHandle =
        if arity < 1 || arity > 8 then
            failwithf
                "ClrProvider: ValueTuple arity %d is out of range — only the generic family `ValueTuple`1..`8` exists (≥9 nests via `ValueTuple`8`'s `TRest`)."
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

    // Forcing `eFun2` without a `Vesper.Core` reference is a hard error — `Fun` lives in Vesper.Core,
    // not this assembly and not FSharp.Core.
    let vesperCoreRef =
        lazy (toEntity (ctx.AssemblyRef(refRequired "Vesper.Core" "a function value needs Vesper.Fun")))

    let eFun2 = lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "Fun`2")))

    // The FLAT 2-arg function interface `Vesper.Fun`3<a,b,c>` a flat-2
    // value-struct closure implements (one `Invoke(a,b):c`). Sibling of `eFun2`
    // (the curried `Fun`2`) — same `Vesper.Fun` name, overloaded by generic arity.
    let eFlatFun = lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "Fun`3")))

    // The wider FLAT function interfaces a flat arity-3 / arity-4 value-struct
    // closure implements: `Vesper.Fun`4<a,b,c,r>` (one `Invoke(a,b,c):r`) and
    // `Vesper.Fun`5<a,b,c,d,r>` (one `Invoke(a,b,c,d):r`). Same `Vesper.Fun`
    // name, overloaded by generic arity — siblings of `eFlatFun` (`Fun`3`).
    let eFun4 = lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "Fun`4")))
    let eFun5 = lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "Fun`5")))

    // Select the flat function interface entity by GENERIC arity (its number of
    // type arguments = flat param count + 1): `3`⇒`Fun`3`, `4`⇒`Fun`4`,
    // `5`⇒`Fun`5`. A flat closure of param-arity N implements `Fun`(N+1)`.
    let flatFunEntity (genericArity: int) =
        match genericArity with
        | 3 -> eFlatFun
        | 4 -> eFun4
        | 5 -> eFun5
        | n -> failwithf "ClrEnv: no flat Fun interface for generic arity %d" n

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

    // `System.Enum` — the IL base type of a numeric enum's `TypeDefinition` (the
    // CLR reads the base chain to mark the type `IsEnum`, value-type by the further
    // `System.ValueType` base it itself extends).
    let eEnum = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Enum")))

    // `System.Runtime.CompilerServices.IsByRefLikeAttribute` — stamped on a
    // `[<IsByRefLike>]` value type so the CLR confines it to the stack.
    let eIsByRefLikeAttr =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Runtime.CompilerServices", "IsByRefLikeAttribute")))

    // Its parameterless `.ctor`, the constructor a `CustomAttribute` row names.
    let eIsByRefLikeAttrCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eIsByRefLikeAttr.Value, ".ctor", s)))

    let eTextWriter =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.IO", "TextWriter")))

    // `System.Text.StringBuilder` — the `bprintf` write-through sink (the leading
    // arg + the third `Formatter` ctor param). In the ref pack it lives in the same
    // core assembly as `TextWriter` (`System.Runtime`), so `coreRef`.
    let eStringBuilder =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Text", "StringBuilder")))

    let eConsole = lazy (toEntity (ctx.TypeRef(consoleRef.Value, "System", "Console")))

    let eFormatter =
        lazy (toEntity (ctx.TypeRef(vesperRef.Value, "Vesper", "Formatter")))

    // The `%A` structural-format interfaces. Owned by `Vesper.Core`: the synthesised
    // `Format` implements a Core-owned interface, so a record-bearing program links
    // only `Vesper.Core` — never `Vesper.Printf` (where only the layout *engine*,
    // `RuntimeFormatState`, lives, implementing this same Core `IFormatSink`).
    // `IStructuralFormattable` is the `InterfaceImpl` a synthesised record/DU declares;
    // `IFormatSink` is its `Format` param type. Both resolve against `vesperCoreRef`,
    // like `Vesper.Fun`.
    let eStructuralFormattable =
        lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "IStructuralFormattable")))

    let eFormatSink =
        lazy (toEntity (ctx.TypeRef(vesperCoreRef.Value, "Vesper", "IFormatSink")))

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

    // `System.NotSupportedException::.ctor()` — thrown by the synthesised
    // `IEnumerator.Reset` co-slot. The parameterless ctor: the BCL's own default message
    // ("Specified method is not supported.") is exactly right, and it keeps the shim's
    // body three instructions with no user string to mint.
    let eNotSupportedException =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "NotSupportedException")))

    let eNotSupportedExceptionCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eNotSupportedException.Value, ".ctor", s)))

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

    // `(typars, ctorParamCount, fields)`. `fields` is the *full* field shape —
    // ctor-param backing fields first, then `val` instance fields, then `static let`
    // backing fields — so a `ClassMember.Field` `MemberRef` resolves any of them by
    // name. `ctorParamCount` records how many leading entries are the *primary
    // ctor's* parameters, so the `ClassMember.Ctor` `MemberRef` signature uses only
    // those (not the `val`/`static let` fields, which a `Set<'T>(comparer, tree)`
    // self-construction in the `.cctor` would otherwise see as phantom ctor args).
    let genericClasses =
        Dictionary<SymbolKey, string list * int * (string * FrozenType) list>()
    // Closures have no `SymbolKey` (synthetic names), so they stay string-keyed —
    // the nominal seam is key-based, closures ride their own provider methods.
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
                    | None -> System.Reflection.AssemblyName(simpleName)

            toEntity (ctx.AssemblyRef an)

    // The single key→string funnel for *type-shape* lookups. A provider may key a
    // generic type bare (`Vesper.Option`, contract layer) or arity-suffixed
    // (`Vesper.Option`1`, metadata layer); `CodegenSymbols.lookupTypeByKey` is the *one*
    // place the two registration conventions are reconciled, replacing the per-lookup
    // `bareName` dual probe that used to leak into every shape consumer.
    let lookupTypeByKey (key: SymbolKey) : ExternalTypeShape voption =
        CodegenSymbols.lookupTypeByKey symbols key

    let lookupClassShape (key: SymbolKey) : ExternalClassShape voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Class info) -> ValueSome info
        | _ -> ValueNone

    /// The `TypeRef` for an external module's compiled holder type (an F# module compiles
    /// to a static class). The mirror of `externalClassRef`'s `typeRefOf`: a walk of the
    /// key's OWN holder chain, where a nested module chains through its parent's `TypeRef`
    /// with the bare name + empty namespace, exactly as a nested class does. The namespace
    /// is read off the chain's root — a module key carries its containment, so nothing here
    /// has to recover where the namespace ends and the module chain begins.
    ///
    /// `origin` is the RESOLVED SHAPE's home (the symbol's `SymbolOrigin`), the only place a
    /// physical location lives: a key names *what* a symbol is, never *where* it is, so the
    /// `AssemblyRef` that scopes the root `TypeRef` must be supplied by the caller that held
    /// the shape.
    let rec externalModuleRef (origin: SymbolOrigin) (m: ModuleKey) : EntityHandle =
        match m.Holder with
        | ModuleHolder.InModule parent -> toEntity (ctx.TypeRef(externalModuleRef origin parent, "", m.Name))
        | ModuleHolder.InNamespace ns -> toEntity (ctx.TypeRef(externalAsmRef origin.Assembly, ns.Dotted, m.Name))

    let rec externalClassRef (key: SymbolKey) : EntityHandle voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
            // A canonically-authored capability interface (`interface disposable`) has no
            // emitted type of its own — re-resolve through its platform face so the
            // InterfaceImpl row binds the real BCL interface (`System.IDisposable`). Analogous
            // to `exn → System.Exception`; the platform shape is a plain `Class`, so the
            // recursion terminates after one hop.
            externalClassRef (SymbolKeyOps.qualifiedTypeKey platform 0)
        | _ ->

            match key, lookupClassShape key with
            | SymbolKey.Type t, ValueSome info ->
                let asm = externalAsmRef info.Origin.Assembly

                // A nested type's `TypeRef` (`List`1+Enumerator`, the duck-typed struct
                // enumerator) must chain through the enclosing type's `TypeRef` as its
                // ResolutionScope with the *bare* nested name + empty namespace — a flat
                // `Outer+Inner` name with the AssemblyRef scope fails to bind
                // (`TypeLoadException`). The `+` is a reflection display convention, not a
                // metadata name. `TypeHolder.InType` IS that chain, so this walks the key's
                // own holders instead of re-parsing a `+`-mangled string. The generic args
                // ride the innermost nested TypeRef, so the encoder needs no further
                // nesting awareness.
                //
                // Each row's name column is that SEGMENT's metadata spelling — its own name
                // plus its own `` `N `` (`typeSegmentName`), which is exactly what the CLR
                // rule says and what `declTypeKey` parsed the key out of.
                let rec typeRefOf (t: TypeKey) : EntityHandle =
                    match t.Holder with
                    | TypeHolder.InType outer ->
                        toEntity (ctx.TypeRef(typeRefOf outer, "", SymbolKeyOps.typeSegmentName t))
                    | TypeHolder.InNamespace ns ->
                        toEntity (ctx.TypeRef(asm, ns.Dotted, SymbolKeyOps.typeSegmentName t))
                    | TypeHolder.InModule m ->
                        // A module-held type compiles to a type NESTED in the module's holder
                        // type, so its `TypeRef` must chain through `externalModuleRef` — not
                        // fall back to the namespace, which would silently drop `m` and emit a
                        // ref that does not bind. No producer mints this holder yet; when one
                        // does, this is the site that must be wired, so it fails loud.
                        failwithf
                            "ClrEnv: module-held external type has no TypeRef encoding yet: %s in %s"
                            t.Name
                            m.Name

                ValueSome(typeRefOf t)
            | _ -> ValueNone

    /// Whether a referenced-assembly type is a .NET value type (`struct`) — `false`
    /// for every reference type and for any name the provider can't resolve as a
    /// class. Drives the `VALUETYPE` vs `CLASS` element tag in `encodeType` and the
    /// value-receiver dispatch for the duck-typed struct enumerator
    /// (`List`1+Enumerator`).
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
        // A non-type key names no type, so it mints no `TypeRef`: `ValueNone` hands the
        // caller its hard error. It must NOT fall back to a fabricated `(ns = "", name =
        // <whole dotted name>)` ref, which binds to nothing and fails at load, not here.
        match key, externalRecordShape key arity with
        | SymbolKey.Type t, ValueSome(fields, origin) ->
            // Namespace + simple name come off the KEY's own containment chain (a record is
            // never a CLR nested type). A metadata `TypeRef` name carries the `` `n `` arity
            // suffix, which the key renders from its own `Arity`.
            let simple = SymbolKeyOps.typeSegmentName t

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Assembly, t.Namespace.Dotted, simple)), fields)
        | _ -> ValueNone

    /// Referenced-assembly union shape by `SymbolKey` + arity — the mirror of
    /// `externalRecordShape` for cross-package case construction (`Some` / `None`).
    /// The bare-vs-arity-suffixed registration split is reconciled once inside `lookupTypeByKey`.
    let externalUnionShape (key: SymbolKey) (arity: int) : (ExternalCaseShape[] * SymbolOrigin) voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Union(a, cases, _, origin)) when a = arity && origin.Assembly.IsSome ->
            ValueSome(cases, origin)
        | _ -> ValueNone

    let externalUnionRef (key: SymbolKey) (arity: int) : (EntityHandle * ExternalCaseShape[]) voption =
        match key, externalUnionShape key arity with
        | SymbolKey.Type t, ValueSome(cases, origin) ->
            let simple = SymbolKeyOps.typeSegmentName t

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Assembly, t.Namespace.Dotted, simple)), cases)
        | _ -> ValueNone

    // While encoding a closure's own members (Invoke / .ctor /
    // capture fields / its TypeSpec from inside its body), the enclosing method's
    // `TyTypar(Method, i)` are the closure *class*'s generic parameters, so they
    // encode as `GenericTypeParameter i` rather than `GenericMethodTypeParameter i`.
    // Closure-typar scope: `ValueNone` ⇒ off (a `FTTypar(Method, i)` encodes to
    // the method's own `!!i`); `ValueSome d` ⇒ inside a closure's own emission,
    // where the closure re-projects the enclosing context's typars onto its own
    // class typars. `d` is the *declaring-typar offset*: the enclosing class
    // typars occupy the closure's first `d` slots (a `FTTypar(Declaring, i)`
    // already encodes `!i`), so a member's `FTTypar(Method, j)` lands at
    // `!(d + j)`. A static-fn closure has `d = 0`, so `Method j → !j` (the old
    // behaviour).
    let mutable closureTyparScope: int voption = ValueNone

    let rec decurryTy (t: FrozenType) : FrozenType list * FrozenType =
        match t with
        | FTFun(a, b) ->
            let ps, r = decurryTy b
            a :: ps, r
        | other -> [], other

    member _.Ctx = ctx
    member _.References = references
    member _.Symbols: ICodegenSymbols = symbols

    /// Resolve a Vesper primitive's canon `SymbolKey` to its IL representation string,
    /// single-sourced from the `.fs` `(# … #)`: (1) this unit's OWN intrinsics (`reprs`
    /// — the `.fs` being compiled, keyed by the bare `simpleName`), then (2) the
    /// provider's harvested forward `{ canon -> platform }` map (the dependency closure),
    /// keyed by the qualified canon `SymbolKey` directly. No hard-coded fallback. Codegen
    /// carries the open-resolved canon key on its `FTConst` node (opens are a name-
    /// resolution concern, already discharged), so the forward lookup is the key itself —
    /// no string round-trip. (NOT `TryLookupType`, which is keyed by qualified compiled
    /// name.)
    member _.TryPrimitiveRepr(key: SymbolKey) : string option =
        match reprs.TryFind(SymbolKeyOps.simpleName key) with
        | Some _ as hit -> hit
        | None ->
            match symbols.IntrinsicForwardRepr.TryGetValue key with
            | true, repr -> Some repr
            | _ -> None

    member _.FsCoreRef = fsCoreRef
    member _.CoreRef = coreRef
    member _.VesperRef = vesperRef
    member _.ConsoleRef = consoleRef
    member _.EValueTuple = eValueTuple
    member _.EValueTupleN arity = eValueTupleN arity
    member _.EPrintfFormat4 = ePrintfFormat4
    member _.VesperCoreRef = vesperCoreRef
    member _.EFun2 = eFun2
    member _.EFlatFun = eFlatFun
    member _.FlatFunEntity(genericArity: int) = flatFunEntity genericArity
    member _.VesperListRef = vesperListRef
    member _.EFSharpList1 = eFSharpList1
    member _.EVesperList1 = eVesperList1
    member _.EListModule = eListModule
    member _.EObject = eObject
    member _.EValueType = eValueType
    member _.EEnum = eEnum
    member _.EIsByRefLikeAttrCtor = eIsByRefLikeAttrCtor
    member _.ETextWriter = eTextWriter
    member _.EStringBuilder = eStringBuilder
    member _.EConsole = eConsole
    member _.EFormatter = eFormatter
    member _.EStructuralFormattable = eStructuralFormattable
    member _.EFormatSink = eFormatSink
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
    member _.ENotSupportedExceptionCtor = eNotSupportedExceptionCtor
    member _.EDecimalCtor = eDecimalCtor
    member _.EHashCodeToHashCode = eHashCodeToHashCode

    member _.FormatterTypeName = formatterTypeName

    member _.UserTypes = userTypes
    member _.UserValueTypes = userValueTypes
    member _.GenericUnions = genericUnions
    member _.GenericRecords = genericRecords
    member _.GenericClasses = genericClasses
    member _.GenericClosures = genericClosures

    member _.MarkFSharpCoreDep construct = markFSharpCoreDep construct

    member _.FSharpCoreDependencies() =
        fsharpCoreDeps |> List.ofSeq |> List.sort

    member _.ClosureTyparScope
        with get () = closureTyparScope
        and set v = closureTyparScope <- v

    member _.ArityOfMetaName name = arityOfMetaName name
    member _.DecurryTy t = decurryTy t

    member _.ExternalAsmRef asm = externalAsmRef asm
    member _.ExternalModuleRef(origin: SymbolOrigin, m: ModuleKey) = externalModuleRef origin m
    member _.ExternalClassRef key = externalClassRef key
    member _.LookupTypeByKey key = lookupTypeByKey key
    member _.ExternalIsValueType key = externalIsValueType key
    member _.ExternalRecordShape(key, arity) = externalRecordShape key arity
    member _.ExternalRecordRef(key, arity) = externalRecordRef key arity
    member _.ExternalUnionShape(key, arity) = externalUnionShape key arity
    member _.ExternalUnionRef(key, arity) = externalUnionRef key arity
