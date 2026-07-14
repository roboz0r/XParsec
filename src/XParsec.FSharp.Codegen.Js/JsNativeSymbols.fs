namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Layer-2 provider supplying the core JS runtime types the backend references.
/// Hand-authored stubs; the seam where a `tsc`-derived metadata format will plug in.
///
/// The referenced runtime type today is `Error` — the native type `exn`'s
/// `(# "Error" #)` repr. `exnReprOf` resolves `exn`'s intrinsic repr through this
/// provider to the `Error` class, so every `exn` subtype lowers to `new Error(message)`.
/// The `message` property is what an `exn`-typed receiver's member access resolves to.
module JsNativeSymbols =

    /// The synthetic home "assembly" the stub types report — a label, not a real reference.
    [<Literal>]
    let private RuntimeAssembly = "Vesper.Js.Runtime"

    /// The home every stub shape reports. It rides the SHAPE's `SymbolOrigin`, never the
    /// key: a key is a nominal identity and carries no assembly.
    let private runtimeHome: Origin = Origin.InAssembly(AssemblyName RuntimeAssembly)

    let private errorOrigin: SymbolOrigin =
        {
            Home = runtimeHome
            Namespace = SymbolKeyOps.namespaceKey ""
        }

    /// `Error` is global, so its compiled / lookup name is the bare `Error`.
    let private errorTypeKey: TypeKey = SymbolKeyOps.typeKeyOf "" "Error"

    let private errorKey: SymbolKey = SymbolKey.Type errorTypeKey

    let private errorTy: FrozenType = FTClass(errorKey, EqArray.empty)

    let private stringTy: FrozenType = FTConst(RuntimeNames.stringKey, EqArray.empty)

    let private unitTy: FrozenType = FTConst(RuntimeNames.unitKey, EqArray.empty)

    /// `new Error(message: string)` — the JS `Error` constructor as an `ExternalMember`.
    let private errorCtor: ExternalMember =
        ExternalMember.ctor
            errorTypeKey
            (ExternalSignature.make (0, 0, stringTy, errorTy))
            (EqArray.singleton "string")
            errorOrigin
            []

    /// `Error.prototype.message : string`.
    let private errorMessage: ExternalMember =
        { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf errorTypeKey "message" EqArray.empty MemberKind.Property) with
            Storage = MemberStorage.Property
            Signature = ExternalSignature.make (0, 0, unitTy, stringTy)
            Origin = errorOrigin
        }

    let private errorShape: ExternalTypeShape =
        ExternalTypeShape.Class
            {
                Arity = 0
                IsInterface = false
                Members = [| errorCtor; errorMessage |]
                FrozenInterfaces = [||]
                FrozenBaseType = ValueNone
                Flags = ExternalClassFlags.Default
                Origin = errorOrigin
            }

    // The capability interfaces (`System.IDisposable` / `IEquatable\`1` / `IComparable\`1`)
    // are NO LONGER fabricated here — a BCL-spelled impl on JS now resolves through the
    // source-level compat shim `capabilities-compat.js.fsi`, which abbreviates each BCL
    // spelling to the canonical BCL-free `Vesper.*`. Only the ITERATION interfaces below
    // (`IEnumerable\`1` / `IEnumerator\`1`) remain fabricated: `seq<'T>`'s abbreviation
    // needs a shape to resolve against, and retrofitting `seq` onto the capability
    // mechanism is out of scope.

    let private boolTy: FrozenType = FTConst(RuntimeNames.boolKey, EqArray.empty)

    /// The single declaring typar `'T` (axis Declaring, index 0).
    let private selfTypar: FrozenType = FTTypar(TyparAxis.Declaring, 0)

    // --- System.Collections.Generic.IEnumerable<'T> / IEnumerator<'T> -----------
    //
    // The iteration capability surface, the enumerable analogue of the erased
    // `IEquatable\`1` / `IComparable\`1` above. On JS these BCL interfaces are ERASED
    // at runtime (a class implementing `seq<'T>` lowers to a native `[Symbol.iterator]`
    // generator that drives the enumerator's `MoveNext()` / `Current`); they exist
    // here as PROVIDER METADATA only so a Vesper class can WRITE the impl —
    // `interface System.Collections.Generic.IEnumerable<int> with member GetEnumerator …`
    // — and the front-end's interface-ness check (`Unification.fs`) + conformance
    // (`checkInterfaceConformance`, which checks only the named interface's OWN
    // members) accept it. `CapabilityIds.Enumerable` resolves to `IEnumerable\`1` off
    // the `seq` abbreviation, so the codegen capability match keys on this exact name.
    //
    // The modelled member surface is the minimal pair the `[Symbol.iterator]` adapter
    // drives, NOT the full BCL shape (no inherited `IEnumerator`/`IDisposable` members):
    // `IEnumerator\`1` carries `MoveNext(): bool` + `Current: 'T`, and the conformance
    // check requires exactly those of an implementer — so a Vesper enumerator declares
    // just `interface IEnumerator<int> with member MoveNext … member Current …`.

    let private collectionsGenericNs = "System.Collections.Generic"

    let private collectionsGenericOrigin: SymbolOrigin =
        {
            Home = runtimeHome
            Namespace = SymbolKeyOps.namespaceKey collectionsGenericNs
        }

    let private ienumeratorTypeKey: TypeKey =
        SymbolKeyOps.typeKeyOf collectionsGenericNs "IEnumerator`1"

    let private ienumerableTypeKey: TypeKey =
        SymbolKeyOps.typeKeyOf collectionsGenericNs "IEnumerable`1"

    let private ienumeratorKey: SymbolKey = SymbolKey.Type ienumeratorTypeKey

    let private ienumerableKey: SymbolKey = SymbolKey.Type ienumerableTypeKey

    /// The compiled qualified name of the erased `IEnumerable\`1` interface — the name
    /// `for … in` recognition matches (`pickEnumerableElem`). A provider that wants a
    /// foreign type treated as `seq<'T>` injects this name into the type's interface set.
    let enumerableInterfaceName: string = SymbolKeyOps.qualifiedName ienumerableKey

    /// An instance interface member of an erased interface. `declaringArity` is the
    /// declaring interface's generic arity (`1` for `IEnumerable<'T>`/`IEnumerator<'T>`,
    /// `0` for the non-generic `System.IDisposable`).
    let private mkIfaceMember
        (origin: SymbolOrigin)
        (declaringArity: int)
        (declKey: TypeKey)
        (name: string)
        (isProperty: bool)
        (parameters: FrozenType)
        (ret: FrozenType)
        : ExternalMember =
        { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey name EqArray.empty (MemberKind.InterfaceMethod declKey)) with
            Storage =
                if isProperty then
                    MemberStorage.Property
                else
                    MemberStorage.Method
            Signature = ExternalSignature.make (declaringArity, 0, parameters, ret)
            Origin = origin
        }

    /// Pair an erased class-interface shape with the map key DERIVED from its head
    /// `SymbolKey` (`SymbolKeyOps.qualifiedName`), so the qualified-name string is never
    /// re-spelled.
    let private erasedClassEntry (key: SymbolKey) (shape: ExternalTypeShape) : string * ExternalTypeShape =
        SymbolKeyOps.qualifiedName key, shape

    let private mkErasedClassIface (arity: int) (origin: SymbolOrigin) (members: ExternalMember[]) : ExternalTypeShape =
        ExternalTypeShape.Class
            {
                Arity = arity
                IsInterface = true
                Members = members
                FrozenInterfaces = [||]
                FrozenBaseType = ValueNone
                Flags = ExternalClassFlags.Default
                Origin = origin
            }

    /// `IEnumerator<'T>` — `MoveNext(): bool` + the `Current: 'T` property.
    let private ienumeratorShape: ExternalTypeShape =
        mkErasedClassIface
            1
            collectionsGenericOrigin
            [|
                mkIfaceMember collectionsGenericOrigin 1 ienumeratorTypeKey "MoveNext" false unitTy boolTy
                mkIfaceMember collectionsGenericOrigin 1 ienumeratorTypeKey "Current" true unitTy selfTypar
            |]

    /// `IEnumerable<'T>` — `GetEnumerator(): IEnumerator<'T>`.
    let private ienumerableShape: ExternalTypeShape =
        mkErasedClassIface
            1
            collectionsGenericOrigin
            [|
                mkIfaceMember
                    collectionsGenericOrigin
                    1
                    ienumerableTypeKey
                    "GetEnumerator"
                    false
                    unitTy
                    (FTClass(ienumeratorKey, EqArray.ofSeq [ selfTypar ]))
            |]

    /// The JS-native type table. `Error` is keyed by its bare global name; the
    /// iteration interfaces by their arity-suffixed qualified name (the form
    /// `tryResolveExternalTypeKey` probes). The capability interfaces
    /// (`System.IDisposable` / `IEquatable\`1` / `IComparable\`1`) are deliberately
    /// ABSENT — they resolve through the `capabilities-compat.js.fsi` source shim.
    let private types: Map<string, ExternalTypeShape> =
        Map
            [
                "Error", errorShape
                erasedClassEntry ienumerableKey ienumerableShape
                erasedClassEntry ienumeratorKey ienumeratorShape
            ]

    /// All overloads of `memberName` on `typeName`, read off the shape's `Members`.
    let private membersOf (typeName: string) (memberName: string) : ExternalMember[] =
        match Map.tryFind typeName types with
        | Some(ExternalTypeShape.Class shape) -> shape.Members |> Array.filter (fun m -> m.Name = memberName)
        | _ -> [||]

    /// The layer-2 provider for JS-native runtime types — a by-name leaf;
    /// `ofNamedLeaf` derives the store face, so the two faces cannot drift.
    let provider: IExternalSymbolProvider =
        ExternalSymbolProviders.ofNamedLeaf
            { ExternalSymbolProviders.NamedLeaf.empty with
                TryLookupType =
                    fun name ->
                        match Map.tryFind name types with
                        | Some s -> ValueSome s
                        | None -> ValueNone
                TryLookupMember =
                    fun (typeName, memberName) ->
                        match membersOf typeName memberName with
                        | [||] -> ValueNone
                        | arr -> ValueSome arr.[0]
                TryLookupMembers = fun (typeName, memberName) -> membersOf typeName memberName
            }

    /// The JS-native layer-2 leaf factory: the JS-native tail instead of BCL reflection,
    /// so `Vesper.Exceptions` contract types resolve through `exn`'s `(# "Error" #)` repr
    /// without a BCL type colliding on the home-assembly invariant. Reverse-map
    /// independent (the JS leaf canonicalizes nothing). The single seam both conveniences
    /// below route through, so they share the `"jsnative"` contract-cache entry.
    let private jsNativeMetaTail: SymbolProviders.MetaTailFactory =
        fun _ -> [ provider ]

    /// The JS-native contract provider. `"jsnative"` keeps the contract-cache entry
    /// distinct from the `"bcl"` one.
    let buildJsNativeContractFor (target: string option) (manifestPaths: string list) : IExternalSymbolProvider =
        SymbolProviders.buildContractWith "jsnative" jsNativeMetaTail target manifestPaths
        |> fst

    /// The raw cross-package inline-body map for the JS-native contract — introspection
    /// seam for the `OpsPlatformJs` tests (shares the `"jsnative"` cache entry with
    /// `buildJsNativeContractFor`). The JS-side counterpart of the (CLR-side)
    /// `ClrSymbolProviders.contractInlineBodiesFor`. JS-target only: the JS-native leaf
    /// resolves no BCL types, so it cannot build the CLR (`target = None`) collection.
    let jsNativeInlineBodiesFor (target: string option) (manifestPaths: string list) : Map<string, InlineBody> =
        SymbolProviders.buildContractWith "jsnative" jsNativeMetaTail target manifestPaths
        |> snd
