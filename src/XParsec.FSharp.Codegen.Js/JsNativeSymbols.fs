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

    let private errorOrigin: SymbolOrigin =
        {
            Assembly = Some RuntimeAssembly
            Namespace = ""
            DeclaringType = None
        }

    /// `Error` is global, so its compiled / lookup name is the bare `Error`.
    let private errorKey: SymbolKey =
        SymbolKey.TypeKey(Some RuntimeAssembly, "", "Error")

    let private errorTy: FrozenType = FTClass(errorKey, EqArray.empty)
    let private stringTy: FrozenType = FTConst("string", EqArray.empty)
    let private unitTy: FrozenType = FTConst("unit", EqArray.empty)

    /// `new Error(message: string)` — the JS `Error` constructor as an `ExternalMember`.
    let private errorCtor: ExternalMember =
        ExternalMember.ctor
            errorKey
            {
                DeclaringArity = 0
                MethodArity = 0
                Parameters = stringTy
                Return = errorTy
            }
            (EqArray.singleton "string")
            errorOrigin
            []

    /// `Error.prototype.message : string`.
    let private errorMessage: ExternalMember =
        {
            Name = "message"
            IsStatic = false
            IsProperty = true
            Signature =
                {
                    DeclaringArity = 0
                    MethodArity = 0
                    Parameters = unitTy
                    Return = stringTy
                }
            MethodArity = 0
            Origin = errorOrigin
            Key = SymbolKey.MemberKey(errorKey, "message", EqArray.empty, MemberKind.Property)
            OptionalDefaults = []
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
                CapabilityFace = ValueNone
            }

    // The capability interfaces (`System.IDisposable` / `IEquatable\`1` / `IComparable\`1`)
    // are NO LONGER fabricated here — a BCL-spelled impl on JS now resolves through the
    // source-level compat shim `capabilities-compat.js.fsi`, which abbreviates each BCL
    // spelling to the canonical BCL-free `Vesper.*`. Only the ITERATION interfaces below
    // (`IEnumerable\`1` / `IEnumerator\`1`) remain fabricated: `seq<'T>`'s abbreviation
    // needs a shape to resolve against, and retrofitting `seq` onto the capability
    // mechanism is out of scope.

    let private boolTy: FrozenType = FTConst("bool", EqArray.empty)
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
            Assembly = Some RuntimeAssembly
            Namespace = collectionsGenericNs
            DeclaringType = None
        }

    let private ienumeratorKey: SymbolKey =
        SymbolKey.TypeKey(Some RuntimeAssembly, collectionsGenericNs, "IEnumerator`1")

    let private ienumerableKey: SymbolKey =
        SymbolKey.TypeKey(Some RuntimeAssembly, collectionsGenericNs, "IEnumerable`1")

    /// An instance interface member of an erased interface. `declaringArity` is the
    /// declaring interface's generic arity (`1` for `IEnumerable<'T>`/`IEnumerator<'T>`,
    /// `0` for the non-generic `System.IDisposable`).
    let private mkIfaceMember
        (origin: SymbolOrigin)
        (declaringArity: int)
        (declKey: SymbolKey)
        (name: string)
        (isProperty: bool)
        (parameters: FrozenType)
        (ret: FrozenType)
        : ExternalMember =
        {
            Name = name
            IsStatic = false
            IsProperty = isProperty
            Signature =
                {
                    DeclaringArity = declaringArity
                    MethodArity = 0
                    Parameters = parameters
                    Return = ret
                }
            MethodArity = 0
            Origin = origin
            Key = SymbolKey.MemberKey(declKey, name, EqArray.empty, MemberKind.InterfaceMethod declKey)
            OptionalDefaults = []
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
                CapabilityFace = ValueNone
            }

    /// `IEnumerator<'T>` — `MoveNext(): bool` + the `Current: 'T` property.
    let private ienumeratorShape: ExternalTypeShape =
        mkErasedClassIface
            1
            collectionsGenericOrigin
            [|
                mkIfaceMember collectionsGenericOrigin 1 ienumeratorKey "MoveNext" false unitTy boolTy
                mkIfaceMember collectionsGenericOrigin 1 ienumeratorKey "Current" true unitTy selfTypar
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
                    ienumerableKey
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

    /// The layer-2 provider for JS-native runtime types.
    let provider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone

            member _.TryLookupType name =
                match Map.tryFind name types with
                | Some s -> ValueSome s
                | None -> ValueNone

            member _.TryLookupMember(typeName, memberName) =
                match membersOf typeName memberName with
                | [||] -> ValueNone
                | arr -> ValueSome arr.[0]

            member _.TryLookupMembers(typeName, memberName) = membersOf typeName memberName
            member _.TryLookupUnionCase _ = ValueNone
            member _.AmbientOpenPrefixes = []
            member _.TryLookupInlineBody _ = ValueNone
            member _.TryLookupInlineBodyByName _ = ValueNone
            member _.IntrinsicReverseCanon = Map.empty
            member _.IntrinsicForwardRepr = Map.empty
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
