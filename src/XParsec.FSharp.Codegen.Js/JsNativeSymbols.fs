namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Hand-authored stub shapes for the JS runtime types the backend resolves BY NAME.
/// `Error` is the name `exn`'s `(# "Error" #)` repr carries, so `MyExn "boom"` lowers to
/// `new Error("boom")` and an `exn`-typed object argument's member probe lands on this shape.
module JsNativeSymbols =

    /// The synthetic home "assembly" the stub types report: a label, not a real reference.
    [<Literal>]
    let private RuntimeAssembly = "Vesper.Js.Runtime"

    let private runtimeHome: Origin = Origin.InAssembly(AssemblyName RuntimeAssembly)

    let private errorOrigin: SymbolOrigin =
        {
            Home = runtimeHome
            Namespace = SymbolKeyOps.namespaceKey ""
        }

    /// `Error` is global, so its compiled / lookup name is the bare `Error`.
    let private errorTypeKey: TypeKey = SymbolKeyOps.typeKeyOf "" "Error"

    let private errorKey: SymbolKey = SymbolKey.Type errorTypeKey

    let private errorTy: FrozenType = FTClass(errorTypeKey, EqArray.empty)

    let private stringTy: FrozenType = FTConst(RuntimeNames.stringKey, EqArray.empty)

    let private unitTy: FrozenType = FTConst(RuntimeNames.unitKey, EqArray.empty)

    /// `new Error(message: string)`.
    let private errorCtor: ExternalMember =
        ExternalMember.ctor
            errorTypeKey
            (ExternalSignature.make (0, 0, stringTy, errorTy))
            (EqArray.singleton stringTy)
            errorOrigin
            []

    /// `Error.prototype.message : string`.
    let private errorMessage: ExternalMember =
        { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf errorTypeKey "message" EqArray.empty 0 MemberKind.Property) with
            Storage = MemberStorage.Property
            Signature = ExternalSignature.make (0, 0, unitTy, stringTy)
            Origin = errorOrigin
        }

    let private errorShape: ExternalTypeShape =
        ExternalTypeShape.Class
            {
                TyparArity = 0
                IsInterface = false
                Members = [| errorCtor; errorMessage |]
                FrozenInterfaces = [||]
                FrozenBaseType = ValueNone
                Flags = ExternalClassFlags.Default
                Origin = errorOrigin
            }

    let private boolTy: FrozenType = FTConst(RuntimeNames.boolKey, EqArray.empty)

    let private selfTypar: FrozenType = FTTypar(TyparAxis.Declaring, 0)

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

    /// `System.Collections.Generic.IEnumerable\`1`. A provider makes a foreign type
    /// enumerable by adding this NAME (never a key) to the type's interface set.
    let enumerableInterfaceName: string = SymbolKeyOps.qualifiedName ienumerableKey

    /// An instance member of an erased interface. `declaringTyparArity` is `1` for
    /// `IEnumerable<'T>`, the arity `'T` is baked against.
    let private mkIfaceMember
        (origin: SymbolOrigin)
        (declaringTyparArity: int)
        (declKey: TypeKey)
        (name: string)
        (isProperty: bool)
        (parameters: FrozenType)
        (ret: FrozenType)
        : ExternalMember =
        { ExternalMember.OfKey(
              SymbolKeyOps.memberKeyOf declKey name EqArray.empty 0 (MemberKind.InterfaceMethod declKey)
          ) with
            Storage =
                if isProperty then
                    MemberStorage.Property
                else
                    MemberStorage.Method
            Signature = ExternalSignature.make (declaringTyparArity, 0, parameters, ret)
            Origin = origin
        }

    let private erasedClassEntry (key: SymbolKey) (shape: ExternalTypeShape) : string * ExternalTypeShape =
        SymbolKeyOps.qualifiedName key, shape

    let private mkErasedClassIface (arity: int) (origin: SymbolOrigin) (members: ExternalMember[]) : ExternalTypeShape =
        ExternalTypeShape.Class
            {
                TyparArity = arity
                IsInterface = true
                Members = members
                FrozenInterfaces = [||]
                FrozenBaseType = ValueNone
                Flags = ExternalClassFlags.Default
                Origin = origin
            }

    /// `IEnumerator<'T>` — `MoveNext(): bool` + the `Current: 'T` property. Conformance
    /// demands exactly the members listed here, so adding the BCL's inherited `Reset` /
    /// `object Current` would force every implementer to write them too.
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
                    (FTClass(ienumeratorTypeKey, EqArray.ofSeq [ selfTypar ]))
            |]

    /// Keyed as a by-name lookup spells it: bare `Error` for the global, arity-suffixed
    /// `System.Collections.Generic.IEnumerable\`1` for the generic interfaces.
    let private types: Map<string, ExternalTypeShape> =
        Map
            [
                "Error", errorShape
                erasedClassEntry ienumerableKey ienumerableShape
                erasedClassEntry ienumeratorKey ienumeratorShape
            ]

    let private membersOf (typeName: string) (memberName: string) : ExternalMember[] =
        match Map.tryFind typeName types with
        | Some(ExternalTypeShape.Class shape) -> shape.Members |> Array.filter (fun m -> m.Name = memberName)
        | _ -> [||]

    /// The stub table as a provider.
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

    /// The metadata tail a JS compile ends in: these stubs stand where a CLR compile puts
    /// BCL reflection. It reads nothing from the reverse-canon map its argument carries.
    let private jsNativeMetaTail: SymbolProviders.MetaTailFactory =
        fun _ -> [ provider ]

    /// The JS-native contract for a manifest set, whole. A compile takes this; the
    /// projections below are for callers that only resolve or introspect.
    let jsNativeContractFor (target: string) (manifestPaths: string list) : SymbolProviders.Contract =
        SymbolProviders.buildContractWith "jsnative" jsNativeMetaTail target manifestPaths

    /// The JS-native contract provider, for a caller that only RESOLVES symbols.
    let buildJsNativeContractFor (target: string) (manifestPaths: string list) : IExternalSymbolProvider =
        (jsNativeContractFor target manifestPaths).Provider

    /// The contract's inline bodies alone, for a caller that only introspects them.
    let jsNativeInlineBodiesFor (target: string) (manifestPaths: string list) : Map<string, InlineBody> =
        (jsNativeContractFor target manifestPaths).BodiesByName

    /// The producer files the contract's inline bodies were unpooled from, alone.
    let jsNativeInlineOriginsFor (target: string) (manifestPaths: string list) : OriginSources =
        (jsNativeContractFor target manifestPaths).Origins
