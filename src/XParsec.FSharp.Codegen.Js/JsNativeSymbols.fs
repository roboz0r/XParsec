namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Hand-authored stub shapes for the JS runtime types the backend resolves BY NAME.
/// `Error` is the name `exn`'s `(# "Error" #)` repr carries, so `MyExn "boom"` lowers to
/// `new Error("boom")` and an `exn`-typed object argument's member probe lands on this shape.
module JsNativeSymbols =

    /// The synthetic home "assembly" a stub type reports: a label, not a real reference.
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
            Signature = ExternalSignature.value (0, 0, stringTy)
            Origin = errorOrigin
        }

    let private errorShape: ExternalTypeShape =
        ExternalTypeShape.Class
            {
                TyparArity = 0
                IsInterface = false
                Members = EqArray.ofSeq [ errorCtor; errorMessage ]
                FrozenInterfaces = EqArray.empty
                FrozenBaseType = ValueNone
                Flags = ExternalClassFlags.Default
                Origin = errorOrigin
            }

    /// Keyed as a by-name lookup spells it: the bare `Error`, because the global has no
    /// namespace to qualify it.
    let private types: Map<string, ExternalTypeShape> = Map [ "Error", errorShape ]

    let private membersOf (typeName: string) (memberName: string) : EqArray<ExternalMember> =
        match Map.tryFind typeName types with
        | Some(ExternalTypeShape.Class shape) -> shape.Members |> EqArray.filter (fun m -> m.Name = memberName)
        | _ -> EqArray.empty

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
                        | EqEmpty -> ValueNone
                        | arr -> ValueSome arr.[0]
                TryLookupMembers = fun (typeName, memberName) -> membersOf typeName memberName
            }

    /// The metadata tail a JS compile ends in: these stubs stand where a CLR compile puts
    /// BCL reflection. It reads nothing from the reverse-canon map its argument carries.
    let private jsNativeMetaTail: SymbolProviders.MetaTailFactory =
        fun _ -> [ provider ]

    /// The JS-native contract for a manifest set, whole. A compile takes this; the
    /// projections below are for callers that only resolve or introspect.
    /// The one place this backend states its target: it RESOLVES each package directory to
    /// `manifest.js.toml`, so a set for another target is not something a caller can hand it.
    let jsNativeContract (packageDirs: string list) : SymbolProviders.Contract =
        SymbolProviders.buildContractWith "jsnative" jsNativeMetaTail Target.Js packageDirs

    /// The JS-native contract provider, for a caller that only RESOLVES symbols.
    let buildJsNativeContract (packageDirs: string list) : IExternalSymbolProvider =
        (jsNativeContract packageDirs).Provider

    /// The contract's inline bodies alone, for a caller that only introspects them.
    let jsNativeInlineBodies (packageDirs: string list) : Map<string, InlineBody> =
        (jsNativeContract packageDirs).BodiesByName

    /// The producer files the contract's inline bodies were unpooled from, alone.
    let jsNativeInlineOrigins (packageDirs: string list) : OriginSources = (jsNativeContract packageDirs).Origins
