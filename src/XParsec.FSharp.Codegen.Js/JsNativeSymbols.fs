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

    let private runtimeHome: SymbolHome =
        SymbolHome.InAssembly(AssemblyName RuntimeAssembly)

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
                Attributes = EqArray.empty
                Origin = errorOrigin
            }

    /// The stub table, one entry per identity.
    let private types: (TypeKey * ExternalTypeShape) list = [ errorTypeKey, errorShape ]

    let private shapesByKey = readOnlyDict types

    let private membersByKey =
        readOnlyDict
            [
                for key, shape in types do
                    match shape with
                    | ExternalTypeShape.Class c -> key, c.Members
                    | _ -> ()
            ]

    let private directorySlots () : seq<struct (string * string * int)> =
        seq {
            for key, _ in types do
                struct (SymbolKeyOps.typeNs key, key.Name, key.TyparArity)
        }

    let private scope: IScopeContents =
        ScopeContents.typeDirectory
            directorySlots
            (fun key ->
                match shapesByKey.TryGetValue key with
                | true, s -> ValueSome s
                | _ -> ValueNone
            )

    let private platformFacts: IPlatformFacts =
        { new IPlatformFacts with
            // JS has no value types, of any key: `int` is a `number` like every other numeric,
            // and a `[<Struct>]` record erases to the same object a plain one is.
            member _.IsValueType _ = ValueSome false

            // A tuple of any arity is one rank-1 array; `unit` and a 1-tuple are not tuple
            // values. WHICH array key is unforced, because the layout above is the same for
            // every key.
            member _.TupleType arity =
                if arity < 2 then
                    ValueNone
                else
                    ValueSome(RuntimeNames.arrayTypeKey 1)
        }

    let provider: IExternalSymbolProvider =
        ExternalSymbolProviders.ofKeyIndexedChannels
            { ExternalSymbolProviders.KeyIndexedChannels.empty with
                Scope = scope
                ShapesByKey = shapesByKey
                MembersByKey = membersByKey
                Platform = ValueSome platformFacts
            }

    /// The platform metadata a JS compile ends in: these stubs stand where a CLR compile puts
    /// .NET reflection. It reads nothing from the intrinsic axis its argument carries.
    let private jsNativeMetadata: SymbolProviders.PlatformMetadataFactory =
        fun _ -> [ provider ]

    /// The JS-native contract for a manifest set, whole. A compile takes this; the
    /// projections below are for callers that only resolve or introspect.
    /// The one place this backend states its target: it RESOLVES each package directory to
    /// `manifest.js.toml`, so a set for another target is not something a caller can hand it.
    let jsNativeContract (packageDirs: string list) : PackageProviders.AnalysedManifest =
        SymbolProviders.buildContract jsNativeMetadata Target.Js packageDirs

    /// The JS-native contract provider, for a caller that only RESOLVES symbols.
    let buildJsNativeContract (packageDirs: string list) : IExternalSymbolProvider =
        (jsNativeContract packageDirs).Provider

    /// The contract's inline bodies alone, for a caller that only introspects them.
    let jsNativeInlineBodies (packageDirs: string list) : Map<string, InlineBody> =
        (jsNativeContract packageDirs).InlineBodies |> InlineBodies.valuesByName

    /// The declaring files the contract's inline bodies were unpooled from, alone.
    let jsNativeInlineRetained (packageDirs: string list) : LexedFiles = (jsNativeContract packageDirs).Retained
