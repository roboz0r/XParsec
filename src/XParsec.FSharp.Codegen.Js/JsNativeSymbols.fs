namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// The JS-target analogue of `MetadataSymbols.provider` (which reflects host BCL
/// metadata): a layer-2 provider supplying the **core JS runtime types** the backend
/// references. It is the seam where a future `tsc`-derived metadata format — parsed by
/// `Codegen.Js` and handed in as an `IExternalSymbolProvider` — will plug in. Until
/// that exists these are **hand-authored truncated stubs**; the JS build composes this
/// (via `buildJsNativeContractFor`) in place of the BCL `MetadataSymbols`
/// (codegen-js-steps.md Step 8), so the front end resolves against JS-native metadata
/// rather than host reflection.
///
/// The one referenced runtime type today is `Error` — the native type `exn`'s
/// `(# "Error" #)` repr names (`prim-types-exn.js.fs`). It is **authoritative, not
/// decorative**: codegen's `exnReprOf` resolves `exn`'s intrinsic repr through this
/// provider to the `Error` *class* and emits its compiled name, so an `exn` subtype
/// (every `Vesper.Exceptions` contract type, reached via its `inherit exn` chain)
/// lowers to `new Error(message)` against this definition — drop `Error` here and the
/// construction fails loudly. The `(message: string)` constructor records the leading
/// arg's slot; the `message` property is what an `exn`-typed receiver's member access
/// resolves to (`Engine.tryExternalReceiver` canonicalises `exn` → `Error` and reads
/// its members). A richer hierarchy waits until the backend references more runtime
/// types.
module JsNativeSymbols =

    /// The synthetic home "assembly" the stub types report — the JS runtime scope a
    /// `tsc` import map would later name. Codegen never mints a TypeRef off it (a JS
    /// exception emits the bare `new Error` token from `exn`'s repr), so it is a label,
    /// not a real reference.
    [<Literal>]
    let private RuntimeAssembly = "Vesper.Js.Runtime"

    let private errorOrigin: SymbolOrigin =
        {
            Assembly = Some RuntimeAssembly
            Namespace = ""
            DeclaringType = None
        }

    /// `Error` is global (no namespace), so its compiled / lookup name is the bare
    /// `Error` — resolvable without an ambient open prefix.
    let private errorKey: SymbolKey =
        SymbolKey.TypeKey(Some RuntimeAssembly, "", "Error")

    let private errorTy: FrozenType = FTClass(errorKey, EqArray.empty)
    let private stringTy: FrozenType = FTConst("string", EqArray.empty)
    let private unitTy: FrozenType = FTConst("unit", EqArray.empty)

    /// `new Error(message: string)` — the leading JS `Error` constructor, mirroring the
    /// `.ctor` shape `MetadataSymbols` builds (instance, `MemberKind.Method`), so a
    /// constructor-as-function reference (`inferExternalCtorOn`'s
    /// `TryLookupMembers(type, ".ctor")`) types against it.
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
            }

    /// The hand-authored JS-native type table. Keyed by the bare global name; grows as
    /// the backend references more runtime types (the eventual `tsc` feed replaces the
    /// hand authoring, not this lookup shape).
    let private types: Map<string, ExternalTypeShape> = Map [ "Error", errorShape ]

    /// All overloads of `memberName` on `typeName`, read off the type's shape
    /// `Members` so the shape stays the single source of truth — adding a runtime
    /// type or member is a one-site change to `types` (and its `Members`), with no
    /// parallel switch to keep in sync.
    let private membersOf (typeName: string) (memberName: string) : ExternalMember[] =
        match Map.tryFind typeName types with
        | Some(ExternalTypeShape.Class shape) -> shape.Members |> Array.filter (fun m -> m.Name = memberName)
        | _ -> [||]

    /// The layer-2 provider the JS build composes (the `MetadataSymbols.provider`
    /// counterpart). Values are absent (JS runtime functions ride the Vesper contracts,
    /// not this layer); only the core runtime *types* live here.
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
            // The JS-native layer publishes runtime classes (`Error`), not Vesper
            // intrinsics — no `platform -> canon` reconciliation to contribute.
            member _.IntrinsicReverseCanon = Map.empty
        }

    /// `SymbolProviders.buildContractFor` over a **JS-native** layer-2 stack: the
    /// front-end-facing provider's metadata tail is `provider` (the JS runtime types)
    /// instead of host BCL reflection (codegen-js-steps.md Step 8). The JS backend uses
    /// this so its `Vesper.Exceptions` contract (`System.*Exception : exn`) resolves
    /// through the contract `inherit` chain — with `exn`'s `(# "Error" #)` repr
    /// resolving to the native `Error` class above — without a same-named BCL metadata
    /// type colliding on the home-assembly invariant. The composition lives in
    /// `Codegen.Js` (not `Codegen.Common`) so Common stays target-agnostic; `"jsnative"`
    /// keeps this stack's contract-cache entry distinct from the `"bcl"` one.
    let buildJsNativeContractFor (target: string option) (manifestPaths: string list) : IExternalSymbolProvider =
        SymbolProviders.buildContractWithMetadata "jsnative" [ provider ] target manifestPaths
