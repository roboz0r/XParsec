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
            }

    // --- System.IEquatable<'T> / System.IComparable<'T> -------------------------
    //
    // The JS provider is BCL-free, so it carries no `System.IEquatable\`1` /
    // `System.IComparable\`1`. Without them a user's `interface System.IEquatable<Self>`
    // on a custom-equality class resolves to an unbound `TyVar`, the
    // "is not an interface" check (`Unification.fs`) fires, and the Phase-3 custom-eq
    // gate (`validateCustomEqCompImpls`) then reports FS0378. These interfaces are
    // ERASED at runtime on JS (the runtime duck-types `.Equals` / `.CompareTo`
    // presence), so they exist here as PROVIDER METADATA only — no `.mjs` artifact.
    //
    // Keyed by the arity-suffixed name `tryResolveExternalTypeKey` probes first for an
    // arity-1 receiver (`System.IEquatable\`1`); the resolved nominal head's
    // `qualifiedName` is then EXACTLY `System.IEquatable\`1` (origin namespace `System`
    // split off, arity suffix retained) — the form the gate matches on.

    let private systemOrigin: SymbolOrigin =
        {
            Assembly = Some RuntimeAssembly
            Namespace = "System"
            DeclaringType = None
        }

    let private boolTy: FrozenType = FTConst("bool", EqArray.empty)
    let private intTy: FrozenType = FTConst("int", EqArray.empty)
    /// The single declaring typar `'T` (axis Declaring, index 0).
    let private selfTypar: FrozenType = FTTypar(TyparAxis.Declaring, 0)

    /// An erased arity-1 `System` interface (`IEquatable\`1` / `IComparable\`1`) as a
    /// single-member `ExternalTypeShape.Class`: `<memberName> : 'T -> ret`. The pair
    /// differ only in `{name, member name, return type}`, so they share this builder.
    let private mkErasedGenericIface (name: string) (memberName: string) (ret: FrozenType) : ExternalTypeShape =
        let key = SymbolKey.TypeKey(Some RuntimeAssembly, "System", name)

        let mem: ExternalMember =
            {
                Name = memberName
                IsStatic = false
                IsProperty = false
                Signature =
                    {
                        DeclaringArity = 1
                        MethodArity = 0
                        Parameters = selfTypar
                        Return = ret
                    }
                MethodArity = 0
                Origin = systemOrigin
                Key = SymbolKey.MemberKey(key, memberName, EqArray.empty, MemberKind.InterfaceMethod key)
                OptionalDefaults = []
            }

        ExternalTypeShape.Class
            {
                Arity = 1
                IsInterface = true
                Members = [| mem |]
                FrozenInterfaces = [||]
                FrozenBaseType = ValueNone
                Flags = ExternalClassFlags.Default
                Origin = systemOrigin
            }

    /// The JS-native type table, keyed by the compiled name resolution probes:
    /// `Error` by bare global name, the generic interfaces by their arity-suffixed
    /// qualified name.
    let private types: Map<string, ExternalTypeShape> =
        Map
            [
                "Error", errorShape
                "System.IEquatable`1", mkErasedGenericIface "IEquatable`1" "Equals" boolTy
                "System.IComparable`1", mkErasedGenericIface "IComparable`1" "CompareTo" intTy
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
        }

    /// `buildContractWithMetadata` with the JS-native metadata tail instead of BCL
    /// reflection, so `Vesper.Exceptions` contract types resolve through `exn`'s
    /// `(# "Error" #)` repr without a BCL type colliding on the home-assembly invariant.
    /// `"jsnative"` keeps the contract-cache entry distinct from the `"bcl"` one.
    let buildJsNativeContractFor (target: string option) (manifestPaths: string list) : IExternalSymbolProvider =
        SymbolProviders.buildContractWithMetadata "jsnative" [ provider ] target manifestPaths
