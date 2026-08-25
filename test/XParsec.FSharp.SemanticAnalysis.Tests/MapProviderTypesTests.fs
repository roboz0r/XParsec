module XParsec.FSharp.SemanticAnalysis.Tests.MapProviderTypesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The fixture plants `marker` in every position a provider puts a type (parameter, return,
// field, interface arg) and asserts which come back as a `witness` of the variance they were
// mapped at. ROOT variance only, not the algebra under a nested node.

let private origin = SymbolOrigin.Empty
let private marker = FTConst(RuntimeNames.opaqueKey "M", EqArray.empty)

let private witness (v: Variance) : FrozenType =
    let name =
        match v with
        | Variance.Co -> "co"
        | Variance.Contra -> "contra"
        | Variance.Inv -> "inv"

    FTConst(RuntimeNames.opaqueKey name, EqArray.empty)

/// Resolve ONLY the marker, to a witness of the variance that position was mapped at.
/// A surviving marker in the output ⇒ that position was left unmapped.
let private resolveMarker (v: Variance) (t: FrozenType) : FrozenType = if t = marker then witness v else t

let private clsKey = SymbolKeyOps.typeKeyOf origin.Namespace.Dotted "Cls"

/// Carries identity + field NAMES only, so nothing in it is a position to map.
let private candidate: ExternalRecordCandidate =
    {
        TypeKey = clsKey
        TyparArity = 0
        FieldNames = EqArray.singleton "f"
        IsRequireQualifiedAccess = false
    }

let private markerMember: ExternalMember =
    { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf clsKey "m" EqArray.empty 0 MemberKind.Method) with
        Signature = TestHelpers.mkSignature 0 0 marker marker
        Origin = origin
    }

let private markerCase: ExternalCaseShape =
    {
        Name = "C"
        FieldNames = EqArray.singleton ValueNone
        FrozenFieldTypes = EqArray.singleton marker
    }

// The by-key channels reach this lookup as the key's qualified name — `Cls`, `Rec`, `Uni`.
let private typeByName (name: string) : ExternalTypeShape voption =
    match name with
    | "Cls" ->
        ValueSome(
            ExternalTypeShape.Class
                { ExternalClassShape.basic (0, false, origin) with
                    Members = EqArray.singleton markerMember
                    FrozenInterfaces =
                        EqArray.singleton (
                            FrozenNominal.OfClass(SymbolKeyOps.qualifiedTypeKeyOf "I" 1, EqArray.singleton marker)
                        )
                    FrozenBaseType =
                        ValueSome(
                            FrozenNominal.OfClass(SymbolKeyOps.qualifiedTypeKeyOf "B" 1, EqArray.singleton marker)
                        )
                }
        )
    | "Rec" ->
        ValueSome(
            ExternalTypeShape.Record(
                1,
                EqArray.singleton
                    {
                        Name = "f"
                        IsMutable = false
                        Frozen = marker
                    },
                origin,
                false,
                false
            )
        )
    | "Uni" ->
        ValueSome(
            ExternalTypeShape.Union(
                1,
                EqArray.singleton markerCase,
                EqArray.singleton (
                    FrozenNominal.OfClass(SymbolKeyOps.qualifiedTypeKeyOf "J" 1, EqArray.singleton marker)
                ),
                origin,
                false
            )
        )
    | "Abb" -> ValueSome(ExternalTypeShape.Abbrev(1, marker))
    | _ -> ValueNone

let private memberByName (t: string) (m: string) : ExternalMember voption =
    if t = "Cls" && m = "m" then
        ValueSome markerMember
    else
        ValueNone

let private fake: IExternalSymbolProvider =
    ExternalSymbolProviders.ofNamedChannels
        { ExternalSymbolProviders.NamedChannels.empty with
            TryLookup =
                fun name ->
                    if name = "sym" then
                        ValueSome(ExternalSymbols.monoFrozen (SymbolKeyOps.inNamespace "") "sym" marker)
                    else
                        ValueNone
            TryLookupType = typeByName
            TryLookupUnionCases =
                fun caseName ->
                    if caseName = "C" then
                        EqArray.singleton
                            {
                                UnionKey = SymbolKeyOps.typeKeyOfArity origin.Namespace.Dotted "Uni" 1
                                Case = markerCase
                                IsRequireQualifiedAccess = false
                            }
                    else
                        EqArray.empty
            AmbientOpenPrefixes = [ "Amb" ]
            TryLookupMembers =
                fun q ->
                    match memberByName q.DeclaringType q.Name with
                    | ValueSome mem -> EqArray.singleton mem
                    | ValueNone -> EqArray.empty
            TryRecordsWithField =
                fun fieldName ->
                    if fieldName = "f" then
                        EqArray.singleton candidate
                    else
                        EqArray.empty
            Platform =
                ValueSome
                    { new IPlatformFacts with
                        member _.IsValueType _ = ValueSome true
                        member _.TupleType _ = ValueNone
                    }
        }

let private wrapped = ExternalSymbolProviders.mapProviderTypes resolveMarker fake

/// The `Class` shape of `Cls`, or fail.
let private clsShape () =
    match wrapped.TryLookupType "Cls" |> ExternalSymbols.typeShapeOf with
    | ValueSome(ExternalTypeShape.Class info) -> info
    | other -> failtestf "expected a Class shape, got %A" other

[<Tests>]
let tests =
    testList
        "ExternalSymbolProviders.mapProviderTypes"
        [
            test "a symbol Scheme is mapped covariantly" {
                match wrapped.TryLookup "sym" with
                | ValueSome s -> Expect.equal s.Scheme (witness Variance.Co) "Scheme root is co"
                | ValueNone -> failtest "sym should resolve"
            }

            test "a member's Parameters are contravariant and its Return covariant" {
                let info = clsShape ()
                let m = info.Members.[0]

                Expect.equal
                    (ExternalSignature.tupledParameters m.Signature)
                    (witness Variance.Contra)
                    "Parameters root is contra"

                Expect.equal m.Signature.Return (witness Variance.Co) "Return root is co"
            }

            test "interface type-arguments are invariant" {
                let info = clsShape ()

                let iface = info.FrozenInterfaces.[0]

                Expect.equal iface.Key (SymbolKeyOps.qualifiedTypeKeyOf "I" 1) "interface identity preserved"

                Expect.equal (EqArray.toArray iface.Args) [| witness Variance.Inv |] "interface arg root is inv"
            }

            // As for an interface: a base type is a REFERENCE, so only its arguments are
            // positions a value passes through.
            test "base-type type-arguments are invariant" {
                let baseTy = (clsShape ()).FrozenBaseType.Value

                Expect.equal baseTy.Key (SymbolKeyOps.qualifiedTypeKeyOf "B" 1) "base type identity preserved"

                Expect.equal (EqArray.toArray baseTy.Args) [| witness Variance.Inv |] "base type arg root is inv"
            }

            test "a record field is covariant" {
                match wrapped.TryLookupType "Rec" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Record(fields = fields)) ->
                    Expect.equal fields.[0].Frozen (witness Variance.Co) "record field root is co"
                | other -> failtestf "expected a Record shape, got %A" other
            }

            test "a union-case field is covariant and the union's interface args invariant" {
                match wrapped.TryLookupType "Uni" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Union(_, cases, ifaces, _, _)) ->
                    Expect.equal
                        cases.[0].FrozenFieldTypes
                        (EqArray.singleton (witness Variance.Co))
                        "case field root is co"

                    Expect.equal
                        ifaces.[0].Key
                        (SymbolKeyOps.qualifiedTypeKeyOf "J" 1)
                        "union interface identity preserved"

                    Expect.equal
                        (EqArray.toArray ifaces.[0].Args)
                        [| witness Variance.Inv |]
                        "union interface arg root is inv"
                | other -> failtestf "expected a Union shape, got %A" other
            }

            test "the member channels (single + overloads) map identically to the class member" {
                match wrapped.TryLookupMember(SymbolKeyOps.qualifiedTypeKeyOf "Cls" 0, "m") with
                | ValueSome m ->
                    Expect.equal
                        (ExternalSignature.tupledParameters m.Signature)
                        (witness Variance.Contra)
                        "single: Parameters contra"

                    Expect.equal m.Signature.Return (witness Variance.Co) "single: Return co"
                | ValueNone -> failtest "member should resolve"

                let all = wrapped.TryLookupMembers(SymbolKeyOps.qualifiedTypeKeyOf "Cls" 0, "m")
                Expect.equal all.Length 1 "one overload"

                Expect.equal
                    (ExternalSignature.tupledParameters all.[0].Signature)
                    (witness Variance.Contra)
                    "overloads: Parameters contra"

                Expect.equal all.[0].Signature.Return (witness Variance.Co) "overloads: Return co"
            }

            test "the reverse union-case channel maps case fields covariantly" {
                match wrapped.TryLookupUnionCases "C" with
                | EqOne uc ->
                    Expect.equal
                        uc.Case.FrozenFieldTypes
                        (EqArray.singleton (witness Variance.Co))
                        "reverse case field root is co"
                | other -> failtestf "expected one declaring union for 'C', got %A" other
            }

            test "an Abbrev body is NOT threaded (no intrinsic variance) — the marker survives" {
                match wrapped.TryLookupType "Abb" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Abbrev(_, body)) ->
                    Expect.equal body marker "the abbreviation body is left for its expansion seam"
                | other -> failtestf "expected an Abbrev shape, got %A" other
            }

            // Value-ness is a LAYOUT, not a type, and a record candidate carries identity +
            // field names only: both ride the wrapper's forward, so both are pinned here.
            test "non-type channels delegate unchanged" {
                Expect.equal wrapped.AmbientOpenPrefixes [ "Amb" ] "ambient delegated"
                Expect.equal (wrapped.IsValueType clsKey) (ValueSome true) "value-ness delegated"
                Expect.equal (wrapped.TryRecordsWithField "f") (EqArray.singleton candidate) "candidates delegated"
                Expect.isTrue (wrapped.TryLookupType "unknown" |> ValueOption.isNone) "unknown type misses"
                Expect.isTrue (wrapped.TryLookup "unknown" |> ValueOption.isNone) "unknown symbol misses"
            }
        ]
