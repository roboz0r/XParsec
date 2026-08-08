module XParsec.FSharp.SemanticAnalysis.Tests.MapProviderTypesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The fixture plants `marker` at every value-flow surface of a fake provider and asserts
// which ones come back as a `witness` of the variance they were mapped at. Coverage and
// ROOT variance only — not the variance algebra under a nested node.

let private origin = SymbolOrigin.Empty
let private marker = FTConst(RuntimeNames.opaqueKey "M", EqArray.empty)

let private witness (v: Variance) : FrozenType =
    let name =
        match v with
        | Variance.Co -> "co"
        | Variance.Contra -> "contra"
        | Variance.Inv -> "inv"

    FTConst(RuntimeNames.opaqueKey name, EqArray.empty)

/// Resolve ONLY the marker, to a witness of the variance the surface was mapped at.
/// A surviving marker in the output ⇒ that surface was left unmapped.
let private resolveMarker (v: Variance) (t: FrozenType) : FrozenType = if t = marker then witness v else t

let private markerMember: ExternalMember =
    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf
              (SymbolKeyOps.typeKeyOf origin.Namespace.Dotted "Cls")
              "m"
              EqArray.empty
              0
              MemberKind.Method
      ) with
        Signature = TestHelpers.mkSignature 0 0 marker marker
        Origin = origin
    }

let private markerCase: ExternalCaseShape =
    {
        Name = "C"
        FieldNames = [| ValueNone |]
        FrozenFieldTypes = [| marker |]
    }

// The by-key channels reach this leaf as the key's qualified name — `Cls`, `Rec`, `Uni`.
let private typeByName (name: string) : ExternalTypeShape voption =
    match name with
    | "Cls" ->
        ValueSome(
            ExternalTypeShape.Class
                { ExternalClassShape.basic (0, false, origin) with
                    Members = [| markerMember |]
                    FrozenInterfaces = [| "I", [| marker |] |]
                    FrozenBaseType = ValueSome marker
                }
        )
    | "Rec" ->
        ValueSome(
            ExternalTypeShape.Record(
                1,
                [|
                    {
                        Name = "f"
                        IsMutable = false
                        Frozen = marker
                    }
                |],
                origin
            )
        )
    | "Uni" -> ValueSome(ExternalTypeShape.Union(1, [| markerCase |], [| "J", [| marker |] |], origin))
    | "Abb" -> ValueSome(ExternalTypeShape.Abbrev(1, marker))
    | _ -> ValueNone

let private memberByName (t: string) (m: string) : ExternalMember voption =
    if t = "Cls" && m = "m" then
        ValueSome markerMember
    else
        ValueNone

let private fake: IExternalSymbolProvider =
    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookup =
                fun name ->
                    if name = "sym" then
                        ValueSome(ExternalSymbols.monoFrozen (SymbolKeyOps.inNamespace "") "sym" marker)
                    else
                        ValueNone
            TryLookupType = typeByName
            TryLookupUnionCase =
                fun caseName ->
                    if caseName = "C" then
                        ValueSome
                            {
                                UnionName = "Uni"
                                TyparArity = 1
                                Origin = origin
                                Case = markerCase
                                IsRequireQualifiedAccess = false
                            }
                    else
                        ValueNone
            AmbientOpenPrefixes = [ "Amb" ]
            TryLookupMember = fun (t, m) -> memberByName t m
            TryLookupMembers =
                fun (t, m) ->
                    match memberByName t m with
                    | ValueSome mem -> [| mem |]
                    | ValueNone -> [||]
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
                Expect.equal m.Signature.Parameters (witness Variance.Contra) "Parameters root is contra"
                Expect.equal m.Signature.Return (witness Variance.Co) "Return root is co"
            }

            test "interface type-arguments are invariant" {
                let info = clsShape ()
                let (name, args) = info.FrozenInterfaces.[0]
                Expect.equal name "I" "interface name preserved"
                Expect.equal args [| witness Variance.Inv |] "interface arg root is inv"
            }

            test "the base type is invariant" {
                let info = clsShape ()
                Expect.equal info.FrozenBaseType (ValueSome(witness Variance.Inv)) "base type root is inv"
            }

            test "a record field is covariant" {
                match wrapped.TryLookupType "Rec" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Record(_, fields, _)) ->
                    Expect.equal fields.[0].Frozen (witness Variance.Co) "record field root is co"
                | other -> failtestf "expected a Record shape, got %A" other
            }

            test "a union-case field is covariant and the union's interface args invariant" {
                match wrapped.TryLookupType "Uni" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Union(_, cases, ifaces, _)) ->
                    Expect.equal cases.[0].FrozenFieldTypes [| witness Variance.Co |] "case field root is co"
                    let (name, args) = ifaces.[0]
                    Expect.equal name "J" "union interface name preserved"
                    Expect.equal args [| witness Variance.Inv |] "union interface arg root is inv"
                | other -> failtestf "expected a Union shape, got %A" other
            }

            test "the member channels (single + overloads) map identically to the class member" {
                match wrapped.TryLookupMember(SymbolKeyOps.qualifiedTypeKey "Cls" 0, "m") with
                | ValueSome m ->
                    Expect.equal m.Signature.Parameters (witness Variance.Contra) "single: Parameters contra"
                    Expect.equal m.Signature.Return (witness Variance.Co) "single: Return co"
                | ValueNone -> failtest "member should resolve"

                let all = wrapped.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey "Cls" 0, "m")
                Expect.equal all.Length 1 "one overload"
                Expect.equal all.[0].Signature.Parameters (witness Variance.Contra) "overloads: Parameters contra"
                Expect.equal all.[0].Signature.Return (witness Variance.Co) "overloads: Return co"
            }

            test "the reverse union-case channel maps case fields covariantly" {
                match wrapped.TryLookupUnionCase "C" with
                | ValueSome uc ->
                    Expect.equal uc.Case.FrozenFieldTypes [| witness Variance.Co |] "reverse case field root is co"
                | ValueNone -> failtest "union case should resolve"
            }

            test "an Abbrev body is NOT threaded (no intrinsic variance) — the marker survives" {
                match wrapped.TryLookupType "Abb" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Abbrev(_, body)) ->
                    Expect.equal body marker "the abbreviation body is left for its expansion seam"
                | other -> failtestf "expected an Abbrev shape, got %A" other
            }

            test "non-type channels delegate unchanged" {
                Expect.equal wrapped.AmbientOpenPrefixes [ "Amb" ] "ambient delegated"
                Expect.isTrue (wrapped.TryLookupType "unknown" |> ValueOption.isNone) "unknown type misses"
                Expect.isTrue (wrapped.TryLookup "unknown" |> ValueOption.isNone) "unknown symbol misses"
            }
        ]
