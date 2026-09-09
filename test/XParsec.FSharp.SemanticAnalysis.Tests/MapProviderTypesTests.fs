module XParsec.FSharp.SemanticAnalysis.Tests.MapProviderTypesTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis

// The fixture plants `marker` in every position a provider puts a type (parameter, return,
// field, interface arg) and asserts which come back as a `witness` of the variance they were
// mapped at. ROOT variance only, not the algebra under a nested node.

let private origin = SymbolOrigin.Empty
let private marker = FTConst(RuntimeNames.opaqueKey "M", Block.empty)

let private witness (v: Variance) : FrozenType =
    let name =
        match v with
        | Variance.Co -> "co"
        | Variance.Contra -> "contra"
        | Variance.Inv -> "inv"

    FTConst(RuntimeNames.opaqueKey name, Block.empty)

/// Resolve ONLY the marker, to a witness of the variance that position was mapped at.
/// A surviving marker in the output ⇒ that position was left unmapped.
let private resolveMarker (v: Variance) (t: FrozenType) : FrozenType = if t = marker then witness v else t

let private clsKey = SymbolKeyOps.typeKeyOf origin.Namespace.Dotted "Cls"

/// Carries identity + field NAMES only, so nothing in it is a position to map.
let private candidate: ExternalRecordCandidate =
    {
        TypeKey = clsKey
        TyparArity = 0<sigSlot>
        FieldNames = Block.singleton "f"
        IsRequireQualifiedAccess = false
    }

let private markerMember: ExternalMember =
    { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf clsKey "m" Block.empty 0<typeSlot> MemberKind.Method) with
        Signature = TestHelpers.mkSignature 0<_> 0<_> marker marker
        Origin = origin
    }

let private markerCase: ExternalCaseShape =
    {
        Name = "C"
        FieldNames = Block.singleton ValueNone
        FrozenFieldTypes = Block.singleton marker
    }

// The by-key channels reach this lookup as the key's qualified name — `Cls`, `Rec`, `Uni`.
let private typeByName (name: string) : ExternalTypeShape voption =
    match name with
    | "Cls" ->
        ValueSome(
            ExternalTypeShape.Class
                { ExternalClassShape.basic (TyparList.empty, ClassCommitment.Class, origin) with
                    Members = Block.singleton markerMember
                    FrozenInterfaces =
                        Block.singleton (
                            NominalG.ofClass (SymbolKeyOps.qualifiedTypeKeyOf "I" 1) (Block.singleton marker)
                        )
                    FrozenBaseType =
                        ValueSome(NominalG.ofClass (SymbolKeyOps.qualifiedTypeKeyOf "B" 1) (Block.singleton marker))
                }
        )
    | "Rec" ->
        ValueSome(
            ExternalTypeShape.Record
                {
                    Typars = TyparList.positional 1<typeSlot>
                    Fields =
                        Block.singleton
                            {
                                Name = "f"
                                IsMutable = false
                                Frozen = marker
                            }
                    Origin = origin
                    IsValueType = false
                    RequiresQualifiedAccess = false
                }
        )
    | "Uni" ->
        ValueSome(
            ExternalTypeShape.Union
                {
                    Typars = TyparList.positional 1<typeSlot>
                    Cases = Block.singleton markerCase
                    Interfaces =
                        Block.singleton (
                            NominalG.ofClass (SymbolKeyOps.qualifiedTypeKeyOf "J" 1) (Block.singleton marker)
                        )
                    Origin = origin
                    IsValueType = false
                    RequiresQualifiedAccess = false
                }
        )
    | "Abb" ->
        ValueSome(
            ExternalTypeShape.Abbrev
                {
                    Typars = TyparList.positional 1<typeSlot>
                    Body = marker
                }
        )
    | _ -> ValueNone

let private memberByName (t: string) (m: string) : ExternalMember voption =
    if t = "Cls" && m = "m" then
        ValueSome markerMember
    else
        ValueNone

let private rootContainer = ModuleContainer.InNamespace NamespaceKey.Global

let private uniCase: ExternalUnionCase =
    {
        UnionKey = SymbolKeyOps.typeKeyOfArity origin.Namespace.Dotted "Uni" 1<typeSlot>
        UnionTypars = TyparList.positional 1<typeSlot>
        Case = markerCase
        IsRequireQualifiedAccess = false
    }

let private fakeScope: IScopeContents =
    { new IScopeContents with
        member _.TryContainer _ = ValueNone

        member _.TryValue key =
            if key.Name = "sym" then
                ValueSome(ExternalSymbols.monoFrozen (SymbolKeyOps.inNamespace "") "sym" marker)
            else
                ValueNone

        member _.UnionCasesNamed(_, name) =
            if name = "C" then Block.singleton uniCase else Block.empty

        member _.TypesNamed(_, name) =
            match typeByName name with
            | ValueSome shape ->
                Block.singleton (struct (SymbolKeyOps.qualifiedTypeKeyOf name (int shape.TyparArity), shape))
            | ValueNone -> Block.empty

        member _.DeclarationsOf _ = Block.empty
    }

// A key-channel source given a scope, so `mapProviderTypes` rewrites both.
let private fake: IExternalSymbolProvider =
    { new ExternalSymbolProviders.ProviderDecorator(ExternalSymbolProviders.nullProvider) with
        override _.Scope = fakeScope
        override _.ImplicitOpens = [ SymbolKeyOps.assemblyAutoOpen "Amb" ]

        override _.TryLookupType key =
            typeByName (SymbolKeyOps.typeMetaName key)

        override _.TryLookupMembers(key, memberName) =
            match memberByName (SymbolKeyOps.typeMetaName key) memberName with
            | ValueSome mem -> Block.singleton mem
            | ValueNone -> Block.empty

        override _.TryRecordsWithField fieldName =
            if fieldName = "f" then
                Block.singleton candidate
            else
                Block.empty

        override _.Platform =
            ValueSome
                { new IPlatformFacts with
                    member _.IsValueType _ = ValueSome true
                    member _.TupleType _ = ValueNone
                    member _.ConstEncoding(_, _) = ConstEncoding.Encodable
                }
    }

let private wrapped = ExternalSymbolProviders.mapProviderTypes resolveMarker fake

/// The mapped shape the compiled rendering `name` reaches, by key.
let private shapeNamed (name: string) : ExternalTypeShape voption =
    wrapped.TryLookupType(SymbolKeyOps.qualifiedTypeKeyOf name 0)

/// The `Class` shape of `Cls`, or fail.
let private clsShape () =
    match shapeNamed "Cls" with
    | ValueSome(ExternalTypeShape.Class info) -> info
    | other -> failtestf "expected a Class shape, got %A" other

[<Tests>]
let tests =
    testList
        "ExternalSymbolProviders.mapProviderTypes"
        [
            test "a symbol Scheme is mapped covariantly" {
                match ScopeContents.tryValueAt wrapped.Scope "sym" with
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

                Expect.equal (Block.toArray iface.Args) [| witness Variance.Inv |] "interface arg root is inv"
            }

            // As for an interface: a base type is a REFERENCE, so only its arguments are
            // positions a value passes through.
            test "base-type type-arguments are invariant" {
                let baseTy = (clsShape ()).FrozenBaseType.Value

                Expect.equal baseTy.Key (SymbolKeyOps.qualifiedTypeKeyOf "B" 1) "base type identity preserved"

                Expect.equal (Block.toArray baseTy.Args) [| witness Variance.Inv |] "base type arg root is inv"
            }

            test "a record field is covariant" {
                match shapeNamed "Rec" with
                | ValueSome(ExternalTypeShape.Record { Fields = fields }) ->
                    Expect.equal fields.[0].Frozen (witness Variance.Co) "record field root is co"
                | other -> failtestf "expected a Record shape, got %A" other
            }

            test "a union-case field is covariant and the union's interface args invariant" {
                match shapeNamed "Uni" with
                | ValueSome(ExternalTypeShape.Union { Cases = cases; Interfaces = ifaces }) ->
                    Expect.equal
                        cases.[0].FrozenFieldTypes
                        (Block.singleton (witness Variance.Co))
                        "case field root is co"

                    Expect.equal
                        ifaces.[0].Key
                        (SymbolKeyOps.qualifiedTypeKeyOf "J" 1)
                        "union interface identity preserved"

                    Expect.equal
                        (Block.toArray ifaces.[0].Args)
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

            test "the scope's union-case channel maps case fields covariantly" {
                match wrapped.Scope.UnionCasesNamed(rootContainer, "C") with
                | BlockOne uc ->
                    Expect.equal
                        uc.Case.FrozenFieldTypes
                        (Block.singleton (witness Variance.Co))
                        "scoped case field root is co"
                | other -> failtestf "expected one declaring union for 'C', got %A" other
            }

            test "the scope's value and type channels carry the same transform" {
                match wrapped.Scope.TryValue(SymbolKeyOps.bindingKeyOf rootContainer "sym") with
                | ValueSome s -> Expect.equal s.Scheme (witness Variance.Co) "scoped Scheme root is co"
                | ValueNone -> failtest "sym is declared in the root namespace"

                match wrapped.Scope.TypesNamed(rootContainer, "Rec") with
                | BlockOne(struct (_, ExternalTypeShape.Record { Fields = fields })) ->
                    Expect.equal fields.[0].Frozen (witness Variance.Co) "scoped record field root is co"
                | other -> failtestf "expected a Record shape for 'Rec', got %A" other
            }

            test "an Abbrev body is NOT threaded (no intrinsic variance) — the marker survives" {
                match shapeNamed "Abb" with
                | ValueSome(ExternalTypeShape.Abbrev { Body = body }) ->
                    Expect.equal body marker "the abbreviation body is left for its expansion seam"
                | other -> failtestf "expected an Abbrev shape, got %A" other
            }

            // Value-ness is a LAYOUT, not a type, and a record candidate carries identity +
            // field names only: the wrapper forwards both unchanged, so both are pinned here.
            test "non-type channels delegate unchanged" {
                Expect.equal wrapped.ImplicitOpens [ SymbolKeyOps.assemblyAutoOpen "Amb" ] "implicit opens delegated"
                Expect.equal (wrapped.IsValueType clsKey) (ValueSome true) "value-ness delegated"
                Expect.equal (wrapped.TryRecordsWithField "f") (Block.singleton candidate) "candidates delegated"
                Expect.isTrue (shapeNamed "unknown" |> ValueOption.isNone) "unknown type misses"

                Expect.isTrue
                    (ScopeContents.tryValueAt wrapped.Scope "unknown" |> ValueOption.isNone)
                    "unknown symbol misses"
            }
        ]
