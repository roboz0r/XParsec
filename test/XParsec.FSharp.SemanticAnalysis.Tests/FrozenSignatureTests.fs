module XParsec.FSharp.SemanticAnalysis.Tests.FrozenSignatureTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The frozen-implementation-file → `IExternalSymbolProvider` projection: file N's
// inferred signature as a provider view, so file N+1 resolves N's exports with no DLL
// emitted. Identities are read out of the frozen file and the provider asked to answer.

/// The pools re-authored as the `Pooled.TastFile` DU, so the assertions can read whole
/// decl trees and the bound-variable-keyed side tables.
let private duOf (frozen: FrozenPools) : Pooled.TastFile = TastUnpool.ofPools frozen

/// The `TypeKey` of the type declared under `name`, read out of the frozen decls.
let private typeKeyOf (frozen: FrozenPools) (name: string) : TypeKey =
    EqArray.toList (duOf frozen).Decls
    |> List.pick (
        function
        | TDeclG.Type td when td.Name = name -> Some td.TypeKey
        | _ -> None
    )

/// The augmentation members declared on the type named `name`.
let private membersOfType (frozen: FrozenPools) (name: string) : Pooled.TTypeMember list =
    EqArray.toList (duOf frozen).Decls
    |> List.pick (
        function
        | TDeclG.Type td when td.Name = name -> Some(EqArray.toList (TTypeKindG.members td.Kind))
        | _ -> None
    )

/// Every module-level binding's `(source name, SymbolKey)`. An `inline` binding rides
/// BOTH `Decls` and the inline vocabulary, so the concatenation can list it twice.
let private moduleBindings (frozen: FrozenPools) : (string * SymbolKey) list =
    let file = duOf frozen

    let fromDecls =
        EqArray.toList file.Decls
        |> List.choose (
            function
            | TDeclG.Let(pattern, _, _, _) ->
                match BoundVarKey.ofPat pattern with
                | ValueSome boundVar ->
                    match Map.tryFind boundVar file.ModuleMembers with
                    | Some info -> Some(info.Name, info.Key)
                    | None -> None
                | ValueNone -> None
            | _ -> None
        )

    let fromInline =
        EqArray.toList file.InlineBodies
        |> List.choose (fun iv ->
            match iv.Key with
            | SymbolKey.Binding bk -> Some(bk.Name, iv.Key)
            | _ -> None
        )

    fromDecls @ fromInline

let private bindingKey (frozen: FrozenPools) (name: string) : SymbolKey =
    moduleBindings frozen |> List.find (fun (n, _) -> n = name) |> snd

/// One integer per curried group (`0` = a `unit` group, `1` = simple, `N` = a tuple of
/// `N`), so it pins the curried/tupled/mixed grouping without naming any type.
let private groupShape (vr: TastAccessor.ValRepr voption) : int list option =
    match vr with
    | ValueNone -> None
    | ValueSome v ->
        v.Groups
        |> List.map (
            function
            | ArgGroupG.GUnit _ -> 0
            | ArgGroupG.GSimple _ -> 1
            | ArgGroupG.GTuple pat -> (TastAccessor.patChildren pat).Length

        )
        |> Some

// --- projection source: one of each exported decl kind + a private + an internal ---
let private projectionSrc =
    "\
namespace Test.Sig

module M =
    type Box<'T> = { value: 'T }

    type Opt<'T> =
        | Nope
        | Just of 'T

    type Widget =
        { size: int }
        member this.Doubled = this.size

    [<Struct>]
    type Point = { x: int; y: int }

    let answer = 42
    let ident (x: 'a) : 'a = x
    let inline twice (x: int) = x + x
    let private secret = 99
    let internal shared = 7
"

[<Tests>]
let tests =
    testList
        "FrozenSignature projection"
        [
            test "record / union / class types project with home-assembly origin" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let provider = FrozenSignature.toSignatures origin frozen
                let store = provider :> IExternalSymbolStore

                match store.TryLookupType(typeKeyOf frozen "Box") with
                | ValueSome(ExternalTypeShape.Record(1, fields, origin, isValueType)) ->
                    Expect.equal (fields |> EqArray.map (fun f -> f.Name)) (EqArray.ofSeq [ "value" ]) "Box field names"
                    Expect.equal origin.Home.AssemblyOption (ValueSome testAsm) "Box carries home-assembly origin"
                    Expect.isFalse isValueType "a plain record projects as a reference layout"
                | other -> failtestf "Box did not project as a generic Record: %A" other

                // Value-ness decides `when 'a : struct` in a CONSUMING unit, which reads only
                // this shape, so the `[<Struct>]` has to survive the freeze.
                match store.TryLookupType(typeKeyOf frozen "Point") with
                | ValueSome(ExternalTypeShape.Record(isValueType = isValueType)) ->
                    Expect.isTrue isValueType "a [<Struct>] record projects as a value layout"
                | other -> failtestf "Point did not project as a Record: %A" other

                match store.TryLookupType(typeKeyOf frozen "Opt") with
                | ValueSome(ExternalTypeShape.Union(1, cases, _, origin)) ->
                    Expect.equal
                        (cases |> EqArray.map (fun c -> c.Name))
                        (EqArray.ofSeq [ "Nope"; "Just" ])
                        "Opt case names"

                    Expect.equal origin.Home.AssemblyOption (ValueSome testAsm) "Opt carries home-assembly origin"
                | other -> failtestf "Opt did not project as a generic Union: %A" other
            }

            test "union cases resolve by bare case name" {
                let origin, frozen = freezeWithOrigin projectionSrc

                let resolver = FrozenSignature.toSignatures origin frozen :> IExternalSymbolResolver

                match resolver.TryLookupUnionCase "Just" with
                | ValueSome uc ->
                    Expect.equal uc.TyparArity 1 "Just's declaring union arity"
                    Expect.equal uc.Case.Name "Just" "matched case name"
                | ValueNone -> failtest "union case 'Just' did not resolve"

                Expect.isSome
                    (resolver.TryLookupUnionCase "Nope"
                     |> function
                         | ValueSome _ -> Some()
                         | _ -> None)
                    "Nope resolves"
            }

            test "augmentation members project on the store view" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let store = FrozenSignature.toSignatures origin frozen :> IExternalSymbolStore
                let widgetKey = typeKeyOf frozen "Widget"

                let memberName = membersOfType frozen "Widget" |> List.head |> (fun m -> m.Name)

                let byName = store.TryLookupMembers(widgetKey, memberName)
                Expect.isNonEmpty byName (sprintf "Widget member '%s' resolves" memberName)
                Expect.isFalse byName.[0].IsStatic "instance member"
            }

            test "module values project; mono vs generic arity preserved" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let provider = FrozenSignature.toSignatures origin frozen
                let store = provider :> IExternalSymbolStore
                let resolver = provider :> IExternalSymbolResolver

                let answerKey = bindingKey frozen "answer"

                match store.TryLookupByKey answerKey with
                | ValueSome s ->
                    Expect.equal s.TyparArity 0 "answer is monomorphic"
                    // The resolver view answers the SAME entry by rendered name.
                    Expect.isSome
                        (resolver.TryLookup(SymbolKeyOps.qualifiedName answerKey)
                         |> function
                             | ValueSome _ -> Some()
                             | _ -> None)
                        "answer resolves by name"
                | ValueNone -> failtest "answer did not project"

                match store.TryLookupByKey(bindingKey frozen "ident") with
                | ValueSome s -> Expect.equal s.TyparArity 1 "ident has one typar"
                | ValueNone -> failtest "ident did not project"
            }

            test "let inline is published as a signature, and its body is a separate object" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let key = bindingKey frozen "twice"
                let signatures = FrozenSignature.toSignatures origin frozen

                match (signatures :> IExternalSymbolStore).TryLookupByKey key with
                | ValueSome s -> Expect.isTrue s.InlineBody.IsNone "the signatures half carries no template"
                | ValueNone -> failtest "twice did not project"

                let bodies = InlineBodies.index (InlineBodies.collect origin frozen)

                Expect.isTrue (bodies key).IsSome "the bodies half is keyed by the binding key"

                // The file's view is the two layered, which is what a later file resolves.
                let view =
                    ExternalSymbolProviders.withInlineBodies bodies signatures :> IExternalSymbolStore

                match view.TryLookupByKey key with
                | ValueSome s -> Expect.isTrue s.InlineBody.IsSome "twice publishes its inline body"
                | ValueNone -> failtest "twice did not project"
            }

            test "internal-or-better filter: private dropped, internal kept" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let provider = FrozenSignature.toSignatures origin frozen
                let store = provider :> IExternalSymbolStore
                let resolver = provider :> IExternalSymbolResolver

                let secretKey = bindingKey frozen "secret"
                Expect.equal (store.TryLookupByKey secretKey) ValueNone "private 'secret' is NOT exported (by key)"

                Expect.equal
                    (resolver.TryLookup(SymbolKeyOps.qualifiedName secretKey))
                    ValueNone
                    "private 'secret' is NOT exported (by name)"

                let sharedKey = bindingKey frozen "shared"

                match store.TryLookupByKey sharedKey with
                | ValueSome _ -> ()
                | ValueNone -> failtest "internal 'shared' MUST be exported"
            }

            test "IntrinsicTypeMap passes this file's IntrinsicReprKeys through verbatim" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let store = FrozenSignature.toSignatures origin frozen :> IExternalSymbolStore
                // A plain impl file declares no intrinsics, so both sides are empty and the
                // wiring is all that is asserted.
                Expect.equal
                    (IntrinsicTypeMap.entries store.IntrinsicTypeMap).Length
                    frozen.Residue.IntrinsicReprKeys.Count
                    "declaration count matches source"
            }

            // --- parity oracle: projected ExternalSymbol ≡ the .fsi-extracted one -------
            // The projection reads a frozen `.fs`, the front end the matching `.fsi`; for one
            // signature the two must agree on typar arity, scheme, and `ValRepr` grouping.

            test "projected symbols agree with the .fsi-extracted ones (schemesAgree oracle)" {
                let implSrc =
                    "\
namespace Test.P

module M =
    let mono = 42
    let gid (x: 'a) : 'a = x
    let curried (a: int) (b: int) : int = a + b
    let tupled (a: int, b: int) : int = a + b
    let mixed (a: int) (b: int, c: int) : int = a + b + c
    let (++) (a: int) (b: int) : int = a + b

    module Inner =
        let deep (x: 'a) : 'a = x
"

                let sigSrc =
                    "\
namespace Test.P

module M =
    val mono: int
    val gid: 'a -> 'a
    val curried: int -> int -> int
    val tupled: int * int -> int
    val mixed: int -> int * int -> int
    val (++): int -> int -> int

    module Inner =
        val deep: 'a -> 'a
"

                // Project the `.fs`.
                let origin, frozen = freezeWithOrigin implSrc
                let store = FrozenSignature.toSignatures origin frozen :> IExternalSymbolStore

                // Resolve the `.fsi` against the SAME provider the implementation was frozen
                // over, so both sides mint one `int` identity.
                let sigSurface =
                    match ParseChain.parseSignature Set.empty sigSrc with
                    | Result.Error f -> failtestf "parse failed: %A" [ for d in f.Diagnostics -> d.Message ]
                    | Result.Ok parsed ->
                        Passes.SignatureResolution.resolveFile
                            realProvider.Value
                            (AssemblyFiles.fileSource "P" (AssemblyFileId.ofRelative "p.fsi") parsed.Lexed)
                            {
                                Assembly = "P"
                                Target = "none"
                                Reprs = System.Collections.Generic.Dictionary()
                            }
                            parsed.File
                        |> fst

                let fsiSymbolBySuffix (name: string) : ExternalSymbol option =
                    let mutable found = None

                    for entry: SurfaceEntry<string, ExternalSymbol> in sigSurface.Symbols do
                        if found.IsNone && entry.Key.EndsWith("." + name) then
                            found <- Some entry.Value

                    found

                // Every projected module binding must match its `.fsi` twin.
                let mutable checked' = 0

                for (name, key) in moduleBindings frozen do
                    match store.TryLookupByKey key, fsiSymbolBySuffix name with
                    | ValueSome proj, Some fsi ->
                        checked' <- checked' + 1
                        Expect.equal proj.TyparArity fsi.TyparArity (sprintf "%s: typar arity agrees" name)

                        Expect.isTrue
                            (ConformanceTypars.schemesAgree fsi.Scheme proj.Scheme)
                            (sprintf "%s: axis-normalized scheme agrees (fsi=%A proj=%A)" name fsi.Scheme proj.Scheme)

                        Expect.equal
                            (groupShape proj.ValRepr)
                            (groupShape fsi.ValRepr)
                            (sprintf "%s: ValRepr grouping agrees" name)
                    | ValueNone, _ -> failtestf "%s: not projected from the .fs" name
                    | _, None -> failtestf "%s: not extracted from the .fsi" name

                // Guard against a vacuous pass: the whole matrix must have been exercised.
                Expect.isGreaterThanOrEqual checked' 7 "all parity bindings compared"
            }

            // --- reverse record-field index ---------------------------------------------
            // `TryRecordsWithField` is a `field-name -> [records]` MULTIMAP, not first-wins.

            test "TryRecordsWithField indexes each record under every field name" {
                let src =
                    "\
namespace Test.RF

module M =
    type R = { X: int; Y: int }
"

                let origin, frozen = freezeWithOrigin src

                let resolver = FrozenSignature.toSignatures origin frozen :> IExternalSymbolResolver

                let rName = SymbolKeyOps.typeMetaName (typeKeyOf frozen "R")

                for field in [ "X"; "Y" ] do
                    match resolver.TryRecordsWithField field with
                    | EqOne c ->
                        Expect.equal
                            (SymbolKeyOps.typeMetaName c.TypeKey)
                            rName
                            (sprintf "field %s -> R's compiled name" field)

                        Expect.equal c.TyparArity 0 "R is monomorphic"
                        Expect.equal c.FieldNames (EqArray.ofSeq [ "X"; "Y" ]) "R's field names"
                    | other -> failtestf "field %s did not resolve to exactly one record: %A" field other

                Expect.equal (resolver.TryRecordsWithField "Z") EqArray.empty "unknown field 'Z' has no candidates"
            }

            // --- enum projection: a frozen enum's case→literal table projects to an `Enum`
            // shape under its nominal key, so a later file resolves `(x: E)` / `E.Ci`. The
            // numeric width is dropped — `ExternalEnumCaseValue` is `int64` or `string`.

            test "numeric enum projects its cases with int64 values under home origin" {
                let src =
                    "\
namespace Test.En

module M =
    type Direction =
        | Up = 0
        | Down = 1
"

                let origin, frozen = freezeWithOrigin src
                let store = FrozenSignature.toSignatures origin frozen :> IExternalSymbolStore

                match store.TryLookupType(typeKeyOf frozen "Direction") with
                | ValueSome(ExternalTypeShape.Enum(cases, origin)) ->
                    Expect.equal
                        (cases |> EqArray.map (fun c -> c.Name))
                        (EqArray.ofSeq [ "Up"; "Down" ])
                        "case names in source order"

                    Expect.equal
                        (cases |> EqArray.map (fun c -> c.Value))
                        (EqArray.ofSeq [ ExternalEnumCaseValue.IntVal 0L; ExternalEnumCaseValue.IntVal 1L ])
                        "case int64 values"

                    Expect.equal origin.Home.AssemblyOption (ValueSome testAsm) "Direction carries home-assembly origin"
                | other -> failtestf "Direction did not project as an Enum: %A" other
            }

            test "string enum projects its cases with string values" {
                let src =
                    "\
namespace Test.En

module M =
    type Mode =
        | On = \"on\"
        | Off = \"off\"
"

                let origin, frozen = freezeWithOrigin src
                let store = FrozenSignature.toSignatures origin frozen :> IExternalSymbolStore

                match store.TryLookupType(typeKeyOf frozen "Mode") with
                | ValueSome(ExternalTypeShape.Enum(cases, _)) ->
                    Expect.equal
                        (cases |> EqArray.map (fun c -> c.Value))
                        (EqArray.ofSeq [ ExternalEnumCaseValue.StringVal "on"; ExternalEnumCaseValue.StringVal "off" ])
                        "case string values"
                | other -> failtestf "Mode did not project as an Enum: %A" other
            }

            test "TryRecordsWithField reports a generic record's typar arity" {
                let src =
                    "\
namespace Test.RF

module M =
    type Box<'T> = { Value: 'T }
"

                let origin, frozen = freezeWithOrigin src

                let resolver = FrozenSignature.toSignatures origin frozen :> IExternalSymbolResolver

                match resolver.TryRecordsWithField "Value" with
                | EqOne c ->
                    Expect.equal
                        (SymbolKeyOps.typeMetaName c.TypeKey)
                        (SymbolKeyOps.typeMetaName (typeKeyOf frozen "Box"))
                        "Box's compiled name"

                    Expect.equal c.TyparArity 1 "Box has one typar"
                | other -> failtestf "field Value did not resolve to exactly one record: %A" other
            }

            test "TryRecordsWithField returns ALL records sharing a field name (multimap append)" {
                let src =
                    "\
namespace Test.RF

module M =
    type A = { Shared: int; OnlyA: int }
    type B = { Shared: int; OnlyB: int }
"

                let origin, frozen = freezeWithOrigin src

                let resolver = FrozenSignature.toSignatures origin frozen :> IExternalSymbolResolver

                let aName = SymbolKeyOps.typeMetaName (typeKeyOf frozen "A")
                let bName = SymbolKeyOps.typeMetaName (typeKeyOf frozen "B")

                let shared =
                    resolver.TryRecordsWithField "Shared"
                    |> EqArray.map (fun c -> SymbolKeyOps.typeMetaName c.TypeKey)
                    |> EqArray.sort

                Expect.equal
                    shared
                    (EqArray.sort (EqArray.ofSeq [ aName; bName ]))
                    "'Shared' resolves to BOTH records, not first-wins"

                // The record-specific fields still pin their single owner.
                Expect.equal
                    (resolver.TryRecordsWithField "OnlyA"
                     |> EqArray.map (fun c -> SymbolKeyOps.typeMetaName c.TypeKey))
                    (EqArray.singleton aName)
                    "'OnlyA' resolves to A alone"
            }
        ]
