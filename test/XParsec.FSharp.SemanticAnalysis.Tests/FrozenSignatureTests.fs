module XParsec.FSharp.SemanticAnalysis.Tests.FrozenSignatureTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The frozen-implementation-file → `IExternalSymbolProvider` projection: file N's
// inferred signature as a provider view, so file N+1 resolves N's exports by name
// with no DLL emitted. Asserts against the provider INTERFACE (both faces), never
// the projector's internal tables; identities are read out of the frozen file (the
// projection's INPUT) and the provider is asked to answer them.

/// The pooled file as the `Frozen.TastFile` DU. The assertions below read whole decl
/// trees and the binder-keyed side tables, which is what `ofPools` re-authors
/// verbatim — the projection's INPUT is the pools, but its shape reads most directly
/// here.
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

/// Every module-level binding's `(source name, SymbolKey)`. An `inline` binding is
/// EMITTED as an ordinary module function AND published as a template, so it rides both
/// `Decls` and the inline vocabulary; the two halves are unioned and deduplicated by the
/// callers' `List.find`.
let private moduleBindings (frozen: FrozenPools) : (string * SymbolKey) list =
    let file = duOf frozen

    let fromDecls =
        EqArray.toList file.Decls
        |> List.choose (
            function
            | TDeclG.Let(head, _, _, _) ->
                match BinderKey.ofPat head with
                | ValueSome binder ->
                    match Map.tryFind binder file.ModuleMembers with
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

/// The grouping SHAPE of a `ValRepr` — one integer per curried group (`0` = lone
/// `unit`, `1` = simple, `N` = a tuple of `N`). Typar-axis-independent, so it pins
/// the curried/tupled/mixed grouping without depending on primitive canon keys.
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
                let provider = FrozenSignature.toProvider testAsm origin frozen
                let store = provider :> IExternalSymbolStore

                match store.TryLookupType(SymbolKey.Type(typeKeyOf frozen "Box")) with
                | ValueSome(ExternalTypeShape.Record(1, fields, origin)) ->
                    Expect.equal (fields |> Array.map (fun f -> f.Name)) [| "value" |] "Box field names"
                    Expect.equal origin.Home.AssemblyOption (ValueSome testAsm) "Box carries home-assembly origin"
                | other -> failtestf "Box did not project as a generic Record: %A" other

                match store.TryLookupType(SymbolKey.Type(typeKeyOf frozen "Opt")) with
                | ValueSome(ExternalTypeShape.Union(1, cases, _, origin)) ->
                    Expect.equal (cases |> Array.map (fun c -> c.Name)) [| "Nope"; "Just" |] "Opt case names"
                    Expect.equal origin.Home.AssemblyOption (ValueSome testAsm) "Opt carries home-assembly origin"
                | other -> failtestf "Opt did not project as a generic Union: %A" other
            }

            test "union cases resolve by bare case name" {
                let origin, frozen = freezeWithOrigin projectionSrc

                let resolver =
                    FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolResolver

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

            test "augmentation members project on the store face" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let store = FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolStore
                let widgetKey = SymbolKey.Type(typeKeyOf frozen "Widget")

                let memberName = membersOfType frozen "Widget" |> List.head |> (fun m -> m.Name)

                let byName = store.TryLookupMembers(widgetKey, memberName)
                Expect.isNonEmpty byName (sprintf "Widget member '%s' resolves" memberName)
                Expect.isFalse byName.[0].IsStatic "instance member"
            }

            test "module values project; mono vs generic arity preserved" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let provider = FrozenSignature.toProvider testAsm origin frozen
                let store = provider :> IExternalSymbolStore
                let resolver = provider :> IExternalSymbolResolver

                let answerKey = bindingKey frozen "answer"

                match store.TryLookupByKey answerKey with
                | ValueSome s ->
                    Expect.equal s.TyparArity 0 "answer is monomorphic"
                    // The resolver face answers the SAME entry by rendered name.
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

            test "let inline carries a frozen inline body" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let store = FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolStore

                match store.TryLookupByKey(bindingKey frozen "twice") with
                | ValueSome s -> Expect.isTrue s.InlineBody.IsSome "twice publishes its inline body"
                | ValueNone -> failtest "twice did not project"
            }

            test "internal-or-better filter: private dropped, internal kept" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let provider = FrozenSignature.toProvider testAsm origin frozen
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

            test "IntrinsicForwardRepr passes this unit's IntrinsicReprKeys through verbatim" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let store = FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolStore
                // A plain impl unit declares no intrinsics, so the forward axis is its
                // (empty) `IntrinsicReprKeys` — the wiring is the assertion.
                Expect.equal
                    (Seq.length store.IntrinsicForwardRepr)
                    (Seq.length frozen.Residue.IntrinsicReprKeys)
                    "forward repr count matches source"
            }

            // --- parity oracle: projected ExternalSymbol ≡ the .fsi-extracted one -------
            //
            // The `schemesAgree` / `normAxis` style: the projection reads a frozen `.fs`,
            // the extractor reads the matching `.fsi`; for the same signature the two must
            // agree on typar arity, axis-normalized scheme, and `ValRepr` grouping. Covers
            // mono / generic value, curried / tupled / mixed function, an operator, and a
            // nested-module binding.

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
                let store = FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolStore

                // Extract the `.fsi`, canonicalizing primitives through the SAME provider
                // the front end used, so both sides mint one `int` identity.
                let sigLexed, sigAst =
                    match Lexing.lexString sigSrc with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed ->
                        let reader = Reader.ofLexed lexed sigSrc Set.empty

                        match FSharpAst.parseSignature reader with
                        | Result.Error e -> failtestf "parse failed: %A" e
                        | Result.Ok ast -> lexed, ast

                let parsed: VesperLibManifest.ParsedFile =
                    {
                        File =
                            {
                                Path = { BucketName = "P"; Relative = "p.fsi" }
                                Absolute = "p.fsi"
                            }
                        Input = sigSrc
                        Lexed = sigLexed
                        Ast = sigAst
                    }

                let ectx = VesperLib.ExtractCtx.empty ()
                ectx.AmbientShapes <- (fun n -> realProvider.Value.TryLookupType n |> ExternalSymbols.typeShapeOf)
                VesperLib.extractSymbols ectx parsed
                VesperLib.finalizeDeferred ectx

                let fsiSymbolBySuffix (name: string) : ExternalSymbol option =
                    let mutable found = None

                    for kv in ectx.Symbols do
                        if found.IsNone && kv.Key.EndsWith("." + name) then
                            found <- Some kv.Value

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

            // --- reverse record-field index (`TryRecordsWithField`) ---------------------
            //
            // The record analogue of the union-case index: a `field-name -> [records]`
            // MULTIMAP. Asserts the resolver face only, against each candidate's identity
            // (`TypeKey` / `TyparArity` / `FieldNames`), never object identity.

            test "TryRecordsWithField indexes each record under every field name" {
                let src =
                    "\
namespace Test.RF

module M =
    type R = { X: int; Y: int }
"

                let origin, frozen = freezeWithOrigin src

                let resolver =
                    FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolResolver

                let rName = SymbolKeyOps.typeMetaName (typeKeyOf frozen "R")

                for field in [ "X"; "Y" ] do
                    match resolver.TryRecordsWithField field with
                    | [| c |] ->
                        Expect.equal
                            (SymbolKeyOps.typeMetaName c.TypeKey)
                            rName
                            (sprintf "field %s -> R's compiled name" field)

                        Expect.equal c.TyparArity 0 "R is monomorphic"
                        Expect.equal c.FieldNames [| "X"; "Y" |] "R's field names"
                    | other -> failtestf "field %s did not resolve to exactly one record: %A" field other

                Expect.equal (resolver.TryRecordsWithField "Z") [||] "unknown field 'Z' has no candidates"
            }

            // --- enum projection (`ExternalTypeShape.Enum`) --------------------------------
            //
            // A frozen enum projects its closed case→literal table to an `Enum` shape under
            // its nominal key, so a later file resolves `(x: E)` / `E.Ci` against it (was
            // left unregistered — a nominal `TyConst` fallback). Numeric and string cases
            // carry their compile-time value; the numeric width is intentionally dropped
            // (`ExternalEnumCaseValue` has none).

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
                let store = FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolStore

                match store.TryLookupType(SymbolKey.Type(typeKeyOf frozen "Direction")) with
                | ValueSome(ExternalTypeShape.Enum(cases, origin)) ->
                    Expect.equal (cases |> Array.map (fun c -> c.Name)) [| "Up"; "Down" |] "case names in source order"

                    Expect.equal
                        (cases |> Array.map (fun c -> c.Value))
                        [| ExternalEnumCaseValue.IntVal 0L; ExternalEnumCaseValue.IntVal 1L |]
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
                let store = FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolStore

                match store.TryLookupType(SymbolKey.Type(typeKeyOf frozen "Mode")) with
                | ValueSome(ExternalTypeShape.Enum(cases, _)) ->
                    Expect.equal
                        (cases |> Array.map (fun c -> c.Value))
                        [|
                            ExternalEnumCaseValue.StringVal "on"
                            ExternalEnumCaseValue.StringVal "off"
                        |]
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

                let resolver =
                    FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolResolver

                match resolver.TryRecordsWithField "Value" with
                | [| c |] ->
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

                let resolver =
                    FrozenSignature.toProvider testAsm origin frozen :> IExternalSymbolResolver

                let aName = SymbolKeyOps.typeMetaName (typeKeyOf frozen "A")
                let bName = SymbolKeyOps.typeMetaName (typeKeyOf frozen "B")

                let shared =
                    resolver.TryRecordsWithField "Shared"
                    |> Array.map (fun c -> SymbolKeyOps.typeMetaName c.TypeKey)
                    |> Array.sort

                Expect.equal shared (Array.sort [| aName; bName |]) "'Shared' resolves to BOTH records, not first-wins"

                // The record-specific fields still pin their single owner.
                Expect.equal
                    (resolver.TryRecordsWithField "OnlyA"
                     |> Array.map (fun c -> SymbolKeyOps.typeMetaName c.TypeKey))
                    [| aName |]
                    "'OnlyA' resolves to A alone"
            }
        ]
