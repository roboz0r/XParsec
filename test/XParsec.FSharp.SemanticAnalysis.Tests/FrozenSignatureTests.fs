module XParsec.FSharp.SemanticAnalysis.Tests.FrozenSignatureTests

open Vesper
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The frozen-implementation-file → `IExternalSymbolProvider` projection: file N's
// inferred signature as a provider view, so file N+1 resolves N's exports with no DLL
// emitted. Identities are read out of the frozen file and resolved through the provider.

/// The pools re-authored as the `Pooled.TastFile` DU, so the assertions can read whole
/// decl trees and the bound-variable-keyed side tables.
let private duOf (frozen: FrozenPools) : Pooled.TastFile = TastUnpool.ofPools frozen

/// The `TypeKey` of the type declared under `name`, read out of the frozen decls.
let private typeKeyOf (frozen: FrozenPools) (name: string) : TypeKey =
    Block.toList (duOf frozen).Decls
    |> List.pick (
        function
        | TDeclG.Type td when td.Name = name -> Some td.TypeKey
        | _ -> None
    )

/// The augmentation members declared on the type named `name`.
let private membersOfType (frozen: FrozenPools) (name: string) : Pooled.TTypeMember list =
    Block.toList (duOf frozen).Decls
    |> List.pick (
        function
        | TDeclG.Type td when td.Name = name -> Some(Block.toList (TTypeKindG.members td.Kind))
        | _ -> None
    )

/// Every module-level binding's `(source name, BindingKey)`. An `inline` binding appears in
/// BOTH `Decls` and the inline vocabulary, so the concatenation can list it twice.
let private moduleBindings (frozen: FrozenPools) : (string * BindingKey) list =
    let file = duOf frozen

    let fromDecls =
        Block.toList file.Decls
        |> List.choose (
            function
            | TDeclG.Let({ Pattern = pattern }, _, _) ->
                match BoundVarKey.ofPat pattern with
                | ValueSome boundVar ->
                    match Map.tryFind boundVar file.ModuleMembers with
                    | Some info -> Some(info.Name, info.BindingKey)
                    | None -> None
                | ValueNone -> None
            | _ -> None
        )

    let fromInline =
        Block.toList file.InlineBodies
        |> List.choose (fun iv ->
            match iv.Key with
            | SymbolKey.Binding bk -> Some(bk.Name, bk)
            | _ -> None
        )

    fromDecls @ fromInline

let private bindingKey (frozen: FrozenPools) (name: string) : BindingKey =
    moduleBindings frozen |> List.find (fun (n, _) -> n = name) |> snd

/// One integer per curried group (`0` = a `unit` group, `1` = simple, `N` = a tuple of
/// `N`), so it pins the curried/tupled/mixed grouping without referring to any type.
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

    let total = 42
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
                | ValueSome(ExternalTypeShape.Record r) when r.TyparArity = 1 ->
                    Expect.equal (r.Fields |> Block.map (fun f -> f.Name)) (Block.ofSeq [ "value" ]) "Box field names"

                    Expect.equal r.Origin.Home.AssemblyOption (ValueSome testAsm) "Box carries home-assembly origin"
                    Expect.isFalse r.IsValueType "a plain record projects as a reference layout"
                | other -> failtestf "Box did not project as a generic Record: %A" other

                // Value-ness decides `when 'a : struct` in a CONSUMING unit, which reads only
                // this shape, so the `[<Struct>]` has to survive the freeze.
                match store.TryLookupType(typeKeyOf frozen "Point") with
                | ValueSome(ExternalTypeShape.Record { IsValueType = isValueType }) ->
                    Expect.isTrue isValueType "a [<Struct>] record projects as a value layout"
                | other -> failtestf "Point did not project as a Record: %A" other

                match store.TryLookupType(typeKeyOf frozen "Opt") with
                | ValueSome(ExternalTypeShape.Union u) when u.TyparArity = 1 ->
                    Expect.equal
                        (u.Cases |> Block.map (fun c -> c.Name))
                        (Block.ofSeq [ "Nope"; "Just" ])
                        "Opt case names"

                    Expect.equal u.Origin.Home.AssemblyOption (ValueSome testAsm) "Opt carries home-assembly origin"
                | other -> failtestf "Opt did not project as a generic Union: %A" other
            }

            test "union cases resolve in their declaring module" {
                let origin, frozen = freezeWithOrigin projectionSrc

                let scope =
                    (FrozenSignature.toSignatures origin frozen :> IExternalSymbolResolver).Scope

                let m =
                    match scope.TryContainer "Test.Sig.M" with
                    | ValueSome c -> c
                    | ValueNone -> failtest "Test.Sig.M is a published module"

                match scope.UnionCasesNamed(m, "Just") with
                | BlockOne uc ->
                    Expect.equal uc.UnionKey.TyparArity 1 "Just's declaring union arity"
                    Expect.equal uc.Case.Name "Just" "matched case name"
                | other -> failtestf "expected one declaring union for 'Just', got %A" other

                Expect.isNonEmpty (scope.UnionCasesNamed(m, "Nope")) "Nope resolves"
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

                let totalKey = bindingKey frozen "total"

                match store.TryLookupByKey totalKey with
                | ValueSome s ->
                    Expect.equal s.TyparArity 0 "total is monomorphic"
                    // The scope resolves the SAME entry through the container that declares it.
                    Expect.isTrue (resolver.Scope.TryValue totalKey).IsSome "total resolves through its container"
                | ValueNone -> failtest "total did not project"

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

                Expect.isTrue (bodies (SymbolKey.Binding key)).IsSome "the bodies half is keyed by the binding key"

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
                    (resolver.Scope.TryValue secretKey)
                    ValueNone
                    "private 'secret' is NOT exported (through its container)"

                let sharedKey = bindingKey frozen "shared"

                match store.TryLookupByKey sharedKey with
                | ValueSome _ -> ()
                | ValueNone -> failtest "internal 'shared' MUST be exported"
            }

            test "IntrinsicTypeMap passes this file's IntrinsicBindings through verbatim" {
                let origin, frozen = freezeWithOrigin projectionSrc
                let store = FrozenSignature.toSignatures origin frozen :> IExternalSymbolStore
                // A plain impl file declares no intrinsics, so both sides are empty and the
                // wiring is all that is asserted.
                Expect.equal
                    (IntrinsicTypeMap.entries store.IntrinsicTypeMap).Length
                    frozen.Residue.IntrinsicBindings.Count
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
                            (LexedFile.inAssembly (AssemblyName "P") (AssemblyFileId.ofRelative "p.fsi") parsed.Lexed)
                            {
                                Assembly = AssemblyName "P"
                                Target = "none"
                                Bindings = System.Collections.Generic.Dictionary()
                            }
                            parsed.Tree
                        |> fst

                let fsiSymbolNamed (name: string) : ExternalSymbol option =
                    let mutable found = None

                    for entry: SurfaceEntry<BindingKey, ExternalSymbol> in sigSurface.Symbols do
                        if found.IsNone && entry.Key.Name = name then
                            found <- Some entry.Value

                    found

                // Every projected module binding must match its `.fsi` twin.
                let mutable checked' = 0

                for (name, key) in moduleBindings frozen do
                    match store.TryLookupByKey key, fsiSymbolNamed name with
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
                    | BlockOne c ->
                        Expect.equal
                            (SymbolKeyOps.typeMetaName c.TypeKey)
                            rName
                            (sprintf "field %s -> R's compiled name" field)

                        Expect.equal c.TyparArity 0 "R is monomorphic"
                        Expect.equal c.FieldNames (Block.ofSeq [ "X"; "Y" ]) "R's field names"
                    | other -> failtestf "field %s did not resolve to exactly one record: %A" field other

                Expect.equal (resolver.TryRecordsWithField "Z") Block.empty "unknown field 'Z' has no candidates"
            }

            // --- enum projection: a frozen enum's case→literal table projects to an `Enum`
            // shape under its nominal key, so a later file resolves `(x: E)` / `E.Ci`. The
            // numeric kind is dropped — `ExternalEnumCaseValue` is `int64` or `string`.

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
                | ValueSome(ExternalTypeShape.Enum {
                                                       Cases = cases
                                                       Underlying = underlying
                                                       Origin = origin
                                                   }) ->
                    Expect.equal
                        (cases |> Block.map (fun c -> c.Name))
                        (Block.ofSeq [ "Up"; "Down" ])
                        "case names in source order"

                    Expect.equal underlying RuntimeNames.intKey "unsuffixed literals make an int enum"

                    Expect.equal
                        (cases |> Block.map (fun c -> c.Value))
                        (Block.ofSeq
                            [
                                ExternalEnumCaseValue.IntVal(IntValue.Int32 0)
                                ExternalEnumCaseValue.IntVal(IntValue.Int32 1)
                            ])
                        "case values at the unsuffixed kind"

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
                | ValueSome(ExternalTypeShape.Enum {
                                                       Cases = cases
                                                       Underlying = underlying
                                                   }) ->
                    Expect.equal
                        (cases |> Block.map (fun c -> c.Value))
                        (Block.ofSeq [ ExternalEnumCaseValue.StringVal "on"; ExternalEnumCaseValue.StringVal "off" ])
                        "case string values"

                    Expect.equal underlying RuntimeNames.stringKey "a string enum is string"
                | other -> failtestf "Mode did not project as an Enum: %A" other
            }

            test "an int64 enum publishes its width as the underlying type" {
                let src =
                    "\
namespace Test.En

module M =
    type Wide =
        | A = 1L
        | B = 2L
"

                let origin, frozen = freezeWithOrigin src
                let store = FrozenSignature.toSignatures origin frozen :> IExternalSymbolStore

                match store.TryLookupType(typeKeyOf frozen "Wide") with
                | ValueSome(ExternalTypeShape.Enum { Underlying = underlying }) ->
                    Expect.equal underlying (RuntimeNames.intKindKey IntKind.Int64) "the explicit suffix is the width"
                | other -> failtestf "Wide did not project as an Enum: %A" other
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
                | BlockOne c ->
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
                    |> Block.map (fun c -> SymbolKeyOps.typeMetaName c.TypeKey)
                    |> Block.sort

                Expect.equal
                    shared
                    (Block.sort (Block.ofSeq [ aName; bName ]))
                    "'Shared' resolves to BOTH records, not first-wins"

                // The record-specific fields still pin their single owner.
                Expect.equal
                    (resolver.TryRecordsWithField "OnlyA"
                     |> Block.map (fun c -> SymbolKeyOps.typeMetaName c.TypeKey))
                    (Block.singleton aName)
                    "'OnlyA' resolves to A alone"
            }
        ]
