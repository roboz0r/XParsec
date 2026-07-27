module XParsec.FSharp.Codegen.Js.Tests.FrozenCodecRoundTripTests

open System.Collections.Generic
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The leaf-codec gate: `read (write x) = x` STRUCTURALLY for every value in the leaf
// domains — `FrozenType`, the `SymbolKey`/`TypeKey` key cluster (`FrozenCodecTypes`),
// `NodeKey`, and `SyntaxToken` (`FrozenCodecPrimitives`). Data comes from two sources: the frozen conformance corpus (realistic
// breadth), harvested from the leaf-bearing side tables + FrozenType child-walk of each
// `Frozen.TastFile` (no full expr/decl tree walk — that arrives with the tree codec), and
// hand-built edge cases that pin EVERY case shape the corpus may not exercise (an `FTOr`
// of several members, a deeply nested type, each `MemberKind`, a `Virtual` token, …).

/// Corpus programs the JS backend actually compiles — the same gate the byte-identity
/// test uses, so `frozenOfJs` never trips on a `Diagnose` program's error diagnostics.
let private gated =
    programs
    |> List.filter (fun p ->
        match Map.tryFind "js" p.Obligations with
        | Some Obligation.Run
        | Some(Obligation.Fault _) -> true
        | _ -> false
    )

/// The deduped leaf values harvested from the corpus plus the hand-built edge cases.
type private Harvest =
    {
        FrozenTypes: FrozenType list
        SymbolKeys: SymbolKey list
        TypeKeys: TypeKey list
        NodeKeys: NodeKey list
        Tokens: SyntaxToken list
    }

let private collect () : Harvest =
    let fts = HashSet<FrozenType>(HashIdentity.Structural)
    let sks = HashSet<SymbolKey>(HashIdentity.Structural)
    let tks = HashSet<TypeKey>(HashIdentity.Structural)
    let nks = HashSet<NodeKey>(HashIdentity.Structural)
    let toks = HashSet<SyntaxToken>(HashIdentity.Structural)

    // FrozenType child-walk: collect the node and every key / nested type / local-typar
    // binder it reaches, so the recursive codec is exercised over whole subtrees.
    let rec visitFt (ft: FrozenType) =
        if fts.Add ft then
            match ft with
            | FTConst(key, args) ->
                visitSym key
                EqArray.iter visitFt args
            | FTFun(a, b) ->
                visitFt a
                visitFt b
            | FTTuple items -> EqArray.iter visitFt items
            | FTRecord(key, args)
            | FTUnion(key, args)
            | FTClass(key, args) ->
                tks.Add key |> ignore
                EqArray.iter visitFt args
            | FTEnum key -> tks.Add key |> ignore
            | FTOr members -> EqSet.iter visitFt members
            | FTLiteral _ -> ()
            | FTKeyOf ty -> visitFt ty
            | FTIndexedAccess(o, i) ->
                visitFt o
                visitFt i
            | FTConditional p ->
                visitFt p.Check
                visitFt p.Extends
                visitFt p.WhenTrue
                visitFt p.WhenFalse
            | FTTypar _ -> ()
            | FTLocalTypar(binder, _) -> nks.Add binder |> ignore
            | FTUnknown _ -> ()

    and visitSym (sk: SymbolKey) =
        sks.Add sk |> ignore

        match sk with
        | SymbolKey.Type tk -> tks.Add tk |> ignore
        | SymbolKey.Binding _ -> ()
        | SymbolKey.Member mk ->
            tks.Add mk.Decl |> ignore
            EqArray.iter visitFt mk.ArgSig

            match mk.Kind with
            | MemberKind.InterfaceMethod iface
            | MemberKind.ExplicitInterfaceImpl iface -> tks.Add iface |> ignore
            | MemberKind.Method
            | MemberKind.Property -> ()

    let harvestFile (file: Frozen.TastFile) =
        // The harvest is over the NodeKeys a file mentions, so the binder-keyed tables are
        // widened here (`BinderKey.toNodeKey`) — `FunVerdicts`, on the lambda id space,
        // already speaks it.
        for KeyValue(k, _) in file.ModuleMembers do
            nks.Add(BinderKey.toNodeKey k) |> ignore

        for KeyValue(k, _) in file.TopLevelNames do
            nks.Add(BinderKey.toNodeKey k) |> ignore

        for KeyValue(k, _) in file.ClosureReprs do
            nks.Add(BinderKey.toNodeKey k) |> ignore

        for KeyValue(k, _) in file.FunVerdicts do
            nks.Add k |> ignore

        for KeyValue(k, _) in file.BindingTyparArities do
            nks.Add(BinderKey.toNodeKey k) |> ignore

        for k in file.IntrinsicReprKeys.Keys do
            visitSym k

        for k in file.Accessibility.Keys do
            visitSym k

        for KeyValue(k, constraints) in file.GenericFnSchemes do
            nks.Add(BinderKey.toNodeKey k) |> ignore

            for c in constraints do
                match c with
                | FrozenConstraint.Coercion(_, target) -> visitFt target

        // No `BindingValReprs` harvest: the DU carries none — a binding's source arity is
        // a PROJECTION of its lambda spine that `TastPools.toPools` derives off the
        // columns, so every type and slot it names is already reached by the spine walk
        // this harvest runs.

        // Real frozen tokens, shallowly: the source anchor of each top-level decl body.
        for decl in file.Decls do
            match decl with
            | TDeclG.Let(_, value, _, _) -> toks.Add(TastWalk.exprTok value) |> ignore
            | TDeclG.Expression(expr, _) -> toks.Add(TastWalk.exprTok expr) |> ignore
            | TDeclG.Type _ -> ()

    for p in gated do
        harvestFile (TastUnpool.ofPools (frozenOfJs p.Source))

    // ── hand-built edge cases: pin every case shape regardless of the corpus ──
    let nsGlobal = NamespaceKey.Global

    let nsSystem =
        {
            Path = EqArray.ofList [ "System"; "Collections"; "Generic" ]
        }

    let tkInt =
        {
            Holder = TypeHolder.InNamespace nsGlobal
            Name = "int"
            TyparArity = 0
        }

    let tkString =
        {
            Holder = TypeHolder.InNamespace nsGlobal
            Name = "string"
            TyparArity = 0
        }

    let tkArray =
        {
            Holder = TypeHolder.InNamespace nsGlobal
            Name = "[]"
            TyparArity = 1
        }

    let tkList =
        {
            Holder = TypeHolder.InNamespace nsSystem
            Name = "List`1"
            TyparArity = 1
        }

    let modKey =
        {
            Holder = ModuleHolder.InNamespace nsSystem
            Name = "ListModule"
        }

    let tkInMod =
        {
            Holder = TypeHolder.InModule modKey
            Name = "Inner"
            TyparArity = 0
        }

    let tkNested =
        {
            Holder = TypeHolder.InType tkList
            Name = "Enumerator"
            TyparArity = 0
        }

    let ftInt = FTConst(SymbolKey.Type tkInt, EqArray.empty)
    let ftString = FTConst(SymbolKey.Type tkString, EqArray.empty)
    let ftArray = FTConst(SymbolKey.Type tkArray, EqArray.singleton ftInt)
    let ftLitStr = FTLiteral(LiteralConst.String "GET")
    let ftLitInt = FTLiteral(LiteralConst.Int 42L)
    let ftRecord = FTRecord(tkList, EqArray.singleton ftInt)

    let ftCond =
        FTConditional
            {
                Check = ftInt
                Extends = ftString
                WhenTrue = ftLitStr
                WhenFalse = ftLitInt
            }

    let memberKey =
        {
            Decl = tkList
            Name = "Add"
            ArgSig = EqArray.ofList [ ftInt; ftString ]
            MethodTyparArity = 1
            Kind = MemberKind.Method
        }

    let edgeSymbols =
        [
            SymbolKey.Type tkNested
            SymbolKey.Binding
                {
                    Decl = ModuleHolder.InNamespace nsSystem
                    Name = "printfn"
                }
            SymbolKey.Binding
                {
                    Decl = ModuleHolder.InModule modKey
                    Name = "map"
                }
            SymbolKey.Member memberKey
            SymbolKey.Member
                { memberKey with
                    Kind = MemberKind.Property
                    Name = "Count"
                    ArgSig = EqArray.empty
                }
            SymbolKey.Member
                { memberKey with
                    Kind = MemberKind.InterfaceMethod tkList
                    Name = "GetEnumerator"
                    ArgSig = EqArray.empty
                }
            SymbolKey.Member
                { memberKey with
                    Kind = MemberKind.ExplicitInterfaceImpl tkNested
                    Name = "System.IDisposable.Dispose"
                    ArgSig = EqArray.empty
                }
        ]

    let edgeTypes = [ tkInMod; tkNested ]

    let edgeFrozen =
        [
            ftInt
            ftString
            ftArray
            FTFun(ftInt, FTFun(ftString, ftInt))
            FTTuple(EqArray.ofList [ ftInt; ftString; ftArray ])
            ftRecord
            FTUnion(tkList, EqArray.singleton ftString)
            FTClass(tkList, EqArray.empty)
            FTEnum tkInt
            // FTOr of several members — must survive as the exact stored set (no MkUnion
            // flatten/collapse on read).
            FTOr(EqSet.ofSeq [ ftInt; ftString; ftLitStr; ftLitInt ])
            ftLitStr
            ftLitInt
            FTKeyOf ftRecord
            FTIndexedAccess(ftRecord, ftLitStr)
            ftCond
            FTTypar(TyparAxis.Declaring, 0)
            FTTypar(TyparAxis.Method, 3)
            FTLocalTypar(NodeKey.ofSyntheticCounter 7 NodeKind.SynthPreFreezeInline, 2)
            FTUnknown "?free-typar"
            // Deeply nested: functions, tuples, sets and computations composed together.
            FTFun(FTTuple(EqArray.ofList [ ftCond; ftArray ]), FTOr(EqSet.ofSeq [ FTKeyOf ftRecord; ftCond; ftInt ]))
        ]

    let mkTok (t: Token) (start: int) (idx: int) =
        SyntaxToken.syntaxToken (PositionedToken.Create(t, start)) idx

    let edgeTokens =
        [
            mkTok Token.None 0 0
            mkTok Token.EOF 100 5
            mkTok Token.Whitespace 7 3
            SyntaxToken.virtualToken (PositionedToken.Create(Token.None, 0))
        ]

    for ft in edgeFrozen do
        visitFt ft

    for sk in edgeSymbols do
        visitSym sk

    for tk in edgeTypes do
        tks.Add tk |> ignore

    for nk in
        [
            NodeKey.ofSource 42 NodeKind.ExprLambda
            NodeKey.ofSynthetic 10 NodeKind.SynthLambdaBody
        ] do
        nks.Add nk |> ignore

    for tok in edgeTokens do
        toks.Add tok |> ignore

    {
        FrozenTypes = List.ofSeq fts
        SymbolKeys = List.ofSeq sks
        TypeKeys = List.ofSeq tks
        NodeKeys = List.ofSeq nks
        Tokens = List.ofSeq toks
    }

let private roundTrips (write: System.IO.BinaryWriter -> 'a -> unit) (read: System.IO.BinaryReader -> 'a) (x: 'a) =
    FrozenCodecPrimitives.ofBytes read (FrozenCodecPrimitives.toBytes write x)

[<Tests>]
let tests =
    let h = collect ()

    testList
        "FrozenCodec leaf round-trip"
        [
            test "FrozenType round-trips structurally" {
                for ft in h.FrozenTypes do
                    Expect.equal
                        (roundTrips FrozenCodecTypes.writeFrozenType FrozenCodecTypes.readFrozenType ft)
                        ft
                        "FrozenType"
            }

            test "SymbolKey round-trips structurally" {
                for sk in h.SymbolKeys do
                    Expect.equal
                        (roundTrips FrozenCodecTypes.writeSymbolKey FrozenCodecTypes.readSymbolKey sk)
                        sk
                        "SymbolKey"
            }

            test "TypeKey round-trips structurally" {
                for tk in h.TypeKeys do
                    Expect.equal (roundTrips FrozenCodecTypes.writeTypeKey FrozenCodecTypes.readTypeKey tk) tk "TypeKey"
            }

            test "NodeKey round-trips (Raw verbatim)" {
                for nk in h.NodeKeys do
                    Expect.equal
                        (roundTrips FrozenCodecPrimitives.writeNodeKey FrozenCodecPrimitives.readNodeKey nk)
                        nk
                        "NodeKey"
            }

            test "SyntaxToken round-trips structurally" {
                for tok in h.Tokens do
                    Expect.equal
                        (roundTrips FrozenCodecPrimitives.writeSyntaxToken FrozenCodecPrimitives.readSyntaxToken tok)
                        tok
                        "SyntaxToken"
            }

            // The harvest must actually reach the corpus, not just the edge cases —
            // otherwise the gate would silently pass on an empty frozen file.
            test "harvest exercises a non-trivial value set" {
                Expect.isGreaterThan (List.length gated) 0 "gated programs"
                Expect.isGreaterThan (List.length h.FrozenTypes) 20 "FrozenTypes"
                Expect.isGreaterThan (List.length h.SymbolKeys) 5 "SymbolKeys"
                Expect.isGreaterThan (List.length h.TypeKeys) 2 "TypeKeys"
                Expect.isGreaterThan (List.length h.NodeKeys) 5 "NodeKeys"
                Expect.isGreaterThan (List.length h.Tokens) 4 "Tokens"
            }
        ]
