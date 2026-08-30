// CST smell analysis with pluggable rules. Each rule receives a parsed file (the repo's
// own lexer + CST parser, so extents and identifier references come from real nodes, not
// text heuristics) and returns findings. Rules are registered in `rules` at the bottom.
//
// Usage: dotnet fsi cstsmells.fsx [rootDir] [ruleId ...]
//        default rootDir: <repo>/src; default rules: all
// Prerequisite: XParsec.FSharp.SemanticAnalysis is built (Debug) — the script #r's its output.
#r "nuget: System.IO.Hashing, 8.0.0"
#r "../../../src/XParsec.FSharp.SemanticAnalysis/bin/Debug/net8.0/XParsec.dll"
#r "../../../src/XParsec.FSharp.SemanticAnalysis/bin/Debug/net8.0/XParsec.FSharp.dll"
#r "../../../src/XParsec.FSharp.SemanticAnalysis/bin/Debug/net8.0/XParsec.Toml.dll"
#r "../../../src/XParsec.FSharp.SemanticAnalysis/bin/Debug/net8.0/XParsec.FSharp.SemanticAnalysis.dll"

open System.IO
open System.Collections.Generic
open XParsec
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// ───────────────────────────── chassis ─────────────────────────────

type FileCtx =
    {
        Path: string
        Rel: string
        Lexed: Lexed
        Li: LineIndex
        Tree: ImplementationFile<SyntaxToken>
    }

type Finding =
    {
        RuleId: string
        File: string
        Line: int
        /// One report line; `Details` are indented under it.
        Header: string
        Details: string list
        /// Sort key across all findings, larger first.
        Weight: int
    }

type Rule =
    {
        Id: string
        Summary: string
        Run: FileCtx -> Finding list
    }

let lineOf (ctx: FileCtx) (offset: int) : int =
    let struct (l, _) = ctx.Li.GetLineCol offset
    l

// ───────────────────────── shared helpers ──────────────────────────

// Copied from Conformance.fs (private there): the raw source spelling a pattern binds.
let identOrOpRaw (lexed: Lexed) (io: IdentOrOp<SyntaxToken>) : string voption =
    match io with
    | IdentOrOp.Ident tok -> ValueSome(SyntaxToken.nameIn lexed tok)
    | IdentOrOp.ParenOp(_, OpName.SymbolicOp op, _) -> ValueSome(SyntaxToken.nameIn lexed op)
    | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDot _), _) -> ValueSome ".."
    | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDotDotDot _), _) -> ValueSome ".. .."
    | IdentOrOp.ParenOp(_, OpName.NilOp _, _) -> ValueSome "[]"
    | IdentOrOp.ParenOp(_, OpName.ActivePatternOp _, _) -> ValueNone

let rec boundName (lexed: Lexed) (p: Pat<SyntaxToken>) : string voption =
    match p with
    | Pat.NamedSimple ident -> ValueSome(SyntaxToken.nameIn lexed ident)
    | Pat.Named(longIdent = li) when li.Idents.Length > 0 ->
        ValueSome(SyntaxToken.nameIn lexed li.Idents.[li.Idents.Length - 1])
    | Pat.Op io
    | Pat.OpNamed(ident = io) -> identOrOpRaw lexed io
    | Pat.EnclosedBlock(pat = inner)
    | Pat.Typed(pat = inner) -> boundName lexed inner
    | _ -> ValueNone

let sccsOf (n: int) (edges: bool[,]) : int list list =
    let index = Array.create n -1
    let low = Array.create n 0
    let onStack = Array.create n false
    let stack = Stack<int>()
    let mutable counter = 0
    let sccs = ResizeArray<int list>()

    let rec strong v =
        index.[v] <- counter
        low.[v] <- counter
        counter <- counter + 1
        stack.Push v
        onStack.[v] <- true

        for w in 0 .. n - 1 do
            if edges.[v, w] then
                if index.[w] = -1 then
                    strong w
                    low.[v] <- min low.[v] low.[w]
                elif onStack.[w] then
                    low.[v] <- min low.[v] index.[w]

        if low.[v] = index.[v] then
            let comp = ResizeArray<int>()
            let mutable u = -1

            while u <> v do
                u <- stack.Pop()
                onStack.[u] <- false
                comp.Add u

            sccs.Add(List.ofSeq comp)

    for v in 0 .. n - 1 do
        if index.[v] = -1 then
            strong v

    List.ofSeq sccs

/// Idents referenced by an expression subtree (first segment of a long ident), plus the
/// largest token offset seen, which approximates the subtree's end.
type ExprRefs =
    {
        Refs: HashSet<string>
        mutable MaxOffset: int
    }

let tokenText (lexed: Lexed) (t: SyntaxToken) : string = SyntaxToken.nameIn lexed t

/// Strips grouping parentheses (`( )` / `begin end`) only; list/array/brace blocks are
/// their own expressions and stay.
let rec stripParens (e: Expr<SyntaxToken>) : Expr<SyntaxToken> =
    match e with
    | Expr.EnclosedBlock(lParen = ParenKind.Paren _; expr = inner)
    | Expr.EnclosedBlock(lParen = ParenKind.BeginEnd _; expr = inner) -> stripParens inner
    | _ -> e

let rec stripTypeParens (t: Type<SyntaxToken>) : Type<SyntaxToken> =
    match t with
    | Type.ParenType(typ = inner) -> stripTypeParens inner
    | _ -> t

let rec tryFirstTokenOfType (t: Type<SyntaxToken>) : SyntaxToken voption =
    match t with
    | Type.ParenType(lParen = l) -> ValueSome l
    | Type.FunctionType(fromType = f) -> tryFirstTokenOfType f
    | Type.TupleType(types = ts) ->
        if ts.Length > 0 then
            tryFirstTokenOfType ts.[0]
        else
            ValueNone
    | Type.StructTupleType(structToken = s) -> ValueSome s
    | Type.VarType tp ->
        match tp with
        | Typar.Anon u -> ValueSome u
        | Typar.Named(quote = q) -> ValueSome q
        | Typar.Static(caret = c) -> ValueSome c
    | Type.NamedType li
    | Type.GenericType(longIdent = li) ->
        (if li.Idents.Length > 0 then
             ValueSome li.Idents.[0]
         else
             ValueNone)
    | Type.SuffixedType(baseType = b)
    | Type.DottedType(baseType = b)
    | Type.ArrayType(baseType = b) -> tryFirstTokenOfType b
    | Type.WhenConstrainedType(typ = inner) -> tryFirstTokenOfType inner
    | Type.AnonymousSubtype(hash = h) -> ValueSome h
    | Type.Null t -> ValueSome t
    | Type.UnionType(left = l) -> tryFirstTokenOfType l
    | Type.ILIntrinsic(lHashParen = l) -> ValueSome l
    | Type.AnonRecordType(lBraceBar = l) -> ValueSome l
    | _ -> ValueNone

let longIdentParts (lexed: Lexed) (lio: LongIdentOrOp<SyntaxToken>) : string list =
    match lio with
    | LongIdentOrOp.LongIdent li -> [ for t in li.Idents -> tokenText lexed t ]
    | _ -> []

let collectExprRefs (lexed: Lexed) (acc: ExprRefs) (e: Expr<SyntaxToken>) : unit =
    let walker =
        { CstWalk.identityExprWalker<unit> with
            Visit =
                fun _ ex ->
                    (match ex with
                     | Expr.Missing -> ()
                     | _ ->
                         let tok = CstKeys.diagTokenOfExpr ex
                         acc.MaxOffset <- max acc.MaxOffset (int tok.StartIndex))

                    match ex with
                    | Expr.Ident t -> acc.Refs.Add(SyntaxToken.nameIn lexed t) |> ignore
                    | Expr.LongIdentOrOp lio ->
                        acc.Refs.Add(SyntaxToken.nameIn lexed (CstKeys.firstTokenOfLongIdentOrOp lio))
                        |> ignore
                    | _ -> ()
        }

    CstWalk.iterExpr walker () e

/// Applies `onBinding` to every top-level binding in the file (module-level `let`s,
/// class-preamble `let`s, member bodies, interface-impl members, type-extension members)
/// and `onExpr` to every standalone expression (module-level expressions, class `do`s).
/// Nested bindings are reachable from each binding's `expr`.
let forEachTopLevel
    (ctx: FileCtx)
    (onBinding: Binding<SyntaxToken> -> unit)
    (onExpr: Expr<SyntaxToken> -> unit)
    : unit =
    let walkElements (elements: TypeDefnElements<SyntaxToken>) =
        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Member(defn = mp)) ->
                for b in CstWalk.memberBindings mp do
                    onBinding b
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(
                objectMembers = ValueSome(ObjectMembers(memberDefns = mds)))) ->
                for md in mds do
                    match md with
                    | MemberDefn.Member(defn = mp) ->
                        for b in CstWalk.memberBindings mp do
                            onBinding b
                    | _ -> ()
            | _ -> ()

    let walkExtensions (ext: TypeExtensionElements<SyntaxToken> voption) =
        match ext with
        | ValueSome(TypeExtensionElements(elements = els)) -> walkElements els
        | ValueNone -> ()

    let walkObjectModel (body: ObjectModelBody<SyntaxToken>) =
        for p in body.classPreamble do
            match p with
            | ClassFunctionOrValueDefn.LetBindings(bindings = bs) ->
                for b in bs do
                    onBinding b
            | ClassFunctionOrValueDefn.Do(expr = e) -> onExpr e

        walkElements body.elements

    let walkTypeDefn (td: TypeDefn<SyntaxToken>) =
        match td with
        | TypeDefn.Anon(body = body)
        | TypeDefn.Class(body = body)
        | TypeDefn.Struct(body = body)
        | TypeDefn.Interface(body = body) -> walkObjectModel body
        | TypeDefn.Abbrev(extensions = ext)
        | TypeDefn.Record(extensions = ext)
        | TypeDefn.Union(extensions = ext) -> walkExtensions ext
        | TypeDefn.TypeExtension(elements = TypeExtensionElements(elements = els)) -> walkElements els
        | _ -> ()

    for elem in CstModuleTree.implFileElems ctx.Tree do
        match elem with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) ->
            for b in bs do
                onBinding b
        | ModuleElem.Expression e -> onExpr e
        | ModuleElem.Type tds ->
            for td in tds do
                walkTypeDefn td
        | _ -> ()

/// Accumulates per-file rule hits, deduplicated by token offset, for rules that report
/// one finding per file with a detail line per hit.
type HitSet =
    {
        Seen: HashSet<int>
        Hits: ResizeArray<struct (int * string)>
    }

    static member Create() =
        {
            Seen = HashSet()
            Hits = ResizeArray()
        }

    member this.Add (ctx: FileCtx) (tok: SyntaxToken) (msg: string) =
        if this.Seen.Add(int tok.StartIndex) then
            this.Hits.Add(struct (lineOf ctx (int tok.StartIndex), msg))

/// One finding per file summarising `hits`; ValueNone when there are none.
let hitFinding (ruleId: string) (ctx: FileCtx) (hits: HitSet) : Finding voption =
    match hits.Hits.Count with
    | 0 -> ValueNone
    | n ->
        let ordered = hits.Hits |> Seq.sortBy (fun (struct (l, _)) -> l) |> List.ofSeq

        ValueSome
            {
                RuleId = ruleId
                File = ctx.Rel
                Line =
                    (match ordered with
                     | struct (l, _) :: _ -> l
                     | [] -> 1)
                Header = sprintf "%s (%s)  hits=%d" ctx.Rel ruleId n
                Details = [ for struct (l, m) in ordered -> sprintf "line %d: %s" l m ]
                Weight = n
            }

// ───────────────────── rule: rec-group ─────────────────────────────
// `let rec ... and` groups (module-level, nested in expressions, class bodies, member
// bodies) whose members do not all participate in one cycle.
//   liftable — members in a singleton SCC with no self-call: plain `let` candidates
//   cycles   — number of multi-member SCCs; > 1 means the chain fuses independent knots
//   pinned   — operator / active-pattern members: their references are not plain idents,
//              so they are conservatively kept in the chain and never reported liftable

type private RecMember =
    {
        Name: string voption
        StartOffset: int
        Refs: ExprRefs
    }

let private analyzeRecBinding (lexed: Lexed) (b: Binding<SyntaxToken>) : RecMember =
    let startTok = CstKeys.firstTokenOfPat b.pattern

    let refs =
        {
            Refs = HashSet()
            MaxOffset = int startTok.StartIndex
        }

    collectExprRefs lexed refs b.expr

    {
        Name = boundName lexed b.pattern
        StartOffset = int startTok.StartIndex
        Refs = refs
    }

let private analyzeRecGroup (ctx: FileCtx) (nested: bool) (bindings: Binding<SyntaxToken>[]) : Finding voption =
    let mems = bindings |> Array.map (analyzeRecBinding ctx.Lexed)
    let n = mems.Length

    let edges =
        Array2D.init
            n
            n
            (fun a b ->
                match mems.[b].Name with
                | ValueSome name -> mems.[a].Refs.Refs.Contains name
                | ValueNone -> false
            )

    // A pinned member's inbound references are invisible to the walk, so treat it as
    // referenced by everyone: it can neither be lifted nor break another member's cycle.
    let pinned = [| for m in mems -> m.Name.IsNone |]

    for a in 0 .. n - 1 do
        for b in 0 .. n - 1 do
            if pinned.[b] then
                edges.[a, b] <- true

    let sccs = sccsOf n edges

    let liftableIdx =
        [
            for comp in sccs do
                match comp with
                | [ v ] when not edges.[v, v] && not pinned.[v] -> yield v
                | _ -> ()
        ]

    let startLines = [| for m in mems -> lineOf ctx m.StartOffset |]

    let endLine =
        lineOf ctx (mems |> Array.map (fun m -> m.Refs.MaxOffset) |> Array.max)

    let sizeOf v =
        let next = if v = n - 1 then endLine + 1 else startLines.[v + 1]
        max 1 (next - startLines.[v])

    let nameOf v =
        match mems.[v].Name with
        | ValueSome s -> s
        | ValueNone -> "<op/active-pattern>"

    let cycles = sccs |> List.filter (fun c -> c.Length > 1) |> List.length
    let pinnedCount = pinned |> Array.filter id |> Array.length
    let liftLines = liftableIdx |> List.sumBy sizeOf

    ValueSome
        {
            RuleId = "rec-group"
            File = ctx.Rel
            Line = startLines.[0]
            Header =
                sprintf
                    "%s:%d%s  members=%d lines=%d  liftable=%d (%d lines)  cycles=%d%s%s"
                    ctx.Rel
                    startLines.[0]
                    (if nested then " (nested)" else "")
                    n
                    (endLine - startLines.[0] + 1)
                    liftableIdx.Length
                    liftLines
                    cycles
                    (if cycles > 1 then "  <-- SPLITTABLE" else "")
                    (if pinnedCount > 0 then
                         sprintf "  pinned=%d" pinnedCount
                     else
                         "")
            Details = [ for v in liftableIdx -> sprintf "lift: %s (%d lines)" (nameOf v) (sizeOf v) ]
            Weight = liftLines
        }

let private recGroupRule: Rule =
    {
        Id = "rec-group"
        Summary = "let rec ... and groups whose members are not all in one cycle"
        Run =
            fun ctx ->
                let findings = ResizeArray<Finding>()

                let handleGroup nested (bindings: Binding<SyntaxToken>[]) =
                    if bindings.Length >= 3 then
                        match analyzeRecGroup ctx nested bindings with
                        | ValueSome f -> findings.Add f
                        | ValueNone -> ()

                let nestedCollector =
                    { CstWalk.identityExprWalker<unit> with
                        Visit =
                            fun _ ex ->
                                match ex with
                                | Expr.LetOrUse(keyword = LetOrUseKeyword.Let _; isRec = ValueSome _; bindings = bs) ->
                                    handleGroup true [| for b in bs -> b |]
                                | _ -> ()
                    }

                let walkElements (elements: TypeDefnElements<SyntaxToken>) =
                    for el in elements do
                        match el with
                        | TypeDefnElement.Member(MemberDefn.Member(defn = mp)) ->
                            for b in CstWalk.memberBindings mp do
                                CstWalk.iterExpr nestedCollector () b.expr
                        | _ -> ()

                let walkExtensions (ext: TypeExtensionElements<SyntaxToken> voption) =
                    match ext with
                    | ValueSome(TypeExtensionElements(elements = els)) -> walkElements els
                    | ValueNone -> ()

                let walkObjectModel (body: ObjectModelBody<SyntaxToken>) =
                    for p in body.classPreamble do
                        match p with
                        | ClassFunctionOrValueDefn.LetBindings(isRec = isRec; bindings = bs) ->
                            if isRec.IsSome then
                                handleGroup true [| for b in bs -> b |]

                            for b in bs do
                                CstWalk.iterExpr nestedCollector () b.expr
                        | ClassFunctionOrValueDefn.Do(expr = e) -> CstWalk.iterExpr nestedCollector () e

                    walkElements body.elements

                let walkTypeDefn (td: TypeDefn<SyntaxToken>) =
                    match td with
                    | TypeDefn.Anon(body = body)
                    | TypeDefn.Class(body = body)
                    | TypeDefn.Struct(body = body)
                    | TypeDefn.Interface(body = body) -> walkObjectModel body
                    | TypeDefn.Abbrev(extensions = ext)
                    | TypeDefn.Record(extensions = ext)
                    | TypeDefn.Union(extensions = ext) -> walkExtensions ext
                    | TypeDefn.TypeExtension(elements = TypeExtensionElements(elements = els)) -> walkElements els
                    | _ -> ()

                for elem in CstModuleTree.implFileElems ctx.Tree do
                    match elem with
                    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(isRec = isRec; bindings = bs)) ->
                        if isRec.IsSome then
                            handleGroup false [| for b in bs -> b |]

                        for b in bs do
                            CstWalk.iterExpr nestedCollector () b.expr
                    | ModuleElem.Expression e -> CstWalk.iterExpr nestedCollector () e
                    | ModuleElem.Type tds ->
                        for td in tds do
                            walkTypeDefn td
                    | _ -> ()

                List.ofSeq findings
    }

// ───────────────────── rule: type-group ────────────────────────────
// `type A = ... and B = ...` groups where the reference graph (declared type positions
// AND expression references inside member bodies) does not put every member in one
// cycle. A liftable member can be its own `type` declaration; `and` scoping is vestigial.
// Unlike `let rec`, a self-referential type stands alone fine, so a self-edge does not
// block lifting. Not walked (conservative gaps that can only under-lift, never
// over-chain): measure arguments, attribute arguments, `member val` initialisers.

let private typeNameIdents (td: TypeDefn<SyntaxToken>) : LongIdent<SyntaxToken> voption =
    match td with
    | TypeDefn.Abbrev(typeName = TypeName(ident = li))
    | TypeDefn.Record(typeName = TypeName(ident = li))
    | TypeDefn.Union(typeName = TypeName(ident = li))
    | TypeDefn.Anon(typeName = TypeName(ident = li))
    | TypeDefn.Class(typeName = TypeName(ident = li))
    | TypeDefn.Struct(typeName = TypeName(ident = li))
    | TypeDefn.Interface(typeName = TypeName(ident = li))
    | TypeDefn.Enum(typeName = TypeName(ident = li))
    | TypeDefn.Delegate(typeName = TypeName(ident = li))
    | TypeDefn.TypeExtension(typeName = TypeName(ident = li))
    | TypeDefn.AbstractType(typeName = TypeName(ident = li)) -> ValueSome li
    | TypeDefn.Missing
    | TypeDefn.SkipsTokens _ -> ValueNone

type private TypeMember =
    {
        Name: string voption
        StartOffset: int
        Refs: ExprRefs
    }

let private analyzeTypeDefn (lexed: Lexed) (td: TypeDefn<SyntaxToken>) : TypeMember voption =
    match typeNameIdents td with
    | ValueSome li when li.Idents.Length > 0 ->
        let startTok = li.Idents.[0]

        let refs =
            {
                Refs = HashSet()
                MaxOffset = int startTok.StartIndex
            }

        let addLongIdent (nameLi: LongIdent<SyntaxToken>) =
            for t in nameLi.Idents do
                refs.Refs.Add(SyntaxToken.nameIn lexed t) |> ignore
                refs.MaxOffset <- max refs.MaxOffset (int t.StartIndex)

        let typeIter: CstTypeWalk.TypeIter =
            {
                VisitType =
                    fun _ ty ->
                        (match ty with
                         | Type.NamedType nameLi
                         | Type.GenericType(longIdent = nameLi)
                         | Type.SuffixedType(longIdent = nameLi)
                         | Type.DottedType(longIdent = nameLi) -> addLongIdent nameLi
                         | _ -> ())

                        true
            }

        let onPat (p: Pat<SyntaxToken>) =
            CstWalk.iterPat
                {
                    VisitPat =
                        fun _ pat ->
                            (match pat with
                             | Pat.Typed(typ = t)
                             | Pat.TypeTest(typ = t)
                             | Pat.TypeTestAs(typ = t) -> CstTypeWalk.iterType typeIter t
                             | _ -> ())

                            true
                }
                p

        CstTypeWalk.iterTypeDefnTypes typeIter onPat (CstTypeWalk.iterType typeIter) td

        // Expression references inside member bodies (ctor calls, static accesses).
        let walkBinding (b: Binding<SyntaxToken>) = collectExprRefs lexed refs b.expr

        let walkElements (els: TypeDefnElements<SyntaxToken>) =
            for el in els do
                match el with
                | TypeDefnElement.Member(MemberDefn.Member(defn = mp)) ->
                    for b in CstWalk.memberBindings mp do
                        walkBinding b
                | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(objectMembers = oms)) ->
                    match oms with
                    | ValueSome(ObjectMembers(memberDefns = mds)) ->
                        for md in mds do
                            match md with
                            | MemberDefn.Member(defn = mp) ->
                                for b in CstWalk.memberBindings mp do
                                    walkBinding b
                            | _ -> ()
                    | ValueNone -> ()
                | _ -> ()

        let walkBody (b: ObjectModelBody<SyntaxToken>) =
            for p in b.classPreamble do
                match p with
                | ClassFunctionOrValueDefn.LetBindings(bindings = bs) ->
                    for binding in bs do
                        walkBinding binding
                | ClassFunctionOrValueDefn.Do(expr = e) -> collectExprRefs lexed refs e

            walkElements b.elements

        (match td with
         | TypeDefn.Anon(body = b)
         | TypeDefn.Class(body = b)
         | TypeDefn.Struct(body = b)
         | TypeDefn.Interface(body = b) -> walkBody b
         | TypeDefn.Abbrev(extensions = ValueSome(TypeExtensionElements(elements = els)))
         | TypeDefn.Record(extensions = ValueSome(TypeExtensionElements(elements = els)))
         | TypeDefn.Union(extensions = ValueSome(TypeExtensionElements(elements = els)))
         | TypeDefn.TypeExtension(elements = TypeExtensionElements(elements = els)) -> walkElements els
         | _ -> ())

        ValueSome
            {
                Name = ValueSome(SyntaxToken.nameIn lexed li.Idents.[li.Idents.Length - 1])
                StartOffset = int startTok.StartIndex
                Refs = refs
            }
    | _ -> ValueNone

let private analyzeTypeGroup (ctx: FileCtx) (tds: TypeDefn<SyntaxToken>[]) : Finding voption =
    let mems =
        tds
        |> Array.map (fun td ->
            match analyzeTypeDefn ctx.Lexed td with
            | ValueSome m -> m
            | ValueNone ->
                {
                    Name = ValueNone
                    StartOffset = 0
                    Refs = { Refs = HashSet(); MaxOffset = 0 }
                }
        )

    let n = mems.Length

    let edges =
        Array2D.init
            n
            n
            (fun a b ->
                match mems.[b].Name with
                | ValueSome name -> mems.[a].Refs.Refs.Contains name
                | ValueNone -> false
            )

    let pinned = [| for m in mems -> m.Name.IsNone |]

    for a in 0 .. n - 1 do
        for b in 0 .. n - 1 do
            if pinned.[b] then
                edges.[a, b] <- true

    let sccs = sccsOf n edges

    // A self-edge does not block lifting: a standalone `type` may reference itself.
    let liftableIdx =
        [
            for comp in sccs do
                match comp with
                | [ v ] when not pinned.[v] -> yield v
                | _ -> ()
        ]

    let cycles = sccs |> List.filter (fun c -> c.Length > 1) |> List.length

    if liftableIdx.IsEmpty && cycles <= 1 then
        ValueNone
    else
        let startLines = [| for m in mems -> lineOf ctx m.StartOffset |]

        let endLine =
            lineOf ctx (mems |> Array.map (fun m -> m.Refs.MaxOffset) |> Array.max)

        let sizeOf v =
            let next = if v = n - 1 then endLine + 1 else startLines.[v + 1]
            max 1 (next - startLines.[v])

        let nameOf v =
            match mems.[v].Name with
            | ValueSome s -> s
            | ValueNone -> "<unnamed>"

        let liftLines = liftableIdx |> List.sumBy sizeOf

        ValueSome
            {
                RuleId = "type-group"
                File = ctx.Rel
                Line = startLines.[0]
                Header =
                    sprintf
                        "%s:%d (type group)  members=%d lines=%d  liftable=%d (%d lines)  cycles=%d%s"
                        ctx.Rel
                        startLines.[0]
                        n
                        (endLine - startLines.[0] + 1)
                        liftableIdx.Length
                        liftLines
                        cycles
                        (if cycles > 1 then "  <-- SPLITTABLE" else "")
                Details =
                    [
                        for v in liftableIdx -> sprintf "lift: type %s (%d lines)" (nameOf v) (sizeOf v)
                    ]
                Weight = liftLines
            }

let private typeGroupRule: Rule =
    {
        Id = "type-group"
        Summary = "type ... and ... groups whose members are not all mutually recursive"
        Run =
            fun ctx ->
                [
                    for elem in CstModuleTree.implFileElems ctx.Tree do
                        match elem with
                        | ModuleElem.Type tds when tds.Length >= 2 ->
                            match analyzeTypeGroup ctx [| for td in tds -> td |] with
                            | ValueSome f -> yield f
                            | ValueNone -> ()
                        | _ -> ()
                ]
    }

// ───────────────────── rule: record-of-closures ────────────────────
// A record whose fields are mostly function-typed is a disguised interface (see the root
// CLAUDE.md design rules).

let private recordOfClosuresRule: Rule =
    {
        Id = "record-of-closures"
        Summary = "record types whose fields are mostly function-typed"
        Run =
            fun ctx ->
                [
                    for elem in CstModuleTree.implFileElems ctx.Tree do
                        match elem with
                        | ModuleElem.Type tds ->
                            for td in tds do
                                match td with
                                | TypeDefn.Record(typeName = TypeName(ident = li); fields = fields) when
                                    li.Idents.Length > 0
                                    ->
                                    let arrowFields =
                                        [
                                            for RecordField(ident = id; typ = t) in fields do
                                                match stripTypeParens t with
                                                | Type.FunctionType _ -> yield tokenText ctx.Lexed id
                                                | _ -> ()
                                        ]

                                    let total = fields.Length

                                    if total >= 2 && arrowFields.Length * 2 > total then
                                        let line = lineOf ctx (int li.Idents.[0].StartIndex)

                                        yield
                                            {
                                                RuleId = "record-of-closures"
                                                File = ctx.Rel
                                                Line = line
                                                Header =
                                                    sprintf
                                                        "%s:%d (record-of-closures)  type %s: %d/%d fields are functions"
                                                        ctx.Rel
                                                        line
                                                        (tokenText ctx.Lexed li.Idents.[li.Idents.Length - 1])
                                                        arrowFields.Length
                                                        total
                                                Details = [ for f in arrowFields -> sprintf "field: %s" f ]
                                                Weight = arrowFields.Length * 10
                                            }
                                | _ -> ()
                        | _ -> ()
                ]
    }

// ───────────────────── rule: tuple-arity ───────────────────────────
// Tuples of three or more components become records (root CLAUDE.md); `string * string`
// is called out by name. Union cases carrying >= 3 positional fields are the same smell.

let private tupleArityRule: Rule =
    {
        Id = "tuple-arity"
        Summary = "tuple types of arity >= 3, string * string, wide positional union cases"
        Run =
            fun ctx ->
                let lexed = ctx.Lexed
                let hits = HitSet.Create()

                let isStringType (t: Type<SyntaxToken>) =
                    match stripTypeParens t with
                    | Type.NamedType li when li.Idents.Length = 1 -> tokenText lexed li.Idents.[0] = "string"
                    | _ -> false

                let checkTuple (tok: SyntaxToken voption) (types: ImArr<Type<SyntaxToken>>) =
                    match tok with
                    | ValueSome tok ->
                        if types.Length >= 3 then
                            hits.Add ctx tok (sprintf "tuple arity %d" types.Length)
                        elif types.Length = 2 && isStringType types.[0] && isStringType types.[1] then
                            hits.Add ctx tok "string * string"
                    | ValueNone -> ()

                let ti: CstTypeWalk.TypeIter =
                    {
                        VisitType =
                            fun _ ty ->
                                (match ty with
                                 | Type.TupleType(types = ts) -> checkTuple (tryFirstTokenOfType ty) ts
                                 | Type.StructTupleType(structToken = s; types = ts) -> checkTuple (ValueSome s) ts
                                 | _ -> ())

                                true
                    }

                let onPat (p: Pat<SyntaxToken>) =
                    CstWalk.iterPat
                        {
                            VisitPat =
                                fun _ pat ->
                                    (match pat with
                                     | Pat.Typed(typ = t)
                                     | Pat.TypeTest(typ = t)
                                     | Pat.TypeTestAs(typ = t) -> CstTypeWalk.iterType ti t
                                     | _ -> ())

                                    true
                        }
                        p

                let exprWalker =
                    { CstWalk.identityExprWalker<unit> with
                        Visit =
                            fun _ ex ->
                                match ex with
                                | Expr.TypeAnnotation(typ = t)
                                | Expr.StaticUpcast(typ = t)
                                | Expr.DynamicTypeTest(typ = t)
                                | Expr.DynamicDowncast(typ = t)
                                | Expr.New(typ = t) -> CstTypeWalk.iterType ti t
                                | Expr.TypeApp(types = ts) ->
                                    for t in ts do
                                        CstTypeWalk.iterType ti t
                                | _ -> ()
                    }

                let onBinding (b: Binding<SyntaxToken>) =
                    onPat b.pattern

                    for p in b.argumentPats do
                        onPat p

                    match b.returnType with
                    | ValueSome(ReturnType(typ = t)) -> CstTypeWalk.iterType ti t
                    | ValueNone -> ()

                    CstWalk.iterExpr exprWalker () b.expr

                forEachTopLevel ctx onBinding (CstWalk.iterExpr exprWalker ())

                for elem in CstModuleTree.implFileElems ctx.Tree do
                    match elem with
                    | ModuleElem.Type tds ->
                        for td in tds do
                            CstTypeWalk.iterTypeDefnTypes ti onPat (CstTypeWalk.iterType ti) td

                            match td with
                            | TypeDefn.Union(cases = cases) ->
                                for UnionTypeCase(data = data) in cases do
                                    match data with
                                    | UnionTypeCaseData.Nary(name = nm; fields = fs) ->
                                        let caseName =
                                            match identOrOpRaw lexed nm with
                                            | ValueSome s -> s
                                            | ValueNone -> "<op>"

                                        let unnamed =
                                            fs
                                            |> Seq.filter (fun f ->
                                                match f with
                                                | UnionTypeField.Unnamed _ -> true
                                                | UnionTypeField.Named _ -> false
                                            )
                                            |> Seq.length

                                        if unnamed >= 3 then
                                            hits.Add
                                                ctx
                                                (CstKeys.firstTokenOfIdentOrOp nm)
                                                (sprintf "union case %s: %d positional fields" caseName unnamed)
                                        elif fs.Length = 2 then
                                            let unnamedString f =
                                                match f with
                                                | UnionTypeField.Unnamed t -> isStringType t
                                                | UnionTypeField.Named _ -> false

                                            if unnamedString fs.[0] && unnamedString fs.[1] then
                                                hits.Add
                                                    ctx
                                                    (CstKeys.firstTokenOfIdentOrOp nm)
                                                    (sprintf "union case %s: string * string" caseName)
                                    | _ -> ()
                            | _ -> ()
                    | _ -> ()

                match hitFinding "tuple-arity" ctx hits with
                | ValueSome f -> [ f ]
                | ValueNone -> []
    }

// ───────────────────── rule: decl-size ─────────────────────────────
// Oversized bindings and long parameter lists. Extents are token-accurate (largest token
// offset in the body), so strings and comments cannot stretch or shrink a measurement.

let private declSizeLineThreshold = 100
let private declSizeParamThreshold = 5

let private declSizeRule: Rule =
    {
        Id = "decl-size"
        Summary = sprintf "bindings over %d lines or with %d+ parameters" declSizeLineThreshold declSizeParamThreshold
        Run =
            fun ctx ->
                let entries = ResizeArray<struct (int * string * int)>()

                let onBinding (b: Binding<SyntaxToken>) =
                    let startTok = CstKeys.firstTokenOfPat b.pattern

                    let refs =
                        {
                            Refs = HashSet()
                            MaxOffset = int startTok.StartIndex
                        }

                    collectExprRefs ctx.Lexed refs b.expr
                    let startLine = lineOf ctx (int startTok.StartIndex)
                    let endLine = lineOf ctx refs.MaxOffset
                    let size = endLine - startLine + 1

                    let name =
                        match boundName ctx.Lexed b.pattern with
                        | ValueSome s -> s
                        | ValueNone -> "<op/pattern>"

                    if size >= declSizeLineThreshold then
                        entries.Add(struct (startLine, sprintf "%s: %d lines" name size, size))

                    if b.argumentPats.Length >= declSizeParamThreshold then
                        entries.Add(struct (startLine, sprintf "%s: %d parameters" name b.argumentPats.Length, 5))

                forEachTopLevel ctx onBinding (fun _ -> ())

                match entries.Count with
                | 0 -> []
                | n ->
                    let ordered = entries |> Seq.sortBy (fun (struct (l, _, _)) -> l) |> List.ofSeq

                    [
                        {
                            RuleId = "decl-size"
                            File = ctx.Rel
                            Line =
                                (match ordered with
                                 | struct (l, _, _) :: _ -> l
                                 | [] -> 1)
                            Header = sprintf "%s (decl-size)  hits=%d" ctx.Rel n
                            Details = [ for struct (l, m, _) in ordered -> sprintf "line %d: %s" l m ]
                            Weight = entries |> Seq.sumBy (fun (struct (_, _, w)) -> w)
                        }
                    ]
    }

// ───────────────────── rule: list-idioms ───────────────────────────
// The retired list idioms (root CLAUDE.md): `@`, `List.rev`, and per-element access
// (`.Length`, `List.item`) inside loop bodies. `.Length` cannot be typed here, so array
// and string hits are expected noise — the rule flags for reading, not for mechanical fix.

let private listIdiomsRule: Rule =
    {
        Id = "list-idioms"
        Summary = "@, List.rev, and .Length / List.item inside loop bodies"
        Run =
            fun ctx ->
                let lexed = ctx.Lexed
                let hits = HitSet.Create()

                let inLoopWalker =
                    { CstWalk.identityExprWalker<unit> with
                        Visit =
                            fun _ ex ->
                                match ex with
                                | Expr.DotLookup(dot = d; longIdentOrOp = lio) when
                                    longIdentParts lexed lio = [ "Length" ]
                                    ->
                                    hits.Add ctx d ".Length in a loop body"
                                | Expr.LongIdentOrOp lio ->
                                    match longIdentParts lexed lio with
                                    | [ "List"; "item" ] ->
                                        hits.Add ctx (CstKeys.firstTokenOfLongIdentOrOp lio) "List.item in a loop body"
                                    | _ -> ()
                                | _ -> ()
                    }

                let mainWalker =
                    { CstWalk.identityExprWalker<unit> with
                        Visit =
                            fun _ ex ->
                                match ex with
                                | Expr.InfixApp(infixOp = op) when tokenText lexed op = "@" ->
                                    hits.Add ctx op "@ (list append)"
                                | Expr.LongIdentOrOp lio when longIdentParts lexed lio = [ "List"; "rev" ] ->
                                    hits.Add ctx (CstKeys.firstTokenOfLongIdentOrOp lio) "List.rev"
                                | Expr.While(body = b)
                                | Expr.ForTo(body = b)
                                | Expr.ForIn(body = b) -> CstWalk.iterExpr inLoopWalker () b
                                | _ -> ()
                    }

                forEachTopLevel ctx (fun b -> CstWalk.iterExpr mainWalker () b.expr) (CstWalk.iterExpr mainWalker ())

                match hitFinding "list-idioms" ctx hits with
                | ValueSome f -> [ f ]
                | ValueNone -> []
    }

// ───────────────────── rule: nits ──────────────────────────────────
// Small shape nits from the root CLAUDE.md: `xs.Length = 0` comparisons (match on the
// shape instead) and calls passing two or more boolean literals.

let private nitsRule: Rule =
    {
        Id = "nits"
        Summary = ".Length compared to 0; two or more boolean-literal arguments"
        Run =
            fun ctx ->
                let lexed = ctx.Lexed
                let hits = HitSet.Create()

                let isBoolLit (e: Expr<SyntaxToken>) =
                    match stripParens e with
                    | Expr.Const(Constant.Literal t) ->
                        let s = tokenText lexed t
                        s = "true" || s = "false"
                    | _ -> false

                let isLengthLookup (e: Expr<SyntaxToken>) =
                    match stripParens e with
                    | Expr.DotLookup(longIdentOrOp = lio) -> longIdentParts lexed lio = [ "Length" ]
                    | _ -> false

                let isZero (e: Expr<SyntaxToken>) =
                    match stripParens e with
                    | Expr.Const(Constant.Literal t) -> tokenText lexed t = "0"
                    | _ -> false

                let walker =
                    { CstWalk.identityExprWalker<unit> with
                        Visit =
                            fun _ ex ->
                                match ex with
                                | Expr.InfixApp(leftExpr = l; infixOp = op; rightExpr = r) when
                                    (let s = tokenText lexed op in s = "=" || s = "<>")
                                    && ((isLengthLookup l && isZero r) || (isZero l && isLengthLookup r))
                                    ->
                                    hits.Add ctx op ".Length compared to 0 (match on the shape instead)"
                                | Expr.App(funcExpr = f; argExprs = args) when
                                    (args |> Seq.filter isBoolLit |> Seq.length) >= 2
                                    ->
                                    hits.Add ctx (CstKeys.diagTokenOfExpr f) "multiple boolean literal arguments"
                                | _ -> ()
                    }

                forEachTopLevel ctx (fun b -> CstWalk.iterExpr walker () b.expr) (CstWalk.iterExpr walker ())

                match hitFinding "nits" ctx hits with
                | ValueSome f -> [ f ]
                | ValueNone -> []
    }

// ─────────────────────────── main ──────────────────────────────────

let rules: Rule list =
    [
        recGroupRule
        typeGroupRule
        recordOfClosuresRule
        tupleArityRule
        declSizeRule
        listIdiomsRule
        nitsRule
    ]

let args = fsi.CommandLineArgs |> Array.skip 1 |> Array.filter (fun a -> a <> "--")

let root =
    match args |> Array.tryFind Directory.Exists with
    | Some d -> Path.GetFullPath d
    | None -> Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "src"))

let selectedRules =
    match args |> Array.filter (Directory.Exists >> not) with
    | [||] -> rules
    | ids ->
        match rules |> List.filter (fun r -> Array.contains r.Id ids) with
        | [] ->
            failwithf
                "no such rule(s): %s (available: %s)"
                (String.concat ", " ids)
                (String.concat ", " [ for r in rules -> r.Id ])
        | rs -> rs

let files =
    Directory.EnumerateFiles(root, "*.fs", SearchOption.AllDirectories)
    |> Seq.filter (fun f ->
        let parts = f.Split([| '\\'; '/' |])
        not (Array.contains "obj" parts || Array.contains "bin" parts)
    )

let findings =
    [
        for f in files do
            let src = (File.ReadAllText f).Replace("\r\n", "\n")

            match ParseChain.parse Set.empty src with
            | Error _ -> eprintfn "PARSE FAILED (skipped): %s" f
            | Ok parsed ->
                let ctx =
                    {
                        Path = f
                        Rel = Path.GetRelativePath(Path.Combine(root, ".."), f)
                        Lexed = parsed.Lexed
                        Li = LineIndex.OfString src
                        Tree = parsed.Tree
                    }

                for rule in selectedRules do
                    yield! rule.Run ctx
    ]
    |> List.sortByDescending (fun f -> f.Weight)

for f in findings do
    printfn "%s" f.Header

    for d in f.Details do
        printfn "    %s" d
