namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The UNIT-level TAST: what a whole compiled file carries — its declarations, its inline
// vocabulary, its intrinsic representations and the side tables keyed by the binders its
// tree introduces (`BinderKey`, the one projection of "a node that binds") — over the term
// shapes of `TastExpr.fs` and the declaration shapes of `TastDecl.fs`.
//
// TAST does NOT preserve trivia, parens, or token layout — tooling consumers
// query the CST for that. TAST exists for consumers that only care about
// semantics (codegen, target plugins).
//
// The TAST term/declaration cluster is parameterized over three axes —
// `TExprG<'ty, 'tok, 'id>`: the type a node carries, the token it anchors on, and the
// space it NAMES BINDERS IN. Each axis has its own instantiations, and no consumer spells
// them out: the central aliases at the bottom of this file do, one alias family per
// domain (`TExpr` pre-freeze, `Frozen.TExpr` after the freeze, `Pooled.TExpr` for a tree
// rebuilt from the columns), so a new axis costs the annotations nothing.
//
// The identity axis is what lets a tree rebuilt from `FrozenPools` name binders by the
// DENSE id the columns already carry (`BinderId`) instead of the `NodeKey` a source-shaped
// tree names them by — the two are the same tree at different instantiations rather than
// one tree and a lossy view of it.

/// A splice TEMPLATE: an inline binding's retained declaration plus the compiler
/// attributes on its parameters, positionally aligned to its curried parameters
/// (`[<CallAtMostOnce>]` &c.; empty for a body whose parameters carry none). The
/// inliner reads both — the attrs gate call-by-name-at-single-use splicing.
type TInlineBodyG<'ty, 'tok, 'id> =
    {
        Decl: TDeclG<'ty, 'tok, 'id>
        ParamAttrs: ParamAttrs[]
    }

/// One entry of a unit's INLINE VOCABULARY (`TastFileG.InlineBodies`): a body,
/// under the identity its home unit interns it by.
///
/// The `Key` is MINTED (from `ModuleBindingInfo`, at freeze), not recovered: an inline
/// binding is the one kind of symbol that is exported but NEVER emitted, so nothing
/// downstream would ever mint its identity as a side effect of emitting it. A consumer
/// splices by this key — the same key its use-site `TExpr.External` carries.
type TInlineValueG<'ty, 'tok, 'id> =
    {
        Key: SymbolKey
        Body: TInlineBodyG<'ty, 'tok, 'id>
    }

/// What a unit's `(# … #)` binding records about one intrinsic: the target
/// representation string, and whether the binding was `class`-tagged
/// (`(# class "System.Attribute" #)`) and so may be inherited.
///
/// ONE entry per intrinsic, carrying both facts: heritability is a property OF a
/// repr, so a heritable intrinsic with no repr is unrepresentable rather than
/// merely unexpected.
type IntrinsicReprInfo =
    {
        /// The target representation (`Vesper.int` → `"System.Int32"`).
        Platform: string
        /// `(# class "…" #)`-tagged: a derived unit may `inherit` this primitive
        /// (`obj` / `exn` / `Attribute`). A scalar primitive (`int`) is `false`.
        Heritable: bool
    }

/// An identity that some node of the tree INTRODUCES as a definition site: what a
/// `TExpr.Var` references, what naming is computed from, and what the frozen binder pool
/// interns. It rides the tree's own identity axis (`'id`), because "is a binder" is a fact
/// about a node and not about the space that node is addressed in: pre-freeze and frozen
/// trees name a binder by `NodeKey`, a tree rebuilt from the pools by `BinderId`, and both
/// have side tables that may only be keyed by a definition site.
///
/// A raw identity addresses ANY node — an expression, a pattern the freeze erases, a
/// binding's CST head (`CstKeys.ofBinding`) — so it is the over-wide type for such a table.
///
/// The representation is PRIVATE, which is the whole mechanism: the constructor is
/// reachable only from this file, where the `BinderKey` module below is the only thing that
/// uses it. So every binder key in the program is a projection of a node that binds, and a
/// binder-keyed table cannot be filed under a key naming something else — which is what
/// `let (x) = 5` did (the paren head is a node `ElaboratePatterns.translatePat` erases, so
/// the entry was unreachable and the binding's name was silently lost) and what a
/// module-level tuple destructuring did (a head that binds no single name at all).
/// Widening is one-way, through `BinderKey.identity`.
[<Struct>]
type BinderKeyG<'id> = private | Binder of 'id

/// The binder key of a tree addressed by `NodeKey` — every pre-freeze and frozen domain.
type BinderKey = BinderKeyG<NodeKey>

/// THE definition of "is a binder", in three projections — one per kind of node that can
/// introduce one (the declaration shape's, `ofDeclSlot`, has a whole-declaration
/// enumeration `ofTypeDecl` over it, but they are the one authority). Every `BinderKey`
/// comes from here; see the type's own doc for why that is enforceable.
module BinderKey =

    /// ONE key slot of a declaration shape (`TTypeMemberG.ThisKey` / `BaseKey` / `Params`,
    /// `TClassG.ThisKey`, `TSecondaryCtorG.Params`, `TCtorLetG.Binder`,
    /// `TBaseCtorCallG.CtorParams`) as the binder it is. Those slots are definition sites
    /// by CONSTRUCTION — the shape has nowhere else to put an identity — which is the same
    /// authority `ofTypeDecl` exercises to enumerate them; this is its per-slot form, for a
    /// consumer that must RE-FILE a slot into another identity space rather than merely
    /// list it. It is the one projection that asks the node nothing, and it is confined to
    /// those slots: nothing else on the tree carries a bare definition-site identity.
    let ofDeclSlot (k: 'id) : BinderKeyG<'id> = Binder k

    /// The single binder a PATTERN introduces. `ValueNone` for a pattern that binds nothing
    /// (`Wildcard`, `Const`, …) or that binds only through nested sub-patterns (`Tuple`,
    /// `Record`, `Union`, `TypeTestAs`, `Or` — walk the children for those).
    ///
    /// Generic over the domain on purpose: the pre-freeze producers of the binder-keyed side
    /// tables (`Elaborate`, `Regions`) decide what to key on with the SAME function the
    /// post-freeze pool enumerates with (`TastPools.toPools`), so the two cannot drift.
    ///
    /// Note what is deliberately NOT a binder: a `let` whose head pattern is composite or
    /// wildcard (`let (a, b) = p`, `let _ = e`) introduces no single binder, so it has no
    /// side-table identity — and it needs none, every reader of those tables looking up a
    /// simple binder's key. (A CST `Pat.As` alias — `| 0 as z ->` — is a binder the frozen
    /// tree does not carry at all: `ElaboratePatterns.translatePat` drops the alias name, so
    /// there is no `TPatG` node here to answer for it.)
    let ofPat (p: TPatG<'ty, 'tok, 'id>) : BinderKeyG<'id> voption =
        match p with
        | TPatG.NamedSimple(binding = binding) -> ValueSome(Binder binding)
        | TPatG.Wildcard _
        | TPatG.Tuple _
        | TPatG.Const _
        | TPatG.Record _
        | TPatG.Union _
        | TPatG.TypeTestAs _
        | TPatG.Null _
        | TPatG.EnumCase _
        | TPatG.Or _ -> ValueNone

    /// The binder an EXPRESSION introduces with no pattern node behind it: a `ForTo` loop
    /// variable, whose `i` token has no surrounding `Pat` in the CST. Every other binding
    /// expression (`Lambda`, `Let`, `ForIn`, a match arm) carries a real `TPatG`, so `ofPat`
    /// answers for it and this stays a single case.
    let ofExpr (e: TExprG<'ty, 'tok, 'id>) : BinderKeyG<'id> voption =
        match e with
        | TExprG.ForTo(var = var) -> ValueSome(Binder var)
        | _ -> ValueNone

    /// Every binder a TYPE DECLARATION introduces with no pattern node to introduce it.
    ///
    /// A member body names its receiver and its parameters by `TExpr.Var`, exactly as a
    /// function body names a `let` or a lambda parameter — but those definition sites are
    /// bare `'id` slots on the declaration shape (`ThisKey` / `BaseKey` / `Params`,
    /// the class's own `ThisKey`, a secondary ctor's `Params` and `Lets[].Binder`, the
    /// base-ctor call's view of the primary ctor's params), NOT `TPatG.NamedSimple`
    /// nodes. So a consumer that enumerates definition sites by walking PATTERNS sees
    /// none of them, and any attempt to resolve such a `Var` to its definition comes up
    /// empty. This is that missing half of the enumeration, and it is why the two are
    /// separate projections rather than one walk: they read different slots.
    ///
    /// `'body`-blind — it touches no body — so it serves the tree form and any pooled
    /// (`'body = <id>`) form alike.
    let ofTypeDecl (td: TTypeDeclG<'ty, 'tok, 'id, 'body>) : BinderKeyG<'id> seq =
        let ofMember (m: TTypeMemberG<'ty, 'id, 'body>) =
            seq {
                match m.ThisKey with
                | ValueSome k -> yield ofDeclSlot k
                | ValueNone -> ()

                match m.BaseKey with
                | ValueSome k -> yield ofDeclSlot k
                | ValueNone -> ()

                for (k, _) in EqArray.toArray m.Params do
                    yield ofDeclSlot k
            }

        seq {
            for m in EqArray.toArray (TTypeKindG.members td.Kind) do
                yield! ofMember m

            for m in TTypeKindG.interfaceMembers td.Kind do
                yield! ofMember m

            match td.Kind with
            | TTypeKindG.Class c ->
                // The class-wide `this`: the INSTANCE preamble's expressions read the
                // class's fields through it, so it is a definition site of preamble
                // bodies as well as of the members that carry their own copy.
                yield ofDeclSlot c.ThisKey

                for sc in EqArray.toArray c.SecondaryCtors do
                    for (k, _) in EqArray.toArray sc.Params do
                        yield ofDeclSlot k

                    for l in EqArray.toArray sc.Lets do
                        yield ofDeclSlot l.Binder

                match c.BaseCtorCall with
                | ValueSome bc ->
                    for (k, _) in EqArray.toArray bc.CtorParams do
                        yield ofDeclSlot k
                | ValueNone -> ()
            | TTypeKindG.Interface _
            | TTypeKindG.Union _
            | TTypeKindG.Record _
            | TTypeKindG.Enum _ -> ()
        }

    /// Widen to the address space that holds every node of the tree's own identity axis —
    /// for a lookup driven by a REFERENCE (a `TExpr.Var` names its binder by that axis) or
    /// by a consumer whose own API is node-keyed (`TastUnpool.nodeKeyedSideTables`).
    /// One-way: nothing re-enters the binder domain through it.
    let identity (Binder k) : 'id = k

    /// A whole binder-keyed table read in the REFERENCE domain — the one reason to widen
    /// more than a single key, and named so the reason is stated once rather than at each
    /// site: a lookup driven by a `TExpr.Var` has only the raw identity the reference
    /// carries. One-way, being `identity` per entry.
    let widenMap (m: Map<BinderKeyG<'id>, 'v>) : Map<'id, 'v> =
        m |> Map.toSeq |> Seq.map (fun (b, v) -> identity b, v) |> Map.ofSeq

type TastFileG<'ty, 'tok, 'id when 'id: comparison> =
    {
        /// Source order, every module-level declaration — `inline` bindings INCLUDED, in
        /// both domains. An inline binding is code as well as vocabulary (it is emitted
        /// as an ordinary module function and its `InlineBodies` entry is additive), so
        /// nothing is partitioned out here. `Passes.InlineExpansion` splices a same-unit
        /// inline call off these.
        Decls: EqArray<TDeclG<'ty, 'tok, 'id>>
        /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
        // Qualified: this file `open`s `XParsec.FSharp.Parser`, which also declares a
        // `Diagnostic`; the bare name would bind to the parser's, mistyping the field.
        Diagnostics: XParsec.FSharp.SemanticAnalysis.Diagnostic list
        /// This unit's OWN intrinsics: the canon `SymbolKey` of a `type x = (# "..." #)`
        /// abbrev → its target representation string (`Vesper.int` → `"System.Int32"`).
        /// The backend keys the emitted IL type off the *representation string* (so a
        /// platform author retargets a primitive by editing one `.fs` line), and asks for
        /// it with the KEY the `FTConst` node carries — the frozen face of
        /// `TypeRegistry.IntrinsicReprKeys`, and the local half of the same forward
        /// `{ canon -> platform repr }` axis the provider's `IntrinsicForwardRepr` serves
        /// for the dependency closure. Keyed by identity, never by declared name: a name
        /// cannot say WHICH `int` it means, so a user type sharing an intrinsic's short
        /// name would otherwise pick up its repr.
        ///
        /// A HASH map, not an F# `Map`: a `SymbolKey` is an identity, so it is equatable
        /// but deliberately not ordered. Same face the provider's `IntrinsicForwardRepr`
        /// presents, so the backend's two halves of the axis read alike.
        IntrinsicReprKeys: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, IntrinsicReprInfo>
        /// A module-level binding's binder → its named-holder placement
        /// (`module Foo`'s functions emit on a real `Foo`/`FooModule` static class,
        /// not the anonymous "Program" holder). Empty for a program with no named
        /// modules — every static method then lands on "Program" as before.
        ModuleMembers: Map<BinderKeyG<'id>, ModuleBindingInfo>
        /// A *top-level* (implicit-"Program"-module) binding's binder → its
        /// source name. Top-level bindings (an exe's last file, FS0222) record no
        /// `ModuleBindingInfo`; this names a top-level value lowered to a
        /// Program-holder static field. Empty for a library or a file led by a
        /// `module`/`namespace` declaration.
        TopLevelNames: Map<BinderKeyG<'id>, string>
        /// A closure binder → its stack-vs-heap verdict
        /// (the `EscapeState.LocalStack ∧ RegionRepr.StackOnlyEligible`
        /// conjunction), snapshotted from `ctx.Bindings.Escape` /
        /// `ctx.Bindings.ClosureRepr` after `Regions.run`. Read by codegen's
        /// `discoverClosures` to set `Emit.Closure.Repr`; a binder absent here
        /// (or any anonymous lambda) defaults to `Heap`. Inert today — emission
        /// still forces heap.
        ClosureReprs: Map<BinderKeyG<'id>, ClosureRepr>
        /// A SOURCE-lambda argument's `NodeKey` → its
        /// value-struct closure verdict (`FunVerdict`: the flat `FunN` arity, plus
        /// the result-typar position for a transformer combinator). Snapshotted from
        /// `ctx.FunVerdicts`; `discoverClosures` reads `Arity` to size the closure's
        /// flat `Invoke`, and `ClosureVerdictRewrite` reads `ResultTyparPos` to lay a
        /// stored binding's `'TFunc` slot out as the `<closure>$` value-struct rather
        /// than the `Fun`2`/`Fun`3` interface. A lambda absent here is an ordinary
        /// curried closure.
        ///
        /// The one table that does NOT ride `'id`: a lambda EXPRESSION is not a
        /// definition site, so its key lives in the node space and not in the binder
        /// space the axis names. The pooled form re-keys it onto the lambda's own dense
        /// space (`DenseTable<ExprPoolId, _>`), which is a third space again.
        FunVerdicts: Map<NodeKey, FunVerdict>
        /// A project-local generalised binding's
        /// binder → its frozen typar bounds (method-axis-indexed
        /// `FrozenConstraint` templates). Snapshotted at `Elaborate.run` (where the
        /// method-typar indices are minted, so the bounds' typar leaves line up with
        /// the body's), threaded `TastFile → HolderPlan → StaticFn/StaticMethodRef`
        /// exactly like `FunVerdicts`. Read by the call-site phantom-typar solve
        /// (`EmitCall`); the emitted arity is re-derived independently by
        /// `staticFnTypars`' body sweep.
        GenericFnSchemes: Map<BinderKeyG<'id>, FrozenConstraint list>
        /// The unit's INLINE VOCABULARY: every `let inline` binding (and every
        /// nullary-intrinsic value alias — `let undefined = (# "undefined" #)`, which
        /// the backends also splice rather than call), keyed by the identity its home
        /// unit interns it under.
        ///
        /// Published by `Freeze`, ADDITIVELY: the binding also stays in `Decls` and is
        /// emitted as an ordinary module function, because F# gives an `inline` binding
        /// both faces and a use that cannot be spliced must have something to call.
        ///
        /// The entry is a DIFFERENT TREE from the decl of the same name, not a copy of
        /// it. `Passes.InlineExpansion` walks the emitted form, resolving its
        /// `StaticOptimization` clauses and trait calls against its own definition site —
        /// where an `^T` body has nothing ground. A consumer must resolve them against ITS
        /// operand types, so what is published is the UNEXPANDED body
        /// (`PassContext.InlineTemplates`).
        ///
        /// This is also the ONLY channel for a body with no compiled form at all: an SRTP
        /// member constraint is not encodable on a CLR generic parameter, so such a
        /// binding is published here and emitted nowhere (`TastLower.lower`).
        ///
        /// EMPTY pre-freeze: the SemType tree carries the templates in `Decls` and the
        /// unexpanded snapshots on the `PassContext`.
        InlineBodies: EqArray<TInlineValueG<'ty, 'tok, 'id>>
        /// Declared accessibility of each top-level EXPORTED entity (type / module
        /// value / inline value), keyed by its `SymbolKey`. Stored HONESTLY (not
        /// pre-thresholded): the file→file projection applies internal-or-better, the
        /// `.fsi` extractor public-only, over the SAME fact. Captured by `Elaborate`
        /// from the CST `access` tokens. A key ABSENT here is `Public` (the F# default
        /// for an unmarked declaration). `SymbolKey`-keyed and `'ty`-free — carried
        /// verbatim across the freeze, modeled on `IntrinsicReprKeys`. Type MEMBER
        /// accessibility rides `TTypeMemberG.Accessibility` (physically on the member,
        /// not here), read on the same threshold by the projection's member filter.
        Accessibility: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, Accessibility>
        /// A module binding's single value/function typar-axis width, minted where the
        /// method-axis indices are minted (`Elaborate.mkMethodQuantEnv`). Keyed by the
        /// binder its head pattern introduces; the projection reads it for
        /// `ExternalSymbol.TyparArity` and to size each binding's frozen `ValRepr`.
        BindingTyparArities: Map<BinderKeyG<'id>, int>
    }

// Central monomorphic SemType aliases. Every consumer today speaks `SemType`, over the
// `NodeKey` identity axis every source-shaped tree is addressed in; these aliases keep the
// bare TAST names stable as the generic shapes gain axes.

type TPat = TPatG<SemType, SyntaxToken, NodeKey>
type HoleSpec = HoleSpecG<SemType, SyntaxToken>
type TExpr = TExprG<SemType, SyntaxToken, NodeKey>
type TMatchArm = TMatchArmG<TPat, TExpr>
type FormatSink = FormatSinkG<TExpr>
type FormatSeg = FormatSegG<SemType, SyntaxToken, TExpr>
type DynFormatHole = DynFormatHoleG<SemType, SyntaxToken, TExpr>
type TStaticOptClause = TStaticOptClauseG<SemType, SyntaxToken, NodeKey>
type TDecl = TDeclG<SemType, SyntaxToken, NodeKey>
type TTypeDecl = TTypeDeclG<SemType, SyntaxToken, NodeKey, TExpr>
type TTypeKind = TTypeKindG<SemType, SyntaxToken, NodeKey, TExpr>
type TClass = TClassG<SemType, NodeKey, TExpr>
type TUnionCase = TUnionCaseG<SemType>
type TEnumCase = TEnumCaseG<SyntaxToken>
type TRecordField = TRecordFieldG<SemType>
type TTypeMember = TTypeMemberG<SemType, NodeKey, TExpr>
type TClassLet = TClassLetG<SemType, TExpr>
type TPreambleEntry = TPreambleEntryG<SemType, TExpr>
type TCtorLet = TCtorLetG<SemType, NodeKey, TExpr>
type TCtorFieldInit = TCtorFieldInitG<TExpr>
type TSecondaryCtor = TSecondaryCtorG<SemType, NodeKey, TExpr>
type TBaseCtorCall = TBaseCtorCallG<SemType, NodeKey, TExpr>
type TAbstractMethod = TAbstractMethodG<SemType>
type TInlineBody = TInlineBodyG<SemType, SyntaxToken, NodeKey>
type TInlineValue = TInlineValueG<SemType, SyntaxToken, NodeKey>
type TastFile = TastFileG<SemType, SyntaxToken, NodeKey>

[<RequireQualifiedAccess>]
module TPreambleEntryG =
    /// The `let` binders of a class preamble, in declaration order — the entries that take a
    /// backing field (a `do` has storage nowhere, only an effect). Generic in `'ty`/`'tok`/`'body`,
    /// so this ONE projection serves every consumer — inference-time TAST, frozen TAST, and both
    /// backends — rather than one copy per stage.
    let lets (entries: seq<TPreambleEntryG<'ty, 'body>>) : TClassLetG<'ty, 'body> list =
        [
            for e in entries do
                match e with
                | TPreambleEntryG.Let l -> yield l
                | TPreambleEntryG.Do _ -> ()
        ]

[<RequireQualifiedAccess>]
module TastFileG =
    /// Key→value set equality for the two `IReadOnlyDictionary<SymbolKey,_>` fields.
    /// A `SymbolKey` is an identity (equatable, deliberately unordered), so the file
    /// stores these as HASH maps — and `IReadOnlyDictionary` carries only REFERENCE
    /// equality, so two dictionaries with identical contents never `=`-match. Compare
    /// them as sets instead: same count, and every key maps to an equal value. Order
    /// independent by construction, which is what a hash map's enumeration order
    /// demands.
    let private dictEqual
        (a: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v>)
        (b: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v>)
        : bool =
        a.Count = b.Count
        && a
           |> Seq.forall (fun (KeyValue(k, v)) ->
               match b.TryGetValue k with
               | true, v2 -> v = v2
               | _ -> false
           )

    /// Whole-file structural equality — the equality a `TastFileG` round-trip (freeze
    /// → serialize → rebuild, or `TastUnpool.ofPools ∘ toPools`) is judged by. This is
    /// LIBRARY knowledge, not test knowledge: two of the record's fields
    /// (`IntrinsicReprKeys`, `Accessibility`) are `IReadOnlyDictionary`, which breaks the
    /// derived structural `=` on the whole record, so `a = b` is unsound on a rebuilt file
    /// and every consumer that wants "same contents" must route through here rather than
    /// rediscover the carve-out. Every other field — the decl trees, the `Map` side tables,
    /// the diagnostics list, the inline vocabulary — has sound structural equality.
    let structurallyEqual (a: TastFileG<'ty, 'tok, 'id>) (b: TastFileG<'ty, 'tok, 'id>) : bool =
        a.Decls = b.Decls
        && a.Diagnostics = b.Diagnostics
        && dictEqual a.IntrinsicReprKeys b.IntrinsicReprKeys
        && a.ModuleMembers = b.ModuleMembers
        && a.TopLevelNames = b.TopLevelNames
        && a.ClosureReprs = b.ClosureReprs
        && a.FunVerdicts = b.FunVerdicts
        && a.GenericFnSchemes = b.GenericFnSchemes
        && a.InlineBodies = b.InlineBodies
        && dictEqual a.Accessibility b.Accessibility
        && a.BindingTyparArities = b.BindingTyparArities

// Parallel frozen aliases. Codegen and the freeze step speak these; the bare names
// above STAY `SemType` (inference, Regions, tests, any non-codegen API).

module Frozen =
    type TPat = TPatG<FrozenType, SyntaxToken, NodeKey>
    type HoleSpec = HoleSpecG<FrozenType, SyntaxToken>
    type TExpr = TExprG<FrozenType, SyntaxToken, NodeKey>
    type TMatchArm = TMatchArmG<TPat, TExpr>
    type FormatSink = FormatSinkG<TExpr>
    type FormatSeg = FormatSegG<FrozenType, SyntaxToken, TExpr>
    type DynFormatHole = DynFormatHoleG<FrozenType, SyntaxToken, TExpr>
    type TStaticOptConstraint = TStaticOptConstraintG<FrozenType>
    type TStaticOptClause = TStaticOptClauseG<FrozenType, SyntaxToken, NodeKey>
    type TDecl = TDeclG<FrozenType, SyntaxToken, NodeKey>
    type TTypeDecl = TTypeDeclG<FrozenType, SyntaxToken, NodeKey, TExpr>
    type TTypeKind = TTypeKindG<FrozenType, SyntaxToken, NodeKey, TExpr>
    type TClass = TClassG<FrozenType, NodeKey, TExpr>
    type TUnionCase = TUnionCaseG<FrozenType>
    // Enum cases are `'ty`-free, so the frozen alias is identical to the SemType one.
    type TEnumCase = TEnumCaseG<SyntaxToken>
    type TRecordField = TRecordFieldG<FrozenType>
    type TTypeMember = TTypeMemberG<FrozenType, NodeKey, TExpr>
    type TClassLet = TClassLetG<FrozenType, TExpr>
    type TPreambleEntry = TPreambleEntryG<FrozenType, TExpr>
    type TCtorLet = TCtorLetG<FrozenType, NodeKey, TExpr>
    type TCtorFieldInit = TCtorFieldInitG<TExpr>
    type TSecondaryCtor = TSecondaryCtorG<FrozenType, NodeKey, TExpr>
    type TBaseCtorCall = TBaseCtorCallG<FrozenType, NodeKey, TExpr>
    type TAbstractMethod = TAbstractMethodG<FrozenType>
    type TInlineBody = TInlineBodyG<FrozenType, SyntaxToken, NodeKey>
    type TInlineValue = TInlineValueG<FrozenType, SyntaxToken, NodeKey>
    type TastFile = TastFileG<FrozenType, SyntaxToken, NodeKey>
    type ForInEnumerator = ForInEnumeratorG<FrozenType>
    // The TREE instantiation of the compiled-form cluster: `'pat` is the frozen pattern
    // node itself. This is the form an EXTERNAL symbol carries (`ExternalSymbol.ValRepr`),
    // whose pats are minted from an `.fsi` contract and belong to no file — see
    // `ArgGroupG`. A file's OWN `ValRepr`s are `PooledValRepr`, derived from its columns
    // and naming their pats by pool id (`FrozenPools.BindingValReprs`).
    type StaticParam = StaticParamG<FrozenType, TPat>
    type ArgGroup = ArgGroupG<FrozenType, TPat>
    type ValRepr = ValReprG<FrozenType, TPat>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, TPat>
