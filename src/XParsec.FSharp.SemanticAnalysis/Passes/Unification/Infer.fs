namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate

module UnificationInfer =

    /// Mint fresh TyVars per quantifier, then rewrite `scheme.Body`.
    /// Non-quantified TyVars are left alone — they're free w.r.t. the
    /// surrounding scope and must keep their identity. `scheme.Body` is
    /// already zonked by `generalise`, so we don't follow Links here.
    let private instantiate (ctx: PassContext) (scheme: TypeScheme) : SemType =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
        // freshOf remembers each fresh TyVar so per-quantifier constraints
        // can be re-stamped onto it below.
        let freshOf = Dictionary<TypeVar, TypeVar>(HashIdentity.Reference)

        for q in scheme.Quantified do
            let qRoot = UnionFind.find q
            let fresh = TypeVar()
            fresh.Level <- ctx.CurrentLevel
            subst.[qRoot] <- TyVar fresh
            freshOf.[qRoot] <- fresh

        // Re-stamp constraints onto the fresh instance TyVars so each use
        // site re-evaluates satisfaction against its own substitution; the
        // original quantified TyVars stay constraint-bearing for the next call.
        for (qTv, c) in scheme.Constraints do
            let qRoot = UnionFind.find qTv

            match freshOf.TryGetValue qRoot with
            | true, fresh ->
                if not (fresh.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
                    fresh.Constraints <- c :: fresh.Constraints
            | false, _ -> ()

        substituteWith subst scheme.Body

    /// True if `t` contains a TyVar whose root carries a deferred
    /// `PendingDotAccess` constraint. Such a binding cannot be safely
    /// generalised in v1 — quantifying a TyVar with pending dot accesses
    /// would freeze the constraint into the scheme, and a use site that
    /// pins the receiver would only resolve a fresh instantiation, leaving
    /// the original (still-quantified) constraint dangling. Keeping the
    /// binding monomorphic lets the first use site unify directly with the
    /// pre-instantiation TyVar, which drains the constraint normally.
    let rec private hasPendingDotAccess (t: SemType) : bool =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            if not (List.isEmpty root.PendingDotAccess) then
                true
            else
                match root.Link with
                | ValueSome target -> hasPendingDotAccess target
                | ValueNone -> false
        | TyConst _ -> false
        | TyFun(a, r) -> hasPendingDotAccess a || hasPendingDotAccess r
        | TyTuple xs -> List.exists hasPendingDotAccess xs
        | TyRecord(_, args) -> List.exists hasPendingDotAccess args
        | TyUnion(_, args) -> List.exists hasPendingDotAccess args
        | TyClass(_, args) -> List.exists hasPendingDotAccess args

    /// Apply `Defaults` entries on free TyVars whose level exceeds
    /// `outerLevel`. A default fires when its target resolves to a concrete
    /// shape. Iterates to fixpoint — a chained default like
    /// `default ^T3 : ^T1 ; default ^T1 : int` needs two passes.
    ///
    /// Defaults walked here are *consumed*: once a fire happens (or once
    /// all candidates fail), the `Defaults` list is cleared so subsequent
    /// passes don't re-walk dead targets. A TyVar generalised at a use-site
    /// instantiation is re-stamped with fresh defaults on the next call to
    /// its `Instantiate` closure.
    let private applyDefaults (zonkedTy: SemType) (outerLevel: int) : unit =
        let visited = HashSet<TypeVar>(HashIdentity.Reference)

        let rec collect (t: SemType) : ResizeArray<TypeVar> =
            let acc = ResizeArray<TypeVar>()

            let rec go (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find tv

                    if visited.Add root then
                        if root.Level > outerLevel && root.Link.IsNone && not (List.isEmpty root.Defaults) then
                            acc.Add root

                        match root.Link with
                        | ValueSome target -> go target
                        | ValueNone -> ()
                | TyConst _ -> ()
                | TyFun(a, r) ->
                    go a
                    go r
                | TyTuple xs -> List.iter go xs
                | TyRecord(_, args) -> List.iter go args
                | TyUnion(_, args) -> List.iter go args
                | TyClass(_, args) -> List.iter go args

            go t
            acc

        let candidates = collect zonkedTy

        // ValueNone if every TyVar in the chain is still free.
        let rec resolveTarget (t: SemType) : SemType voption =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> resolveTarget target
                | ValueNone -> ValueNone
            | _ -> ValueSome t

        let tryDefault (tv: TypeVar) : bool =
            let mutable fired = false
            let defaults = tv.Defaults

            for target in defaults do
                if not fired then
                    match resolveTarget target with
                    | ValueSome concrete when not (occursAndAdjust tv concrete) ->
                        // Occurs guard: a chain like `default ^T3 : ^T1`
                        // with a structural target (`^T1 list`) could build
                        // a `concrete` transitively containing tv; linking
                        // through would create an infinite type. Skip on
                        // occurs — the default is unsatisfiable.
                        tv.Link <- ValueSome concrete
                        fired <- true
                    | _ -> ()

            // Clear regardless — discharged, or not worth chasing further.
            tv.Defaults <- []
            fired

        // Iterate to fixpoint: each pass may unblock chained defaults.
        let mutable changed = true

        while changed do
            changed <- false

            for tv in candidates do
                if tv.Link.IsNone && not (List.isEmpty tv.Defaults) then
                    if tryDefault tv then
                        changed <- true

    /// Settle the flexible list-literal containers (R3) reachable from a binding's
    /// type *before* it generalises, so the bare container `TypeVar` is never
    /// quantified as `∀L. L`:
    ///   - element still free (`let xs = []`) → link the container to FSharp.Core's
    ///     `list` now, so the *element* generalises normally (`'a list`);
    ///   - element already concrete (`let nums = [1;2;3]`) → leave the container
    ///     free but drop its level to the outer scope so generalisation skips it,
    ///     deferring the FSharpList-vs-Vesper choice to `resolveListLiterals` (a
    ///     later consumer like `List.fold` can still flip it to the Vesper list).
    let private prepareListLiterals (ctx: PassContext) (ty: SemType) (outerLevel: int) : unit =
        if ctx.ListLiterals.Count = 0 then
            ()
        else
            let flexElem (root: TypeVar) : SemType voption =
                let mutable result = ValueNone

                for (lv, elem) in ctx.ListLiterals do
                    if result.IsNone && System.Object.ReferenceEquals(UnionFind.find lv, root) then
                        result <- ValueSome elem

                result

            let seen = HashSet<TypeVar>(HashIdentity.Reference)

            let rec walk (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find tv

                    if seen.Add root then
                        match root.Link with
                        | ValueSome target -> walk target
                        | ValueNone ->
                            match flexElem root with
                            | ValueSome elemTy when root.Level > outerLevel ->
                                match zonk elemTy with
                                | TyVar _ ->
                                    root.Link <- ValueSome(TyRecord("Microsoft.FSharp.Collections.list", [ elemTy ]))
                                | _ -> root.Level <- outerLevel
                            | _ -> ()
                | TyFun(a, b) ->
                    walk a
                    walk b
                | TyTuple xs
                | TyRecord(_, xs)
                | TyUnion(_, xs)
                | TyClass(_, xs) -> List.iter walk xs
                | TyConst _ -> ()

            walk ty

    let private generalise (zonkedTy: SemType) (outerLevel: int) : TypeScheme =
        // Apply defaults before quantifying: a default that resolves links
        // its source TyVar, which the quantifier walk then skips. Without
        // this, `let x = 1 + 2` would generalise as `∀'a. 'a` instead of
        // `int` (the unbound `^T3` from external-symbol Instantiate).
        applyDefaults zonkedTy outerLevel

        let quantified = ResizeArray<TypeVar>()
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if root.Level > outerLevel && root.Link.IsNone && seen.Add(root) then
                    quantified.Add(root)
            | TyConst _ -> ()
            | TyFun(a, r) ->
                walk a
                walk r
            | TyTuple xs -> List.iter walk xs
            | TyRecord(_, args) -> List.iter walk args
            | TyUnion(_, args) -> List.iter walk args
            | TyClass(_, args) -> List.iter walk args

        walk zonkedTy

        // `instantiate` swaps these onto fresh substitutions per use site
        // so satisfaction is re-evaluated independently.
        let constraints =
            [
                for tv in quantified do
                    for c in tv.Constraints -> tv, c
            ]

        TypeScheme(List.ofSeq quantified, zonkedTy, constraints)

    /// Single-name `let` generalises unless the binding is `mutable`.
    /// Mutable bindings stay monomorphic: every use of the name unifies
    /// against the binding's own TyVar (no instantiation), so a free TyVar
    /// in a mutable binding's type can be pinned later by any use or
    /// assignment — but the binding is never made polymorphic at the
    /// scheme level, which would re-introduce the classic value-
    /// restriction soundness hole. Compound destructuring heads and
    /// bindings whose head is something other than `Pat.NamedSimple`
    /// don't get schemes either — they bind values, not function
    /// abstractions, and the scheme table is keyed by a single NodeKey.
    let private shouldGeneralise (b: Binding<SyntaxToken>) : bool =
        if b.mutableToken.IsSome then
            false
        else
            match b.headPat with
            | Pat.NamedSimple _ -> true
            // An operator-named binding (`let inline (=) …`) is a single-name
            // head; generalise it like any other function value.
            | Pat.Op _ -> true
            | _ -> false

    /// Pulled out of `inferConst` so the measured-literal arm can stamp this
    /// onto a TyVar's `Link` while the measure rides on `Units`.
    let private literalCarrier (t: SyntaxToken) : SemType =
        match t.Token with
        | Token.KWTrue
        | Token.KWFalse -> BuiltinTypes.tyBool
        | Token.NumIEEE64
        | Token.NumIEEE64Hex
        | Token.NumIEEE64Octal
        | Token.NumIEEE64Binary -> BuiltinTypes.tyFloat
        | Token.NumInt64
        | Token.NumInt64Hex
        | Token.NumInt64Octal
        | Token.NumInt64Binary -> BuiltinTypes.tyInt64
        | Token.NumByte
        | Token.NumByteHex
        | Token.NumByteOctal
        | Token.NumByteBinary -> BuiltinTypes.tyByte
        | Token.CharLiteral -> BuiltinTypes.tyChar
        | Token.NumDecimal
        | Token.NumDecimalHex
        | Token.NumDecimalOctal
        | Token.NumDecimalBinary -> BuiltinTypes.tyDecimal
        | _ -> BuiltinTypes.tyInt

    let private inferConst (ctx: PassContext) (c: Constant<SyntaxToken>) : SemType =
        // Unrecognised tokens still type as int (the parser's commonest case)
        // — extend as new literal kinds become reachable.
        match c with
        | Constant.Literal t -> literalCarrier t
        | Constant.MeasuredLiteral(value = t; measure = m) ->
            let carrier = literalCarrier t
            let diagKey = NodeKey.ofToken t NodeKind.ExprConst
            let mt = translateMeasure ctx diagKey m
            let tv = freshTyVar ctx
            tv.Link <- ValueSome carrier
            tv.Units <- ValueSome mt
            TyVar tv

    /// Reads `Units` straight off the root — does NOT use `resolveStep`,
    /// which would follow a measured TyVar through its `Link` to the bare
    /// carrier and drop the measure.
    let private unitsOf (t: SemType) : MeasureTerm voption =
        match t with
        | TyVar tv -> (UnionFind.find tv).Units
        | _ -> ValueNone

    /// Underlying numeric carrier of a (possibly measure-wrapped) type. A
    /// free variable (no Link) is returned as-is so a later unification can
    /// pin it.
    let private carrierOf (t: SemType) : SemType =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome link -> link
            | ValueNone -> TyVar root
        | other -> other

    /// Fresh TyVar pre-stamped with a carrier link and (optionally) a measure.
    let private freshTyVarWith (ctx: PassContext) (carrier: SemType) (units: MeasureTerm voption) : TypeVar =
        let tv = freshTyVar ctx
        tv.Link <- ValueSome carrier
        tv.Units <- units
        tv

    let private isComparisonOp (name: string) : bool =
        match name with
        | "op_Equality"
        | "op_Inequality"
        | "op_LessThan"
        | "op_GreaterThan"
        | "op_LessThanOrEqual"
        | "op_GreaterThanOrEqual" -> true
        | _ -> false

    /// Fires before the provider lookup in `inferInfix` so measured
    /// arithmetic / comparison operators get measure-correct result types and
    /// a dedicated "Measure mismatch" diagnostic rather than a generic
    /// carrier-type mismatch. Returns `None` for the all-dimensionless case
    /// (or operators we don't dispatch); the caller falls through to the
    /// provider path.
    let private tryMeasuredArith
        (ctx: PassContext)
        (key: NodeKey)
        (name: string)
        (leftTy: SemType)
        (rightTy: SemType)
        : SemType option =
        let leftUnits = unitsOf leftTy
        let rightUnits = unitsOf rightTy

        match leftUnits, rightUnits with
        | ValueNone, ValueNone -> None
        | _ ->
            let carrier = carrierOf leftTy
            // Carriers must agree even between measured operands (no
            // `float<m> + int<m>`). Surface that as a normal type mismatch.
            unify ctx key carrier (carrierOf rightTy)

            match name, leftUnits, rightUnits with
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 when m1.Equals m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Code = ""
                        Severity = Error
                    }

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m, ValueNone
            | ("op_Addition" | "op_Subtraction"), ValueNone, ValueSome m ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Code = ""
                        Severity = Error
                    }

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Multiply", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.mul m1 m2))))
            | "op_Multiply", ValueSome m, ValueNone
            | "op_Multiply", ValueNone, ValueSome m -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.div m1 m2))))
            | "op_Division", ValueSome m, ValueNone -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueNone, ValueSome m ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.inv m))))
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name && m1.Equals m2 -> Some BuiltinTypes.tyBool
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Code = ""
                        Severity = Error
                    }

                Some BuiltinTypes.tyBool
            | name, ValueSome m, ValueNone
            | name, ValueNone, ValueSome m when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Code = ""
                        Severity = Error
                    }

                Some BuiltinTypes.tyBool
            | _ -> None

    /// v1 only supports single-segment (`X`) and two-segment qualified
    /// (`R.X`) forms. Multi-segment qualifiers (`A.B.X`) fall through as
    /// ValueNone for the qualifier and the last segment for the field name.
    let private fieldNameAndQualifier (ctx: PassContext) (li: LongIdent<SyntaxToken>) : string voption * string =
        let idents = li.Idents
        let last = ctx.NameOf idents.[idents.Length - 1]

        if idents.Length = 1 then
            ValueNone, last
        elif idents.Length = 2 then
            ValueSome(ctx.NameOf idents.[0]), last
        else
            ValueNone, last

    /// Returns the fresh args together with the substitution mapping each
    /// prototype `TypeVar` onto its fresh stand-in — callers walk declared
    /// field / case-arg types through this subst so every reference to `'a`
    /// lines up with the value in `args`.
    let private freshNamedInstance
        (ctx: PassContext)
        (typeParams: (string * TypeVar) list)
        : SemType list * Dictionary<TypeVar, SemType> =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

        let args =
            [
                for (_, tp) in typeParams ->
                    let fresh = TypeVar()
                    fresh.Level <- ctx.CurrentLevel
                    let protoRoot = UnionFind.find tp
                    // Copy prototype constraints onto the fresh instance so
                    // every use site re-evaluates satisfaction independently
                    // (a `Set<int>` and a `Set<int -> int>` each get their own
                    // copy of `'a : comparison`).
                    fresh.Constraints <- protoRoot.Constraints
                    let asTy = TyVar fresh
                    subst.[protoRoot] <- asTy
                    asTy
            ]

        args, subst

    /// Function value whose argument shape matches the primary constructor
    /// and whose result is the constructed `TyClass`. Routes bare
    /// `Point(3, 4)` calls (no `new`) through the function-application
    /// machinery. `ValueNone` if `name` isn't in `ctx.Types.Class`.
    let private tryClassCtorAsFunction (ctx: PassContext) (name: string) : SemType voption =
        match ctx.Types.Class.TryGetValue name with
        | true, info ->
            let args, subst = freshNamedInstance ctx info.TypeParams
            let receiverTy = TyClass(info.Name, args)

            let paramTys =
                info.CtorParams
                |> Array.map (fun p -> substituteWith subst p.Type)
                |> Array.toList

            let arg =
                match paramTys with
                | [] -> BuiltinTypes.tyUnit
                | [ t ] -> t
                | many -> TyTuple many

            ValueSome(TyFun(arg, receiverTy))
        | false, _ -> ValueNone

    let private classCtorAsFunction (ctx: PassContext) (name: string) : SemType =
        match tryClassCtorAsFunction ctx name with
        | ValueSome t -> t
        | ValueNone -> TyVar(freshTyVar ctx)

    /// Function-shaped type for a DU ctor reference. Multi-field cases bundle
    /// the fields into a tuple — F# DUs take a tuple as their single argument.
    /// The receiver union's typars are instantiated fresh so two independent
    /// uses of `Some` don't share a `'a`.
    let private ctorType (ctx: PassContext) (info: UnionCaseInfo) : SemType =
        let unionInfo = ctx.Types.Union.[info.UnionName]
        let args, subst = freshNamedInstance ctx unionInfo.TypeParams
        let unionTy = TyUnion(info.UnionName, args)

        let walkedFields = info.Fields |> Array.map (substituteWith subst)

        match walkedFields.Length with
        | 0 -> unionTy
        | 1 -> TyFun(walkedFields.[0], unionTy)
        | _ -> TyFun(TyTuple(List.ofArray walkedFields), unionTy)

    /// ValueNone with `count = 0` means "no such ctor"; `count >= 2` means
    /// ambiguous — the caller emits the appropriate diagnostic.
    let private resolveCtorName (ctx: PassContext) (name: string) : UnionCaseInfo voption * int =
        match ctx.Types.CtorIndex.TryGetValue name with
        | false, _ -> ValueNone, 0
        | true, [ info ] -> ValueSome info, 1
        | true, infos -> ValueNone, List.length infos

    /// Resolve a qualified ctor reference `Type.Case` against the union
    /// registry.
    let private resolveQualifiedCtor (ctx: PassContext) (typeName: string) (caseName: string) : UnionCaseInfo voption =
        match ctx.Types.Union.TryGetValue typeName with
        | false, _ -> ValueNone
        | true, info ->
            match info.Cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some c -> ValueSome c
            | None -> ValueNone

    /// Unique record type whose declared field set equals `names`
    /// (order-insensitive). Returns (info, candidateCount); candidateCount
    /// disambiguates the "no match" vs "ambiguous" diagnostic paths.
    let private findUniqueRecordByFieldSet (ctx: PassContext) (names: string list) : RecordTypeInfo voption * int =
        match names with
        | [] -> ValueNone, 0
        | first :: _ ->
            match ctx.Types.FieldIndex.TryGetValue first with
            | false, _ -> ValueNone, 0
            | true, candidates ->
                let nameSet = Set.ofList names

                let matches =
                    candidates
                    |> List.filter (fun info ->
                        let declared = info.Fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                        declared = nameSet
                    )

                match matches with
                | [ info ] -> ValueSome info, 1
                | [] -> ValueNone, 0
                | many -> ValueNone, List.length many

    /// `Circle(r)` parses as `Circle (EnclosedBlock r)`; `Rectangle(w, h)`
    /// as `Circle (EnclosedBlock (Tuple [w; h]))`. v1 supports the
    /// tuple-argument form and a bare single arg — both are what the F# DU
    /// ctor application convention emits.
    let private unwrapCtorArgPattern (p: Pat<SyntaxToken>) : Pat<SyntaxToken> list =
        match p with
        | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) -> List.ofSeq pats
        | Pat.EnclosedBlock(pat = inner) -> [ inner ]
        | Pat.Tuple(patterns = pats) -> List.ofSeq pats
        | _ -> [ p ]

    let rec private inferPat (ctx: PassContext) (p: Pat<SyntaxToken>) : SemType =
        // Each pattern node gets its own TypeVar keyed on its NodeKey; for
        // compound patterns the outer TypeVar is linked to the underlying
        // shape so a lookup against any pattern node returns the right type.
        let key = CstKeys.ofPat p

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t
            n.Length > 0 && System.Char.IsUpper n.[0] && ctx.Types.CtorIndex.ContainsKey n
            ->
            // Uppercase-leading bare ident matching a known ctor —
            // reinterpret as a nullary ctor pattern. Multi-candidate names
            // require a qualifier; diagnose ambiguity, best-effort otherwise.
            let n = ctx.NameOf t
            let info, count = resolveCtorName ctx n

            match info with
            | ValueSome i when i.Fields.Length = 0 ->
                let unionInfo = ctx.Types.Union.[i.UnionName]
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(i.UnionName, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
            | ValueSome i ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Constructor '%s' takes %d argument(s) but is used nullary in pattern position"
                                n
                                i.Fields.Length
                        Code = ""
                        Severity = Error
                    }

                let unionInfo = ctx.Types.Union.[i.UnionName]
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(i.UnionName, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
            | ValueNone when count >= 2 ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf "Ambiguous constructor '%s'; declared in %d union types — add a qualifier" n count
                        Code = ""
                        Severity = Error
                    }

                TyVar(freshTv ctx key)
            | ValueNone -> TyVar(freshTv ctx key)
        | Pat.NamedSimple _ ->
            // Use tvOf so a let-rec sibling whose TyVar was already lazy-minted
            // by a forward reference (or pre-allocated by inferBindingGroup)
            // is reused, not overwritten.
            TyVar(tvOf ctx key)
        | Pat.Op _ ->
            // An operator-named binding head (`let (=) x y = …`) introduces a
            // single name, exactly like a `Pat.NamedSimple`; its name is the
            // operator's compiled name (`op_Equality`), surfaced by Freeze.
            TyVar(tvOf ctx key)
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                last.Length > 0 && System.Char.IsUpper last.[0])
            && (li.Idents.Length = 1
                && ctx.Types.CtorIndex.ContainsKey(ctx.NameOf li.Idents.[0])
                || li.Idents.Length = 2
                   && ctx.Types.Union.ContainsKey(ctx.NameOf li.Idents.[0])
                   && (let info = ctx.Types.Union.[ctx.NameOf li.Idents.[0]]
                       let caseName = ctx.NameOf li.Idents.[1]
                       info.Cases |> Array.exists (fun c -> c.Name = caseName)))
            ->
            // Ctor pattern: `Circle r`, `Rectangle(w, h)`, `Result1.Ok x`.
            let info =
                if li.Idents.Length = 1 then
                    let name = ctx.NameOf li.Idents.[0]

                    match resolveCtorName ctx name with
                    | ValueSome i, _ -> ValueSome i
                    | ValueNone, count when count >= 2 ->
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message =
                                    sprintf
                                        "Ambiguous constructor '%s'; declared in %d union types — add a qualifier"
                                        name
                                        count
                                Code = ""
                                Severity = Error
                            }

                        ValueNone
                    | _ -> ValueNone
                else
                    let typeName = ctx.NameOf li.Idents.[0]
                    let caseName = ctx.NameOf li.Idents.[1]
                    resolveQualifiedCtor ctx typeName caseName

            match info with
            | ValueNone ->
                for sub in args do
                    inferPat ctx sub |> ignore

                TyVar(freshTv ctx key)
            | ValueSome i ->
                // The parser wraps multi-arg ctor patterns in
                // `EnclosedBlock(Tuple [...])`; flatten to the field list.
                let subPats =
                    if args.Length = 1 then
                        unwrapCtorArgPattern args.[0]
                    else
                        List.ofSeq args

                if subPats.Length <> i.Fields.Length then
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message =
                                sprintf
                                    "Constructor '%s' expects %d argument(s) but got %d"
                                    i.Name
                                    i.Fields.Length
                                    subPats.Length
                            Code = ""
                            Severity = Error
                        }

                let unionInfo = ctx.Types.Union.[i.UnionName]
                let args, subst = freshNamedInstance ctx unionInfo.TypeParams
                let m = min subPats.Length i.Fields.Length

                for j = 0 to m - 1 do
                    let sub = subPats.[j]
                    let subTy = inferPat ctx sub
                    unify ctx (CstKeys.ofPat sub) subTy (substituteWith subst i.Fields.[j])

                // Walk any extra sub-patterns so binders still register.
                for j = m to subPats.Length - 1 do
                    inferPat ctx subPats.[j] |> ignore

                let ty = TyUnion(i.UnionName, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
        | Pat.Wildcard _ -> TyVar(freshTv ctx key)
        | Pat.EnclosedBlock(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Tuple(patterns = pats) ->
            let elemTys = [ for p in pats -> inferPat ctx p ]
            let tupleTy = TyTuple elemTys
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome tupleTy
            tupleTy
        | Pat.Const c ->
            let constTy = inferConst ctx c
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome constTy
            constTy
        | Pat.As(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Typed(pat = inner; typ = t) ->
            let innerTy = inferPat ctx inner
            let annTy = translateType ctx t
            unify ctx key innerTy annTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome annTy
            annTy
        | Pat.EmptyBlock _ ->
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome BuiltinTypes.tyUnit
            BuiltinTypes.tyUnit
        | Pat.Or(left = leftPat; right = rightPat) ->
            // Validation checks the name set; here we only unify the
            // patterns' overall types for scrutinee consistency.
            let leftTy = inferPat ctx leftPat
            let rightTy = inferPat ctx rightPat
            unify ctx key leftTy rightTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome leftTy
            leftTy
        | Pat.Record(fieldPats = fieldPats) ->
            let pairs =
                [
                    for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                        let q, n = fieldNameAndQualifier ctx li
                        q, n, sub
                ]

            let qualifier =
                pairs
                |> List.tryPick (fun (q, _, _) ->
                    match q with
                    | ValueSome q -> Some q
                    | _ -> None
                )

            let names = pairs |> List.map (fun (_, n, _) -> n)

            let candidate =
                match qualifier with
                | Some typeName ->
                    match ctx.Types.Record.TryGetValue typeName with
                    | true, info -> ValueSome info
                    | false, _ ->
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message = sprintf "Unknown record type qualifier: %s" typeName
                                Code = ""
                                Severity = Error
                            }

                        ValueNone
                | None ->
                    let cand, count = findUniqueRecordByFieldSet ctx names

                    match cand with
                    | ValueSome _ -> cand
                    | ValueNone ->
                        if count = 0 then
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf "No record type matches the field set: %s" (String.concat ", " names)
                                    Code = ""
                                    Severity = Error
                                }
                        else
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf
                                            "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                            count
                                    Code = ""
                                    Severity = Error
                                }

                        ValueNone

            match candidate with
            | ValueNone ->
                // Walk sub-patterns so binders register as free TyVars.
                for _, _, sub in pairs do
                    inferPat ctx sub |> ignore

                let nodeTv = freshTv ctx key
                TyVar nodeTv
            | ValueSome info ->
                let args, subst = freshNamedInstance ctx info.TypeParams

                for _, fieldName, sub in pairs do
                    let subTy = inferPat ctx sub

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofPat sub) subTy (substituteWith subst field.Type)
                    | None ->
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofPat sub
                                Message = sprintf "Type '%s' has no field '%s'" info.Name fieldName
                                Code = ""
                                Severity = Error
                            }

                let recTy = TyRecord(info.Name, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome recTy
                recTy
        | _ ->
            // TODO: Named (DU ctor) / Cons patterns — they need
            // provider lookups or recursive shape unification.
            TyVar(freshTv ctx key)

    /// Reuses the lexer's canonical placeholder parser
    /// (`Lexing.parseFormatSpecifierView`) so no second copy of the format
    /// grammar lives here. `ValueNone` when the string carries interpolation
    /// holes or lexer-error parts (not a simple format literal), so the
    /// printf special-case falls through to standard inference.
    let private formatSpecifiers (ctx: PassContext) (e: Expr<SyntaxToken>) : FormatType list voption =
        match e with
        | Expr.String(parts = parts) ->
            let acc = ResizeArray<FormatType>()
            let mutable ok = true

            for part in parts do
                match part with
                | StringPart.Text _
                | StringPart.EscapeSequence _
                | StringPart.EscapePercent _
                | StringPart.VerbatimEscapeQuote _ -> ()
                | StringPart.FormatSpecifier t ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome placeholder -> acc.Add placeholder.Type
                    | ValueNone -> ok <- false
                | StringPart.Expr _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> ok <- false

            if ok then ValueSome(List.ofSeq acc) else ValueNone
        | _ -> ValueNone

    /// Whether every specifier is one the happy path lowers inline
    /// (`PrintfSpec.tryHoleFormat`); a `false` keeps the FSharp.Core cold
    /// path. `%%` escapes are lowerable (P2): Freeze collapses `%%`→`%` in the
    /// literal segment. Only interpolation holes (`Expr`), orphan specifiers
    /// and lexer-error parts force the cold path.
    let private lowerablePlaceholders (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
        match e with
        | Expr.String(parts = parts) ->
            let mutable ok = true

            for part in parts do
                match part with
                | StringPart.FormatSpecifier t ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p ->
                        match PrintfSpec.tryHoleFormat p with
                        | ValueSome _ -> ()
                        | ValueNone -> ok <- false
                    | ValueNone -> ok <- false
                // A `%%` escape arrives as raw `Text` "%%" — still lowerable.
                | StringPart.Text _
                | StringPart.EscapeSequence _
                | StringPart.VerbatimEscapeQuote _
                | StringPart.EscapePercent _ -> ()
                | StringPart.Expr _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> ok <- false

            ok
        | _ -> false

    let rec infer (ctx: PassContext) (e: Expr<SyntaxToken>) : SemType =
        let key = CstKeys.ofExpr e
        let nodeTv = freshTv ctx key

        let inferredTy =
            match e with
            | Expr.Const c -> inferConst ctx c
            | Expr.Ident _ -> inferIdent ctx e key
            | Expr.LongIdentOrOp _ -> inferIdent ctx e key
            | Expr.App(fn, args) -> inferApp ctx key fn args
            | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) -> inferHighPrecApp ctx key fn arg
            | Expr.InfixApp(left, _, right) -> inferInfix ctx key left right
            | Expr.PrefixApp(_, operand) -> inferPrefix ctx key operand
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun ctx argPats body
            | Expr.LetOrUse(bindings = bindings; body = body) -> inferLet ctx key bindings body
            | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                inferListLikeLiteral ctx key inner false
            | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                inferListLikeLiteral ctx key inner true
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse ctx key cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple ctx items
            | Expr.Sequential(exprs = items) -> inferSequential ctx key items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation ctx key inner t
            | Expr.EmptyBlock(lParen = ParenKind.List _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                emptyListLikeLiteral ctx key false
            | Expr.EmptyBlock(lParen = ParenKind.Array _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                emptyListLikeLiteral ctx key true
            | Expr.EmptyBlock _ -> BuiltinTypes.tyUnit
            | Expr.While(condition = cond; body = body) -> inferWhile ctx key cond body
            | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
                inferForTo ctx key ident startE endE body
            | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) -> inferForIn ctx key pat src body
            | Expr.String(parts = parts) -> inferString ctx key parts
            | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) -> inferMatch ctx key scrutinee rules
            | Expr.Function(rules = Rules(rules = rules)) -> inferFunction ctx key rules
            | Expr.TryWith(expr = body; rules = Rules(rules = rules)) -> inferTryWith ctx key body rules
            | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) -> inferTryFinally ctx key body finallyE
            | Expr.Assignment(leftExpr = left; rightExpr = right) -> inferAssignment ctx key left right
            | Expr.Range(fromExpr = a; toExpr = b) -> inferRange ctx key a ValueNone b
            | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) -> inferRange ctx key a (ValueSome s) b
            | Expr.Null _ ->
                // No reference-type bound yet — free TypeVar so surrounding
                // context can pin it.
                TyVar(freshTyVar ctx)
            | Expr.Record(fieldInitializers = inits) -> inferRecord ctx key inits
            | Expr.RecordClone(expr = src; fieldInitializers = inits) -> inferRecordClone ctx key src inits
            // Static member on an *external* type: `EqualityComparer<int>.Default`
            // — the receiver is a (generic) type name the provider resolves, not a
            // value. Checked before the field-access arm so the type-name receiver
            // isn't `infer`d as a value. (Instance access — `value.Member` — falls
            // through to `inferFieldAccess`/`resolveFieldStep`.)
            | Expr.DotLookup(expr = recv; longIdentOrOp = LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 1 && (tryExternalTypeReceiver ctx recv).IsSome
                ->
                let (metaName, typeArgsCst) = (tryExternalTypeReceiver ctx recv).Value
                let args = [ for t in typeArgsCst -> translateType ctx t ]
                inferExternalStaticMember ctx key metaName args li.Idents.[0]
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                inferFieldAccess ctx key r li.Idents.[0]
            | Expr.New(typ = t; expr = argExpr) -> inferNew ctx key t argExpr
            | Expr.ILIntrinsic(args = args; returnType = rt) -> inferILIntrinsic ctx args rt
            | Expr.LibraryOnlyStaticOptimization(expr = baseE; constraints = cs; optimizedExpr = optE) ->
                inferLibraryOnlyStaticOptimization ctx key baseE cs optE
            | _ ->
                // Surface the unhandled case loudly rather than fabricating a
                // free TyVar and silently producing a broken type for every
                // use site. Matches the precedent in
                // `Freeze.translateExpr` (file: Freeze.fs).
                failwithf "infer: TODO %A" e

        nodeTv.Link <- ValueSome inferredTy
        inferredTy

    and private inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        // A multi-segment LongIdent whose head is a local binding is a
        // record-field access chain (`r.X.Y`), not a qualified name — the
        // parser rides these inside a single `Expr.LongIdentOrOp` rather
        // than emitting `Expr.DotLookup`.
        match e with
        // `(+)` used as a value: resolve the operator's compiled name through
        // the provider, instantiating its scheme like any external symbol.
        // Freeze projects this to `External("op_Addition", …)`.
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name ->
                // Operator compiled names (`op_Addition`) resolve through the
                // open scope: bare name first, then explicit opens, then the
                // ambient prelude (a contract's `[<AutoOpen>]` operator module).
                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
                | ValueSome sym -> sym.Instantiate ctx.CurrentLevel
                | ValueNone ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Operator '%s' is not available from the symbol provider" name
                            Code = ""
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | ValueNone -> TyVar(freshTyVar ctx)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx key li
        // Two-segment qualified reference whose head is *not* a local binding:
        // `Math.Pi` / `Lst.Empty` / `Result2.Ok`. Dispatches on whether the head
        // names a class or a union (static-member vs union-case lookup).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2 && not (ctx.Bindings.Binding.ContainsKey key)
            ->
            let headName = ctx.NameOf li.Idents.[0]
            let tailName = ctx.NameOf li.Idents.[1]

            let tryStaticMember (typeParams: (string * TypeVar) list) (members: TypeMemberInfo[]) =
                match members |> Array.tryFind (fun m -> m.IsStatic && m.Name = tailName) with
                | Some m ->
                    let _, subst = freshNamedInstance ctx typeParams
                    ValueSome(substituteWith subst m.Type)
                | None -> ValueNone

            // Class static member takes priority over union static member which
            // takes priority over a union ctor — preserves the original cascade
            // order so a static member shadows the not-a-case diagnostic.
            let classHit =
                match ctx.Types.Class.TryGetValue headName with
                | true, info -> tryStaticMember info.TypeParams info.Members
                | false, _ -> ValueNone

            match classHit with
            | ValueSome ty -> ty
            | ValueNone ->
                match ctx.Types.Union.TryGetValue headName with
                | true, info ->
                    match tryStaticMember info.TypeParams info.Members with
                    | ValueSome ty -> ty
                    | ValueNone ->
                        // Qualified ctor reference `Result2.Ok` — via the union
                        // registry, bypassing the CtorIndex ambiguity check.
                        match resolveQualifiedCtor ctx headName tailName with
                        | ValueSome info -> ctorType ctx info
                        | ValueNone ->
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message = sprintf "Union '%s' has no case '%s'" headName tailName
                                    Code = ""
                                    Severity = Error
                                }

                            TyVar(freshTyVar ctx)
                | false, _ -> inferIdentDefault ctx e key
        | _ -> inferIdentDefault ctx e key

    /// Fallback for `inferIdent`: identifiers that aren't recognised as a
    /// field-access chain or a qualified class/union reference. Resolves
    /// through the local binding map, then the provider, then `Class`-name and
    /// `Union`-case registries (the latter two only for single-segment names).
    and private inferIdentDefault (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb ->
            // Already-generalised binding: instantiate its scheme for
            // independent use-sites. Otherwise the monomorphic TyVar from
            // inferPat — including uses inside a sibling's RHS in the same
            // `let rec` group, which is what forbids polymorphic recursion.
            match ctx.Bindings.Scheme.TryGetValue rb.BindingSite with
            | ValueSome scheme -> instantiate ctx scheme
            | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
        | ValueNone ->
            // Provider first — provider hits beat ctor-name resolution
            // when both exist (a let-bound `Ok` would have a Binding entry
            // and never reach here). Bare single-segment idents absent
            // from the provider fall to the ctor registry.
            let name = qualifiedNameOf ctx e

            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym -> sym.Instantiate ctx.CurrentLevel
            | ValueNone ->

                match tryExternalStaticLongIdent ctx key e with
                | ValueSome ty -> ty
                | ValueNone ->
                    let singleSegName =
                        match e with
                        | Expr.Ident t -> ValueSome(ctx.NameOf t)
                        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                            ValueSome(ctx.NameOf li.Idents.[0])
                        | _ -> ValueNone

                    match singleSegName with
                    | ValueSome n ->
                        let info, count = resolveCtorName ctx n

                        match info with
                        | ValueSome i -> ctorType ctx i
                        | ValueNone when count >= 2 ->
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf
                                            "Ambiguous constructor '%s'; declared in %d union types — add a qualifier or annotation"
                                            n
                                            count
                                    Code = ""
                                    Severity = Error
                                }

                            TyVar(freshTyVar ctx)
                        | ValueNone ->
                            // Class-name-as-function: `Point(3, 4)` parses as
                            // `Expr.App (Expr.Ident "Point", ...)`. Return the
                            // ctor as a function value so `inferApp` types the
                            // call through the normal function arm.
                            classCtorAsFunction ctx n
                    | ValueNone -> TyVar(freshTyVar ctx)

    /// Joins multi-segment names with `.` so the provider can look up dotted
    /// names like `Math.PI` directly.
    and private qualifiedNameOf (ctx: PassContext) (e: Expr<SyntaxToken>) : string =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
        | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

    and private inferApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        match tryInferPrintfApp ctx key fn args with
        | ValueSome ty -> ty
        | ValueNone ->
            // A .NET static method is tupled: `String.Concat ("a", "b")` is one
            // tuple argument. Resolve a multi-overload static method by its arg
            // types at the call site (type-args-bug.md Layer 2) before the generic
            // curried application path.
            match
                (if args.Length = 1 then
                     tryInferExternalStaticMethodCall ctx key fn args.[0]
                 else
                     ValueNone)
            with
            | ValueSome ty -> ty
            | ValueNone ->

                let mutable currTy = infer ctx fn

                for a in args do
                    let argTy = infer ctx a
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key currTy (TyFun(argTy, resultTy))
                    currTy <- resultTy

                currTy

    /// Printf-family typing rule (front-end-gaps-plan §B). When `fn` is a
    /// recognised printf entry point (not shadowed by a local binding) with a
    /// plain-literal format argument, the format spec — not the literal's
    /// apparent `string` type — drives the call's curried result type. The
    /// format argument types as `PrintfFormat<printer, …>`. Non-literal
    /// format strings return `ValueNone` and fall through to standard inference.
    and private tryInferPrintfApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType voption =
        let fnKey = CstKeys.ofExpr fn

        // A local binding shadowing a printf name is an ordinary function —
        // don't apply the special rule.
        if ctx.Bindings.Binding.ContainsKey fnKey then
            ValueNone
        else
            match fn with
            | Expr.Ident _
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
                match PrintfSpec.tryFamily (qualifiedNameOf ctx fn) with
                | ValueNone -> ValueNone
                | ValueSome fam ->
                    let idx = fam.FormatArgIndex

                    if args.Length <= idx then
                        // Format argument not supplied (e.g. partially-applied
                        // `fprintf writer`); defer to standard inference.
                        ValueNone
                    else
                        match formatSpecifiers ctx args.[idx] with
                        | ValueNone -> ValueNone
                        | ValueSome specs ->
                            let fresh () = TyVar(freshTyVar ctx)

                            match PrintfSpec.appliedTypeOf fresh specs fam with
                            // A specifier we don't type in v1 (`%a` / `%t`);
                            // defer to standard inference.
                            | ValueNone -> ValueNone
                            | ValueSome(fnTy, fmtTy, _) ->
                                // Stamp the function node so Freeze threads the
                                // curried result type through the App chain.
                                (freshTv ctx fnKey).Link <- ValueSome fnTy

                                let mutable currTy = fnTy

                                for i in 0 .. args.Length - 1 do
                                    let a = args.[i]

                                    let argTy =
                                        if i = idx then
                                            // The format literal types as the
                                            // PrintfFormat — not as `string`.
                                            (freshTv ctx (CstKeys.ofExpr a)).Link <- ValueSome fmtTy
                                            fmtTy
                                        else
                                            infer ctx a

                                    let resultTy = TyVar(freshTyVar ctx)
                                    unify ctx key currTy (TyFun(argTy, resultTy))
                                    currTy <- resultTy

                                // P1 happy-path lowering marker: fully-applied
                                // literal call, a StdOut/StdErr/StringResult
                                // sink, and every specifier in `tryHoleFormat`
                                // → Freeze mints a `TExpr.Format`. Otherwise the
                                // FSharp.Core path stands (additive — `%A`,
                                // partial application, etc. unaffected).
                                match PrintfSpec.sinkOf (qualifiedNameOf ctx fn) with
                                | ValueSome sink when
                                    idx = 0
                                    && args.Length = specs.Length + 1
                                    && lowerablePlaceholders ctx args.[idx]
                                    ->
                                    ctx.PrintfApp.Set(key, sink)
                                | _ -> ()

                                ValueSome currTy
            | _ -> ValueNone

    and private inferHighPrecApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        : SemType =
        // `f(x)` — same shape as `Expr.App fn [|arg|]`, a separate CST case. A
        // no-space method call (`String.Concat("a", "b")`) is a HighPrecedenceApp,
        // so the call-site overload resolver is checked here too.
        match tryInferExternalStaticMethodCall ctx key fn arg with
        | ValueSome ty -> ty
        | ValueNone ->
            let fnTy = infer ctx fn
            let argTy = infer ctx arg
            let resultTy = TyVar(freshTyVar ctx)
            unify ctx key fnTy (TyFun(argTy, resultTy))
            resultTy

    and private inferRange
        (ctx: PassContext)
        (key: NodeKey)
        (fromE: Expr<SyntaxToken>)
        (stepE: Expr<SyntaxToken> voption)
        (toE: Expr<SyntaxToken>)
        : SemType =
        // Tiny subset: endpoints (and step) constrained to int, result the
        // `seq<int>` placeholder. Real F# is generic over the `..` overload.
        let fromTy = infer ctx fromE
        unify ctx key fromTy BuiltinTypes.tyInt

        match stepE with
        | ValueSome s ->
            let stepTy = infer ctx s
            unify ctx key stepTy BuiltinTypes.tyInt
        | ValueNone -> ()

        let toTy = infer ctx toE
        unify ctx key toTy BuiltinTypes.tyInt
        BuiltinTypes.tySeqInt

    and private inferInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        let leftTy = infer ctx left
        let rightTy = infer ctx right

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match tryMeasuredArith ctx key name leftTy rightTy with
            | Some resultTy -> resultTy
            | None ->
                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
                | ValueSome sym ->
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(leftTy, TyFun(rightTy, resultTy)))
                    resultTy
                | ValueNone ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Unknown operator symbol: %s" name
                            Code = ""
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
        | ValueSome _
        | ValueNone ->
            // Desugar didn't recognise the operator (non-OpName can't happen
            // for an InfixApp key) — leave the result free.
            TyVar(freshTyVar ctx)

    and private inferPrefix (ctx: PassContext) (key: NodeKey) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown prefix operator: %s" name
                        Code = ""
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | ValueSome _
        | ValueNone -> TyVar(freshTyVar ctx)

    and private inferIfThenElse
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy BuiltinTypes.tyBool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx key elifCondTy BuiltinTypes.tyBool
            let elifTy = infer ctx elifExpr
            unify ctx key thenTy elifTy

        match elseB with
        | ValueSome(ElseBranch(expr = elseExpr)) ->
            let elseTy = infer ctx elseExpr
            unify ctx key thenTy elseTy
            thenTy
        | ValueNone ->
            // `if c then e` (no else) requires e : unit — not yet supported.
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "if-then without else not yet supported"
                    Code = ""
                    Severity = Error
                }

            thenTy

    and private inferFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let argTypes = [ for p in argPats -> inferPat ctx p ]
        let bodyTy = infer ctx body
        List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

    and private inferTuple (ctx: PassContext) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        TyTuple [ for e in items -> infer ctx e ]

    and private inferSequential (ctx: PassContext) (key: NodeKey) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        // All but the last must be unit; result is the last's type.
        if items.Length = 0 then
            BuiltinTypes.tyUnit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty BuiltinTypes.tyUnit

            infer ctx items.[items.Length - 1]

    /// `pEnclosed` virtual-inserts the expected close token (with a
    /// parser-side diagnostic) when the source token is missing or
    /// mismatched. That parser diagnostic isn't visible to semantic-analysis
    /// consumers, so surface the breakage on `ctx.Diagnostics` too —
    /// otherwise the malformed literal types successfully and Freeze emits a
    /// well-shaped TAST as if the source were correct.
    and private checkLiteralClose
        (ctx: PassContext)
        (key: NodeKey)
        (rTok: SyntaxToken)
        (expected: Token)
        (display: string)
        : unit =
        match rTok.Index with
        | TokenIndex.Virtual ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Mismatched or missing closing delimiter: expected '%s'" display
                    Code = ""
                    Severity = Error
                }
        | TokenIndex.Regular _ when rTok.Token <> expected ->
            // Defensive: pEnclosed only emits a real rParen when the peeked
            // token matched, so this can't trigger today — guards against a
            // future parser change letting a mismatched close-token through.
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Mismatched closing delimiter: expected '%s'" display
                    Code = ""
                    Severity = Error
                }
        | TokenIndex.Regular _ -> ()

    /// The list type a `[…]` literal carries. Three cases:
    ///   1. A program that declares its own `'T list` abbreviation (the self-host
    ///      shape — `List.fs`'s `and 'T list = List<'T>`) resolves eagerly to its
    ///      RHS union (unchanged).
    ///   2. A bare program (R3): the container is left *flexible* — a fresh
    ///      `TypeVar` registered in `ctx.ListLiterals` (with its element). A
    ///      consumer can drive it: `List.fold`'s `Vesper.Collections.List`
    ///      parameter flips it to the Vesper list (so the literal emits BCL-only),
    ///      while a literal nothing else pins (`printfn "%A" [1;2;3]`) defaults
    ///      back to FSharp.Core's `list` in `resolveListLiterals`. This is the
    ///      consumer-driven typing handoff R3 calls for: `%A` stays `FSharpList`
    ///      (its cold printf path), `List.fold` retargets to the Vesper list.
    and private listLiteralTy (ctx: PassContext) (key: NodeKey) (elemTy: SemType) : SemType =
        match ctx.Types.Abbreviation.TryGetValue "list" with
        | true, info ->
            forceFill ctx info
            expandAbbreviation ctx key info [ elemTy ]
        | false, _ ->
            let tv = freshTyVar ctx
            ctx.ListLiterals.Add(UnionFind.find tv, elemTy)
            TyVar tv

    and private inferListLikeLiteral
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (isArray: bool)
        : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        let items =
            match body with
            | Expr.Sequential(exprs = items) -> items
            | single -> ImmutableArray.Create(single)

        for i = 0 to items.Length - 1 do
            let itemTy = infer ctx items.[i]
            unify ctx key itemTy elemTy

        if isArray then
            TyRecord("Microsoft.FSharp.Core.[]", [ elemTy ])
        else
            listLiteralTy ctx key elemTy

    /// Element type stays free so context can pin it (`let xs : int list = []`).
    and private emptyListLikeLiteral (ctx: PassContext) (key: NodeKey) (isArray: bool) : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        if isArray then
            TyRecord("Microsoft.FSharp.Core.[]", [ elemTy ])
        else
            listLiteralTy ctx key elemTy

    and private inferWhile
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy BuiltinTypes.tyBool
        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    and private inferForTo
        (ctx: PassContext)
        (key: NodeKey)
        (ident: SyntaxToken)
        (startE: Expr<SyntaxToken>)
        (endE: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let startTy = infer ctx startE
        unify ctx key startTy BuiltinTypes.tyInt
        let endTy = infer ctx endE
        unify ctx key endTy BuiltinTypes.tyInt
        let varKey = CstKeys.ofForToVar ident
        let varTv = freshTv ctx varKey
        varTv.Link <- ValueSome BuiltinTypes.tyInt
        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    and private inferForIn
        (ctx: PassContext)
        (key: NodeKey)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        // Int-range source: element type is int. No `seq<T>` machinery yet
        // for anything else — pattern stays unconstrained, Info flags the gap.
        let srcTy = infer ctx src
        let patTy = inferPat ctx pat

        let isRangeSource =
            match src with
            | Expr.Range _
            | Expr.SteppedRange _ -> true
            | Expr.EnclosedBlock(expr = Expr.Range _)
            | Expr.EnclosedBlock(expr = Expr.SteppedRange _) -> true
            | _ -> false

        if isRangeSource then
            unify ctx key srcTy BuiltinTypes.tySeqInt
            unify ctx key patTy BuiltinTypes.tyInt
        else
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "for-in: enumerable / element-type checking not yet implemented"
                    Code = ""
                    Severity = Error
                }

        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    and private inferRules
        (ctx: PassContext)
        (key: NodeKey)
        (scrutineeTy: SemType)
        (resultTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : unit =
        for r in rules do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let patTy = inferPat ctx pat
                unify ctx key patTy scrutineeTy

                match guard with
                | ValueSome(PatternGuard(expr = g)) ->
                    let gTy = infer ctx g
                    unify ctx key gTy BuiltinTypes.tyBool
                | ValueNone -> ()

                let bodyTy = infer ctx body
                unify ctx key bodyTy resultTy
            | _ -> ()

    and private inferMatch
        (ctx: PassContext)
        (key: NodeKey)
        (scrutinee: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        let scrutineeTy = infer ctx scrutinee
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key scrutineeTy resultTy rules
        resultTy

    and private inferFunction (ctx: PassContext) (key: NodeKey) (rules: ImmutableArray<Rule<SyntaxToken>>) : SemType =
        // `function … ` ~ `fun x -> match x with …`. The synthesised
        // parameter's TypeVar IS the scrutinee's — every arm's pattern
        // unifies with it.
        let paramTy = TyVar(freshTyVar ctx)
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key paramTy resultTy rules
        TyFun(paramTy, resultTy)

    and private inferTryWith
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // Until a real `exn` type lands, pin the scrutinee to placeholder
        // `TyConst "exn"`. A fresh TyVar would let wildcard / variable arm
        // patterns carry an unresolved TyVar into the TAST, which
        // `ResolvedTypes` correctly flags.
        let resultTy = infer ctx body
        let exnTy = TyConst "exn"
        inferRules ctx key exnTy resultTy rules
        resultTy

    and private inferTryFinally
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : SemType =
        let resultTy = infer ctx body
        let finallyTy = infer ctx finallyE
        unify ctx key finallyTy BuiltinTypes.tyUnit
        resultTy

    and private inferAssignment
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        // Mutability of the LHS is a Validation concern; here we only typecheck.
        let leftTy = infer ctx left
        let rightTy = infer ctx right
        unify ctx key leftTy rightTy
        BuiltinTypes.tyUnit

    and private inferRecord
        (ctx: PassContext)
        (key: NodeKey)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let pairs =
            [
                for FieldInitializer(longIdent = li; expr = e) in inits ->
                    let q, n = fieldNameAndQualifier ctx li
                    q, n, e
            ]

        let qualifier =
            pairs
            |> List.tryPick (fun (q, _, _) ->
                match q with
                | ValueSome q -> Some q
                | _ -> None
            )

        let names = pairs |> List.map (fun (_, n, _) -> n)

        let candidate =
            match qualifier with
            | Some typeName ->
                match ctx.Types.Record.TryGetValue typeName with
                | true, info -> ValueSome info
                | false, _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Unknown record type qualifier: %s" typeName
                            Code = ""
                            Severity = Error
                        }

                    ValueNone
            | None ->
                let cand, count = findUniqueRecordByFieldSet ctx names

                match cand with
                | ValueSome _ -> cand
                | ValueNone ->
                    if count = 0 then
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message = sprintf "No record type matches the field set: %s" (String.concat ", " names)
                                Code = ""
                                Severity = Error
                            }
                    else
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message =
                                    sprintf
                                        "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                        count
                                Code = ""
                                Severity = Error
                            }

                    ValueNone

        match candidate with
        | ValueNone ->
            for _, _, e in pairs do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)
        | ValueSome info ->
            // Fresh typars per literal so independent literals get independent
            // vars; each initialiser unifies against the field type *under this
            // substitution*, pinning a `'a` field to the initialiser's type.
            let args, subst = freshNamedInstance ctx info.TypeParams

            for _, fieldName, e in pairs do
                let eTy = infer ctx e

                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                | None ->
                    ctx.Diagnostics.Add
                        {
                            Key = CstKeys.ofExpr e
                            Message = sprintf "Type '%s' has no field '%s'" info.Name fieldName
                            Code = ""
                            Severity = Error
                        }

            TyRecord(info.Name, args)

    and private inferRecordClone
        (ctx: PassContext)
        (key: NodeKey)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let srcTy = infer ctx src

        match resolveStep srcTy with
        | TyRecord(recName, srcArgs) ->
            match ctx.Types.Record.TryGetValue recName with
            | true, info ->
                // Clone preserves the source's arg list — overrides unify
                // against the substituted field type (`'a` → source's arg).
                let subst = mkNamedTypeSubst info.TypeParams srcArgs

                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let _, fieldName = fieldNameAndQualifier ctx li
                    let eTy = infer ctx e

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                    | None ->
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofExpr e
                                Message = sprintf "Type '%s' has no field '%s'" recName fieldName
                                Code = ""
                                Severity = Error
                            }

                TyRecord(recName, srcArgs)
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown record type '%s'" recName
                        Code = ""
                        Severity = Error
                    }

                for FieldInitializer(expr = e) in inits do
                    infer ctx e |> ignore

                TyRecord(recName, srcArgs)
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "Record clone requires the source expression to be a record"
                    Code = ""
                    Severity = Error
                }

            for FieldInitializer(expr = e) in inits do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)

    /// One step of dot-access resolution. Deferred when the receiver is a
    /// free TyVar. For a generic receiver `(b : Box<int>).Value`, the
    /// declared field / member type `'a` is substituted against the
    /// receiver's arg list so `Value` types as `int`, not a free typar.
    /// Record-field, class- and union-member access all route through here;
    /// the receiver's shape discriminates.
    and private resolveFieldStep (ctx: PassContext) (diagKey: NodeKey) (rTy: SemType) (memberName: string) : SemType =
        match resolveStep rTy with
        | TyRecord(recName, args) ->
            match ctx.Types.Record.TryGetValue recName with
            | true, info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = memberName) with
                | Some field -> instantiateMember (info.TypeParams, args) field.Type
                | None ->
                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message = sprintf "Type '%s' has no field '%s'" recName memberName
                            Code = ""
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = diagKey
                        Message = sprintf "Unknown record type '%s'" recName
                        Code = ""
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | TyClass(clsName, args) ->
            match ctx.Types.Class.TryGetValue clsName with
            | true, info ->
                match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                | Some m -> instantiateMember (info.TypeParams, args) m.Type
                | None ->
                    // Distinguish "no such member" from "member is static —
                    // access via class name, not an instance".
                    let isStaticHit =
                        info.Members |> Array.exists (fun m -> m.Name = memberName && m.IsStatic)

                    let msg =
                        if isStaticHit then
                            sprintf
                                "Member '%s' on type '%s' is static; access it via '%s.%s'"
                                memberName
                                clsName
                                clsName
                                memberName
                        else
                            sprintf "Type '%s' has no instance member '%s'" clsName memberName

                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message = msg
                            Code = ""
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | false, _ ->
                // Not a project-local class — an *external* type (e.g. a BCL
                // `TyClass("…EqualityComparer`1", [int])` produced by a prior static
                // access). Resolve the instance member through the provider and
                // record it for Freeze (symbol-resolution-plan §7.2, P3).
                match ctx.Provider.TryLookupMember(clsName, memberName) with
                | ValueSome m when not m.IsStatic ->
                    ctx.Resolution.ExternalAccess.Set(
                        diagKey,
                        {
                            Key = m.Key
                            IsStatic = false
                            IsProperty = m.IsProperty
                        }
                    )

                    m.BuildSignature(List.toArray args)
                | _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message = sprintf "Unknown class type '%s'" clsName
                            Code = ""
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
        | TyUnion(unionName, args) ->
            // Union instance member access (P3d.3) — mirrors the `TyClass`
            // arm against the union's augmentation members.
            match ctx.Types.Union.TryGetValue unionName with
            | true, info ->
                match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                | Some m -> instantiateMember (info.TypeParams, args) m.Type
                | None ->
                    let isStaticHit =
                        info.Members |> Array.exists (fun m -> m.Name = memberName && m.IsStatic)

                    let msg =
                        if isStaticHit then
                            sprintf
                                "Member '%s' on type '%s' is static; access it via '%s.%s'"
                                memberName
                                unionName
                                unionName
                                memberName
                        else
                            sprintf "Type '%s' has no instance member '%s'" unionName memberName

                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message = msg
                            Code = ""
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = diagKey
                        Message = sprintf "Unknown union type '%s'" unionName
                        Code = ""
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | TyVar tv ->
            let root = UnionFind.find tv
            let resultTv = freshTyVar ctx

            let access =
                {
                    MemberName = memberName
                    UseKey = diagKey
                    ResultTv = resultTv
                }

            root.PendingDotAccess <- access :: root.PendingDotAccess
            TyVar resultTv
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = sprintf "Cannot read member '%s' from non-record non-class type" memberName
                    Code = ""
                    Severity = Error
                }

            TyVar(freshTyVar ctx)

    /// If `recv` is an *external generic type name* used as a static-access
    /// receiver (`EqualityComparer<int>` in `EqualityComparer<int>.Default`),
    /// return its metadata name (`` …EqualityComparer`1 ``) and the raw CST type
    /// args (translation deferred to the caller so the guard stays side-effect
    /// free — it only probes the provider). `ValueNone` for a value expression or
    /// an unknown type. v1 handles the `TypeApp` form only; non-generic external
    /// static access (`System.Console.Out`) is a follow-up.
    and private tryExternalTypeReceiver
        (ctx: PassContext)
        (recv: Expr<SyntaxToken>)
        : (string * Type<SyntaxToken> list) voption =
        // The receiver type name as written: a single-segment name parses as
        // `Expr.Ident` (`EqualityComparer<int>`), a dotted one as a `LongIdent`
        // (`System.Collections.Generic.EqualityComparer<int>`).
        let nameAndArgs =
            match recv with
            | Expr.TypeApp(expr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li); types = typeArgs) ->
                ValueSome(li.Idents |> Seq.map ctx.NameOf |> String.concat ".", typeArgs)
            | Expr.TypeApp(expr = Expr.Ident tok; types = typeArgs) -> ValueSome(ctx.NameOf tok, typeArgs)
            | _ -> ValueNone

        match nameAndArgs with
        | ValueNone -> ValueNone
        | ValueSome(qualName, typeArgs) ->
            let arity = typeArgs.Length
            // The arity-suffixed metadata name for a candidate (`EqualityComparer`1`).
            let metaNameOf (n: string) =
                if arity = 0 then n else sprintf "%s`%d" n arity

            let probe (n: string) =
                match ctx.Provider.TryLookupType(metaNameOf n) with
                | ValueSome(ExternalTypeShape.Class _) -> true
                | _ -> false

            // `tryQualify` applies the `open` prefixes, so a short
            // `EqualityComparer<int>` receiver resolves to its qualified metadata
            // name (symbol-resolution-handoff.md, open-resolution).
            match OpenScope.tryQualify ctx.Resolution.OpenScope probe qualName with
            | ValueSome resolved -> ValueSome(metaNameOf resolved, List.ofSeq typeArgs)
            | ValueNone -> ValueNone

    /// `System.Console.Out` / `Console.Out` (under `open System`): a multi-segment
    /// LongIdent whose prefix resolves as a *non-generic* external type and whose
    /// last segment is a static member. The non-generic analogue of the generic
    /// `EqualityComparer<int>.Default` DotLookup arm — there the `<int>` keeps the
    /// type receiver a separate `Expr.TypeApp`, but a non-generic type folds into a
    /// single LongIdent (the parser merges consecutive `.ident`), so the split is
    /// recovered here. Resolves the prefix through `OpenScope` like
    /// `tryExternalTypeReceiver`, then records the access
    /// (`inferExternalStaticMember`) so Freeze stamps a keyed `TExpr.ExternalMember`.
    /// Always static — an instance receiver is either a local binding (caught by
    /// the field-chain arm) or a `DotLookup`. A resolved prefix whose last segment
    /// is *not* an accessible static member (e.g. a const field, not modelled yet)
    /// falls through silently rather than diagnosing — it's valid F#, just
    /// unsupported (symbol-resolution-handoff.md: static fields are a later phase).
    and private tryExternalStaticLongIdent (ctx: PassContext) (key: NodeKey) (e: Expr<SyntaxToken>) : SemType voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length >= 2 ->
            let lastTok = li.Idents.[li.Idents.Length - 1]

            let prefixName =
                seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                |> String.concat "."

            let probe (n: string) =
                match ctx.Provider.TryLookupType n with
                | ValueSome(ExternalTypeShape.Class _) -> true
                | _ -> false

            match OpenScope.tryQualify ctx.Resolution.OpenScope probe prefixName with
            | ValueSome resolved ->
                // Claim it only if the member actually resolves; otherwise leave
                // the node to the ctor/TyVar fallback without a spurious error.
                match ctx.Provider.TryLookupMember(resolved, ctx.NameOf lastTok) with
                | ValueSome _ -> ValueSome(inferExternalStaticMember ctx key resolved [] lastTok)
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    // ---- Application-site overload resolution (type-args-bug.md Layer 2) ----
    //
    // .NET methods are an unresolved *method group* until applied; overloading is
    // resolved at the call with the argument types in hand (fsc
    // `ConstraintSolver.ResolveOverloadingCore`). This project resolves only the
    // **static, folded-LongIdent** method-call shape (`String.Concat("a", "b")`)
    // this way, and only when the name has **more than one** mapped overload — a
    // single candidate (the common case, incl. the `EqualityComparer.Equals`
    // equality fall-clause whose `DeclaredOnly` lookup is unique) keeps the eager
    // `DotLookup`/`tryExternalStaticLongIdent` single-pick, unchanged. Resolution
    // is: filter by **arity**, then by **applicability** (each arg assignable to
    // the param), then **betterness** (the unique most-specific parameter set).
    // The specificity test is **non-mutating** over (ground) zonked types — no
    // speculative unify/undo (type-args-bug.md "Hard stop").

    /// Structural `SemType` equality (ground types; primitive alias names compared
    /// verbatim — the overload sets we resolve don't hinge on `int`/`int32`).
    and private semTypeEq (a: SemType) (b: SemType) : bool =
        match zonk a, zonk b with
        | TyConst x, TyConst y -> x = y
        | TyVar x, TyVar y -> System.Object.ReferenceEquals(UnionFind.find x, UnionFind.find y)
        | TyFun(a1, r1), TyFun(a2, r2) -> semTypeEq a1 a2 && semTypeEq r1 r2
        | TyTuple xs, TyTuple ys -> xs.Length = ys.Length && List.forall2 semTypeEq xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) -> n1 = n2 && xs.Length = ys.Length && List.forall2 semTypeEq xs ys
        | _ -> false

    /// `System.Object` / `obj` — the universal supertype in our conservative
    /// subtype model (everything boxes to it; we model no other reference
    /// hierarchy, so a non-`object` param only matches an arg it equals).
    and private isObjectTy (t: SemType) : bool =
        match zonk t with
        | TyClass("System.Object", []) -> true
        | TyConst "obj" -> true
        | _ -> false

    /// An argument of type `argTy` is assignable to a parameter of type `paramTy`
    /// (conservative: exact match, or the param is `object`).
    and private argAssignable (argTy: SemType) (paramTy: SemType) : bool =
        semTypeEq argTy paramTy || isObjectTy paramTy

    /// `aTy` is at least as specific as `bTy` for betterness (equal, or `bTy` is
    /// the universal `object` and `aTy` is something more derived).
    and private asSpecificOrEq (aTy: SemType) (bTy: SemType) : bool = semTypeEq aTy bTy || isObjectTy bTy

    /// The declared parameter count of an external member (its key's `argSig`
    /// length — authoritative, distinguishes a flattened N-param method from a
    /// genuine single tuple param).
    and private memberParamCount (m: ExternalMember) : int =
        match m.Key with
        | SymbolKey.MemberKey(_, _, argSig) -> List.length argSig
        | _ -> 0

    /// The member's parameter types (instantiated at `typeArgs`), flattening the
    /// tupled signature back to N parameters (type-args-bug.md Layer 1/3).
    and private memberParamTypes (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
        let n = memberParamCount m

        match zonk (m.BuildSignature typeArgs) with
        | TyFun(TyTuple elems, _) when n >= 2 && List.length elems = n -> elems
        | TyFun(TyConst "unit", _) when n = 0 -> []
        | TyFun(p, _) -> [ p ]
        | _ -> []

    /// Pick the overload for a call of arg types `argElems`: arity, then
    /// applicability, then betterness. `ValueNone` = none applicable, or no unique
    /// best (ambiguous — the caller diagnoses).
    and private pickStaticOverload
        (typeArgs: SemType[])
        (candidates: ExternalMember[])
        (argElems: SemType list)
        : ExternalMember voption =
        let arity = List.length argElems

        let applicable =
            candidates
            |> Array.filter (fun m ->
                memberParamCount m = arity
                && (let ps = memberParamTypes typeArgs m
                    List.length ps = arity && List.forall2 argAssignable argElems ps)
            )

        match applicable with
        | [||] -> ValueNone
        | [| only |] -> ValueSome only
        | many ->
            let betterThan (a: ExternalMember) (b: ExternalMember) =
                let pa = memberParamTypes typeArgs a
                let pb = memberParamTypes typeArgs b

                List.forall2 asSpecificOrEq pa pb
                && List.exists2 (fun x y -> not (semTypeEq x y)) pa pb

            let best =
                many
                |> Array.filter (fun a ->
                    many
                    |> Array.forall (fun b -> System.Object.ReferenceEquals(a, b) || betterThan a b)
                )

            match best with
            | [| unique |] -> ValueSome unique
            | _ -> ValueNone

    /// Resolve a folded-LongIdent external *static* member reference
    /// (`System.String.Concat`) to its declaring type's metadata name + member
    /// token, when the prefix is an external `Class` declaring ≥1 such member.
    /// (The head being a local binding — a `r.X.Y` field chain — is excluded.)
    and private tryResolveExternalStaticMemberRef
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        : (string * SyntaxToken) voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length >= 2
            && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
            ->
            let lastTok = li.Idents.[li.Idents.Length - 1]

            let prefixName =
                seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                |> String.concat "."

            let probe (n: string) =
                match ctx.Provider.TryLookupType n with
                | ValueSome(ExternalTypeShape.Class _) -> true
                | _ -> false

            match OpenScope.tryQualify ctx.Resolution.OpenScope probe prefixName with
            | ValueSome resolved when (ctx.Provider.TryLookupMembers(resolved, ctx.NameOf lastTok)).Length > 0 ->
                ValueSome(resolved, lastTok)
            | _ -> ValueNone
        | _ -> ValueNone

    /// Application-site overload resolution for a static external method call
    /// (`String.Concat("a", "b")`). Fires only when the member name has >1 mapped
    /// overload (single-candidate access keeps the existing single-pick path, so
    /// behaviour is unchanged everywhere it already worked). Resolves the overload
    /// by the argument types, commits the chosen `SymbolKey` to `ExternalAccess`
    /// keyed on the member node (where Freeze reads it), and types the call.
    and private tryInferExternalStaticMethodCall
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match tryResolveExternalStaticMemberRef ctx fn with
        | ValueNone -> ValueNone
        | ValueSome(metaName, memberTok) ->
            let memberName = ctx.NameOf memberTok
            let candidates = ctx.Provider.TryLookupMembers(metaName, memberName)

            // A folded LongIdent names a non-generic type (generics need `<>`), so
            // the declaring type has no type arguments to instantiate.
            let typeArgs: SemType[] = [||]

            if candidates.Length <= 1 then
                // 0 / 1 candidate: defer to the eager single-pick path unchanged.
                ValueNone
            else
                let argTy = infer ctx argExpr

                let argElems =
                    match zonk argTy with
                    | TyTuple xs -> xs
                    | TyConst "unit" -> []
                    | single -> [ single ]

                match pickStaticOverload typeArgs candidates argElems with
                | ValueSome chosen ->
                    let fnKey = CstKeys.ofExpr fn

                    ctx.Resolution.ExternalAccess.Set(
                        fnKey,
                        {
                            Key = chosen.Key
                            IsStatic = chosen.IsStatic
                            IsProperty = chosen.IsProperty
                        }
                    )

                    let memberSig = chosen.BuildSignature typeArgs
                    (freshTv ctx fnKey).Link <- ValueSome memberSig
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key memberSig (TyFun(argTy, resultTy))
                    ValueSome resultTy
                | ValueNone ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message =
                                sprintf
                                    "No applicable (or no unique best) overload of '%s' on type '%s' for the given arguments"
                                    memberName
                                    metaName
                            Code = ""
                            Severity = Error
                        }

                    ValueSome(TyVar(freshTyVar ctx))

    /// Type a static member access on an external type via `TryLookupMember`,
    /// recording the resolved member (its interned `SymbolKey`) so Freeze stamps a
    /// `TExpr.ExternalMember` (symbol-resolution-plan §7.2). `typeArgs` instantiate
    /// the declaring type's typars, so `EqualityComparer<int>.Default` types as
    /// `EqualityComparer<int>`.
    and private inferExternalStaticMember
        (ctx: PassContext)
        (key: NodeKey)
        (metaName: string)
        (typeArgs: SemType list)
        (memberTok: SyntaxToken)
        : SemType =
        let memberName = ctx.NameOf memberTok

        match ctx.Provider.TryLookupMember(metaName, memberName) with
        | ValueSome m ->
            ctx.Resolution.ExternalAccess.Set(
                key,
                {
                    Key = m.Key
                    IsStatic = m.IsStatic
                    IsProperty = m.IsProperty
                }
            )

            m.BuildSignature(List.toArray typeArgs)
        | ValueNone ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Type '%s' has no accessible member '%s'" metaName memberName
                    Code = ""
                    Severity = Error
                }

            TyVar(freshTyVar ctx)

    and private inferFieldAccess
        (ctx: PassContext)
        (key: NodeKey)
        (receiver: Expr<SyntaxToken>)
        (fieldTok: SyntaxToken)
        : SemType =
        let fieldName = ctx.NameOf fieldTok
        let rTy = infer ctx receiver
        resolveFieldStep ctx key rTy fieldName

    /// `new T(args)`. Mirrors a single application against the value returned
    /// by `classCtorAsFunction` — kept inline so a bare `Expr.New` doesn't
    /// need to fabricate an `Expr.App` first.
    and private inferNew
        (ctx: PassContext)
        (key: NodeKey)
        (t: Type<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let receiverTy = translateType ctx t

        match resolveStep receiverTy with
        | TyClass(name, args) ->
            match ctx.Types.Class.TryGetValue name with
            | true, info ->
                let subst = mkNamedTypeSubst info.TypeParams args

                let paramTys =
                    info.CtorParams
                    |> Array.map (fun p -> substituteWith subst p.Type)
                    |> Array.toList

                let expected =
                    match paramTys with
                    | [] -> BuiltinTypes.tyUnit
                    | [ t ] -> t
                    | many -> TyTuple many

                let argTy = infer ctx argExpr
                unify ctx (CstKeys.ofExpr argExpr) argTy expected
                receiverTy
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown class type '%s'" name
                        Code = ""
                        Severity = Error
                    }

                infer ctx argExpr |> ignore
                TyVar(freshTyVar ctx)
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "'new' requires a class type"
                    Code = ""
                    Severity = Error
                }

            infer ctx argExpr |> ignore
            TyVar(freshTyVar ctx)

    /// Value-level inline IL `(# "op" args : retTy #)`. The instruction string
    /// and the operand types are opaque to the type-checker (the IL contract is
    /// the platform author's responsibility); we only type each operand so its
    /// own subtree is solved, and take the node's type from the declared result
    /// annotation. An IL op with no result annotation produces `unit`. This is
    /// the value-level analogue of the type-level intrinsic (`Type.ILIntrinsic`,
    /// which NameResolution records into `IntrinsicReprTypes`).
    and private inferILIntrinsic
        (ctx: PassContext)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (returnType: ReturnType<SyntaxToken> voption)
        : SemType =
        for a in args do
            infer ctx a |> ignore

        match returnType with
        | ValueSome(ReturnType(typ = t)) -> translateType ctx t
        | ValueNone -> BuiltinTypes.tyUnit

    /// `expr when ^T : Type [and ^U : Type]* = optimizedExpr` — one clause of an
    /// F# library-only static optimization. Type the default `baseE` (its type is
    /// the node's type — the operator's declared result, e.g. `bool` for the
    /// equality family, `^T` for `(+)`) and type this clause's `optimizedExpr` so
    /// its own subtree (operands, nested inline IL) is solved.
    ///
    /// The clause body is **NOT** cross-unified with the base. F#'s static-opt
    /// rule is per-clause — "assume the constraint, then check the body against the
    /// return type": under `when ^T : int` the body's `int` matches the (then-also
    /// -`int`) declared result `^T`. The earlier blanket `unify baseTy optTy` only
    /// happens to work when every clause shares one concrete type (the equality
    /// family's `bool`); it wrongly fuses the distinct clause results of an
    /// `^T`-returning op — `byte`/`int16`/`^T` for `(+)` — and fails to unify them.
    /// We omit that check (a fully sound version would speculatively unify under
    /// the assumed constraint and undo — out of scope, type-args-bug.md's
    /// no-speculative-unification stop); soundness rides on the clause being
    /// selected (and its body substituted) at expansion, where `^T` is concrete.
    ///
    /// The `when ^T : Type` constraints are a *compile-time dispatch*, NOT
    /// unification constraints, so the typar is **not** unified with its required
    /// type; it is translated only to record the verdict for `Inline.inlineExpand`
    /// to resolve at the call site. The typar resolves through `ctx.Resolution.TyparScope` —
    /// already seeded by the enclosing binding's parameters (`(x: ^T)`) — so the
    /// recorded `SemType` carries the binding's quantified root. See
    /// docs/operators-plan.md (the arithmetic/bitwise/unary task).
    and private inferLibraryOnlyStaticOptimization
        (ctx: PassContext)
        (key: NodeKey)
        (baseE: Expr<SyntaxToken>)
        (constraints: ImmutableArray<StaticOptimizationConstraint<SyntaxToken>>)
        (optimizedExpr: Expr<SyntaxToken>)
        : SemType =
        let baseTy = infer ctx baseE
        infer ctx optimizedExpr |> ignore

        let resolved =
            [
                for c in constraints do
                    match c with
                    | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(typar = tp; rhsType = rhs) ->
                        TStaticOptConstraint.TyconEquals(translateType ctx (Type.VarType tp), translateType ctx rhs)
                    | StaticOptimizationConstraint.WhenTyparIsStruct(typar = tp) ->
                        TStaticOptConstraint.IsStruct(translateType ctx (Type.VarType tp))
            ]

        ctx.StaticOpt.Set(key, resolved)
        baseTy

    /// `r.X.Y…` parsed as a single multi-segment `Expr.LongIdentOrOp`. The
    /// head segment was resolved by NameResolution as a local binding — type
    /// it through `ctx.Bindings.Binding`/`ctx.Bindings.Scheme`, then walk the remaining
    /// segments as a field-access chain.
    and private inferLongIdentFieldChain (ctx: PassContext) (key: NodeKey) (li: LongIdent<SyntaxToken>) : SemType =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent

        let headTy =
            match ctx.Bindings.Binding.TryGetValue headKey with
            | ValueSome rb ->
                match ctx.Bindings.Scheme.TryGetValue rb.BindingSite with
                | ValueSome scheme -> instantiate ctx scheme
                | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = headTy

        for i = 1 to li.Idents.Length - 1 do
            let seg = li.Idents.[i]
            let segName = ctx.NameOf seg
            // Diagnose against the LongIdent's overall key — there's no
            // separate sub-expression NodeKey for an intermediate segment.
            currTy <- resolveFieldStep ctx key currTy segName

        currTy

    and private inferString
        (ctx: PassContext)
        (_key: NodeKey)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : SemType =
        // Interpolated strings are `string` too — Freeze lowers them to a
        // `TExpr.Format` (D9). Recurse into every hole expr so its type is
        // computed (Freeze reads it back to emit `AppendFormatted<T>`); a
        // `%d{x}` specifier additionally constrains the hole.
        for part in parts do
            match part with
            | StringPart.Expr(formatSpecifier = fs; expr = e) ->
                let holeTy = infer ctx e

                match fs with
                | ValueSome ft ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
                    | ValueSome p ->
                        match PrintfSpec.argType (fun () -> TyVar(freshTyVar ctx)) p.Type with
                        | ValueSome t -> unify ctx (CstKeys.ofExpr e) holeTy t
                        | ValueNone -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
            | _ -> ()

        BuiltinTypes.tyString

    and private inferTypeAnnotation
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let innerTy = infer ctx inner
        let annTy = translateType ctx t
        unify ctx key innerTy annTy
        annTy

    and private inferLet
        (ctx: PassContext)
        (key: NodeKey)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : SemType =
        inferBindingGroup ctx bindings
        infer ctx (CstWalk.requireLetBody body)

    and inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        // One typar scope per binding signature: explicit `<'a>` typars seed
        // it first so later implicit `'a` mentions share the same TyVar.
        let savedScope = ctx.Resolution.TyparScope
        ctx.Resolution.TyparScope <- Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

        match b.typarDefns with
        | ValueSome(TyparDefns(defns = ds; constraints = bindingConstraints)) ->
            for TyparDefn(typar = t) in ds do
                match t with
                | Typar.Named(ident = id)
                | Typar.Static(ident = id) ->
                    let n = ctx.NameOf id

                    if not (ctx.Resolution.TyparScope.ContainsKey n) then
                        let tv = TypeVar()
                        tv.Level <- ctx.CurrentLevel
                        ctx.Resolution.TyparScope.[n] <- tv
                | Typar.Anon _ -> ()

            match bindingConstraints with
            | ValueSome cs -> translateConstraints ctx cs
            | ValueNone -> ()
        | ValueNone -> ()

        try
            let patTy = inferPat ctx b.headPat

            let rhsTy =
                if b.argumentPats.IsEmpty then
                    let bodyTy = infer ctx b.expr

                    match b.returnType with
                    | ValueSome(ReturnType(typ = t)) ->
                        let annTy = translateType ctx t
                        unify ctx (CstKeys.ofBinding b) bodyTy annTy
                        annTy
                    | ValueNone -> bodyTy
                else
                    // `let f x y = body` is `let f = fun x y -> body`.
                    let argTypes = [ for p in b.argumentPats -> inferPat ctx p ]
                    let bodyTy = infer ctx b.expr

                    let bodyTy =
                        match b.returnType with
                        | ValueSome(ReturnType(typ = t)) ->
                            let annTy = translateType ctx t
                            unify ctx (CstKeys.ofBinding b) bodyTy annTy
                            annTy
                        | ValueNone -> bodyTy

                    List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

            unify ctx (CstKeys.ofBinding b) patTy rhsTy
        finally
            ctx.Resolution.TyparScope <- savedScope

    /// Type a `let` / `let rec` group with Rémy-level discipline. Key
    /// subtlety: pre-allocate single-name sibling headPat TyVars (step 2) so
    /// forward references from inside one RHS (or a nested let) find the
    /// sibling's TyVar at this group's level rather than lazy-minting at a
    /// deeper one — which would let a nested let generalise a var that
    /// actually belongs to an un-typed outer sibling. RHSes type at the
    /// pushed level (sibling lookups stay monomorphic — no scheme written
    /// yet); generalisation happens against the outer level after popping.
    and inferBindingGroup (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        let outerLevel = ctx.CurrentLevel
        enterLevel ctx

        for b in bindings do
            match b.headPat with
            | Pat.NamedSimple _
            | Pat.Op _ -> tvOf ctx (CstKeys.ofPat b.headPat) |> ignore
            | _ -> ()

        for b in bindings do
            inferBinding ctx b

        exitLevel ctx

        for b in bindings do
            if shouldGeneralise b then
                let key = CstKeys.ofPat b.headPat
                let headTv = tvOf ctx key
                let zonked = zonk (TyVar headTv)

                if not (hasPendingDotAccess zonked) then
                    // Settle flexible list-literal containers first (R3), then
                    // re-zonk so the (now-linked) FSharpList element generalises.
                    prepareListLiterals ctx zonked outerLevel
                    let scheme = generalise (zonk zonked) outerLevel
                    ctx.Bindings.Scheme.Set(key, scheme)
