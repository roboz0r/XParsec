namespace XParsec.FSharp.SemanticAnalysis

// T8 Step 4.2 — SEMANTIC typar-count + α-equivalence + ORDER conformance.
//
// `Conformance.fs` / `ConformancePass.fs` are the CST-level halves (presence +
// extern/intrinsic pairing). They CANNOT check typar ORDER: a `.fsi` and its `.fs`
// spell types differently in entirely legal ways (`'a list` vs `List<'a>`, `seq<'a>`
// vs `IEnumerable<'a>`, transparent abbreviations), so a syntactic comparison flags
// false drift; and the impl's INFERRED typar order is invisible without running
// inference (the `.fs` rarely writes an explicit `<'a,'b>`). This module is the
// semantic half.
//
// The faithful check (per the T8 plan "Decision"): F# CHECKS the `.fsi` typar order
// against the `.fs` and then PUBLISHES the `.fsi`'s order, so drift is impossible by
// construction. Here both sides are already quantified in their OWN canonical order —
// the `.fsi` by `VesperLib.translateCurriedSig` (args-first appearance), the `.fs` by
// `GeneralizedTypars.canonical` (declared-first, then appearance) — and a `FrozenType`
// carries each open typar as a POSITIONAL `FTTypar(axis, index)`. So a structural
// `FrozenType` equality (after collapsing the two sides' single typar axis to one) IS
// α-equivalence-WITH-ORDER: it holds exactly when the two sides number their typars in
// the same order. A `.fs` that declares `<'b,'a>` against a `.fsi` whose appearance
// order is `'a,'b` produces a different positional skeleton and is caught.
//
// SCOPE: NON-inline generic MODULE functions only.
//  - `let inline` operators are EXEMPT — and NOT because their `.fsi`/`.fs` typars
//    happen to differ today. An inline body is expanded and SRTP-solved at each call
//    site; it is NEVER emitted as a fixed-arity generic method, so it has no
//    emitted/extracted typar order for the contract's order to drive. The signature is
//    the sole ABI surface, and `Elaborate` drops inline templates, so they never reach
//    `checkFile`. 4.2 would exempt them even if the body matched the contract exactly.
//    (Aside, lest the current state mislead: `ops-platform.fs` implements `(+)` as the
//    homogeneous `^T -> ^T -> ^T`, while the contract — and real FSharp.Core's body,
//    `prim-types.fs` `let inline (+) (x:^T) (y:^U) : ^V` — is the general
//    `^T1 -> ^T2 -> ^T3`. That one-typar body is a known SIMPLIFICATION, not an intended
//    invariant; it is orthogonal to this check either way.)
//  - A MONOMORPHIC binding has no typar order to disagree on, so it is skipped.
//  - A binding the contract does not publish (a private HiddenVal) is skipped —
//    PRESENCE is Step 4.1's job, not this pass's.
//
// Step 6 extends the SAME faithfulness invariant to generic type MEMBERS
// (`Formatter.AppendFormatted: 'T -> unit`, the overloaded set) via `checkMembers`
// below — now that `.fsi` extraction publishes a member's own method typars
// (`reaxisMethodTypars`, `MethodTyparArity > 0`) rather than dropping the member. A
// member carries TWO typar axes (the declaring type's + its own), so — unlike the
// single-axis module binding — its check compares the two frozen signatures
// DIRECTLY (no axis collapse): both sides already write the declaring type's typars
// on `FTTypar(Declaring,i)` and the method's own on `FTTypar(Method,j)`, each in
// canonical order, so a structural equality IS α-equivalence-with-order across both
// axes. Collapsing them (as `normAxis` does for a free value) would conflate a
// declaring slot with a method slot.

module ConformanceTypars =

    /// A binding whose `.fs`-inferred generic scheme disagrees with its
    /// `.fsi`-declared one — a different typar COUNT or a different typar ORDER (one
    /// structural inequality once both axes are normalized). `Declared` / `Inferred`
    /// are the axis-normalized schemes, for a diagnostic that shows the two skeletons.
    [<NoEquality; NoComparison>]
    type TyparMismatch =
        {
            /// The provider name the binding resolved under (e.g. `ListModule.fold`).
            Name: string
            /// The `.fsi`-declared scheme, axis-normalized.
            Declared: FrozenType
            /// The `.fs`-inferred scheme, axis-normalized.
            Inferred: FrozenType
        }

    /// Collapse a frozen scheme's typar axis onto a single axis (`Method`). A free
    /// value / function has exactly ONE typar axis — it has no enclosing generic type —
    /// so the `.fsi` side's `FTTypar(Declaring, i)` and the `.fs` module binding's
    /// `FTTypar(Method, i)` denote the same single axis. Normalizing both lets a
    /// structural equality line the positions up. (A module-level binding can never
    /// reference a real declaring-type typar, so this conflates nothing.)
    let rec normAxisTo (axis: TyparAxis) (t: FrozenType) : FrozenType =
        match t with
        | FTTypar(_, i) -> FTTypar(axis, i)
        | t -> FrozenType.mapChildren (normAxisTo axis) t

    let normAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Method t

    /// Land a FREE value/function's single typar axis on `Declaring` — the axis
    /// `ExternalSymbol.Scheme` / `instantiateDeclaring` require. The frozen→provider
    /// projection remaps a module binding's `FTTypar(Method, i)` here: a module
    /// binding has no enclosing generic type, so every typar it carries IS that one
    /// axis, and `Declaring` is the convention every provider scheme speaks.
    let toDeclaringAxis (t: FrozenType) : FrozenType = normAxisTo TyparAxis.Declaring t

    /// True iff the `.fsi`-declared and `.fs`-inferred schemes are α-equivalent WITH
    /// typar order. Because `FTTypar` is positional (its index IS the quantification
    /// order) and `FrozenType` has value-based structural equality, axis-normalized
    /// structural equality fails EXACTLY when the two sides order their typars
    /// differently — the T8 G2 faithfulness invariant. Abbreviations are erased on both
    /// sides by `freeze`, so the legal `seq`/`IEnumerable` spelling differences do not
    /// surface here.
    let schemesAgree (declared: FrozenType) (inferred: FrozenType) : bool = normAxis declared = normAxis inferred

    /// The provider lookup names to try for a frozen module binding, most-specific
    /// first. A contract provider registers a module function under its compiled name
    /// (`ListModule.fold` = `Holder.Name`) AND its source-name alias (`List.fold`), so
    /// `Holder.Name` resolves a `[<RequireQualifiedAccess>]` / `ModuleSuffix` module's
    /// members; the bare `Name` covers a binding compiled with no holder (a top-level
    /// value in a named module).
    let private lookupNames (info: ModuleBindingInfo option) (name: string) : string list =
        match info with
        | Some mi -> [ mi.HolderName + "." + mi.Name; mi.Name ] |> List.distinct
        | None -> [ name ]

    /// Check every NON-inline generic module binding of a frozen `.fs` file against the
    /// `.fsi` contract `provider`. A binding the provider does not publish (a private
    /// HiddenVal) or that is monomorphic (`TyparArity = 0`) is skipped — it has no typar
    /// order to compare. Returns one `TyparMismatch` per generic binding whose inferred
    /// scheme disagrees with its declared one, in source-declaration order.
    let checkFile (provider: IExternalSymbolProvider) (tast: Frozen.TastFile) : TyparMismatch list =
        [
            for decl in tast.Decls do
                match decl with
                // `Freeze` partitions inline templates out of `Decls` (they are vocabulary,
                // not code), so a surviving `Let` is never inline; matching `false`
                // documents the scope and is robust to that changing.
                | Frozen.TDecl.Let(Frozen.TPat.NamedSimple(key, _, _), _, false, ty) ->
                    let info = Map.tryFind key tast.ModuleMembers

                    let nameOpt =
                        match info with
                        | Some mi -> Some mi.Name
                        | None -> Map.tryFind key tast.TopLevelNames

                    match nameOpt with
                    | None -> ()
                    | Some name ->
                        let resolved =
                            lookupNames info name
                            |> List.tryPick (fun n ->
                                match provider.TryLookup n with
                                | ValueSome s -> Some(n, s)
                                | ValueNone -> None
                            )

                        match resolved with
                        // Only a generic contract binding has a typar order to disagree
                        // on; an unpublished or monomorphic binding is not this pass's.
                        | Some(n, sym) when sym.TyparArity > 0 ->
                            if not (schemesAgree sym.Scheme ty) then
                                yield
                                    {
                                        Name = n
                                        Declared = normAxis sym.Scheme
                                        Inferred = normAxis ty
                                    }
                        | _ -> ()
                | _ -> ()
        ]

    /// A generic type MEMBER whose `.fs`-inferred signature disagrees with every
    /// published `.fsi` overload of matching method arity — a different method-typar
    /// ORDER or signature SHAPE (one structural inequality; the two axes line up by
    /// construction). `Published` carries the candidate overloads' signatures for a
    /// diagnostic that shows what the member could have matched.
    [<NoEquality; NoComparison>]
    type MemberMismatch =
        {
            /// The declaring type's compiled name (`Vesper.Formatter`).
            TypeName: string
            /// The member name (`AppendFormatted`).
            MemberName: string
            /// The member's own method-typar count (`> 0` — this pass only checks
            /// generic members).
            MethodTyparArity: int
            /// The `.fs`-inferred member signature (`params → return`, or the bare
            /// value type for a property).
            Inferred: FrozenType
            /// The matching-arity published overloads' signatures.
            Published: FrozenType list
        }

    /// The `.NET`-tupled `Parameters` form of a frozen member's parameter binders —
    /// the `FrozenType` shape `ExternalSignature.Parameters` carries (0 ⇒ `unit`,
    /// 1 ⇒ itself, N ⇒ a tuple), so the two sides compare directly.
    let private tupledParams (ps: EqArray<NodeKey * FrozenType>) : FrozenType =
        match ps.Length with
        | 0 -> FTConst(RuntimeNames.unitKey, EqArray.empty)
        | 1 -> snd ps.[0]
        | _ -> FTTuple(EqArray.ofSeq (seq { for kv in ps -> snd kv }))

    /// One member's signature as a single `FrozenType` — `params → return` for a
    /// method, the bare value type for a property — the shape both the `.fs` member
    /// (`tupledParams`/`ReturnTy`) and the `.fsi` overload
    /// (`Signature.Parameters`/`Return`) fold to, so a `=` is the conformance check.
    let private memberSigOf (isProperty: bool) (parameters: FrozenType) (ret: FrozenType) : FrozenType =
        if isProperty then ret else FTFun(parameters, ret)

    let private extractedSigOf (m: ExternalMember) : FrozenType =
        memberSigOf m.IsValueMember m.Signature.Parameters m.Signature.Return

    /// The generic members of a frozen type declaration's body, paired with their
    /// property-ness. Augmentation members ride `Class`/`Union`/`Record` (the shared
    /// `TTypeKindG.members`); an `Interface`'s abstract methods carry a different
    /// (`TAbstractMethodG`) shape and are not checked here (no concrete `.fs` impl pairs
    /// with them in the same file), and an enum is niladic — both yield no members.
    let private bodyMembers (kind: Frozen.TTypeKind) : Frozen.TTypeMember list =
        EqArray.toList (TTypeKindG.members kind)

    /// Check every generic (method-owned-typar) MEMBER of a frozen `.fs` file against
    /// its `.fsi` contract `provider`. For each such member, the published overloads
    /// of the same name + method arity are the candidate set; conformance holds when
    /// one of them is structurally equal to the inferred signature (the two-axis
    /// faithfulness invariant). A member with NO matching-arity published overload is
    /// skipped — that is member PRESENCE, not this pass's typar-order remit. Returns
    /// one `MemberMismatch` per generic member whose `.fs` signature matches no
    /// published overload of its arity, in source-declaration order.
    let checkMembers (provider: IExternalSymbolProvider) (tast: Frozen.TastFile) : MemberMismatch list =
        [
            for decl in tast.Decls do
                match decl with
                | Frozen.TDecl.Type td ->
                    let typeName = SymbolKeyOps.qualifiedName td.Key

                    for m in bodyMembers td.Kind do
                        let arity = m.MethodTypeParams.Length

                        if arity > 0 then
                            let isProperty = (m.Kind = TMemberKind.Property)
                            let inferred = memberSigOf isProperty (tupledParams m.Params) m.ReturnTy
                            let overloads = provider.TryLookupMembers(td.Key, m.Name)
                            // Candidate set: overloads with the SAME method arity. A
                            // different-arity overload is a different generic member.
                            let candidates = overloads |> Array.filter (fun em -> em.MethodTyparArity = arity)

                            // Skip when the contract publishes no matching-arity overload
                            // at all — that is PRESENCE (Step 4.1 / `ConformancePass`),
                            // not a typar-order disagreement.
                            if
                                candidates.Length > 0
                                && not (candidates |> Array.exists (fun em -> extractedSigOf em = inferred))
                            then
                                yield
                                    {
                                        TypeName = typeName
                                        MemberName = m.Name
                                        MethodTyparArity = arity
                                        Inferred = inferred
                                        Published = [ for em in candidates -> extractedSigOf em ]
                                    }
                | _ -> ()
        ]
