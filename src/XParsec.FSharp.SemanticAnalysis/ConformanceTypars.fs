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
//    the sole ABI surface, and `Freeze` drops inline templates, so they never reach
//    `checkFile`. 4.2 would exempt them even if the body matched the contract exactly.
//    (Aside, lest the current state mislead: `ops-platform.fs` implements `(+)` as the
//    homogeneous `^T -> ^T -> ^T`, while the contract — and real FSharp.Core's body,
//    `prim-types.fs` `let inline (+) (x:^T) (y:^U) : ^V` — is the general
//    `^T1 -> ^T2 -> ^T3`. That one-typar body is a known SIMPLIFICATION, not an intended
//    invariant; it is orthogonal to this check either way.)
//  - A MONOMORPHIC binding has no typar order to disagree on, so it is skipped.
//  - A binding the contract does not publish (a private HiddenVal) is skipped —
//    PRESENCE is Step 4.1's job, not this pass's.
//  - Type MEMBERS (overloaded `AppendFormatted`, ctors) are Step 6.

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
    let rec normAxis (t: FrozenType) : FrozenType =
        match t with
        | FTTypar(_, i) -> FTTypar(TyparAxis.Method, i)
        | FTConst(n, args) -> FTConst(n, EqArray.map normAxis args)
        | FTFun(a, r) -> FTFun(normAxis a, normAxis r)
        | FTTuple items -> FTTuple(EqArray.map normAxis items)
        | FTRecord(k, args) -> FTRecord(k, EqArray.map normAxis args)
        | FTUnion(k, args) -> FTUnion(k, EqArray.map normAxis args)
        | FTClass(k, args) -> FTClass(k, EqArray.map normAxis args)
        | FTOr members -> FTOr(EqArray.map normAxis members)
        | FTUnknown _ -> t

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
    let private lookupNames (info: ModuleMemberInfo option) (name: string) : string list =
        match info with
        | Some mi -> [ mi.Holder + "." + mi.Name; mi.Name ] |> List.distinct
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
                // `Freeze` drops inline templates, so a surviving `Let` is never inline;
                // matching `false` documents the scope and is robust to that changing.
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
