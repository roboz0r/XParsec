namespace XParsec.FSharp.SemanticAnalysis

/// Structural operations on `SemType` that need `SymbolKeyOps` (nominal-key
/// ordering) but must precede the unifier. Currently just the anonymous-union
/// canonicaliser (`mkUnion`) and the total order it sorts members by.
[<AutoOpen>]
module SemTypeOps =

    /// A total structural order over `SemType`, used **only** to canonicalise
    /// anonymous-union (`TyOr`) members so `string | int` and `int | string`
    /// freeze and unify to the same value. Deterministic by structure — nominal
    /// types ordered by home assembly + qualified name — and pointedly NOT by
    /// `TyVar` identity: v1 unions are annotation-driven over ground members, so a
    /// `TyVar` never reaches here; were one to (v2 union inference) it orders equal
    /// to every other var, keeping the sort total without inventing a
    /// non-deterministic identity order. Distinctness is the dedup's job
    /// (structural `=`), not the comparator's, so two members that order equal are
    /// still both kept.
    let rec compareSemType (a: SemType) (b: SemType) : int =
        let tag t =
            match t with
            | TyVar _ -> 0
            | TyConst _ -> 1
            | TyFun _ -> 2
            | TyTuple _ -> 3
            | TyRecord _ -> 4
            | TyUnion _ -> 5
            | TyClass _ -> 6
            | TyOr _ -> 7
            | TyUnknown _ -> 8
            | TyTypar _ -> 9

        let axisTag =
            function
            | TyparAxis.Declaring -> 0
            | TyparAxis.Method -> 1

        let cmpMany (xs: EqArray<SemType>) (ys: EqArray<SemType>) =
            let mutable r = 0
            let mutable i = 0
            let n = min xs.Length ys.Length

            while r = 0 && i < n do
                r <- compareSemType xs.[i] ys.[i]
                i <- i + 1

            if r <> 0 then r else compare xs.Length ys.Length

        let nominalKey k =
            struct (SymbolKeyOps.keyAsm k, SymbolKeyOps.qualifiedName k)

        match a, b with
        | TyVar _, TyVar _ -> 0
        | TyConst(n1, a1), TyConst(n2, a2) ->
            let r = compare n1 n2
            if r <> 0 then r else cmpMany a1 a2
        | TyFun(a1, r1), TyFun(a2, r2) ->
            let r = compareSemType a1 a2
            if r <> 0 then r else compareSemType r1 r2
        | TyTuple xs, TyTuple ys -> cmpMany xs ys
        | TyRecord(k1, a1), TyRecord(k2, a2)
        | TyUnion(k1, a1), TyUnion(k2, a2)
        | TyClass(k1, a1), TyClass(k2, a2) ->
            let r = compare (nominalKey k1) (nominalKey k2)
            if r <> 0 then r else cmpMany a1 a2
        | TyOr m1, TyOr m2 -> cmpMany m1 m2
        | TyUnknown n1, TyUnknown n2 -> compare n1 n2
        | TyTypar(ax1, i1), TyTypar(ax2, i2) ->
            let r = compare (axisTag ax1) (axisTag ax2)
            if r <> 0 then r else compare i1 i2
        // Different cases — order by case tag.
        | _ -> compare (tag a) (tag b)

    /// The smart constructor for anonymous (structural) unions — the ONLY
    /// sanctioned producer of `TyOr`. Enforces the canonical form the equality
    /// layer's `n1 = n2` discipline relies on:
    ///   * flatten  `(A | B) | C ≡ A | B | C`
    ///   * dedup    `A | A ≡ A`        (structural `=`)
    ///   * collapse `TyOr [A] ≡ A`     (a one-member union is just that member)
    ///   * sort     members by `compareSemType` so `string | int ≡ int | string`
    /// `mkUnion []` is `TyOr []` = `never` (bottom). Members are not resolved /
    /// zonked here — canonicalisation is purely structural; a member that is itself
    /// a still-free `TyVar` is a v2 concern (annotation-driven v1 members are
    /// ground).
    let mkUnion (members: SemType seq) : SemType =
        let acc = ResizeArray<SemType>()

        let rec add (t: SemType) =
            match t with
            | TyOr ms -> EqArray.iter add ms
            | _ ->
                if not (acc.Contains t) then
                    acc.Add t

        for m in members do
            add m

        acc.Sort(System.Comparison<SemType>(compareSemType))

        if acc.Count = 1 then
            acc.[0]
        else
            TyOr(EqArray.ofResizeArray acc)
