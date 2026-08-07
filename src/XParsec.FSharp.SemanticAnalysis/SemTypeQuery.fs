namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// Whole-`SemType` questions asked THROUGH union-find. Each step zonks: pre-freeze a
// structural head is often reachable only through a Link, and a raw match would see a `TyVar`.

module SemTypeQuery =

    /// `TyFun`-chain views: which domains a curried function type has, and what it returns
    /// after `n` of them are applied. A chain shorter than `n` is not an error — callers cap
    /// `n` at a count they measured — so a short chain yields what it has instead of failing.
    [<RequireQualifiedAccess>]
    module internal Funs =

        /// The number of `->` in the chain.
        let rec count (store: TypeStore) (t: SemType) : int =
            match UnificationEngineCore.zonk store t with
            | TyFun(_, r) -> 1 + count store r
            | _ -> 0

        /// The first `n` domain types, left to right.
        let rec domains (store: TypeStore) (n: int) (t: SemType) : SemType list =
            if n <= 0 then
                []
            else
                match UnificationEngineCore.zonk store t with
                | TyFun(a, b) -> a :: domains store (n - 1) b
                | _ -> []

        /// What the chain returns once `n` arguments have been applied.
        let rec resultAfter (store: TypeStore) (n: int) (t: SemType) : SemType =
            let t = UnificationEngineCore.zonk store t

            if n <= 0 then
                t
            else
                match t with
                | TyFun(_, b) -> resultAfter store (n - 1) b
                | _ -> t

    /// A (zonked) `SemType` with no free `TyVar` anywhere — fully monomorphic.
    let rec internal isGround (store: TypeStore) (t: SemType) : bool =
        match UnificationEngineCore.zonk store t with
        | TyVar _
        | TyUnknown _
        | TyTypar _ -> false
        | t -> SemType.forallChildren (isGround store) t
