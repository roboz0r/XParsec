namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// The immutable→mutable transition on the provider seam: realise a cross-unit inline body,
// which arrives as cell-free `Wire.TDecl`, in the CONSUMER's `SemType` domain.
//
// The provider hands out `FrozenType`, so nothing a consumer does can reach back into a
// producer's inference state; the cells a splice then unifies against are minted HERE, out of
// leaves that name nothing but positions in the template. After this, `substType` / `freshen` /
// SRTP resolution run unchanged — they key on `TyVar` roots, and the roots now exist and are
// this unit's.

module InlineThaw =

    /// A typar leaf of a FROZEN template, across all three axes — the key of the thaw's
    /// freshener cache. One cache, one key type: a `Declaring 0` and a `Method 0` are different
    /// typars and must not collide, and an `FTLocalTypar` is identified by the
    /// `(scheme, index)` PAIR, never the index alone.
    [<RequireQualifiedAccess>]
    type private TyparLeaf =
        | Declaring of declIndex: int
        | Method of methodIndex: int
        | Local of scheme: SchemeId * localIndex: int

    /// ONE cache for the WHOLE decl, shared across all three axes — not one per node. Two
    /// occurrences of one typar must land on ONE cell, or the body's internal type links (a
    /// parameter's type and the use of that parameter) come apart.
    ///
    /// It consults no ambient unit state: a leaf is interpreted against the body carrying it and
    /// nothing else. That is what makes an `FTLocalTypar`'s body-relative `SchemeId` safe across
    /// units — it addresses nothing outside the body it arrived with.
    ///
    /// Realised WHERE IT WAS WRITTEN: every node keeps the producer's own token, read out of that
    /// file's retained `Lexed`. There is no second reading — a `Wire.TDecl` carries the
    /// producer's real token indices, and a thaw that moved them onto a consuming position would
    /// be throwing away the only thing that tells a node of the body from a node of the file it
    /// is consumed in.
    ///
    /// `origin` is checked against the retained source's content hash on every node, so a
    /// producer edited since the body was built faults here rather than silently re-attributing
    /// the whole body to whatever now sits at those indices.
    let bodyAtOrigin (store: TypeStore) (sources: OriginSources) (origin: OriginFile) (decl: Wire.TDecl) : TDecl =
        let cache = Dictionary<TyparLeaf, SemType>()

        let mint (leaf: TyparLeaf) : SemType =
            match cache.TryGetValue leaf with
            | true, v -> v
            | _ ->
                let v = TyVar(store.NewTypeVar())
                cache.[leaf] <- v
                v

        TastConvert.decl
            (FrozenTypeBridge.instantiateWith
                (fun i -> mint (TyparLeaf.Declaring i))
                (fun j -> mint (TyparLeaf.Method j))
                (fun scheme k -> mint (TyparLeaf.Local(scheme, k))))
            (OriginSources.tokenAt sources origin)
            decl
