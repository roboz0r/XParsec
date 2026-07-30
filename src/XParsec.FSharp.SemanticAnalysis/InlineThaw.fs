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
    /// `readAt` decides the body's POSITIONS, and that is the caller's choice rather than a fact
    /// about the wire: a `Wire.TDecl` carries the producer's real token indices, which index the
    /// producer's `Lexed` and not this unit's. Private, so a caller cannot invent a third
    /// reading beyond the two exported below.
    let private thawWith (store: TypeStore) (readAt: ForeignAnchor -> SyntaxToken) (decl: Wire.TDecl) : TDecl =
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
            readAt
            decl

    /// Realise a wire body AT A CALL SITE: every node takes `at`, the position an inlined body
    /// means once it has been physically spliced. Taking the site as an ARGUMENT is what makes
    /// "every node of a file anchors in that file" hold by construction for a splice — there is
    /// no way to get a spliceable `TExpr` out of the wire without saying where it lands.
    ///
    /// The cost is that the producer's own positions are gone, and with them any way to tell a
    /// node written in the consuming file from one that came from the body. That is what
    /// `bodyAtOrigin` keeps.
    let body (store: TypeStore) (at: SyntaxToken) (decl: Wire.TDecl) : TDecl =
        thawWith store (fun (_: ForeignAnchor) -> at) decl

    /// Realise a wire body WHERE IT WAS WRITTEN: every node keeps the producer's own token, read
    /// out of that file's retained `Lexed`.
    ///
    /// For a body that is NOT physically spliced — one that stays a declaration of its own,
    /// reached by an edge — so its nodes are never mixed into the consuming file's tree and its
    /// indices never have to mean anything against this unit's tokens. `origin` is checked
    /// against the retained source's content hash on every node, so a producer edited since the
    /// body was built faults here rather than silently re-attributing the whole body to whatever
    /// now sits at those indices.
    let bodyAtOrigin (store: TypeStore) (sources: OriginSources) (origin: OriginFile) (decl: Wire.TDecl) : TDecl =
        thawWith store (OriginSources.tokenAt sources origin) decl
