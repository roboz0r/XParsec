namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

module InlineThaw =

    [<RequireQualifiedAccess>]
    type private TyparLeaf =
        | Declaring of declIndex: int
        | Method of methodIndex: int
        | Local of scheme: SchemeId * localIndex: int

    /// One cache for the WHOLE decl: two occurrences of one typar must land on ONE cell, or a
    /// parameter's type and the uses of that parameter come apart. Tokens stay the PRODUCER's,
    /// read out of `origin`'s retained text, so a node still spells where it was written.
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
