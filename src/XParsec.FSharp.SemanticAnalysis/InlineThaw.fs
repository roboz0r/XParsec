namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

module InlineThaw =

    [<RequireQualifiedAccess>]
    type private TyparKey =
        | Declaring of declIndex: int
        | Method of methodIndex: int
        | Local of scheme: SchemeId * localIndex: int

    /// One cache for the WHOLE decl: two occurrences of one typar must land on ONE cell, or a
    /// parameter's type and the uses of that parameter come apart. Tokens stay the DECLARING file's,
    /// read out of `path`'s retained text, so a node still spells where it was written.
    let bodyAtPath (store: TypeStore) (retained: LexedFiles) (path: AssemblyFilePath) (decl: Wire.TDecl) : TDecl =
        let cache = Dictionary<TyparKey, SemType>()

        let mint (key: TyparKey) : SemType =
            match cache.TryGetValue key with
            | true, v -> v
            | _ ->
                let v = TyVar(store.NewTypeVar())
                cache.[key] <- v
                v

        TastConvert.decl
            (FrozenTypeBridge.instantiateWith
                (fun i -> mint (TyparKey.Declaring i))
                (fun j -> mint (TyparKey.Method j))
                (fun scheme k -> mint (TyparKey.Local(scheme, k))))
            (LexedFiles.tokenAt retained path)
            decl
