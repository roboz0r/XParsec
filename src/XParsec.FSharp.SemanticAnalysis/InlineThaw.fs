namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

module InlineThaw =

    [<RequireQualifiedAccess>]
    type private TyparKey =
        | Declaring of declIndex: int
        | Method of methodIndex: int
        | Local of scheme: SchemeId * localIndex: int

    /// A splice template in the consuming file's own inference cells.
    [<NoEquality; NoComparison>]
    type ThawedTemplate =
        {
            Decl: TDecl
            /// The fresh root each quantified typar thawed to, at the position a type argument
            /// for it is supplied: declaring axis first, then method axis, each in index order.
            /// A body-local scheme quantifies its own typars below the template, so it contributes
            /// none.
            Typars: TyVarId[]
        }

    /// One cache for the WHOLE decl: two occurrences of one typar must land on ONE cell, or a
    /// parameter's type and the uses of that parameter come apart. Tokens stay the DECLARING file's,
    /// read out of `path`'s retained text, so a node still spells where it was written.
    let bodyAtPath
        (store: TypeStore)
        (retained: LexedFiles)
        (path: AssemblyFilePath)
        (decl: Wire.TDecl)
        : ThawedTemplate =
        let cache = Dictionary<TyparKey, TyVarId>()

        let mint (key: TyparKey) : SemType =
            match cache.TryGetValue key with
            | true, v -> TyVar v
            | _ ->
                let v = store.NewTypeVar()
                cache.[key] <- v
                TyVar v

        let thawed =
            TastConvert.decl
                (FrozenTypeBridge.instantiateWith
                    (fun i -> mint (TyparKey.Declaring i))
                    (fun j -> mint (TyparKey.Method j))
                    (fun scheme k -> mint (TyparKey.Local(scheme, k))))
                (LexedFiles.tokenAt retained path)
                decl

        let rank (key: TyparKey) : (int * int) voption =
            match key with
            | TyparKey.Declaring i -> ValueSome(0, i)
            | TyparKey.Method j -> ValueSome(1, j)
            | TyparKey.Local _ -> ValueNone

        let typars =
            cache
            |> Seq.choose (fun (KeyValue(key, tv)) ->
                match rank key with
                | ValueSome r -> Some(r, tv)
                | ValueNone -> None
            )
            |> Seq.sortBy fst
            |> Seq.map snd
            |> Array.ofSeq

        { Decl = thawed; Typars = typars }
