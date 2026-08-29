namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

module InlineThaw =

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

    /// Tokens stay the DECLARING file's, read out of `path`'s retained text, so a node still
    /// spells where it was written.
    let bodyAtPath
        (store: TypeStore)
        (retained: LexedFiles)
        (path: AssemblyFilePath)
        (decl: Wire.TDecl)
        : ThawedTemplate =
        // One root per distinct typar of the WHOLE decl, on any axis: two occurrences of one
        // typar must land on ONE cell, or a parameter's type and the uses of that parameter
        // come apart. Slot `i` of an axis array holds typar `i`'s root.
        let declaringRoots = ResizeArray<TyVarId voption>()
        let methodRoots = ResizeArray<TyVarId voption>()
        let localRoots = Dictionary<LocalTyparKey, TyVarId>()

        let mintAt (roots: ResizeArray<TyVarId voption>) (i: int) : SemType =
            while roots.Count <= i do
                roots.Add ValueNone

            match roots.[i] with
            | ValueSome v -> TyVar v
            | ValueNone ->
                let v = store.NewTypeVar()
                roots.[i] <- ValueSome v
                TyVar v

        let inst =
            { new ITyparInstantiation with
                member _.Declaring i = mintAt declaringRoots i
                member _.Method j = mintAt methodRoots j

                member _.Local(scheme, k) =
                    let key = { Scheme = scheme; Index = k }

                    match localRoots.TryGetValue key with
                    | true, v -> TyVar v
                    | _ ->
                        let v = store.NewTypeVar()
                        localRoots.[key] <- v
                        TyVar v
            }

        let thawed =
            TastConvert.decl (FrozenTypeBridge.instantiateWith inst) (LexedFiles.tokenAt retained path) decl

        {
            Decl = thawed
            Typars =
                Seq.append declaringRoots methodRoots
                |> Seq.choose (
                    function
                    | ValueSome v -> Some v
                    | ValueNone -> None
                )
                |> Array.ofSeq
        }
