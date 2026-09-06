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
            /// for it is supplied: the declaring type's first, then the function's own, each in
            /// index order. A body-local scheme quantifies its own typars below the template,
            /// so it contributes none.
            Typars: TyVarId[]
        }

    /// Tokens stay the DECLARING file's, read out of `path`'s retained text, so a node still
    /// spells where it was written.
    let bodyAtPath
        (thaw: IMeasuredThaw)
        (retained: LexedFiles)
        (path: AssemblyFilePath)
        (decl: Wire.TDecl)
        : ThawedTemplate =
        // One root per distinct typar of the WHOLE decl, in any scope: two occurrences of one
        // typar must land on ONE cell, or a parameter's type and the uses of that parameter
        // come apart. Slot `i` of a scope array holds typar `i`'s root.
        let store = thaw.Store
        let declaringRoots = ResizeArray<TyVarId voption>()
        let methodRoots = ResizeArray<TyVarId voption>()

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
            FrozenTypeBridge.TyparInstantiation.ofScopes
                (fun _ i -> mintAt declaringRoots i)
                (fun _ i -> mintAt methodRoots i)
                (fun _ i -> mintAt methodRoots i)
                (FrozenTypeBridge.TyparInstantiation.mintLocals store)

        let thawed =
            TastConvert.decl (FrozenTypeBridge.instantiateWith thaw inst) (LexedFiles.tokenAt retained path) decl

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
