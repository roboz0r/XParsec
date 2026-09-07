namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open Vesper
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
            /// The scheme each generalised body-local of the template thawed to, keyed by the
            /// bound variable `Decl` binds it at. The host must quantify these.
            Locals: Map<BoundVarKey, TypeScheme>
        }

    /// The scheme each body-local in `schemes` regeneralises to: the type its `let` was thawed
    /// to, quantified over `roots`' cells for the local's own typars.
    let private thawedLocals
        (roots: LocalTyparRoots)
        (schemes: Map<BoundVarKey, LocalScheme>)
        (decl: TDecl)
        : Map<BoundVarKey, TypeScheme> =
        let boundTys = Dictionary<BoundVarKey, SemType>(HashIdentity.Structural)

        let it =
            { TastWalk.identityIter with
                VisitPat =
                    fun _ p ->
                        match BoundVarKey.ofPat p with
                        | ValueSome k when schemes.ContainsKey k -> boundTys.[k] <- TPatG.ty p
                        | _ -> ()

                        true
            }

        match decl with
        | TDecl.Let(m, _, _) ->
            TastWalk.iterPat it m.Pattern
            TastWalk.iterExpr it m.Value
        | TDecl.LetGroup(members = members) ->
            for m in members do
                TastWalk.iterPat it m.Pattern
                TastWalk.iterExpr it m.Value
        | TDecl.Expression(e, _) -> TastWalk.iterExpr it e
        | TDecl.Type _ -> ()

        schemes
        |> Map.map (fun key scheme ->
            // A typar of the scheme absent from the body has no leaf and so no cell yet;
            // minting it here keeps index `i` at position `i`.
            let typars =
                Block.toList (Block.init scheme.TyparArity (fun i -> roots.At(scheme.Id, i)))

            match boundTys.TryGetValue key with
            | true, ty -> TypeScheme(typars, ty)
            | _ ->
                failwithf
                    "InlineThaw: the template's local scheme at %O names a bound variable the thawed body does not bind"
                    (BoundVarKey.identity key)
        )

    /// Tokens stay the DECLARING file's, read out of `path`'s retained text, so a node still
    /// spells where it was written.
    let bodyAtPath
        (thaw: IMeasuredThaw)
        (retained: LexedFiles)
        (path: AssemblyFilePath)
        (body: Wire.UnpooledDecl)
        : ThawedTemplate =
        // One root per distinct typar of the WHOLE decl, in any scope: two occurrences of one
        // typar must land on ONE cell, or a parameter's type and the uses of that parameter
        // come apart.
        let store = thaw.Store
        let declaringRoots = TyparRoots store
        let methodRoots = TyparRoots store
        let localRoots = LocalTyparRoots store

        let inst =
            FrozenTypeBridge.TyparInstantiation.ofScopes
                (fun _ i -> TyVar(declaringRoots.At i))
                (fun _ i -> TyVar(methodRoots.At i))
                (fun _ i -> TyVar(methodRoots.At i))
                (fun binding i -> TyVar(localRoots.At(binding, TyparIndex.typeSlot i)))

        let thawed =
            TastConvert.decl (FrozenTypeBridge.instantiateWith thaw inst) (LexedFiles.tokenAt retained path) body.Decl

        {
            Decl = thawed
            Typars = Seq.append declaringRoots.Minted methodRoots.Minted |> Array.ofSeq
            Locals = thawedLocals localRoots body.LocalSchemes thawed
        }
