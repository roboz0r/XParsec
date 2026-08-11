namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferResolve
open UnificationInferOverload
open UnificationInferDispatch
open UnificationInferRecordAccess

// Typing an assignment's LEFT-hand side. A read types itself; a write may instead type off a
// declared `set_` accessor, which is why the LHS walk lives here rather than in `infer`.

module internal UnificationInferAssign =

    /// The value type a one-argument `set_P : T -> unit` accepts, pinned on `node` so Elaborate
    /// calls the level declaring it. The written value is inferred after the LHS, so the fresh
    /// var discriminates by ARITY: an indexed `set_P : i -> v -> unit` takes two and loses.
    let private trySlotSetter
        (ctx: PassContext)
        (node: NodeSite)
        (objArgTy: SemType)
        (propName: string)
        : SemType voption =
        let valueSlot = TyVar(freshTyVar ctx)

        match pickInstanceMember ctx objArgTy (AccessorNames.setterName propName) [ valueSlot ] with
        | InstanceMemberPick.Resolved setter ->
            // A lone `set_P` skips the ranker, so an indexed-only property still arrives here
            // and its second arrow is what rejects it.
            match zonk ctx.Store setter.MemberTy with
            | TyFun(valueTy, ret) ->
                match zonk ctx.Store ret with
                | TyFun _ -> ValueNone
                | _ ->
                    stampInstanceMember ctx node.Key setter
                    ValueSome valueTy
            | _ -> ValueNone
        // A write does not report a `set_P` miss; the read fall-through below diagnoses it.
        | InstanceMemberPick.Unresolved _
        | InstanceMemberPick.NotFound -> ValueNone

    /// The value type a STATIC `set_P` accepts, instantiated at fresh type args. An INDEXED
    /// property is the one rejection left: `set_P : i -> v -> unit` wants an index the write
    /// supplies nothing for.
    let private staticSlotValueTy
        (ctx: PassContext)
        (node: NodeSite)
        (setter: TypeRegistry.NominalMember)
        (propName: string)
        : SemType =
        let setterTy = freshMemberInstance ctx setter

        match zonk ctx.Store setterTy, SemTypeQuery.Funs.count ctx.Store setterTy with
        | TyFun(valueTy, _), 1 -> valueTy
        | _ ->
            errorTy
                ctx
                node.Tok
                (Kind.Message(sprintf "'%s' is an indexed property; a write must supply its index" propName))

    let private inferAssignObjArg (infer: Infer) (ctx: PassContext) (access: NodeSite) (objArg: AssignObjArg) =
        match objArg with
        | AssignObjArg.Expr e -> infer ctx e
        | AssignObjArg.ChainPrefix li -> inferLongIdentPrefix ctx access li

    /// The type an assignment LHS accepts.
    let private inferAssignLhs
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (lhs: AssignLhs)
        (left: Expr<SyntaxToken>)
        : SemType =
        let access = lhs.Access

        // `infer` links every node it walks; the paths below bypass it, so the LHS node and
        // any parens around it link here.
        let linkLhs (ty: SemType) =
            unify ctx access.Tok (TyVar(tvOf ctx access.Key)) ty
            unify ctx access.Tok (TyVar(tvOf ctx (CstKeys.ofExpr left))) ty
            ty

        match lhs.Target with
        | AssignTarget.Slot(objArg, slotTok) ->
            let objArgTy = inferAssignObjArg infer ctx access objArg
            let name = ctx.NameOf slotTok
            let setterValueTy = trySlotSetter ctx node objArgTy name

            // A readable half types the LHS itself, so only a WRITE-ONLY slot takes its type
            // from the setter. Both halves are looked up over the `inherit` chain.
            match setterValueTy with
            | ValueSome valueTy when List.isEmpty (memberLevels ctx objArgTy name) -> linkLhs valueTy
            | _ -> linkLhs (resolveFieldStep ctx access slotTok objArgTy)
        // A static slot has only the setter to type it: no qualifier is walked the way an
        // object argument's `inherit` chain is.
        | AssignTarget.StaticSlot(setter, slotTok) -> linkLhs (staticSlotValueTy ctx node setter (ctx.NameOf slotTok))
        // A getter of ANY provenance types the element. Failing that the slot is write-only,
        // whatever declared it, so the fresh var is left for the write to pin.
        | AssignTarget.Indexed(objArg, index) ->
            let objArgTy = infer ctx objArg
            let idxTy = infer ctx index

            match tryResolveIndexedGet ctx access objArgTy idxTy with
            | ValueSome elemTy -> linkLhs elemTy
            | ValueNone -> linkLhs (TyVar(freshTyVar ctx))
        // The dispatcher routes an unparenthesised `x?n <- v` to `inferDynamicSet` before
        // reaching here, so a dynamic LHS only ever arrives wrapped, as a read.
        | AssignTarget.Dynamic _
        | AssignTarget.Plain -> infer ctx left

    let inferAssignment
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        let lhs = AssignTarget.ofExpr ctx left
        // Mutability of the LHS is a Validation concern; here we only typecheck.
        let leftTy = inferAssignLhs infer ctx node lhs left
        let rightTy = infer ctx right
        unify ctx node.Tok leftTy rightTy

        // The LHS walk types `arr.[i] <- v` off the GETTER, so the write is resolved here,
        // under this node's key: the key the `set_Item` call and the `$0[$1] = $2` body are
        // both emitted by.
        match lhs.Target with
        | AssignTarget.Indexed(objArg = arrE; index = idxE) ->
            let arrTy = zonk ctx.Store (TyVar(tvOf ctx (CstKeys.ofExpr arrE)))
            let idxTy = zonk ctx.Store (TyVar(tvOf ctx (CstKeys.ofExpr idxE)))
            resolveIndexedSet ctx node arrTy idxTy rightTy
        | _ -> ()

        ctx.Intrinsics.Unit
