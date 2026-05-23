namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

// The TAST walker: `TExpr` → IL, via the depth-tracked untyped `Cil` helpers
// (the dynamic compiled-name → recipe dispatch can't preserve the phantom
// stack types across the provider boundary; hand-written bodies that can use
// the typed `Op` surface live in the tests).
//
// Slices 2–4 grew coverage to arithmetic intrinsics, `let`/`Var` locals, the
// `FSharpFunc.Invoke` *consumption* path, `let inline` expansion, and list
// construction.
//
// Slice 5 adds the function-*value* half: closure synthesis. A `lower` pre-pass
// expands every `inline` reference (so the walker never sees one) and
// eta-reifies every function-typed `External` used as a value into an explicit
// lambda chain — after lowering, every `TExpr.Lambda` is a function value, and
// every such value is realised at runtime as an `FSharpFunc\`2` subclass.
// `discoverClosures` enumerates those lambdas leaves-first with their captured
// free variables; `Codegen` emits one synthesised type per closure and feeds
// the ctor handles back so a `Lambda`-as-value lowers to `newobj`. Variable
// resolution is now per-method: in `Main` a `Var` is a local; in a closure
// `Invoke` it is the parameter (`ldarg.1`) or a capture (`ldarg.0; ldfld`).

module Emit =

    /// A synthesised closure: one `FSharpFunc\`2<ParamTy, ResultTy>` subclass.
    /// `Node` is the originating `TExpr.Lambda` (matched by reference identity
    /// in the *lowered* tree, which both discovery and emission share, so a
    /// `Lambda`-as-value resolves to its ctor handle). `Captures` is the free
    /// variables in stable order = field order = ctor-arg order = the order
    /// they are pushed at the construction site.
    type Closure =
        {
            Node: TExpr
            Name: string
            ParamKey: NodeKey
            ParamTy: SemType
            ResultTy: SemType
            Body: TExpr
            Captures: (NodeKey * SemType) list
        }

    /// The inferred type carried inline on any `TExpr` node.
    let private typeOfExpr (e: TExpr) : SemType =
        match e with
        | TExpr.Const(_, ty) -> ty
        | TExpr.Var(_, ty) -> ty
        | TExpr.External(_, ty) -> ty
        | TExpr.Lambda(_, _, ty) -> ty
        | TExpr.App(_, _, ty) -> ty
        | TExpr.Let(_, _, _, ty) -> ty
        | TExpr.IfThenElse(_, _, _, ty) -> ty
        | TExpr.Tuple(_, ty) -> ty
        | TExpr.Sequential(_, ty) -> ty
        | TExpr.While(_, _, ty) -> ty
        | TExpr.ForTo(_, _, _, _, ty) -> ty
        | TExpr.ForIn(_, _, _, ty) -> ty
        | TExpr.Match(_, _, ty) -> ty
        | TExpr.TryWith(_, _, ty) -> ty
        | TExpr.TryFinally(_, _, ty) -> ty
        | TExpr.Assignment(_, _, ty) -> ty
        | TExpr.Null ty -> ty
        | TExpr.Range(_, _, _, ty) -> ty
        | TExpr.RecordCons(_, ty) -> ty
        | TExpr.RecordClone(_, _, ty) -> ty
        | TExpr.FieldGet(_, _, ty) -> ty
        | TExpr.FieldSet(_, _, _, ty) -> ty
        | TExpr.UnionCons(_, _, ty) -> ty
        | TExpr.New(_, _, ty) -> ty
        | TExpr.MethodCall(_, _, _, ty) -> ty
        | TExpr.PropertyGet(_, _, ty) -> ty
        | TExpr.StaticMethodCall(_, _, _, ty) -> ty
        | TExpr.StaticPropertyGet(_, _, ty) -> ty
        | TExpr.Format(_, _, ty) -> ty

    /// Rebuild every immediate sub-expression of `e` through `f`. The single
    /// structural recursion the lowering map, the closure collector, and the
    /// free-variable walk all share (the latter two via `iterChildren`).
    let private mapChildren (f: TExpr -> TExpr) (e: TExpr) : TExpr =
        match e with
        | TExpr.Const _
        | TExpr.Var _
        | TExpr.External _
        | TExpr.Null _
        | TExpr.StaticPropertyGet _ -> e
        | TExpr.Lambda(p, b, t) -> TExpr.Lambda(p, f b, t)
        | TExpr.App(fn, a, t) -> TExpr.App(f fn, f a, t)
        | TExpr.Let(p, v, b, t) -> TExpr.Let(p, f v, f b, t)
        | TExpr.IfThenElse(c, th, el, t) -> TExpr.IfThenElse(f c, f th, f el, t)
        | TExpr.Tuple(xs, t) -> TExpr.Tuple(List.map f xs, t)
        | TExpr.Sequential(xs, t) -> TExpr.Sequential(List.map f xs, t)
        | TExpr.While(c, b, t) -> TExpr.While(f c, f b, t)
        | TExpr.ForTo(v, s, e2, b, t) -> TExpr.ForTo(v, f s, f e2, f b, t)
        | TExpr.ForIn(p, src, b, t) -> TExpr.ForIn(p, f src, f b, t)
        | TExpr.Match(sc, arms, t) ->
            TExpr.Match(
                f sc,
                arms
                |> List.map (fun a ->
                    { a with
                        Guard = Option.map f a.Guard
                        Body = f a.Body
                    }
                ),
                t
            )
        | TExpr.TryWith(b, arms, t) ->
            TExpr.TryWith(
                f b,
                arms
                |> List.map (fun a ->
                    { a with
                        Guard = Option.map f a.Guard
                        Body = f a.Body
                    }
                ),
                t
            )
        | TExpr.TryFinally(b, c, t) -> TExpr.TryFinally(f b, f c, t)
        | TExpr.Assignment(l, r, t) -> TExpr.Assignment(f l, f r, t)
        | TExpr.Range(s, step, stop, t) -> TExpr.Range(f s, Option.map f step, f stop, t)
        | TExpr.RecordCons(fields, t) -> TExpr.RecordCons([ for (n, v) in fields -> n, f v ], t)
        | TExpr.RecordClone(src, ov, t) -> TExpr.RecordClone(f src, [ for (n, v) in ov -> n, f v ], t)
        | TExpr.FieldGet(r, n, t) -> TExpr.FieldGet(f r, n, t)
        | TExpr.FieldSet(r, n, v, t) -> TExpr.FieldSet(f r, n, f v, t)
        | TExpr.UnionCons(c, args, t) -> TExpr.UnionCons(c, List.map f args, t)
        | TExpr.New(c, args, t) -> TExpr.New(c, List.map f args, t)
        | TExpr.MethodCall(r, n, args, t) -> TExpr.MethodCall(f r, n, List.map f args, t)
        | TExpr.PropertyGet(r, n, t) -> TExpr.PropertyGet(f r, n, t)
        | TExpr.StaticMethodCall(c, n, args, t) -> TExpr.StaticMethodCall(c, n, List.map f args, t)
        | TExpr.Format(sink, segs, t) ->
            let sink =
                match sink with
                | FormatSink.ToWriter w -> FormatSink.ToWriter(f w)
                | FormatSink.ToBuilder w -> FormatSink.ToBuilder(f w)
                | other -> other

            let segs =
                segs
                |> EqArray.map (fun seg ->
                    match seg with
                    | FormatSeg.Lit _ -> seg
                    | FormatSeg.Hole(h, a) -> FormatSeg.Hole(h, f a)
                )

            TExpr.Format(sink, segs, t)

    /// Visit every immediate sub-expression of `e` for side effects, reusing
    /// `mapChildren` (the rebuilt tree is discarded — only the discovery /
    /// free-variable passes call this, each a one-shot pre-pass).
    let private iterChildren (f: TExpr -> unit) (e: TExpr) : unit =
        mapChildren
            (fun c ->
                f c
                c
            )
            e
        |> ignore

    /// Peel a curried `App` chain into its head and the arguments paired with
    /// each `App` node's *result* type.
    let rec private collectSpine (acc: (TExpr * SemType) list) (e: TExpr) : TExpr * (TExpr * SemType) list =
        match e with
        | TExpr.App(fn, arg, ty) -> collectSpine ((arg, ty) :: acc) fn
        | head -> head, acc

    /// Rebuild a left-associated `App` spine from a head and `(arg, resultTy)`
    /// pairs (the inverse of `collectSpine`).
    let private rebuildApp (head: TExpr) (args: (TExpr * SemType) list) : TExpr =
        List.fold (fun acc (arg, resTy) -> TExpr.App(acc, arg, resTy)) head args

    /// Beta-reduce a curried lambda (an inline expansion's output) against its
    /// spine arguments, lowering each application to a `TExpr.Let`. The lambda
    /// count must match the spine-arg count for a fully applied call.
    let rec private betaReduce (fn: TExpr) (args: (TExpr * SemType) list) : TExpr =
        match fn, args with
        | _, [] -> fn
        | TExpr.Lambda(TPat.NamedSimple(k, paramTy), lamBody, _), (arg, _) :: rest ->
            let reduced = betaReduce lamBody rest
            TExpr.Let(TPat.NamedSimple(k, paramTy), arg, reduced, typeOfExpr reduced)
        | TExpr.Lambda(param, _, _), _ -> failwithf "Emit: inline parameter destructuring is out of scope: %A" param
        | _, _ :: _ -> failwith "Emit: over-application of an inline function"

    // ---- Lowering: inline expansion + External-as-value eta-reification ----

    let private isFunTy (t: SemType) : bool =
        match t with
        | TyFun _ -> true
        | _ -> false

    /// Lower a decl list into a closure-bearing, inline-free, External-value-
    /// free tree. After this, every `TExpr.Lambda` is a function value the
    /// closure machinery realises, and every `External` is either a call head
    /// or has non-function type. Inline bindings are dropped (fully expanded at
    /// their use sites); the call-site args drive beta reduction.
    let lower (decls: TDecl list) : TDecl list =
        let inlines = Dictionary<NodeKey, TDecl>()

        for d in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(b, _), _, true, _) -> inlines.[b] <- d
            | _ -> ()

        // Build-wide monotone counter for freshened inline binders and eta
        // parameters, so independent expansions never share a NodeKey.
        let mutable counter = 0

        let mint () =
            let k = NodeKey.ofSynthetic counter NodeKind.SynthInlineExpansion
            counter <- counter + 1
            k

        // `External(name, a -> b -> … -> r)` used as a value becomes
        // `fun p0 -> fun p1 -> … -> name p0 p1 …` — the eta-reification that
        // turns an operator/function name into a constructible closure.
        let etaExpand (name: string) (ty: SemType) : TExpr =
            let rec arrows t =
                match t with
                | TyFun(a, b) ->
                    let ps, r = arrows b
                    (a :: ps), r
                | _ -> [], t

            let paramTys, retTy = arrows ty
            let kts = paramTys |> List.map (fun pty -> mint (), pty)

            let rec applyAll acc accTy ks =
                match ks, accTy with
                | [], _ -> acc
                | (k, pty) :: rest, TyFun(_, resTy) -> applyAll (TExpr.App(acc, TExpr.Var(k, pty), resTy)) resTy rest
                | _ -> failwith "Emit: eta-reification arity mismatch"

            let appBody = applyAll (TExpr.External(name, ty)) ty kts

            kts
            |> List.foldBack (fun (k, pty) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty), innerBody, lamTy), lamTy
            )
            <| (appBody, retTy)
            |> fst

        let expandInline (k: NodeKey) : TExpr =
            Inline.inlineExpand inlines.[k] [||] |> Inline.freshen mint

        let rec lowerExpr (e: TExpr) : TExpr =
            match e with
            | TExpr.App _ ->
                let head, spineArgs = collectSpine [] e

                match head with
                | TExpr.Var(k, _) when inlines.ContainsKey k ->
                    // Inline call site: expand the body, beta-reduce against
                    // the args into a `Let` chain, then lower that.
                    lowerExpr (betaReduce (expandInline k) spineArgs)
                | _ ->
                    // The head stays in call position (an `External` head is a
                    // recipe call — not eta-reified); the args are values.
                    let head' =
                        match head with
                        | TExpr.External _ -> head
                        | _ -> lowerExpr head

                    rebuildApp head' [ for (a, t) in spineArgs -> lowerExpr a, t ]
            | TExpr.External(name, ty) when isFunTy ty -> etaExpand name ty
            | TExpr.Var(k, _) when inlines.ContainsKey k -> lowerExpr (expandInline k)
            | _ -> mapChildren lowerExpr e

        decls
        |> List.choose (fun d ->
            match d with
            | TDecl.Let(_, _, true, _) -> None
            | TDecl.Let(p, value, false, t) -> Some(TDecl.Let(p, lowerExpr value, false, t))
            | TDecl.Expression(e, t) -> Some(TDecl.Expression(lowerExpr e, t))
        )

    // ---- Closure discovery + capture analysis ----

    let private patKeys (p: TPat) : NodeKey list =
        let rec go p =
            match p with
            | TPat.NamedSimple(k, _) -> [ k ]
            | TPat.Wildcard _
            | TPat.Const _ -> []
            | TPat.Tuple(items, _) -> List.collect go items
            | TPat.Record(fields, _) -> fields |> List.collect (fun (_, sub) -> go sub)
            | TPat.Union(_, fields, _) -> List.collect go fields

        go p

    /// The free variables of a closure body (referenced `Var` keys minus the
    /// parameter and any binder introduced within the body), in first-
    /// occurrence order. Drives capture field order.
    let private freeVars (paramKey: NodeKey) (body: TExpr) : (NodeKey * SemType) list =
        let bound = HashSet<NodeKey>()
        bound.Add paramKey |> ignore
        let acc = ResizeArray<NodeKey * SemType>()
        let seen = HashSet<NodeKey>()

        let scoped (keys: NodeKey list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: TExpr) =
            match e with
            | TExpr.Var(key, ty) ->
                if not (bound.Contains key) && seen.Add key then
                    acc.Add(key, ty)
            | TExpr.Lambda(p, b, _) -> scoped (patKeys p) (fun () -> go b)
            | TExpr.Let(p, v, b, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExpr.ForTo(var, s, e2, b, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExpr.ForIn(p, src, b, _) ->
                go src
                scoped (patKeys p) (fun () -> go b)
            | TExpr.Match(sc, arms, _) ->
                go sc

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | TExpr.TryWith(b, arms, _) ->
                go b

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | _ -> iterChildren go e

        go body
        List.ofSeq acc

    /// Enumerate every `Lambda` in the lowered tree leaves-first (a closure
    /// before any closure that constructs it), with its capture set. The
    /// returned dictionary maps each lambda node (by reference) to its
    /// `Closure`, so the walker resolves a `Lambda`-as-value to its metadata.
    let discoverClosures (decls: TDecl list) : Closure list * Dictionary<TExpr, Closure> =
        let order = ResizeArray<TExpr>()
        let lookup = Dictionary<TExpr, Closure>(HashIdentity.Reference)
        let mutable counter = 0

        let rec go (e: TExpr) =
            iterChildren go e // children (and so inner lambdas) first → leaves-first

            match e with
            | TExpr.Lambda(TPat.NamedSimple(p, pty), body, lamTy) ->
                let resultTy =
                    match lamTy with
                    | TyFun(_, r) -> r
                    | _ -> failwithf "Emit: closure type is not a function: %A" lamTy

                let c =
                    {
                        Node = e
                        Name = sprintf "<closure>$%d" counter
                        ParamKey = p
                        ParamTy = pty
                        ResultTy = resultTy
                        Body = body
                        Captures = freeVars p body
                    }

                counter <- counter + 1
                lookup.[e] <- c
                order.Add e
            | TExpr.Lambda(p, _, _) -> failwithf "Emit: closure parameter destructuring is out of scope: %A" p
            | _ -> ()

        for d in decls do
            match d with
            | TDecl.Let(_, value, _, _) -> go value
            | TDecl.Expression(e, _) -> go e

        [ for n in order -> lookup.[n] ], lookup

    // ---- The walker ----

    /// Per-method codegen context. `Slots` maps locals of the *current* method
    /// to slot indices; `ParamKey` / `CaptureFields` give a closure `Invoke`
    /// body its argument and capture resolution (both empty / `ValueNone` in
    /// `Main`). The closure dictionaries are shared across every method build:
    /// `ClosureByNode` resolves a `Lambda` value to its `Closure`, and
    /// `CtorHandleByNode` (filled leaves-first as `Codegen` emits each closure)
    /// to its ctor handle.
    type private EmitEnv =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            Slots: Dictionary<NodeKey, int>
            ClosureByNode: Dictionary<TExpr, Closure>
            CtorHandleByNode: Dictionary<TExpr, EntityHandle>
            ParamKey: NodeKey voption
            CaptureFields: Dictionary<NodeKey, EntityHandle>
        }

    /// Run a recipe whose operands are already on the stack, settling depth.
    let private applyRecipe (il: Il) (recipe: CallRecipe) : unit =
        recipe.Emit il
        il.Adjust(recipe.Pushes - recipe.ArgCount)

    /// Load a variable for the current method: the closure parameter
    /// (`ldarg.1`), a capture (`ldarg.0; ldfld`), or a local slot (`ldloc`).
    let private emitVarLoad (env: EmitEnv) (il: Il) (key: NodeKey) : unit =
        match env.ParamKey with
        | ValueSome p when p = key -> Cil.emitLdarg il 1
        | _ ->
            match env.CaptureFields.TryGetValue key with
            | true, field ->
                Cil.emitLdarg il 0
                Cil.emitLdfld il field
            | false, _ ->
                match env.Slots.TryGetValue key with
                | true, slot -> Cil.emitLdloc il slot
                | false, _ -> failwithf "Emit: no binding for variable %O" key

    let rec private emitExpr (env: EmitEnv) (il: Il) (e: TExpr) : unit =
        match e with
        | TExpr.Const(TConstValue.String s, _) -> Cil.emitLdstr il (env.Ctx.UserString s)
        | TExpr.Const(TConstValue.Int n, _) -> Cil.emitLdcI4 il n
        | TExpr.Const(TConstValue.Bool b, _) -> Cil.emitLdcI4 il (if b then 1 else 0)
        | TExpr.Const(TConstValue.Byte n, _) -> Cil.emitLdcI4 il (int n)
        | TExpr.Const(TConstValue.Float x, _) -> Cil.emitLdcR8 il x
        | TExpr.Const(TConstValue.Char c, _) -> Cil.emitLdcI4 il (int c)
        | TExpr.Const(TConstValue.Decimal d, _) ->
            // Materialise via `Decimal..ctor(lo, mid, hi, isNegative, scale)` from
            // the value's bit representation — the same shape F#/Roslyn emit.
            let bits = System.Decimal.GetBits d
            let flags = bits.[3]
            Cil.emitLdcI4 il bits.[0] // lo
            Cil.emitLdcI4 il bits.[1] // mid
            Cil.emitLdcI4 il bits.[2] // hi
            Cil.emitLdcI4 il (if flags < 0 then 1 else 0) // sign (high bit of flags)
            Cil.emitLdcI4 il ((flags >>> 16) &&& 0xFF) // scale
            Cil.emitNewobj il env.Provider.DecimalCtor 5

        | TExpr.Var(binding, _) -> emitVarLoad env il binding

        | TExpr.Let(TPat.NamedSimple(binding, ty), value, body, _) ->
            let slot = il.DeclareLocal ty
            env.Slots.[binding] <- slot
            emitExpr env il value
            Cil.emitStloc il slot
            emitExpr env il body
        | TExpr.Let(pat, _, _, _) -> failwithf "Emit: destructuring let-binding is out of scope: %A" pat

        | TExpr.Lambda _ ->
            // A function value: construct its closure. Captures are pushed via
            // the *current* resolver (a local in `Main`, the param or a capture
            // inside an enclosing closure), then `newobj` its ctor.
            match env.ClosureByNode.TryGetValue e with
            | true, closure ->
                for (k, _) in closure.Captures do
                    emitVarLoad env il k

                match env.CtorHandleByNode.TryGetValue e with
                | true, ctor -> Cil.emitNewobj il ctor (List.length closure.Captures)
                | false, _ -> failwith "Emit: closure constructor not yet emitted (leaves-first ordering broken)"
            | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

        | TExpr.New(className, args, ty) ->
            for a in args do
                emitExpr env il a

            let tyArgs =
                match ty with
                | TyClass(_, xs) -> xs
                | _ -> []

            match env.Provider.TryEmitCtor(className, tyArgs) with
            | ValueSome recipe -> Cil.emitNewobj il recipe.Handle recipe.ArgCount
            | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className

        | TExpr.App _ ->
            let head, spineArgs = collectSpine [] e

            match head with
            | TExpr.External(name, _) ->
                // The recipe reads its generic instantiation from the head's
                // full curried type (`fnTy`).
                match env.Provider.TryEmitCall(name, typeOfExpr head) with
                | ValueSome recipe ->
                    let leading, rest = List.splitAt recipe.ArgCount spineArgs

                    for (a, _) in leading do
                        emitExpr env il a

                    applyRecipe il recipe

                    // Whatever the recipe left on the stack — a function value
                    // the rest of the spine is applied to.
                    let funcTy =
                        match List.tryLast leading with
                        | Some(_, ty) -> ty
                        | None -> typeOfExpr head

                    foldInvoke env il funcTy rest
                | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

            | _ ->
                // The head is itself a function value (a closure local or a
                // partially applied result): emit it, then `Invoke` each arg.
                emitExpr env il head
                foldInvoke env il (typeOfExpr head) spineArgs

        | TExpr.UnionCons(caseName, args, ty) ->
            let typeName, tyArgs =
                match ty with
                | TyRecord(n, xs)
                | TyUnion(n, xs) -> n, xs
                | other -> failwithf "Emit: UnionCons with non-union type %A" other

            for a in args do
                emitExpr env il a

            match env.Provider.TryEmitUnionCons(typeName, caseName, tyArgs) with
            | ValueSome recipe -> applyRecipe il recipe
            | ValueNone -> failwithf "Emit: no union-cons recipe for %s.%s" typeName caseName

        | TExpr.Format(sink, segments, _) -> emitFormat env il sink segments

        | other -> failwithf "Emit: unsupported expression: %A" other

    /// Lower a `TExpr.Format` to the `Vesper.Formatter` write-through handler: a
    /// ref-struct local constructed in place, then each segment folded
    /// left-to-right (`AppendLiteral` for a literal run, `AppendFormatted<T>`
    /// for a hole — its arg evaluated *here*, at its position), then a trailing
    /// newline (printfn-style sinks) and flush, or `ToStringAndClear` for the
    /// string sink. The node yields a value: the `unit` (null) of the writing
    /// sinks, or the result string of `sprintf`. Not a `CallRecipe` — the recipe
    /// model can't interleave literals/args around a ref-struct local + sink.
    and private emitFormat (env: EmitEnv) (il: Il) (sink: FormatSink) (segments: EqArray<FormatSeg>) : unit =
        let fh = env.Provider.FormatHandles()
        let slot = il.DeclareLocal fh.HandlerLocal

        // Capacity hints for the ctor; the handler grows past them as needed, so
        // they need not be exact.
        let mutable litLen = 0
        let mutable holeCount = 0

        for seg in segments do
            match seg with
            | FormatSeg.Lit s -> litLen <- litLen + s.Length
            | FormatSeg.Hole _ -> holeCount <- holeCount + 1

        // Construct in place: `ldloca h; ldc litLen; ldc holeCount; <sink?>; call .ctor`.
        Cil.emitLdloca il slot
        Cil.emitLdcI4 il litLen
        Cil.emitLdcI4 il holeCount

        match sink with
        | FormatSink.ToString -> Cil.emitCall il fh.CtorString 3 0
        | FormatSink.ToStdOut _ ->
            Cil.emitCall il fh.ConsoleOut 0 1
            Cil.emitCall il fh.CtorWriter 4 0
        | FormatSink.ToStdErr _ ->
            Cil.emitCall il fh.ConsoleError 0 1
            Cil.emitCall il fh.CtorWriter 4 0
        | FormatSink.ToWriter w ->
            emitExpr env il w
            Cil.emitCall il fh.CtorWriter 4 0
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

        for seg in segments do
            match seg with
            | FormatSeg.Lit s ->
                Cil.emitLdloca il slot
                Cil.emitLdstr il (env.Ctx.UserString s)
                Cil.emitCall il fh.AppendLiteral 2 0
            | FormatSeg.Hole(hole, arg) ->
                match hole.Kind with
                | PrintfSpec.HoleKind.Formatted ->
                    Cil.emitLdloca il slot
                    emitExpr env il arg

                    // Push optional args in the C# parameter order: alignment, then format.
                    match hole.Alignment with
                    | Some a -> Cil.emitLdcI4 il a
                    | None -> ()

                    match hole.Format with
                    | Some f -> Cil.emitLdstr il (env.Ctx.UserString f)
                    | None -> ()

                    let handle = fh.AppendFormatted(hole.Ty, hole.Alignment.IsSome, hole.Format.IsSome)

                    let argc =
                        2
                        + (if hole.Alignment.IsSome then 1 else 0)
                        + (if hole.Format.IsSome then 1 else 0)

                    Cil.emitCall il handle argc 0

                | PrintfSpec.HoleKind.BoolText
                | PrintfSpec.HoleKind.Octal
                | PrintfSpec.HoleKind.Unsigned ->
                    // A dedicated handler member `(value, int alignment)` — no
                    // .NET format string. The alignment is always pushed (0 ⇒ no
                    // padding); `%u`'s `int`→`uint` is a free CLI-stack
                    // reinterpret, so the arg is emitted unchanged.
                    let handle =
                        match hole.Kind with
                        | PrintfSpec.HoleKind.BoolText -> fh.AppendBool
                        | PrintfSpec.HoleKind.Octal -> fh.AppendOctal
                        | _ -> fh.AppendUnsigned

                    Cil.emitLdloca il slot
                    emitExpr env il arg
                    Cil.emitLdcI4 il (defaultArg hole.Alignment 0)
                    Cil.emitCall il handle 3 0

                | PrintfSpec.HoleKind.ZeroPaddedFloat ->
                    // `AppendZeroPaddedFloat(value, "F<prec>", width)` — the
                    // `"F<prec>"` body rides in `Format`, the field width in
                    // `Alignment` (both guaranteed present by `tryHoleFormat`).
                    let fmt =
                        match hole.Format with
                        | Some f -> f
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its format string"

                    let width =
                        match hole.Alignment with
                        | Some w -> w
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its width"

                    Cil.emitLdloca il slot
                    emitExpr env il arg
                    Cil.emitLdstr il (env.Ctx.UserString fmt)
                    Cil.emitLdcI4 il width
                    Cil.emitCall il fh.AppendZeroPaddedFloat 4 0

        match sink with
        | FormatSink.ToString ->
            // Leaves the built string on the stack (the `sprintf` result).
            Cil.emitLdloca il slot
            Cil.emitCall il fh.ToStringAndClear 1 1
        | FormatSink.ToStdOut nl
        | FormatSink.ToStdErr nl ->
            if nl then
                Cil.emitLdloca il slot
                Cil.emitLdstr il (env.Ctx.UserString "\n")
                Cil.emitCall il fh.AppendLiteral 2 0

            Cil.emitLdloca il slot
            Cil.emitCall il fh.Flush 1 0
            Cil.emitLdnull il // unit value
        | FormatSink.ToWriter _ ->
            Cil.emitLdloca il slot
            Cil.emitCall il fh.Flush 1 0
            Cil.emitLdnull il // unit value
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

    /// Apply each remaining argument to the function value on the stack via
    /// `FSharpFunc.Invoke`, threading the running function type.
    and private foldInvoke (env: EmitEnv) (il: Il) (funcTy0: SemType) (args: (TExpr * SemType) list) : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitInvoke funcTy with
            | ValueSome recipe ->
                emitExpr env il arg
                applyRecipe il recipe
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to value of type %A" funcTy

    /// Emit an expression as a statement: evaluate it and discard any value.
    let private emitStatement (env: EmitEnv) (il: Il) (e: TExpr) : unit =
        emitExpr env il e

        while il.Depth > 0 do
            Cil.emitPop il

    /// Build the `Main` body from the *lowered* decls. Each top-level `let`
    /// binds a `Main` local; each effectful expression is emitted in source
    /// order; then `ldc.i4.0; ret`. (Inline bindings were removed by `lower`.)
    let emitMain
        (provider: ICodegenProvider)
        (ctx: MetadataContext)
        (closureByNode: Dictionary<TExpr, Closure>)
        (ctorHandleByNode: Dictionary<TExpr, EntityHandle>)
        (decls: TDecl list)
        (il: Il)
        : unit =
        let env =
            {
                Provider = provider
                Ctx = ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = closureByNode
                CtorHandleByNode = ctorHandleByNode
                ParamKey = ValueNone
                CaptureFields = Dictionary<NodeKey, EntityHandle>()
            }

        for d in decls do
            match d with
            | TDecl.Expression(e, _) -> emitStatement env il e
            | TDecl.Let(TPat.NamedSimple(binding, _), value, _, ty) ->
                let slot = il.DeclareLocal ty
                env.Slots.[binding] <- slot
                emitExpr env il value
                Cil.emitStloc il slot
            | TDecl.Let _ -> ()

        Cil.emitLdcI4 il 0
        Cil.emitRet il

    /// Build a closure's `Invoke` body: evaluate its (lowered) body under a
    /// resolver mapping the parameter to `ldarg.1` and each capture to its
    /// field, leaving the result on the stack, then `ret`.
    let emitClosureInvoke
        (provider: ICodegenProvider)
        (ctx: MetadataContext)
        (closureByNode: Dictionary<TExpr, Closure>)
        (ctorHandleByNode: Dictionary<TExpr, EntityHandle>)
        (closure: Closure)
        (captureFields: Dictionary<NodeKey, EntityHandle>)
        (il: Il)
        : unit =
        let env =
            {
                Provider = provider
                Ctx = ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = closureByNode
                CtorHandleByNode = ctorHandleByNode
                ParamKey = ValueSome closure.ParamKey
                CaptureFields = captureFields
            }

        emitExpr env il closure.Body
        Cil.emitRet il

    /// Build a closure's `.ctor` body: chain to the `FSharpFunc\`2` base ctor,
    /// then store each capture argument into its field.
    let emitClosureCtor (baseCtor: EntityHandle) (fields: EntityHandle list) (il: Il) : unit =
        Cil.emitLdarg il 0
        Cil.emitCall il baseCtor 1 0 // call instance void base::.ctor()

        fields
        |> List.iteri (fun i field ->
            Cil.emitLdarg il 0
            Cil.emitLdarg il (i + 1)
            Cil.emitStfld il field
        )

        Cil.emitRet il
