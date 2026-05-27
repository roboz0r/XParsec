namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The TAST walker: `TExpr` → IL, via the depth-tracked untyped `Cil` helpers
// (the dynamic compiled-name → recipe dispatch can't preserve the phantom
// stack types across the provider boundary).
//
// A `lower` pre-pass expands every `inline` reference and eta-reifies every
// function-typed `External` used as a value into an explicit lambda chain — so
// afterwards every `TExpr.Lambda` is a function value, realised at runtime as
// an `FSharpFunc\`2` subclass. `discoverClosures` enumerates those lambdas
// leaves-first with their captured free variables. Variable resolution is
// per-method: in `Main` a `Var` is a local; in a closure `Invoke` it is the
// parameter (`ldarg.1`) or a capture (`ldarg.0; ldfld`).

module Emit =

    /// One `FSharpFunc\`2<ParamTy, ResultTy>` subclass. `Node` (the originating
    /// `TExpr.Lambda`) is matched by reference identity in the *lowered* tree
    /// that both discovery and emission share. `Captures` order = field order =
    /// ctor-arg order = the order pushed at the construction site.
    type Closure =
        {
            Node: TExpr
            Name: string
            ParamKey: NodeKey
            ParamTy: SemType
            ResultTy: SemType
            Body: TExpr
            Captures: (NodeKey * SemType) list
            /// The binding key of the `let [rec] f = <this lambda>` this is the
            /// value of, when any. A recursive self-reference resolves to `this`
            /// (`ldarg.0`) in the `Invoke` body rather than being captured, so
            /// there's no self-capture chicken-and-egg at construction.
            /// `ValueNone` for an anonymous lambda.
            SelfKey: NodeKey voption
            /// The enclosing static method's typars (`TypeVar` union-find roots,
            /// in `StaticMethodRef.Typars` order) at this closure's discovery
            /// point — empty for a closure resident in `Main` / a top-level
            /// value let / a monomorphic static method. A non-empty set marks
            /// this as a *generic* closure (C3): its `TypeDefinition` carries
            /// matching `GenericParam` rows, its capture-field / ctor / Invoke
            /// signatures encode free `TyVar`s as `!i`, and the construction
            /// site `Newobj`s a `MemberRef` on the instantiated `TypeSpec`.
            /// An inner closure inherits the enclosing closure's set (and
            /// therefore, transitively, the enclosing static method's set).
            Typars: TypeVar list
        }

    /// One case of an emitted union: its runtime `Tag`, the static factory
    /// `TExpr.UnionCons` `call`s, and its payload field handles in declaration order.
    type EmittedCase =
        {
            Tag: int
            Factory: EntityHandle
            Fields: EntityHandle list
        }

    /// An augmentation member emitted onto a union's `TypeDefinition`. A
    /// property's `Handle` is its `get_<name>` method; `Arity` excludes `this`.
    /// `Handle` is the member's `Def` token — used directly for a monomorphic
    /// union. A *generic* union reaches the member through a `MemberRef` on the
    /// instantiated `TypeSpec` (R2), built from `MetaName` + the signature
    /// (`ParamTys` / `RetTy`, in declaring-typar markers).
    type EmittedMember =
        {
            Handle: EntityHandle
            IsStatic: bool
            Arity: int
            MetaName: string
            ParamTys: SemType list
            RetTy: SemType
        }

    /// A union emitted into this assembly: the shared `int` discriminant field,
    /// each case's emission handles, and its augmentation members by name.
    ///
    /// `Typars` is the declaring type's generic parameters; **empty ⇒ a
    /// monomorphic union** (a single sealed class with `Def`-token member
    /// access). A *generic* union reaches its members through
    /// `ICodegenProvider.GenericUnionMemberRef` (a `MemberRef` on the
    /// instantiated `TypeSpec`) rather than the `Def`-token `TagField` /
    /// `EmittedCase.Factory` / `EmittedCase.Fields` (valid only for the
    /// monomorphic case). `Name` is the registry key the provider mints refs against.
    type EmittedUnion =
        {
            Name: string
            Typars: string list
            TagField: EntityHandle
            Cases: Dictionary<string, EmittedCase>
            Members: Dictionary<string, EmittedMember>
        }

    /// A record emitted into this assembly: a sealed class with one public
    /// field per record field and a single ctor taking the fields in
    /// declaration order. `Fields` is `(name, handle, declared type)` in
    /// declaration order — name-keyed lookup for `FieldGet` / `FieldSet`,
    /// ordered iteration for `RecordCons` (which reorders source-order
    /// initialisers to declaration order before pushing).
    ///
    /// `Typars` ⇒ empty for a monomorphic record (the `Field` / `Ctor` handles
    /// are `Def` tokens used directly). A *generic* record reaches its members
    /// through `ICodegenProvider.GenericRecordMemberRef` (a `MemberRef` on the
    /// instantiated `TypeSpec`, exactly like the generic-union machinery).
    type EmittedRecord =
        {
            Name: string
            Typars: string list
            Fields: (string * EntityHandle * SemType) list
            Ctor: EntityHandle
        }

    /// A top-level function binding lowered to a **static method**:
    /// `let [rec] f p0 p1 … = body` becomes `static <ResultTy> f(p0, p1, …)`,
    /// the curried parameters flattened to method parameters. Eligible only when
    /// the function never escapes as a value and captures no module-level local —
    /// see `collectStaticFns`. A recursive self-call is a direct `call` of the
    /// method's own handle, so no closure self-reference is needed.
    type StaticFn =
        {
            Key: NodeKey
            Name: string
            /// `Some(namespace, holderName)` when this binding came from a named
            /// `module Foo = …` (R3 deferred): it emits as a public static method
            /// named `Name` on a `Foo`/`FooModule` holder type rather than on the
            /// anonymous "Program" holder (which carries the `None` functions). The
            /// front-end records this in `TastFile.ModuleMembers`.
            Holder: (string option * string) option
            Params: (NodeKey * SemType) list
            Body: TExpr
            ResultTy: SemType
        }

    /// The emission handle + shape of a static-method function, resolved before
    /// any body is built (the `MethodDefinition` handle is *predicted* from the
    /// row order). A call site `f a b` `call`s `Handle` with the first `Arity`
    /// args, then `Invoke`s the result with any remainder.
    ///
    /// A *generic* static method (`fold`, R3) carries its type-parameter
    /// `TypeVar`s (the free vars of its signature, by union-find root) and its
    /// declared parameter types: a call site recovers the per-typar instantiation
    /// by matching `ParamTys` against the actual argument types, then `call`s a
    /// `MethodSpec` instead of the bare `MethodDefinition`. Empty `Typars` ⇒ a
    /// monomorphic method (a plain `call`).
    type StaticMethodRef =
        {
            Handle: EntityHandle
            Arity: int
            ResultTy: SemType
            Typars: TypeVar list
            ParamTys: SemType list
        }

    let private typeOfExpr (e: TExpr) : SemType =
        match e with
        | TExpr.Const(_, ty) -> ty
        | TExpr.Var(_, ty) -> ty
        | TExpr.External(_, _, ty) -> ty
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
        | TExpr.ExternalMember(_, _, _, _, ty) -> ty
        | TExpr.Format(_, _, ty) -> ty
        | TExpr.ILIntrinsic(_, _, ty) -> ty
        | TExpr.StaticOptimization(_, _, ty) -> ty

    let private typeOfPat (p: TPat) : SemType =
        match p with
        | TPat.NamedSimple(_, ty)
        | TPat.Wildcard ty
        | TPat.Tuple(_, ty)
        | TPat.Const(_, ty)
        | TPat.Record(_, ty)
        | TPat.Union(_, _, ty) -> ty

    /// Resolve a `SemType`'s `TypeVar` links to their representatives (the codegen
    /// project keeps its own copy rather than depend on the `Passes` namespace —
    /// the `ClrProvider` has the same private helper). A free `TypeVar` stays a
    /// `TyVar root`; a solved one resolves through to its concrete shape.
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let r = UnionFind.find tv

            match r.Link with
            | ValueSome target -> zonk target
            | ValueNone -> TyVar r
        | TyFun(a, b) -> TyFun(zonk a, zonk b)
        | TyTuple xs -> TyTuple(List.map zonk xs)
        | TyRecord(n, xs) -> TyRecord(n, List.map zonk xs)
        | TyUnion(n, xs) -> TyUnion(n, List.map zonk xs)
        | TyClass(n, xs) -> TyClass(n, List.map zonk xs)
        | TyConst _ -> t

    /// Recover a generic static method's per-typar instantiation at a call site
    /// (R3): structurally match each declared parameter type (`defTys`, carrying
    /// the method's typar `TypeVar`s) against the actual argument type, recording
    /// the actual sub-type that lands on each typar. First occurrence wins. For a
    /// recursive self-call the actuals reference the method's own typars, so the
    /// result is those typars (encoded `!!i`); for an external call they are
    /// concrete. `typars` is the ordered typar set (`StaticMethodRef.Typars`).
    let private matchInstantiation
        (typars: TypeVar list)
        (defTys: SemType list)
        (actualTys: SemType list)
        : SemType list =
        let roots = typars |> List.map UnionFind.find
        let result = Array.create roots.Length ValueNone

        let rec go (defT: SemType) (actT: SemType) =
            match zonk defT, zonk actT with
            | TyVar tv, act ->
                let r = UnionFind.find tv

                match roots |> List.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                | Some i ->
                    if result.[i].IsNone then
                        result.[i] <- ValueSome act
                | None -> ()
            | TyFun(a1, r1), TyFun(a2, r2) ->
                go a1 a2
                go r1 r2
            | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> List.iter2 go xs ys
            | TyRecord(_, xs), TyRecord(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
            | TyUnion(_, xs), TyUnion(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
            | TyClass(_, xs), TyClass(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
            | _ -> ()

        List.iter2 go defTys actualTys

        [
            for i in 0 .. roots.Length - 1 ->
                match result.[i] with
                | ValueSome t -> t
                | ValueNone -> failwithf "Emit: could not infer instantiation for static-method type parameter %d" i
        ]

    /// The single structural recursion the lowering map, the closure collector,
    /// and the free-variable walk all share (the latter two via `iterChildren`).
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
        | TExpr.ExternalMember(r, k, n, isProp, t) -> TExpr.ExternalMember(ValueOption.map f r, k, n, isProp, t)
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
        | TExpr.ILIntrinsic(op, args, t) -> TExpr.ILIntrinsic(op, List.map f args, t)
        | TExpr.StaticOptimization(clauses, def, t) ->
            TExpr.StaticOptimization(clauses |> List.map (fun cl -> { cl with Body = f cl.Body }), f def, t)

    /// Reuses `mapChildren`, discarding the rebuilt tree — only the one-shot
    /// discovery / free-variable pre-passes call this.
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

    let private rebuildApp (head: TExpr) (args: (TExpr * SemType) list) : TExpr =
        List.fold (fun acc (arg, resTy) -> TExpr.App(acc, arg, resTy)) head args

    /// Source of synthetic `NodeKey`s for unit-parameter binders (`fun () -> …`
    /// in `peelLambda` / `discoverClosures`). A unit-typed `TPat.Const(Unit, _)`
    /// has no name to bind, so the lambda body never references its key — any
    /// non-colliding key serves. We mint a fresh synthetic key per call so an
    /// `args.[key]` dict (closure `Invoke`, static-method body) can map it to
    /// the corresponding `ldarg` slot without clashing with other binders, and
    /// `freeVars`/`freeVarKeys` can treat it as bound. `Interlocked` keeps it
    /// safe across the parallel test runner; the `synBit` keeps it disjoint
    /// from source keys.
    let mutable private unitParamSynthCounter = 0

    let private mintUnitParamKey () : NodeKey =
        let c = System.Threading.Interlocked.Increment(&unitParamSynthCounter)
        NodeKey.ofSynthetic c NodeKind.SynthLambdaBody

    /// Peel a curried `Lambda` chain of simple (`NamedSimple`) or unit-pattern
    /// (`TPat.Const(Unit, _)`, from `fun () -> …`) parameters. A unit binder
    /// gets a synthetic placeholder `NodeKey` (the body never references it)
    /// so the static-method emission still allocates an `ldarg` slot for the
    /// unit value the caller pushes. Any other pattern stops the peel.
    let rec private peelLambda (e: TExpr) : (NodeKey * SemType) list * TExpr =
        match e with
        | TExpr.Lambda(TPat.NamedSimple(k, pty), body, _) ->
            let ps, b = peelLambda body
            (k, pty) :: ps, b
        | TExpr.Lambda(TPat.Const(TConstValue.Unit, pty), body, _) ->
            let ps, b = peelLambda body
            (mintUnitParamKey (), pty) :: ps, b
        | _ -> [], e

    /// Beta-reduce a curried lambda (an inline expansion's output) against its
    /// spine args, lowering each application to a `TExpr.Let`. Lambda count must
    /// match spine-arg count for a fully applied call.
    let rec private betaReduce (fn: TExpr) (args: (TExpr * SemType) list) : TExpr =
        match fn, args with
        | _, [] -> fn
        | TExpr.Lambda(TPat.NamedSimple(k, paramTy), lamBody, _), (arg, _) :: rest ->
            let reduced = betaReduce lamBody rest
            TExpr.Let(TPat.NamedSimple(k, paramTy), arg, reduced, typeOfExpr reduced)
        | TExpr.Lambda(param, _, _), _ -> failwithf "Emit: inline parameter destructuring is out of scope: %A" param
        | _, _ :: _ -> failwith "Emit: over-application of an inline function"

    // ---- Built-in operator bodies (the inline-IL the operator surface lowers onto) ----

    /// The stopgap source of operator `.fs` bodies, expressed as the inline IL the
    /// general `TExpr.ILIntrinsic` path emits. An operator use site (`a = b`,
    /// `x + y`) freezes to an `External(compiledName)` call head; `expandBuiltinOps`
    /// rewrites the saturated application to the matching body here, so the operator
    /// collapses to an `ILIntrinsic` and codegen owns *no* per-operator dispatch (the
    /// op→opcode choice lives in these bodies, exactly as it will in the eventual
    /// operator `.fs`). The bodies are *monomorphic* at the use-site type: the
    /// int/float/char primitive clauses all share an opcode, so no `when ^T : …`
    /// static-optimization resolution is done here.
    ///
    /// DELETE-WHEN-COMPLETE. This table is a *fallback* now that operator `.fs`
    /// bodies are landing in `src/Vesper.Core/ops-platform.fs`
    /// (operators-plan.md "Phase 3"). The EQUALITY family (`=`/`<>`) is
    /// already sourced from the contract: a saturated `External(op_Equality)` is
    /// expanded from the frozen inline body by `lowerWith` *before* this table's
    /// `expandBuiltinOps` closing phase runs, so the contract body wins on the
    /// primary lowering path and `op_Equality`/`op_Inequality` here only still serve
    /// union member-bodies (Codegen `expandBuiltinOps mem.Body`) and eta-reified
    /// operator values, which do not yet route through the inline bodies. Once the
    /// arithmetic/bitwise `.fs` bodies land AND those two secondary paths route
    /// through them, delete this whole table.
    module private BuiltinOps =

        /// `(# op operands : retTy #)` — the operator whose body is a single opcode.
        let private ilBin (op: string) : TExpr list -> SemType -> TExpr =
            fun operands retTy -> TExpr.ILIntrinsic(op, operands, retTy)

        /// `not (# op operands : retTy #)`, realised as `ceq (# op … #) false` — the
        /// derived ops with no direct opcode (`<>` = `not =`, `<=` = `not >`,
        /// `>=` = `not <`). `retTy` is `bool`, so the inner result and the `false`
        /// literal are both bool; the outer `ceq` against 0 negates it. Each operand
        /// still appears once, so the body needs no rebinding.
        let private ilBinNot (op: string) : TExpr list -> SemType -> TExpr =
            fun operands retTy ->
                let inner = TExpr.ILIntrinsic(op, operands, retTy)
                TExpr.ILIntrinsic("ceq", [ inner; TExpr.Const(TConstValue.Bool false, retTy) ], retTy)

        /// compiled name → (arity, body builder over the operand expressions).
        /// `&&` / `||` are intentionally absent — they short-circuit and freeze to
        /// `IfThenElse`, not an opcode.
        let private table: Map<string, int * (TExpr list -> SemType -> TExpr)> =
            Map
                [
                    // Equality family (C-Eq1) — `=` / `<>`.
                    "op_Equality", (2, ilBin "ceq")
                    "op_Inequality", (2, ilBinNot "ceq")
                    // Ordering family — primitive `clt` / `cgt` (IEEE on floats, O7).
                    "op_LessThan", (2, ilBin "clt")
                    "op_GreaterThan", (2, ilBin "cgt")
                    "op_LessThanOrEqual", (2, ilBinNot "cgt")
                    "op_GreaterThanOrEqual", (2, ilBinNot "clt")
                    // Arithmetic — supersedes the per-op `ClrProvider.TryEmitCall` arms.
                    "op_Addition", (2, ilBin "add")
                    "op_Subtraction", (2, ilBin "sub")
                    "op_Multiply", (2, ilBin "mul")
                    "op_Division", (2, ilBin "div")
                    "op_Modulus", (2, ilBin "rem")
                    "op_UnaryNegation", (1, ilBin "neg")
                    // Bitwise / shift — the signed/default IL form. The contract `.fs`
                    // bodies (`ops-platform.fs`) win at a ground use site (with the
                    // narrow-int / unsigned refinements); these serve the un-ground
                    // fallback (nested / generic operands) the same way arithmetic does.
                    "op_BitwiseAnd", (2, ilBin "and")
                    "op_BitwiseOr", (2, ilBin "or")
                    "op_ExclusiveOr", (2, ilBin "xor")
                    "op_LeftShift", (2, ilBin "shl")
                    "op_RightShift", (2, ilBin "shr")
                    "op_LogicalNot", (1, ilBin "not")
                ]

        /// True when `name` is a built-in operator applied to exactly its arity —
        /// the saturated use site rewritten to inline IL. A partial application
        /// (`(=) 1`) is left as a call head for the eta path.
        let isSaturated (name: string) (spineLen: int) : bool =
            match Map.tryFind name table with
            | Some(arity, _) -> spineLen = arity
            | None -> false

        /// Build the operator's inline-IL body, splicing the (already-rewritten)
        /// operand expressions directly. `retTy` is the application's result type.
        let buildApp (name: string) (opArgs: TExpr list) (retTy: SemType) : TExpr =
            let _, makeInner = table.[name]
            makeInner opArgs retTy

    /// Rewrite every saturated built-in operator application (`a = b`, `x + y`,
    /// `-x`) to its inline-IL body, so it emits through the single
    /// `TExpr.ILIntrinsic` path and codegen owns no per-operator recipe. Run as the
    /// closing phase of `lower` (after inline expansion + eta-reification have
    /// surfaced every operator application) and over type-member bodies, which are
    /// emitted straight from `tast.Decls` and so never pass through `lower`. The
    /// splice is direct — each body uses each operand exactly once — so no binder is
    /// introduced and closure discovery / free-variable analysis are undisturbed.
    /// See docs/operators-plan.md (C-Eq1 last mile).
    let rec expandBuiltinOps (e: TExpr) : TExpr =
        match e with
        | TExpr.App _ ->
            let head, spine = collectSpine [] e

            match head with
            | TExpr.External(name, _, _) when BuiltinOps.isSaturated name (List.length spine) ->
                // `collectSpine` pairs each arg with its `App` node's result type,
                // so the last pair's type is the whole application's result.
                let retTy = snd (List.last spine)
                let opArgs = [ for (a, _) in spine -> expandBuiltinOps a ]
                BuiltinOps.buildApp name opArgs retTy
            | _ -> mapChildren expandBuiltinOps e
        | _ -> mapChildren expandBuiltinOps e

    // ---- Lowering: inline expansion + External-as-value eta-reification ----

    let private isFunTy (t: SemType) : bool =
        match t with
        | TyFun _ -> true
        | _ -> false

    /// Does `e` contain a `TExpr.StaticOptimization` anywhere? An inline body that
    /// does must be expanded with the call site's type arguments (so the clause
    /// resolves against the monomorphised operand type); a body that doesn't keeps
    /// the existing zero-type-arg expansion path unchanged.
    let rec private containsStaticOpt (e: TExpr) : bool =
        match e with
        | TExpr.StaticOptimization _ -> true
        | _ ->
            let mutable found = false
            iterChildren (fun c -> found <- found || containsStaticOpt c) e
            found

    /// Recover an inline binding's type arguments at a call site by matching its
    /// declared parameter types (carrying the quantified typars) against the
    /// actual spine-arg types — the same structural match `matchInstantiation`
    /// does for generic static methods, but **tolerant**: a typar the params don't
    /// pin is left as its own `TyVar` so the catch-all `when ^T : ^T` clause still
    /// selects rather than crashing. Returned in `Inline.quantifiedTypars` order.
    /// A (zonked) `SemType` with no free `TyVar` anywhere — fully monomorphic, so
    /// codegen can encode it. The cross-package equality/`hash` inline bodies reach
    /// `EqualityComparer<^T>`, which `recoverTypeArgs`/`encodeType` can only emit
    /// when `^T` is ground; an unpinned operand (`let f a b = a = b`) leaves it free
    /// and must fall back to `Emit.BuiltinOps` instead (type-args-bug.md DoD §3).
    let rec private isGroundType (t: SemType) : bool =
        match zonk t with
        | TyVar _ -> false
        | TyConst _ -> true
        | TyFun(a, b) -> isGroundType a && isGroundType b
        | TyTuple xs -> List.forall isGroundType xs
        | TyRecord(_, xs)
        | TyUnion(_, xs)
        | TyClass(_, xs) -> List.forall isGroundType xs

    let private deriveInlineTypeArgs (declTy: SemType) (spineArgs: (TExpr * SemType) list) : SemType[] =
        match Inline.quantifiedTypars declTy with
        | [] -> [||]
        | typars ->
            let roots = typars |> List.map UnionFind.find
            let result = Array.create roots.Length ValueNone

            let rec go (defT: SemType) (actT: SemType) =
                match zonk defT, zonk actT with
                | TyVar tv, act ->
                    let r = UnionFind.find tv

                    match roots |> List.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                    | Some i ->
                        if result.[i].IsNone then
                            result.[i] <- ValueSome act
                    | None -> ()
                | TyFun(a1, r1), TyFun(a2, r2) ->
                    go a1 a2
                    go r1 r2
                | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> List.iter2 go xs ys
                | TyRecord(_, xs), TyRecord(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
                | TyUnion(_, xs), TyUnion(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
                | TyClass(_, xs), TyClass(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
                | _ -> ()

            let rec peelParams n t =
                if n <= 0 then
                    []
                else
                    match zonk t with
                    | TyFun(a, b) -> a :: peelParams (n - 1) b
                    | _ -> []

            let rec pairGo ps acts =
                match ps, acts with
                | p :: ps', a :: acts' ->
                    go p a
                    pairGo ps' acts'
                | _ -> ()

            pairGo (peelParams (List.length spineArgs) declTy) [ for (a, _) in spineArgs -> typeOfExpr a ]

            Array.mapi
                (fun i v ->
                    match v with
                    | ValueSome t -> t
                    | ValueNone -> TyVar roots.[i]
                )
                result

    /// Lower a decl list into a closure-bearing, inline-free, External-value-free
    /// tree. After this, every `TExpr.Lambda` is a function value and every
    /// `External` is either a call head or has non-function type. Inline bindings
    /// are dropped (fully expanded at their use sites).
    ///
    /// `externalInlines` maps a referenced package's inline `val` name to its
    /// frozen body (a cross-package inline — milestone M, loaded by
    /// `SymbolProviders.inlineBodies`). A saturated `External(name)` call head
    /// found in that map is expanded in place exactly like a local `let inline`,
    /// so `hash 5` becomes the `EqualityComparer<int>.Default.GetHashCode 5`
    /// `ExternalMember` nodes the frozen body already carries (emitted by P4).
    let lowerWith (externalInlines: Map<string, TDecl>) (decls: TDecl list) : TDecl list =
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

        // Eta-reify `External(name, a -> … -> r)` used as a value into
        // `fun p0 -> … -> name p0 …`, turning a function name into a closure.
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

            let appBody = applyAll (TExpr.External(name, ValueNone, ty)) ty kts

            kts
            |> List.foldBack (fun (k, pty) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty), innerBody, lamTy), lamTy
            )
            <| (appBody, retTy)
            |> fst

        let expandInline (k: NodeKey) : TExpr =
            Inline.inlineExpand inlines.[k] [||] |> Inline.freshen mint

        // An inline whose body carries a static optimization is expanded with the
        // call site's type arguments so the `when ^T : …` clause resolves against
        // the monomorphised operand type (prereq 3); all other inlines keep the
        // zero-type-arg path (`expandInline`) unchanged.
        let expandInlineAt (k: NodeKey) (spineArgs: (TExpr * SemType) list) : TExpr =
            match inlines.[k] with
            | TDecl.Let(_, value, _, declTy) when containsStaticOpt value ->
                Inline.inlineExpand inlines.[k] (deriveInlineTypeArgs declTy spineArgs)
                |> Inline.freshen mint
            | _ -> expandInline k

        // A cross-package inline (milestone M): expand the frozen referenced body
        // with the call site's type arguments — always derived (unlike a local
        // inline, whose non-static-opt path leaves typars abstract), because the
        // body's `EqualityComparer<'T>` `ExternalMember` nodes need `'T` pinned to
        // a concrete type before P4 can encode them.
        let expandExternalInlineAt (decl: TDecl) (spineArgs: (TExpr * SemType) list) : TExpr =
            match decl with
            | TDecl.Let(_, _, _, declTy) ->
                Inline.inlineExpand decl (deriveInlineTypeArgs declTy spineArgs)
                |> Inline.freshen mint
            | _ -> failwith "Emit: external inline body must be a TDecl.Let"

        // A cross-package inline whose static-opt fall-clause rides
        // `EqualityComparer<^T>` (the `=`/`<>`/`hash` family) can only be expanded
        // when the call site pins `^T` to a ground type — otherwise the comparer
        // can't encode the free `!0`. An unpinned operand (`let f a b = a = b`)
        // leaves the External call head in place so the closing `expandBuiltinOps`
        // routes it to `Emit.BuiltinOps`'s `ceq` instead (type-args-bug.md DoD §3).
        let externalInlineArgsGround (decl: TDecl) (spineArgs: (TExpr * SemType) list) : bool =
            match decl with
            | TDecl.Let(_, _, _, declTy) -> deriveInlineTypeArgs declTy spineArgs |> Array.forall isGroundType
            | _ -> false

        let rec lowerExpr (e: TExpr) : TExpr =
            match e with
            | TExpr.App _ ->
                let head, spineArgs = collectSpine [] e

                match head with
                | TExpr.Var(k, _) when inlines.ContainsKey k ->
                    lowerExpr (betaReduce (expandInlineAt k spineArgs) spineArgs)
                | TExpr.External(name, _, _) when
                    externalInlines.ContainsKey name
                    && externalInlineArgsGround externalInlines.[name] spineArgs
                    ->
                    lowerExpr (betaReduce (expandExternalInlineAt externalInlines.[name] spineArgs) spineArgs)
                | _ ->
                    // An `External` head is a recipe call, so it stays in call
                    // position and is not eta-reified; the args are values.
                    let head' =
                        match head with
                        | TExpr.External _ -> head
                        | _ -> lowerExpr head

                    rebuildApp head' [ for (a, t) in spineArgs -> lowerExpr a, t ]
            | TExpr.External(name, _, ty) when isFunTy ty -> etaExpand name ty
            | TExpr.Var(k, _) when inlines.ContainsKey k -> lowerExpr (expandInline k)
            | _ -> mapChildren lowerExpr e

        // Inline / eta lowering surfaces operator applications (an inline body's
        // `+`, an eta-reified `(+)`); `expandBuiltinOps` then collapses every
        // saturated one to inline IL — a closing phase so it sees them all.
        decls
        |> List.choose (fun d ->
            match d with
            | TDecl.Let(_, _, true, _) -> None
            | TDecl.Let(p, value, false, t) -> Some(TDecl.Let(p, expandBuiltinOps (lowerExpr value), false, t))
            | TDecl.Expression(e, t) -> Some(TDecl.Expression(expandBuiltinOps (lowerExpr e), t))
            // Type declarations are emitted as metadata, not through the expr stream.
            | TDecl.Type _ -> None
        )

    /// `lowerWith` with no cross-package inline bodies — the pure-local-inline
    /// path (every caller that does not reference a manifest with `impl` bodies).
    let lower (decls: TDecl list) : TDecl list = lowerWith Map.empty decls

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

    /// The free variables of a closure body, in first-occurrence order — drives
    /// capture field order. `staticFnKeys` are excluded: a reference to a
    /// static-method function is a direct `call`, not a captured value.
    let private freeVars
        (staticFnKeys: HashSet<NodeKey>)
        (paramKey: NodeKey)
        (selfKey: NodeKey voption)
        (body: TExpr)
        : (NodeKey * SemType) list =
        let bound = HashSet<NodeKey>()
        bound.Add paramKey |> ignore
        bound.UnionWith staticFnKeys // static-method references are calls, not captures

        match selfKey with
        | ValueSome k -> bound.Add k |> ignore // the recursive self isn't captured — it's `this`
        | ValueNone -> ()

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

    // ---- Static-method classification (P3b) ----

    /// Like `freeVars` but keeps only keys (no types, no static-method exclusion):
    /// the capture test in `collectStaticFns` must *see* every referenced binding.
    let private freeVarKeys (boundKeys: NodeKey seq) (body: TExpr) : HashSet<NodeKey> =
        let bound = HashSet<NodeKey>(boundKeys)
        let acc = HashSet<NodeKey>()

        let scoped (keys: NodeKey list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: TExpr) =
            match e with
            | TExpr.Var(key, _) ->
                if not (bound.Contains key) then
                    acc.Add key |> ignore
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
        acc

    /// Classify which top-level function bindings can be emitted as **static
    /// methods** rather than closures. A candidate is `let [rec] f p0 … = body`
    /// whose value peels to at least one simple parameter. Eligible only when:
    ///   1. it never *escapes* — every use is a saturated call, so it is never
    ///      needed as a function value (a bare/under-applied reference forces a closure).
    ///   2. it captures no module-level local — its free variables (minus its
    ///      parameters and self) are all themselves eligible static functions
    ///      (direct `call`s). A value-local reference would need a capture field,
    ///      which a static method has no `this` to hold.
    /// Rule 2 is a fixpoint, resolved by removing offenders until stable.
    let collectStaticFns
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (decls: TDecl list)
        : StaticFn list * HashSet<NodeKey> =
        let candidates = Dictionary<NodeKey, (NodeKey * SemType) list * TExpr>()
        let order = ResizeArray<NodeKey>()

        for d in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) ->
                match peelLambda value with
                | (_ :: _ as ps), body ->
                    candidates.[k] <- (ps, body)
                    order.Add k
                | [], _ -> ()
            | _ -> ()

        let arity k =
            let ps, _ = candidates.[k]
            List.length ps

        // Escape analysis: a candidate used as a value or under-applied escapes.
        let escapes = HashSet<NodeKey>()

        let rec walkUses (e: TExpr) =
            match e with
            | TExpr.Var(k, _) when candidates.ContainsKey k -> escapes.Add k |> ignore
            | TExpr.App _ ->
                let head, args = collectSpine [] e

                match head with
                | TExpr.Var(k, _) when candidates.ContainsKey k ->
                    if List.length args < arity k then
                        escapes.Add k |> ignore

                    for (a, _) in args do
                        walkUses a
                | _ ->
                    walkUses head

                    for (a, _) in args do
                        walkUses a
            | _ -> iterChildren walkUses e

        for d in decls do
            match d with
            | TDecl.Let(_, value, _, _) -> walkUses value
            | TDecl.Expression(e, _) -> walkUses e
            | TDecl.Type _ -> ()

        // Each candidate's capture set (free vars minus its own params), tested by rule 2.
        let bodyFree =
            Dictionary<NodeKey, HashSet<NodeKey>>(
                seq {
                    for k in order do
                        let ps, body = candidates.[k]
                        KeyValuePair(k, freeVarKeys (ps |> List.map fst) body)
                }
            )

        // Fixpoint: from the non-escaping candidates, drop any whose free vars
        // reach outside the eligible set (own self-reference allowed).
        let eligible = HashSet<NodeKey>(order |> Seq.filter (escapes.Contains >> not))
        let mutable changed = true

        while changed do
            changed <- false

            for k in List.ofSeq eligible do
                let free = bodyFree.[k]

                let captures = free |> Seq.exists (fun v -> v <> k && not (eligible.Contains v))

                if captures && eligible.Remove k then
                    changed <- true

        let staticFns =
            [
                for k in order do
                    if eligible.Contains k then
                        let ps, body = candidates.[k]

                        // A binding inside a named module emits with its source
                        // name on its holder type (R3 deferred); a top-level
                        // function keeps the anonymous `fn$<offset>` name on the
                        // "Program" holder (`Holder = None`).
                        let name, holder =
                            match Map.tryFind k.Raw moduleMembers with
                            | Some info -> info.Name, Some(info.Namespace, info.Holder)
                            | None -> sprintf "fn$%d" k.Offset, None

                        yield
                            {
                                Key = k
                                Name = name
                                Holder = holder
                                Params = ps
                                Body = body
                                ResultTy = typeOfExpr body
                            }
            ]

        staticFns, eligible

    /// A generic static method's type parameters (R3): the distinct free
    /// `TypeVar` roots of its signature (parameter types, then result type), in
    /// first-appearance order. Empty ⇒ a monomorphic method, emitted unchanged.
    /// These same `TypeVar` objects appear in the method's body, so the
    /// backend's ambient typar set (`ClrProvider.SetMethodTypars`) maps them to
    /// `!!i`. A closure walked from this fn's body inherits this set on its
    /// `Closure.Typars` (function-representation-plan §Generic closures, C1).
    let staticFnTypars (fn: StaticFn) : TypeVar list =
        let seen = HashSet<TypeVar>(HashIdentity.Reference)
        let acc = ResizeArray<TypeVar>()

        let rec go (t: SemType) =
            match zonk t with
            | TyVar tv ->
                let r = UnionFind.find tv

                if seen.Add r then
                    acc.Add r
            | TyFun(a, b) ->
                go a
                go b
            | TyTuple xs
            | TyRecord(_, xs)
            | TyUnion(_, xs)
            | TyClass(_, xs) -> List.iter go xs
            | TyConst _ -> ()

        for (_, pty) in fn.Params do
            go pty

        go fn.ResultTy
        List.ofSeq acc

    /// Enumerate every `Lambda` in the lowered tree leaves-first (a closure
    /// before any closure that constructs it), with its capture set; the returned
    /// dictionary maps each lambda node (by reference) to its `Closure`.
    /// `staticFnKeys`' outer lambdas are *not* closures (only their bodies are
    /// walked for inner closures), since a reference to one is a direct call.
    ///
    /// `staticFnTypars` maps each static-method key to its typar set
    /// (`StaticMethodRef.Typars`). A closure walked from a generic static fn's
    /// body inherits that fn's typars on its `Closure.Typars`; an inner closure
    /// inherits the enclosing closure's set verbatim (rule generalises cleanly
    /// when a future change makes a child's typars a strict subset of the
    /// enclosing method's — see function-representation-plan §Generic closures).
    let discoverClosures
        (staticFnKeys: HashSet<NodeKey>)
        (staticFnTypars: IReadOnlyDictionary<NodeKey, TypeVar list>)
        (decls: TDecl list)
        : Closure list * Dictionary<TExpr, Closure> =
        let order = ResizeArray<TExpr>()
        let lookup = Dictionary<TExpr, Closure>(HashIdentity.Reference)
        let mutable counter = 0

        // `selfKey` is the binding key when this node is the immediate value of a
        // `let f = …` lambda — a recursive self-reference resolves to `this`.
        // `currentTypars` is the ambient typar set inherited from the enclosing
        // static method (or, for inner closures, the enclosing closure verbatim).
        let rec go (currentTypars: TypeVar list) (selfKey: NodeKey voption) (e: TExpr) =
            (match e with
             | TExpr.Let(TPat.NamedSimple(k, _), (TExpr.Lambda _ as v), body, _) ->
                 go currentTypars (ValueSome k) v
                 go currentTypars ValueNone body
             | _ -> iterChildren (go currentTypars ValueNone) e) // children (and inner lambdas) first → leaves-first

            let registerClosure (p: NodeKey) (pty: SemType) (body: TExpr) (lamTy: SemType) =
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
                        Captures = freeVars staticFnKeys p selfKey body
                        SelfKey = selfKey
                        Typars = currentTypars
                    }

                counter <- counter + 1
                lookup.[e] <- c
                order.Add e

            match e with
            | TExpr.Lambda(TPat.NamedSimple(p, pty), body, lamTy) -> registerClosure p pty body lamTy
            | TExpr.Lambda(TPat.Const(TConstValue.Unit, pty), body, lamTy) ->
                // A `fun () ->` unit binder has no name to reference, but the
                // closure's `Invoke` still allocates `ldarg.1` for the unit
                // value the caller pushes; mint a synthetic placeholder so the
                // `args` map (and `freeVars`'s bound set) still has a key.
                registerClosure (mintUnitParamKey ()) pty body lamTy
            | TExpr.Lambda(p, _, _) -> failwithf "Emit: closure parameter destructuring is out of scope: %A" p
            | _ -> ()

        let typarsForStaticFn (k: NodeKey) : TypeVar list =
            match staticFnTypars.TryGetValue k with
            | true, tps -> tps
            | false, _ -> []

        for d in decls do
            match d with
            // A static-method function's lambda is not a closure, but its body
            // may still construct inner closures — walk only the body. The
            // closures inherit the method's typars.
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) when staticFnKeys.Contains k ->
                let _, body = peelLambda value
                go (typarsForStaticFn k) ValueNone body
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) -> go [] (ValueSome k) value
            | TDecl.Let(_, value, _, _) -> go [] ValueNone value
            | TDecl.Expression(e, _) -> go [] ValueNone e
            | TDecl.Type _ -> ()

        [ for n in order -> lookup.[n] ], lookup

    // ---- The walker ----

    /// Per-method codegen context. `Slots` maps the current method's locals to
    /// slot indices; `Args` maps a method parameter to its `ldarg` index (closure
    /// `Invoke`: `this` is 0, the parameter 1; static method: flattened params
    /// 0…N-1; `Main`: none). `CaptureFields` resolves a closure's captures. The
    /// shared dictionaries `ClosureByNode` / `CtorHandleByNode` / `StaticMethods`
    /// resolve a `Lambda` value, its (leaves-first) ctor handle, and a top-level
    /// function reference to a direct `call`.
    /// The codegen-run-wide registries every builder needs: the provider seam,
    /// the metadata writer, and the four shared dictionaries that resolve a
    /// `Lambda` value to its emitted closure, its (leaves-first) `.ctor` handle,
    /// a nominal type reference, and a top-level function to a direct `call`.
    /// Constructed once at the top of the codegen run and threaded into every
    /// builder; per-method state (slots, args, captures, self) is layered on
    /// top inside each builder when it constructs an `EmitEnv`.
    type EmitContext =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            ClosureByNode: Dictionary<TExpr, Closure>
            CtorHandleByNode: Dictionary<TExpr, EntityHandle>
            Unions: Dictionary<string, EmittedUnion>
            Records: Dictionary<string, EmittedRecord>
            StaticMethods: Dictionary<NodeKey, StaticMethodRef>
        }

    type private EmitEnv =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            Slots: Dictionary<NodeKey, int>
            ClosureByNode: Dictionary<TExpr, Closure>
            CtorHandleByNode: Dictionary<TExpr, EntityHandle>
            Args: Dictionary<NodeKey, int>
            /// The recursive self of the current closure `Invoke` body — resolved
            /// to `this` (`ldarg.0`). `ValueNone` in `Main` / a static method
            /// (whose self-call is a direct `call`).
            SelfKey: NodeKey voption
            CaptureFields: Dictionary<NodeKey, EntityHandle>
            Unions: Dictionary<string, EmittedUnion>
            Records: Dictionary<string, EmittedRecord>
            StaticMethods: Dictionary<NodeKey, StaticMethodRef>
        }

    /// Load a variable for the current method: a method parameter (`ldarg.i`),
    /// the recursive self of a closure (`this`, `ldarg.0`), a capture
    /// (`ldarg.0; ldfld`), or a local slot (`ldloc`).
    let private buildVarLoad (env: EmitEnv) (b: IlBuilder) (key: NodeKey) : unit =
        match env.Args.TryGetValue key with
        | true, i -> b.Add(ILInstr.Ldarg i)
        | false, _ ->

            match env.SelfKey with
            | ValueSome s when s = key -> b.Add(ILInstr.Ldarg 0) // `this` — the recursive self
            | _ ->
                match env.CaptureFields.TryGetValue key with
                | true, field ->
                    b.Add(ILInstr.Ldarg 0)
                    b.Add(ILInstr.Ldfld field)
                | false, _ ->
                    match env.Slots.TryGetValue key with
                    | true, slot -> b.Add(ILInstr.Ldloc slot)
                    | false, _ -> failwithf "Emit: no binding for variable %O" key

    /// Test a pattern against the value already stored in local `scrutSlot`:
    /// branch to `nextLabel` on mismatch, and bind any pattern variables. A
    /// `Const` compares (`bne.un` skips the arm); `Wildcard` / `NamedSimple`
    /// always match (the latter aliases its binding to `scrutSlot`, so
    /// `emitVarLoad` resolves it to the same local — no copy). Union / tuple /
    /// record patterns land in later rung-2 slices.
    let rec private buildMatchTest (env: EmitEnv) (b: IlBuilder) (scrutSlot: int) (nextLabel: int) (pat: TPat) : unit =
        match pat with
        | TPat.Wildcard _ -> ()
        | TPat.NamedSimple(binding, _) -> env.Slots.[binding] <- scrutSlot
        | TPat.Const(value, _) ->
            b.Add(ILInstr.Ldloc scrutSlot)

            match value with
            | TConstValue.Int n -> b.Add(ILInstr.LdcI4 n)
            | TConstValue.Bool v -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
            | TConstValue.Byte n -> b.Add(ILInstr.LdcI4(int n))
            | TConstValue.Char c -> b.Add(ILInstr.LdcI4(int c))
            | other -> failwithf "Emit: match on constant %A is out of scope" other

            b.Add(ILInstr.BneUn nextLabel)
        | TPat.Union(caseName, subPats, ty) ->
            let typeName, tyArgs =
                match ty with
                | TyUnion(n, xs)
                | TyRecord(n, xs) -> n, xs
                | other -> failwithf "Emit: union pattern with non-union type %A" other

            match env.Unions.TryGetValue typeName with
            | true, u ->
                let c = u.Cases.[caseName]

                // Tag / field access is a `Def` token for a monomorphic union, but
                // a `MemberRef` on the instantiated `TypeSpec` for a generic one
                // (`List<int>::_tag` etc.) — see `EmittedUnion.Typars` (P3d.4).
                let tagRef =
                    if List.isEmpty u.Typars then
                        u.TagField
                    else
                        env.Provider.GenericUnionMemberRef(typeName, tyArgs, UnionMember.Tag)

                // Skip the arm unless `scrut._tag = case.Tag`.
                b.Add(ILInstr.Ldloc scrutSlot)
                b.Add(ILInstr.Ldfld tagRef)
                b.Add(ILInstr.LdcI4 c.Tag)
                b.Add(ILInstr.BneUn nextLabel)

                // Extract each non-wildcard field into a fresh local, then test
                // its sub-pattern (a named sub-pattern just aliases that local).
                subPats
                |> List.iteri (fun i subPat ->
                    match subPat with
                    | TPat.Wildcard _ -> ()
                    | _ ->
                        let fieldRef =
                            if List.isEmpty u.Typars then
                                c.Fields.[i]
                            else
                                env.Provider.GenericUnionMemberRef(typeName, tyArgs, UnionMember.Field(caseName, i))

                        let fldSlot = b.Local(typeOfPat subPat)
                        b.Add(ILInstr.Ldloc scrutSlot)
                        b.Add(ILInstr.Ldfld fieldRef)
                        b.Add(ILInstr.Stloc fldSlot)
                        buildMatchTest env b fldSlot nextLabel subPat
                )
            | false, _ -> failwithf "Emit: no emitted union for match on '%s'" typeName
        | TPat.Record(fields, ty) ->
            // A record pattern never fails on shape (no tag to compare): for each
            // named sub-pattern, `ldfld` the field into a fresh local and recurse
            // — only the sub-patterns themselves can branch to `nextLabel`. A
            // wildcard sub-pattern is skipped (it would always match), exactly
            // like the union arm above.
            let typeName, tyArgs =
                match ty with
                | TyRecord(n, xs) -> n, xs
                | other -> failwithf "Emit: record pattern with non-record type %A" other

            match env.Records.TryGetValue typeName with
            | true, r ->
                for (fieldName, subPat) in fields do
                    match subPat with
                    | TPat.Wildcard _ -> ()
                    | _ ->
                        match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                        | Some(_, handle, _) ->
                            let fieldRef =
                                if List.isEmpty r.Typars then
                                    handle
                                else
                                    env.Provider.GenericRecordMemberRef(typeName, tyArgs, RecordMember.Field fieldName)

                            let fldSlot = b.Local(typeOfPat subPat)
                            b.Add(ILInstr.Ldloc scrutSlot)
                            b.Add(ILInstr.Ldfld fieldRef)
                            b.Add(ILInstr.Stloc fldSlot)
                            buildMatchTest env b fldSlot nextLabel subPat
                        | None -> failwithf "Emit: record '%s' has no field '%s'" typeName fieldName
            | false, _ -> failwithf "Emit: no emitted record for pattern on '%s'" typeName
        | other -> failwithf "Emit: match pattern is out of scope: %A" other

    /// The fallthrough a `match` reaches when no arm matched — `throw new
    /// System.Exception("…")`. An exhaustive match never reaches it at runtime,
    /// but it keeps the emitted IL well-formed (and gives a non-exhaustive one
    /// defined behaviour).
    let private buildMatchFailure (env: EmitEnv) (b: IlBuilder) : unit =
        b.Add(ILInstr.Ldstr(env.Ctx.UserString "The match cases were incomplete"))
        b.Add(ILInstr.Newobj(env.Provider.ExceptionCtor, 1))
        b.Add ILInstr.Throw

    /// Resolve the member-call handle for an instance access on `receiverTy`
    /// (P3d.3, generalised to generic unions in R2). A monomorphic union uses the
    /// member's `Def` token directly; a *generic* union goes through a `MemberRef`
    /// on the receiver's instantiated `TypeSpec` (`List<int>::get_Head`). Only
    /// emitted unions carry members today (records / concrete classes are P3e), so
    /// a non-union receiver is a gap.
    let private resolveInstanceMember (env: EmitEnv) (receiverTy: SemType) (name: string) : EntityHandle =
        let typeName, tyArgs =
            match receiverTy with
            | TyUnion(n, xs)
            | TyRecord(n, xs) -> n, xs
            | other -> failwithf "Emit: member '%s' access on non-union receiver %A" name other

        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    env.Provider.GenericUnionMemberRef(
                        typeName,
                        tyArgs,
                        UnionMember.Member(m.MetaName, false, m.ParamTys, m.RetTy)
                    )
            | false, _ -> failwithf "Emit: union '%s' has no emitted member '%s'" typeName name
        | false, _ -> failwithf "Emit: no emitted union for member access on '%s'" typeName

    /// The static-member equivalent. Generic-union *static* augmentation members
    /// are out of scope in R2 (a static member's typars aren't tied to the type's
    /// via `this`, so the front-end leaves them un-remapped — the type's generic
    /// `Cons` / `Empty` come from its case factories instead), so a generic union
    /// fails here loudly rather than minting a malformed `Def` call.
    let private resolveStaticMember (env: EmitEnv) (typeName: string) (name: string) : EntityHandle =
        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    failwithf
                        "Emit: generic-union static augmentation member '%s.%s' is out of scope (R2)"
                        typeName
                        name
            | false, _ -> failwithf "Emit: union '%s' has no emitted static member '%s'" typeName name
        | false, _ -> failwithf "Emit: no emitted union for static member access on '%s'" typeName

    /// Resolve a record field by name on a `TyRecord` receiver to its emit
    /// handle and declared type. A monomorphic record returns the field's `Def`
    /// token; a *generic* record returns a `MemberRef` on the receiver's
    /// instantiated `TypeSpec` (`Box<int>::Value`) — the records-plan §B3 mirror
    /// of `resolveInstanceMember` for unions. A referenced-assembly record
    /// (records-plan §B7) goes through the provider's
    /// `TryResolveExternalRecordField`. The receiver must be a record (the
    /// front end has already routed non-record field access elsewhere).
    let private resolveRecordField (env: EmitEnv) (receiverTy: SemType) (fieldName: string) : EntityHandle * SemType =
        let typeName, tyArgs =
            match receiverTy with
            | TyRecord(n, xs) -> n, xs
            | other -> failwithf "Emit: field '%s' access on non-record receiver %A" fieldName other

        match env.Records.TryGetValue typeName with
        | true, r ->
            match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
            | Some(_, h, ty) ->
                let handle =
                    if List.isEmpty r.Typars then
                        h
                    else
                        env.Provider.GenericRecordMemberRef(typeName, tyArgs, RecordMember.Field fieldName)

                handle, ty
            | None -> failwithf "Emit: record '%s' has no field '%s'" typeName fieldName
        | false, _ ->
            match env.Provider.TryResolveExternalRecordField(typeName, tyArgs, fieldName) with
            | ValueSome(handle, ty) -> handle, ty
            | ValueNone -> failwithf "Emit: no emitted record for field access on '%s'" typeName

    /// `failwith "msg"` resolves through the symbol provider to this name; the
    /// backend lowers it to a BCL-only `throw new System.Exception(msg)` (P3d.3),
    /// rather than the FSharp.Core `Operators.FailWith` recipe.
    let private isFailwith (name: string) : bool =
        name = "failwith" || name.EndsWith ".failwith" || name.EndsWith "FailWith"

    /// The cold printf path (`printfn "%A"` …). Its `PrintFormatLine` recipe leaves
    /// an FSharp.Core `FSharpFunc` printer on the stack, so the printer is applied
    /// via `FSharpFunc::Invoke`, not `Vesper.Fun::Invoke` (R1 leaves this FSharp.Core
    /// island alone until the printf engine — handoff §R9).
    let private isColdPrintf (name: string) : bool =
        name = "printfn" || name.EndsWith ".printfn"

    let rec private buildExpr (env: EmitEnv) (b: IlBuilder) (e: TExpr) : unit =
        match e with
        | TExpr.Const(TConstValue.String s, _) -> b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
        | TExpr.Const(TConstValue.Int n, _) -> b.Add(ILInstr.LdcI4 n)
        | TExpr.Const(TConstValue.Bool v, _) -> b.Add(ILInstr.LdcI4(if v then 1 else 0))
        | TExpr.Const(TConstValue.Byte n, _) -> b.Add(ILInstr.LdcI4(int n))
        | TExpr.Const(TConstValue.Float x, _) -> b.Add(ILInstr.LdcR8 x)
        | TExpr.Const(TConstValue.Char c, _) -> b.Add(ILInstr.LdcI4(int c))
        | TExpr.Const(TConstValue.Decimal d, _) ->
            // Materialise via `Decimal..ctor(lo, mid, hi, isNegative, scale)` from
            // the value's bit representation — the same shape F#/Roslyn emit.
            let bits = System.Decimal.GetBits d
            let flags = bits.[3]
            b.Add(ILInstr.LdcI4 bits.[0]) // lo
            b.Add(ILInstr.LdcI4 bits.[1]) // mid
            b.Add(ILInstr.LdcI4 bits.[2]) // hi
            b.Add(ILInstr.LdcI4(if flags < 0 then 1 else 0)) // sign (high bit of flags)
            b.Add(ILInstr.LdcI4((flags >>> 16) &&& 0xFF)) // scale
            b.Add(ILInstr.Newobj(env.Provider.DecimalCtor, 5))
        | TExpr.Const(TConstValue.Unit, _) ->
            // `()` literal — the unit value is `Unit`'s null (encodeTypeCore
            // maps `unit` to `FSharp.Core.Unit`, whose canonical value is
            // `null`). Pushed when a closure invocation needs a unit arg
            // (`c ()`) or a unit value is otherwise reified — F3 (Phase 2 §1
            // mkCounter pattern).
            b.Add ILInstr.Ldnull

        | TExpr.Var(binding, _) -> buildVarLoad env b binding

        | TExpr.Let(TPat.NamedSimple(binding, ty), value, body, _) ->
            let slot = b.Local ty
            env.Slots.[binding] <- slot
            buildExpr env b value
            b.Add(ILInstr.Stloc slot)
            buildExpr env b body
        | TExpr.Let(pat, _, _, _) -> failwithf "Emit: destructuring let-binding is out of scope: %A" pat

        | TExpr.Sequential(items, _) ->
            // Every item but the last is a unit-typed statement: emit it and
            // discard whatever value it leaves (popping back to the pre-item
            // depth); the last item leaves the sequence's result.
            let n = List.length items

            items
            |> List.iteri (fun i it ->
                if i = n - 1 then
                    buildExpr env b it
                else
                    let baseDepth = b.Depth
                    buildExpr env b it

                    while b.Depth > baseDepth do
                        b.Add ILInstr.Pop
            )

        | TExpr.IfThenElse(cond, thenExpr, elseExpr, _) ->
            // `<cond>; brfalse else; <then>; br end; else: <else>; end:`. Both
            // arms leave one value; the builder's linear depth tracker (which
            // follows only the then-arm) is reset to the post-`brfalse` base
            // before the else-arm so subsequent statement-discards stay correct —
            // the *buffer's* merge depths are re-derived by `IlIr.analyze`.
            let elseLabel = b.Label()
            let endLabel = b.Label()
            buildExpr env b cond
            b.Add(ILInstr.Brfalse elseLabel)
            let baseDepth = b.Depth
            buildExpr env b thenExpr
            b.Add(ILInstr.Br endLabel)
            b.SetDepth baseDepth
            b.Add(ILInstr.Mark elseLabel)
            buildExpr env b elseExpr
            b.Add(ILInstr.Mark endLabel)

        | TExpr.Match(scrutinee, arms, _) ->
            // Evaluate the scrutinee once into a local, then test each arm in
            // order: on a mismatch branch to the next arm; on a match (and a
            // passing guard) emit the body and branch to the shared end. The
            // builder's depth tracker is reset to the post-scrutinee base before
            // each arm and before the end label (every body leaves one result);
            // `IlIr.analyze` re-derives the buffer's merge depths.
            let scrutSlot = b.Local(typeOfExpr scrutinee)
            buildExpr env b scrutinee
            b.Add(ILInstr.Stloc scrutSlot)
            let baseDepth = b.Depth
            let endLabel = b.Label()

            for arm in arms do
                let nextLabel = b.Label()
                buildMatchTest env b scrutSlot nextLabel arm.Pat

                match arm.Guard with
                | Some g ->
                    buildExpr env b g
                    b.Add(ILInstr.Brfalse nextLabel)
                | None -> ()

                buildExpr env b arm.Body
                b.Add(ILInstr.Br endLabel)
                b.SetDepth baseDepth
                b.Add(ILInstr.Mark nextLabel)

            buildMatchFailure env b
            b.SetDepth(baseDepth + 1)
            b.Add(ILInstr.Mark endLabel)

        | TExpr.Lambda _ ->
            // A function value: construct its closure. Captures are pushed via
            // the *current* resolver (a local in `Main`, the param or a capture
            // inside an enclosing closure), then `newobj` its ctor.
            //
            // A *generic* closure (function-representation-plan §Generic closures, C3) routes the `Newobj`
            // through a `MemberRef` on `<closure>$n<args>`, where `args` is the
            // closure's typars zonked at the call site (`!!i` inside the
            // enclosing static method's body, `!i` inside an enclosing closure's
            // `Invoke`) — both encodings reference the same TypeVar roots, and
            // the parent's `TypeSpec` captures the use-site instantiation.
            match env.ClosureByNode.TryGetValue e with
            | true, closure ->
                for (k, _) in closure.Captures do
                    buildVarLoad env b k

                let ctorHandle =
                    if List.isEmpty closure.Typars then
                        match env.CtorHandleByNode.TryGetValue e with
                        | true, ctor -> ctor
                        | false, _ ->
                            failwith "Emit: closure constructor not yet emitted (leaves-first ordering broken)"
                    else
                        env.Provider.GenericClosureMemberRef(
                            closure.Name,
                            closure.Typars |> List.map TyVar,
                            ClosureMember.Ctor
                        )

                b.Add(ILInstr.Newobj(ctorHandle, List.length closure.Captures))
            | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

        | TExpr.New(className, args, ty) ->
            for a in args do
                buildExpr env b a

            let tyArgs =
                match ty with
                | TyClass(_, xs) -> xs
                | _ -> []

            match env.Provider.TryEmitCtor(className, tyArgs) with
            | ValueSome recipe -> b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
            | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className

        | TExpr.App _ ->
            let head, spineArgs = collectSpine [] e

            match head with
            | TExpr.External(name, _, _) when isFailwith name ->
                // `failwith "msg"` → `ldstr msg; newobj System.Exception(string);
                // throw`. BCL-only (the provider's `ExceptionCtor`), and terminal
                // — `throw` ends the path, so it tolerates a value position the
                // way a non-exhaustive `match` fallthrough does (P3d.3).
                match spineArgs with
                | (arg, _) :: _ ->
                    buildExpr env b arg
                    b.Add(ILInstr.Newobj(env.Provider.ExceptionCtor, 1))
                    b.Add ILInstr.Throw
                | [] -> failwith "Emit: failwith with no argument"
            | TExpr.External(name, _, _) ->
                // The recipe reads its generic instantiation from the head's
                // full curried type (`fnTy`).
                match env.Provider.TryEmitCall(name, typeOfExpr head) with
                | ValueSome recipe ->
                    let leading, rest = List.splitAt recipe.ArgCount spineArgs

                    for (a, _) in leading do
                        buildExpr env b a

                    b.Add(ILInstr.Recipe recipe)

                    // Whatever the recipe left on the stack — a function value
                    // the rest of the spine is applied to.
                    let funcTy =
                        match List.tryLast leading with
                        | Some(_, ty) -> ty
                        | None -> typeOfExpr head

                    // The cold printf printer is an FSharp.Core `FSharpFunc`, so it
                    // is applied via `FSharpFunc::Invoke`; every other recipe result
                    // is a native `Vesper.Fun` (R1).
                    if isColdPrintf name then
                        foldInvokeFSharpFunc env b funcTy rest
                    else
                        foldInvoke env b funcTy rest
                | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

            | TExpr.Var(k, _) when env.StaticMethods.ContainsKey k ->
                // A top-level function emitted as a static method (P3b): `call`
                // it with the first `Arity` args (always present — a non-saturated
                // use would have escaped to a closure, see `collectStaticFns`),
                // then `Invoke` the result with any remainder. A *generic* static
                // method (R3) `call`s a `MethodSpec` instantiating it — recovered
                // by matching its declared parameter types against the actual
                // argument types (recursion yields the method's own typars ⇒ `!!i`).
                let sm = env.StaticMethods.[k]
                let leading, rest = List.splitAt sm.Arity spineArgs

                for (a, _) in leading do
                    buildExpr env b a

                let callHandle =
                    if List.isEmpty sm.Typars then
                        sm.Handle
                    else
                        // Each spine arg's *own* type (`collectSpine` pairs it with
                        // the application's *result* type instead), matched against
                        // the declared parameter types to recover the instantiation.
                        let actualTys = leading |> List.map (fun (a, _) -> typeOfExpr a)
                        let inst = matchInstantiation sm.Typars sm.ParamTys actualTys
                        env.Provider.StaticFnMethodSpec(sm.Handle, inst)

                b.Add(ILInstr.Call(callHandle, sm.Arity, 1))
                foldInvoke env b sm.ResultTy rest

            | TExpr.ExternalMember(receiver, key, name, false, memberTy) ->
                // An external instance/static *method* call (P4): push the receiver
                // (instance only) beneath the arguments, then `call` (static) /
                // `callvirt` (instance) the keyed member ref. A .NET method is
                // **tupled** (`m(a, b)` = one application to `(a, b)`), so the call
                // consumes a single spine element — the argument list — and the
                // parameter count comes from the chosen key's `argSig` length
                // (authoritative: `memberTy` alone can't tell a flattened 2-param
                // method from a genuine single `(int*int)` param — type-args-bug.md
                // Layer 3). A literal `TExpr.Tuple` argument is pushed element-wise
                // (no tuple object is constructed).
                let isStatic = ValueOption.isNone receiver

                let argCount =
                    match key with
                    | SymbolKey.MemberKey(_, _, argSig, _) -> List.length argSig
                    | other -> failwithf "Emit: ExternalMember key is not a MemberKey: %A" other

                // The method consumes one spine element (its argument list); any
                // remainder is further application of the result (rare).
                let argList, rest =
                    match spineArgs with
                    | first :: more -> ValueSome first, more
                    | [] -> ValueNone, []

                match receiver with
                | ValueSome r -> buildExpr env b r
                | ValueNone -> ()

                let pushedArgs =
                    match argList with
                    | ValueNone -> 0 // no argument supplied (a 0-param method)
                    | ValueSome(argExpr, _) ->
                        if argCount >= 2 then
                            match argExpr with
                            | TExpr.Tuple(elems, _) when List.length elems = argCount ->
                                for el in elems do
                                    buildExpr env b el

                                argCount
                            | _ ->
                                failwithf
                                    "Emit: external member '%s' expects %d tupled arguments but the argument is not a literal %d-tuple"
                                    name
                                    argCount
                                    argCount
                        elif argCount = 1 then
                            buildExpr env b argExpr
                            1
                        else
                            // argCount = 0: a `unit → ret` method; the lone arg is
                            // `()`, which has no IL value to push.
                            0

                let handle = env.Provider.ExternalMemberRef(key, false, isStatic, zonk memberTy)
                let total = (if isStatic then 0 else 1) + pushedArgs

                if isStatic then
                    b.Add(ILInstr.Call(handle, total, 1))
                else
                    b.Add(ILInstr.Callvirt(handle, total, 1))

                // A method returning a function value applied further (rare): the
                // result type is the consumed `App` node's type.
                let resultTy =
                    match argList with
                    | ValueSome(_, ty) -> ty
                    | ValueNone -> typeOfExpr head

                foldInvoke env b resultTy rest

            | _ ->
                // The head is itself a function value (a closure local or a
                // partially applied result): emit it, then `Invoke` each arg.
                buildExpr env b head
                foldInvoke env b (typeOfExpr head) spineArgs

        | TExpr.RecordCons(srcFields, ty) ->
            // The source-order initialiser list (`{ Y = …; X = … }`) is reordered
            // to the type's *declaration* order before the ctor is invoked
            // (records-plan §B3): the ctor's parameter slots correspond to
            // declaration order so the field-store sequence in `buildRecordCtor`
            // lines up. A generic record's `.ctor` is a `MemberRef` on its own
            // `TypeSpec` (`Box\`1<!0>::.ctor`), exactly like a generic union's
            // factory.
            let typeName, tyArgs =
                match ty with
                | TyRecord(n, xs) -> n, xs
                | other -> failwithf "Emit: RecordCons with non-record type %A" other

            match env.Records.TryGetValue typeName with
            | true, r ->
                let srcMap = Map.ofList srcFields

                for (fieldName, _, _) in r.Fields do
                    match Map.tryFind fieldName srcMap with
                    | Some e -> buildExpr env b e
                    | None ->
                        failwithf
                            "Emit: record literal for '%s' is missing initialiser for field '%s'"
                            typeName
                            fieldName

                let ctor =
                    if List.isEmpty r.Typars then
                        r.Ctor
                    else
                        env.Provider.GenericRecordMemberRef(typeName, tyArgs, RecordMember.Ctor)

                b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
            | false, _ ->
                // Records-handoff Phase 2 follow-up F2: the record lives in a
                // referenced assembly (`Vesper.Ref\`1` in `Vesper.Core.dll`,
                // routed here from `RefCellPromotion`). The provider mints a
                // `MemberRef` on its instantiated `TypeSpec`; field arguments are
                // pushed in source order (the contract layer's field order is
                // also the declaration order, which matches the ctor's parameter
                // layout, so no reorder is required for the supported one-field
                // `Ref<'T>` shape — multi-field external records will revisit).
                let fieldNames = [ for (n, _) in srcFields -> n ]

                match env.Provider.TryEmitRecordCons(typeName, tyArgs, fieldNames) with
                | ValueSome recipe ->
                    for (_, e) in srcFields do
                        buildExpr env b e

                    b.Add(ILInstr.Newobj(recipe.Handle, recipe.ArgCount))
                | ValueNone -> failwithf "Emit: no emitted record for '%s'" typeName

        | TExpr.FieldGet(receiver, name, _) ->
            // `r.X` — load the receiver and `ldfld` the field. The field handle is
            // a `Def` token for a monomorphic record, a `MemberRef` on the receiver's
            // `TypeSpec` for a generic one (`resolveRecordField`). A
            // referenced-assembly record (F2) routes through the provider.
            let handle, _ = resolveRecordField env (typeOfExpr receiver) name
            buildExpr env b receiver
            b.Add(ILInstr.Ldfld handle)

        | TExpr.FieldSet(receiver, name, value, _) ->
            // `r.X <- v` on a `mutable` field. Validation has rejected the
            // immutable case before we reach here. `stfld` consumes both pushes
            // and leaves nothing on the stack, but a `FieldSet` is *unit-typed*
            // — every consumer (`Sequential` middle items, the body of a
            // unit-returning closure / static method) expects a unit value to be
            // present. Push `ldnull` (Unit's value) to match F#'s emission and
            // keep the IL verifier happy when the body is just a FieldSet
            // (`fun () -> n <- n + 1`, F3 §1).
            let handle, _ = resolveRecordField env (typeOfExpr receiver) name
            buildExpr env b receiver
            buildExpr env b value
            b.Add(ILInstr.Stfld handle)
            b.Add ILInstr.Ldnull // unit value

        | TExpr.RecordClone(source, overrides, ty) ->
            // `{ r with X = v; … }` — evaluate `r` into a local, then per
            // declaration-order field: push the override expression if it's in
            // the override list, else `ldloc; ldfld` from the saved source. Then
            // `newobj` the ctor. Direct field reads (no `MemberwiseClone`) keeps
            // it BCL-only and works identically for a generic record.
            let typeName, tyArgs =
                match ty with
                | TyRecord(n, xs) -> n, xs
                | other -> failwithf "Emit: RecordClone with non-record type %A" other

            match env.Records.TryGetValue typeName with
            | true, r ->
                let overrideMap = Map.ofList overrides
                let srcSlot = b.Local ty
                buildExpr env b source
                b.Add(ILInstr.Stloc srcSlot)

                for (fieldName, handle, _) in r.Fields do
                    match Map.tryFind fieldName overrideMap with
                    | Some e -> buildExpr env b e
                    | None ->
                        let fieldRef =
                            if List.isEmpty r.Typars then
                                handle
                            else
                                env.Provider.GenericRecordMemberRef(typeName, tyArgs, RecordMember.Field fieldName)

                        b.Add(ILInstr.Ldloc srcSlot)
                        b.Add(ILInstr.Ldfld fieldRef)

                let ctor =
                    if List.isEmpty r.Typars then
                        r.Ctor
                    else
                        env.Provider.GenericRecordMemberRef(typeName, tyArgs, RecordMember.Ctor)

                b.Add(ILInstr.Newobj(ctor, List.length r.Fields))
            | false, _ -> failwithf "Emit: no emitted record for '%s'" typeName

        | TExpr.UnionCons(caseName, args, ty) ->
            let typeName, tyArgs =
                match ty with
                | TyRecord(n, xs)
                | TyUnion(n, xs) -> n, xs
                | other -> failwithf "Emit: UnionCons with non-union type %A" other

            for a in args do
                buildExpr env b a

            match env.Unions.TryGetValue typeName with
            | true, u ->
                // Our own emitted union: `call` the case's static factory (the
                // fields are already on the stack in declaration order). A
                // monomorphic factory is a `Def` token; a generic one is a
                // `MemberRef` on the instantiated `TypeSpec` (`List<int>::Cons`).
                let factoryRef =
                    if List.isEmpty u.Typars then
                        u.Cases.[caseName].Factory
                    else
                        env.Provider.GenericUnionMemberRef(typeName, tyArgs, UnionMember.Factory caseName)

                b.Add(ILInstr.Call(factoryRef, List.length args, 1))
            | false, _ ->
                // The provider's special-case (FSharp.Core list) for `[]` / `::`.
                match env.Provider.TryEmitUnionCons(typeName, caseName, tyArgs) with
                | ValueSome recipe -> b.Add(ILInstr.Recipe recipe)
                | ValueNone -> failwithf "Emit: no union-cons recipe for %s.%s" typeName caseName

        | TExpr.PropertyGet(receiver, name, _) ->
            // Instance property read (P3d.3): load the receiver, `call` the
            // union's `get_<name>` (the receiver is its sole argument). On a
            // generic union the call goes through a `MemberRef` on the receiver's
            // `TypeSpec` (`List<int>::get_Head`) (R2).
            let handle = resolveInstanceMember env (typeOfExpr receiver) name
            buildExpr env b receiver
            b.Add(ILInstr.Call(handle, 1, 1))

        | TExpr.MethodCall(receiver, name, args, _) ->
            // Instance method call (P3d.3): receiver then args, `call` the
            // member (non-virtual — the union is sealed).
            let handle = resolveInstanceMember env (typeOfExpr receiver) name
            buildExpr env b receiver

            for a in args do
                buildExpr env b a

            b.Add(ILInstr.Call(handle, 1 + List.length args, 1))

        | TExpr.StaticPropertyGet(className, name, _) ->
            let handle = resolveStaticMember env className name
            b.Add(ILInstr.Call(handle, 0, 1))

        | TExpr.StaticMethodCall(className, name, args, _) ->
            let handle = resolveStaticMember env className name

            for a in args do
                buildExpr env b a

            b.Add(ILInstr.Call(handle, List.length args, 1))

        | TExpr.ExternalMember(receiver, key, _, true, ty) ->
            // A standalone external *property* get (P4): a static one (`call
            // get_<name>()`) or an instance one reached as the receiver of an outer
            // access (`<receiver>; callvirt get_<name>()`). The keyed member ref is
            // minted from the node's `SymbolKey` (`ExternalMemberRef`).
            let isStatic = ValueOption.isNone receiver
            let handle = env.Provider.ExternalMemberRef(key, true, isStatic, zonk ty)

            match receiver with
            | ValueNone -> b.Add(ILInstr.Call(handle, 0, 1))
            | ValueSome r ->
                buildExpr env b r
                b.Add(ILInstr.Callvirt(handle, 1, 1))

        | TExpr.ExternalMember(_, _, _, false, _) ->
            // An external method used as a first-class value (a method group, not
            // applied) needs closure synthesis — out of scope. Applied methods are
            // handled as an `App` head above.
            failwith "Emit: external method used as a first-class value is out of scope"

        | TExpr.Format(sink, segments, _) -> buildFormat env b sink segments

        | TExpr.ILIntrinsic(opCode, args, _) ->
            // Push each operand, then append the mapped opcode. The dispatch
            // (which opcode for which operator/primitive) lives in the operator
            // `.fs` body this node was lowered from, not here — codegen only
            // interprets the IL. See docs/operators-plan.md.
            for a in args do
                buildExpr env b a

            match Cil.tryOpCodeOfMnemonic opCode with
            | ValueSome code ->
                match List.length args with
                | 2 -> b.Add(ILInstr.Bin code)
                | 1 -> b.Add(ILInstr.Un code)
                | n -> failwithf "Emit: %d-ary inline-IL instruction '%s' is out of scope" n opCode
            | ValueNone -> failwithf "Emit: unsupported inline-IL instruction '%s'" opCode

        | TExpr.StaticOptimization(_, def, _) ->
            // Reaching codegen unresolved means the function was never
            // inline-expanded against a concrete operand type (used as a
            // first-class value, or declared without `inline`). F#'s semantics
            // fall back to the leading (dynamic) expression in that case.
            buildExpr env b def

        | other -> failwithf "Emit: unsupported expression: %A" other

    /// Lower a `TExpr.Format` to the `Vesper.Formatter` write-through handler: a
    /// ref-struct local constructed in place, then each segment folded
    /// left-to-right (`AppendLiteral` for a literal run, `AppendFormatted<T>`
    /// for a hole — its arg evaluated *here*, at its position), then a trailing
    /// newline (printfn-style sinks) and flush, or `ToStringAndClear` for the
    /// string sink. The node yields a value: the `unit` (null) of the writing
    /// sinks, or the result string of `sprintf`. Not a `CallRecipe` — the recipe
    /// model can't interleave literals/args around a ref-struct local + sink.
    and private buildFormat (env: EmitEnv) (b: IlBuilder) (sink: FormatSink) (segments: EqArray<FormatSeg>) : unit =
        let fh = env.Provider.FormatHandles()
        let slot = b.Local fh.HandlerLocal

        // Capacity hints for the ctor; the handler grows past them as needed, so
        // they need not be exact.
        let mutable litLen = 0
        let mutable holeCount = 0

        for seg in segments do
            match seg with
            | FormatSeg.Lit s -> litLen <- litLen + s.Length
            | FormatSeg.Hole _ -> holeCount <- holeCount + 1

        // Construct in place: `ldloca h; ldc litLen; ldc holeCount; <sink?>; call .ctor`.
        b.Add(ILInstr.Ldloca slot)
        b.Add(ILInstr.LdcI4 litLen)
        b.Add(ILInstr.LdcI4 holeCount)

        match sink with
        | FormatSink.ToString -> b.Add(ILInstr.Call(fh.CtorString, 3, 0))
        | FormatSink.ToStdOut _ ->
            b.Add(ILInstr.Call(fh.ConsoleOut, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToStdErr _ ->
            b.Add(ILInstr.Call(fh.ConsoleError, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToWriter w ->
            buildExpr env b w
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

        for seg in segments do
            match seg with
            | FormatSeg.Lit s ->
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSeg.Hole(hole, arg) ->
                match hole.Kind with
                | PrintfSpec.HoleKind.Formatted ->
                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg

                    // Push optional args in the C# parameter order: alignment, then format.
                    match hole.Alignment with
                    | Some a -> b.Add(ILInstr.LdcI4 a)
                    | None -> ()

                    match hole.Format with
                    | Some f -> b.Add(ILInstr.Ldstr(env.Ctx.UserString f))
                    | None -> ()

                    let handle = fh.AppendFormatted(hole.Ty, hole.Alignment.IsSome, hole.Format.IsSome)

                    let argc =
                        2
                        + (if hole.Alignment.IsSome then 1 else 0)
                        + (if hole.Format.IsSome then 1 else 0)

                    b.Add(ILInstr.Call(handle, argc, 0))

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

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.LdcI4(defaultArg hole.Alignment 0))
                    b.Add(ILInstr.Call(handle, 3, 0))

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

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.Ldstr(env.Ctx.UserString fmt))
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(fh.AppendZeroPaddedFloat, 4, 0))

        match sink with
        | FormatSink.ToString ->
            // Leaves the built string on the stack (the `sprintf` result).
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.ToStringAndClear, 1, 1))
        | FormatSink.ToStdOut nl
        | FormatSink.ToStdErr nl ->
            if nl then
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString "\n"))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))

            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            b.Add ILInstr.Ldnull // unit value
        | FormatSink.ToWriter _ ->
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            b.Add ILInstr.Ldnull // unit value
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

    /// Apply each remaining argument to the function value on the stack via
    /// `FSharpFunc.Invoke`, threading the running function type.
    and private foldInvoke (env: EmitEnv) (b: IlBuilder) (funcTy0: SemType) (args: (TExpr * SemType) list) : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitInvoke funcTy with
            | ValueSome recipe ->
                buildExpr env b arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to value of type %A" funcTy

    /// Apply a curried FSharp.Core `FSharpFunc` value (the cold printf printer)
    /// argument by argument via `FSharpFunc::Invoke` — the FSharpFunc twin of
    /// `foldInvoke` (R1; retargeted with the printf engine, handoff §R9).
    and private foldInvokeFSharpFunc
        (env: EmitEnv)
        (b: IlBuilder)
        (funcTy0: SemType)
        (args: (TExpr * SemType) list)
        : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitFSharpFuncInvoke funcTy with
            | ValueSome recipe ->
                buildExpr env b arg
                b.Add(ILInstr.Recipe recipe)
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to FSharpFunc value of type %A" funcTy

    /// Emit an expression as a statement: evaluate it and discard any value.
    let private buildStatement (env: EmitEnv) (b: IlBuilder) (e: TExpr) : unit =
        buildExpr env b e

        while b.Depth > 0 do
            b.Add ILInstr.Pop

    /// Build the `Main` body from the *lowered* decls. Each top-level `let`
    /// binds a `Main` local — except a function lowered to a static method (P3b),
    /// which has no value here; each effectful expression is emitted in source
    /// order; then `ldc.i4.0; ret`. (Inline bindings were removed by `lower`.)
    let buildMain (ctx: EmitContext) (decls: TDecl list) : ILBody =
        let b = IlBuilder()

        let env =
            {
                Provider = ctx.Provider
                Ctx = ctx.Ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = ctx.ClosureByNode
                CtorHandleByNode = ctx.CtorHandleByNode
                Args = Dictionary<NodeKey, int>()
                SelfKey = ValueNone
                CaptureFields = Dictionary<NodeKey, EntityHandle>()
                Unions = ctx.Unions
                Records = ctx.Records
                StaticMethods = ctx.StaticMethods
            }

        for d in decls do
            match d with
            | TDecl.Expression(e, _) -> buildStatement env b e
            // A function emitted as a static method has no Main local.
            | TDecl.Let(TPat.NamedSimple(binding, _), _, _, _) when ctx.StaticMethods.ContainsKey binding -> ()
            | TDecl.Let(TPat.NamedSimple(binding, _), value, _, ty) ->
                let slot = b.Local ty
                env.Slots.[binding] <- slot
                buildExpr env b value
                b.Add(ILInstr.Stloc slot)
            | TDecl.Let _ -> ()
            | TDecl.Type _ -> ()

        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// Build a closure's `Invoke` body: evaluate its (lowered) body under a
    /// resolver mapping the parameter to `ldarg.1` and each capture to its
    /// field, leaving the result on the stack, then `ret`.
    let buildClosureInvoke
        (ctx: EmitContext)
        (closure: Closure)
        (captureFields: Dictionary<NodeKey, EntityHandle>)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<NodeKey, int>()
        args.[closure.ParamKey] <- 1 // `this` is 0; the single applied parameter is 1

        let env =
            {
                Provider = ctx.Provider
                Ctx = ctx.Ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = ctx.ClosureByNode
                CtorHandleByNode = ctx.CtorHandleByNode
                Args = args
                SelfKey = closure.SelfKey
                CaptureFields = captureFields
                Unions = ctx.Unions
                Records = ctx.Records
                StaticMethods = ctx.StaticMethods
            }

        buildExpr env b closure.Body
        b.Add ILInstr.Ret
        b.Body

    /// Build a static-method function's body (P3b): bind each flattened
    /// parameter to its `ldarg` index (a static method has no `this`, so the
    /// first parameter is `ldarg.0`), evaluate the body leaving its result on the
    /// stack, then `ret`. A recursive self-call resolves to a direct `call`
    /// through `staticMethods` (the `App` arm), so no self-binding is needed.
    let buildStaticMethod (ctx: EmitContext) (fn: StaticFn) : ILBody =
        let b = IlBuilder()
        let args = Dictionary<NodeKey, int>()
        fn.Params |> List.iteri (fun i (k, _) -> args.[k] <- i)

        let env =
            {
                Provider = ctx.Provider
                Ctx = ctx.Ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = ctx.ClosureByNode
                CtorHandleByNode = ctx.CtorHandleByNode
                Args = args
                SelfKey = ValueNone
                CaptureFields = Dictionary<NodeKey, EntityHandle>()
                Unions = ctx.Unions
                Records = ctx.Records
                StaticMethods = ctx.StaticMethods
            }

        buildExpr env b fn.Body
        b.Add ILInstr.Ret
        b.Body

    /// Build a union augmentation member's body (P3d.3). An instance member's
    /// `this` is `ldarg.0` (`thisKey`), its parameters `ldarg.1…`; a static
    /// member's parameters start at `ldarg.0`. The body leaves its result on the
    /// stack, then `ret`. Member bodies don't synthesise closures (the closure
    /// discovery pass walks only value/expression decls), so an empty
    /// closure/ctor map is passed.
    let buildMember
        (ctx: EmitContext)
        (thisKey: NodeKey voption)
        (prms: (NodeKey * SemType) list)
        (body: TExpr)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<NodeKey, int>()

        let baseIdx =
            match thisKey with
            | ValueSome k ->
                args.[k] <- 0 // `this`
                1
            | ValueNone -> 0

        prms |> List.iteri (fun i (k, _) -> args.[k] <- baseIdx + i)

        let env =
            {
                Provider = ctx.Provider
                Ctx = ctx.Ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = ctx.ClosureByNode
                CtorHandleByNode = ctx.CtorHandleByNode
                Args = args
                SelfKey = ValueNone
                CaptureFields = Dictionary<NodeKey, EntityHandle>()
                Unions = ctx.Unions
                Records = ctx.Records
                StaticMethods = ctx.StaticMethods
            }

        buildExpr env b body
        b.Add ILInstr.Ret
        b.Body

    /// Build a closure's `.ctor` body: chain to the `FSharpFunc\`2` base ctor,
    /// then store each capture argument into its field.
    let buildClosureCtor (baseCtor: EntityHandle) (fields: EntityHandle list) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Call(baseCtor, 1, 0)) // call instance void base::.ctor()

        fields
        |> List.iteri (fun i field ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldarg(i + 1))
            b.Add(ILInstr.Stfld field)
        )

        b.Add ILInstr.Ret
        b.Body

    /// Build a union case's static factory body: allocate via the union's
    /// parameterless ctor, stamp the discriminant `tag`, store each factory
    /// parameter into its field, and return the object. `fieldHandles` are in
    /// declaration order = the factory's parameter order (static `ldarg.i`).
    let buildUnionFactory
        (unionCtor: EntityHandle)
        (tag: int)
        (tagField: EntityHandle)
        (fieldHandles: EntityHandle list)
        : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Newobj(unionCtor, 0))
        b.Add ILInstr.Dup
        b.Add(ILInstr.LdcI4 tag)
        b.Add(ILInstr.Stfld tagField)

        fieldHandles
        |> List.iteri (fun i field ->
            b.Add ILInstr.Dup
            b.Add(ILInstr.Ldarg i)
            b.Add(ILInstr.Stfld field)
        )

        b.Add ILInstr.Ret
        b.Body

    // ---- Structural equality / hashing for a monomorphic union (C-Eq1) ----

    /// The resolved handles a monomorphic union's synthesised `Equals(object)` /
    /// `GetHashCode()` bodies need. Codegen builds this from the concrete
    /// `ClrProvider`; the bodies below stay decoupled from how the BCL refs are
    /// minted (the per-field-type recipes are passed as functions).
    ///
    /// **Why a flat field walk works.** Every value is built through a case
    /// factory (`emitUnionFactory`), which sets only its *own* case's payload
    /// fields; a DU is immutable, so a field belonging to any other case is
    /// always its default. So once the tags match, comparing / hashing *every*
    /// field (not just the active case's) is equivalent to the §5.2 per-case
    /// walk, and needs no `_tag` switch — fewer branches, same result.
    type UnionEqualitySupport =
        {
            /// The union's own `TypeDefinition` — the `isinst` target.
            SelfType: EntityHandle
            /// `TyUnion(name, [])` — the type of the cast `other` local.
            SelfSemType: SemType
            TagField: EntityHandle
            /// `(field handle, field type)` across every case, declaration order.
            Fields: (EntityHandle * SemType) list
            /// `int` — the tag's type, for `HashCode.Add<int>`.
            IntType: SemType
            /// `EqualityComparer<T>.Default` getter for a field type.
            ComparerDefault: SemType -> EntityHandle
            /// `EqualityComparer<T>::Equals(T, T) : bool` for a field type.
            ComparerEquals: SemType -> EntityHandle
            /// The `System.HashCode` value-type local.
            HashCodeLocal: SemType
            /// `HashCode::Add<T>(T)` for a field/tag type.
            HashCodeAdd: SemType -> EntityHandle
            /// `HashCode::ToHashCode() : int`.
            HashCodeToHashCode: EntityHandle
        }

    /// The §5.2 tag-then-field comparison shared by both equality entry points
    /// (the `Equals(object)` override and the typed `IEquatable<Self>::Equals`):
    /// `this` is `ldarg.0`, `other` is pushed by `loadOther` (already a non-null
    /// `Self`). Tags must match, then each field via `EqualityComparer<F>.Default`
    /// (the §3.2 rule — total, so a `float` field gets `NaN = NaN` in this
    /// structural context, O7). Any mismatch branches to `falseLabel`; on
    /// fall-through the operands are equal.
    let private buildTagAndFieldEquality
        (s: UnionEqualitySupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (falseLabel: int)
        : unit =
        // if (this._tag != other._tag) return false;
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        loadOther b
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.BneUn falseLabel)

        // per field: if (!comparer.Equals(this.F, other.F)) return false;
        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1)) // EqualityComparer<F>.Default
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerEquals fieldTy, 3, 1)) // .Equals(this.F, other.F)
            b.Add(ILInstr.Brfalse falseLabel)

    /// `override bool Equals(object obj)` for a monomorphic union: `obj is Self`
    /// (also rejects `null`), then the shared tag/field walk. Any failure jumps to
    /// the shared `false` tail (whose merge depth `IlIr.analyze` derives — no
    /// manual `SetDepth`).
    let buildUnionEquals (s: UnionEqualitySupport) : ILBody =
        let b = IlBuilder()
        let other = b.Local s.SelfSemType
        let falseLabel = b.Label()

        // other = obj as Self;  if (other == null) return false;
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst s.SelfType)
        b.Add(ILInstr.Stloc other)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Brfalse falseLabel)

        buildTagAndFieldEquality s b (fun b -> b.Add(ILInstr.Ldloc other)) falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        // The `false` tail: every branch above merges here at depth 0.
        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals` a
    /// monomorphic union implements (C-Eq1). `other` (`ldarg.1`) is already `Self`,
    /// so no `isinst` — just a `null` guard, then the same tag/field walk. This is
    /// the boxing-free path `EqualityComparer<Self>.Default` (now a
    /// `GenericEqualityComparer`, since the union declares `IEquatable<Self>`)
    /// reaches, so it — not `Equals(object)` — is the one a nested DU field
    /// recurses through.
    let buildUnionEqualsTyped (s: UnionEqualitySupport) : ILBody =
        let b = IlBuilder()
        let falseLabel = b.Label()

        // if (other == null) return false;
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse falseLabel)

        buildTagAndFieldEquality s b (fun b -> b.Add(ILInstr.Ldarg 1)) falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        // The `false` tail: every branch above merges here at depth 0.
        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `override int GetHashCode()` for a monomorphic union: a `System.HashCode`
    /// accumulator seeded with the `_tag`, then every field added through it
    /// (`HashCode.Add<T>` itself routes through `EqualityComparer<T>.Default`, so
    /// it is the same §3.2 rule), then `ToHashCode()`. Equal values hash equal:
    /// the tag distinguishes cases and inactive-case fields are uniformly default.
    let buildUnionGetHashCode (s: UnionEqualitySupport) : ILBody =
        let b = IlBuilder()
        let hc = b.Local s.HashCodeLocal

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.Call(s.HashCodeAdd s.IntType, 2, 0))

        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Ldloca hc)
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Call(s.HashCodeAdd fieldTy, 2, 0))

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Call(s.HashCodeToHashCode, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    // ---- Record emission (records-plan §B2/B4) ----

    /// Build a record's `.ctor` body: chain to `Object::.ctor()`, then store
    /// each ctor argument into the matching field (declaration order ⇒ argument
    /// `ldarg.(i+1)` ⇒ field handle `fields.[i]`). Structurally identical to
    /// `buildClosureCtor`, but named separately so call sites read as record vs
    /// closure emission.
    let buildRecordCtor (baseCtor: EntityHandle) (fields: EntityHandle list) : ILBody = buildClosureCtor baseCtor fields

    // ---- Structural equality / hashing for a record (records-plan §B4) ----

    /// The record-shaped analogue of `UnionEqualitySupport`: every field is
    /// compared / hashed via `EqualityComparer<F>.Default` / `HashCode.Add<F>`
    /// (the §3.2 rule), but there is **no `_tag`** to compare or seed — a record
    /// is one nameless "case", so the union walk minus the tag is the record
    /// triple. Fields are declaration-order (same store/read order the ctor
    /// uses). Both generic and monomorphic records share this support shape; the
    /// caller mints the field handles as `Def` tokens or `MemberRef`s on the
    /// type's own `TypeSpec` (`Box\`1<!0>::Value`).
    type RecordEqualitySupport =
        {
            /// The record's own `TypeDefinition` — the `isinst` target.
            SelfType: EntityHandle
            /// `TyRecord(name, …)` — the type of the cast `other` local.
            SelfSemType: SemType
            /// `(field handle, field type)` in declaration order.
            Fields: (EntityHandle * SemType) list
            ComparerDefault: SemType -> EntityHandle
            ComparerEquals: SemType -> EntityHandle
            HashCodeLocal: SemType
            HashCodeAdd: SemType -> EntityHandle
            HashCodeToHashCode: EntityHandle
        }

    /// The field-by-field comparison shared by both record equality entry
    /// points (the `Equals(object)` override and the typed
    /// `IEquatable<Self>::Equals`): same shape as `buildTagAndFieldEquality`
    /// minus the leading tag compare. Any field mismatch branches to
    /// `falseLabel`; on fall-through the operands are field-wise equal.
    let private buildRecordFieldEquality
        (s: RecordEqualitySupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (falseLabel: int)
        : unit =
        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerEquals fieldTy, 3, 1))
            b.Add(ILInstr.Brfalse falseLabel)

    /// `override bool Equals(object obj)` for a record: `obj is Self` (also
    /// rejects null), then the shared field-by-field walk. Same structure as
    /// `buildUnionEquals` minus the tag compare.
    let buildRecordEquals (s: RecordEqualitySupport) : ILBody =
        let b = IlBuilder()
        let other = b.Local s.SelfSemType
        let falseLabel = b.Label()

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst s.SelfType)
        b.Add(ILInstr.Stloc other)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Brfalse falseLabel)

        buildRecordFieldEquality s b (fun b -> b.Add(ILInstr.Ldloc other)) falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals` the
    /// record implements. `other` (`ldarg.1`) is already `Self`, so a `null`
    /// guard suffices; then the same field walk. This is the boxing-free path
    /// `EqualityComparer<Self>.Default` reaches once the record declares
    /// `IEquatable<Self>` — so a nested record-typed field recurses through it.
    let buildRecordEqualsTyped (s: RecordEqualitySupport) : ILBody =
        let b = IlBuilder()
        let falseLabel = b.Label()

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse falseLabel)

        buildRecordFieldEquality s b (fun b -> b.Add(ILInstr.Ldarg 1)) falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `override int GetHashCode()` for a record: a `System.HashCode`
    /// accumulator with every field added through `HashCode.Add<T>`, then
    /// `ToHashCode()`. No tag seed — a record has one shape.
    let buildRecordGetHashCode (s: RecordEqualitySupport) : ILBody =
        let b = IlBuilder()
        let hc = b.Local s.HashCodeLocal

        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Ldloca hc)
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Call(s.HashCodeAdd fieldTy, 2, 0))

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Call(s.HashCodeToHashCode, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    // ---- Structural comparison for a union (records-plan §B6) ----
    //
    // Mirror of `UnionEqualitySupport` minus the hashing surface (no
    // `HashCode.Add` for comparison) and plus the comparer/IComparable handles
    // a `CompareTo(Self)` body and the `CompareTo(object)` boxing entry need.
    //
    // The per-field walk uses `Comparer<F>.Default.Compare(this.F, other.F)`,
    // returning the first non-zero result (lexicographic). The union compares
    // tags first via `sub` — tag values are small case indices, so subtraction
    // is safe. Same flat walk as equality: every payload field is compared in
    // declaration order regardless of active case, sound because inactive-case
    // fields are always default (per `emitUnionFactory`).

    type UnionComparisonSupport =
        {
            /// The union's own `TypeDefinition` — the `isinst` target the
            /// `CompareTo(object)` boxing entry uses to cast and type-check.
            SelfType: EntityHandle
            /// `TyUnion(name, …)` — the type of the cast `other` local and the
            /// param type of the typed `CompareTo(Self)`.
            SelfSemType: SemType
            TagField: EntityHandle
            /// `(field handle, field type)` across every case, declaration order.
            Fields: (EntityHandle * SemType) list
            /// `Comparer<T>.Default` getter for a field type.
            ComparerDefault: SemType -> EntityHandle
            /// `Comparer<T>::Compare(T, T) : int32` for a field type.
            ComparerCompare: SemType -> EntityHandle
            /// `System.ArgumentException::.ctor(string)` — the
            /// `CompareTo(object)` body throws this on a non-`Self` arg.
            ArgumentExceptionCtor: EntityHandle
            /// `UserStringHandle` for the `"Object type mismatch"` literal the
            /// `CompareTo(object)` body pushes onto the stack. Codegen mints
            /// this via `ctx.UserString` before building the support struct
            /// (the builder owns no metadata context).
            MismatchMessage: UserStringHandle
        }

    /// The §5.2 tag-then-field lex comparison shared by both entry points (the
    /// `CompareTo(object)` override and the typed `IComparable<Self>::CompareTo`):
    /// `this` is `ldarg.0`, `other` is loaded by `loadOther` (already non-null
    /// `Self`). The first non-zero result is left in `cLocal` and `brtrue`-ed to
    /// `returnLabel`; on fall-through every comparison returned 0.
    let private buildTagAndFieldComparison
        (s: UnionComparisonSupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (cLocal: int)
        (returnLabel: int)
        : unit =
        // c = this._tag - other._tag;  if (c != 0) goto return;
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        loadOther b
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.Bin ILOpCode.Sub)
        b.Add(ILInstr.Stloc cLocal)
        b.Add(ILInstr.Ldloc cLocal)
        b.Add(ILInstr.Brtrue returnLabel)

        // per field: c = Comparer<F>.Default.Compare(this.F, other.F);
        //           if (c != 0) goto return;
        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerCompare fieldTy, 3, 1))
            b.Add(ILInstr.Stloc cLocal)
            b.Add(ILInstr.Ldloc cLocal)
            b.Add(ILInstr.Brtrue returnLabel)

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo`
    /// the union implements (records-plan §B6). A `null` `other` sorts
    /// before any non-null value (brainstorm-comparison §5.3, matching BCL
    /// convention), so this returns `1` in that case; otherwise the shared tag/
    /// field lex walk. The walk stores its current `c` in a local and branches
    /// to a shared return label as soon as `c != 0`; on fall-through every
    /// comparison was equal, so it returns `0`.
    let buildUnionCompareTo (s: UnionComparisonSupport) : ILBody =
        let b = IlBuilder()
        let c = b.Local(TyConst "int")
        let nullLabel = b.Label()
        let returnLabel = b.Label()

        // if (other == null) return 1;
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)

        buildTagAndFieldComparison s b (fun b -> b.Add(ILInstr.Ldarg 1)) c returnLabel

        // Fall-through: every comparison returned 0.
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        // First non-zero result is in `c`.
        b.Add(ILInstr.Mark returnLabel)
        b.Add(ILInstr.Ldloc c)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body

    /// `int CompareTo(object obj)` — the non-generic
    /// `IComparable::CompareTo(object)` entry the union implements. Matches
    /// F#'s convention: `null` sorts first (returns `1`), a non-`Self` argument
    /// throws `ArgumentException`, otherwise delegate to the typed
    /// `CompareTo(Self)`. Uses `isinst` + a `Self`-typed local to avoid an
    /// explicit `castclass` (same pattern `buildUnionEquals` uses).
    let buildUnionCompareToObj (s: UnionComparisonSupport) (typedCompareTo: EntityHandle) : ILBody =
        let b = IlBuilder()
        let other = b.Local s.SelfSemType
        let nullLabel = b.Label()
        let throwLabel = b.Label()

        // if (obj == null) return 1;
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)
        // other = obj as Self;  if (other == null) goto throw;
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst s.SelfType)
        b.Add(ILInstr.Stloc other)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Brfalse throwLabel)
        // return this.CompareTo(other);
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Call(typedCompareTo, 2, 1))
        b.Add ILInstr.Ret
        // throw new ArgumentException("Object type mismatch");
        b.Add(ILInstr.Mark throwLabel)
        b.Add(ILInstr.Ldstr s.MismatchMessage)
        b.Add(ILInstr.Newobj(s.ArgumentExceptionCtor, 1))
        b.Add ILInstr.Throw
        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body

    // ---- Structural comparison for a record (records-plan §B6) ----

    /// Mirror of `RecordEqualitySupport` for the comparison pair. Same shape
    /// as `UnionComparisonSupport` minus the tag — a record is one nameless
    /// "case", so the union walk minus the tag compare is the record pair.
    type RecordComparisonSupport =
        {
            SelfType: EntityHandle
            SelfSemType: SemType
            /// `(field handle, field type)` in declaration order.
            Fields: (EntityHandle * SemType) list
            ComparerDefault: SemType -> EntityHandle
            ComparerCompare: SemType -> EntityHandle
            ArgumentExceptionCtor: EntityHandle
            MismatchMessage: UserStringHandle
        }

    /// The field-by-field lex comparison shared by both record `CompareTo`
    /// entry points. Same shape as `buildTagAndFieldComparison` minus the
    /// leading tag compare. The first non-zero result is stored in `cLocal`
    /// and branched to `returnLabel`; on fall-through every field was equal.
    let private buildRecordFieldComparison
        (s: RecordComparisonSupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (cLocal: int)
        (returnLabel: int)
        : unit =
        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerCompare fieldTy, 3, 1))
            b.Add(ILInstr.Stloc cLocal)
            b.Add(ILInstr.Ldloc cLocal)
            b.Add(ILInstr.Brtrue returnLabel)

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo`
    /// the record implements. `null` `other` sorts before any non-null value
    /// (returns `1`); otherwise the shared field lex walk.
    let buildRecordCompareTo (s: RecordComparisonSupport) : ILBody =
        let b = IlBuilder()
        let c = b.Local(TyConst "int")
        let nullLabel = b.Label()
        let returnLabel = b.Label()

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)

        buildRecordFieldComparison s b (fun b -> b.Add(ILInstr.Ldarg 1)) c returnLabel

        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark returnLabel)
        b.Add(ILInstr.Ldloc c)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body

    /// `int CompareTo(object obj)` — the non-generic
    /// `IComparable::CompareTo(object)` entry the record implements. Same
    /// shape as `buildUnionCompareToObj` (the record's
    /// `RecordComparisonSupport` and the union's `UnionComparisonSupport`
    /// share the relevant fields here — `SelfType` / `ArgumentExceptionCtor`
    /// / `MismatchMessage`).
    let buildRecordCompareToObj (s: RecordComparisonSupport) (typedCompareTo: EntityHandle) : ILBody =
        let b = IlBuilder()
        let other = b.Local s.SelfSemType
        let nullLabel = b.Label()
        let throwLabel = b.Label()

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst s.SelfType)
        b.Add(ILInstr.Stloc other)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Brfalse throwLabel)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Call(typedCompareTo, 2, 1))
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark throwLabel)
        b.Add(ILInstr.Ldstr s.MismatchMessage)
        b.Add(ILInstr.Newobj(s.ArgumentExceptionCtor, 1))
        b.Add ILInstr.Throw
        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body
