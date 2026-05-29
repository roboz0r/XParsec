namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitExpr

/// The codegen TAST walker, split across modules: `EmitLower` (inline expansion
/// + External-as-value eta-reification), `EmitClosures` (closure / static-method
/// discovery), `EmitExpr` (`TExpr` -> IL via the depth-tracked `Cil` helpers),
/// and this `Emit` (the method/body builders codegen calls). The shared data
/// types live in `EmitTypes`. This module re-exports the public surface of the
/// helper modules so callers keep using `Emit.*`.
module Emit =

    type Closure = EmitTypes.Closure
    type EmittedCase = EmitTypes.EmittedCase
    type EmittedMember = EmitTypes.EmittedMember
    type EmittedUnion = EmitTypes.EmittedUnion
    type EmittedRecord = EmitTypes.EmittedRecord
    type EmittedClass = EmitTypes.EmittedClass
    type StaticFn = EmitTypes.StaticFn
    type StaticMethodRef = EmitTypes.StaticMethodRef

    let zonk = EmitLower.zonk
    let expandBuiltinOps = EmitLower.expandBuiltinOps
    let spliceExternalInlinesInExpr = EmitLower.spliceExternalInlinesInExpr
    let lowerWith = EmitLower.lowerWith
    let lower = EmitLower.lower
    let collectStaticFns = EmitClosures.collectStaticFns
    let staticFnTypars = EmitClosures.staticFnTypars
    let discoverClosures = EmitClosures.discoverClosures

    /// The run-wide registries every builder needs: the provider seam, the
    /// metadata writer, and the shared dictionaries that resolve a `Lambda` value
    /// to its emitted closure, its `.ctor` handle, the nominal type tables, and a
    /// top-level function to a direct `call`. Per-method state is layered on top
    /// inside each builder as an `EmitEnv`.
    type EmitContext =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            ClosureByNode: Dictionary<TExpr, Closure>
            CtorHandleByNode: Dictionary<TExpr, EntityHandle>
            Unions: Dictionary<string, EmittedUnion>
            Records: Dictionary<string, EmittedRecord>
            Classes: Dictionary<string, EmittedClass>
            StaticMethods: Dictionary<NodeKey, StaticMethodRef>
        }

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
                Classes = ctx.Classes
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
                Classes = ctx.Classes
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
                Classes = ctx.Classes
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
        (prms: EqArray<NodeKey * SemType>)
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

        prms |> EqArray.iteri (fun i (k, _) -> args.[k] <- baseIdx + i)

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
                Classes = ctx.Classes
                StaticMethods = ctx.StaticMethods
            }

        buildExpr env b body
        b.Add ILInstr.Ret
        b.Body

    /// Build a secondary constructor body (vesper-set-sprint-plan §1.9 / B-11):
    /// run the `let`-preamble into locals, then chain to the primary `.ctor`
    /// (`ldarg.0; <primaryArgs>; call instance void Self::.ctor`). There is no
    /// base-ctor call — the primary ctor performs it. `this` is `ldarg.0`; the
    /// overload's parameters are `ldarg.1…`. The body never synthesises closures,
    /// so an empty closure/ctor map is passed (as `buildMember`).
    let buildSecondaryCtor
        (ctx: EmitContext)
        (prms: EqArray<NodeKey * SemType>)
        (lets: TCtorLet list)
        (primaryCtor: EntityHandle)
        (primaryArgs: TExpr list)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<NodeKey, int>()
        prms |> EqArray.iteri (fun i (k, _) -> args.[k] <- 1 + i)

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
                Classes = ctx.Classes
                StaticMethods = ctx.StaticMethods
            }

        for l in lets do
            let slot = b.Local l.Type
            env.Slots.[l.Binder] <- slot
            buildExpr env b l.Init
            b.Add(ILInstr.Stloc slot)

        b.Add(ILInstr.Ldarg 0)

        for a in primaryArgs do
            buildExpr env b a

        b.Add(ILInstr.Call(primaryCtor, List.length primaryArgs + 1, 0))
        b.Add ILInstr.Ret
        b.Body

    /// Build a class `.cctor` body for its `static let`s (vesper-set-sprint-plan
    /// §1.8 / B-10): evaluate each initialiser in declaration order and `stsfld`
    /// it into its backing field, then `ret`. The body sees no `this` / params
    /// (a `.cctor` is parameterless), so the env mirrors `buildMember`'s static
    /// path with empty arg/slot maps.
    let buildStaticCctor (ctx: EmitContext) (lets: (EntityHandle * TExpr) list) : ILBody =
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
                Classes = ctx.Classes
                StaticMethods = ctx.StaticMethods
            }

        for (field, init) in lets do
            buildExpr env b init
            b.Add(ILInstr.Stsfld field)

        b.Add ILInstr.Ret
        b.Body

    /// Build a closure's `.ctor` body: chain to the `FSharpFunc\`2` base ctor,
    /// then store each capture argument into its field.
    let buildClosureCtor (baseCtor: EntityHandle) (fields: EntityHandle list) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Call(baseCtor, 1, 0))

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
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        loadOther b
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.BneUn falseLabel)

        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerEquals fieldTy, 3, 1))
            b.Add(ILInstr.Brfalse falseLabel)

    /// `override bool Equals(object obj)` for a monomorphic union: `obj is Self`
    /// (also rejects `null`), then the shared tag/field walk. Any failure jumps to
    /// the shared `false` tail (whose merge depth `IlIr.analyze` derives — no
    /// manual `SetDepth`).
    let buildUnionEquals (s: UnionEqualitySupport) : ILBody =
        let b = IlBuilder()
        let other = b.Local s.SelfSemType
        let falseLabel = b.Label()

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst s.SelfType)
        b.Add(ILInstr.Stloc other)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Brfalse falseLabel)

        buildTagAndFieldEquality s b (fun b -> b.Add(ILInstr.Ldloc other)) falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
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

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse falseLabel)

        buildTagAndFieldEquality s b (fun b -> b.Add(ILInstr.Ldarg 1)) falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
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

    /// Build a record's `.ctor` body: chain to `Object::.ctor()`, then store each
    /// ctor argument into the matching field. Structurally identical to
    /// `buildClosureCtor`, named separately so call sites read as record emission.
    let buildRecordCtor (baseCtor: EntityHandle) (fields: EntityHandle list) : ILBody = buildClosureCtor baseCtor fields

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

    /// Comparison support for a monomorphic union: like `UnionEqualitySupport`
    /// minus hashing, plus the `Comparer`/`IComparable` handles the `CompareTo`
    /// bodies need. Tags are compared first via `sub` (small case indices, so
    /// safe), then each field via `Comparer<F>.Default.Compare`, returning the
    /// first non-zero result (lexicographic). Same flat walk as equality:
    /// inactive-case fields are always default (per the case factory).
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
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        loadOther b
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.Bin ILOpCode.Sub)
        b.Add(ILInstr.Stloc cLocal)
        b.Add(ILInstr.Ldloc cLocal)
        b.Add(ILInstr.Brtrue returnLabel)

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

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)

        buildTagAndFieldComparison s b (fun b -> b.Add(ILInstr.Ldarg 1)) c returnLabel

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
