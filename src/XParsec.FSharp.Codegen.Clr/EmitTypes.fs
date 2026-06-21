namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

module EmitTypes =

    /// One `FSharpFunc\`2<ParamTy, ResultTy>` subclass. `Node` is matched by
    /// reference identity in the *lowered* tree shared by discovery and emission.
    /// `Captures` order = field order = ctor-arg order = order pushed at the
    /// construction site.
    type Closure =
        {
            Node: Frozen.TExpr
            Name: string
            ParamKey: NodeKey
            ParamTy: FrozenType
            /// The closure's parameter pattern. A `NamedSimple` / unit `Const`
            /// binds `ldarg.1` directly through `ParamKey`; a `Tuple` pattern
            /// (`fun (a, b) -> …`) is destructured out of the `ldarg.1`
            /// `ValueTuple`n` value by `bindPattern` before the body runs, so
            /// `ParamKey` is a synthetic placeholder for that slot.
            ParamPat: Frozen.TPat
            ResultTy: FrozenType
            Body: Frozen.TExpr
            Captures: (NodeKey * FrozenType) list
            /// Binding key of the `let [rec] f = <this lambda>` this is the value
            /// of. A recursive self-reference resolves to `this` (`ldarg.0`), so
            /// it is not captured. `ValueNone` for an anonymous lambda.
            SelfKey: NodeKey voption
            /// `> 0` ⇒ a *generic* closure: the *total* number of typars this
            /// closure's `TypeDefinition` carries (`GenericParam` rows `T0…`). For a
            /// static-fn closure this is the enclosing method's typar count, all
            /// method-axis. For a *member-body* closure it is `DeclaringTypars` (the
            /// enclosing class typars) + the member's own method typars. Its
            /// signatures encode the body's typars onto the
            /// closure class's `!i` (via `ClrEnv.ClosureTyparScope`); the
            /// construction site `Newobj`s a `MemberRef` on the instantiated
            /// `TypeSpec`.
            Typars: int
            /// The closure's declaring-typar offset: its first `DeclaringTypars`
            /// slots are the enclosing class's typars (a member-body closure on a
            /// generic class). `0` for a static-fn closure (all typars method-axis).
            /// Drives the `ClosureTyparScope` offset and the construction-site
            /// instantiation split (declaring-axis args, then method-axis).
            DeclaringTypars: int
            /// Stack vs heap representation, decided from the Regions verdict
            /// (Axis 1 `LocalStack` ∧ Axis 2 `StackOnlyEligible`) and snapshotted
            /// onto `TastFile.ClosureReprs`. `Heap` = the v1 reference-type
            /// `Vesper.Fun<_,_>` subclass; `Stack` = the Regions verdict that a
            /// readonly-struct shape is *admissible*. This is the front-end SNAPSHOT
            /// — a *necessary* condition for the value-struct lowering, NOT the
            /// codegen trigger (which is the stricter `IsValueStruct` gate below).
            /// On its own it remains inert (it changes no IL).
            Repr: ClosureRepr
            /// rung-4 Step C: the CODEGEN decision to emit this closure as a
            /// zero-alloc value-struct (`System.ValueType` base, by-value
            /// construction, constrained-slot `!TF` override). `true` for a
            /// monomorphic, *anonymous* lambda threaded through a constrained `Fun`
            /// slot — captureless (M1, `initobj`) or capturing (M2, value-type ctor).
            /// Distinct from `Repr`: the front-end `Stack` verdict is necessary but
            /// not sufficient, so this is the single source of truth for the struct
            /// path.
            IsValueStruct: bool
            /// rung-4 M3: the FLAT `FunN` arity this closure implements. `1` (the
            /// default / M1+M2 path) is the single-arg `Vesper.Fun<P,R>` interface
            /// with `Invoke(P):R`. `2` is the flat `Vesper.Fun2<P1,P2,R>` interface
            /// with one flat `Invoke(P1,P2):R` — the curried 2-arg source lambda
            /// `fun x y -> …` peeled so the inner arrow is NOT a separate closure.
            /// Driven by the node-keyed verdict (`TastFile.FunVerdicts`); only a
            /// value-struct closure (`IsValueStruct`) is ever arity > 1 today.
            FunArity: int
            /// The SECOND flat parameter for an arity-2 (`Fun2`) closure: its binder
            /// key, type, and pattern (peeled from the inner `Lambda`). `ValueNone`
            /// for the arity-1 path. The closure's `Invoke` binds `Param2` to
            /// `ldarg.2`.
            Param2: (NodeKey * FrozenType * Frozen.TPat) voption
        }

    /// A non-capturing (`Captures` empty), monomorphic (`Typars = 0`) closure is
    /// STATELESS — a single shared instance suffices, so it is cached in a
    /// `static readonly` singleton field on the closure type itself, `newobj`'d once
    /// in the closure's `.cctor`, and every construction site `ldsfld`s it instead of
    /// allocating (rung-4 Step B; fsc's no-capture-closure caching). A capturing
    /// closure differs per construction (caching would be wrong), and a generic one
    /// needs a per-instantiation singleton (deferred) — both keep `newobj`.
    /// A value-struct (rung-4 Step C) closure is constructed by-value (`initobj`),
    /// never cached as a heap singleton — the two paths are mutually exclusive.
    /// Only the heap non-capturing monomorphic closure caches (Step B).
    let closureIsCached (c: Closure) : bool =
        List.isEmpty c.Captures && c.Typars = 0 && not c.IsValueStruct

    /// One case of an emitted union: runtime `Tag`, the static factory
    /// `TExpr.UnionCons` `call`s, and its payload field handles in declaration order.
    type EmittedCase =
        {
            Tag: int
            Factory: EntityHandle
            Fields: EntityHandle list
        }

    /// An augmentation member on a union/class `TypeDefinition`. A property's
    /// `Handle` is its `get_<name>` method; `Arity` excludes `this`. `Handle` is
    /// the `Def` token (monomorphic); a generic type reaches the member through a
    /// `MemberRef` on the instantiated `TypeSpec` built from `MetaName` + signature.
    type EmittedMember =
        {
            Handle: EntityHandle
            IsStatic: bool
            Arity: int
            MetaName: string
            ParamTys: FrozenType list
            RetTy: FrozenType
            /// The member's *own* generic-method typar count (`member s.Map<'U> …`).
            /// 0 for the common non-generic member. > 0 ⇒ the member-ref must carry
            /// the `GENERIC` header and the call site a `MethodSpec`.
            MethodTyparCount: int
        }

    /// A union emitted into this assembly. `Typars` empty ⇒ a monomorphic union
    /// (single sealed class, `Def`-token member access); non-empty ⇒ a generic
    /// union whose members are reached via `ICodegenProvider.GenericUnionMemberRef`
    /// (a `MemberRef` on the instantiated `TypeSpec`) rather than the `Def`-token
    /// `TagField` / `EmittedCase.Factory` / `EmittedCase.Fields`. `Name` is the
    /// registry key the provider mints refs against.
    type EmittedUnion =
        {
            Name: string
            Typars: string list
            TagField: EntityHandle
            Cases: Dictionary<string, EmittedCase>
            /// Augmentation members keyed by source name. A name maps to a *list*
            /// of overloads (declaration order — the type's own members first,
            /// interface-impl members last), so an overloaded member
            /// (`AppendFormatted(value:'T)` / `(value:'T, alignment:int)` / …) keeps
            /// every signature; the call site disambiguates by argument types
            /// (`EmitResolve.pickOverload`). A single-element list is the common,
            /// non-overloaded case.
            Members: Dictionary<string, EmittedMember list>
        }

    /// A record emitted into this assembly: a sealed class, one public field per
    /// record field, one ctor taking the fields in declaration order. `Fields` is
    /// `(name, handle, declared type)` in declaration order. `Typars` empty ⇒
    /// monomorphic (`Def`-token handles); non-empty ⇒ generic, reached via
    /// `ICodegenProvider.GenericRecordMemberRef`.
    type EmittedRecord =
        {
            Name: string
            Typars: string list
            Fields: (string * EntityHandle * FrozenType) list
            Ctor: EntityHandle
        }

    /// A class emitted into this assembly. Same `Members` shape as
    /// `EmittedUnion`, so `resolveInstanceMember` / `resolveStaticMember` extend to
    /// classes unchanged. `Typars` empty ⇒ monomorphic; non-empty ⇒ generic,
    /// reached via `ICodegenProvider.UserGenericMemberRef (ClassMember _)`.
    type EmittedClass =
        {
            Name: string
            Typars: string list
            /// Primary-constructor backing fields, `(name, handle, type)` in
            /// declaration order. Its *length* is the primary ctor's arity (a
            /// `TExpr.New` matches against it), so explicit `val` fields are kept
            /// out of it — they live in `InstanceFields`.
            Fields: (string * EntityHandle * FrozenType) list
            /// Explicit `val [mutable] x: T` instance fields,
            /// `(name, handle, type)`. Default-initialised (not set by the primary
            /// ctor); a `this.x` `FieldGet`/`FieldSet` resolves its handle here.
            InstanceFields: (string * EntityHandle * FrozenType) list
            /// `true` for a `[<Struct>]` value type — drives `isValueType` at use
            /// sites (box on `:>`, `unbox.any` on `:?>`).
            IsValueType: bool
            Ctor: EntityHandle
            /// `true` when a synthesised primary `.ctor` was emitted (so `Ctor` is a
            /// real primary handle a `TExpr.New` of the field arity may target). For
            /// the `val`-field form (`type T = val …; new(…) = …`) with secondary
            /// ctors this is `false`: there is no primary, `Ctor` aliases the first
            /// secondary, and every construction resolves to a secondary by arity.
            HasPrimaryCtor: bool
            /// Augmentation members keyed by source name. A name maps to a *list*
            /// of overloads (declaration order — the type's own members first,
            /// interface-impl members last), so an overloaded member
            /// (`AppendFormatted(value:'T)` / `(value:'T, alignment:int)` / …) keeps
            /// every signature; the call site disambiguates by argument types
            /// (`EmitResolve.pickOverload`). A single-element list is the common,
            /// non-overloaded case.
            Members: Dictionary<string, EmittedMember list>
            /// `static let` backing fields keyed by source name; a
            /// `TExpr.StaticFieldGet` resolves its `ldsfld` handle here. A mono class
            /// stores the field `Def` token, a generic class a `MemberRef` on the
            /// open self-`TypeSpec`.
            StaticFields: Dictionary<string, EntityHandle>
            /// Secondary constructors keyed by arity → (declared param types,
            /// `.ctor` handle). A `TExpr.New` whose arg count differs from the
            /// primary's selects the matching overload here. A monomorphic class
            /// uses the `Def` handle directly; a generic one mints a `MemberRef` on
            /// the instantiated `TypeSpec` from the param types (in declaring-typar
            /// markers).
            SecondaryCtors: (int * FrozenType list * EntityHandle) list
            /// rung-4 §9.4 Stage 3 (3b): the implemented-interface `FrozenType`
            /// TEMPLATES, each written over THIS class's declaring typars (its arg
            /// leaves are `FTTypar(TyparAxis.Declaring, i)`). Sourced from
            /// `ClassDecl.Interfaces` (the `fst` of each impl pair). The codegen
            /// analog of the front-end's `info.InterfaceImpls` that
            /// `Engine.subtypeInterfacesOf` instantiates by the receiver's args —
            /// `EmitResolve.tryInterfaceWitness` walks these to recover a phantom
            /// enumerator typar's bound at a call site. Carried but UNREAD until that
            /// Stage-3 solve calls the witness.
            Interfaces: FrozenType list
        }

    /// An interface emitted into this assembly. Only its `Members` matter at use
    /// sites: a method call on an interface-typed receiver (or, later, a
    /// `constrained.` call on an interface-constrained typar) resolves the member
    /// here and `callvirt`s the interface slot (`MethodKey.InterfaceMethod` handle).
    /// Interfaces have no ctor / fields, so — unlike a class — that's all that's
    /// carried. `Typars` empty ⇒ monomorphic. Same `Members` shape as
    /// `EmittedClass`, so `resolveInstanceMember` reuses `pickOverload`.
    type EmittedInterface =
        {
            Name: string
            Typars: string list
            Members: Dictionary<string, EmittedMember list>
        }

    /// A named module holder's identity: `(namespace, holderName)` as recorded in
    /// `TastFile.ModuleMembers`. One static holder class per `module Foo = …`.
    type HolderKey = string option * string

    /// One flattened parameter of a `StaticFn`. A simple binder's `Slot` key is
    /// referenced directly by the body (it resolves to the parameter's `ldarg`
    /// index); a destructuring tuple parameter (`fun (a, b) -> …`) carries
    /// `Pat = Some …` and a synthetic `Slot`, whose `ldarg` value
    /// `buildStaticMethod` spills to a local and `bindPattern`s into the leaf
    /// bindings — exactly as `buildClosureInvoke` does for a tuple closure param.
    /// The type itself is platform-neutral, so it lives in `TastLower`; this alias
    /// keeps the CLR call sites reading `StaticParam`.
    type StaticParam = TastLower.StaticParam

    /// A top-level function lowered to a **static method**: `let [rec] f p0 p1 …`
    /// becomes `static f(p0, p1, …)`, curried parameters flattened. Eligible only
    /// when the function never escapes as a value and captures no module-level
    /// local (see `collectStaticFns`); a recursive self-call is a direct `call`.
    type StaticFn =
        {
            Key: NodeKey
            Name: string
            /// `Some holderKey` when from a named `module Foo = …`: emits as a
            /// public static method on the `Foo` holder type. `None` ⇒ the
            /// anonymous "Program" holder.
            Holder: HolderKey option
            /// The flat, tuple-expanded, lone-unit-erased compiled parameters
            /// (`CompiledForm.Params`): one CLR `ldarg` slot each. A tupled source
            /// group `(x, y)` contributes N flat params (full F# flattening), so
            /// `Params.Length` is the CLR method's parameter count — NOT the number
            /// of source applications a call collapses (that is `Groups.Length`).
            Params: StaticParam list
            /// The SOURCE curried/tupled groups (`ValRepr.Groups`): how many spine
            /// applications a saturated call consumes (`Groups.Length`) and which of
            /// them are tuple groups whose single argument the call site flattens to
            /// N pushed values. Distinct from `Params` because the flat compiled
            /// signature alone cannot tell `f(int,int)` (tupled group) from a genuine
            /// single `(int*int)` param.
            Groups: Frozen.ArgGroup list
            Body: Frozen.TExpr
            ResultTy: FrozenType
            /// `true` when the source result type is `unit` — the method emits as
            /// genuine CLR `void` (full F# fidelity, "void everywhere"), its
            /// body pops the trailing `unit`, and a
            /// value-position call reifies a `unit` after the `call`.
            ReturnsVoid: bool
            /// rung-4 §9 (Direction B): the binding's frozen typar bounds, method-axis-
            /// indexed templates over the method typars (`FrozenConstraint.Coercion`).
            /// Read by the call-site phantom-typar solve (`EmitCall`) to recover a
            /// phantom typar (`fold`'s `'E`) no parameter/result mentions.
            Constraints: FrozenConstraint list
        }

    /// A module-level value (`let x = e` at module scope) lowered to a `public
    /// static` field on its module holder, initialised by the holder's `.cctor`
    ///. Only values on a *named* module classify
    /// (anonymous "Program" values keep their `Main`-local treatment, see
    /// `collectModuleValues`), so the holder is always known. `Init` is the
    /// initialiser the `.cctor` evaluates and `stsfld`s — taken from the lowered
    /// decls, so built-in operators are already expanded.
    type ModuleValue =
        {
            Key: NodeKey
            Name: string
            Ty: FrozenType
            Init: Frozen.TExpr
            Holder: HolderKey
        }

    /// Emission handle + shape of a static-method function, resolved before any
    /// body is built (the `MethodDefinition` handle is predicted from row order).
    /// A call site `f a b` `call`s `Handle` with the first `Arity` args, then
    /// `Invoke`s the result with any remainder. A generic method carries its typar
    /// *count* and declared `ParamTys` (which embed `TyTypar(Method, i)`): the
    /// call site recovers the instantiation by matching `ParamTys` against the
    /// actual argument types by typar index and `call`s a `MethodSpec`. `Typars = 0`
    /// ⇒ monomorphic (a plain `call`).
    type StaticMethodRef =
        {
            Handle: EntityHandle
            /// The flat CLR parameter count (`StaticFn.Params.Length`) — the `call`
            /// instruction's argument count. With tuple flattening this can exceed
            /// the number of source applications a call collapses; the spine split
            /// is driven by `Groups.Length`, not this.
            Arity: int
            /// The SOURCE groups (mirrors `StaticFn.Groups`): `Groups.Length` spine
            /// applications collapse into one `call`, and each tuple group's single
            /// argument is flattened to N pushed values at the call site.
            Groups: Frozen.ArgGroup list
            ResultTy: FrozenType
            Typars: int
            ParamTys: FrozenType list
            /// `true` ⇒ the method is CLR `void`: the `call` declares 0 results and a
            /// value-position consumer reifies a `unit` afterward.
            ReturnsVoid: bool
            /// rung-4 §9 (Direction B): the method's frozen typar bounds (mirrors
            /// `StaticFn.Constraints`), method-axis-indexed templates over the method
            /// typars. Read by the call-site phantom-typar solve (`EmitCall`) to
            /// recover a phantom typar (`fold`'s `'E`) from its bound's seq impl.
            Constraints: FrozenConstraint list
        }

    /// The run-wide registries every builder needs: the provider seam, the
    /// metadata writer, and the shared dictionaries that resolve a `Lambda` value
    /// to its emitted closure, its `.ctor` handle, the nominal type tables, and a
    /// top-level function to a direct `call`. Per-method state is layered on top
    /// inside each builder as an `EmitEnv` (via `EmitEnv.ofContext`).
    type EmitContext =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            ClosureByNode: Dictionary<Frozen.TExpr, Closure>
            CtorHandleByNode: Dictionary<Frozen.TExpr, EntityHandle>
            /// A non-capturing, monomorphic closure's cached `instance` field
            /// (rung-4 Step B): a `Lambda` node here loads its one cached singleton
            /// with `ldsfld` instead of `newobj`'ing per construction.
            CachedClosureFieldByNode: Dictionary<Frozen.TExpr, EntityHandle>
            /// rung-4 Step C (M1): a captureless `Stack` (value-struct) closure
            /// `Lambda` node → its synthetic encodable `FrozenType` (its by-value
            /// local + the constrained-slot `MethodSpec` type-argument) and its
            /// closure-`TypeDef` handle (`initobj` operand).
            ClosureValueTypeByNode: Dictionary<Frozen.TExpr, FrozenType>
            ClosureTypeDefByNode: Dictionary<Frozen.TExpr, EntityHandle>
            Unions: Dictionary<SymbolKey, EmittedUnion>
            Records: Dictionary<SymbolKey, EmittedRecord>
            Classes: Dictionary<SymbolKey, EmittedClass>
            Interfaces: Dictionary<SymbolKey, EmittedInterface>
            StaticMethods: Dictionary<NodeKey, StaticMethodRef>
            /// Module-level value bindings → their emitted `public static` field
            /// (`ldsfld`). Shared by every body builder so a module value resolves
            /// uniformly in any method/ctor/cctor.
            ModuleValues: Dictionary<NodeKey, EntityHandle>
            /// The subset of top-level ("Program") values that are **initialised in
            /// `Main`** via `stsfld` (the trailing values, after a top-level
            /// `do`) → their field handle. `buildMain` emits the store here instead of
            /// allocating a `Main` local; references still read `ldsfld` via
            /// `ModuleValues`. Leading-prefix values are absent (their `.cctor`
            /// initialises them), as are named-holder values.
            MainInitValues: Dictionary<NodeKey, EntityHandle>
        }

    /// Per-method codegen state, layered on top of the run-wide `EmitContext`.
    /// `Slots` maps the current method's locals to slot indices; `Args` maps a
    /// method parameter to its `ldarg` index (closure `Invoke`: `this` 0, param 1;
    /// static method: flattened params 0…N-1; `Main`: none). `SelfKey` is the
    /// recursive self of a closure `Invoke` body (resolved to `this`).
    /// `CaptureFields` resolves a closure's captures.
    type EmitEnv =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            Slots: Dictionary<NodeKey, int>
            ClosureByNode: Dictionary<Frozen.TExpr, Closure>
            CtorHandleByNode: Dictionary<Frozen.TExpr, EntityHandle>
            /// Cached non-capturing closure singleton fields (rung-4 Step B);
            /// a `Lambda` value here `ldsfld`s instead of `newobj`ing.
            CachedClosureFieldByNode: Dictionary<Frozen.TExpr, EntityHandle>
            /// rung-4 Step C (M1) value-struct closures: synthetic encodable
            /// `FrozenType` + closure-`TypeDef` handle per `Stack` `Lambda` node.
            ClosureValueTypeByNode: Dictionary<Frozen.TExpr, FrozenType>
            ClosureTypeDefByNode: Dictionary<Frozen.TExpr, EntityHandle>
            Args: Dictionary<NodeKey, int>
            SelfKey: NodeKey voption
            CaptureFields: Dictionary<NodeKey, EntityHandle>
            Unions: Dictionary<SymbolKey, EmittedUnion>
            Records: Dictionary<SymbolKey, EmittedRecord>
            Classes: Dictionary<SymbolKey, EmittedClass>
            Interfaces: Dictionary<SymbolKey, EmittedInterface>
            StaticMethods: Dictionary<NodeKey, StaticMethodRef>
            /// Module-level values (`let x = e` at module scope), lowered to a
            /// `public static` field on their module holder and resolved here by
            /// binding `NodeKey` → field handle (`ldsfld`).
            ModuleValues: Dictionary<NodeKey, EntityHandle>
        }

    /// `EmitEnv` constructors layering per-method state over the run-wide
    /// `EmitContext`, so a new shared registry is a change here — not in every
    /// builder.
    module EmitEnv =
        /// `args` maps each parameter to its `ldarg` index; locals (`Slots`)
        /// always start empty. `selfKey` / `captureFields` are the
        /// closure-`Invoke` extras — every other builder has neither.
        let create
            (ctx: EmitContext)
            (selfKey: NodeKey voption)
            (captureFields: Dictionary<NodeKey, EntityHandle>)
            (args: Dictionary<NodeKey, int>)
            : EmitEnv =
            {
                Provider = ctx.Provider
                Ctx = ctx.Ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = ctx.ClosureByNode
                CtorHandleByNode = ctx.CtorHandleByNode
                CachedClosureFieldByNode = ctx.CachedClosureFieldByNode
                ClosureValueTypeByNode = ctx.ClosureValueTypeByNode
                ClosureTypeDefByNode = ctx.ClosureTypeDefByNode
                Args = args
                SelfKey = selfKey
                CaptureFields = captureFields
                Unions = ctx.Unions
                Records = ctx.Records
                Classes = ctx.Classes
                Interfaces = ctx.Interfaces
                StaticMethods = ctx.StaticMethods
                ModuleValues = ctx.ModuleValues
            }

        /// The common builder shape: parameters only — no recursive self, no
        /// captures.
        let ofContext (ctx: EmitContext) (args: Dictionary<NodeKey, int>) : EmitEnv =
            create ctx ValueNone (Dictionary()) args

    /// Materialise the `unit` value (`()`) on the stack. `unit` is the zero-field
    /// BCL struct `System.ValueTuple` (its `prim-types-min.fs` binding), not
    /// FSharp.Core's null `Unit`, so the value is reified by zero-initialising a
    /// scratch local: `ldloca; initobj System.ValueTuple; ldloc` (net +1). Every
    /// site that leaves a unit result — `()`, a `for` loop, a `FieldSet`, a
    /// `printfn` flush — funnels through here so the BCL-only representation stays
    /// consistent (and the local's `unit` type encodes off the same repr).
    let buildUnitValue (env: EmitEnv) (b: IlBuilder) : unit =
        let slot = b.Local(FTConst("unit", EqArray.empty))
        b.Add(ILInstr.Ldloca slot)
        b.Add(ILInstr.Initobj(env.Provider.TypeToken(FTConst("unit", EqArray.empty))))
        b.Add(ILInstr.Ldloc slot)
