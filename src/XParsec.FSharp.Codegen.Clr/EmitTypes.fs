namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

module EmitTypes =

    /// One synthesised closure class: a `System.Object` implementing
    /// `Vesper.Fun\`2<ParamTy, ResultTy>`. `Node`, its pool id, keys every `…ByNode` table.
    /// `Captures` order = field order = ctor-arg order = order pushed at construction.
    type Closure =
        {
            Node: TastAccessor.ExprId
            Name: string
            ParamKey: BoundVarId
            ParamTy: FrozenType
            /// The closure's parameter pattern. A `NamedSimple` / unit `Const` binds
            /// `ldarg.1` directly through `ParamKey`; a `fun (a, b) -> …` tuple pattern is
            /// destructured out of it, so `ParamKey` is a synthetic placeholder.
            ParamPat: TastAccessor.PatId
            ResultTy: FrozenType
            Body: TastAccessor.ExprId
            Captures: (BoundVarId * FrozenType) list
            /// Binding key of the `let [rec] f = <this lambda>` this is the value of. A
            /// recursive self-reference resolves to `this` (`ldarg.0`), so it is not
            /// captured. `ValueNone` for an anonymous lambda.
            SelfKey: BoundVarId voption
            /// `> 0` ⇒ a generic closure: the TOTAL number of `GenericParam` rows (`T0…`)
            /// on its `TypeDefinition`, the enclosing class's typars followed by the
            /// enclosing method's. The construction site `Newobj`s a `MemberRef` on the
            /// `TypeSpec`.
            Typars: int
            /// How many of `Typars` are the enclosing class's (a member-body closure on a
            /// generic class); `0` for a static-fn closure, all of whose typars are
            /// method-axis. Splits the construction-site instantiation into the two axes.
            DeclaringTypars: int
            /// The front-end verdict that a readonly-struct shape is ADMISSIBLE for this
            /// closure. Necessary but not sufficient: `IsValueStruct` is the codegen gate.
            Repr: ClosureRepr
            /// Emit this closure as a zero-alloc value struct: `System.ValueType` base,
            /// by-value construction (`initobj` captureless, ctor capturing), and a
            /// constrained-slot `!TF` override in place of a `callvirt`.
            IsValueStruct: bool
            /// The FLAT arity of the `Vesper.Fun` interface this closure implements: `1` ⇒
            /// `Fun\`2<P,R>` with `Invoke(P):R`, `2` ⇒ `Fun\`3<P1,P2,R>` with one flat
            /// `Invoke(P1,P2):R`, up to `4` ⇒ `Fun\`5`. Always `1 + ExtraParams.Length`.
            FunArity: int
            /// The extra flat parameters beyond the first, in flat order. Peeled from
            /// successive inner `Lambda`s of a curried `fun x y … -> …` so they do NOT
            /// become their own closures. `Invoke` binds extra param `i` to `ldarg.(2+i)`.
            ExtraParams: (BoundVarId * FrozenType * TastAccessor.PatId) list
        }

    /// A capture-free monomorphic heap closure is stateless, so one shared instance
    /// suffices: it is `newobj`'d once into a singleton field by the closure's `.cctor`
    /// and every construction site `ldsfld`s that instead of allocating.
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

    /// An augmentation member on a union/class `TypeDefinition`; `ParamArity` excludes
    /// `this`, and a property's `Handle` is its `get_<name>` method. `Handle` is the `Def`
    /// token, but a generic type reaches the member by `MemberRef` off `MetaName` +
    /// signature.
    type EmittedMember =
        {
            Handle: EntityHandle
            IsStatic: bool
            ParamArity: int
            MetaName: string
            ParamTys: FrozenType list
            RetTy: FrozenType
            /// The member's *own* generic-method typar count (`member s.Map<'U> …`).
            /// 0 for the common non-generic member. > 0 ⇒ the member-ref must carry
            /// the `GENERIC` header and the call site a `MethodSpec`.
            MethodTyparCount: int
        }

    /// One step of a class preamble, in declaration order: what the `.cctor` (static
    /// sequence) or the primary `.ctor` (instance sequence) runs. One step type serves
    /// both: the enclosing builder decides whether `Store` is `stsfld` or `stfld`.
    [<RequireQualifiedAccess>]
    type PreambleStep =
        /// A `[static] let`: evaluate the initialiser and store it into the backing field.
        | Store of field: EntityHandle * init: TastAccessor.ExprId
        /// A `[static] do`: run the body for effect (its `unit` result is popped).
        | Run of body: TastAccessor.ExprId

    /// A class primary `.ctor`'s base-constructor chain.
    [<RequireQualifiedAccess>]
    type CtorChain =
        /// `inherit Base(args)`, an external base's `.ctor`, or `System.Object::.ctor()`
        /// for an `inherit`-less reference class. The args are evaluated before
        /// `this` is constructed, so they may only reference the ctor params (`ldarg`).
        | Base of ctor: EntityHandle * args: TastAccessor.ExprId list
        /// A value type: `System.ValueType` has no accessible ctor and value types do
        /// not chain.
        | None

    /// A union emitted into this assembly. `Typars` empty ⇒ monomorphic: a single sealed
    /// class whose `TagField` / `Factory` / `Fields` handles are usable as `Def` tokens.
    /// Non-empty ⇒ generic, and every member is reached by `MemberRef` off `Name` instead.
    type EmittedUnion =
        {
            Name: string
            Typars: string list
            TagField: EntityHandle
            ValueKind: UnionValueKind
            Cases: Dictionary<string, EmittedCase>
            /// Augmentation members by source name, each mapping to the LIST of its
            /// overloads: own members first, interface impls last. `Append(v:'T)` and
            /// `Append(v:'T, width:int)` share a key; the call site picks by argument type.
            Members: Dictionary<string, EqArray<EmittedMember>>
        }

        /// Drives `isValueType` at use sites: box on `:>`, `unbox.any` on `:?>`.
        member this.IsValueType: bool = this.ValueKind.IsValueType

        /// The metadata shape this union is emitted in.
        member this.Regime: UnionRegime =
            UnionRegime.classify
                this.ValueKind
                this.Cases.Count
                (this.Cases.Values |> Seq.exists (fun c -> not c.Fields.IsEmpty))

    /// A record emitted into this assembly: a sealed class, one public field per record
    /// field, one ctor taking `Fields` in declaration order. `Typars` empty ⇒ monomorphic
    /// (`Def`-token handles), non-empty ⇒ generic.
    type EmittedRecord =
        {
            Name: string
            Typars: string list
            Fields: (string * EntityHandle * FrozenType) list
            /// `true` for a `[<Struct>]` value-type record. Drives `isValueType`
            /// at use sites (box on `:>`, `unbox.any` on `:?>`), like the class flag.
            IsValueType: bool
            Ctor: EntityHandle
            /// Augmentation members, on the same terms as `EmittedUnion.Members`.
            Members: Dictionary<string, EqArray<EmittedMember>>
        }

    /// A class emitted into this assembly. `Typars` empty ⇒ monomorphic; non-empty ⇒
    /// generic, and its members are reached by `MemberRef` rather than `Def` token.
    type EmittedClass =
        {
            Name: string
            Typars: string list
            /// Primary-constructor backing fields, `(name, handle, type)` in declaration
            /// order. Its LENGTH is the primary ctor's arity, which a `TExpr.New` matches
            /// against, so explicit `val` fields stay out of it, in `InstanceFields`.
            Fields: (string * EntityHandle * FrozenType) list
            /// Explicit `val [mutable] x: T` instance fields, `(name, handle, type)`.
            /// Default-initialised (not set by the primary ctor); a `this.x`
            /// `FieldGet`/`FieldSet` resolves its handle here.
            InstanceFields: (string * EntityHandle * FrozenType) list
            /// `true` for a `[<Struct>]` value type. Drives `isValueType` at use
            /// sites (box on `:>`, `unbox.any` on `:?>`).
            IsValueType: bool
            Ctor: EntityHandle
            /// `true` when a synthesised primary `.ctor` was emitted, so `Ctor` is a real
            /// primary a `TExpr.New` of the field arity may target. `false` for `type T =
            /// val …; new(…) = …`: `Ctor` aliases the first secondary, unusable as one.
            HasPrimaryCtor: bool
            /// Augmentation members, on the same terms as `EmittedUnion.Members`.
            Members: Dictionary<string, EqArray<EmittedMember>>
            /// `static let` backing fields keyed by source name; a `TExpr.StaticFieldGet`
            /// resolves its `ldsfld` handle here. A mono class stores the field `Def`
            /// token, a generic class a `MemberRef` on the open self-`TypeSpec`.
            StaticFields: Dictionary<string, EntityHandle>
            /// Secondary constructors keyed by arity → (declared param types, `.ctor`
            /// handle). A `TExpr.New` whose arg count differs from the primary's selects
            /// the matching overload here.
            SecondaryCtors: (int * FrozenType list * EntityHandle) list
            /// The implemented interfaces, each written over THIS class's declaring typars
            /// (arg leaves are `FTTypar(TyparAxis.Declaring, i)`), for instantiation at an
            /// object argument. Direct impls only, not a base's.
            Interfaces: FrozenNominal list
        }

    /// How an emitted enum's cases are loaded and compared.
    type EmittedEnumRepr =
        /// A numeric enum: a `System.Enum` subclass. Its case fields are `literal`, so
        /// metadata-only (`ldsfld` on one throws `MissingFieldException`). Both `E.A` and
        /// `| E.A` push the underlying integer `CaseValues.[case]` directly instead.
        | NumericEnum of CaseValues: Dictionary<string, TConstValue>
        /// A string / mixed enum: a `[<Struct>]` wrapper whose cases are
        /// `.cctor`-initialised `static initonly` fields, so `E.A` is `ldsfld
        /// caseFields.[case]` and `| E.A` compares `backingField` against `caseLits.[case]`.
        | StructEnum of
            isMixed: bool *
            backingField: EntityHandle *
            caseFields: Dictionary<string, EntityHandle> *
            caseLits: Dictionary<string, TEnumLiteral>

    /// An enum emitted into this assembly: monomorphic and memberless, so `Repr` is all
    /// there is to carry.
    type EmittedEnum = { Repr: EmittedEnumRepr }

    /// An interface emitted into this assembly. Only `Members` matters at use sites: a call
    /// on an interface-typed object arg resolves the member here and `callvirt`s its slot.
    /// There is no ctor or field to carry. `Typars` empty ⇒ monomorphic.
    type EmittedInterface =
        {
            Name: string
            Typars: string list
            Members: Dictionary<string, EqArray<EmittedMember>>
        }

    /// One static module class per `module Foo = …`, identified by the whole `ModuleKey`
    /// rather than a `(namespace, name)` pair: modules nest, and the nesting chain is what
    /// the module-class tree and its `NestedClass` rows are read off.
    type ModuleClassKey = ModuleKey

    /// One flattened parameter of a `StaticFn`. A simple bound variable's `Slot` key resolves
    /// directly to the parameter's `ldarg` index; a `fun (a, b) -> …` destructuring
    /// parameter carries `Pat = Some …` and a synthetic `Slot` spilled to a local first.
    type StaticParam = TastLower.StaticParam

    /// A top-level function lowered to a static method: `let [rec] f p0 p1 …` becomes
    /// `static f(p0, p1, …)`, curried parameters flattened. Eligible only when the function
    /// never escapes as a value and captures no module-level local.
    type StaticFn =
        {
            Key: BoundVarId
            /// The stable handle key, used instead of the per-file `Key`, a bare offset
            /// that collides across files. An exportable binding carries exactly its own
            /// identity, so a cross-file call re-homes here; others carry an unspellable mint.
            SymbolKey: SymbolKey
            Name: string
            /// `Some k` when from a named `module Foo = …`: emits as a public
            /// static method on the `Foo` module class. `None` ⇒ the anonymous "Program"
            /// module class.
            ModuleClass: ModuleClassKey option
            /// The SOURCE groups and the flat, tuple-expanded, lone-unit-erased parameters
            /// they expand to: one CLR `ldarg` slot per flat parameter. The flat signature
            /// cannot tell tupled `f(int,int)` from one `(int*int)` param, which is why the
            /// groups travel with it.
            Params: CompiledFns.FlatParams<StaticParam>
            Body: TastAccessor.ExprId
            ResultTy: FrozenType
            /// `true` when the source result type is `unit`: the method emits as genuine
            /// CLR `void`, its body pops the trailing `unit`, and a value-position call
            /// reifies a `unit` after the `call`.
            ReturnsVoid: bool
            /// The binding's frozen typar bounds, method-axis-indexed templates over the
            /// method typars. Read by the call-site phantom-typar solve to recover a typar
            /// that no parameter or result mentions, like `fold`'s `'E`.
            Constraints: FrozenConstraint list
        }

    /// A module-level value (`let x = e` at module scope) lowered to a `public static`
    /// field, whose module class's `.cctor` evaluates `Init` and `stsfld`s it. A value on a named
    /// module gets that module's class, a top-level one the anonymous "Program" class.
    type ModuleValue =
        {
            Key: BoundVarId
            /// This value's stable handle key, on the same terms as `StaticFn.SymbolKey`.
            SymbolKey: SymbolKey
            Name: string
            Ty: FrozenType
            Init: TastAccessor.ExprId
            ModuleClass: ModuleClassKey
        }

    /// Emission handle + shape of a static-method function, resolved before any body is
    /// built (the `MethodDefinition` handle is predicted from row order). A call site
    /// consumes one argument per source group, `call`s `Handle` with the flat values those
    /// flatten to, then `Invoke`s any remainder.
    type StaticMethodRef =
        {
            Handle: EntityHandle
            /// The source groups and the flat CLR parameter types, mirroring `StaticFn.Params`.
            Params: CompiledFns.FlatParams<FrozenType>
            ResultTy: FrozenType
            /// `0` ⇒ monomorphic, a plain `call`. Otherwise the call site recovers the
            /// instantiation by matching the flat parameter types, whose leaves are
            /// `FTTypar(TyparAxis.Method, i)`, against the actual argument types.
            Typars: int
            /// `true` ⇒ the method is CLR `void`: the `call` declares 0 results and a
            /// value-position consumer reifies a `unit` afterward.
            ReturnsVoid: bool
            /// The method's frozen typar bounds, mirroring `StaticFn.Constraints`.
            Constraints: FrozenConstraint list
        }

    /// The run-wide registries every builder needs: the provider seam, the metadata writer,
    /// and the shared tables resolving a `Lambda` to its emitted closure, a nominal to its
    /// rows, and a top-level function to a direct `call`. Per-method state layers on top.
    type EmitContext =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            ClosureByNode: Dictionary<TastAccessor.ExprId, Closure>
            CtorHandleByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
            /// A cached closure singleton field: a `Lambda` node here `ldsfld`s its one
            /// shared instance instead of `newobj`ing per construction.
            CachedClosureFieldByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
            /// A value-struct closure `Lambda` node → the synthetic encodable `FrozenType`
            /// of its by-value local, and (`ClosureTypeDefByNode`) the closure-`TypeDef`
            /// handle that is the `initobj` operand.
            ClosureValueTypeByNode: Dictionary<TastAccessor.ExprId, FrozenType>
            ClosureTypeDefByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
            Unions: Dictionary<TypeKey, EmittedUnion>
            Records: Dictionary<TypeKey, EmittedRecord>
            Classes: Dictionary<TypeKey, EmittedClass>
            Interfaces: Dictionary<TypeKey, EmittedInterface>
            /// Enums emitted into this assembly, by nominal `SymbolKey`. A
            /// `StaticFieldGet` / `EnumCase` resolves a case's literal field here.
            Enums: Dictionary<TypeKey, EmittedEnum>
            StaticMethods: Dictionary<BoundVarId, StaticMethodRef>
            /// Module-level value bindings → their emitted `public static` field, so a
            /// module value resolves the same way in any method, `.ctor` or `.cctor`.
            ModuleValues: Dictionary<BoundVarId, EntityHandle>
            /// The top-level values `Main` initialises by `stsfld`, those trailing a
            /// top-level `do`, rather than allocating a `Main` local for. Reads still go
            /// through `ModuleValues`; values a `.cctor` initialises are absent.
            MainInitValues: Dictionary<BoundVarId, EntityHandle>
        }

    /// Per-method codegen state over the run-wide `EmitContext`. `Slots` maps this method's
    /// locals to slot indices; `Args` maps a parameter to its `ldarg` index (closure
    /// `Invoke`: `this` 0, param 1; static method: flat params 0…N-1; `Main`: none).
    type EmitEnv =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            Slots: Dictionary<BoundVarId, int>
            ClosureByNode: Dictionary<TastAccessor.ExprId, Closure>
            CtorHandleByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
            /// Cached closure singleton fields; a `Lambda` value here `ldsfld`s instead of
            /// `newobj`ing.
            CachedClosureFieldByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
            /// Value-struct closures: synthetic encodable `FrozenType` + closure-`TypeDef`
            /// handle per value-struct `Lambda` node.
            ClosureValueTypeByNode: Dictionary<TastAccessor.ExprId, FrozenType>
            ClosureTypeDefByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
            Args: Dictionary<BoundVarId, int>
            SelfKey: BoundVarId voption
            /// The declaring VALUE TYPE of the member being emitted: `this` (`ldarg.0`)
            /// is then a managed pointer, and a value use of it deref-copies (`ldobj`)
            /// through this type's token. `ValueNone` for a reference type.
            SelfValueType: FrozenType voption
            CaptureFields: Dictionary<BoundVarId, EntityHandle>
            Unions: Dictionary<TypeKey, EmittedUnion>
            Records: Dictionary<TypeKey, EmittedRecord>
            Classes: Dictionary<TypeKey, EmittedClass>
            Interfaces: Dictionary<TypeKey, EmittedInterface>
            /// Enums emitted into this assembly, so a `StaticFieldGet` / `EnumCase` in any
            /// body resolves a case's field here.
            Enums: Dictionary<TypeKey, EmittedEnum>
            StaticMethods: Dictionary<BoundVarId, StaticMethodRef>
            /// Module-level values, each lowered to a `public static` field on its module
            /// class and resolved here by bound variable → field handle (`ldsfld`).
            ModuleValues: Dictionary<BoundVarId, EntityHandle>
        }

    /// A `Var` bound to an addressable local slot in `env` → its slot index. The shared
    /// "is this an addressable local?" test in front of struct object-arg addressing, the
    /// `&`-address-of intrinsic, and the struct-argument spill, each with its own fallback.
    [<return: Struct>]
    let (|LocalSlot|_|) (env: EmitEnv) (e: TastAccessor.ExprId) : int voption =
        match e with
        | TastAccessor.EVar k ->
            match env.Slots.TryGetValue k with
            | true, slot -> ValueSome slot
            | false, _ -> ValueNone
        | _ -> ValueNone

    module EmitEnv =
        /// `args` maps each parameter to its `ldarg` index; locals (`Slots`) always start
        /// empty. `selfKey` / `captureFields` are the closure-`Invoke` extras, so every
        /// other builder passes neither.
        let create
            (ctx: EmitContext)
            (selfKey: BoundVarId voption)
            (captureFields: Dictionary<BoundVarId, EntityHandle>)
            (args: Dictionary<BoundVarId, int>)
            : EmitEnv =
            {
                Provider = ctx.Provider
                Ctx = ctx.Ctx
                Slots = Dictionary<BoundVarId, int>()
                ClosureByNode = ctx.ClosureByNode
                CtorHandleByNode = ctx.CtorHandleByNode
                CachedClosureFieldByNode = ctx.CachedClosureFieldByNode
                ClosureValueTypeByNode = ctx.ClosureValueTypeByNode
                ClosureTypeDefByNode = ctx.ClosureTypeDefByNode
                Args = args
                SelfKey = selfKey
                SelfValueType = ValueNone
                CaptureFields = captureFields
                Unions = ctx.Unions
                Records = ctx.Records
                Classes = ctx.Classes
                Interfaces = ctx.Interfaces
                Enums = ctx.Enums
                StaticMethods = ctx.StaticMethods
                ModuleValues = ctx.ModuleValues
            }

        /// The common builder shape: parameters only, with no recursive self and no
        /// captures.
        let ofContext (ctx: EmitContext) (args: Dictionary<BoundVarId, int>) : EmitEnv =
            create ctx ValueNone (Dictionary()) args

    /// The `ldc` for an integral constant. `sbyte` … `uint32` all have int32 as their CIL
    /// stack type, so `ldc.i4` of the low 32 bits is the whole load; signedness is a
    /// type-level distinction the verifier reads off the slot. Wider widths push `ldc.i8`.
    let intConstLoad (k: IntKind) (bits: int64) : ILInstr =
        if IntKind.isWide k then
            ILInstr.LdcI8 bits
        else
            ILInstr.LdcI4(int32 bits)

    /// `intConstLoad` plus, for `nativeint` / `unativeint` alone, the `conv.i` / `conv.u`
    /// their signedness calls for: `native int` is a distinct CIL stack type, and without
    /// the conversion an int64 lands in a `native int` slot and the IL is unverifiable.
    let pushIntConst (b: IlBuilder) (k: IntKind) (bits: int64) : unit =
        b.Add(intConstLoad k bits)

        if IntKind.isNative k then
            b.Add(
                ILInstr.Un(
                    if IntKind.isSigned k then
                        ILOpCode.Conv_i
                    else
                        ILOpCode.Conv_u
                )
            )

    /// Materialise `()` on the stack (net +1). `unit` is a zero-field struct, so the value
    /// is reified by zero-initialising a scratch local: `ldloca; initobj; ldloc`.
    let buildUnitValue (env: EmitEnv) (b: IlBuilder) : unit =
        let slot = b.Local(FTConst(RuntimeNames.unitKey, EqArray.empty))
        b.Add(ILInstr.Ldloca slot)
        b.Add(ILInstr.Initobj(env.Provider.TypeToken(FTConst(RuntimeNames.unitKey, EqArray.empty))))
        b.Add(ILInstr.Ldloc slot)

    /// What a call leaves on the stack.
    [<RequireQualifiedAccess>]
    type CallResult =
        /// An F# `unit` return is emitted .NET `void`, so the call pushes nothing and `()`
        /// is reified after it for a value-position consumer.
        | Void
        | Value

        /// The instruction's push count.
        member this.Pushes =
            match this with
            | CallResult.Void -> 0
            | CallResult.Value -> 1

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CallResult =

        /// The verdict off a member's declared return type.
        let ofReturnTy (ty: FrozenType) : CallResult =
            match ty with
            | FTUnit -> CallResult.Void
            | _ -> CallResult.Value

        /// The verdict for a member whose `void`-ness is read off metadata instead: `M: 'a ->
        /// 'a` at `'a = unit` still returns `!0`, so the applied type does not determine it.
        let ofReturnsVoid (returnsVoid: bool) : CallResult =
            if returnsVoid then CallResult.Void else CallResult.Value

        /// Emit whatever the call did not push.
        let reify (env: EmitEnv) (b: IlBuilder) (result: CallResult) : unit =
            match result with
            | CallResult.Void -> buildUnitValue env b
            | CallResult.Value -> ()
