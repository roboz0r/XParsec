namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

module EmitTypes =

    /// How a closure's capture field receives its value.
    [<RequireQualifiedAccess>]
    type CaptureFill =
        /// Pushed at the construction site and stored by the `.ctor`.
        | ByCtor
        /// The `.ctor` receives `null`. The `let rec … and …` group constructing the closure
        /// stores the sibling once every member is bound.
        | BackPatched

    type Capture =
        {
            Key: BoundVarId
            Ty: FrozenType
            Fill: CaptureFill
        }

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
            Captures: Capture list
            /// Binding key of the `let [rec] f = <this lambda>` this is the value of. A
            /// recursive self-reference resolves to `this` (`ldarg.0`), so it is not
            /// captured. `ValueNone` for an anonymous lambda.
            SelfKey: BoundVarId voption
            /// The typars the closure lifts onto its own class: every scope visible at its
            /// construction site. `Count > 0` ⇒ a generic closure, with that many
            /// `GenericParam` rows (`T0…`) on its `TypeDefinition`.
            Frame: TyparFrame
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

        /// The total `GenericParam` row count on the closure's `TypeDefinition`.
        member c.TypeArity: int<typeSlot> = c.Frame.Count

    /// A generalised body-local `let` with a positive typar count, lifted to a generic static
    /// method on the Program class, as `fsc` emits it. Its method typars are `Frame`, and a
    /// call passes `Captures` ahead of `Params`.
    type LiftedLocal =
        {
            /// The local's compiled form: its key, its flat parameters and its body.
            Fn: CompiledFns.CompiledFn
            /// The metadata name, unique assembly-wide: `<source>@<n>`.
            Name: string
            /// The scopes visible at the local's `let`.
            Enclosing: TyparFrame
            /// The local's own scope and typar count, `Frame`'s last entry.
            Own: FrameScope
            /// The free variables of the local's body, in first-occurrence order: the leading
            /// parameters of the lifted method.
            Captures: (BoundVarId * FrozenType) list
        }

        member x.Key: BoundVarId = x.Fn.Key
        member x.Frame: TyparFrame = x.Enclosing.Push x.Own

    /// Emission handle and call shape of a lifted local, on the same terms as
    /// `StaticMethodRef`. A call site's `MethodSpec` is `Enclosing.Instantiation`, the
    /// caller's own leaves, followed by the local's typars recovered from the arguments.
    type LiftedLocalRef =
        {
            Handle: EntityHandle
            Enclosing: TyparFrame
            Own: FrameScope
            Captures: (BoundVarId * FrozenType) list
            Params: CompiledFns.FlatParams<FrozenType>
            ResultTy: FrozenType
            ReturnsVoid: bool
        }

    /// A capture-free monomorphic heap closure is stateless, so one shared instance
    /// suffices: it is `newobj`'d once into a singleton field by the closure's `.cctor`
    /// and every construction site `ldsfld`s that instead of allocating.
    let closureIsCached (c: Closure) : bool =
        List.isEmpty c.Captures && c.TypeArity = 0<_> && not c.IsValueStruct

    /// The `Def` tokens of a MONOMORPHIC closure, valid in every body. A generic closure has
    /// none: its `TypeSpec` and `MemberRef`s encode `FTTypar` relative to the referencing
    /// body's typar slots, so `closureToken` mints them per site.
    type EmittedClosure =
        {
            /// The `initobj` and `castclass` operand.
            Type: EntityHandle
            Ctor: EntityHandle
            /// In `Closure.Captures` order.
            CaptureFields: EntityHandle[]
            /// The singleton field of a `closureIsCached` closure; construction `ldsfld`s it.
            CachedField: EntityHandle voption
            /// The synthetic encodable type of a value-struct closure's by-value local.
            ValueType: FrozenType voption
        }

    /// A token of a closure as referenced from a body.
    [<RequireQualifiedAccess>]
    type ClosureToken =
        | Type
        | Ctor
        /// By index into `Closure.Captures`.
        | CaptureField of int

    /// The token of `c` for the body being emitted: a monomorphic closure's `Def` token, or a
    /// generic closure's `TypeSpec` / `MemberRef` minted under that body's typar slots.
    let closureTokenWith
        (provider: ICodegenProvider)
        (closures: Dictionary<TastAccessor.ExprId, EmittedClosure>)
        (c: Closure)
        (which: ClosureToken)
        : EntityHandle =
        if c.TypeArity = 0<_> then
            let emitted = closures.[c.Node]

            match which with
            | ClosureToken.Type -> emitted.Type
            | ClosureToken.Ctor -> emitted.Ctor
            | ClosureToken.CaptureField i -> emitted.CaptureFields.[i]
        else
            let args = c.Frame.Instantiation

            match which with
            | ClosureToken.Type -> provider.UserClosureTypeSpec(c.Name, args)
            | ClosureToken.Ctor -> provider.UserClosureMemberRef(c.Name, args, ClosureMember.Ctor)
            | ClosureToken.CaptureField i -> provider.UserClosureMemberRef(c.Name, args, ClosureMember.CaptureField i)

    /// One `ldfld` step of a union field's read path, over the field's identity `'h`: its
    /// layout `FieldKey` while the union is laid out, its `Def` token once emitted.
    [<RequireQualifiedAccess>]
    type FieldStep<'h> =
        /// A field of the union, its `Payload` or a hierarchy case type: the field itself at
        /// a monomorphic union, else `member'` re-spelled on the instantiated `TypeSpec`.
        | Member of field: 'h * member': UnionMember
        /// A field of a non-generic type nested in the union, the same at every
        /// instantiation.
        | Def of field: 'h

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module FieldStep =

        let field (step: FieldStep<'h>) : 'h =
            match step with
            | FieldStep.Member(f, _)
            | FieldStep.Def f -> f

        let map (f: 'a -> 'b) (step: FieldStep<'a>) : FieldStep<'b> =
            match step with
            | FieldStep.Member(h, member') -> FieldStep.Member(f h, member')
            | FieldStep.Def h -> FieldStep.Def(f h)

    /// One logical case field of a union emitted here, by the read path a match arm takes:
    /// the non-empty `ldfld` chain from the scrutinee, or from the case type in a hierarchy
    /// regime. An `Erased` path ends on an `object` slot, which a read `castclass`es back to
    /// the field's type at the use site.
    type EmittedCaseField =
        {
            Steps: FieldStep<EntityHandle> list
            Erased: bool
        }

    /// One case of an emitted union: runtime `Tag`, the static factory
    /// `TExpr.UnionCons` `call`s, and its payload fields in declaration order.
    type EmittedCase =
        {
            Tag: int
            Factory: EntityHandle
            Fields: EmittedCaseField list
            /// The case's own nested type in a hierarchy regime, which its payload fields
            /// are declared on and which a match arm casts to. `ValueNone` where the union
            /// is flat and every case reads its payload from slots on the union itself.
            CaseType: TypeKey voption
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
            MethodTyparCount: int<typeSlot>
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

    /// A union's discriminant, in the two spellings a reader reaches it by: the private
    /// field for a body on the union or one of its case types, and the public accessor
    /// everywhere else.
    type EmittedTag =
        {
            Field: EntityHandle
            Getter: EntityHandle
        }

    /// A union emitted into this assembly. `TypeArity` 0 ⇒ monomorphic: a single sealed
    /// class whose `Tag` / `Factory` / `Fields` handles are usable as `Def` tokens.
    /// Positive ⇒ generic, and every member is reached by `MemberRef` off `Name` instead.
    type EmittedUnion =
        {
            Name: string
            TypeArity: int<typeSlot>
            /// Held exactly where `UnionRegime.hasTag` holds.
            Tag: EmittedTag voption
            ValueKind: NominalValueKind
            Cases: Dictionary<string, EmittedCase>
            /// Augmentation members by source name, each mapping to the LIST of its
            /// overloads: own members first, interface impls last. `Append(v:'T)` and
            /// `Append(v:'T, width:int)` share a key; the call site picks by argument type.
            Members: Dictionary<string, Block<EmittedMember>>
        }

        /// Drives `isValueType` at use sites: box on `:>`, `unbox.any` on `:?>`.
        member this.IsValueType: bool = this.ValueKind.IsValueType

        /// The metadata shape this union is emitted in.
        member this.Regime: UnionRegime =
            UnionRegime.classify
                this.ValueKind
                this.Cases.Count
                (this.Cases.Values |> Seq.exists (fun c -> not c.Fields.IsEmpty))

    /// The accessors a record field declares: `get_<Field>` always, `set_<Field>` for a
    /// `mutable` field.
    type RecordFieldAccessorRefs =
        {
            Getter: EntityHandle
            Setter: EntityHandle voption
        }

    module RecordFieldAccessorRefs =
        let create (isMutable: bool) (mint: TAccessorRole -> EntityHandle) : RecordFieldAccessorRefs =
            {
                Getter = mint TAccessorRole.Getter
                Setter =
                    if isMutable then
                        ValueSome(mint TAccessorRole.Setter)
                    else
                        ValueNone
            }

        /// The accessor in `role`, `ValueNone` for the setter of an immutable field.
        let tryRole (role: TAccessorRole) (refs: RecordFieldAccessorRefs) : EntityHandle voption =
            match role with
            | TAccessorRole.Getter -> ValueSome refs.Getter
            | TAccessorRole.Setter -> refs.Setter

    /// One field of a record emitted into this assembly. `Field` and `Accessors` hold `Def`
    /// tokens; a generic record's use site re-mints them as `MemberRef`s through
    /// `RecordMember`.
    type EmittedRecordField =
        {
            Name: string
            Field: EntityHandle
            Ty: FrozenType
            Accessors: RecordFieldAccessorRefs
        }

    /// A record emitted into this assembly: a sealed class, one field per record field behind
    /// its accessors, one ctor taking `Fields` in declaration order. `TypeArity` 0 ⇒
    /// monomorphic (`Def`-token handles), positive ⇒ generic.
    type EmittedRecord =
        {
            Name: string
            TypeArity: int<typeSlot>
            Fields: EmittedRecordField list
            /// `true` for a `[<Struct>]` value-type record. Drives `isValueType`
            /// at use sites (box on `:>`, `unbox.any` on `:?>`), like the class flag.
            IsValueType: bool
            Ctor: EntityHandle
            /// Augmentation members, on the same terms as `EmittedUnion.Members`.
            Members: Dictionary<string, Block<EmittedMember>>
        }

    /// The path a use site takes to a named field on a record or class object argument, for
    /// one accessor role.
    [<RequireQualifiedAccess>]
    type FieldAccess =
        /// A class field, reached directly: `ldfld` / `stfld` / `ldflda`.
        | Direct of EntityHandle
        /// A record field, reached through the accessor in the requested role.
        | Accessor of EntityHandle

    /// A class emitted into this assembly. `TypeArity` 0 ⇒ monomorphic; positive ⇒
    /// generic, and its members are reached by `MemberRef` rather than `Def` token.
    type EmittedClass =
        {
            Name: string
            TypeArity: int<typeSlot>
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
            Members: Dictionary<string, Block<EmittedMember>>
            /// `static let` backing fields keyed by source name; a `TExpr.StaticFieldGet`
            /// resolves its `ldsfld` handle here. A mono class stores the field `Def`
            /// token, a generic class a `MemberRef` on the open self-`TypeSpec`.
            StaticFields: Dictionary<string, EntityHandle>
            /// Secondary constructors keyed by arity → (declared param types, `.ctor`
            /// handle). A `TExpr.New` whose arg count differs from the primary's selects
            /// the matching overload here.
            SecondaryCtors: (int * FrozenType list * EntityHandle) list
            /// The implemented interfaces, each written over THIS class's declaring typars
            /// (arg leaves are `FTTypar(Type _, i)`), for instantiation at an
            /// object argument. Direct impls only, not a base's.
            Interfaces: FrozenNominal list
        }

    /// How an emitted enum's cases are loaded and compared.
    type EmittedEnumRepr =
        /// A numeric enum: a `System.Enum` subclass. Its case fields are `literal`, so
        /// metadata-only (`ldsfld` on one throws `MissingFieldException`). Both `E.A` and
        /// `| E.A` push the underlying integer `CaseValues.[case]` directly instead.
        | NumericEnum of Underlying: TypeKey * CaseValues: Dictionary<string, TConstValue>
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
    /// There is no ctor or field to carry. `TypeArity` 0 ⇒ monomorphic.
    type EmittedInterface =
        {
            Name: string
            TypeArity: int<typeSlot>
            Members: Dictionary<string, Block<EmittedMember>>
        }

    /// One static module class per `module Foo = …`, identified by the whole `ModuleKey`
    /// rather than a `(namespace, name)` pair: modules nest, and the nesting chain is what
    /// the module-class tree and its `NestedClass` rows are read off.
    type ModuleClassKey = ModuleKey

    /// The class a top-level binding's member is emitted on.
    [<RequireQualifiedAccess>]
    type EmitHome =
        /// The class of the declaring `module Foo = …`.
        | Named of ModuleClassKey
        /// The anonymous "Program" class, which holds every binding declared at file scope.
        | Program of ModuleClassKey

        member this.Class: ModuleClassKey =
            match this with
            | Named c
            | Program c -> c

    /// Where a top-level binding's emitted name comes from.
    [<RequireQualifiedAccess>]
    type EmittedNaming =
        /// The source name; the member is public ABI.
        | Source
        /// A compiler-minted `<name>$<slot>`; the member is assembly-visible.
        | Minted

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
            BindingKey: BindingKey
            Name: string
            Home: EmitHome
            Naming: EmittedNaming
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
            /// The binding's scheme: the emitted method-typar count and the constraints the
            /// call-site phantom-typar solve reads to recover a typar that no parameter or
            /// result mentions, like `fold`'s `'E`.
            Scheme: FunctionScheme
        }

    /// A module-level value (`let x = e` at module scope) lowered to a `static`
    /// field, whose module class's `.cctor` evaluates `Init` and `stsfld`s it. A value on a named
    /// module gets that module's class, a top-level one the anonymous "Program" class.
    type ModuleValue =
        {
            Key: BoundVarId
            /// This value's stable handle key, on the same terms as `StaticFn.BindingKey`.
            BindingKey: BindingKey
            Name: string
            Naming: EmittedNaming
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
            /// An arity of `0` ⇒ monomorphic, a plain `call`. Otherwise the call site
            /// recovers the instantiation by matching the flat parameter types, whose leaves
            /// are `FTTypar(ModuleFunction _, i)`, against the actual argument types, and
            /// solves a phantom typar from the constraints.
            Scheme: FunctionScheme
            /// `true` ⇒ the method is CLR `void`: the `call` declares 0 results and a
            /// value-position consumer reifies a `unit` afterward.
            ReturnsVoid: bool
        }

    /// The run-wide registries every builder needs: the provider seam, the metadata writer,
    /// and the shared tables resolving a `Lambda` to its emitted closure, a nominal to its
    /// rows, and a top-level function to a direct `call`. Per-method state layers on top.
    type EmitContext =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            /// The pool that issued the ids in the bodies this context emits.
            Pool: PoolBuilder
            ClosureByNode: Dictionary<TastAccessor.ExprId, Closure>
            /// Every MONOMORPHIC closure `Lambda` node → its `Def` tokens.
            Closures: Dictionary<TastAccessor.ExprId, EmittedClosure>
            Unions: Dictionary<TypeKey, EmittedUnion>
            Records: Dictionary<TypeKey, EmittedRecord>
            Classes: Dictionary<TypeKey, EmittedClass>
            Interfaces: Dictionary<TypeKey, EmittedInterface>
            /// Enums emitted into this assembly, by nominal `SymbolKey`. A
            /// `StaticFieldGet` / `EnumCase` resolves a case's literal field here.
            Enums: Dictionary<TypeKey, EmittedEnum>
            StaticMethods: Dictionary<BoundVarId, StaticMethodRef>
            LiftedLocals: Dictionary<BoundVarId, LiftedLocalRef>
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
            Closures: Dictionary<TastAccessor.ExprId, EmittedClosure>
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
            /// A generalised local of this file → its lifted generic static method. A
            /// reference is a `call`, never a slot load or a capture.
            LiftedLocals: Dictionary<BoundVarId, LiftedLocalRef>
            /// Module-level values, each lowered to a `public static` field on its module
            /// class and resolved here by bound variable → field handle (`ldsfld`).
            ModuleValues: Dictionary<BoundVarId, EntityHandle>
        }

    /// The load of local `slot` as the `this` of one of its type's own instance methods: a
    /// value type is called on its address.
    let loadSlotAsThis (isValueType: bool) (slot: int) : ILInstr =
        if isValueType then
            ILInstr.Ldloca slot
        else
            ILInstr.Ldloc slot

    let closureToken (env: EmitEnv) (c: Closure) (which: ClosureToken) : EntityHandle =
        closureTokenWith env.Provider env.Closures c which

    /// The by-value type of `e` when it constructs a value-struct closure.
    let closureValueType (env: EmitEnv) (e: TastAccessor.ExprId) : FrozenType voption =
        match env.Closures.TryGetValue e with
        | true, ec -> ec.ValueType
        | false, _ -> ValueNone

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
                Closures = ctx.Closures
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
                LiftedLocals = ctx.LiftedLocals
                ModuleValues = ctx.ModuleValues
            }

        /// The common builder shape: parameters only, with no recursive self and no
        /// captures.
        let ofContext (ctx: EmitContext) (args: Dictionary<BoundVarId, int>) : EmitEnv =
            create ctx ValueNone (Dictionary()) args

    /// The `ldc` for an integral constant. `sbyte` … `uint32` all have int32 as their CIL
    /// stack type, so `ldc.i4` of the low 32 bits is the whole load; signedness is a
    /// type-level distinction the verifier reads off the slot. Wider widths push `ldc.i8`.
    let intConstLoad (v: IntValue) : ILInstr =
        match v with
        | IntValue.SByte n -> ILInstr.LdcI4(int32 n)
        | IntValue.Byte n -> ILInstr.LdcI4(int32 n)
        | IntValue.Int16 n -> ILInstr.LdcI4(int32 n)
        | IntValue.UInt16 n -> ILInstr.LdcI4(int32 n)
        | IntValue.Int32 n -> ILInstr.LdcI4 n
        | IntValue.UInt32 n -> ILInstr.LdcI4(int32 n)
        | IntValue.Int64 n -> ILInstr.LdcI8 n
        | IntValue.UInt64 n -> ILInstr.LdcI8(int64 n)
        | IntValue.NativeInt n -> ILInstr.LdcI8 n
        | IntValue.UNativeInt n -> ILInstr.LdcI8(int64 n)

    /// `intConstLoad` plus, for `nativeint` / `unativeint` alone, the `conv.i` / `conv.u`
    /// their signedness calls for: `native int` is a distinct CIL stack type, and without
    /// the conversion an int64 lands in a `native int` slot and the IL is unverifiable.
    let pushIntConst (b: IlBuilder) (v: IntValue) : unit =
        b.Add(intConstLoad v)

        let k = IntValue.kind v

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
        let slot = b.Local(RuntimeNames.unitTy)
        b.Add(ILInstr.Ldloca slot)
        b.Add(ILInstr.Initobj(env.Provider.TypeToken(RuntimeNames.unitTy)))
        b.Add(ILInstr.Ldloc slot)

    /// Whether an expression's own value is consumed.
    [<RequireQualifiedAccess>]
    type ExprPos =
        /// The value is left on the stack for a consumer.
        | Value
        /// The expression runs for effect. Emission returns the operand stack to the depth
        /// it had on entry.
        | Statement

        /// The operand-stack depth the expression adds.
        member this.Pushes =
            match this with
            | ExprPos.Value -> 1
            | ExprPos.Statement -> 0

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ExprPos =

        let ofReturnsVoid (returnsVoid: bool) : ExprPos =
            if returnsVoid then ExprPos.Statement else ExprPos.Value

        /// Emit the reified `unit` of a `unit`-typed construct in value position.
        let reifyUnit (env: EmitEnv) (b: IlBuilder) (pos: ExprPos) : unit =
            match pos with
            | ExprPos.Value -> buildUnitValue env b
            | ExprPos.Statement -> ()

        /// Discard back to `baseDepth`, the depth recorded before the expression was
        /// emitted. In value position the expression's result stands.
        let discardTo (b: IlBuilder) (baseDepth: int) (pos: ExprPos) : unit =
            match pos with
            | ExprPos.Value -> ()
            | ExprPos.Statement ->
                while b.Depth > baseDepth do
                    b.Add ILInstr.Pop

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

        /// Emit whatever the call did not push, in value position.
        let reify (env: EmitEnv) (b: IlBuilder) (pos: ExprPos) (result: CallResult) : unit =
            match result with
            | CallResult.Void -> ExprPos.reifyUnit env b pos
            | CallResult.Value -> ()
