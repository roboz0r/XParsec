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
            ResultTy: FrozenType
            Body: Frozen.TExpr
            Captures: (NodeKey * FrozenType) list
            /// Binding key of the `let [rec] f = <this lambda>` this is the value
            /// of. A recursive self-reference resolves to `this` (`ldarg.0`), so
            /// it is not captured. `ValueNone` for an anonymous lambda.
            SelfKey: NodeKey voption
            /// `> 0` ⇒ a *generic* closure (C3): the number of typars the enclosing
            /// static method (or enclosing closure) declares, inherited verbatim at
            /// this closure's discovery point. Its
            /// `TypeDefinition` carries that many `GenericParam` rows; its signatures
            /// encode the body's `TyTypar(Method, i)` as the closure *class*'s `!i`
            /// (via `ClrEnv.ClosureTyparMode`); and the construction site `Newobj`s a
            /// `MemberRef` on the instantiated `TypeSpec`.
            Typars: int
        }

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
            Members: Dictionary<string, EmittedMember>
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

    /// A class emitted into this assembly (B-1). Same `Members` shape as
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
            /// Explicit `val [mutable] x: T` instance fields (vesper-set-sprint-phase-6),
            /// `(name, handle, type)`. Default-initialised (not set by the primary
            /// ctor); a `this.x` `FieldGet`/`FieldSet` resolves its handle here.
            InstanceFields: (string * EntityHandle * FrozenType) list
            /// `true` for a `[<Struct>]` value type — drives `isValueType` at use
            /// sites (box on `:>`, `unbox.any` on `:?>`).
            IsValueType: bool
            Ctor: EntityHandle
            Members: Dictionary<string, EmittedMember>
            /// `static let` backing fields keyed by source name (B-10); a
            /// `TExpr.StaticFieldGet` resolves its `ldsfld` handle here. Only
            /// monomorphic classes populate this (generic `static let` deferred).
            StaticFields: Dictionary<string, EntityHandle>
            /// Secondary constructors (B-11) keyed by arity → `.ctor` handle. A
            /// `TExpr.New` whose arg count differs from the primary's selects the
            /// matching overload here. Monomorphic only at call sites (generic
            /// secondary-ctor *call sites* are deferred — the bodies still emit).
            SecondaryCtors: (int * EntityHandle) list
        }

    /// A top-level function lowered to a **static method**: `let [rec] f p0 p1 …`
    /// becomes `static f(p0, p1, …)`, curried parameters flattened. Eligible only
    /// when the function never escapes as a value and captures no module-level
    /// local (see `collectStaticFns`); a recursive self-call is a direct `call`.
    type StaticFn =
        {
            Key: NodeKey
            Name: string
            /// `Some(namespace, holderName)` when from a named `module Foo = …`
            /// (recorded in `TastFile.ModuleMembers`): emits as a public static
            /// method on a `Foo` holder type. `None` ⇒ the anonymous "Program" holder.
            Holder: (string option * string) option
            Params: (NodeKey * FrozenType) list
            Body: Frozen.TExpr
            ResultTy: FrozenType
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
            Arity: int
            ResultTy: FrozenType
            Typars: int
            ParamTys: FrozenType list
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
            Args: Dictionary<NodeKey, int>
            SelfKey: NodeKey voption
            CaptureFields: Dictionary<NodeKey, EntityHandle>
            Unions: Dictionary<SymbolKey, EmittedUnion>
            Records: Dictionary<SymbolKey, EmittedRecord>
            Classes: Dictionary<SymbolKey, EmittedClass>
            StaticMethods: Dictionary<NodeKey, StaticMethodRef>
        }

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
