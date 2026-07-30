namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The DECLARATION shapes of the TAST: a module-level declaration, and the type declaration
// under it in each of its five kinds (interface / union / record / class / enum) with the
// members, preambles and constructors they carry. The term shapes a declaration's bodies
// are made of live in `TastExpr.fs`, which this file reads and which does not read it back;
// the unit-level file shape and the monomorphic instantiations of every name here are
// `Tast.fs`.
//
// The projections over a type kind (`module TTypeKindG`) live with the shapes they project,
// so a consumer asking "the members of this declaration" has one answer that stays
// exhaustive as kinds are added.
//
// `BinderKeyG` opens the file and `module BinderKey` closes it: a declaration's key slots
// are TYPED by the binder key, and the projections that produce one are the only code that
// may see its representation, so the type, the slots it types, and the projections must
// share a file.

/// An identity that some node of the tree INTRODUCES as a definition site: what a
/// `TExpr.Var` references, what naming is computed from, and what the frozen binder pool
/// interns. It rides the tree's own identity axis (`'id`), because "is a binder" is a fact
/// about a node and not about the space that node is addressed in: pre-freeze and frozen
/// trees name a binder by `NodeKey`, a tree rebuilt from the pools by `BinderId`, and both
/// have side tables that may only be keyed by a definition site.
///
/// A raw identity addresses ANY node — an expression, a pattern the freeze erases, a
/// binding's CST head (`CstKeys.ofBinding`) — so it is the over-wide type for such a table,
/// and for a declaration's pattern-less key slots (`TTypeMemberG.ThisKey`, …), which is why
/// those slots are typed by this and not by `'id`.
///
/// The representation is PRIVATE, which is the whole mechanism: the constructor is
/// reachable only from this file, where the `BinderKey` module below is the only thing that
/// uses it. So every binder key in the program is a projection of a node that binds, and a
/// binder-keyed table cannot be filed under a key naming something else — which is what
/// `let (x) = 5` did (the paren head is a node `ElaboratePatterns.translatePat` erases, so
/// the entry was unreachable and the binding's name was silently lost) and what a
/// module-level tuple destructuring did (a head that binds no single name at all).
/// Widening is one-way, through `BinderKey.identity`.
[<Struct>]
type BinderKeyG<'id> = private | Binder of 'id

/// The binder key of a tree addressed by `NodeKey` — every pre-freeze and frozen domain.
type BinderKey = BinderKeyG<NodeKey>

/// A binder's definition site said both ways at once: the key a reference resolves through,
/// and the token that spells its name. `BinderKey.siteOfCstPat` is the only producer and
/// answers both from ONE match, so a slot filled from it cannot record a key for one pattern
/// and a name for another.
[<Struct>]
type BinderSite = { Binder: BinderKey; Tok: SyntaxToken }

/// How the SOURCE writes a binder: the identifier, and where it is written.
///
/// RECORDED where the binder's key is minted from a token
/// (`PassContext.BinderSpellings`), never recovered afterwards. Not recovered from the
/// key, whose number is a character offset that only happens to be a token start —
/// searching for a token there is a coincidence dressed as a lookup. And not read back off
/// the introducing node either, because the two come apart: `Inline.spliceAt` moves a
/// spliced body onto its CALL SITE, after which the node's own token spells something else
/// entirely.
///
/// A binder with no spelling is one no source writes — a class's `this`/`base`, a binder
/// `Inline.freshen` minted — and the freeze stores that as the empty name, which
/// `BinderNaming.ofColumn` reads back as `Minted`.
[<Struct>]
type BinderSpelling = { Name: string; At: Anchor }

module BinderSpelling =

    /// A binder no source writes.
    let unspelled: BinderSpelling = { Name = ""; At = Anchor.nowhere }

/// Whether a class declaration emits as a reference type, a `[<Struct>]` value
/// type, or a `[<IsByRefLike>]` byref-like value type. Collapses the former
/// `isStruct`/`isByRefLike` bool pair so the illegal `(isStruct = false,
/// isByRefLike = true)` combination is unrepresentable; `RefStruct` implies
/// value-type emission. Projected at `Elaborate` from `ClassTypeInfo`
/// (`IsValueType` / `IsByRefLike`).
[<RequireQualifiedAccess>]
type ClassValueKind =
    | RefType
    | Struct
    | RefStruct

[<RequireQualifiedAccess>]
type TDeclG<'ty, 'tok, 'id> =
    /// `isInline` lets codegen expand the body per call site via `Inline.inlineExpand`
    /// rather than emit a single callable.
    | Let of binding: TPatG<'ty, 'tok, 'id> * value: TExprG<'ty, 'tok, 'id> * isInline: bool * ty: 'ty
    | Expression of expr: TExprG<'ty, 'tok, 'id> * ty: 'ty
    | Type of TTypeDeclG<'ty, 'tok, 'id, TExprG<'ty, 'tok, 'id>>

/// `'body` abstracts over how a member/preamble/ctor BODY is carried. A body slot is
/// either the expression tree itself (`TExprG<'ty,'tok,'id>` — every tree-shaped domain,
/// SemType and frozen alike) or a dense id naming that expression in a pool; the
/// declaration shape is indifferent to which, holding only the slots and their order.
/// Nothing structural about a `type` declaration depends on a body being walkable, so
/// the parameter costs the shape nothing and buys the pooled form a home.
and TTypeDeclG<'ty, 'tok, 'id, 'body> =
    {
        /// Simple (unqualified) type name, e.g. `"Fun"`. The metadata name gets
        /// the arity suffix (`` Fun`2 ``) from `TypeParams.Length`.
        Name: string
        /// The type's stable nominal identity:
        /// the registry `info.TypeKey` (`TypeKey(Some homeAsm, declNs, name\`arity)`),
        /// carried into the backend so the emitted-type tables key off it directly
        /// instead of re-deriving a string. Codegen branches local-vs-external on
        /// its home `asm` (= the assembly being emitted).
        TypeKey: TypeKey
        /// `None` for a module-level type.
        Namespace: string option
        /// Declared type parameters in source order (e.g. `["'A"; "'B"]`).
        TypeParams: EqArray<string>
        /// `[<RequireQualifiedAccess>]` posture. Type-level so it covers records
        /// AND unions in one carrier: it is F#'s `isILOrRequiredQualifiedAccess`
        /// signal (`NameResolution.fs:1277`) projected through freeze — a cross-unit
        /// RQA record is kept OUT of the consumer's unqualified field-set index
        /// (a bare `{ X = … }` must qualify), and an RQA union's case out of the
        /// bare case index. Carried here so the frozen-tree projection
        /// (`FrozenSignature`) can honour it, mirroring the `.fsi` extractor's
        /// `RqaTypes` thread. Default `false`; interfaces / enums leave it unread.
        IsRequireQualifiedAccess: bool
        Kind: TTypeKindG<'ty, 'tok, 'id, 'body>
        /// Equality posture for this type (records / unions / interfaces).
        /// Defaults to `Structural` — interfaces ignore it (no triple is ever
        /// synthesised), records / unions consume it in the codegen loops.
        EqualitySupport: EqualityVerdict
        /// Comparison posture for this type (records / unions / interfaces).
        /// Defaults to `NoComparison` — interfaces ignore it (no pair is ever
        /// synthesised), records / unions consume it in the codegen loops to
        /// decide whether to emit the `IComparable<Self>` / `IComparable`
        /// `InterfaceImpl`s and the `CompareTo(Self)` / `CompareTo(object)`
        /// pair. The default is **opt-in**, so an
        /// unannotated record / union skips the pair.
        ComparisonSupport: ComparisonVerdict
    }

    /// The nominal identity widened for the key-kind-blind sinks a declaration still feeds
    /// (`MethodKey`/`FieldKey`/`TypeSlotKey` minting, `provider.RegisterUserType`).
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey

and [<RequireQualifiedAccess>] TTypeKindG<'ty, 'tok, 'id, 'body> =
    /// A nominal type whose members are all abstract and which has no base type /
    /// field.
    | Interface of methods: EqArray<TAbstractMethodG<'ty>>
    /// `cases` in declaration order (the index is the runtime tag), plus any
    /// augmentation members (`with member …` / `static member …`). `interfaces`
    /// mirrors `TClassG.Interfaces`: each entry pairs a resolved interface type
    /// with its already-typed member bodies (the `interface IFace with member …`
    /// blocks declared on the union). Empty for a plain union. The JS backend emits
    /// these as the union BASE class's capability protocol members (an enumerable impl
    /// → `[Symbol.iterator]`, inherited by every case subclass); the CLR backend's
    /// emission is still deferred.
    | Union of
        cases: EqArray<TUnionCaseG<'ty>> *
        members: EqArray<TTypeMemberG<'ty, 'id, 'body>> *
        interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>>
    /// `fields` are the record's payload in declaration order, paired with their
    /// declared types and mutability. `members` carries augmentation members
    /// (`with member …` / `static member …`). `interfaces` mirrors
    /// `TClassG.Interfaces` / `Union.interfaces`: each entry pairs a resolved
    /// interface type with its already-typed member bodies (the
    /// `interface IFace with member …` blocks declared on the record). Empty for a
    /// plain record. The CLR backend emits the impl methods as `InterfaceImpl` rows;
    /// the JS backend attaches them to the record's class (local interface →
    /// attached method; capability interface → iterator / registry symbol).
    /// `valueKind` is `Struct` for a `[<Struct>]` record (emitted as a
    /// `System.ValueType`-based value type, sealed) and `RefType` otherwise;
    /// projected at `Elaborate` from `RecordTypeInfo.IsValueType`, mirroring how
    /// the class arm carries `ClassValueKind`. `RefStruct` is unreachable — a
    /// record cannot be `[<IsByRefLike>]`.
    | Record of
        fields: EqArray<TRecordFieldG<'ty>> *
        members: EqArray<TTypeMemberG<'ty, 'id, 'body>> *
        interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>> *
        valueKind: ClassValueKind
    /// Class type emission.
    /// `fields` are mutable instance fields (currently empty);
    /// `ctorParams` borrows the `TRecordField` shape for the primary
    /// constructor's parameter list (name / type / mutability=false).
    /// `members` carries every instance / static method / property (the
    /// instance-vs-static split is the member's own `IsStatic`).
    /// `baseType` is `ValueNone` (codegen defaults the IL
    /// `TypeDefinition.BaseType` to `Object`); a later slice fills it from
    /// `ClassTypeInfo.BaseType`. `interfaces` is empty; another slice
    /// fills it from the interface-impl registry — each entry pairs the
    /// resolved interface type (a `TyClass`, remapped onto this class's typar
    /// markers so a generic interface arg like `IEnumerable<'T>` encodes against
    /// the declaring type's generic parameters) with its already-typed member
    /// bodies. Codegen emits one `InterfaceImpl` row per entry and one
    /// `MethodDefinition` per member (implicit impl — bound by name + signature;
    /// explicit `.override` rows are deferred with the `MethodImpl` table).
    /// `isSealed` reflects `[<Sealed>]`: when `true`, codegen flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition` — derivation
    /// is rejected at use sites (`subsumes` already excludes
    /// `Sealed`).
    /// `staticPreamble` / `instancePreamble` are the class's `[static] let` / `[static] do`
    /// entries: codegen emits one private field per `let` (static / instance respectively)
    /// and runs each sequence, in declaration order, in the `.cctor` / the primary ctor.
    /// `secondaryCtors` are `new(args) = SelfType(primaryArgs)` overloads:
    /// codegen emits each as a `.ctor` overload whose body runs the let-preamble
    /// then chains to the primary `.ctor`. Empty unless the class declares any.
    /// `baseCtorCall` is the `inherit Base(args)` invocation: codegen makes the primary `.ctor` chain to the parent's `.ctor` with
    /// these args before storing fields. `ValueNone` for a parent-less class (the
    /// primary `.ctor` then chains to `System.Object::.ctor`). Always present
    /// together with a `ValueSome baseType`.
    /// `ValueKind` (`ClassValueKind`) replaces the former `isStruct`/`isByRefLike`
    /// bool pair: `Struct` ⇒ codegen emits a `System.ValueType`-based value type
    /// (sealed, sequential layout, ctor without a base-ctor chain) instead of a
    /// reference class; `RefStruct` additionally stamps
    /// `System.Runtime.CompilerServices.IsByRefLikeAttribute` so the CLR confines
    /// the value type to the stack. `fields`
    /// (the explicit `val [mutable] x: T` instance fields) are populated for both
    /// structs and classes that declare them — each emits a `FieldDefinition` and
    /// a mutable one admits `this.x <- …`.
    | Class of TClassG<'ty, 'id, 'body>
    /// `cases` in declaration order, each pairing a case identifier with its
    /// **resolved** compile-time literal (`| C = v`). An enum is `'ty`-free: a
    /// case value is an integer or string literal, never a typed term. The
    /// numeric / string / mixed variant is NOT stored here — it is **derived**
    /// from the case literals on demand (`TEnumCases.classify`), the single
    /// source of truth. See `TEnumCaseG` / `TEnumLiteral`.
    | Enum of cases: EqArray<TEnumCaseG<'tok>>

/// The payload of `TTypeKindG.Class`, lifted out of an 11-wide positional
/// tuple into a named record. See the `Class` case doc for per-field semantics.
///
/// No `'tok`: nothing a class carries is token-bearing — every member / preamble /
/// ctor slot went to `'body` and the field shapes are `'ty`-only. (`TTypeKindG` keeps
/// `'tok` for `Enum`'s case identifiers, which are the kind's only tokens.)
and TClassG<'ty, 'id, 'body> =
    {
        Fields: EqArray<TRecordFieldG<'ty>>
        CtorParams: EqArray<TRecordFieldG<'ty>>
        Members: EqArray<TTypeMemberG<'ty, 'id, 'body>>
        BaseType: 'ty voption
        Interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>>
        IsSealed: bool
        /// `static let` / `static do`, in declaration order: the body of the
        /// synthesised `.cctor`. Empty unless the class declares any.
        StaticPreamble: EqArray<TPreambleEntryG<'ty, 'body>>
        /// Instance `let` / `do`, in declaration order: the tail of the primary ctor,
        /// running after the base-ctor call and the ctor-param field stores. Empty
        /// unless the class declares any; a class with NO primary ctor can never have
        /// one (the front-end rejects it, F#'s FS0963).
        InstancePreamble: EqArray<TPreambleEntryG<'ty, 'body>>
        /// The `this` binder every instance member body already carries
        /// (`TTypeMemberG.ThisKey`), lifted onto the class because the INSTANCE
        /// preamble's expressions read the class's fields through it too — a ctor-param
        /// or instance-`let` reference in an initialiser or `do` body is a
        /// `TExpr.FieldGet(TExpr.Var(ThisKey), …)`, so the backend must map this key to
        /// the primary ctor's `this` argument.
        ThisKey: BinderKeyG<'id>
        SecondaryCtors: EqArray<TSecondaryCtorG<'ty, 'id, 'body>>
        BaseCtorCall: TBaseCtorCallG<'ty, 'id, 'body> voption
        ValueKind: ClassValueKind
        /// True when the class declares a *primary* constructor (`type T(args) =`,
        /// including the parameterless `type T() =`); false for the `val`-field form
        /// (`type T = val …; new(…) = { … }`) whose only ctors are secondaries. The
        /// backend emits a synthesised primary `.ctor` only when this is true — for
        /// the val-field form the secondaries ARE the ctors, and a synthesised
        /// parameterless primary would collide with a parameterless `new()` (two
        /// identical `.ctor()` rows) and shadow it at construction.
        HasPrimaryCtor: bool
    }

/// `Fields` are the case's payload in declaration order; a field's name is
/// `ValueNone` when the source is positional (`Cons of 'T * list`). Empty
/// `Fields` ⇒ a nullary case (`Nil`).
and TUnionCaseG<'ty> =
    {
        Name: string
        Fields: EqArray<string voption * 'ty>
    }

/// A resolved enum-case literal — the classified result of reading
/// `EnumTypeCase.constValue` through the canonical literal reader
/// (`ElaborateLiterals.parseConst` / `foldStringParts`). Restricting the shape to
/// `Int` / `String` makes the non-int-non-string values the elaborator rejects
/// unrepresentable on the node (illegal cases never construct a `TEnumLiteral`;
/// they record `ValueNone` on `TEnumCaseG.Value`).
and [<RequireQualifiedAccess>] TEnumLiteral =
    /// An integer enum-case value: the authored integral literal exactly as `parseConst`
    /// resolved it, so its `IntWidth` is the authored width. `int` doubles as the
    /// unsuffixed default — step 2/freeze maps it to `I32`, every other width to its own
    /// CLR underlying type. Width is therefore *preserved*, not defaulted, here.
    ///
    /// Invariant: always a `TConstValue.Integral` whose width satisfies `IntWidth.isEnumBase`
    /// — the elaborator rejects every other constant, `nativeint` / `unativeint` included
    /// (no `System.Enum` may be based on a pointer-width integer).
    | Int of value: TConstValue
    /// A string enum-case value — the stitched literal text (escapes decoded).
    | String of value: string

/// One case of a `TTypeKind.Enum`, in declaration order. `'tok` is carried for
/// the case identifier's source token (diagnostics / source-maps), matching the
/// token-preserving convention of the sibling AST nodes.
and TEnumCaseG<'tok> =
    {
        /// Case identifier (`C` in `| C = v`).
        Name: string
        /// The case's resolved compile-time literal, classified `Int` / `String`
        /// (`TEnumLiteral`). `ValueNone` when `constValue` failed to resolve to a
        /// legal literal — a non-literal expression, an interpolated string, or a
        /// non-int-non-string constant — for which a hard error was reported at
        /// the case's source token. The case is still recorded so the enum's
        /// shape and its sibling cases survive a single bad case.
        Value: TEnumLiteral voption
        /// Source token of the case identifier.
        Tok: 'tok
    }

/// One field of a `TTypeKind.Record`. `Type` carries the field's declared
/// type — with the declaring type's typar markers (`TyConst "'T"`) for a
/// generic record, exactly like `TUnionCase.Fields`. `IsMutable` is the
/// source-level `mutable` annotation; downstream consumers (the equality
/// triple's "all-immutable record" gate) read it from here rather
/// than re-querying `ctx.Types.Record`.
and TRecordFieldG<'ty> =
    {
        Name: string
        Type: 'ty
        IsMutable: bool
    }

and [<RequireQualifiedAccess>] TMemberKind =
    /// Invoked through `TExpr.MethodCall` / `TExpr.StaticMethodCall`.
    | Method
    /// A parameterless getter, read through `TExpr.PropertyGet` /
    /// `TExpr.StaticPropertyGet`. Emitted as a `get_<Name>` method (no
    /// `PropertyDefinition` row yet).
    | Property

/// An instance member's body sees `this` (its `ThisKey`, resolved to `ldarg.0`)
/// and its parameters; a static member's body sees only its parameters.
and TTypeMemberG<'ty, 'id, 'body> =
    {
        Name: string
        IsStatic: bool
        /// Declared accessibility of the member (`member private`, `member internal`,
        /// or the public default). Carried physically on the member — not in
        /// `TastFile.Accessibility`, which keys top-level entities — so the frozen
        /// file→file projection (`FrozenSignature`) can honour member-level
        /// accessibility and NOT leak a `member private` across the unit boundary
        /// (internal-or-better threshold: same-assembly visible, `Private` dropped).
        Accessibility: Accessibility
        Kind: TMemberKind
        /// `true` when declared with the `override`/`default` keyword — i.e. it
        /// overrides a base virtual slot. For a class with no `inherit` clause
        /// that base is `System.Object`, so an `override` `Equals`/`GetHashCode`/
        /// `ToString` reuses the Object virtual slot and must emit *virtual*
        /// (reusing the slot, no `NewSlot`); a plain `member` is non-virtual.
        /// Without this the override emits `Public HideBySig` (non-virtual), so it
        /// never replaces `Object.Equals` and — for a structural-equality interface
        /// like `IStructuralEquatable` — the type fails to satisfy its slots.
        IsOverride: bool
        /// Instance members only; `ValueNone` for a static member.
        ThisKey: BinderKeyG<'id> voption
        /// The synthetic `base` binder of the declaring class, shared across every
        /// member body. A `base.M(...)`
        /// receiver is a `TExpr.Var(BaseKey, parentTy)`; codegen maps it to the
        /// same `ldarg.0` as `this`, so this key is loaded identically — the
        /// `CallVia.Base` discriminator (not the receiver) drives non-virtual
        /// dispatch. `ValueNone` for a static member, a union member, or a class
        /// without an `inherit` clause.
        BaseKey: BinderKeyG<'id> voption
        /// The declaring type (a `TyUnion`) — the receiver type for an instance
        /// member's `this`.
        ThisTy: 'ty
        /// Parameter binders in declaration order (each `ldarg` after `this` for
        /// an instance method); empty for a property or a nullary method.
        Params: EqArray<BinderKeyG<'id> * 'ty>
        Body: 'body
        ReturnTy: 'ty
        /// The member's *own* generic parameters (`member this.Map<'C> …`) — distinct from the declaring
        /// type's `TTypeDecl.TypeParams`. Each entry pairs the source name
        /// (`"'C"`, for the `GenericParam` row) with the typar's *own* type: a
        /// `TyVar root` at build time, flipped — like every other embedded type — by
        /// `Elaborate.freezeTypars` / `TastConvert.file` to `TyTypar(Method, i)` then
        /// `FTTypar(Method, i)`, exactly as the declaring type's typars ride the
        /// `Declaring` axis; codegen's encoder resolves both axes by index
        /// (`!!i` / `!i`) with no ambient window. This list feeds the `GenericParam`
        /// rows and the `GENERIC` header arity (name = `fst`, arity = `.Length`).
        /// Empty for a non-generic member. Rides `'ty` so the frozen tree carries no
        /// union-find cell — the canonical ABI order is built once by
        /// `GeneralizedTypars.canonical` in the side-table `Generalized` and
        /// materialized here in that order.
        MethodTypeParams: EqArray<string * 'ty>
    }

/// One `[static] let [mutable] x = <init>` of a class preamble.
///
/// A STATIC entry is a private static field, initialised by the synthesised `.cctor`; a
/// `static let`-bound name referenced anywhere in the class lowers to
/// `TExpr.StaticFieldGet`. On a *generic* class the field rides the open `TypeDefinition`
/// (one per closed instantiation, `.cctor`-initialised) and the read/store mint a
/// `MemberRef` on the self-`TypeSpec` at the declaring typars.
///
/// An INSTANCE entry is a private instance field, initialised by the primary ctor — the
/// same lowering a primary-ctor parameter already gets, with the value coming from `Init`
/// instead of an argument. Its references (in a member body or in a later preamble entry)
/// are therefore `TExpr.FieldGet`/`FieldSet` on `this`, never a `TExpr.Let` binder: an
/// instance `let mutable` captured by a preamble closure must stay ONE field, so it must
/// never reach `RefCellPromotion` (which would fork the storage between a promoted cell in
/// the closure and the field every member reads).
and TClassLetG<'ty, 'body> =
    {
        Name: string
        Type: 'ty
        /// `let mutable` ⇒ the field is writable.
        IsMutable: bool
        Init: 'body
    }

/// One entry of a class preamble, in DECLARATION order. Interleaving is
/// order-sensitive (`static let a = f()` / `static do g a` / `static let b = h()`), so a
/// preamble is one ordered sequence — not parallel lists of lets and dos.
and [<RequireQualifiedAccess>] TPreambleEntryG<'ty, 'body> =
    | Let of TClassLetG<'ty, 'body>
    | Do of 'body

/// One `let`-preamble binding inside a secondary constructor body
/// (`new(args) = let x = e in SelfType(...)`). `Binder` is the local's
/// identity (codegen allocates a local slot and a body reference to the name
/// loads it); `Init` is the right-hand side. Only simple (single-name) binders
/// are modelled in v1.
and TCtorLetG<'ty, 'id, 'body> =
    {
        Binder: BinderKeyG<'id>
        Type: 'ty
        Init: 'body
    }

/// One `field = expr` initialiser of a secondary constructor's explicit
/// field-init block (`new(s) = { stack = s; started = false }`).
/// `Field` names a declared instance field (an explicit
/// `val` or a primary-ctor backing field); `Init` is the value stored into it
/// (`ldarg.0; <Init>; stfld Field`). Used only when a secondary ctor takes the
/// explicit-init form instead of chaining to the primary ctor.
and TCtorFieldInitG<'body> = { Field: string; Init: 'body }

/// A secondary constructor. Codegen emits a
/// `.ctor` overload: `Params` are the overload's parameters (`ldarg` after
/// `this`); `Lets` run as locals in declaration order. The body then takes one
/// of two shapes, never both:
/// - **Chain form** (`new(args) = SelfType(...)`): `FieldInits` is empty and the
///   body chains to the primary `.ctor` with `PrimaryArgs` (`ldarg.0; <args>;
///   call instance void SelfType::.ctor`). There is no usable `this` before the
///   chain call, so `Lets` / `PrimaryArgs` only reference the ctor params and
///   earlier lets.
/// - **Explicit field-init form** (`new(args) = { f = e; … }`):
///   `PrimaryArgs` is empty and each `FieldInits` entry stores into a
///   declared field (`ldarg.0; <Init>; stfld f`). No primary chain — the fields
///   not listed are left default-initialised. `this`'s storage is the freshly
///   allocated (zeroed) instance, so `Init` may reference ctor params and lets.
and TSecondaryCtorG<'ty, 'id, 'body> =
    {
        Params: EqArray<BinderKeyG<'id> * 'ty>
        Lets: EqArray<TCtorLetG<'ty, 'id, 'body>>
        PrimaryArgs: EqArray<'body>
        FieldInits: EqArray<TCtorFieldInitG<'body>>
    }

/// An `inherit Base(args)` base-constructor invocation. Codegen wires the primary `.ctor` to chain to the
/// parent's `.ctor`: `ldarg.0; <Args>; call instance void Base::.ctor(…)` before
/// storing the derived class's own fields. `CtorParams` are the *derived* class's
/// primary-ctor parameters (the `ldarg` mapping the base-ctor `Args` reference —
/// `this` isn't constructed yet, so an arg can only name a primary-ctor param or
/// a `static let`). The parent type itself rides the `Class` kind's `baseType`
/// slot, which also supplies the IL `TypeDefinition.BaseType`.
and TBaseCtorCallG<'ty, 'id, 'body> =
    {
        CtorParams: EqArray<BinderKeyG<'id> * 'ty>
        Args: EqArray<'body>
        /// The chosen base `.ctor`'s identity for an EXTERNAL base (`inherit exn(msg)`),
        /// recorded by `Unification.fillBaseCtorCall` so codegen chains the base ctor by key
        /// rather than re-picking by arity. `ValueNone` for a project-local base (its ctor is
        /// the local class's, resolved from the emitted class table) and for an external base
        /// whose overload identity was not recorded (codegen falls back to arity).
        ChosenCtor: SymbolKey voption
    }

/// `Signature` is the curried function type; a type parameter of the *declaring
/// type* is carried as `TyConst "'A"` (a name marker the backend resolves to a
/// `GenericTypeParameter` index).
///
/// `MethodTypeParams` are the method's *own* generic parameters in source order
/// (e.g. `["'C"]` for `abstract Map<'C> : 'A -> 'C`), distinct from the declaring
/// type's `TTypeDecl.TypeParams`. They are also carried as `TyConst "'C"` markers
/// in `Signature`; the backend maps them to `GenericMethodParameter` indices (vs
/// the declaring type's `GenericTypeParameter`). Empty ⇒ a non-generic method.
and TAbstractMethodG<'ty> =
    {
        Name: string
        MethodTypeParams: EqArray<string>
        Signature: 'ty
        /// `true` for an abstract *property* (`abstract member Current : int` —
        /// an arg-less member sig). It emits as a `get_<Name>` getter slot so a
        /// property impl (`get_Current`) binds to it by name + signature; a
        /// `false` (method) slot keeps its bare name.
        IsProperty: bool
    }

[<RequireQualifiedAccess>]
module TTypeKindG =
    /// The augmentation / instance members a type kind carries, uniform across the
    /// three member-bearing kinds (class / union / record). `Interface` (abstract,
    /// bodyless) and `Enum` (literal cases only) carry none. The single accessor for
    /// "the members of a type declaration", shared by the member-inline harvest
    /// (`SymbolProviders.collectInlineBodies`) and `ConformanceTypars.bodyMembers` —
    /// so neither hard-codes a single kind and a `(# … #)` member on any host is
    /// harvested, not silently dropped.
    let members (kind: TTypeKindG<'ty, 'tok, 'id, 'body>) : EqArray<TTypeMemberG<'ty, 'id, 'body>> =
        match kind with
        | TTypeKindG.Class c -> c.Members
        | TTypeKindG.Union(_, members, _) -> members
        | TTypeKindG.Record(_, members, _, _) -> members
        | TTypeKindG.Interface _
        | TTypeKindG.Enum _ -> EqArray.empty

    /// The INTERFACE-IMPLEMENTATION member bodies a type kind carries (`interface IFace
    /// with member …`), flattened across every implemented interface — the companion to
    /// `members`, which yields only the type's own augmentation members. The two together
    /// are every member body under a type declaration. `Interface` (abstract, bodyless)
    /// and `Enum` (literal cases only) carry none.
    let interfaceMembers (kind: TTypeKindG<'ty, 'tok, 'id, 'body>) : TTypeMemberG<'ty, 'id, 'body> seq =
        let flatten (ifaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>>) =
            seq {
                for (_, ms) in EqArray.toArray ifaces do
                    yield! EqArray.toArray ms
            }

        match kind with
        | TTypeKindG.Class c -> flatten c.Interfaces
        | TTypeKindG.Union(_, _, ifaces) -> flatten ifaces
        | TTypeKindG.Record(_, _, ifaces, _) -> flatten ifaces
        | TTypeKindG.Interface _
        | TTypeKindG.Enum _ -> Seq.empty

/// THE definition of "is a binder" — one projection per way a definition site comes to
/// exist: a pattern introduces it (`ofPat`, `ofCstPat` before the tree does), an expression
/// introduces it with no pattern behind it (`ofExpr`), a declaration MINTS it because no
/// node spells it (`ofDeclaredThis` / `ofDeclaredBase`), or a pool interned it and hands
/// back its dense id (`ofInterned`). Every `BinderKey` comes from here; see the type's own
/// doc for why that is enforceable.
///
/// A declaration's key slots need no projection of their own: the shape TYPES them, so what
/// fills one is already a binder key, `refile` moves it between identity spaces, and
/// `ofTypeDecl` only has to enumerate them.
module BinderKey =

    /// The single binder a PATTERN introduces. `ValueNone` for a pattern that binds nothing
    /// (`Wildcard`, `Const`, …) or that binds only through nested sub-patterns (`Tuple`,
    /// `Record`, `Union`, `TypeTestAs`, `Or` — walk the children for those).
    ///
    /// Generic over the domain on purpose: the pre-freeze producers of the binder-keyed side
    /// tables (`Elaborate`, `Regions`) decide what to key on with the SAME function the
    /// post-freeze pool enumerates with (`TastPools.toPools`), so the two cannot drift.
    ///
    /// Note what is deliberately NOT a binder: a `let` whose head pattern is composite or
    /// wildcard (`let (a, b) = p`, `let _ = e`) introduces no single binder, so it has no
    /// side-table identity — and it needs none, every reader of those tables looking up a
    /// simple binder's key. (A CST `Pat.As` alias — `| 0 as z ->` — is a binder the frozen
    /// tree does not carry at all: `ElaboratePatterns.translatePat` drops the alias name, so
    /// there is no `TPatG` node here to answer for it.)
    let ofPat (p: TPatG<'ty, 'tok, 'id>) : BinderKeyG<'id> voption =
        match p with
        | TPatG.NamedSimple(binding = binding) -> ValueSome(Binder binding)
        | TPatG.Wildcard _
        | TPatG.Tuple _
        | TPatG.Const _
        | TPatG.Record _
        | TPatG.Union _
        | TPatG.TypeTestAs _
        | TPatG.Null _
        | TPatG.EnumCase _
        | TPatG.Or _ -> ValueNone

    /// The binder an EXPRESSION introduces with no pattern node behind it, TOGETHER with
    /// the token that spells its name: a `ForTo` loop variable, whose `i` token has no
    /// surrounding `Pat` in the CST. Every other binding expression (`Lambda`, `Let`,
    /// `ForIn`, a match arm) carries a real `TPatG`, so `ofPat` answers for it and this
    /// stays a single case.
    ///
    /// The two answers come from ONE match so that "which expression binds" and "where its
    /// name is written" cannot come apart — a pooling walk needs both, and `ofExpr` is this
    /// with the token forgotten.
    let siteOfExpr (e: TExprG<'ty, 'tok, 'id>) : struct (BinderKeyG<'id> * 'tok) voption =
        match e with
        | TExprG.ForTo(var = var; identTok = identTok) -> ValueSome(struct (Binder var, identTok))
        | _ -> ValueNone

    /// The binder an EXPRESSION introduces — `siteOfExpr` without the anchor, for a caller
    /// that only has to know a node binds.
    let ofExpr (e: TExprG<'ty, 'tok, 'id>) : BinderKeyG<'id> voption =
        siteOfExpr e |> ValueOption.map (fun (struct (b, _)) -> b)

    /// `ofPat` before the tree exists: the binder a CST pattern introduces, for a front-end
    /// producer that must name a definition site while it is still translating towards one
    /// (a class constructor's parameters, a secondary ctor's `let` head). The key is the
    /// INNERMOST `NamedSimple`'s, after peeling the wrappers `translatePat` erases
    /// (`[<…>] p`, `(p)`, `p : t`, `p as x`) — so it is the very key the elaborated
    /// `TPatG.NamedSimple` will carry, and a body's `Var` reference resolves to it.
    ///
    /// Peeling is the point: `let (x) = 5`'s head is a `Pat.EnclosedBlock` the tree does not
    /// keep, so a key taken off it names a node nothing binds.
    /// The two answers come from ONE match so that "which CST pattern binds" and "where its
    /// name is written" cannot come apart — a producer filling a declaration's key slot
    /// needs both, the slot keeping no token of its own (`PassContext.SpellBinder`).
    let rec siteOfCstPat (p: Pat<SyntaxToken>) : BinderSite voption =
        match p with
        | Pat.NamedSimple t ->
            ValueSome
                {
                    Binder = Binder(CstKeys.ofPat p)
                    Tok = t
                }
        | Pat.Attributed(pat = inner)
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner)
        | Pat.As(pat = inner) -> siteOfCstPat inner
        | _ -> ValueNone

    /// The binder a CST pattern introduces — `siteOfCstPat` without the token, for a caller
    /// that only has to know a pattern binds.
    let ofCstPat (p: Pat<SyntaxToken>) : BinderKey voption =
        siteOfCstPat p |> ValueOption.map (fun s -> s.Binder)

    /// The `this` binder a type declaration introduces, shared by every member body and by
    /// the instance preamble. No node spells it — `type C() =` writes no `this` token — so
    /// there is nothing to project from and the key is MINTED from the declaration's own,
    /// here and nowhere else. Minting IS what makes it a binder: the key does not exist
    /// until this call, so it cannot already name a node that binds nothing.
    let ofDeclaredThis (declKey: NodeKey) : BinderKey =
        Binder(NodeKey.ofSynthetic declKey.Offset NodeKind.SynthThisBinding)

    /// The `base` binder of a type declaration, minted from the declaration exactly as
    /// `ofDeclaredThis` is and distinguished only by kind. Always allocated; only read when
    /// the class has an `inherit` clause.
    let ofDeclaredBase (declKey: NodeKey) : BinderKey =
        Binder(NodeKey.ofSynthetic declKey.Offset NodeKind.SynthBaseBinding)

    /// The binder a pool INTERNED under this dense id. Unlike a `NodeKey`, which addresses
    /// any node at all, a `BinderId` is minted by one thing only — interning a binder key —
    /// so its whole space is definition sites and re-admitting one invents nothing.
    ///
    /// The seam that needs it is DESERIALIZATION: a decl's key slots are written as the ids
    /// they hold and read back with no key to project from. Nothing else should reach for
    /// it; a pass that already holds a key re-files it (`refile`).
    let ofInterned (id: BinderId) : BinderKeyG<BinderId> = Binder id

    /// Every binder a TYPE DECLARATION introduces with no pattern node to introduce it.
    ///
    /// A member body names its receiver and its parameters by `TExpr.Var`, exactly as a
    /// function body names a `let` or a lambda parameter — but those definition sites are
    /// key SLOTS on the declaration shape (`ThisKey` / `BaseKey` / `Params`, the class's own
    /// `ThisKey`, a secondary ctor's `Params` and `Lets[].Binder`, the base-ctor call's view
    /// of the primary ctor's params), NOT `TPatG.NamedSimple` nodes. So a consumer that
    /// enumerates definition sites by walking PATTERNS sees none of them, and any attempt to
    /// resolve such a `Var` to its definition comes up empty. This is that missing half of
    /// the enumeration, and it is why the two are separate projections rather than one walk:
    /// they read different slots.
    ///
    /// `'body`-blind — it touches no body — so it serves the tree form and any pooled
    /// (`'body = <id>`) form alike.
    let ofTypeDecl (td: TTypeDeclG<'ty, 'tok, 'id, 'body>) : BinderKeyG<'id> seq =
        let ofMember (m: TTypeMemberG<'ty, 'id, 'body>) =
            seq {
                match m.ThisKey with
                | ValueSome k -> yield k
                | ValueNone -> ()

                match m.BaseKey with
                | ValueSome k -> yield k
                | ValueNone -> ()

                for (k, _) in EqArray.toArray m.Params do
                    yield k
            }

        seq {
            for m in EqArray.toArray (TTypeKindG.members td.Kind) do
                yield! ofMember m

            for m in TTypeKindG.interfaceMembers td.Kind do
                yield! ofMember m

            match td.Kind with
            | TTypeKindG.Class c ->
                // The class-wide `this`: the INSTANCE preamble's expressions read the
                // class's fields through it, so it is a definition site of preamble
                // bodies as well as of the members that carry their own copy.
                yield c.ThisKey

                for sc in EqArray.toArray c.SecondaryCtors do
                    for (k, _) in EqArray.toArray sc.Params do
                        yield k

                    for l in EqArray.toArray sc.Lets do
                        yield l.Binder

                match c.BaseCtorCall with
                | ValueSome bc ->
                    for (k, _) in EqArray.toArray bc.CtorParams do
                        yield k
                | ValueNone -> ()
            | TTypeKindG.Interface _
            | TTypeKindG.Union _
            | TTypeKindG.Record _
            | TTypeKindG.Enum _ -> ()
        }

    /// Widen to the address space that holds every node of the tree's own identity axis —
    /// for a lookup driven by a REFERENCE (a `TExpr.Var` names its binder by that axis) or
    /// by a consumer that holds the raw identity and not the binder (`DenseTable.index`).
    /// One-way: nothing re-enters the binder domain through it.
    let identity (Binder k) : 'id = k

    /// Re-file a binder into ANOTHER identity space: `f` answers with the identity THIS
    /// binder takes there (interning it into a pool, widening a dense id back to a key).
    /// The result names a definition site because the argument did — which is why the
    /// mapping takes the key and not a bare identity, and why a pass that re-axises a tree
    /// (`TastConvert.typeDecl`) can move every key slot without being able to invent one.
    let refile (f: BinderKeyG<'a> -> 'b) (k: BinderKeyG<'a>) : BinderKeyG<'b> = Binder(f k)

    /// A whole binder-keyed table read in the REFERENCE domain — the one reason to widen
    /// more than a single key, and named so the reason is stated once rather than at each
    /// site: a lookup driven by a `TExpr.Var` has only the raw identity the reference
    /// carries. One-way, being `identity` per entry.
    let widenMap (m: Map<BinderKeyG<'id>, 'v>) : Map<'id, 'v> =
        m |> Map.toSeq |> Seq.map (fun (b, v) -> identity b, v) |> Map.ofSeq
