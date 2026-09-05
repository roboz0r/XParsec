namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The DECLARATION shapes of the TAST: a module-level declaration, and the type declaration
// under it in each of its five kinds, with the members, preambles and ctors they carry.

/// An identity a node of the tree INTRODUCES as a definition site, over the tree's own
/// identity axis (`'id`): pre-freeze and frozen trees identify a bound variable by `NodeKey`, a tree
/// rebuilt from the pools by `BoundVarId`.
[<Struct>]
type BoundVarKeyG<'id> = private | BoundVar of 'id

type BoundVarKey = BoundVarKeyG<NodeKey>

[<Struct>]
type BoundVarSite =
    {
        BoundVar: BoundVarKey
        Tok: SyntaxToken
    }

/// The identifier a bound variable is written with. Recorded where the key is minted from a
/// token, never recovered afterwards: the key's number is a character offset, not a token index,
/// and inline expansion copies a body onto its CALL SITE, whose token spells something else.
[<Struct>]
type BoundVarIdent = { Text: string; At: Anchor }

module BoundVarIdent =

    /// A bound variable no source names.
    let unnamed: BoundVarIdent = { Text = ""; At = Anchor.nowhere }

/// `[<Struct>]` ⇒ `Struct`, `[<IsByRefLike>]` ⇒ `RefStruct`; `RefStruct` implies
/// value-type emission too.
[<RequireQualifiedAccess>]
type ClassValueKind =
    | RefType
    | Struct
    | RefStruct

    member this.IsValueType: bool =
        match this with
        | RefType -> false
        | Struct
        | RefStruct -> true

/// Reference-type versus value-type emission for a declaration that cannot be byref-like.
/// `[<Struct>]` ⇒ `Struct`.
[<RequireQualifiedAccess>]
type NominalValueKind =
    | RefType
    | Struct

    member this.IsValueType: bool =
        match this with
        | RefType -> false
        | Struct -> true

/// What a class declaration STATES about itself with an attribute, as against what its
/// contents decide. Carried whole through the TAST, the freeze and the external shape.
[<Struct>]
type DeclaredClassFlags =
    {
        IsSealed: bool
        IsAbstract: bool
        /// `[<AllowNullLiteral>]`: `null` inhabits this class, so it satisfies `when 'T : null`.
        AllowNullLiteral: bool
    }

    static member Default =
        {
            IsSealed = false
            IsAbstract = false
            AllowNullLiteral = false
        }

/// `Fields` are the case's payload in declaration order; a field's name is
/// `ValueNone` when the source is positional (`Cons of 'T * list`). Empty
/// `Fields` ⇒ a nullary case (`Empty`).
type TUnionCaseG<'ty> =
    {
        Name: string
        Fields: EqArray<string voption * 'ty>
        Attributes: TAttributes
    }

/// A resolved enum-case literal. Only `Int` / `String` are representable; the elaborator
/// records `ValueNone` for any other constant it rejects.
[<RequireQualifiedAccess>]
type TEnumLiteral =
    /// Always a `TConstValue.Integral` whose kind satisfies `IntKind.isEnumBase` (never
    /// pointer-width), carrying the AUTHORED kind, so an unsuffixed `int` becomes `I32` at
    /// freeze, not here.
    | Int of value: TConstValue
    /// A string enum-case value: the stitched literal text, escapes decoded.
    | String of value: string

type TEnumCaseG<'tok> =
    {
        /// Case identifier (`C` in `| C = v`).
        Name: string
        /// `ValueNone` when the source value is not a legal literal (a non-literal
        /// expression, an interpolated string, a non-int-non-string constant), for which a
        /// hard error was reported; the case is kept so its siblings live.
        Value: TEnumLiteral voption
        Tok: 'tok
        Attributes: TAttributes
    }

/// One field of a `TTypeKind.Record`. `Type` carries the field's declared type, which for a generic
/// record uses the declaring type's typar markers (`TyTypar(Declaring, i)`). `IsMutable` is the
/// source-level `mutable` annotation.
type TRecordFieldG<'ty> =
    {
        Name: string
        Type: 'ty
        IsMutable: bool
        Attributes: TAttributes
    }

/// Which half of a property an accessor supplies.
[<RequireQualifiedAccess>]
type TAccessorRole =
    | Getter
    | Setter

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module TAccessorRole =

    /// The accessor's `MethodDef` name over its property: `get_<prop>` / `set_<prop>`.
    let methodName (role: TAccessorRole) (propName: string) : string =
        match role with
        | TAccessorRole.Getter -> AccessorNames.getterName propName
        | TAccessorRole.Setter -> AccessorNames.setterName propName

[<RequireQualifiedAccess>]
type TMemberKind =
    | Method
    /// A parameterless getter, emitted as a `get_<Name>` method.
    | Property
    /// One accessor of the property `prop`, declared by `member x.prop with get … and set …`.
    /// The member's own `Name` is the accessor's (`get_<prop>` / `set_<prop>`); a
    /// parameterless getter takes the `Property` form instead.
    | Accessor of prop: string * role: TAccessorRole

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module TMemberKind =

    /// The property a member accesses, with the half it supplies; `ValueNone` for a method.
    /// `name` is the member's own, which the parameterless-getter form spells the property with.
    let propertyOf (name: string) (kind: TMemberKind) : (string * TAccessorRole) voption =
        match kind with
        | TMemberKind.Method -> ValueNone
        | TMemberKind.Property -> ValueSome(name, TAccessorRole.Getter)
        | TMemberKind.Accessor(prop, role) -> ValueSome(prop, role)

    /// How a `MemberKey` spells the member: a parameterless getter keys as a value, and
    /// every other form keys with the parameter vector its overloads differ at.
    let keyKind (kind: TMemberKind) : MemberKind =
        match kind with
        | TMemberKind.Property -> MemberKind.Property
        | TMemberKind.Method
        | TMemberKind.Accessor _ -> MemberKind.Method

type TTypeMemberG<'ty, 'id, 'body> =
    {
        Name: string
        IsStatic: bool
        Accessibility: Accessibility
        IsInline: bool
        Kind: TMemberKind
        /// `true` when declared with the `override` OR the `default` keyword.
        IsOverride: bool
        /// Instance members only; `ValueNone` for a static member.
        ThisKey: BoundVarKeyG<'id> voption
        /// The synthetic `base` bound variable of the declaring class; `ValueNone` for a static or
        /// union member, or a class with no `inherit`. A `base.M(…)` object argument loads as the
        /// same `ldarg.0` as `this`; `CallVia.Base` is what makes the dispatch non-virtual.
        BaseKey: BoundVarKeyG<'id> voption
        ThisTy: 'ty
        /// Parameter bound variables in declaration order; empty for a property or a nullary method.
        Params: EqArray<BoundVarKeyG<'id> * 'ty>
        Body: 'body
        ReturnTy: 'ty
        /// The member's *own* generic parameters (`member this.Map<'C> …`), distinct from
        /// the declaring type's `TypeParams`. Each entry pairs the source name with the
        /// typar's own type.
        MethodTypeParams: EqArray<string * 'ty>
        /// The bounds on `MethodTypeParams`, declared or inferred, indexed on the method axis.
        MethodTyparConstraints: EqSet<TyparConstraintG<'ty>>
        /// The member's attributes, resolved and constant-folded.
        Attributes: TAttributes
    }

/// The payload of `TTypeKindG.Union`.
type TUnionG<'ty, 'id, 'body> =
    {
        /// In declaration order, so a case's index is its runtime tag.
        Cases: EqArray<TUnionCaseG<'ty>>
        /// The augmentation members (`with member …` / `static member …`).
        Members: EqArray<TTypeMemberG<'ty, 'id, 'body>>
        /// Each entry pairs a resolved interface type with the bodies of its
        /// `interface … with` block.
        Interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>>
        /// `Struct` is the flat tag-plus-all-case-fields value type; `RefType` is the
        /// reference emission of that same flat shape as a sealed class.
        ValueKind: NominalValueKind
    }

/// The payload of `TTypeKindG.Record`.
type TRecordG<'ty, 'id, 'body> =
    {
        /// The record's payload in declaration order.
        Fields: EqArray<TRecordFieldG<'ty>>
        /// The augmentation members (`with member …` / `static member …`).
        Members: EqArray<TTypeMemberG<'ty, 'id, 'body>>
        /// Each entry pairs a resolved interface type with the bodies of its
        /// `interface … with` block.
        Interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>>
        ValueKind: NominalValueKind
    }

/// One `[static] let [mutable] x = <init>` of a class preamble, lowered to backing storage the
/// class initialises: static storage for `static let`, instance storage otherwise. A backend
/// picks its own visibility for it. One `let`, one field, so references to it are
/// `FieldGet`/`FieldSet` on `this`, never a `TExpr.Let`.
type TClassLetG<'ty, 'body> =
    {
        Name: string
        Type: 'ty
        IsMutable: bool
        Init: 'body
    }

/// One entry of a class preamble, in DECLARATION order, because interleaving is order-sensitive
/// (`static let a = f()` / `static do g a` / `static let b = h()`).
[<RequireQualifiedAccess>]
type TPreambleEntryG<'ty, 'body> =
    | Let of TClassLetG<'ty, 'body>
    | Do of 'body

/// One `let`-preamble binding inside a secondary constructor body
/// (`new(args) = let x = e in SelfType(...)`). `BoundVar` is the local's identity: codegen
/// allocates a local slot, and a reference to the name in the body loads it.
type TCtorLetG<'ty, 'id, 'body> =
    {
        BoundVar: BoundVarKeyG<'id>
        Type: 'ty
        Init: 'body
    }

/// One `field = expr` initialiser of a secondary constructor's explicit field-init block
/// (`new(s) = { stack = s; started = false }`). `Field` identifies a declared instance field (an
/// explicit `val` or a primary-ctor backing field); `Init` is stored into it via `stfld`.
type TCtorFieldInitG<'body> = { Field: string; Init: 'body }

/// What a secondary constructor's body does after its `let` preamble.
[<RequireQualifiedAccess>]
type TSecondaryCtorBodyG<'body> =
    /// `new(args) = SelfType(...)`: the arguments chained to the primary `.ctor`. `this` is
    /// not yet constructed at this point.
    | Chain of primaryArgs: EqArray<'body>
    /// `new(args) = { f = e; … }`: stores into declared instance fields, with no chain.
    | ExplicitFieldInit of fieldInits: EqArray<TCtorFieldInitG<'body>>

/// A secondary constructor, emitted as a `.ctor` overload.
type TSecondaryCtorG<'ty, 'id, 'body> =
    {
        Params: EqArray<BoundVarKeyG<'id> * 'ty>
        Lets: EqArray<TCtorLetG<'ty, 'id, 'body>>
        Body: TSecondaryCtorBodyG<'body>
    }

/// An `inherit Base(args)` invocation: the primary `.ctor` chains to the parent's
/// (`ldarg.0; <Args>; call instance void Base::.ctor(…)`) before storing its own fields.
/// `CtorParams` are the *derived* class's primary-ctor params, because `this` isn't constructed yet.
type TBaseCtorCallG<'ty, 'id, 'body> =
    {
        CtorParams: EqArray<BoundVarKeyG<'id> * 'ty>
        Args: EqArray<'body>
        /// The chosen base `.ctor`'s identity for an EXTERNAL base (`inherit exn(msg)`).
        /// `ValueNone` for a project-local base, and for an external base whose overload
        /// identity was never recorded.
        ChosenCtor: SymbolKey voption
    }

/// A class's `inherit` clause on the typed node: the admitted parent, and the primary
/// `.ctor`'s chain to it. `Ctor` is `ValueNone` when the clause carries no argument list
/// (the `val`-field form's secondaries chain themselves).
type TBaseG<'ty, 'id, 'body> =
    {
        Parent: BaseParentG<'ty>
        Ctor: TBaseCtorCallG<'ty, 'id, 'body> voption
    }

/// `Signature` is the curried function type. `MethodTypeParams` are the method's own generic
/// parameters in source order (`["'C"]` for `abstract Map<'C> : 'A -> 'C`): the names as written,
/// whereas in `Signature` they appear as `TyTypar(Method, i)`, the declaring type's as
/// `TyTypar(Declaring, i)`.
type TAbstractMethodG<'ty> =
    {
        Name: string
        MethodTypeParams: EqArray<string>
        /// The bounds on `MethodTypeParams`, indexed on the method axis.
        MethodTyparConstraints: EqSet<TyparConstraintG<'ty>>
        Signature: 'ty
        /// The argument names the signature spells, one per source argument across every
        /// curried group in source order; `ValueNone` for an argument written as a bare type.
        ParamNames: EqArray<string voption>
        /// The same three forms a concrete member takes: `Property` for an arg-less member sig
        /// (`abstract member Current: int`, emitted as a `get_Current` slot), `Accessor` for one
        /// half of `abstract P: int with get, set`, and `Method` for a slot keeping its bare name.
        Kind: TMemberKind
        /// `static abstract Op: …`. Declaring one is diagnosed `NotYetSupported`, so codegen
        /// sees only `false`.
        IsStatic: bool
    }

/// The payload of `TTypeKindG.Class`. No `'tok`: a class bears no token of its own, `Enum`'s
/// case identifiers being the only tokens under a type declaration.
type TClassG<'ty, 'id, 'body> =
    {
        /// The explicit `val [mutable] x: T` instance fields.
        Fields: EqArray<TRecordFieldG<'ty>>
        /// The primary constructor's parameters, borrowing the record-field shape.
        CtorParams: EqArray<TRecordFieldG<'ty>>
        Members: EqArray<TTypeMemberG<'ty, 'id, 'body>>
        Base: TBaseG<'ty, 'id, 'body> voption
        Interfaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>>
        Declared: DeclaredClassFlags
        /// `static let` / `static do`, in declaration order: the body of the synthesised `.cctor`.
        StaticPreamble: EqArray<TPreambleEntryG<'ty, 'body>>
        /// Instance `let` / `do`, in declaration order: the END of the primary ctor,
        /// running after the base-ctor call and the ctor-param field stores.
        InstancePreamble: EqArray<TPreambleEntryG<'ty, 'body>>
        /// The `this` bound variable, on the class and not only on each member because the INSTANCE
        /// preamble reads fields through it too: a ctor-param reference in an initialiser is
        /// a `FieldGet` on a `Var` of this key.
        ThisKey: BoundVarKeyG<'id>
        SecondaryCtors: EqArray<TSecondaryCtorG<'ty, 'id, 'body>>
        ValueKind: ClassValueKind
        /// True when the class declares a *primary* constructor (`type T(args) =`, including
        /// `type T() =`); false for the `val`-field form (`type T = val …; new(…) = { … }`),
        /// whose secondaries ARE the ctors, because a synthesised primary would collide with `new()`.
        HasPrimaryCtor: bool
    }

[<RequireQualifiedAccess>]
type TTypeKindG<'ty, 'tok, 'id, 'body> =
    | Interface of methods: EqArray<TAbstractMethodG<'ty>>
    | Union of TUnionG<'ty, 'id, 'body>
    | Record of TRecordG<'ty, 'id, 'body>
    | Class of TClassG<'ty, 'id, 'body>
    /// `cases` in declaration order, each pairing a case identifier with its **resolved**
    /// compile-time literal (`| C = v`). An enum is `'ty`-free: a case value is an integer or
    /// string literal, never a typed term. Numeric / string / mixed is derived, not stored.
    | Enum of cases: EqArray<TEnumCaseG<'tok>>
    /// `type t = body`, a transparent alias: `body` is the right-hand side over the
    /// declaration's own typars, and a use of the name expands to it. A backend emits nothing
    /// for this kind.
    | Abbrev of body: 'ty
    /// A `[<Measure>]` declaration. `term` is the measure it stands for, over base-measure
    /// atoms: a base measure's own atom, or an abbreviation's expanded body. A backend emits
    /// nothing for this kind.
    | Measure of term: MeasureTerm

/// A declared type parameter as the frozen contract carries it.
type TTypeParam =
    {
        /// Source-text name, leading `'`/`^` included (`'a`).
        Name: string
        Kind: TyparKind
    }

[<RequireQualifiedAccess>]
module TTypeParam =

    let ofDeclared (ts: EqArray<DeclaredTypar>) : EqArray<TTypeParam> =
        ts |> EqArray.map (fun t -> { Name = t.Name; Kind = t.Kind })

    let names (ps: EqArray<TTypeParam>) : EqArray<string> = ps |> EqArray.map (fun p -> p.Name)

    let kinds (ps: EqArray<TTypeParam>) : EqArray<TyparKind> = ps |> EqArray.map (fun p -> p.Kind)

/// `'body` abstracts how a member/preamble/ctor BODY is carried: either the expression tree
/// itself (`TExprG<'ty,'tok,'id>`) or a dense id identifying that expression in a pool.
type TTypeDeclG<'ty, 'tok, 'id, 'body> =
    {
        /// Simple (unqualified) type name, e.g. `"Fun"`, never `` `arity ``-mangled: the
        /// suffix belongs to the metadata name (`` Fun`2 ``) instead.
        Name: string
        /// The type's stable nominal identity, carried into the backend so the emitted-type
        /// tables key off it instead of re-deriving a string.
        TypeKey: TypeKey
        /// Declared type parameters in source order.
        TypeParams: EqArray<TTypeParam>
        /// The bounds on `TypeParams`, declared or inferred from a member body, indexed on
        /// the declaring axis.
        TyparConstraints: EqSet<TyparConstraintG<'ty>>
        Kind: TTypeKindG<'ty, 'tok, 'id, 'body>
        /// The declaration's attributes, resolved and constant-folded. The equality /
        /// comparison / qualified-access verdicts are views over this list.
        Attributes: TAttributes
    }

    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey

    member this.DefnKind: TypeDefnKind =
        match this.Kind with
        | TTypeKindG.Record r -> TypeDefnKind.ofRecord r.ValueKind.IsValueType
        | TTypeKindG.Union u -> TypeDefnKind.ofUnion u.ValueKind.IsValueType
        | TTypeKindG.Class c -> TypeDefnKind.ofClass c.ValueKind.IsValueType
        | TTypeKindG.Enum _ -> TypeDefnKind.Enum
        | TTypeKindG.Abbrev _ -> TypeDefnKind.Abbrev
        | TTypeKindG.Measure _ -> TypeDefnKind.Measure
        | TTypeKindG.Interface _ -> TypeDefnKind.Interface

    /// `[<RequireQualifiedAccess>]`, type-level so it covers records AND unions: a bare
    /// `{ X = … }` or a bare case name does not resolve to this type.
    member this.IsRequireQualifiedAccess: bool =
        AttributeVerdicts.isRequireQualifiedAccess this.Attributes

    /// The attribute-decided verdict, else the kind's default: `Reference` for a reference
    /// class / interface, `Structural` for every data kind.
    member this.EqualitySupport: EqualityVerdict =
        AttributeVerdicts.equalitySupport this.DefnKind this.Attributes

    /// The attribute-decided verdict, else `NoComparison`: comparison is opt-in.
    member this.ComparisonSupport: ComparisonVerdict =
        AttributeVerdicts.comparisonSupport this.DefnKind this.Attributes

[<RequireQualifiedAccess>]
type TDeclG<'ty, 'tok, 'id> =
    | Let of pattern: TPatG<'ty, 'tok, 'id> * value: TExprG<'ty, 'tok, 'id> * isInline: bool * ty: 'ty
    | Expression of expr: TExprG<'ty, 'tok, 'id> * ty: 'ty
    | Type of TTypeDeclG<'ty, 'tok, 'id, TExprG<'ty, 'tok, 'id>>

[<RequireQualifiedAccess>]
module TTypeKindG =
    /// The augmentation / instance members a type kind carries.
    let members (kind: TTypeKindG<'ty, 'tok, 'id, 'body>) : EqArray<TTypeMemberG<'ty, 'id, 'body>> =
        match kind with
        | TTypeKindG.Class c -> c.Members
        | TTypeKindG.Union u -> u.Members
        | TTypeKindG.Record r -> r.Members
        | TTypeKindG.Interface _
        | TTypeKindG.Enum _
        | TTypeKindG.Abbrev _
        | TTypeKindG.Measure _ -> EqArray.empty

    /// The `interface IFace with member …` bodies a type kind carries, flattened across
    /// every implemented interface. With `members`, every member body under a type decl.
    let interfaceMembers (kind: TTypeKindG<'ty, 'tok, 'id, 'body>) : TTypeMemberG<'ty, 'id, 'body> seq =
        let flatten (ifaces: EqArray<'ty * EqArray<TTypeMemberG<'ty, 'id, 'body>>>) =
            seq {
                for (_, ms) in EqArray.toArray ifaces do
                    yield! EqArray.toArray ms
            }

        match kind with
        | TTypeKindG.Class c -> flatten c.Interfaces
        | TTypeKindG.Union u -> flatten u.Interfaces
        | TTypeKindG.Record r -> flatten r.Interfaces
        | TTypeKindG.Interface _
        | TTypeKindG.Enum _
        | TTypeKindG.Abbrev _
        | TTypeKindG.Measure _ -> Seq.empty

/// One projection per way a definition site comes to exist: a pattern introduces it, an
/// expression introduces it with no pattern behind it, a declaration MINTS it because no
/// node spells it, or a pool interned it under a dense id.
module BoundVarKey =

    /// The single bound variable a PATTERN introduces. `ValueNone` for one that binds nothing, and
    /// for one that binds only through nested sub-patterns, so walk the children for those.
    let ofPat (p: TPatG<'ty, 'tok, 'id>) : BoundVarKeyG<'id> voption =
        match p with
        | TPatG.NamedSimple(boundVar = boundVar) -> ValueSome(BoundVar boundVar)
        | TPatG.Wildcard _
        | TPatG.Tuple _
        | TPatG.Const _
        | TPatG.Record _
        | TPatG.Union _
        | TPatG.TypeTestAs _
        | TPatG.Null _
        | TPatG.EnumCase _
        | TPatG.Or _ -> ValueNone

    /// The bound variable an EXPRESSION introduces with no pattern node behind it, TOGETHER with
    /// the token that spells its name: a `ForTo` loop variable, whose `i` token has no
    /// surrounding `Pat` in the CST. Every other binding expression carries a real `TPatG`.
    let siteOfExpr (e: TExprG<'ty, 'tok, 'id>) : struct (BoundVarKeyG<'id> * 'tok) voption =
        match e with
        | TExprG.ForTo(var = var; identTok = identTok) -> ValueSome(struct (BoundVar var, identTok))
        | _ -> ValueNone

    /// The bound variable an EXPRESSION introduces: `siteOfExpr` without the token.
    let ofExpr (e: TExprG<'ty, 'tok, 'id>) : BoundVarKeyG<'id> voption =
        siteOfExpr e |> ValueOption.map (fun (struct (b, _)) -> b)

    /// `ofPat` before the tree exists: the bound variable a CST pattern introduces. The key is the
    /// INNERMOST `NamedSimple`'s, after peeling `[<…>] p` / `(p)` / `p : t` / `p as x`, because the
    /// elaborated tree drops those: a key off `let (x) = 5`'s pattern points to a node nothing binds.
    let rec siteOfCstPat (p: Pat<SyntaxToken>) : BoundVarSite voption =
        match p with
        | Pat.NamedSimple t ->
            ValueSome
                {
                    BoundVar = BoundVar(CstKeys.ofPat p)
                    Tok = t
                }
        | Pat.Attributed(pat = inner)
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner)
        | Pat.As(pat = inner) -> siteOfCstPat inner
        | _ -> ValueNone

    /// The bound variable a CST pattern introduces: `siteOfCstPat` without the token.
    let ofCstPat (p: Pat<SyntaxToken>) : BoundVarKey voption =
        siteOfCstPat p |> ValueOption.map (fun s -> s.BoundVar)

    /// The `this` bound variable a type declaration introduces, shared by every member body and by
    /// the instance preamble. `type C() =` writes no `this` token, so the key is MINTED from
    /// the declaration's own.
    let ofDeclaredThis (declKey: NodeKey) : BoundVarKey =
        BoundVar(NodeKey.ofSynthetic declKey.Offset NodeKind.SynthThisBinding)

    /// The `base` bound variable of a type declaration, minted from the declaration exactly as the
    /// `this` one is and distinguished only by kind. Always allocated; only read when the
    /// class has an `inherit` clause.
    let ofDeclaredBase (declKey: NodeKey) : BoundVarKey =
        BoundVar(NodeKey.ofSynthetic declKey.Offset NodeKind.SynthBaseBinding)

    /// The bound variable a pool INTERNED under this dense id. Every `BoundVarId` is a definition
    /// site, so this needs no `voption`. Needed at DESERIALIZATION, where a decl's key slots are
    /// read back with no key to project from.
    let ofInterned (id: BoundVarId) : BoundVarKeyG<BoundVarId> = BoundVar id

    /// Every bound variable a TYPE DECLARATION introduces with no pattern node to introduce it. A
    /// member body references `this` and its parameters by `TExpr.Var`, but those definition
    /// sites are key SLOTS on the shape, which a walk over PATTERNS alone never reaches.
    let ofTypeDecl (td: TTypeDeclG<'ty, 'tok, 'id, 'body>) : BoundVarKeyG<'id> seq =
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
                // The class-wide `this`: a definition site of the preamble bodies as well as
                // of the members that carry their own copy.
                yield c.ThisKey

                for sc in EqArray.toArray c.SecondaryCtors do
                    for (k, _) in EqArray.toArray sc.Params do
                        yield k

                    for l in EqArray.toArray sc.Lets do
                        yield l.BoundVar

                match c.Base with
                | ValueSome { Ctor = ValueSome bc } ->
                    for (k, _) in EqArray.toArray bc.CtorParams do
                        yield k
                | _ -> ()
            | TTypeKindG.Interface _
            | TTypeKindG.Union _
            | TTypeKindG.Record _
            | TTypeKindG.Enum _
            | TTypeKindG.Abbrev _
            | TTypeKindG.Measure _ -> ()
        }

    /// Widen to the tree's own identity axis, for a lookup driven by a REFERENCE: a
    /// `TExpr.Var` identifies its bound variable by that axis.
    let identity (BoundVar k) : 'id = k

    /// Re-file a bound variable into ANOTHER identity space: `f` returns the identity the bound
    /// variable takes there (interning it into a pool, widening a dense id back to a key).
    let refile (f: BoundVarKeyG<'a> -> 'b) (k: BoundVarKeyG<'a>) : BoundVarKeyG<'b> = BoundVar(f k)

    /// A whole bound-variable-keyed table read in the REFERENCE domain: a lookup driven by a
    /// `TExpr.Var` has only the raw identity the reference carries.
    let widenMap (m: Map<BoundVarKeyG<'id>, 'v>) : Map<'id, 'v> =
        m |> Map.toSeq |> Seq.map (fun (b, v) -> identity b, v) |> Map.ofSeq
