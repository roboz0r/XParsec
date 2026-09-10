namespace XParsec.FSharp.SemanticAnalysis

open Vesper
open XParsec.FSharp.Lexer

// The CONSTANT tier of the TAST: what an attribute argument and every other constant
// position carries. Types are ground throughout and held as `FrozenType`; a site is an
// `Anchor`, the form carried across assemblies by `TAttributes`.

/// The value a constant expression denotes.
[<RequireQualifiedAccess>]
type TConstResult =
    | Scalar of TConstValue
    | Null
    | TypeVal of FrozenType
    | ArrayVal of Block<TConstResult>

/// A checked constant expression, its operators resolved to the bindings they denote. Every
/// node carries the value it denotes.
[<RequireQualifiedAccess>]
type TConstExpr =
    | Literal of value: TConstValue * ty: FrozenType * tok: Anchor
    /// `null`, which takes its type from the position it fills.
    | Null of ty: FrozenType * tok: Anchor
    /// A reference to a `[<Literal>]` binding, carrying that binding's own value.
    | LiteralRef of binding: BindingKey * result: TConstResult * ty: FrozenType * tok: Anchor
    /// `E.C`, carrying the case's underlying literal. The node's type is `FTEnum enumKey`.
    | EnumCase of enumKey: TypeKey * caseName: string * result: TConstResult * tok: Anchor
    /// `typeof<T>`; `ty` is `System.Type`.
    | TypeOf of operand: FrozenType * ty: FrozenType * tok: Anchor
    /// `nameof x`; `ty` is `string`.
    | NameOf of target: SymbolKey * name: string * ty: FrozenType * tok: Anchor
    /// `[| … |]`; `ty` is the array nominal.
    | ArrayLit of items: Block<TConstExpr> * ty: FrozenType * tok: Anchor
    | Unary of op: BindingKey * operand: TConstExpr * result: TConstResult * ty: FrozenType * tok: Anchor
    | Binary of
        op: BindingKey *
        left: TConstExpr *
        right: TConstExpr *
        result: TConstResult *
        ty: FrozenType *
        tok: Anchor

/// What a constant expression denotes, independent of its source site. `[<A(0x1)>]` and
/// `[<A(1)>]` carry equal denotations.
type TConstDenotation =
    { Result: TConstResult; Ty: FrozenType }

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TConstValue =

    /// The canon primitive a scalar is an instance of: the width an integral holds, the
    /// primitive of every other scalar.
    let canonKey (v: TConstValue) : TypeKey =
        match v with
        | TConstValue.Integral i -> RuntimeNames.intKindKey (IntValue.kind i)
        | TConstValue.Float _ -> RuntimeNames.floatKey
        | TConstValue.Float32 _ -> RuntimeNames.float32Key
        | TConstValue.Bool _ -> RuntimeNames.boolKey
        | TConstValue.Char _ -> RuntimeNames.charKey
        | TConstValue.Decimal _ -> RuntimeNames.decimalKey
        | TConstValue.String _ -> RuntimeNames.stringKey
        | TConstValue.Unit -> RuntimeNames.unitKey

[<RequireQualifiedAccess>]
module TConstResult =

    /// The scalar; `ValueNone` for `null`, a type value and an array.
    let tryScalar (r: TConstResult) : TConstValue voption =
        match r with
        | TConstResult.Scalar v -> ValueSome v
        | TConstResult.Null
        | TConstResult.TypeVal _
        | TConstResult.ArrayVal _ -> ValueNone

[<RequireQualifiedAccess>]
module TConstDenotation =

    /// The scalar; `ValueNone` for `null`, a type value and an array.
    let tryScalar (d: TConstDenotation) : TConstValue voption = TConstResult.tryScalar d.Result

[<RequireQualifiedAccess>]
module TConstExpr =

    let ty (e: TConstExpr) : FrozenType =
        match e with
        | TConstExpr.EnumCase(enumKey = key) -> FTEnum key
        | TConstExpr.Literal(ty = ty)
        | TConstExpr.Null(ty = ty)
        | TConstExpr.LiteralRef(ty = ty)
        | TConstExpr.TypeOf(ty = ty)
        | TConstExpr.NameOf(ty = ty)
        | TConstExpr.ArrayLit(ty = ty)
        | TConstExpr.Unary(ty = ty)
        | TConstExpr.Binary(ty = ty) -> ty

    let rec result (e: TConstExpr) : TConstResult =
        match e with
        | TConstExpr.Literal(value = v) -> TConstResult.Scalar v
        | TConstExpr.Null _ -> TConstResult.Null
        | TConstExpr.TypeOf(operand = t) -> TConstResult.TypeVal t
        | TConstExpr.NameOf(name = n) -> TConstResult.Scalar(TConstValue.String n)
        | TConstExpr.ArrayLit(items = items) -> TConstResult.ArrayVal(Block.map result items)
        | TConstExpr.LiteralRef(result = r)
        | TConstExpr.EnumCase(result = r)
        | TConstExpr.Unary(result = r)
        | TConstExpr.Binary(result = r) -> r

    let denotation (e: TConstExpr) : TConstDenotation = { Result = result e; Ty = ty e }

    /// The scalar the expression denotes; `ValueNone` for `null`, a `typeof<T>` and an array.
    let tryScalar (e: TConstExpr) : TConstValue voption = TConstResult.tryScalar (result e)
