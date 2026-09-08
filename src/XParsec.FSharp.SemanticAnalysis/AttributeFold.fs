namespace XParsec.FSharp.SemanticAnalysis

open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The declaration element an attribute set is written on, classified as fsc classifies
/// elements for `[<AttributeUsage>]` enforcement (each case's flag set below is fsc's, read
/// off FS0842's wording).
[<RequireQualifiedAccess>]
type AttrTarget =
    | Class
    | Struct
    | Enum
    | Interface
    /// A `type X = Y` alias, which fsc admits at every type kind.
    | Abbreviation
    /// A method member, static or instance.
    | Method
    /// A property member, static or instance, including an auto-property.
    | Property
    | RecordField
    | UnionCase
    | EnumCase
    /// A module-level `let` or `val` whose compiled shape is a non-function value.
    | ModuleValue
    /// A module-level `let` or `val` function, or a generalised (explicitly generic) value —
    /// fsc classifies both as methods.
    | ModuleFunction
    /// A position with no element classification (an abstract member signature, a
    /// splice with no CST site); enforcement passes.
    | Unchecked

[<RequireQualifiedAccess>]
module AttrTarget =

    /// The `AttributeTargetFlags` the element occupies; `ValueNone` for `Unchecked`.
    let mask (t: AttrTarget) : int voption =
        match t with
        | AttrTarget.Class -> ValueSome AttributeTargetFlags.Class
        | AttrTarget.Struct -> ValueSome AttributeTargetFlags.Struct
        | AttrTarget.Enum -> ValueSome AttributeTargetFlags.Enum
        | AttrTarget.Interface -> ValueSome AttributeTargetFlags.Interface
        | AttrTarget.Abbreviation ->
            ValueSome(
                AttributeTargetFlags.Class
                ||| AttributeTargetFlags.Struct
                ||| AttributeTargetFlags.Enum
                ||| AttributeTargetFlags.Interface
                ||| AttributeTargetFlags.Delegate
            )
        | AttrTarget.Method -> ValueSome(AttributeTargetFlags.Method ||| AttributeTargetFlags.ReturnValue)
        | AttrTarget.Property ->
            ValueSome(
                AttributeTargetFlags.Property
                ||| AttributeTargetFlags.Event
                ||| AttributeTargetFlags.ReturnValue
            )
        | AttrTarget.RecordField -> ValueSome(AttributeTargetFlags.Property ||| AttributeTargetFlags.Field)
        | AttrTarget.UnionCase -> ValueSome(AttributeTargetFlags.Method ||| AttributeTargetFlags.Property)
        | AttrTarget.EnumCase -> ValueSome AttributeTargetFlags.Field
        | AttrTarget.ModuleValue ->
            ValueSome(
                AttributeTargetFlags.Property
                ||| AttributeTargetFlags.Field
                ||| AttributeTargetFlags.ReturnValue
            )
        | AttrTarget.ModuleFunction -> ValueSome(AttributeTargetFlags.Method ||| AttributeTargetFlags.ReturnValue)
        | AttrTarget.Unchecked -> ValueNone

    /// fsc's classification of a module-level value: a function shape and a generalised
    /// (explicitly generic) value compile as methods, any other value as a property / field.
    let ofModuleValue (isFunctionShaped: bool) (isGeneric: bool) : AttrTarget =
        if isFunctionShaped || isGeneric then
            AttrTarget.ModuleFunction
        else
            AttrTarget.ModuleValue

/// `ResolvedAttributes` → the frozen `TAttributes`: each argument checked through
/// `ConstExprCheck`, each attribute's declared `[<AttributeUsage>]` mask enforced against the
/// `usedOn` element.
module internal AttributeFold =

    /// The written argument list of a construction: `[<A>]` and `[<A()>]` carry none,
    /// `[<A(x, y)>]` one per tuple component, `[<A "s">]` / `[<A(x)>]` exactly one.
    let private argExprs (oc: ObjectConstruction<SyntaxToken>) : Expr<SyntaxToken> list =
        match oc with
        | InterfaceConstruction _ -> []
        | ObjectConstruction(_, e) ->
            match e with
            | Expr.EmptyBlock _ -> []
            | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> List.ofSeq items
            | Expr.EnclosedBlock(expr = single) -> [ single ]
            | direct -> [ direct ]

    /// `Prop = v` in argument position is a named setter; anything else is positional.
    let private splitNamed (ctx: PassContext) (e: Expr<SyntaxToken>) : struct (string voption * Expr<SyntaxToken>) =
        match e with
        | Expr.InfixApp(leftExpr = CstKeys.SingleIdent nameTok; infixOp = op; rightExpr = v) when
            op.Token = Token.OpEquality
            ->
            struct (ValueSome(ctx.NameOf nameTok), v)
        | _ -> struct (ValueNone, e)

    /// The `[<AttributeUsage>]` mask the used attribute's own declaration carries: the local
    /// registry first, then the referenced contracts, as the folded first fixed argument of
    /// the declaration's `AttributeUsage` entry. A declaration without a reachable
    /// `AttributeUsage` is `AttributeTargetFlags.All`.
    let private declaredValidOn (ctx: PassContext) (attrKey: TypeKey) : int =
        let declAttrs =
            match TypeRegistry.tryClassByKey ctx.Types attrKey with
            | ValueSome info -> info.Attributes
            | ValueNone -> ctx.Provider.TryLookupAttributes(SymbolKey.Type attrKey)

        match
            declAttrs
            |> Block.tryFind (fun a -> a.Key = RuntimeNames.attributeUsageAttributeKey)
        with
        | ValueSome usage ->
            let firstFixed =
                Block.toList usage.Args
                |> List.tryPick (fun a ->
                    match a.Name, a.Value with
                    // `AttributeTargets` is an `int`-based enum.
                    | ValueNone, ValueSome(TConstValue.Integral(IntValue.Int32 v)) -> Some v
                    | _ -> None
                )

            match firstFixed with
            | Some m -> m
            | None -> AttributeTargetFlags.All
        | ValueNone -> AttributeTargetFlags.All

    let private enforceTarget (ctx: PassContext) (usedOn: AttrTarget) (entry: ResolvedAttribute) : unit =
        match AttrTarget.mask usedOn with
        | ValueNone -> ()
        | ValueSome element ->
            let validOn = declaredValidOn ctx entry.Key

            if validOn &&& element = 0 then
                ctx.Report(entry.TypeRef.Site.Tok, Kind.AttributeTargetInvalid(element, validOn))

    /// `ValueNone` when any argument is outside the constant domain: every rejected argument
    /// is diagnosed and the attribute is omitted whole, so `Args` never carries a
    /// positionally-shifted list.
    let private foldAttribute (ctx: PassContext) (entry: ResolvedAttribute) : TAttribute voption =
        // The attribute's own type-ref site scopes the arguments' name resolutions.
        let useSite = ctx.UseSiteAt entry.TypeRef.Site.Key

        let args = ResizeArray<TAttributeArg>()
        let mutable allFolded = true

        for arg in argExprs entry.Construction do
            let struct (name, valueExpr) = splitNamed ctx arg

            match ConstExprCheck.check ctx useSite valueExpr with
            | ValueSome e -> args.Add { Name = name; Expr = e }
            | ValueNone -> allFolded <- false

        if allFolded then
            ValueSome
                {
                    Key = entry.Key
                    Args = Block.ofSeq args
                }
        else
            ValueNone

    /// Every resolved attribute whose arguments all folded, in written order, each checked
    /// against `usedOn`. An attribute with a rejected argument was diagnosed and is absent.
    let build (ctx: PassContext) (usedOn: AttrTarget) (attrs: ResolvedAttributes) : TAttributes =
        match attrs.Entries with
        | [] -> Block.empty
        | entries ->
            Block.ofList
                [
                    for entry in entries do
                        enforceTarget ctx usedOn entry

                        match foldAttribute ctx entry with
                        | ValueSome a -> yield a
                        | ValueNone -> ()
                ]

    let resolveAndBuild (ctx: PassContext) (usedOn: AttrTarget) (attrs: Attributes<SyntaxToken> voption) : TAttributes =
        build ctx usedOn (ctx.ResolveAttributes attrs)
