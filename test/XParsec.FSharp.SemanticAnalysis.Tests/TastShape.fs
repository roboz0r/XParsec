module XParsec.FSharp.SemanticAnalysis.Tests.TastShape

open System.Collections.Generic
open System.Text
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis

// Renders the TAST as F#-like text for a test to assert on: `prettyDecl tast.Decls.[0]` gives
// `"let v0 = fun v1 -> ((-v1) + 0)"`. `v0`, `v1`, … are fresh names assigned to `NodeKey`s in
// first-encounter order, so a string survives a source-offset change. Types and offsets dropped.

/// Compiled name → source form, so binary ops render as `(L + R)` and unary minus as `(-X)`.
/// Anything not in this map renders as `(External arg arg)`.
let private opSym =
    [
        "op_Addition", "+"
        "op_Subtraction", "-"
        "op_Multiply", "*"
        "op_Division", "/"
        "op_Modulus", "%"
        "op_LessThan", "<"
        "op_GreaterThan", ">"
        "op_LessThanOrEqual", "<="
        "op_GreaterThanOrEqual", ">="
        "op_Equality", "="
        "op_Inequality", "<>"
        "op_BooleanAnd", "&&"
        "op_BooleanOr", "||"
        "op_ComposeRight", ">>"
        "op_ComposeLeft", "<<"
        "op_PipeRight", "|>"
        "op_PipeLeft", "<|"
    ]
    |> Map.ofList

let private prefixSym = Map.ofList [ "op_UnaryNegation", "-" ]

/// A constant's source form, carrying the AUTHORED KIND as its F# suffix, so a `10us` silently
/// arriving as an `int` is visible in every snapshot. Both halves of an integral constant come
/// off its `IntKind`, so a new kind renders with no edit.
let private constText (v: TConstValue) : string =
    let inv (x: 'a :> System.IFormattable) =
        x.ToString(null, System.Globalization.CultureInfo.InvariantCulture)

    match v with
    | TConstValue.Integral(k, bits) -> IntKind.render k bits + IntKind.suffix k
    | TConstValue.Float n -> inv n
    | TConstValue.Float32 n -> inv n + "f"
    | TConstValue.Decimal d -> inv d + "M"
    | TConstValue.Bool true -> "true"
    | TConstValue.Bool false -> "false"
    | TConstValue.Char c -> "'" + string c + "'"
    | TConstValue.String s -> "\"" + s + "\""
    | TConstValue.Unit -> "()"

/// A key's display name, unwrapped: this module is a HUMAN-facing renderer, which is what a
/// `DisplayName` is for. Nothing here resolves anything by it.
let shownName (key: SymbolKey) : string =
    let (DisplayName name) = SymbolKeyOps.simpleName key
    name

/// `shownName` for a nominal type constructor, which carries the narrow `TypeKey`.
let shownTypeName (key: TypeKey) : string =
    let (DisplayName name) = SymbolKeyOps.typeSimpleName key
    name

/// The declaring type's simple name for a member key — `StaticMethodCall` / `StaticPropertyGet`
/// carry a `SymbolKey.Member`, whose `Decl` is the class. Any other shape falls back to its own.
let private memberDeclName (key: SymbolKey) : string =
    match key with
    | SymbolKey.Member mk -> SymbolKeyOps.bareName mk.Decl.Name
    | _ -> shownName key

/// Minimal `SemType` → readable name, for rendering cast targets (`:>` / `:?` /
/// `:?>`). Nominal types render as their name; structural ones approximate.
let rec private tyName (t: SemType) : string =
    match t with
    | TyConst(key, _) -> shownTypeName key
    | TyVar _ -> "_"
    | TyFun(a, b) -> tyName a + " -> " + tyName b
    | TyTuple ts -> [ for t in ts -> tyName t ] |> String.concat " * "
    | TyRecord(n, _)
    | TyUnion(n, _)
    | TyClass(n, _)
    | TyEnum n -> shownTypeName n
    | TyOr disjuncts -> [ for d in disjuncts.Disjuncts -> tyName d ] |> String.concat " | "
    | TyLiteral(LiteralConst.String s) -> "\"" + s + "\""
    | TyLiteral(LiteralConst.Int n) -> string n
    | TyKeyOf t -> "keyof " + tyName t
    | TyIndexedAccess(objTy, index) -> tyName objTy + "[" + tyName index + "]"
    | TyConditional c ->
        tyName c.Check
        + " extends "
        + tyName c.Extends
        + " ? "
        + tyName c.WhenTrue
        + " : "
        + tyName c.WhenFalse
    | TyUnknown reason -> "?" + reason.Render
    | TyTypar(TyparScope.Type _, i) -> "!" + string i
    | TyTypar(TyparScope.LocalFunction _, i) -> "!local" + string i
    | TyTypar(_, i) -> "!!" + string i

let private (|InfixOp|_|) (e: TExpr) =
    match e with
    | TExpr.App(TExpr.App(TExpr.External(key, _, _), left, _, _), right, _, _) ->
        match Map.tryFind key.Name opSym with
        | Some sym -> Some(sym, left, right)
        | None -> None
    | _ -> None

let private (|PrefixOp|_|) (e: TExpr) =
    match e with
    | TExpr.App(TExpr.External(key, _, _), operand, _, _) ->
        match Map.tryFind key.Name prefixSym with
        | Some sym -> Some(sym, operand)
        | None -> None
    | _ -> None

type private Renderer() =
    let map = Dictionary<NodeKey, string>()
    let mutable counter = 0
    let sb = StringBuilder()

    let nameOf (k: NodeKey) =
        match map.TryGetValue k with
        | true, n -> n
        | false, _ ->
            let n = sprintf "v%d" counter
            counter <- counter + 1
            map.[k] <- n
            n

    let push (s: string) = sb.Append(s) |> ignore

    member _.Result = sb.ToString()

    member this.Expr(e: TExpr) : unit =
        match e with
        | TExpr.Const(cv, _, _) -> push (constText cv)
        | TExpr.Var(k, _, _) -> push (nameOf k)
        | TExpr.External(key, _, _) -> push key.Name
        | TExpr.Unresolved _ -> push "<unresolved>"

        | InfixOp(sym, l, r) ->
            push "("
            this.Expr l
            push " "
            push sym
            push " "
            this.Expr r
            push ")"

        | PrefixOp(sym, x) ->
            push "("
            push sym
            this.Expr x
            push ")"

        | TExpr.App(fn, arg, _, _) ->
            push "("
            this.Expr fn
            push " "
            this.Expr arg
            push ")"

        | TExpr.Lambda(p, body, _, _) ->
            push "fun "
            this.Pat p
            push " -> "
            this.Expr body

        | TExpr.Let({ Pattern = p; Value = v }, b, _, _) ->
            push "let "
            this.Pat p
            push " = "
            this.Expr v
            push " in "
            this.Expr b

        | TExpr.LetGroup(members, _, b, _, _) ->
            members
            |> EqArray.iteri (fun i m ->
                push (if i = 0 then "let rec " else " and ")
                this.Pat m.Pattern
                push " = "
                this.Expr m.Value
            )

            push " in "
            this.Expr b

        | TExpr.Use(p, v, b, _, _, _) ->
            push "use "
            this.Pat p
            push " = "
            this.Expr v
            push " in "
            this.Expr b
        | TExpr.IfThenElse(c, t, e, _, _) ->
            push "if "
            this.Expr c
            push " then "
            this.Expr t
            push " else "
            this.Expr e

        | TExpr.Tuple(items, _, _) ->
            push "("

            items
            |> EqArray.iteri (fun i x ->
                if i > 0 then
                    push ", "

                this.Expr x
            )

            push ")"

        | TExpr.ArrayLit(elems, _, _) ->
            push "[|"

            elems
            |> EqArray.iteri (fun i x ->
                if i > 0 then
                    push "; "

                this.Expr x
            )

            push "|]"

        | TExpr.Sequential(items, _, _) ->
            push "("

            items
            |> EqArray.iteri (fun i x ->
                if i > 0 then
                    push "; "

                this.Expr x
            )

            push ")"

        | TExpr.While(c, b, _, _) ->
            push "while "
            this.Expr c
            push " do "
            this.Expr b

        | TExpr.ForTo(v, _, s, e2, b, _, _) ->
            push "for "
            push (nameOf v)
            push " = "
            this.Expr s
            push " to "
            this.Expr e2
            push " do "
            this.Expr b

        | TExpr.ForIn(p, src, b, _, _, _) ->
            push "for "
            this.Pat p
            push " in "
            this.Expr src
            push " do "
            this.Expr b

        | TExpr.Match(scrutinee, arms, _, _) ->
            push "match "
            this.Expr scrutinee
            push " with"

            for arm in arms do
                push " | "
                this.Pat arm.Pat

                match arm.Guard with
                | ValueSome g ->
                    push " when "
                    this.Expr g
                | ValueNone -> ()

                push " -> "
                this.Expr arm.Body

        | TExpr.TryWith(body, arms, _, _) ->
            push "try "
            this.Expr body
            push " with"

            for arm in arms do
                push " | "
                this.Pat arm.Pat

                match arm.Guard with
                | ValueSome g ->
                    push " when "
                    this.Expr g
                | ValueNone -> ()

                push " -> "
                this.Expr arm.Body

        | TExpr.TryFinally(body, cleanup, _, _) ->
            push "try "
            this.Expr body
            push " finally "
            this.Expr cleanup

        | TExpr.Assignment(lhs, rhs, _, _) ->
            this.Expr lhs
            push " <- "
            this.Expr rhs

        | TExpr.Null(_, _) -> push "null"

        | TExpr.Range(s, stepOpt, e2, _, _) ->
            push "("
            this.Expr s
            push ".."

            match stepOpt with
            | Some step ->
                this.Expr step
                push ".."
            | None -> ()

            this.Expr e2
            push ")"

        | TExpr.RecordCons(fields, _, _) ->
            push "{ "

            fields
            |> EqArray.iteri (fun i (n, v) ->
                if i > 0 then
                    push "; "

                push n
                push " = "
                this.Expr v
            )

            push " }"

        | TExpr.RecordClone(src, overrides, _, _) ->
            push "{ "
            this.Expr src
            push " with "

            overrides
            |> EqArray.iteri (fun i (n, v) ->
                if i > 0 then
                    push "; "

                push n
                push " = "
                this.Expr v
            )

            push " }"

        | TExpr.FieldGet(objArg, name, _, _) ->
            this.Expr objArg
            push "."
            push name

        | TExpr.FieldSet(objArg, name, value, _, _) ->
            this.Expr objArg
            push "."
            push name
            push " <- "
            this.Expr value

        | TExpr.UnionCons(caseName, args, _, _) ->
            push caseName

            if args.Length = 0 then
                ()
            elif args.Length = 1 then
                push " "
                this.Expr args.[0]
            else
                push "("

                args
                |> EqArray.iteri (fun i a ->
                    if i > 0 then
                        push ", "

                    this.Expr a
                )

                push ")"

        | TExpr.New(className, _, args, _, _) ->
            push "new "
            push className
            push "("

            args
            |> EqArray.iteri (fun i a ->
                if i > 0 then
                    push ", "

                this.Expr a
            )

            push ")"

        | TExpr.MethodCall(objArg, key, via, args, _, _) ->
            this.Expr objArg
            // `base.M(…)` renders `^`, a constrained call on a typar coerced to an interface `:`,
            // so both read distinctly from a virtual `this.M(…)`.
            push (
                match via with
                | CallVia.Base -> "^"
                | CallVia.Self -> "."
                | CallVia.Interface _ -> ":"
            )

            push (shownName key)
            push "("

            args
            |> EqArray.iteri (fun i a ->
                if i > 0 then
                    push ", "

                this.Expr a
            )

            push ")"

        | TExpr.PropertyGet(objArg, key, via, _, _) ->
            this.Expr objArg

            push (
                match via with
                | CallVia.Base -> "^"
                | CallVia.Self -> "."
                | CallVia.Interface _ -> ":"
            )

            push (shownName key)

        | TExpr.StaticMethodCall(key, _, args, _, _) ->
            push (memberDeclName key)
            push "."
            push (shownName key)
            push "("

            args
            |> EqArray.iteri (fun i a ->
                if i > 0 then
                    push ", "

                this.Expr a
            )

            push ")"

        | TExpr.StaticPropertyGet(key, _, _, _) ->
            push (memberDeclName key)
            push "."
            push (shownName key)

        | TExpr.StaticFieldGet(declKey, name, _, _) ->
            push (shownTypeName declKey)
            push "."
            push name

        | TExpr.StaticFieldSet(declKey, name, value, _, _) ->
            push (shownTypeName declKey)
            push "."
            push name
            push " <- "
            this.Expr value

        | TExpr.Format(sink, segments, _, _) ->
            let sinkStr =
                match sink with
                | FormatSink.ToStdOut nl -> if nl then "stdoutln" else "stdout"
                | FormatSink.ToStdErr nl -> if nl then "stderrln" else "stderr"
                | FormatSink.ToWriter(_, nl) -> if nl then "writerln" else "writer"
                | FormatSink.ToBuilder _ -> "builder"
                | FormatSink.ToString -> "string"

            push "format:"
            push sinkStr
            push "["

            segments
            |> EqArray.toList
            |> List.iteri (fun i seg ->
                if i > 0 then
                    push "; "

                match seg with
                | FormatSeg.Lit s ->
                    push "\""
                    push s
                    push "\""
                | FormatSeg.Hole(_, arg) ->
                    push "{"
                    this.Expr arg
                    push "}"
                | FormatSeg.DynHole d ->
                    push "{"

                    d.Width
                    |> ValueOption.iter (fun w ->
                        push "*="
                        this.Expr w
                        push " "
                    )

                    d.Precision
                    |> ValueOption.iter (fun p ->
                        push ".*="
                        this.Expr p
                        push " "
                    )

                    this.Expr d.Value
                    push "}"
                | FormatSeg.CallbackHole(_, residue) ->
                    push "{cb="
                    this.Expr residue
                    push "}"
            )

            push "]"

        | TExpr.ExternalMember(objArg, _, name, _, _, _, _) ->
            match objArg with
            | ValueSome r ->
                this.Expr r
                push "."
                push name
            | ValueNone -> push name
        | TExpr.ILIntrinsic(opCode, _, args, _, _) ->
            push "(# \""
            push opCode
            push "\""

            for a in args do
                push " "
                this.Expr a

            push " #)"

        | TExpr.StaticOptimization(clauses, def, _, _) ->
            push "staticopt["
            this.Expr def

            for cl in clauses do
                push "; when "
                push (string cl.Constraints.Length)
                push " -> "
                this.Expr cl.Body

            push "]"

        | TExpr.Upcast(source, ty, _) ->
            push "("
            this.Expr source
            push " :> "
            push (tyName ty)
            push ")"
        | TExpr.Downcast(source, ty, _) ->
            push "("
            this.Expr source
            push " :?> "
            push (tyName ty)
            push ")"
        | TExpr.TypeTest(source, testTy, _, _) ->
            push "("
            this.Expr source
            push " :? "
            push (tyName testTy)
            push ")"
        | TExpr.TraitCall(supportTys, memberName, args, _, _) ->
            push (supportTys |> EqArray.toArray |> Array.map tyName |> String.concat " or ")
            push "."
            push memberName
            push "("

            args
            |> EqArray.iteri (fun i a ->
                if i > 0 then
                    push ", "

                this.Expr a
            )

            push ")"

        // The entry is a separate root, so render the SLOT and the call's own args: inlining the
        // body here would print one body per call site.
        | TExpr.InlineCall(spec = SpecializationId spec; args = args) ->
            push "spec#"
            push (string spec)
            push "("

            args
            |> EqArray.iteri (fun i a ->
                if i > 0 then
                    push ", "

                this.Expr a
            )

            push ")"

        // Rendered, not elided: the marker says the subtree was written in the CALLER's file,
        // and a rendering that hid it would show a fused entry as if it were homogeneous.
        | TExpr.CallerExpr(body = body) ->
            push "caller<"
            this.Expr body
            push ">"

    member this.Pat(p: TPat) : unit =
        match p with
        | TPat.NamedSimple(k, _, _, isMutable) ->
            if isMutable then
                push "mutable "

            push (nameOf k)
        | TPat.Wildcard _ -> push "_"
        | TPat.Const(cv, _, _) -> push (constText cv)
        | TPat.Tuple(items, _, _) ->
            push "("

            items
            |> EqArray.iteri (fun i x ->
                if i > 0 then
                    push ", "

                this.Pat x
            )

            push ")"

        | TPat.Record(fields, _, _) ->
            push "{ "

            fields
            |> EqArray.iteri (fun i (n, p) ->
                if i > 0 then
                    push "; "

                push n
                push " = "
                this.Pat p
            )

            push " }"

        | TPat.Union(caseName, fields, _, _) ->
            push caseName

            if fields.Length = 0 then
                ()
            elif fields.Length = 1 then
                push " "
                this.Pat fields.[0]
            else
                push "("

                fields
                |> EqArray.iteri (fun i p ->
                    if i > 0 then
                        push ", "

                    this.Pat p
                )

                push ")"

        | TPat.TypeTestAs(testTy, inner, _, _) ->
            push ":? "
            push (tyName testTy)
            push " as "
            this.Pat inner
        | TPat.Null _ -> push "null"
        | TPat.EnumCase(enumKey, caseName, _, _) ->
            // Renders identically to the `E.C1` expression form (`StaticFieldGet`).
            push (shownTypeName enumKey)
            push "."
            push caseName
        | TPat.Or(alts, _, _) ->
            alts
            |> EqArray.iteri (fun i p ->
                if i > 0 then
                    push " | "

                this.Pat p
            )

    member this.Decl(d: TDecl) : unit =
        match d with
        | TDecl.Let({ Pattern = p; Value = v }, isInline, _) ->
            push (if isInline then "let inline " else "let ")
            this.Pat p
            push " = "
            this.Expr v
        | TDecl.LetGroup(members, _) ->
            members
            |> EqArray.iteri (fun i m ->
                push (if i = 0 then "let rec " else " and ")
                this.Pat m.Pattern
                push " = "
                this.Expr m.Value
            )
        | TDecl.Expression(e, _) ->
            push "do "
            this.Expr e
        | TDecl.Type td ->
            let rec tyStr t =
                match t with
                | TyConst(key, _) -> shownTypeName key
                | TyVar _ -> "_"
                | TyFun(a, b) -> tyStr a + " -> " + tyStr b
                | TyTuple ts -> [ for t in ts -> tyStr t ] |> String.concat " * "
                | TyRecord(n, _)
                | TyUnion(n, _)
                | TyClass(n, _)
                | TyEnum n -> shownTypeName n
                | TyOr disjuncts -> [ for d in disjuncts.Disjuncts -> tyStr d ] |> String.concat " | "
                | TyLiteral(LiteralConst.String s) -> "\"" + s + "\""
                | TyLiteral(LiteralConst.Int n) -> string n
                | TyKeyOf t -> "keyof " + tyStr t
                | TyIndexedAccess(objTy, index) -> tyStr objTy + "[" + tyStr index + "]"
                | TyConditional c ->
                    tyStr c.Check
                    + " extends "
                    + tyStr c.Extends
                    + " ? "
                    + tyStr c.WhenTrue
                    + " : "
                    + tyStr c.WhenFalse
                | TyUnknown reason -> "?" + reason.Render
                | TyTypar(TyparScope.Type _, i) -> "!" + string i
                | TyTypar(TyparScope.LocalFunction _, i) -> "!local" + string i
                | TyTypar(_, i) -> "!!" + string i

            push "type "

            match SymbolKeyOps.typeNs td.TypeKey with
            | "" -> ()
            | ns ->
                push ns
                push "."

            push td.Name

            if not td.TypeParams.IsEmpty then
                push "<"
                push (String.concat ", " (EqArray.toList (TTypeParam.names td.TypeParams)))
                push ">"

            match td.Kind with
            | TTypeKind.Interface methods ->
                push " = interface"

                for m in methods do
                    push " member "
                    push m.Name
                    push " : "
                    push (tyStr m.Signature)

                push " end"
            | TTypeKind.Union u ->
                push " ="

                for c in u.Cases do
                    push " | "
                    push c.Name

                    if c.Fields.Length > 0 then
                        push " of "
                        push ([ for (_, t) in c.Fields -> tyStr t ] |> String.concat " * ")

                for m in u.Members do
                    push (if m.IsStatic then " static member " else " member ")
                    push m.Name
                    push " : "
                    push (tyStr m.ReturnTy)
            | TTypeKind.Record r ->
                push " = { "

                r.Fields
                |> EqArray.iteri (fun i f ->
                    if i > 0 then
                        push "; "

                    if f.IsMutable then
                        push "mutable "

                    push f.Name
                    push " : "
                    push (tyStr f.Type)
                )

                push " }"

                for m in r.Members do
                    push (if m.IsStatic then " static member " else " member ")
                    push m.Name
                    push " : "
                    push (tyStr m.ReturnTy)
            | TTypeKind.Class c ->
                let ctorParams = c.CtorParams
                let members = c.Members
                let secondaryCtors = c.SecondaryCtors

                if c.Declared.IsSealed then
                    push "[<Sealed>] "

                push "("

                ctorParams
                |> EqArray.iteri (fun i p ->
                    if i > 0 then
                        push ", "

                    push p.Name
                    push " : "
                    push (tyStr p.Type)
                )

                push ") = class"

                let pushPreamble (prefix: string) (entries: EqArray<TPreambleEntry>) =
                    for entry in entries do
                        match entry with
                        | TPreambleEntry.Let l ->
                            push prefix
                            push "let "

                            if l.IsMutable then
                                push "mutable "

                            push l.Name
                            push " : "
                            push (tyStr l.Type)
                        | TPreambleEntry.Do _ ->
                            push prefix
                            push "do"

                pushPreamble " static " c.StaticPreamble
                pushPreamble " " c.InstancePreamble

                for m in members do
                    push (if m.IsStatic then " static member " else " member ")
                    push m.Name
                    push " : "
                    push (tyStr m.ReturnTy)

                for sc in secondaryCtors do
                    push " new("

                    sc.Params
                    |> EqArray.iteri (fun i (_, ty) ->
                        if i > 0 then
                            push ", "

                        push (tyStr ty)
                    )

                    push ")"

                push " end"
            | TTypeKind.Enum cases ->
                // Render the derived variant tag + each case's resolved literal,
                // so the snapshot pins classification *and* the preserved width.
                let variant =
                    match TEnumCases.classify cases with
                    | ValueSome TEnumVariant.Numeric -> "numeric"
                    | ValueSome TEnumVariant.String -> "string"
                    | ValueSome TEnumVariant.Mixed -> "mixed"
                    | ValueNone -> "?"

                push " = enum<"
                push variant
                push ">"

                let litStr (lit: TEnumLiteral) : string =
                    match lit with
                    | TEnumLiteral.Int v -> constText v
                    | TEnumLiteral.String s -> "\"" + s + "\""

                for c in cases do
                    push " | "
                    push c.Name
                    push " = "

                    match c.Value with
                    | ValueSome lit -> push (litStr lit)
                    | ValueNone -> push "<unresolved>"
            | TTypeKind.Abbrev body ->
                push " = "
                push (tyStr body)
            | TTypeKind.Measure term ->
                push " = <"
                push (string term)
                push ">"

let prettyExpr (e: TExpr) : string =
    let r = Renderer()
    r.Expr e
    r.Result

let prettyDecl (d: TDecl) : string =
    let r = Renderer()
    r.Decl d
    r.Result
