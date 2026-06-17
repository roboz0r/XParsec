namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open JsEmitHelpers

/// The `TAST → JsAst` walker. Every un-handled node is an explicit `failwithf`,
/// so an unsupported arm fails loudly rather than dropping silently.
///
/// Durable conventions:
///   * Functions are **curried unary arrows** — `Lambda` → nested `(a) => (b) => …`,
///     `App` → unary calls (`f a b` → `f(a)(b)`). Tail self-recursion trampolines to
///     `while (true)` with param-shadow mutation for constant stack.
///   * Operator bodies arrive pre-spliced as `ILIntrinsic` `$N`-templates;
///     `finishOps` is identity (templates are already JS-emit-able). `TastLower.lower`
///     drops `type` decls — record/union/member shapes are read off the un-lowered
///     decls in `collectTypes`.
///   * Records/unions emit as data-only JS `class`es (positional ctor; union = base
///     `tag` + one `extends`-subclass per case). Members emit as free, curried,
///     *receiver-first* functions — never prototype methods (match + the structural
///     runtime read `.tag`/own-keys, never `instanceof`).
///   * `Match` lowers to an IIFE testing each arm in order, an unmatched value throwing.
module EmitJs =

    /// Maps a source char offset to 0-based (line, column) — V3 source-map
    /// coordinates. `Starts.[n]` is the char offset at which line `n` begins.
    /// Columns count UTF-16 code units, as V3 maps require.
    type LineIndex = { Starts: int[]; Length: int }

    module LineIndex =
        let build (source: string) : LineIndex =
            let starts = ResizeArray<int>()
            starts.Add 0

            for i in 0 .. source.Length - 1 do
                if source.[i] = '\n' then
                    starts.Add(i + 1)

            {
                Starts = starts.ToArray()
                Length = source.Length
            }

        /// Resolve a char offset to a `JsLoc`. Clamps past-end offsets to the last line.
        let resolve (idx: LineIndex) (offset: int) : JsLoc =
            let offset = max 0 (min offset idx.Length)
            let starts = idx.Starts
            // Binary search for the greatest line start <= offset.
            let mutable lo = 0
            let mutable hi = starts.Length - 1

            while lo < hi do
                let mid = (lo + hi + 1) / 2

                if starts.[mid] <= offset then lo <- mid else hi <- mid - 1

            {
                Line = lo
                Column = offset - starts.[lo]
            }

    /// Resolve a node's `'tok` to a source `loc`. `ValueNone` disables maps
    /// (no source text supplied); `ValueSome` carries the line index.
    type Resolver = LineIndex voption

    /// A record type's JS shape: the emitted class `Name` and its `Fields` in
    /// *declaration* order — `RecordCons`/`RecordClone` reorder source args to match.
    type JsRecordInfo = { Name: string; Fields: string list }

    /// A union type's JS shape: the emitted base-class `Name` and its cases keyed by
    /// F# case name. `UnionCons` and union patterns look up subclass + field order here.
    type JsUnionInfo =
        {
            Name: string
            Cases: System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>
        }

    /// The walker's ambient context.
    type WalkCtx =
        {
            Resolver: Resolver
            Source: string voption
            Records: System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>
            Unions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
            /// External union types (`Option`, `List`) not in the file's `tast.Decls`;
            /// their case shapes are read off the provider on first use and emitted as
            /// nominal JS classes (same base-class + subclass shape a local union gets).
            Provider: IExternalSymbolProvider voption
            /// External unions resolved on demand, keyed by `SymbolKey`. A miss in
            /// `Unions` falls back here; `ExternalUnionDecls` tracks emission order
            /// so `buildProgram` can prepend the classes (JS classes are not hoisted).
            ExternalUnions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
            ExternalUnionDecls: ResizeArray<JsStatement>
            Imports: JsImports
            /// `true` in library mode: top-level `let` emits `export const …`.
            ExportTopLevel: bool
        }

    let private locOf (ctx: WalkCtx) (tok: SyntaxToken) : JsLoc voption =
        match ctx.Resolver with
        | ValueSome idx -> ValueSome(LineIndex.resolve idx tok.StartIndex)
        | ValueNone -> ValueNone

    // ---- Records -------------------------------------------------------------

    /// The nominal `SymbolKey` of a record/union construct's receiver type.
    /// A non-nominal receiver is an invariant break.
    let private nominalKey (what: string) (ty: FrozenType) : SymbolKey =
        match TastLower.receiverShape ty with
        | ValueSome(key, _) -> key
        | ValueNone -> failwithf "EmitJs: %s on non-nominal type %A" what ty

    /// Resolve a `RecordCons` / `RecordClone` / `FieldGet` receiver to its `JsRecordInfo`.
    let private recordInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsRecordInfo =
        let key = nominalKey what ty

        match ctx.Records.TryGetValue key with
        | true, info -> info
        | _ -> failwithf "EmitJs: %s on record with no emitted type (key %A)" what key

    // ---- Unions --------------------------------------------------------------

    /// A union case's declaration-order field names: named fields verbatim; a lone
    /// positional becomes `Item`; multiple positionals become `Item1`/`Item2`/….
    let private synthFieldNames (fieldNames: string voption list) : string list =
        match fieldNames with
        | [ ValueSome n ] -> [ n ]
        | [ ValueNone ] -> [ "Item" ]
        | many ->
            many
            |> List.mapi (fun i nm ->
                match nm with
                | ValueSome n -> n
                | ValueNone -> "Item" + string (i + 1)
            )

    /// Build a `JsUnionInfo` for `baseName` with `(caseName, fieldNames)` in declaration
    /// order: tag = declaration index, subclass = `<baseName>_<case>`.
    let private buildUnionInfo
        (baseName: string)
        (cases: (string * string voption list) list)
        : JsUnionInfo * JsUnionCaseDecl list =
        let caseDecls =
            cases
            |> List.mapi (fun tag (caseName, fieldNames) ->
                {
                    CaseName = caseName
                    ClassName = baseName + "_" + caseName
                    Tag = tag
                    Fields = synthFieldNames fieldNames
                }
            )

        let table = System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>()

        for c in caseDecls do
            table.[c.CaseName] <- c

        { Name = baseName; Cases = table }, caseDecls

    /// Resolve an external union to a `JsUnionInfo` via the provider, queuing its class
    /// decl in `ExternalUnionDecls` and caching in `ExternalUnions`. `ValueNone` when
    /// no provider or the type is not a union — caller fails loudly.
    let private resolveExternalUnion (ctx: WalkCtx) (key: SymbolKey) : JsUnionInfo voption =
        match ctx.ExternalUnions.TryGetValue key with
        | true, info -> ValueSome info
        | _ ->
            match ctx.Provider with
            | ValueNone -> ValueNone
            | ValueSome provider ->
                match ExternalSymbols.tryLookupType provider key with
                | ValueSome(ExternalTypeShape.Union(_, cases, _)) ->
                    let baseName = SymbolKeyOps.simpleName key

                    let info, caseDecls =
                        buildUnionInfo baseName [ for c in cases -> c.Name, List.ofArray c.FieldNames ]

                    ctx.ExternalUnions.[key] <- info
                    ctx.ExternalUnionDecls.Add(JsStatement.Union(baseName, caseDecls))
                    ValueSome info
                | _ -> ValueNone

    /// Resolve a `UnionCons` / union-pattern receiver type + case name to the emitted
    /// `JsUnionCaseDecl`. Falls through to the external-union provider on a local miss.
    let private unionInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsUnionInfo =
        let key = nominalKey what ty

        match ctx.Unions.TryGetValue key with
        | true, info -> info
        | _ ->
            match resolveExternalUnion ctx key with
            | ValueSome info -> info
            | ValueNone -> failwithf "EmitJs: %s on union with no emitted type (key %A)" what key

    let private unionCaseOf (ctx: WalkCtx) (what: string) (ty: FrozenType) (caseName: string) : JsUnionCaseDecl =
        let info = unionInfoOf ctx what ty

        match info.Cases.TryGetValue caseName with
        | true, c -> c
        | _ -> failwithf "EmitJs: %s on union '%s' has no case '%s'" what info.Name caseName

    // ---- Members -------------------------------------------------------------

    /// Name-mangling and key→name resolution for member calls, shared between
    /// `emitMemberFn` and the call-site lowerings in the walker.
    module private Members =

        /// Instance method → `<Type>__<member>`; instance property getter →
        /// `<Type>__get_<Prop>`; static member → `<Type>_<member>`.
        let mangledName (typeName: string) (isStatic: bool) (isProperty: bool) (memberName: string) : string =
            if isStatic then typeName + "_" + memberName
            elif isProperty then typeName + "__get_" + memberName
            else typeName + "__" + memberName

        /// The declaring type's `SymbolKey` from a member-call node's `key`.
        let declKey (key: SymbolKey) : SymbolKey =
            match key with
            | SymbolKey.MemberKey(decl, _, _, _) -> decl
            | _ -> key

        /// The home assembly of an external type, for selecting its runtime-js module.
        /// Falls back to the provider's type-shape `origin` when the key has no assembly.
        let assemblyOf (ctx: WalkCtx) (key: SymbolKey) (what: string) : string =
            match SymbolKeyOps.keyAsm key with
            | Some a -> a
            | None ->
                let origin =
                    match ctx.Provider with
                    | ValueSome provider ->
                        match ExternalSymbols.tryLookupType provider key with
                        | ValueSome(ExternalTypeShape.Union(_, _, o))
                        | ValueSome(ExternalTypeShape.Record(_, _, o)) -> o.Assembly
                        | ValueSome(ExternalTypeShape.Class shape) -> shape.Origin.Assembly
                        | _ -> None
                    | ValueNone -> None

                match origin with
                | Some a -> a
                | None -> failwithf "EmitJs (Step 7): %s has no resolvable home assembly (key %A)" what key

        /// The emitted type name for mangling: local union/record `Name`, else the key's simple name.
        let typeName (ctx: WalkCtx) (key: SymbolKey) : string =
            match ctx.Unions.TryGetValue key with
            | true, info -> info.Name
            | _ ->
                match ctx.Records.TryGetValue key with
                | true, info -> info.Name
                | _ -> SymbolKeyOps.simpleName key

        /// The callable identifier of a local member's emitted function.
        let localFn (ctx: WalkCtx) (key: SymbolKey) (isStatic: bool) (isProperty: bool) (loc: JsLoc voption) : JsExpr =
            let dk = declKey key
            JsExpr.Identifier(mangledName (typeName ctx dk) isStatic isProperty (SymbolKeyOps.simpleName key), loc)

    /// `throw new Error("…")` — the fallthrough for a non-exhaustive match.
    let private matchFailure: JsStatement =
        JsStatement.Throw(
            JsExpr.New(
                JsExpr.Identifier("Error", ValueNone),
                [
                    JsExpr.Literal(JsLiteral.String "The match cases were incomplete", ValueNone)
                ],
                ValueNone
            )
        )

    /// Conjoin a list of optional pattern tests with `&&` (a `None` test is
    /// always-true and drops out). `None` ⇒ the pattern is irrefutable.
    let private conjoin (tests: JsExpr option list) : JsExpr option =
        match List.choose id tests with
        | [] -> None
        | t :: rest -> Some(List.fold (fun acc x -> JsExpr.Logical("&&", acc, x, ValueNone)) t rest)

    /// Emit a top-level binding as `export const` (library) or `const` (script).
    let private topLevelBinding (ctx: WalkCtx) (name: string) (init: JsExpr) : JsStatement =
        if ctx.ExportTopLevel then
            JsStatement.Export(name, init)
        else
            JsStatement.Const(name, init)

    /// Walk a type's `inherit` chain up to the `exn` intrinsic root and resolve its
    /// `(# "Error" #)` repr to the native runtime class name (`Error` on JS). Returns
    /// `ValueNone` when the type is not an `exn` subtype or the repr names no provider class.
    let private exnReprOf (ctx: WalkCtx) (ty: FrozenType) : string voption =
        match ctx.Provider with
        | ValueNone -> ValueNone
        | ValueSome provider ->
            let shapeOf (ft: FrozenType) : ExternalTypeShape voption =
                match ft with
                | FTClass(key, _)
                | FTUnion(key, _)
                | FTRecord(key, _) -> ExternalSymbols.tryLookupType provider key
                | FTConst(name, _) -> ExternalSymbols.tryRuntimeType provider name
                | _ -> ValueNone

            // Depth cap backstops a malformed cyclic `inherit`; each hop is a strict
            // ancestor so the chain is finite in practice.
            let rec climb (depth: int) (ft: FrozenType) : string voption =
                if depth > 16 then
                    ValueNone
                else
                    match shapeOf ft with
                    | ValueSome(ExternalTypeShape.Intrinsic(platform = Some platform)) ->
                        // Read the PLATFORM repr, not `canon` — `canon` is the unifier's
                        // identity key (`"System.Exception"`) and has no JS class analogue.
                        match ExternalSymbols.tryRuntimeType provider platform with
                        | ValueSome(ExternalTypeShape.Class _) -> ValueSome platform
                        | _ -> ValueNone
                    | ValueSome(ExternalTypeShape.Class shape) ->
                        match shape.FrozenBaseType with
                        | ValueSome b -> climb (depth + 1) b
                        | ValueNone -> ValueNone
                    | _ -> ValueNone

            climb 0 ty

    // ---- The walker ----------------------------------------------------------

    let rec buildExpr (ctx: WalkCtx) (e: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok e)

        match e with
        | TExprG.Const(value, _, _) -> constExpr value loc

        | TExprG.Var(k, _, _) -> JsExpr.Identifier(identName ctx.Source k, loc)

        // An external module function — imported from its package's JS runtime module.
        // The reference is the import alias; `App` saturates it.
        | TExprG.External(compiledName, key, _, _) ->
            JsExpr.Identifier(JsImports.addRef ctx.Imports compiledName key, loc)

        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            JsExpr.Conditional(buildExpr ctx cond, buildExpr ctx thenE, buildExpr ctx elseE, loc)

        // A `Sequential` in expression position is a comma expression (top-level it
        // expands to statements via `buildStatements`).
        | TExprG.Sequential(xs, _, _) -> JsExpr.Sequence([ for x in xs -> buildExpr ctx x ], loc)

        // A tuple `(a, b, …)` is a JS array `[a, b, …]`; a pattern reads elements by index.
        | TExprG.Tuple(items, _, _) -> JsExpr.Array([ for x in items -> buildExpr ctx x ], loc)

        // Pure `let` in expression position: substitute into uses (collapse operator templates).
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value ->
            buildExpr ctx (substVar k value body)

        // Non-pure `let` in expression position: JS has no let-expression, so lowers
        // to an IIFE `((x) => <body>)(<value>)` — the binder evaluated once.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let name = identName ctx.Source k

            JsExpr.Call(
                JsExpr.Arrow([ name ], JsFnBody.Expr(buildExpr ctx body), ValueNone),
                [ buildExpr ctx value ],
                loc
            )

        // Anonymous lambda — no binder key, so no self-tail-call analysis applies.
        | TExprG.Lambda _ -> emitFunction ctx ValueNone e

        // Curried application: `f a b` → `f(a)(b)` — one unary call per `App`.
        | TExprG.App(fn, arg, _, _) -> JsExpr.Call(buildExpr ctx fn, [ buildExpr ctx arg ], loc)

        // A record literal `{ X = e1; Y = e2 }` → `new R(args…)`, the args
        // reordered from source order to the class's *declaration*-order
        // positional constructor.
        | TExprG.RecordCons(srcFields, ty, _) ->
            let info = recordInfoOf ctx "RecordCons" ty
            let srcMap = Map.ofSeq (EqArray.toList srcFields)

            let args =
                [
                    for f in info.Fields ->
                        match Map.tryFind f srcMap with
                        | Some e -> buildExpr ctx e
                        | None -> failwithf "EmitJs (Step 3): record literal for '%s' is missing field '%s'" info.Name f
                ]

            JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), args, loc)

        // `{ r with X = v; … }` → reconstruction `new R(…)`: each field takes its
        // override if listed, else reads `<src>.field`. `<src>` is read once per
        // copied field, so a bare `Var` is spliced inline; any other source is bound
        // once through an IIFE binder (avoids re-evaluating / duplicating it).
        | TExprG.RecordClone(source, overrides, ty, _) ->
            let info = recordInfoOf ctx "RecordClone" ty
            let overrideMap = Map.ofSeq (EqArray.toList overrides)

            let argsFrom (srcRef: JsExpr) =
                [
                    for f in info.Fields ->
                        match Map.tryFind f overrideMap with
                        | Some ov -> buildExpr ctx ov
                        | None -> JsExpr.Member(srcRef, JsExpr.Identifier(f, ValueNone), false, ValueNone)
                ]

            match source with
            | TExprG.Var _ -> JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), argsFrom (buildExpr ctx source), loc)
            | _ ->
                let sName = "_rc" + string (TastWalk.exprTok e).StartIndex

                let newExpr =
                    JsExpr.New(
                        JsExpr.Identifier(info.Name, ValueNone),
                        argsFrom (JsExpr.Identifier(sName, ValueNone)),
                        loc
                    )

                JsExpr.Call(JsExpr.Arrow([ sName ], JsFnBody.Expr newExpr, ValueNone), [ buildExpr ctx source ], loc)

        // `r.X` → `r.X` — a member access on the record's like-named property
        // (the emitted class stores each field under its source field name).
        | TExprG.FieldGet(receiver, fieldName, _, _) ->
            JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(fieldName, ValueNone), false, loc)

        // A union constructor `Case e0 e1 …` → `new <Union>_<Case>(args…)`. The
        // args already arrive in declaration (field) order, so — unlike a record
        // literal — no reordering is needed; the subclass constructor stores them
        // positionally under the case's field names.
        | TExprG.UnionCons(caseName, args, ty, _) ->
            let c = unionCaseOf ctx "UnionCons" ty caseName

            JsExpr.New(JsExpr.Identifier(c.ClassName, ValueNone), [ for a in args -> buildExpr ctx a ], loc)

        // External exception construction → `new <exn repr>(msg)`. The repr is sourced
        // from the `inherit` chain via `exnReprOf`; only the leading message arg is kept
        // (`Error` has no slot for further args). Non-`exn`-subtype external `New` fails loudly.
        | TExprG.New(className, args, ty, _) ->
            match exnReprOf ctx ty with
            | ValueSome repr ->
                let errArgs =
                    match EqArray.toList args with
                    | [] -> []
                    | msg :: _ -> [ buildExpr ctx msg ]

                JsExpr.New(JsExpr.Identifier(repr, ValueNone), errArgs, loc)
            | ValueNone ->
                failwithf
                    "EmitJs (Step 8): construction of external type '%s' has no JS analogue (only `exn` subtypes lower to `new <exn repr>`)"
                    className

        // Member calls on a local record/union: each member is a free receiver-first function.
        | TExprG.PropertyGet(receiver, key, _, _, _) ->
            JsExpr.Call(Members.localFn ctx key false true ValueNone, [ buildExpr ctx receiver ], loc)

        | TExprG.MethodCall(receiver, key, _, args, _, _) ->
            let withRecv =
                JsExpr.Call(Members.localFn ctx key false false ValueNone, [ buildExpr ctx receiver ], loc)

            applyArgs ctx withRecv args

        | TExprG.StaticPropertyGet(key, _, _) -> Members.localFn ctx key true true loc

        | TExprG.StaticMethodCall(key, args, _, _) -> applyArgs ctx (Members.localFn ctx key true false loc) args

        // A member on an external type — imported from its runtime-js module and applied
        // receiver-first; instance method arguments arrive through the enclosing `App`.
        | TExprG.ExternalMember(receiver, key, memberName, isProperty, _, _) ->
            let declKey = Members.declKey key
            let isStatic = (receiver = ValueNone)

            let exportName =
                Members.mangledName (SymbolKeyOps.simpleName declKey) isStatic isProperty memberName

            let asm = Members.assemblyOf ctx declKey (sprintf "external member '%s'" memberName)
            let local = JsImports.addMemberRef ctx.Imports asm exportName

            match receiver with
            | ValueSome r -> JsExpr.Call(JsExpr.Identifier(local, ValueNone), [ buildExpr ctx r ], loc)
            | ValueNone -> JsExpr.Identifier(local, loc)

        // `match scrut with …` → an IIFE binding the scrutinee once, then testing each
        // arm in order and `return`ing the first whose pattern (+ guard) matches; an
        // unmatched value `throw`s. Sequential test (not `switch(tag)`) so it covers
        // guards, constants, nested patterns, and non-union scrutinees uniformly.
        | TExprG.Match(scrutinee, arms, _, _) ->
            let mv = "_m" + string (TastWalk.exprTok e).StartIndex
            let access = JsExpr.Identifier(mv, ValueNone)

            let body =
                [
                    for arm in EqArray.toList arms do
                        yield! buildMatchArm ctx access arm
                    yield matchFailure
                ]

            JsExpr.Call(JsExpr.Arrow([ mv ], JsFnBody.Block body, loc), [ buildExpr ctx scrutinee ], loc)

        | TExprG.ILIntrinsic(opCode, _, args, _, _) -> JsExpr.Raw(expandTemplate ctx opCode (EqArray.toList args), loc)

        | TExprG.Format(sink, segments, _, _) ->
            let arg = buildFormatArg ctx segments

            match sink with
            | FormatSinkG.ToStdOut true -> JsExpr.Call(console "log", [ arg ], loc)
            | FormatSinkG.ToStdErr true -> JsExpr.Call(console "error", [ arg ], loc)
            | other -> failwithf "EmitJs: unsupported format sink %A" other

        | other -> failwithf "EmitJs: unsupported expression %A" other

    and private console (method: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("console", ValueNone), JsExpr.Identifier(method, ValueNone), false, ValueNone)

    /// Expand a `$N` JS-expression template into `JsRawSeg`s: verbatim chunks
    /// interleaved with operand expressions. `$$` is a literal `$`. A bare CIL
    /// mnemonic with operands but no `$N` hole is a hard error.
    and private expandTemplate (ctx: WalkCtx) (template: string) (args: Frozen.TExpr list) : JsRawSeg list =
        let segs = ResizeArray<JsRawSeg>()
        let buf = System.Text.StringBuilder()
        let mutable sawHole = false

        let flush () =
            if buf.Length > 0 then
                segs.Add(JsRawSeg.Verbatim(buf.ToString()))
                buf.Clear() |> ignore

        let mutable i = 0

        while i < template.Length do
            let c = template.[i]

            if c = '$' && i + 1 < template.Length && template.[i + 1] = '$' then
                buf.Append '$' |> ignore
                i <- i + 2
            elif c = '$' && i + 1 < template.Length && System.Char.IsDigit template.[i + 1] then
                flush ()
                let mutable j = i + 1

                while j < template.Length && System.Char.IsDigit template.[j] do
                    j <- j + 1

                let idx =
                    System.Int32.Parse(template.Substring(i + 1, j - i - 1), CultureInfo.InvariantCulture)

                if idx < 0 || idx >= List.length args then
                    failwithf
                        "EmitJs: template '%s' references operand $%d but only %d supplied"
                        template
                        idx
                        (List.length args)

                segs.Add(JsRawSeg.Hole(buildExpr ctx (List.item idx args)))
                sawHole <- true
                i <- j
            else
                buf.Append c |> ignore
                i <- i + 1

        flush ()

        // Operands present but no hole → bare CIL mnemonic escaped the CLR-only finish pass.
        if not (List.isEmpty args) && not sawHole then
            failwithf "EmitJs: non-template ILIntrinsic opcode '%s' reached the JS backend" template

        List.ofSeq segs

    /// Build the single argument a `console.log`/`error` call prints from format segments.
    /// Mixed formats are concatenations seeded with `""` so every `+` is string-valued.
    and private buildFormatArg (ctx: WalkCtx) (segments: EqArray<Frozen.FormatSeg>) : JsExpr =
        match EqArray.toList segments with
        | [ FormatSegG.Lit s ] -> JsExpr.Literal(JsLiteral.String s, ValueNone)
        | [ FormatSegG.Hole(_, operand) ] -> buildExpr ctx operand
        | segs ->
            let pieces = ResizeArray<JsRawSeg>()
            // Seed with `""` so the first `+` already concatenates strings, even
            // when the format opens with two adjacent holes (`%d%d`).
            // Seed with `""` so the first `+` already concatenates strings even when
            // the format opens with two adjacent holes (`%d%d`).
            pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String "", ValueNone)))

            for seg in segs do
                pieces.Add(JsRawSeg.Verbatim " + ")

                match seg with
                | FormatSegG.Lit s -> pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String s, ValueNone)))
                | FormatSegG.Hole(_, operand) -> pieces.Add(JsRawSeg.Hole(buildExpr ctx operand))

            JsExpr.Raw(List.ofSeq pieces, ValueNone)

    /// Compile a pattern against a pure scrutinee-access expression `access` into a
    /// refutability test (`None` ⇒ irrefutable) and the `const` bindings its named
    /// sub-patterns introduce. `access` must be pure — it is duplicated across test
    /// and bindings; the short-circuit ensures a sub-field is read only after its tag matched.
    and private compileMatchPattern
        (ctx: WalkCtx)
        (access: JsExpr)
        (pat: Frozen.TPat)
        : JsExpr option * JsStatement list =
        let memberAccess (field: string) =
            JsExpr.Member(access, JsExpr.Identifier(field, ValueNone), false, ValueNone)

        match pat with
        | TPatG.Wildcard _ -> None, []
        | TPatG.NamedSimple(k, _, _) -> None, [ JsStatement.Const(identName ctx.Source k, access) ]
        | TPatG.Const(value, _, _) -> Some(JsExpr.Binary("===", access, constExpr value ValueNone, ValueNone)), []
        | TPatG.Union(caseName, subPats, ty, _) ->
            let c = unionCaseOf ctx "match pattern" ty caseName

            let tagTest =
                JsExpr.Binary(
                    "===",
                    memberAccess "tag",
                    JsExpr.Literal(JsLiteral.Number(string c.Tag), ValueNone),
                    ValueNone
                )

            // `map2` asserts the front-end invariant of one sub-pattern per field.
            let childTests, childBinds =
                List.map2
                    (fun fld sub -> compileMatchPattern ctx (memberAccess fld) sub)
                    c.Fields
                    (EqArray.toList subPats)
                |> List.unzip

            conjoin (Some tagTest :: childTests), List.concat childBinds
        | TPatG.Record(fields, ty, _) ->
            // Validate each field name against the emitted record so a stale name
            // fails here rather than silently reading `undefined`.
            let info = recordInfoOf ctx "record pattern" ty
            let known = Set.ofList info.Fields

            let tests, binds =
                EqArray.toList fields
                |> List.map (fun (fieldName, sub) ->
                    if not (Set.contains fieldName known) then
                        failwithf "EmitJs: record pattern on '%s' names unknown field '%s'" info.Name fieldName

                    compileMatchPattern ctx (memberAccess fieldName) sub
                )
                |> List.unzip

            conjoin tests, List.concat binds
        // Tuple pattern: each element matches its positional index `access[i]`.
        | TPatG.Tuple(items, _, _) ->
            let indexAccess i =
                JsExpr.Member(access, JsExpr.Literal(JsLiteral.Number(string i), ValueNone), true, ValueNone)

            let tests, binds =
                EqArray.toList items
                |> List.mapi (fun i sub -> compileMatchPattern ctx (indexAccess i) sub)
                |> List.unzip

            conjoin tests, List.concat binds
        | TPatG.TypeTestAs _ -> failwithf "EmitJs: type-test patterns are not supported"
        // `null` pattern: JS loose `== null` matches both `null` and `undefined`.
        | TPatG.Null _ -> Some(JsExpr.Binary("==", access, JsExpr.Identifier("null", ValueNone), ValueNone)), []

    /// Build one `match` arm's statements: when the pattern matches (and the guard,
    /// if any, passes) the arm `return`s its body. An always-matching arm
    /// (wildcard / bare variable, `test = None`) emits a bare `Block` so its
    /// bindings stay scoped (two arms may bind the same source name); a refutable
    /// arm guards that block with `if (test)`.
    and private buildMatchArm (ctx: WalkCtx) (access: JsExpr) (arm: Frozen.TMatchArm) : JsStatement list =
        let test, binds = compileMatchPattern ctx access arm.Pat

        let inner =
            match arm.Guard with
            | None -> binds @ [ JsStatement.Return(buildExpr ctx arm.Body) ]
            | Some g ->
                binds
                @ [
                    JsStatement.If(buildExpr ctx g, [ JsStatement.Return(buildExpr ctx arm.Body) ], [])
                ]

        match test with
        | None -> [ JsStatement.Block inner ]
        | Some t -> [ JsStatement.If(t, inner, []) ]

    /// Emit a function value as nested *unary* arrows. When `selfKey` names the
    /// binding and its body makes a saturated tail self-call, the innermost arrow
    /// becomes a `while (true)` trampoline (`buildTailBody`) for constant-stack
    /// recursion; else the innermost body is the plain expression. The nested-unary
    /// shape keeps every arrow's param in scope at the innermost body, which is what
    /// lets the trampoline write them back and `continue`.
    and emitFunction (ctx: WalkCtx) (selfKey: NodeKey voption) (lam: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok lam)
        let names, body = peelArrow ctx.Source lam
        let arity = List.length names

        let innermost =
            match selfKey with
            | ValueSome k when hasTailSelfCall k arity body ->
                JsFnBody.Block
                    [
                        JsStatement.While(
                            JsExpr.Literal(JsLiteral.Boolean true, ValueNone),
                            buildTailBody ctx k names body
                        )
                    ]
            | _ -> JsFnBody.Expr(buildExpr ctx body)

        nestUnaryArrows loc names innermost

    /// Build the statements of a self-tail-call trampoline's loop body, walking
    /// tail position. A saturated tail self-call writes its arguments back to the
    /// parameter variables — through per-argument temporaries first, so an
    /// argument that reads a parameter (`sum (n-1) (acc+n)`) sees the *old* value
    /// — then `continue`s. Tail `if`/`let`/`Sequential`-tail thread through;
    /// every other tail expression `return`s its value.
    and buildTailBody (ctx: WalkCtx) (selfKey: NodeKey) (paramNames: string list) (e: Frozen.TExpr) : JsStatement list =
        let arity = List.length paramNames
        let recur = buildTailBody ctx selfKey paramNames

        match e with
        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            [ JsStatement.If(buildExpr ctx cond, recur thenE, recur elseE) ]
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value ->
            recur (substVar k value body)
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            JsStatement.Const(identName ctx.Source k, buildExpr ctx value) :: recur body
        | TExprG.Sequential(xs, _, _) when xs.Length > 0 ->
            let items = EqArray.toList xs
            let init = items.[.. items.Length - 2]
            let last = items.[items.Length - 1]
            (init |> List.collect (buildStatements ctx)) @ recur last
        | TailSelfCall selfKey arity args ->
            // `_tc<i>` temporaries: evaluate every new argument before any write-back,
            // so a self-call arg that mentions a parameter reads its pre-iteration
            // value. (Not collision-proof against a source param literally named
            // `_tc0` — synthetic names are keyed off strings, not `NodeKey`s.)
            let tmp i = "_tc" + string i

            [ for i, a in List.indexed args -> JsStatement.Const(tmp i, buildExpr ctx a) ]
            @ [
                for i, name in List.indexed paramNames -> JsStatement.Assign(name, JsExpr.Identifier(tmp i, ValueNone))
            ]
            @ [ JsStatement.Continue ]
        | _ -> [ JsStatement.Return(buildExpr ctx e) ]

    /// Curry `base` over `args` — one unary `Call` per argument, in source order
    /// (`base(a)(b)…`). Shared by the `MethodCall` / `StaticMethodCall` lowerings.
    and private applyArgs (ctx: WalkCtx) (baseExpr: JsExpr) (args: EqArray<Frozen.TExpr>) : JsExpr =
        EqArray.toList args
        |> List.fold (fun acc a -> JsExpr.Call(acc, [ buildExpr ctx a ], ValueNone)) baseExpr

    /// Emit a record/union member as a free, curried, receiver-first top-level function:
    /// `member this.Foo a b` → `<Type>__Foo = (this$) => (a) => (b) => <body>`.
    /// Static members drop the receiver; a static property emits as a plain value binding.
    and emitMemberFn (ctx: WalkCtx) (typeName: string) (m: Frozen.TTypeMember) : JsStatement =
        let isProperty = (m.Kind = TMemberKind.Property)
        let name = Members.mangledName typeName m.IsStatic isProperty m.Name

        let receiverNames =
            if m.IsStatic then
                []
            else
                match m.ThisKey with
                | ValueSome k -> [ identName ctx.Source k ]
                | ValueNone -> [ "this$" ]

        let paramNames = [ for (pk, _) in m.Params -> identName ctx.Source pk ]
        let allNames = receiverNames @ paramNames
        let body = buildExpr ctx m.Body

        let init =
            match allNames with
            | [] -> body
            | _ -> nestUnaryArrows ValueNone allNames (JsFnBody.Expr body)

        topLevelBinding ctx name init

    /// A value bound to a name (a module value, or a `let` binder). A `Lambda`
    /// value routes through `emitFunction` carrying its binder key, so a
    /// recursive binding (`let rec`) can recognise its own tail calls; any other
    /// value is a plain `buildExpr`.
    and emitBound (ctx: WalkCtx) (k: NodeKey) (value: Frozen.TExpr) : JsExpr =
        match value with
        | TExprG.Lambda _ -> emitFunction ctx (ValueSome k) value
        | _ -> buildExpr ctx value

    /// An expression in statement position. `Sequential` flattens; a `let` binder
    /// becomes a `const`; anything else is one `ExpressionStatement`.
    and buildStatements (ctx: WalkCtx) (e: Frozen.TExpr) : JsStatement list =
        match e with
        | TExprG.Sequential(xs, _, _) ->
            [
                for x in xs do
                    yield! buildStatements ctx x
            ]
        // Pure binder: substitute away so synthetic operand lets don't surface as `const`s.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value ->
            buildStatements ctx (substVar k value body)
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            JsStatement.Const(identName ctx.Source k, emitBound ctx k value)
            :: buildStatements ctx body
        | _ -> [ JsStatement.Expression(buildExpr ctx e) ]

    /// `finishOps` knob for JS: identity — operators are already `$N`-templates pre-freeze.
    let private jsFinishOps (e: Frozen.TExpr) : Frozen.TExpr = e

    /// Collect the file's nominal `type` decls (in source order) into the emission list,
    /// the two lookup tables, and the member list. Read off the un-lowered decls —
    /// `TastLower.lower` drops `type` decls.
    let private collectTypes (tast: Frozen.TastFile) =
        let ordered = ResizeArray<JsStatement>()
        let records = System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>()
        let unions = System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>()
        let members = ResizeArray<string * Frozen.TTypeMember>()

        let addMembers (typeName: string) (ms: EqArray<Frozen.TTypeMember>) =
            for m in ms do
                members.Add(typeName, m)

        for decl in tast.Decls do
            match decl with
            | TDeclG.Type td ->
                match td.Kind with
                | TTypeKindG.Record(fields, recMembers) ->
                    let info =
                        {
                            Name = td.Name
                            Fields = [ for f in fields -> f.Name ]
                        }

                    records.[td.Key] <- info
                    ordered.Add(JsStatement.Class(info.Name, info.Fields))
                    addMembers td.Name recMembers
                | TTypeKindG.Union(cases, unionMembers) ->
                    let info, caseDecls =
                        buildUnionInfo td.Name [ for case in cases -> case.Name, [ for (nm, _) in case.Fields -> nm ] ]

                    unions.[td.Key] <- info
                    ordered.Add(JsStatement.Union(td.Name, caseDecls))
                    addMembers td.Name unionMembers
                | _ -> ()
            | _ -> ()

        List.ofSeq ordered, records, unions, List.ofSeq members

    /// The whole frozen file → a `Program`. Type declarations become JS `class`es first
    /// (classes are not hoisted); remaining decls are lowered — `let inline` templates
    /// and `type` decls drop out, leaving module values and effectful expressions.
    let buildProgram (ctx0: WalkCtx) (tast: Frozen.TastFile) : JsProgram =
        let classDecls, recordTable, unionTable, memberDefs = collectTypes tast

        let ctx =
            { ctx0 with
                Records = recordTable
                Unions = unionTable
            }

        // Member functions emitted after the class decls (they reference the classes
        // via `new`/match, and `const` arrows are not hoisted) and before the body.
        let memberDecls = [ for (typeName, m) in memberDefs -> emitMemberFn ctx typeName m ]

        let lowered = TastLower.lower jsFinishOps tast.Decls

        let body =
            [
                for decl in lowered do
                    match decl with
                    | TDeclG.Expression(e, _) -> yield! buildStatements ctx e
                    | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                        topLevelBinding ctx (identName ctx.Source k) (emitBound ctx k value)
                    | other -> failwithf "EmitJs: unsupported declaration %A" other
            ]

        // Imports lead the program; external + local class decls follow — classes
        // are not hoisted and must precede every `new`/match site.
        {
            Body =
                JsImports.importStatements ctx.Imports
                @ classDecls
                @ List.ofSeq ctx.ExternalUnionDecls
                @ memberDecls
                @ body
        }
