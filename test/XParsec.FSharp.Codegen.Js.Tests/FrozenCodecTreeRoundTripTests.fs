module XParsec.FSharp.Codegen.Js.Tests.FrozenCodecTreeRoundTripTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The wire-format gate: `thaw (flatten f)` is STRUCTURALLY equal to `f` for every JS-gated
// conformance program's whole `Frozen.TastFile`. That also gates the pool interconversion:
// the columns must re-author every node, and the bound-variable/lambda id remap must invert.

/// Filtered, so `frozenOfJs` never trips on a `Diagnose` program's error diagnostics.
let private gated = compiledBy "js"

/// Freeze each gated program's source ONCE, because the front-end pass is the expensive part.
let private frozenFiles: (string * FrozenPools) list =
    gated |> List.map (fun p -> p.Name, frozenOfJs p.Source)

/// Equality is taken on the DU the columns encode, `FrozenPools` itself carrying no equality.
let private survivesRoundTrip (f: FrozenPools) : bool =
    TastUnpool.ofPools f = TastUnpool.ofPools (FrozenCodec.thaw (FrozenCodec.flatten f))

/// A pools value bearing a specialization entry, an `InlineCall` edge referencing it, and a
/// `CallerExpr` mark. No corpus program reaches any of the three, so the carriers are grafted
/// onto a real frozen file, leaving every other column exactly as the freeze built it.
let private withSpecialization () : FrozenPools =
    // A NESTED module: a top-level `let inline` has no home module, so no exportable identity,
    // and publishes nothing. `not` is a ONE-operand intrinsic, so the tree is guaranteed a node
    // with exactly one expr child and no pattern, which is what the `CallerExpr` graft needs.
    let pools =
        frozenOfJs "module M\n\nmodule N =\n    let inline f x = x + 1\n\nlet y = N.f 2\nlet z = not true\n"

    let template =
        match pools.InlineTemplates with
        | [| t |] -> t
        | ts -> failtestf "expected exactly one published template, got %d" ts.Length

    // `InlineCall` draws its args from the expr child column and owns no pattern, so a slot
    // with neither can stand in for a nullary one without disturbing the columns around it.
    let leaf =
        [ 0 .. pools.ExprPayloads.Length - 1 ]
        |> List.find (fun i ->
            ChildColumn.count pools.ExprChildren i = 0
            && ChildColumn.count pools.ExprPatChildren i = 0
        )

    // `CallerExpr` wraps exactly one expression and owns no pattern, so a slot with that shape
    // stands in for one the same way.
    let unary =
        [ 0 .. pools.ExprPayloads.Length - 1 ]
        |> List.find (fun i ->
            ChildColumn.count pools.ExprChildren i = 1
            && ChildColumn.count pools.ExprPatChildren i = 0
        )

    // The two grafted nodes are CALL-SITE material, so their domain is the compiling file and
    // deliberately NOT the entry's declaring file: a codec that dropped a node's own path and
    // recovered it from the entry would still round-trip if the two agreed.
    let consumer =
        AssemblyFilePath.InFile(ValueSome(AssemblyName "App"), AssemblyFileId.ofRelative "m.fs")

    // The template's own `let` halves, which is the one pair of roots in these pools known to
    // be a binding of a lambda.
    let templatePat, templateValue =
        let (DeclPoolId d) = template.Decl

        match ChildColumn.slice pools.DeclPatChildren d, ChildColumn.slice pools.DeclExprChildren d with
        | [| p |], [| v |] -> p, v
        | ps, vs ->
            failtestf "expected the template's `let` to own one pattern and one value, got %d/%d" ps.Length vs.Length

    let payloads = Array.copy pools.ExprPayloads

    payloads.[leaf] <-
        ExprPayload.InlineCall
            {|
                Path = consumer
                Spec = SpecializationId 0
            |}

    payloads.[unary] <- ExprPayload.CallerExpr consumer

    { pools with
        ExprPayloads = payloads
        Specializations =
            [|
                {
                    Key =
                        {
                            Template = template.Key
                            TypeArgs = EqArray.ofList [ FTConst(RuntimeNames.intKey, EqArray.empty) ]
                        }
                    // A file OTHER than the one the blob is keyed by, so nothing about the
                    // source is recoverable from the key. Synthetic: no anchor is resolved here.
                    Path = AssemblyFilePath.InFile(ValueSome(AssemblyName "Lib"), AssemblyFileId.ofRelative "n.fs")
                    Pat = templatePat
                    Value = templateValue
                }
            |]
    }

/// The same graft with a SECOND entry carrying the same declaring file: one template grounded
/// two ways. The two entries differ only in the grounding, so the source is what they share.
let private withSharedFilePath () : FrozenPools =
    let pools = withSpecialization ()

    let first =
        match pools.Specializations with
        | [| s |] -> s
        | ss -> failtestf "expected the one grafted entry, got %d" ss.Length

    { pools with
        Specializations =
            [|
                first
                { first with
                    Key =
                        { first.Key with
                            TypeArgs = EqArray.ofList [ FTConst(RuntimeNames.boolKey, EqArray.empty) ]
                        }
                }
            |]
    }

[<Tests>]
let tests =
    testList
        "FrozenCodec tree round-trip"
        [
            test "thaw (flatten f) is structurally equal to f over the JS corpus" {
                for (name, f) in frozenFiles do
                    Expect.isTrue
                        (survivesRoundTrip f)
                        (sprintf "frozen file for %s did not survive flatten/thaw structurally" name)
            }

            // The gate must exercise a non-trivial corpus, else an empty run would make
            // `thaw ∘ flatten = id` vacuously true.
            test "the corpus exercises a non-trivial number of decls" {
                let totalDecls = frozenFiles |> List.sumBy (fun (_, f) -> f.Roots.Length)

                Expect.isGreaterThan (List.length frozenFiles) 0 "gated programs"
                Expect.isGreaterThan totalDecls 20 "total top-level decls across the corpus"
            }

            // The corpus above declares ZERO generic members, so it never populates a member's
            // `MethodTypeParams`. This case forces a non-empty carrier: it must be stored as
            // `(name, FTTypar(Member _, i))` and round-trip as plain data.
            test "a generic member's MethodTypeParams is stored on the frozen tree as FTTypar and round-trips" {
                let f = frozenOfJs "type C() =\n    member this.Id<'T> (x: 'T) : 'T = x\n"

                let cKey, idMember =
                    (TastUnpool.ofPools f).Decls
                    |> EqArray.toList
                    |> List.tryPick (fun d ->
                        match d with
                        | TDeclG.Type td ->
                            TTypeKindG.members td.Kind
                            |> EqArray.toList
                            |> List.tryPick (fun (m: Pooled.TTypeMember) ->
                                if m.Name = "Id" then Some(td.TypeKey, m) else None
                            )
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failtest "no member `Id` in the frozen tree")

                // Genuinely populated, not silently frozen empty, and scoped by the member's
                // ordinal: `1`, after the primary constructor.
                Expect.equal idMember.Ordinal (MemberOrdinal 1) "Id follows the primary constructor"

                Expect.equal
                    (EqArray.toList idMember.MethodTypeParams)
                    [ "'T", FTTypar(TyparScope.Member(cKey, MemberOrdinal 1), 0) ]
                    "member's own typar is stored as (name, FTTypar(Member(C, 1), 0))"

                Expect.isTrue (survivesRoundTrip f) "generic-member file survived flatten/thaw structurally"
            }

            // The corpus reaches no `TraitCall` payload: one survives freezing only inside a
            // published inline template, which an UNCALLED user SRTP inline provides. Its two
            // distinct typars must come back as a two-element support set.
            test "a TraitCall payload's support set and member name round-trip" {
                let f =
                    frozenOfJs
                        "module M\n\nmodule N =\n    let inline plus (a: ^T) (b: ^U) : ^T = ((^T or ^U): (static member (+): ^T * ^U -> ^T) (a, b))\n"

                let payload =
                    f.ExprPayloads
                    |> Array.tryPick (fun p ->
                        match p with
                        | ExprPayload.TraitCall p -> Some p
                        | _ -> None
                    )

                match payload with
                | Some p ->
                    Expect.equal p.SupportTys.Length 2 "both operand typars are candidates"
                    Expect.equal p.MemberName "op_Addition" "the compiled member name"
                | None -> failtest "no TraitCall payload in the frozen inline template"

                Expect.isTrue (survivesRoundTrip f) "TraitCall file did not survive flatten/thaw structurally"
            }

            // Every binding in the conformance corpus introduces a bound variable, so it never
            // exercised the seam where a side table is filed under a key the frozen tree does
            // not bear. These pin those shapes through the stored form.
            for name, src in
                [
                    "module-level tuple destructuring", "let p = (1, 2)\nlet (a, b) = p\nlet s = a + b\n"
                    "module-level wildcard binding", "let _ = 5\n"
                    "parenthesised simple binding pattern", "let (x) = 5\nlet y = x + 1\n"
                    "wildcard binding in a function body", "let f x =\n    let _ = x\n    x\n"
                    "top-level inline binding", "module M\nlet inline f x = x + 1\nlet y = f 2\n"
                    "inline binding in a named module",
                    "module M\n\nmodule N =\n    let inline f x = x + 1\n\nlet y = N.f 2\n"
                ] do
                test (
                    "a binding pattern that does not introduce a bound variable, or whose variable is unpooled, round-trips: "
                    + name
                ) {
                    Expect.isTrue
                        (survivesRoundTrip (frozenOfJs src))
                        (name + " did not survive flatten/thaw structurally")
                }

            // A `type` declaration's bodies are the one place the stored form identifies an
            // expression by pool id from INSIDE a declaration SHAPE, so the shape's writer and
            // reader must stay in lockstep slot for slot. A plain `member` misses these slots.
            for name, src in
                [
                    "static and instance preamble",
                    "type C(a: int) =\n    static let s = 1\n    let b = a + 1\n    do ()\n    member this.M() = b + s\n"
                    "secondary constructor", "type C(x: int) =\n    new() = C(0)\n    member this.X = x\n"
                    "base-ctor call",
                    "type Shape(x: int) =\n    member this.Raw = x\n\ntype Circle(r: int, t: int) =\n    inherit Shape(t)\n    member this.Radius = r\n"
                    "interface implementation",
                    "type IBox =\n    abstract member Unwrap : unit -> int\n\ntype Box(value: int) =\n    interface IBox with\n        member this.Unwrap() : int = value\n"
                ] do
                test ("a type declaration's body slots round-trip: " + name) {
                    Expect.isTrue
                        (survivesRoundTrip (frozenOfJs src))
                        (name + " did not survive flatten/thaw structurally")
                }

            // The corpus writes no attributes, so the five `Attributes` slots (type decl,
            // member, union case, record field, enum case) would round-trip vacuously empty.
            // This source populates every one and asserts on the DECODED tree.
            test "attributes at every declaration position are stored on the frozen tree and round-trip" {
                let f =
                    frozenOfJs (
                        String.concat
                            "\n"
                            [
                                "type MarkAttribute(n: int, s: string) ="
                                "    member this.N = n"
                                ""
                                "type Targets ="
                                "    | A = 1"
                                "    | B = 2"
                                ""
                                "[<Mark(-3, \"hi\", Extra = (Targets.A ||| Targets.B))>]"
                                "type Point = { [<Mark(1, \"f\")>] X: int }"
                                ""
                                "[<Mark(2, \"u\")>]"
                                "[<RequireQualifiedAccess>]"
                                "type Shape = | [<Mark(3, \"c\")>] Circle of int"
                                ""
                                "type Palette = | [<Mark(4, \"e\")>] Red = 1"
                                ""
                                "type Widget() ="
                                "    [<Mark(5, \"m\")>]"
                                "    member this.M() = 1"
                            ]
                    )

                let decoded = TastUnpool.ofPools (FrozenCodec.thaw (FrozenCodec.flatten f))

                let typeDecl name =
                    decoded.Decls
                    |> EqArray.toList
                    |> List.tryPick (fun d ->
                        match d with
                        | TDeclG.Type td when td.Name = name -> Some td
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failtestf "no decoded type decl named %s" name)

                let markArgs (attrs: TAttributes) =
                    match attrs |> EqArray.toList |> List.filter (fun a -> a.Key.Name = "MarkAttribute") with
                    | [ a ] -> EqArray.toList a.Args
                    | other -> failtestf "expected exactly one Mark attribute, got %d" (List.length other)

                let int32 (v: int64) =
                    TConstValue.Integral(XParsec.FSharp.Lexer.IntKind.Int32, v)

                let positional (v: TConstValue) : TAttributeArg =
                    {
                        Name = ValueNone
                        Value = v
                        EnumKey = ValueNone
                    }

                Expect.equal
                    (markArgs (typeDecl "Point").Attributes)
                    [
                        positional (int32 -3L)
                        positional (TConstValue.String "hi")
                        {
                            Name = ValueSome "Extra"
                            Value = int32 3L
                            EnumKey = ValueSome (typeDecl "Targets").TypeKey
                        }
                    ]
                    "the type decl's folded args came back off the wire, enum identity included"

                let fieldAttrs =
                    match (typeDecl "Point").Kind with
                    | TTypeKindG.Record r -> r.Fields.[0].Attributes
                    | other -> failtestf "Point is not a record: %A" other

                Expect.equal
                    (markArgs fieldAttrs)
                    [ positional (int32 1L); positional (TConstValue.String "f") ]
                    "the record field's attribute came back off the wire"

                let caseAttrs =
                    match (typeDecl "Shape").Kind with
                    | TTypeKindG.Union u -> u.Cases.[0].Attributes
                    | other -> failtestf "Shape is not a union: %A" other

                Expect.equal
                    (markArgs caseAttrs)
                    [ positional (int32 3L); positional (TConstValue.String "c") ]
                    "the union case's folded args came back off the wire"

                // The verdicts are VIEWS over the attribute list, so asserting them on the
                // decoded tree pins that the list they derive from round-tripped.
                Expect.isTrue
                    (typeDecl "Shape").IsRequireQualifiedAccess
                    "[<RequireQualifiedAccess>] is derivable off the wire"

                Expect.equal
                    (typeDecl "Point").EqualitySupport
                    EqualityVerdict.Structural
                    "the record's structural default is derivable off the wire"

                let enumCaseAttrs =
                    match (typeDecl "Palette").Kind with
                    | TTypeKindG.Enum cases -> cases.[0].Attributes
                    | other -> failtestf "Palette is not an enum: %A" other

                Expect.equal
                    (markArgs enumCaseAttrs)
                    [ positional (int32 4L); positional (TConstValue.String "e") ]
                    "the enum case's folded args came back off the wire"

                let memberAttrs =
                    TTypeKindG.members (typeDecl "Widget").Kind
                    |> EqArray.toList
                    |> List.pick (fun m -> if m.Name = "M" then Some m.Attributes else None)

                Expect.equal
                    (markArgs memberAttrs)
                    [ positional (int32 5L); positional (TConstValue.String "m") ]
                    "the member's folded args came back off the wire"

                Expect.isTrue (survivesRoundTrip f) "the attributed file survived flatten/thaw structurally"
            }

            // The corpus leaves the specialization array and the `InlineCall`/`CallerExpr`
            // payloads at zero. Assert on the decoded carriers themselves: an entry the writer
            // skipped and the reader defaulted to empty is invisible to a tree comparison.
            test "a specialization entry, the InlineCall referencing it and a CallerExpr survive flatten/thaw" {
                let grafted = withSpecialization ()
                let rt = FrozenCodec.thaw (FrozenCodec.flatten grafted)

                Expect.equal rt.Specializations grafted.Specializations "the specialization table survived the wire"

                Expect.sequenceEqual rt.ExprPayloads grafted.ExprPayloads "every expr payload survived the wire"

                Expect.isTrue (survivesRoundTrip grafted) "the grafted file survived flatten/thaw structurally"
            }

            // The file's OWN path is the one field no structural comparison can reach: the
            // unpooled tree has no field for it, so a writer that dropped it passes every gate
            // above, and a file carrying no path calls all of its own code foreign.
            test "the file's own path survives flatten/thaw" {
                let frozen = frozenOfJs "module M\n\nlet y = 1 + 2\n"

                Expect.notEqual
                    frozen.Path
                    AssemblyFilePath.Nowhere
                    "the freeze recorded a real file, or what follows is vacuous"

                Expect.equal
                    (FrozenCodec.thaw (FrozenCodec.flatten frozen)).Path
                    frozen.Path
                    "the compiling file's identity came back off the wire"
            }

            // The path is a REF into a table of its own, and one reference exercises the ref
            // but not the interning. Asserted on the decoded row array, where the sharing is
            // observable: references carrying equal `AssemblyFilePath` values decode alike either way.
            test "every reference to a file resolves to ONE file-path row" {
                let grafted = withSharedFilePath ()
                let rt = FrozenCodec.thaw (FrozenCodec.flatten grafted)

                Expect.equal (rt.Specializations.Length) 2 "both entries survived the wire"
                Expect.equal rt.Specializations grafted.Specializations "…each with the source it was written with"

                // Counted rather than assumed: two entries reference the declaring file and the two
                // grafted nodes the consumer, on top of whatever the freeze itself anchored.
                let referenced =
                    [
                        for s in rt.Specializations do
                            yield s.Path

                        for p in rt.ExprPayloads do
                            match p with
                            | ExprPayload.InlineCall c -> yield c.Path
                            | ExprPayload.CallerExpr o -> yield o
                            | _ -> ()
                    ]
                    |> List.distinct

                Expect.isGreaterThan
                    referenced.Length
                    1
                    "the fixture must name more than one file, else sharing is vacuous"

                Expect.equal
                    (rt.Types.Rows.FilePaths.Length)
                    referenced.Length
                    "each file occupies ONE row, not one per reference that names it"
            }
        ]
