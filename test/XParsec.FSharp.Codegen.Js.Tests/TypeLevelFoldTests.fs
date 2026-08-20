module XParsec.FSharp.Codegen.Js.Tests.TypeLevelFoldTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The front end GROUND-EVALUATES the TS type-level computations `keyof T`, `T[K]` and
// `check extends E ? X : Y`. This hand-built manifest is mitt-SHAPED, but keeps the FAITHFUL
// function-typed handler mitt's committed golden degrades, so the folds are pinned unaided.

// ─── Schema builders ────────────────────────────────────────────────────────

let private named n = Schema.TypeRef.Named(n, [])
let private unitT = named "unit"
let private typar i = Schema.TypeRef.Typar i
let private mtypar i = Schema.TypeRef.MethodTypar i
let private keyof t = Schema.TypeRef.KeyOf t
let private idx o i = Schema.TypeRef.IndexedAccess(o, i)

let private cond c e wt wf =
    Schema.TypeRef.Conditional(c, e, wt, wf)

let private fn args ret = Schema.TypeRef.Fun(args, ret)

let private strLit s =
    Schema.TypeRef.Literal(Schema.LiteralValue.StringVal s)

let private param name ty : Schema.Param =
    {
        Name = name
        Type = ty
        Optional = false
        Rest = false
    }

let private methodMem name (sigs: Schema.Signature list) : Schema.Member =
    {
        Name = name
        Kind = Schema.MemberKind.Method
        Type = None
        Signatures = sigs
        Static = false
        Optional = false
    }

// `subscribe(key: keyof Events): unit` — a bare `keyof` parameter (keyof fold).
let private subscribeSig: Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = [ param "key" (keyof (typar 0)) ]
        Returns = unitT
    }

// `on<Key extends keyof Events>(type: Key, handler: (Events[Key]) -> unit): unit`
// (method-typar grounding + T[K] fold) with the wildcard overload `on(type: '*', handler: (unit)->unit): unit`
// so the call site is genuinely MULTI-candidate (the literal must select the typar arm).
let private onKeyedSig: Schema.Signature =
    {
        TypeParams = 1
        TypeParamBounds = [ Some(keyof (typar 0)) ]
        Params =
            [
                param "type" (mtypar 0)
                param "handler" (fn [ idx (typar 0) (mtypar 0) ] unitT)
            ]
        Returns = unitT
    }

let private onWildcardSig: Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = [ param "type" (strLit "*"); param "handler" (fn [ unitT ] unitT) ]
        Returns = unitT
    }

// `emit<Key extends keyof Events>(type: Key, event: Events[Key]): unit`, paired with a
// wildcard overload (as mitt's own `emit` is) so the call is MULTI-candidate, which is what
// routes it through the commit seam where the syntactic constant grounds `Key`.
let private emitKeyedSig: Schema.Signature =
    {
        TypeParams = 1
        TypeParamBounds = [ Some(keyof (typar 0)) ]
        Params = [ param "type" (mtypar 0); param "event" (idx (typar 0) (mtypar 0)) ]
        Returns = unitT
    }

let private emitWildcardSig: Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = [ param "type" (strLit "*"); param "event" unitT ]
        Returns = unitT
    }

// Ground conditionals (conditional fold): the true branch (`string extends string ? int : bool`)
// and the false branch (`int extends string ? int : bool`).
let private condTrueSig: Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params =
            [
                param "x" (cond (named "string") (named "string") (named "int") (named "bool"))
            ]
        Returns = unitT
    }

let private condFalseSig: Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = [ param "x" (cond (named "int") (named "string") (named "int") (named "bool")) ]
        Returns = unitT
    }

let private busManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "buslib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Bus",
                    1,
                    [
                        methodMem "subscribe" [ subscribeSig ]
                        methodMem "on" [ onKeyedSig; onWildcardSig ]
                        methodMem "emit" [ emitKeyedSig; emitWildcardSig ]
                        methodMem "cond" [ condTrueSig ]
                        methodMem "cond2" [ condFalseSig ]
                    ],
                    [],
                    []
                )
                Schema.Export.Function(
                    "makeBus",
                    [
                        {
                            TypeParams = 1
                            TypeParamBounds = []
                            Params = []
                            Returns = Schema.TypeRef.Named("Bus", [ typar 0 ])
                        }
                    ],
                    Schema.ImportShape.Named
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private busProvider: IExternalSymbolProvider =
    // Ambient AGGREGATED from the sources (not `stackJs []`): the intrinsic resolver reaches
    // `Vesper.unit` only through the `Vesper` open-prefix, which a dropped ambient would hide.
    stackWithAmbient [ TsManifestProvider.providerOfManifest busManifest; jsProvider.Value ]

// A project-local record supplies the ground `Events` the folds read member names/types
// off of: `ping : int`, `pong : string`.
let private prelude =
    String.concat
        "\n"
        [
            "type MyEvents = { ping: int; pong: string }"
            "let e : Bus<MyEvents> = makeBus()"
        ]

let private analyse (body: string) : Diagnostic list =
    let input = prelude + "\n" + body + "\n"
    let lexed, file = parseFile input

    let tast =
        Pipeline.analyseSemFor testCompiling busProvider (LexedFile.ofText lexed) file

    tast.Diagnostics |> Diagnostic.errors

let private errorText (ds: Diagnostic list) : string =
    ds |> List.map (fun d -> d.Message) |> String.concat "\n"

[<Tests>]
let tests =
    testList
        "TypeLevelFold"
        [
            // ── keyof fold via a bare `keyof Events` parameter ──
            test "(keyof) a member-name constant admits into a keyof parameter" {
                let errors = analyse "e.subscribe(\"ping\")"
                Expect.isEmpty errors (sprintf "expected no errors, got:\n%s" (errorText errors))
            }

            test "(keyof) a non-member constant errors, listing the member set" {
                let errors = analyse "e.subscribe(\"nope\")"
                Expect.isNonEmpty errors "a non-key constant must be rejected"
                let msg = errorText errors
                Expect.stringContains msg "ping" "the message names the allowed key 'ping'"
                Expect.stringContains msg "pong" "the message names the allowed key 'pong'"
            }

            // ── method-typar grounding + T[K] fold types the handler param ──
            test "(on) the handler param is typed from the addressed member (ping:int)" {
                // method-typar grounding solves `Key := "ping"`, then the T[K] fold grounds
                // `Events[Key]` to `int`, so a `%d` body type-checks.
                let errors = analyse "e.on(\"ping\", fun p -> printfn \"%d\" p)"
                Expect.isEmpty errors (sprintf "expected p : int, got:\n%s" (errorText errors))
            }

            test "(on) a wrong-typed handler body errors (ping is int, not string)" {
                let errors = analyse "e.on(\"ping\", fun p -> printfn \"%s\" p)"
                Expect.isNonEmpty errors "the ping handler param is int; a %s body must error"
            }

            test "(on) a DIFFERENT key types its own payload (pong:string)" {
                let errors = analyse "e.on(\"pong\", fun p -> printfn \"%s\" p)"
                Expect.isEmpty errors (sprintf "expected p : string, got:\n%s" (errorText errors))
            }

            // ── T[K] fold types the emit payload ──
            test "(emit) the payload is typed from the addressed member (ping:int)" {
                let errors = analyse "e.emit(\"ping\", 5)"
                Expect.isEmpty errors (sprintf "expected an int payload, got:\n%s" (errorText errors))
            }

            test "(emit) a wrong-typed payload errors (ping wants int)" {
                let errors = analyse "e.emit(\"ping\", \"x\")"
                Expect.isNonEmpty errors "a string payload for the int key 'ping' must error"
            }

            // ── conditional fold picks the branch by `subsumes` membership ──
            test "(conditional) the TRUE branch is selected when check subsumes extends" {
                // `string extends string ? int : bool` folds to `int`.
                Expect.isEmpty (analyse "e.cond(5)") "true-branch param folds to int"
                Expect.isNonEmpty (analyse "e.cond(true)") "a bool arg must not match the int branch"
            }

            test "(conditional) the FALSE branch is selected when check is unrelated" {
                // `int extends string ? int : bool` folds to `bool`.
                Expect.isEmpty (analyse "e.cond2(true)") "false-branch param folds to bool"
                Expect.isNonEmpty (analyse "e.cond2(5)") "an int arg must not match the bool branch"
            }
        ]
