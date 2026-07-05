/// JSON codec for the manifest schema, built on `XParsec.Json` (the repo's own
/// multi-platform parser) — no external JSON dependency. `encode`/`decode` map
/// to/from `JsonValue`; `serialize`/`deserialize` go all the way to/from text
/// (parse via `JsonParsers`, emit via `JsonWriter`). The same code runs on .NET
/// (loader) and under Fable (extractor), so producer and consumer share one
/// wire format.
module Vesper.Ts.Manifest.Codec

open System.Collections.Immutable

open XParsec
open XParsec.Json

open Vesper.Ts.Manifest.Schema

// ─── Result plumbing ───────────────────────────────────────────────────────

let inline private (>>=) (r: Result<'a, string>) (f: 'a -> Result<'b, string>) = Result.bind f r

/// Minimal `result { }` CE over `Result<_, string>` — keeps the decoders flat
/// (linear `let!`s) instead of nested `>>= fun x ->` ladders, without taking a
/// dependency on an external Result library. Fable-compiles (just `Result.bind`).
type private ResultBuilder() =
    member inline _.Bind(r: Result<'a, string>, f: 'a -> Result<'b, string>) = Result.bind f r
    member inline _.Return(x: 'a) : Result<'a, string> = Ok x
    member inline _.ReturnFrom(r: Result<'a, string>) = r
    member inline _.Zero() : Result<unit, string> = Ok()
    member inline _.Delay(f: unit -> Result<'a, string>) = f
    member inline _.Run(f: unit -> Result<'a, string>) = f ()

    member inline _.Combine(r: Result<unit, string>, f: unit -> Result<'a, string>) =
        match r with
        | Ok() -> f ()
        | Error e -> Error e

let private result = ResultBuilder()

let private traverse (f: 'a -> Result<'b, string>) (xs: 'a seq) : Result<'b list, string> =
    let acc = System.Collections.Generic.List<'b>()
    let mutable err = None
    use e = xs.GetEnumerator()

    while err.IsNone && e.MoveNext() do
        match f e.Current with
        | Ok v -> acc.Add v
        | Error msg -> err <- Some msg

    match err with
    | Some msg -> Error msg
    | None -> Ok(List.ofSeq acc)

// ─── JsonValue readers ─────────────────────────────────────────────────────

let private asObject =
    function
    | JsonValue.Object m -> Ok m
    | other -> Error(sprintf "expected object, got %A" other)

let private asString =
    function
    | JsonValue.String s -> Ok s
    | other -> Error(sprintf "expected string, got %A" other)

let private asInt =
    function
    | JsonValue.Number n -> Ok(int n)
    | other -> Error(sprintf "expected number, got %A" other)

let private asBool =
    function
    | JsonValue.True -> Ok true
    | JsonValue.False -> Ok false
    | other -> Error(sprintf "expected bool, got %A" other)

let private asArray =
    function
    | JsonValue.Array a -> Ok a
    | other -> Error(sprintf "expected array, got %A" other)

let private tryField (name: string) (m: JsonObject) : JsonValue option =
    let mutable result = None
    let mutable i = 0

    while result.IsNone && i < m.Length do
        if m.[i].Name = name then
            result <- Some m.[i].Value

        i <- i + 1

    result

let private field (name: string) (m: JsonObject) : Result<JsonValue, string> =
    match tryField name m with
    | Some v -> Ok v
    | None -> Error(sprintf "missing field '%s'" name)

/// Read a required field and run a reader/decoder over it.
let inline private readField (name: string) (reader: JsonValue -> Result<'a, string>) (m: JsonObject) =
    field name m >>= reader

/// Read a required array field and decode each element.
let inline private listField (name: string) (decode: JsonValue -> Result<'a, string>) (m: JsonObject) =
    field name m >>= asArray >>= traverse decode

/// Read an optional field (absent or JSON `null` → `None`).
let private optField
    (name: string)
    (decode: JsonValue -> Result<'a, string>)
    (m: JsonObject)
    : Result<'a option, string> =
    match tryField name m with
    | Some JsonValue.Null
    | None -> Ok None
    | Some v -> decode v >>= (fun x -> Ok(Some x))

// ─── JsonValue builders ────────────────────────────────────────────────────

let private jObj (fields: (string * JsonValue) list) : JsonValue =
    JsonValue.Object(ImmutableArray.CreateRange(fields |> List.map (fun (n, v) -> { Name = n; Value = v })))

let private jArr (xs: JsonValue list) : JsonValue =
    JsonValue.Array(ImmutableArray.CreateRange xs)

let private jStr (s: string) = JsonValue.String s
let private jInt (i: int) = JsonValue.Number(float i)

let private jBool b =
    if b then JsonValue.True else JsonValue.False

let private jStrOpt =
    function
    | Some s -> JsonValue.String s
    | None -> JsonValue.Null

// ─── literal payload (shared by `TypeRef.Literal` and enum member values) ───

let private asInt64 =
    function
    | JsonValue.Number n -> Ok(int64 n)
    | other -> Error(sprintf "expected number, got %A" other)

/// The `kind`/`value` field pair a type-tagged literal contributes to its host
/// object. ONE encoding for the two wire hosts: `TypeRef.Literal` splices it
/// beside its `k` tag; an enum member value is exactly this object.
let private literalPayloadFields (v: LiteralValue) : (string * JsonValue) list =
    match v with
    | LiteralValue.IntVal n -> [ "kind", jStr "int"; "value", JsonValue.Number(float n) ]
    | LiteralValue.StringVal s -> [ "kind", jStr "string"; "value", jStr s ]

let private decodeLiteralPayload (m: JsonObject) : Result<LiteralValue, string> =
    result {
        let! kind = readField "kind" asString m

        match kind with
        | "int" ->
            let! n = readField "value" asInt64 m
            return LiteralValue.IntVal n
        | "string" ->
            let! s = readField "value" asString m
            return LiteralValue.StringVal s
        | other -> return! Error(sprintf "unknown literal kind '%s'" other)
    }

// ─── TypeRef ───────────────────────────────────────────────────────────────

let rec encodeTypeRef (t: TypeRef) : JsonValue =
    match t with
    | TypeRef.Named(name, args) ->
        jObj
            [
                "k", jStr "named"
                "name", jStr name
                "args", jArr (List.map encodeTypeRef args)
            ]
    | TypeRef.Typar i -> jObj [ "k", jStr "typar"; "i", jInt i ]
    | TypeRef.MethodTypar i -> jObj [ "k", jStr "methodTypar"; "i", jInt i ]
    | TypeRef.Fun(args, ret) ->
        jObj
            [
                "k", jStr "fun"
                "args", jArr (List.map encodeTypeRef args)
                "ret", encodeTypeRef ret
            ]
    | TypeRef.Tuple items -> jObj [ "k", jStr "tuple"; "items", jArr (List.map encodeTypeRef items) ]
    | TypeRef.Union members -> jObj [ "k", jStr "union"; "members", jArr (List.map encodeTypeRef members) ]
    // A literal TYPE carries its constant, tagged int/string exactly like an enum
    // member value (the shared payload pair). ADDITIVE — no `SchemaVersion` bump
    // (prototyping policy).
    | TypeRef.Literal v -> jObj (("k", jStr "literal") :: literalPayloadFields v)
    // keyof / indexed-access / conditional: FAITHFUL carrier arms (design §"keyof …
    // ride on top"), each recording its child type(s) verbatim so the front end can
    // ground-evaluate later. ADDITIVE — no `SchemaVersion` bump (prototyping policy).
    | TypeRef.KeyOf t -> jObj [ "k", jStr "keyof"; "ty", encodeTypeRef t ]
    | TypeRef.IndexedAccess(objTy, index) ->
        jObj
            [
                "k", jStr "indexedAccess"
                "obj", encodeTypeRef objTy
                "index", encodeTypeRef index
            ]
    | TypeRef.Conditional(check, extends, whenTrue, whenFalse) ->
        jObj
            [
                "k", jStr "conditional"
                "check", encodeTypeRef check
                "extends", encodeTypeRef extends
                "whenTrue", encodeTypeRef whenTrue
                "whenFalse", encodeTypeRef whenFalse
            ]
    | TypeRef.Dynamic -> jObj [ "k", jStr "dynamic" ]
    | TypeRef.Structural(hash, fields, index) ->
        jObj (
            [
                "k", jStr "structural"
                "hash", jStr hash
                "fields",
                jArr (
                    fields
                    |> List.map (fun (n, ft) -> jObj [ "name", jStr n; "type", encodeTypeRef ft ])
                )
            ]
            @ encodeIndexFields index
        )

/// The `index` facet an index-signature-bearing host (`Structural`/`Interface`/`Class`)
/// contributes to its wire object — a JSON ARRAY of `{ key, value }` pairs, OMITTED (not
/// `null`) when EMPTY so a facet-free host stays BYTE-IDENTICAL to a pre-facet golden (the
/// `refs`/`typeParamBounds` omit-when-empty precedent). In the `encodeTypeRef` rec group
/// so both it and `encodeExport` reach it.
and private encodeIndexFields (index: (TypeRef * TypeRef) list) : (string * JsonValue) list =
    match index with
    | [] -> []
    | pairs ->
        [
            "index",
            jArr (
                pairs
                |> List.map (fun (k, v) -> jObj [ "key", encodeTypeRef k; "value", encodeTypeRef v ])
            )
        ]

let rec decodeTypeRef (j: JsonValue) : Result<TypeRef, string> =
    result {
        let! m = asObject j
        let! k = readField "k" asString m

        match k with
        | "named" ->
            let! name = readField "name" asString m
            let! args = listField "args" decodeTypeRef m
            return TypeRef.Named(name, args)
        | "typar" ->
            let! i = readField "i" asInt m
            return TypeRef.Typar i
        | "methodTypar" ->
            let! i = readField "i" asInt m
            return TypeRef.MethodTypar i
        | "fun" ->
            let! args = listField "args" decodeTypeRef m
            let! ret = readField "ret" decodeTypeRef m
            return TypeRef.Fun(args, ret)
        | "tuple" ->
            let! items = listField "items" decodeTypeRef m
            return TypeRef.Tuple items
        | "union" ->
            let! members = listField "members" decodeTypeRef m
            return TypeRef.Union members
        | "literal" ->
            let! v = decodeLiteralPayload m
            return TypeRef.Literal v
        | "keyof" ->
            let! t = readField "ty" decodeTypeRef m
            return TypeRef.KeyOf t
        | "indexedAccess" ->
            let! objTy = readField "obj" decodeTypeRef m
            let! index = readField "index" decodeTypeRef m
            return TypeRef.IndexedAccess(objTy, index)
        | "conditional" ->
            let! check = readField "check" decodeTypeRef m
            let! extends = readField "extends" decodeTypeRef m
            let! whenTrue = readField "whenTrue" decodeTypeRef m
            let! whenFalse = readField "whenFalse" decodeTypeRef m
            return TypeRef.Conditional(check, extends, whenTrue, whenFalse)
        | "dynamic" -> return TypeRef.Dynamic
        | "structural" ->
            let! h = readField "hash" asString m
            let! fields = listField "fields" decodeStructField m
            let! index = decodeIndex m
            return TypeRef.Structural(h, fields, index)
        | other -> return! Error(sprintf "unknown TypeRef kind '%s'" other)
    }

and private decodeStructField (j: JsonValue) : Result<string * TypeRef, string> =
    result {
        let! m = asObject j
        let! n = readField "name" asString m
        let! t = readField "type" decodeTypeRef m
        return (n, t)
    }

/// Read the omitted-when-empty `index` facet (a JSON array of `{ key, value }` pairs) off
/// a host object (`Structural`/`Interface`/`Class`); absent or `null` → `[]`. In the
/// `decodeTypeRef` rec group so both it and `decodeExport` reach it.
and private decodeIndex (m: JsonObject) : Result<(TypeRef * TypeRef) list, string> =
    match tryField "index" m with
    | None
    | Some JsonValue.Null -> Ok []
    | Some v ->
        asArray v
        >>= traverse (fun el ->
            result {
                let! im = asObject el
                let! k = readField "key" decodeTypeRef im
                let! vv = readField "value" decodeTypeRef im
                return (k, vv)
            }
        )

// ─── leaf enums ────────────────────────────────────────────────────────────

let private encodeMemberKind =
    function
    | MemberKind.Property -> jStr "property"
    | MemberKind.Method -> jStr "method"

let private decodeMemberKind j =
    asString j
    >>= function
        | "property" -> Ok MemberKind.Property
        | "method" -> Ok MemberKind.Method
        | o -> Error(sprintf "unknown member kind '%s'" o)

let private encodeImport =
    function
    | ImportShape.Named -> jStr "named"
    | ImportShape.Default -> jStr "default"
    | ImportShape.Namespace -> jStr "namespace"
    | ImportShape.CommonJsExport -> jStr "commonjs"

let private decodeImport j =
    asString j
    >>= function
        | "named" -> Ok ImportShape.Named
        | "default" -> Ok ImportShape.Default
        | "namespace" -> Ok ImportShape.Namespace
        | "commonjs" -> Ok ImportShape.CommonJsExport
        | o -> Error(sprintf "unknown import shape '%s'" o)

// ─── enum member values ──────────────────────────────────────────────────────

/// A type-tagged enum member value (or `None` for a computed member) — the shared
/// literal payload as its own object, `null` for the computed case.
let private encodeEnumValue (v: LiteralValue option) : JsonValue =
    match v with
    | Some lit -> jObj (literalPayloadFields lit)
    | None -> JsonValue.Null

let private decodeEnumValue (j: JsonValue) : Result<LiteralValue option, string> =
    match j with
    | JsonValue.Null -> Ok None
    | _ -> asObject j >>= decodeLiteralPayload >>= (Some >> Ok)

// ─── Diagnostic ──────────────────────────────────────────────────────────────

let private encodeSeverity =
    function
    | Severity.Warning -> jStr "warning"
    | Severity.Error -> jStr "error"

let private decodeSeverity j =
    asString j
    >>= function
        | "warning" -> Ok Severity.Warning
        | "error" -> Ok Severity.Error
        | o -> Error(sprintf "unknown severity '%s'" o)

let private encodeSpan (s: Span) =
    jObj [ "file", jStr s.File; "start", jInt s.Start; "end", jInt s.End ]

let private decodeSpan j =
    result {
        let! m = asObject j
        let! file = readField "file" asString m
        let! start = readField "start" asInt m
        let! end_ = readField "end" asInt m

        return
            {
                File = file
                Start = start
                End = end_
            }
    }

let private encodeDiagnostic (d: Diagnostic) =
    jObj
        [
            "severity", encodeSeverity d.Severity
            "code", jStr d.Code.Wire
            "symbol", jStr d.Symbol
            "span",
            (match d.Span with
             | Some s -> encodeSpan s
             | None -> JsonValue.Null)
            "message", jStr d.Message
        ]

let private decodeDiagnostic j =
    result {
        let! m = asObject j
        let! severity = readField "severity" decodeSeverity m
        // `OfWire` is total (an unrecognised code decodes as `DiagCode.Unknown`),
        // so a manifest from a NEWER extractor never fails the whole decode here.
        let! code = readField "code" asString m
        let! symbol = readField "symbol" asString m
        let! span = optField "span" decodeSpan m
        let! message = readField "message" asString m

        return
            {
                Severity = severity
                Code = DiagCode.OfWire code
                Symbol = symbol
                Span = span
                Message = message
            }
    }

// ─── Param / Signature / Member ────────────────────────────────────────────

let private encodeParam (p: Param) =
    jObj
        [
            "name", jStr p.Name
            "type", encodeTypeRef p.Type
            "optional", jBool p.Optional
            "rest", jBool p.Rest
        ]

let private decodeParam j =
    result {
        let! m = asObject j
        let! name = readField "name" asString m
        let! ty = readField "type" decodeTypeRef m
        let! opt = readField "optional" asBool m
        let! rest = readField "rest" asBool m

        return
            {
                Name = name
                Type = ty
                Optional = opt
                Rest = rest
            }
    }

let private encodeSignature (s: Signature) =
    let baseFields =
        [
            "typeParams", jInt s.TypeParams
            "params", jArr (List.map encodeParam s.Params)
            "returns", encodeTypeRef s.Returns
        ]

    // Emit the per-typar bounds ONLY when at least one constraint is present, so a
    // constraint-free signature (the common case) stays BYTE-IDENTICAL to a pre-slot
    // golden. When emitted the list is full-length (`null` per unconstrained slot) so
    // it stays aligned to the method axis; the decoder rebuilds an all-`None` list of
    // the right length when the field is absent.
    let fields =
        if s.TypeParamBounds |> List.exists Option.isSome then
            baseFields
            @ [
                "typeParamBounds",
                jArr (
                    s.TypeParamBounds
                    |> List.map (
                        function
                        | Some t -> encodeTypeRef t
                        | None -> JsonValue.Null
                    )
                )
            ]
        else
            baseFields

    jObj fields

let private decodeSignature j =
    result {
        let! m = asObject j
        let! tp = readField "typeParams" asInt m
        let! prms = listField "params" decodeParam m
        let! ret = readField "returns" decodeTypeRef m

        let! bounds =
            match tryField "typeParamBounds" m with
            | None -> Ok(List.replicate tp None)
            | Some v ->
                asArray v
                >>= traverse (
                    function
                    | JsonValue.Null -> Ok None
                    | x -> decodeTypeRef x |> Result.map Some
                )
                // The bounds list indexes the METHOD axis: a length that disagrees
                // with `typeParams` would silently misalign every bound the provider
                // reads by index, so a malformed manifest fails HERE, not downstream.
                >>= (fun bs ->
                    if List.length bs = tp then
                        Ok bs
                    else
                        Error(sprintf "typeParamBounds length %d does not match typeParams %d" (List.length bs) tp)
                )

        return
            {
                TypeParams = tp
                TypeParamBounds = bounds
                Params = prms
                Returns = ret
            }
    }

let private encodeMember (mem: Member) =
    jObj
        [
            "name", jStr mem.Name
            "kind", encodeMemberKind mem.Kind
            "type",
            (match mem.Type with
             | Some t -> encodeTypeRef t
             | None -> JsonValue.Null)
            "signatures", jArr (List.map encodeSignature mem.Signatures)
            "static", jBool mem.Static
            "optional", jBool mem.Optional
        ]

let private decodeMember j =
    result {
        let! m = asObject j
        let! name = readField "name" asString m
        let! kind = readField "kind" decodeMemberKind m
        let! signatures = listField "signatures" decodeSignature m
        let! isStatic = readField "static" asBool m
        let! opt = readField "optional" asBool m
        let! ty = optField "type" decodeTypeRef m

        return
            {
                Name = name
                Kind = kind
                Type = ty
                Signatures = signatures
                Static = isStatic
                Optional = opt
            }
    }

// ─── Export (recursive via Namespace) ──────────────────────────────────────

let rec encodeExport (e: Export) : JsonValue =
    match e with
    | Export.Function(name, sigs, import) ->
        jObj
            [
                "export", jStr "function"
                "name", jStr name
                "signatures", jArr (List.map encodeSignature sigs)
                "import", encodeImport import
            ]
    | Export.Interface(name, tp, members, heritage, index) ->
        jObj (
            [
                "export", jStr "interface"
                "name", jStr name
                "typeParams", jInt tp
                "members", jArr (List.map encodeMember members)
                "heritage", jArr (List.map encodeTypeRef heritage)
            ]
            @ encodeIndexFields index
        )
    | Export.Class(name, tp, members, heritage, import, index) ->
        jObj (
            [
                "export", jStr "class"
                "name", jStr name
                "typeParams", jInt tp
                "members", jArr (List.map encodeMember members)
                "heritage", jArr (List.map encodeTypeRef heritage)
                "import", encodeImport import
            ]
            @ encodeIndexFields index
        )
    | Export.TypeAlias(name, tp, target) ->
        jObj
            [
                "export", jStr "typeAlias"
                "name", jStr name
                "typeParams", jInt tp
                "target", encodeTypeRef target
            ]
    | Export.Enum(name, members) ->
        jObj
            [
                "export", jStr "enum"
                "name", jStr name
                "members",
                jArr (
                    members
                    |> List.map (fun (n, v) -> jObj [ "name", jStr n; "value", encodeEnumValue v ])
                )
            ]
    | Export.Variable(name, ty, isConst, import) ->
        jObj
            [
                "export", jStr "variable"
                "name", jStr name
                "type", encodeTypeRef ty
                "const", jBool isConst
                "import", encodeImport import
            ]
    | Export.Namespace(name, exports) ->
        jObj
            [
                "export", jStr "namespace"
                "name", jStr name
                "exports", jArr (List.map encodeExport exports)
            ]

let rec decodeExport (j: JsonValue) : Result<Export, string> =
    result {
        let! m = asObject j
        let! tag = readField "export" asString m
        let! name = readField "name" asString m

        match tag with
        | "function" ->
            let! signatures = listField "signatures" decodeSignature m
            let! import = readField "import" decodeImport m
            return Export.Function(name, signatures, import)
        | "interface" ->
            let! tp = readField "typeParams" asInt m
            let! members = listField "members" decodeMember m
            let! heritage = listField "heritage" decodeTypeRef m
            let! index = decodeIndex m
            return Export.Interface(name, tp, members, heritage, index)
        | "class" ->
            let! tp = readField "typeParams" asInt m
            let! members = listField "members" decodeMember m
            let! heritage = listField "heritage" decodeTypeRef m
            let! import = readField "import" decodeImport m
            let! index = decodeIndex m
            return Export.Class(name, tp, members, heritage, import, index)
        | "typeAlias" ->
            let! tp = readField "typeParams" asInt m
            let! target = readField "target" decodeTypeRef m
            return Export.TypeAlias(name, tp, target)
        | "enum" ->
            let! members = listField "members" decodeEnumMember m
            return Export.Enum(name, members)
        | "variable" ->
            let! ty = readField "type" decodeTypeRef m
            let! isConst = readField "const" asBool m
            let! import = readField "import" decodeImport m
            return Export.Variable(name, ty, isConst, import)
        | "namespace" ->
            let! exports = listField "exports" decodeExport m
            return Export.Namespace(name, exports)
        | other -> return! Error(sprintf "unknown export '%s'" other)
    }

and private decodeEnumMember (j: JsonValue) : Result<string * LiteralValue option, string> =
    result {
        let! m = asObject j
        let! n = readField "name" asString m
        // The `value` field is always present (JSON `null` for a computed member);
        // `decodeEnumValue` maps that null → `None`, so read it directly rather
        // than through `optField` (which would double-wrap the option).
        let! v = readField "value" decodeEnumValue m
        return (n, v)
    }

// ─── Refs table (foreign identity: home + kind + arity) ────────────────────

let private encodeRefEntry (name: string) (e: RefEntry) : JsonValue =
    jObj
        [
            "name", jStr name
            "home", jStr e.Home
            "kind", jStr e.Kind.Wire
            "arity", jInt e.Arity
        ]

let private decodeRefEntry (j: JsonValue) : Result<string * RefEntry, string> =
    result {
        let! m = asObject j
        let! name = readField "name" asString m
        let! home = readField "home" asString m
        // `RefKind.OfWire` is a closed-set decode (`Result`), unlike `DiagCode.OfWire`:
        // an unknown kind fails the manifest rather than degrading, since the provider's
        // re-mint dispatch depends on knowing it.
        let! kind = readField "kind" asString m >>= RefKind.OfWire
        let! arity = readField "arity" asInt m

        return
            (name,
             {
                 Home = home
                 Kind = kind
                 Arity = arity
             })
    }

// ─── Manifest + text entrypoints ───────────────────────────────────────────

let encodeManifest (man: PackageManifest) : JsonValue =
    let baseFields =
        [
            "schemaVersion", jInt man.SchemaVersion
            "package", jStr man.Package
            "version", jStrOpt man.Version
            "exports", jArr (List.map encodeExport man.Exports)
            "diagnostics", jArr (List.map encodeDiagnostic man.Diagnostics)
        ]

    // OMIT `refs` when empty, so a ref-free manifest stays byte-identical to a
    // pre-refs golden — the `typeParamBounds` omit-when-empty precedent. When present
    // the wire order mirrors the source list (`Refs` is a list, not a `Map`).
    let fields =
        match man.Refs with
        | [] -> baseFields
        | refs ->
            baseFields
            @ [ "refs", jArr (refs |> List.map (fun (n, e) -> encodeRefEntry n e)) ]

    jObj fields

let decodeManifest (j: JsonValue) : Result<PackageManifest, string> =
    result {
        let! m = asObject j
        let! ver = readField "schemaVersion" asInt m

        // Exactly one wire version is understood per build. A different version
        // means the wire format has evolved —
        // throw loudly so versioning/back-compat gets designed deliberately rather
        // than decoded against a grammar it may not match.
        if ver <> SchemaVersion then
            failwithf "unsupported manifest schemaVersion %d (this build only understands v%d)" ver SchemaVersion

        let! pkg = readField "package" asString m
        let! version = optField "version" asString m
        let! exports = listField "exports" decodeExport m

        // Tolerant on read: an absent `diagnostics` field decodes to `[]`. Required
        // here would be self-defeating — golden regeneration round-trips through
        // `deserialize`, so a strict read could never parse a pre-channel manifest to
        // re-serialize it. A PRESENT field is still validated strictly (each element
        // must decode), and `encodeManifest` always WRITES the field.
        let! diagnostics =
            match tryField "diagnostics" m with
            | None -> Ok []
            | Some v -> (asArray v >>= traverse decodeDiagnostic)

        // Tolerant on read, exactly like `diagnostics`: an absent `refs` field decodes
        // to `[]` (the codec omits it when empty), so a pre-refs manifest round-trips.
        let! refs =
            match tryField "refs" m with
            | None -> Ok []
            | Some v -> (asArray v >>= traverse decodeRefEntry)

        return
            {
                SchemaVersion = ver
                Package = pkg
                Version = version
                Exports = exports
                Diagnostics = diagnostics
                Refs = refs
            }
    }

let private parseJson (json: string) : Result<JsonValue, string> =
    let reader = Reader.ofString json ()

    match JsonParsers<_>.Parser reader with
    | Ok v -> Ok v
    | Error e -> Error(sprintf "JSON parse error: %A" e)

/// Pretty-printed manifest JSON (2-space indent).
let serialize (man: PackageManifest) : string =
    JsonWriter.writeIndented 2 (encodeManifest man)

/// Parse + decode a manifest from JSON text.
let deserialize (json: string) : Result<PackageManifest, string> = parseJson json >>= decodeManifest
