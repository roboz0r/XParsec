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

let private optStringField (name: string) (m: JsonObject) : Result<string option, string> =
    match tryField name m with
    | Some JsonValue.Null
    | None -> Ok None
    | Some v -> asString v >>= (fun s -> Ok(Some s))

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
    | TypeRef.Fun(args, ret) ->
        jObj
            [
                "k", jStr "fun"
                "args", jArr (List.map encodeTypeRef args)
                "ret", encodeTypeRef ret
            ]
    | TypeRef.Tuple items -> jObj [ "k", jStr "tuple"; "items", jArr (List.map encodeTypeRef items) ]
    | TypeRef.Union members -> jObj [ "k", jStr "union"; "members", jArr (List.map encodeTypeRef members) ]
    | TypeRef.Dynamic -> jObj [ "k", jStr "dynamic" ]
    | TypeRef.Structural(hash, fields) ->
        jObj
            [
                "k", jStr "structural"
                "hash", jStr hash
                "fields",
                jArr (
                    fields
                    |> List.map (fun (n, ft) -> jObj [ "name", jStr n; "type", encodeTypeRef ft ])
                )
            ]

let rec decodeTypeRef (j: JsonValue) : Result<TypeRef, string> =
    asObject j
    >>= fun m ->
        field "k" m
        >>= asString
        >>= fun k ->
            match k with
            | "named" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "args" m
                    >>= asArray
                    >>= traverse decodeTypeRef
                    >>= fun a -> Ok(TypeRef.Named(name, a))
            | "typar" -> field "i" m >>= asInt >>= (fun i -> Ok(TypeRef.Typar i))
            | "fun" ->
                field "args" m
                >>= asArray
                >>= traverse decodeTypeRef
                >>= fun a -> field "ret" m >>= decodeTypeRef >>= (fun r -> Ok(TypeRef.Fun(a, r)))
            | "tuple" ->
                field "items" m
                >>= asArray
                >>= traverse decodeTypeRef
                >>= fun a -> Ok(TypeRef.Tuple a)
            | "union" ->
                field "members" m
                >>= asArray
                >>= traverse decodeTypeRef
                >>= fun a -> Ok(TypeRef.Union a)
            | "dynamic" -> Ok TypeRef.Dynamic
            | "structural" ->
                field "hash" m
                >>= asString
                >>= fun h ->
                    field "fields" m
                    >>= asArray
                    >>= traverse decodeStructField
                    >>= fun fields -> Ok(TypeRef.Structural(h, fields))
            | other -> Error(sprintf "unknown TypeRef kind '%s'" other)

and private decodeStructField (j: JsonValue) : Result<string * TypeRef, string> =
    asObject j
    >>= fun m ->
        field "name" m
        >>= asString
        >>= fun n -> field "type" m >>= decodeTypeRef >>= (fun t -> Ok(n, t))

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
    asObject j
    >>= fun m ->
        field "name" m
        >>= asString
        >>= fun name ->
            field "type" m
            >>= decodeTypeRef
            >>= fun ty ->
                field "optional" m
                >>= asBool
                >>= fun opt ->
                    field "rest" m
                    >>= asBool
                    >>= fun rest ->
                        Ok
                            {
                                Name = name
                                Type = ty
                                Optional = opt
                                Rest = rest
                            }

let private encodeSignature (s: Signature) =
    jObj
        [
            "typeParams", jInt s.TypeParams
            "params", jArr (List.map encodeParam s.Params)
            "returns", encodeTypeRef s.Returns
        ]

let private decodeSignature j =
    asObject j
    >>= fun m ->
        field "typeParams" m
        >>= asInt
        >>= fun tp ->
            field "params" m
            >>= asArray
            >>= traverse decodeParam
            >>= fun prms ->
                field "returns" m
                >>= decodeTypeRef
                >>= fun ret ->
                    Ok
                        {
                            TypeParams = tp
                            Params = prms
                            Returns = ret
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
    asObject j
    >>= fun m ->
        field "name" m
        >>= asString
        >>= fun name ->
            field "kind" m
            >>= decodeMemberKind
            >>= fun kind ->
                field "signatures" m
                >>= asArray
                >>= traverse decodeSignature
                >>= fun signatures ->
                    field "static" m
                    >>= asBool
                    >>= fun isStatic ->
                        field "optional" m
                        >>= asBool
                        >>= fun opt ->
                            (match tryField "type" m with
                             | Some JsonValue.Null
                             | None -> Ok None
                             | Some t -> decodeTypeRef t >>= (fun x -> Ok(Some x)))
                            >>= fun ty ->
                                Ok
                                    {
                                        Name = name
                                        Kind = kind
                                        Type = ty
                                        Signatures = signatures
                                        Static = isStatic
                                        Optional = opt
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
    | Export.Interface(name, tp, members, heritage) ->
        jObj
            [
                "export", jStr "interface"
                "name", jStr name
                "typeParams", jInt tp
                "members", jArr (List.map encodeMember members)
                "heritage", jArr (List.map encodeTypeRef heritage)
            ]
    | Export.Class(name, tp, members, heritage, import) ->
        jObj
            [
                "export", jStr "class"
                "name", jStr name
                "typeParams", jInt tp
                "members", jArr (List.map encodeMember members)
                "heritage", jArr (List.map encodeTypeRef heritage)
                "import", encodeImport import
            ]
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
                "members", jArr (members |> List.map (fun (n, v) -> jObj [ "name", jStr n; "value", jStrOpt v ]))
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
    asObject j
    >>= fun m ->
        field "export" m
        >>= asString
        >>= fun tag ->
            match tag with
            | "function" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "signatures" m
                    >>= asArray
                    >>= traverse decodeSignature
                    >>= fun signatures ->
                        field "import" m
                        >>= decodeImport
                        >>= fun import -> Ok(Export.Function(name, signatures, import))
            | "interface" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "typeParams" m
                    >>= asInt
                    >>= fun tp ->
                        field "members" m
                        >>= asArray
                        >>= traverse decodeMember
                        >>= fun members ->
                            field "heritage" m
                            >>= asArray
                            >>= traverse decodeTypeRef
                            >>= fun heritage -> Ok(Export.Interface(name, tp, members, heritage))
            | "class" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "typeParams" m
                    >>= asInt
                    >>= fun tp ->
                        field "members" m
                        >>= asArray
                        >>= traverse decodeMember
                        >>= fun members ->
                            field "heritage" m
                            >>= asArray
                            >>= traverse decodeTypeRef
                            >>= fun heritage ->
                                field "import" m
                                >>= decodeImport
                                >>= fun import -> Ok(Export.Class(name, tp, members, heritage, import))
            | "typeAlias" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "typeParams" m
                    >>= asInt
                    >>= fun tp ->
                        field "target" m
                        >>= decodeTypeRef
                        >>= fun target -> Ok(Export.TypeAlias(name, tp, target))
            | "enum" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "members" m
                    >>= asArray
                    >>= traverse decodeEnumMember
                    >>= fun members -> Ok(Export.Enum(name, members))
            | "variable" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "type" m
                    >>= decodeTypeRef
                    >>= fun ty ->
                        field "const" m
                        >>= asBool
                        >>= fun isConst ->
                            field "import" m
                            >>= decodeImport
                            >>= fun import -> Ok(Export.Variable(name, ty, isConst, import))
            | "namespace" ->
                field "name" m
                >>= asString
                >>= fun name ->
                    field "exports" m
                    >>= asArray
                    >>= traverse decodeExport
                    >>= fun exports -> Ok(Export.Namespace(name, exports))
            | other -> Error(sprintf "unknown export '%s'" other)

and private decodeEnumMember (j: JsonValue) : Result<string * string option, string> =
    asObject j
    >>= fun m ->
        field "name" m
        >>= asString
        >>= fun n ->
            (match tryField "value" m with
             | Some JsonValue.Null
             | None -> Ok None
             | Some v -> asString v >>= (fun s -> Ok(Some s)))
            >>= fun v -> Ok(n, v)

// ─── Manifest + text entrypoints ───────────────────────────────────────────

let encodeManifest (man: PackageManifest) : JsonValue =
    jObj
        [
            "schemaVersion", jInt man.SchemaVersion
            "package", jStr man.Package
            "version", jStrOpt man.Version
            "exports", jArr (List.map encodeExport man.Exports)
        ]

let decodeManifest (j: JsonValue) : Result<PackageManifest, string> =
    asObject j
    >>= fun m ->
        field "schemaVersion" m
        >>= asInt
        >>= fun ver ->
            field "package" m
            >>= asString
            >>= fun pkg ->
                optStringField "version" m
                >>= fun version ->
                    field "exports" m
                    >>= asArray
                    >>= traverse decodeExport
                    >>= fun exports ->
                        Ok
                            {
                                SchemaVersion = ver
                                Package = pkg
                                Version = version
                                Exports = exports
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
