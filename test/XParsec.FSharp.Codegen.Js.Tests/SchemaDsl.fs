module XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// Shared `Vesper.Ts.Manifest.Schema` builder set for the TS-provider tests. Previously
// re-declared privately in each package's test file; consolidated here so a schema shape
// change (or a new `TypeRef`/`Signature`/`Member` field) is a one-site edit. Building a
// manifest through these total F# constructors — rather than a raw JSON string handed to
// `Codec.deserialize` — moves a mis-shaped schema from a run-time parse failure to a
// compile error.

open Vesper.Ts.Manifest

// ─── TypeRef constructors ──────────────────────────────────────────────────────

/// A named/intrinsic type with no arguments (`int`, `string`, `Bus`).
let named (n: string) : Schema.TypeRef = Schema.TypeRef.Named(n, [])

/// A named generic type applied to `args` (`Emitter<Events>`).
let namedG (n: string) (args: Schema.TypeRef list) : Schema.TypeRef = Schema.TypeRef.Named(n, args)

/// A declaring-axis open type parameter (the enclosing type's `i`-th typar).
let typar (i: int) : Schema.TypeRef = Schema.TypeRef.Typar i

/// A method-axis open type parameter (a generic MEMBER's own `i`-th typar).
let methodTypar (i: int) : Schema.TypeRef = Schema.TypeRef.MethodTypar i

/// A curried function arrow (`int -> unit`).
let fn (args: Schema.TypeRef list) (ret: Schema.TypeRef) : Schema.TypeRef = Schema.TypeRef.Fun(args, ret)

/// A structural union (`"auto" | "manual"`, `string | null`).
let union (members: Schema.TypeRef list) : Schema.TypeRef = Schema.TypeRef.Union members

/// A TS string-literal TYPE (`"auto"`).
let strLit (s: string) : Schema.TypeRef =
    Schema.TypeRef.Literal(Schema.LiteralValue.StringVal s)

/// TS `any` — arrives F#-side as the opaque `dynamic` JS intrinsic (`FTConst "dynamic"`).
let dynamic: Schema.TypeRef = Schema.TypeRef.Dynamic

/// `keyof t`.
let keyof (t: Schema.TypeRef) : Schema.TypeRef = Schema.TypeRef.KeyOf t

/// An anonymous structural object type: a tsc-`printed` diagnostic string paired with
/// its harvested `(name, type)` fields (`{x:number;y:number}`).
let structural (printed: string) (fields: (string * Schema.TypeRef) list) : Schema.TypeRef =
    Schema.TypeRef.Structural(printed, fields)

/// `obj[index]` (an indexed-access type).
let idx (obj: Schema.TypeRef) (index: Schema.TypeRef) : Schema.TypeRef =
    Schema.TypeRef.IndexedAccess(obj, index)

/// `check extends extends_ ? whenTrue : whenFalse`.
let cond
    (check: Schema.TypeRef)
    (extends_: Schema.TypeRef)
    (whenTrue: Schema.TypeRef)
    (whenFalse: Schema.TypeRef)
    : Schema.TypeRef =
    Schema.TypeRef.Conditional(check, extends_, whenTrue, whenFalse)

// ─── Param / Signature / Member builders ───────────────────────────────────────

/// A required, non-rest parameter.
let param' (name: string) (ty: Schema.TypeRef) : Schema.Param =
    {
        Name = name
        Type = ty
        Optional = false
        Rest = false
    }

/// A signature with `typeParams` own method typars, the given params, and no bounds.
let sigG (typeParams: int) (ps: Schema.Param list) (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = typeParams
        TypeParamBounds = []
        Params = ps
        Returns = ret
    }

/// A non-generic nullary signature (`() -> ret`).
let sig0 (ret: Schema.TypeRef) : Schema.Signature = sigG 0 [] ret

/// A non-generic unary signature (`(pname: pty) -> ret`).
let sig1 (pname: string) (pty: Schema.TypeRef) (ret: Schema.TypeRef) : Schema.Signature =
    sigG 0 [ param' pname pty ] ret

/// A non-generic binary signature (`(p1n: p1t, p2n: p2t) -> ret`).
let sig2
    (p1n: string)
    (p1t: Schema.TypeRef)
    (p2n: string)
    (p2t: Schema.TypeRef)
    (ret: Schema.TypeRef)
    : Schema.Signature =
    sigG 0 [ param' p1n p1t; param' p2n p2t ] ret

/// A method member with the given signature list and static flag.
let methodOf (name: string) (isStatic: bool) (sigs: Schema.Signature list) : Schema.Member =
    {
        Name = name
        Kind = Schema.MemberKind.Method
        Type = None
        Signatures = sigs
        Static = isStatic
        Optional = false
    }

/// An INSTANCE method with a single signature (the common interface-member form).
let method' (name: string) (sg: Schema.Signature) : Schema.Member = methodOf name false [ sg ]

/// A STATIC method carrying one or more (overloaded) signatures.
let staticMethod' (name: string) (sigs: Schema.Signature list) : Schema.Member = methodOf name true sigs

// ─── Refs-table builders (foreign identity: home + kind + arity) ────────────────

/// A refs-table entry pairing the FOREIGN bare name with its `{home; kind; arity}`
/// identity. `Box` referenced from package `A` at arity 1: `refEntry "Box" "A"
/// Schema.RefKind.Class 1`.
let refEntry (name: string) (home: string) (kind: Schema.RefKind) (arity: int) : string * Schema.RefEntry =
    name,
    {
        Home = home
        Kind = kind
        Arity = arity
    }

/// An instance data Property of type `ty`.
let property' (name: string) (ty: Schema.TypeRef) : Schema.Member =
    {
        Name = name
        Kind = Schema.MemberKind.Property
        Type = Some ty
        Signatures = []
        Static = false
        Optional = false
    }

/// An OPTIONAL instance data Property (`name?: ty`).
let optProperty' (name: string) (ty: Schema.TypeRef) : Schema.Member =
    { property' name ty with
        Optional = true
    }
