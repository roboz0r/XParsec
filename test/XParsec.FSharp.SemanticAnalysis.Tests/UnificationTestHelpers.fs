module XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyseParsed (input: string) (lexed, file) =
    let ctx = PassContext(realProvider.Value, Hashing.originSourceOfText lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    Unification.run ctx file
    ctx

let analyse (input: string) = analyseParsed input (parseFile input)

/// `analyse` for a source the parser can only complete by PATCHING it: what the passes
/// then say about the patched tree is the assertion.
let analyseRecovered (input: string) =
    analyseParsed input (parseRecoveredFile input)

// The bindings-accumulating trial matcher (`matchTypes`) behind
// `pickBestOverload`. These call the picker DIRECTLY on hand-built
// `ExternalMember` candidates: the front end does not yet form a
// user-declared overload SET, and the external providers reach the picker
// only through the call-site machinery, so a focused matcher test builds
// the candidate array itself. Each candidate is keyed as the member it is
// (a static method on a stub type `C`); only `argSig` length (arity) and
// the `Signature` parameters are load-bearing here.
let unitFt = FTConst(RuntimeNames.unitKey, EqArray.empty)
let intFt = FTConst(RuntimeNames.intKey, EqArray.empty)
let stringFt = FTConst(RuntimeNames.stringKey, EqArray.empty)

let overloadMember (paramFts: FrozenType list) (methodTyparArity: int) : ExternalMember =
    let parameters =
        match paramFts with
        | [] -> unitFt
        | [ p ] -> p
        | many -> FTTuple(EqArray.ofList many)

    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf
              (SymbolKeyOps.qualifiedTypeKeyOf "C" 0)
              "M"
              (EqArray.ofList paramFts)
              methodTyparArity
              MemberKind.Method
      ) with
        IsStatic = true
        MethodTyparArity = methodTyparArity
        Signature = TestHelpers.mkSignature 0 methodTyparArity parameters unitFt
    }

// A one-parameter candidate keyed by its parameter shape (so two same-name
// members are distinct); the declaring type is irrelevant to the pick.
let classMember (paramTy: SemType) : ExternalMember = overloadMember [ toFrozen paramTy ] 0

// A one-parameter instance member on the generic declaring type `Box<'a>`; its
// single declaring typar is substituted from the picker's `typeArgs` before any
// ranking, so `M('a)` at `Box<int>` presents an `int` parameter.
let boxMember (paramFt: FrozenType) : ExternalMember =
    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf
              (SymbolKeyOps.qualifiedTypeKeyOf "Box" 1)
              "M"
              (EqArray.singleton paramFt)
              0
              MemberKind.Method
      ) with
        IsStatic = false
        Signature = TestHelpers.mkSignature 1 0 paramFt unitFt
    }

let chosenParamsWith (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
    UnificationInferOverload.memberParamTypes (TypeStore()) typeArgs m

let pickWith
    (ctx: PassContext)
    (typeArgs: SemType[])
    (candidates: ExternalMember[])
    (args: SemType list)
    : ExternalMember voption =
    UnificationInferOverload.pickBestOverload ctx typeArgs candidates args

let chosenParams (m: ExternalMember) : SemType list =
    UnificationInferOverload.memberParamTypes (TypeStore()) [||] m

// Immutable class-type values from the `GrandBase :> Base :> Derived` chain the
// subtyping cases exercise — pure data, safe to share across parallel tests.
let grandBaseTy = TyClass("GrandBase", EqArray.empty)
let baseTy = TyClass("Base", EqArray.empty)
let derivedTy = TyClass("Derived", EqArray.empty)

// `pickBestOverload` needs a `PassContext` — `subsumes` walks the class hierarchy the
// ctx carries and `canon` is `capabilityCanonKey ctx`. Each factory returns a FRESH ctx
// so parallel tests never share its mutable state:
//   * `overloadCtx` — a minimal program; the pure-structural cases (no residue / shared
//     typar / non-ground arg) need only the BCL primitives it provides.
//   * `hierCtx` — a real `Base`/`Derived` source, so `ctx.Types` holds the inherit chain
//     `subsumes` reads.
let overloadCtx () = analyse "let _ = 0"

let hierCtx () =
    analyse
        "type GrandBase() =\n    member this.G = 1\ntype Base() =\n    inherit GrandBase()\n    member this.B = 1\ntype Derived() =\n    inherit Base()\n    member this.D = 1"

// --- User-declared member overload resolution ---------------
// These analyse a full source so the call-seam probe forms the candidate set from the
// type's own `Members` and picks by the argument types. The pattern-ident offset of
// `let <name>` locates each binding's key.
let keyOfLet (input: string) (name: string) =
    NodeKey.ofSource (input.IndexOf("let " + name + " ") + 4) NodeKind.PatIdent

let errors (ctx: PassContext) =
    ctx.Diagnostics |> Diagnostic.errors |> Seq.toList

// The union front door: `translateType` maps the CST `Type.UnionType` / `Type.Null`
// surface to a canonical `TyOr` via `mkUnion`. With no assignability yet, a union only
// enters here through an *annotation* on a parameter, whose fresh TyVar links to it
// without any subtyping — the function's domain is the translated union. We assert on the
// translated `SemType`, not on any call type-checking.
let unionDomainOf (input: string) : SemType =
    let ctx = analyse input
    let patKey = NodeKey.ofSource (input.IndexOf "f ") NodeKind.PatIdent

    match typeOf ctx patKey with
    | TyFun(dom, _) -> dom
    | other -> failtestf "expected f : _ -> _, got %A" other

// The directional `subsumes` query learns union membership. `unify` is untouched; these
// are read-only calls (no `Link` mutation), asserted directly. All three relations are
// covered, including the negative `A | B ⋠ A`.
let subsumeCtx () = analyse "let x = 1"
let intTy = BuiltinTypes.tyInt
let strTy = BuiltinTypes.tyString
let boolTy = BuiltinTypes.tyBool

// Constraint reduction. `equality` and `comparison` are deliberately asymmetric
// (§Constraints): a union satisfies EQUALITY iff *every* member does — generic `=` is
// total on the union's repr (cross-member is `false`, never throws) — but any real
// (≥2-member) union FAILS COMPARISON outright, because generic `compare` *throws* across
// distinct runtime types, so an individually-comparable member set is still
// non-comparable as a whole. `checkConstraint` is read-only here (no `Link` mutation), so
// the outcomes are asserted by direct calls.
let checkConstraintKind ctx kind ty =
    let c: SemanticConstraint =
        {
            Kind = kind
            DeclKey = NodeKey.ofSource 0 NodeKind.PatIdent
        }

    UnificationEngine.checkConstraint ctx c ty

// Binder narrowing + closed-union exhaustiveness. A `match` on a `TyOr` scrutinee narrows
// each `:? M as x` arm to `M`, narrows a fall-through catch-all to the residual `mkUnion
// (ts \ matched)`, and — because the union is *closed* — warns when the arms leave a
// member uncovered.
let hasUnionExhaustivenessWarning (ctx: PassContext) =
    ctx.Diagnostics
    |> Seq.exists (fun d -> d.Severity = Severity.Warning && d.Message.Contains "anonymous union")

// The closing freeze round-trip + backend handoff. No codegen — the front end must hand a
// well-formed `FTOr` (in canonical order) to the backend boundary. `TyOr → FTOr` is
// already mapped in `freezeTy`; this is the *end-to-end* assertion through a real annotated
// binding, run all the way through `Pipeline.analyse` (every pass + the final
// `SemType → FrozenType` freeze).
let freezeDecls (input: string) : Pooled.TastFile =
    let lexed, file = parseFile input
    // The freeze yields pools; these assertions read the decl tree, which `ofPools`
    // re-authors.
    TastUnpool.ofPools (Pipeline.analyse realProvider.Value (Hashing.originSourceOfText lexed) file)

// The frozen type of the (sole) top-level `let f` binding.
let frozenLetTy (file: Pooled.TastFile) : FrozenType =
    file.Decls
    |> EqArray.toList
    |> List.tryPick (fun d ->
        match d with
        // Both a function binding (`let f … = …`) and a plain value
        // freeze to `Let`, carrying the binding's frozen type as `ty`.
        | TDeclG.Let(ty = ty) -> Some ty
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failtest "expected a frozen `let` decl")
