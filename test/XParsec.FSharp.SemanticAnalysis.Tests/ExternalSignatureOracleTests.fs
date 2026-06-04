module XParsec.FSharp.SemanticAnalysis.Tests.ExternalSignatureOracleTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The external-signature-plan **step-1 oracle** — the load-bearing safety net
// for the two-headed window in which every external descriptor carries BOTH a
// legacy `SemType[] -> SemType` closure AND its derived `FrozenType` template.
//
// Two laws, the plan's wording:
//
//   1. `template ≡ toFrozen (closure markerArgs)` — the template is the freeze
//      of the closure run on the declaring-typar markers. This is
//      `templateOfClosure` by definition today (producers derive it that way),
//      so the test guards against *drift* once step 5 has producers build
//      templates natively: the two must still agree.
//
//   2. `closure args ≡ instantiate* template args` — the production realisers
//      (`instantiateDeclaring` for a declaring-only shape, `instantiateSignature`
//      for a member's two-axis signature) reproduce the closure byte-for-byte on
//      the post-freeze subset. This is the real check: it proves the realisers
//      correctly invert the freeze, so inference can read templates instead of
//      running the closures without behaviour change.
//
// Method-typar handling is checked separately (a method placeholder freshens to
// a `TyVar`, which has no structural counterpart in the closure's baked
// `TempTypar(Method,j)` — the equality there is by *sharing + level*, not value).

/// Ground (`TyVar`-free, `TempTypar`-free) types to substitute for declaring
/// args, so both the closure and `instantiate` produce structurally-comparable
/// `SemType`s (no reference-identity `TyVar` leaves to defeat `=`).
let private groundArgs: SemType[] =
    [|
        TyConst("int", EqArray.empty)
        TyConst("string", EqArray.empty)
        TyClass(SymbolKeyOps.qualifiedTypeKey "Test.Widget" 0, EqArray.empty)
    |]

let private kRec = SymbolKeyOps.qualifiedTypeKey "Test.Box" 1
let private kUnion = SymbolKeyOps.qualifiedTypeKey "Test.Option" 1

/// A representative closure paired with its declaring arity. Each is a *linear
/// substitution* (it only places its `args.[i]` into a fixed skeleton, never
/// inspects their content) — the shape every real descriptor closure has — so
/// `instantiate (templateOfClosure arity c) args` must equal `c args` on ground
/// args. None of these bake a method typar; the method axis is tested below.
let private declaringClosures: (string * int * (SemType[] -> SemType)) list =
    [
        "argless const", 0, (fun _ -> TyConst("bool", EqArray.empty))
        "identity typar", 1, (fun a -> a.[0])
        "array of typar", 1, (fun a -> TyConst("[]", EqArray.singleton a.[0]))
        "curried fun over two typars", 2, (fun a -> TyFun(a.[0], TyFun(a.[1], TyConst("unit", EqArray.empty))))
        "tupled fun", 2, (fun a -> TyFun(TyTuple(EqArray.ofList [ a.[0]; a.[1] ]), a.[0]))
        "record of typar", 1, (fun a -> TyRecord(kRec, EqArray.singleton a.[0]))
        "union of typar", 1, (fun a -> TyUnion(kUnion, EqArray.singleton a.[0]))
        "nested generic intrinsic",
        1,
        (fun a -> TyConst("[]", EqArray.singleton (TyUnion(kUnion, EqArray.singleton a.[0]))))
        "unknown head", 0, (fun _ -> TyUnknown "Unresolved.Head")
    ]

/// Slice `groundArgs` to the closure's arity (the declaring substitution the
/// caller mints fresh at a use site — here, ground stand-ins).
let private argsForArity (arity: int) : SemType[] = Array.sub groundArgs 0 arity

[<Tests>]
let tests =
    testList
        "ExternalSignature oracle (two-headed window)"
        [
            test "law 1: template ≡ toFrozen (closure markerArgs)" {
                for name, arity, closure in declaringClosures do
                    let template = templateOfClosure arity closure
                    let expected = toFrozen (closure (declaringMarkers arity))
                    Expect.equal template expected (sprintf "template of '%s'" name)
            }

            test "law 2: instantiateDeclaring template args ≡ closure args (no method axis)" {
                for name, arity, closure in declaringClosures do
                    let template = templateOfClosure arity closure
                    let args = argsForArity arity
                    let viaTemplate = instantiateDeclaring template args
                    let viaClosure = closure args
                    Expect.equal viaTemplate viaClosure (sprintf "instantiateDeclaring ≡ closure for '%s'" name)
            }

            test "method placeholders freshen to one shared TyVar per index at the given level" {
                // A closure baking method index 0 twice and index 1 once — exactly
                // the shape `MetadataMapping.tryBuildType` produces for a generic
                // method (`Dictionary.TryGetValue<...>`-style). Routed through the
                // production realiser `instantiateSignature`, which must freshen one
                // var per method index, shared across `Parameters`/`Return`, stamped
                // at `level`.
                let closure (a: SemType[]) : SemType =
                    TyFun(
                        TyTuple(EqArray.ofList [ a.[0]; TempTypar(TyparAxis.Method, 0) ]),
                        TyTuple(EqArray.ofList [ TempTypar(TyparAxis.Method, 0); TempTypar(TyparAxis.Method, 1) ])
                    )

                let m: ExternalMember =
                    {
                        Name = "genericMethod"
                        IsStatic = true
                        IsProperty = false
                        Signature = ExternalSignature.ofClosure (false, 1, 2, closure)
                        MethodArity = 2
                        Origin = SymbolOrigin.Empty
                        Key = SymbolKeyOps.valueKeyOf None "genericMethod"
                    }

                let level = 7
                let result = ExternalSymbols.instantiateSignature m (argsForArity 1) level

                match result with
                | TyFun(TyTuple ins, TyTuple outs) ->
                    let declaring = ins.[0]
                    let m0a = ins.[1]
                    let m0b = outs.[0]
                    let m1 = outs.[1]

                    Expect.equal declaring (TyConst("int", EqArray.empty)) "declaring arg substituted"

                    let asTyVar label (t: SemType) =
                        match t with
                        | TyVar tv -> tv
                        | other -> failtestf "%s: expected a fresh TyVar, got %A" label other

                    let v0a = asTyVar "m0a" m0a
                    let v0b = asTyVar "m0b" m0b
                    let v1 = asTyVar "m1" m1

                    Expect.isTrue (System.Object.ReferenceEquals(v0a, v0b)) "same method index 0 → same TyVar"
                    Expect.isFalse (System.Object.ReferenceEquals(v0a, v1)) "distinct method index → distinct TyVar"
                    Expect.equal v0a.Level level "fresh method var stamped at level"
                    Expect.equal v1.Level level "fresh method var stamped at level"
                | other -> failtestf "unexpected instantiate result: %A" other
            }

            // --- Member path: ExternalSignature.ofClosure + instantiateSignature ---

            /// Build a member, round-trip its `BuildSignature` through the two-axis
            /// `ExternalSignature`, and assert `instantiateSignature` reproduces the
            /// closure on ground args. `declArity` is the declaring type's arity.
            let memberOracle name isProperty declArity methodArity (build: SemType[] -> SemType) =
                test name {
                    let m: ExternalMember =
                        {
                            Name = name
                            IsStatic = true
                            IsProperty = isProperty
                            Signature = ExternalSignature.ofClosure (isProperty, declArity, methodArity, build)
                            MethodArity = methodArity
                            Origin = SymbolOrigin.Empty
                            Key = SymbolKeyOps.valueKeyOf None name
                        }

                    let args = argsForArity declArity

                    if methodArity = 0 then
                        // No method axis ⇒ structurally comparable to the closure.
                        let viaSig = ExternalSymbols.instantiateSignature m args 0
                        Expect.equal viaSig (build args) "instantiateSignature ≡ BuildSignature"
                    else
                        // With a method axis the closure bakes `TempTypar(Method,_)`
                        // and `instantiateSignature` freshens it; assert the arities
                        // round-trip and the signature is a `TyFun` with the method
                        // var present (structural value-equality doesn't apply).
                        Expect.equal m.Signature.DeclaringArity declArity "declaring arity preserved"
                        Expect.equal m.Signature.MethodArity methodArity "method arity preserved"

                        match ExternalSymbols.instantiateSignature m args 0 with
                        | TyFun _ -> ()
                        | other -> failtestf "expected a TyFun member signature, got %A" other
                }

            testList
                "member signatures round-trip via ExternalSignature"
                [
                    // 0-param method: `unit -> ret` (curried/tupled coincide).
                    memberOracle
                        "nullary method"
                        false
                        0
                        0
                        (fun _ -> TyFun(TyConst("unit", EqArray.empty), TyConst("int", EqArray.empty)))

                    // 1-param method over a declaring typar: `'T0 -> bool`.
                    memberOracle
                        "unary method over declaring typar"
                        false
                        1
                        0
                        (fun a -> TyFun(a.[0], TyConst("bool", EqArray.empty)))

                    // N≥2 params: one tupled arg.
                    memberOracle
                        "binary method tupled"
                        false
                        2
                        0
                        (fun a -> TyFun(TyTuple(EqArray.ofList [ a.[0]; a.[1] ]), TyConst("unit", EqArray.empty)))

                    // Property: bare value type, no leading arrow.
                    memberOracle "property bare value" true 1 0 (fun a -> TyClass(kRec, EqArray.singleton a.[0]))

                    // Ctor-shaped: `(p1 * p2) -> declType`.
                    memberOracle
                        "ctor tupled to declaring type"
                        false
                        1
                        0
                        (fun a ->
                            TyFun(
                                TyTuple(EqArray.ofList [ a.[0]; TyConst("int", EqArray.empty) ]),
                                TyRecord(kRec, EqArray.singleton a.[0])
                            )
                        )

                    // Generic method: bakes a method typar.
                    memberOracle
                        "generic method bakes method typar"
                        false
                        1
                        1
                        (fun a ->
                            TyFun(
                                TempTypar(TyparAxis.Method, 0),
                                TyTuple(EqArray.ofList [ a.[0]; TempTypar(TyparAxis.Method, 0) ])
                            )
                        )
                ]
        ]
