module XParsec.FSharp.SemanticAnalysis.Tests.ExternalSignatureOracleTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The external-signature realiser oracle. The two-headed window is closed: every
// external descriptor now carries ONLY its `FrozenType` template (the
// `SemType[] -> SemType` closures that producers once derived templates from are
// gone). What remains load-bearing is that the
// production realisers correctly turn a template back into the `SemType` a use
// site unifies against:
//
//   * `instantiateDeclaring` — a declaring-only type-shape descriptor (record
//     field, union-case field, abbreviation body): `FTTypar(Declaring,i) →
//     declaringArgs.[i]`.
//   * `instantiateSignature` — a member's two-axis signature: declaring
//     placeholders resolve to the caller's args, method placeholders freshen to
//     fresh `TyVar`s (shared per method index, stamped at the requested level).
//
// Each case below is a HAND-WRITTEN template paired with the `SemType` it must
// realise to on `groundArgs` — no closure derives the expected value, so the test
// pins the realisers directly. (The end-to-end `translateType` path — CST to
// template — is covered by the package-corpus tests in `VesperLibTests`.)

/// Ground (`TyVar`-free, `TyTypar`-free) types to substitute for declaring
/// args, so `instantiate*` produces structurally-comparable `SemType`s (no
/// reference-identity `TyVar` leaves to defeat `=`).
let private groundArgs: SemType[] =
    [|
        TyConst("int", EqArray.empty)
        TyConst("string", EqArray.empty)
        TyClass(SymbolKeyOps.qualifiedTypeKey "Test.Widget" 0, EqArray.empty)
    |]

let private kRec = SymbolKeyOps.qualifiedTypeKey "Test.Box" 1
let private kUnion = SymbolKeyOps.qualifiedTypeKey "Test.Option" 1

/// A declaring-typar `i` as a template leaf, and the ground `SemType` it must
/// realise to (`groundArgs.[i]`). Pairing them keeps each oracle case honest:
/// the template names `FTTypar(Declaring,i)`, the expected names `groundArgs.[i]`
/// by hand.
let private d (i: int) : FrozenType = FTTypar(TyparAxis.Declaring, i)

/// A representative template paired with its declaring arity and the `SemType`
/// `instantiateDeclaring` must yield on `groundArgs`. None bake a method typar;
/// the method axis is exercised by the member tests below.
let private declaringTemplates: (string * int * FrozenType * SemType) list =
    [
        "argless const", 0, FTConst("bool", EqArray.empty), TyConst("bool", EqArray.empty)
        "identity typar", 1, d 0, groundArgs.[0]
        "array of typar", 1, FTConst("[]", EqArray.singleton (d 0)), TyConst("[]", EqArray.singleton groundArgs.[0])
        "curried fun over two typars",
        2,
        FTFun(d 0, FTFun(d 1, FTConst("unit", EqArray.empty))),
        TyFun(groundArgs.[0], TyFun(groundArgs.[1], TyConst("unit", EqArray.empty)))
        "tupled fun",
        2,
        FTFun(FTTuple(EqArray.ofList [ d 0; d 1 ]), d 0),
        TyFun(TyTuple(EqArray.ofList [ groundArgs.[0]; groundArgs.[1] ]), groundArgs.[0])
        "record of typar", 1, FTRecord(kRec, EqArray.singleton (d 0)), TyRecord(kRec, EqArray.singleton groundArgs.[0])
        "union of typar", 1, FTUnion(kUnion, EqArray.singleton (d 0)), TyUnion(kUnion, EqArray.singleton groundArgs.[0])
        "nested generic intrinsic",
        1,
        FTConst("[]", EqArray.singleton (FTUnion(kUnion, EqArray.singleton (d 0)))),
        TyConst("[]", EqArray.singleton (TyUnion(kUnion, EqArray.singleton groundArgs.[0])))
        "unknown head", 0, FTUnknown "Unresolved.Head", TyUnknown "Unresolved.Head"
    ]

/// Slice `groundArgs` to the template's arity (the declaring substitution the
/// caller mints fresh at a use site — here, ground stand-ins).
let private argsForArity (arity: int) : SemType[] = Array.sub groundArgs 0 arity

[<Tests>]
let tests =
    testList
        "ExternalSignature realiser oracle"
        [
            test "instantiateDeclaring template args ≡ hand-written expected (no method axis)" {
                for name, arity, template, expected in declaringTemplates do
                    let args = argsForArity arity
                    let viaTemplate = instantiateDeclaring template args
                    Expect.equal viaTemplate expected (sprintf "instantiateDeclaring for '%s'" name)
            }

            test "substituteDeclaring is the frozen sibling of instantiateDeclaring" {
                // Codegen substitutes declaring args in frozen-space directly; it
                // must agree with `toFrozen ∘ instantiateDeclaring` (the path
                // inference takes) on the post-freeze subset.
                for name, arity, template, _ in declaringTemplates do
                    let frozenArgs = argsForArity arity |> Array.map toFrozen
                    let viaSubstitute = substituteDeclaring frozenArgs template
                    let viaInstantiate = toFrozen (instantiateDeclaring template (argsForArity arity))

                    Expect.equal
                        viaSubstitute
                        viaInstantiate
                        (sprintf "substituteDeclaring ≡ instantiateDeclaring for '%s'" name)
            }

            test "method placeholders freshen to one shared TyVar per index at the given level" {
                // A two-axis signature baking method index 0 twice and index 1 once
                // — exactly the shape `MetadataMapping.tryBuildType` produces for a
                // generic method (`Dictionary.TryGetValue<...>`-style). Routed
                // through the production realiser `instantiateSignature`, which must
                // freshen one var per method index, shared across
                // `Parameters`/`Return`, stamped at `level`.
                let signature: ExternalSignature =
                    {
                        DeclaringArity = 1
                        MethodArity = 2
                        Parameters = FTTuple(EqArray.ofList [ d 0; FTTypar(TyparAxis.Method, 0) ])
                        Return = FTTuple(EqArray.ofList [ FTTypar(TyparAxis.Method, 0); FTTypar(TyparAxis.Method, 1) ])
                        MethodTyparBounds = [||]
                    }

                let m: ExternalMember =
                    {
                        Name = "genericMethod"
                        IsStatic = true
                        Storage = MemberStorage.Method
                        Signature = signature
                        MethodArity = 2
                        Origin = SymbolOrigin.Empty
                        Key = SymbolKeyOps.valueKeyOf None "genericMethod"
                        OptionalDefaults = []
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

            // --- Member path: hand-written ExternalSignature + instantiateSignature ---

            /// Build a member from a hand-written two-axis `ExternalSignature` and
            /// assert `instantiateSignature` realises it to `expected` on ground
            /// args. `declArity` is the declaring type's arity; `expected` is
            /// `None` for a generic member (method axis freshens to `TyVar`s, which
            /// have no structural counterpart — only the arities + `TyFun` shape are
            /// asserted there).
            let memberOracle
                name
                isProperty
                declArity
                methodArity
                (signature: ExternalSignature)
                (expected: SemType option)
                =
                test name {
                    let m: ExternalMember =
                        {
                            Name = name
                            IsStatic = true
                            Storage =
                                if isProperty then
                                    MemberStorage.Property
                                else
                                    MemberStorage.Method
                            Signature = signature
                            MethodArity = methodArity
                            Origin = SymbolOrigin.Empty
                            Key = SymbolKeyOps.valueKeyOf None name
                            OptionalDefaults = []
                        }

                    let args = argsForArity declArity

                    match expected with
                    | Some exp ->
                        Expect.equal
                            (ExternalSymbols.instantiateSignature m args 0)
                            exp
                            "instantiateSignature ≡ expected"
                    | None ->
                        // With a method axis the realiser freshens `TyVar`s;
                        // structural value-equality doesn't apply, so assert the
                        // arities round-trip and the signature is a `TyFun`.
                        Expect.equal m.Signature.DeclaringArity declArity "declaring arity preserved"
                        Expect.equal m.Signature.MethodArity methodArity "method arity preserved"

                        match ExternalSymbols.instantiateSignature m args 0 with
                        | TyFun _ -> ()
                        | other -> failtestf "expected a TyFun member signature, got %A" other
                }

            testList
                "member signatures realise via instantiateSignature"
                [
                    // 0-param method: `unit -> ret`.
                    memberOracle
                        "nullary method"
                        false
                        0
                        0
                        {
                            DeclaringArity = 0
                            MethodArity = 0
                            Parameters = FTConst("unit", EqArray.empty)
                            Return = FTConst("int", EqArray.empty)
                            MethodTyparBounds = [||]
                        }
                        (Some(TyFun(TyConst("unit", EqArray.empty), TyConst("int", EqArray.empty))))

                    // 1-param method over a declaring typar: `'T0 -> bool`.
                    memberOracle
                        "unary method over declaring typar"
                        false
                        1
                        0
                        {
                            DeclaringArity = 1
                            MethodArity = 0
                            Parameters = d 0
                            Return = FTConst("bool", EqArray.empty)
                            MethodTyparBounds = [||]
                        }
                        (Some(TyFun(groundArgs.[0], TyConst("bool", EqArray.empty))))

                    // N≥2 params: one tupled arg.
                    memberOracle
                        "binary method tupled"
                        false
                        2
                        0
                        {
                            DeclaringArity = 2
                            MethodArity = 0
                            Parameters = FTTuple(EqArray.ofList [ d 0; d 1 ])
                            Return = FTConst("unit", EqArray.empty)
                            MethodTyparBounds = [||]
                        }
                        (Some(
                            TyFun(
                                TyTuple(EqArray.ofList [ groundArgs.[0]; groundArgs.[1] ]),
                                TyConst("unit", EqArray.empty)
                            )
                        ))

                    // Property: bare value type, no parameters / leading arrow.
                    memberOracle
                        "property bare value"
                        true
                        1
                        0
                        {
                            DeclaringArity = 1
                            MethodArity = 0
                            Parameters = FTConst("unit", EqArray.empty)
                            Return = FTClass(kRec, EqArray.singleton (d 0))
                            MethodTyparBounds = [||]
                        }
                        (Some(TyClass(kRec, EqArray.singleton groundArgs.[0])))

                    // Ctor-shaped: `(p1 * p2) -> declType`.
                    memberOracle
                        "ctor tupled to declaring type"
                        false
                        1
                        0
                        {
                            DeclaringArity = 1
                            MethodArity = 0
                            Parameters = FTTuple(EqArray.ofList [ d 0; FTConst("int", EqArray.empty) ])
                            Return = FTRecord(kRec, EqArray.singleton (d 0))
                            MethodTyparBounds = [||]
                        }
                        (Some(
                            TyFun(
                                TyTuple(EqArray.ofList [ groundArgs.[0]; TyConst("int", EqArray.empty) ]),
                                TyRecord(kRec, EqArray.singleton groundArgs.[0])
                            )
                        ))

                    // Generic method: bakes a method typar (no structural expected).
                    memberOracle
                        "generic method bakes method typar"
                        false
                        1
                        1
                        {
                            DeclaringArity = 1
                            MethodArity = 1
                            Parameters = FTTypar(TyparAxis.Method, 0)
                            Return = FTTuple(EqArray.ofList [ d 0; FTTypar(TyparAxis.Method, 0) ])
                            MethodTyparBounds = [||]
                        }
                        None
                ]
        ]
