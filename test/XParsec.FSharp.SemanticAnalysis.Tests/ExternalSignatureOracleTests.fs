module XParsec.FSharp.SemanticAnalysis.Tests.ExternalSignatureOracleTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis

// Each case is a HAND-WRITTEN `FrozenType` template paired with the `SemType` it must
// instantiate to on `groundArgs`. `instantiateDeclaring` maps `FTTypar(Type _, i)` to
// `declaringArgs.[i]`; `instantiateSignature` also freshens the member's own typars to
// `TyVar`s.

/// Ground (`TyVar`-free, `TyTypar`-free) types for the declaring args, so `instantiate*`
/// produces structurally-comparable `SemType`s — a `TyVar` would defeat `=`.
let private groundArgs: SemType[] =
    [|
        TyConst(RuntimeNames.intKey, Block.empty)
        TyConst(RuntimeNames.stringKey, Block.empty)
        TyClass(SymbolKeyOps.qualifiedTypeKeyOf "Test.Widget" 0, Block.empty)
    |]

let private kRec = SymbolKeyOps.qualifiedTypeKeyOf "Test.Box" 1
let private kUnion = SymbolKeyOps.qualifiedTypeKeyOf "Test.Option" 1

/// The scope the templates' typars are written under.
let private oracleKey = SymbolKeyOps.qualifiedTypeKeyOf "Oracle.Decl" 1

/// The declaring-typar template `FTTypar(Type _, i)`, which must instantiate to
/// `groundArgs.[i]`.
let private d (i: int) : FrozenType =
    FTTypar(TyparScope.Type oracleKey, TyparIndex.typeSlot i)

/// A member's own typar `i`.
let private m (i: int) : FrozenType =
    FTTypar(TyparScope.Member oracleKey, TyparIndex.typeSlot i)

/// Name, declaring arity, template, and the `SemType` `instantiateDeclaring` must yield
/// on `groundArgs`. None bake a method typar.
let private declaringTemplates: (string * int * FrozenType * SemType) list =
    [
        "argless const", 0, FTConst(RuntimeNames.boolKey, Block.empty), TyConst(RuntimeNames.boolKey, Block.empty)
        "identity typar", 1, d 0, groundArgs.[0]
        "array of typar",
        1,
        FTConst(RuntimeNames.arrayKey 1, Block.singleton (d 0)),
        TyConst(RuntimeNames.arrayKey 1, Block.singleton groundArgs.[0])
        "curried fun over two typars",
        2,
        FTFun(d 0, FTFun(d 1, FTConst(RuntimeNames.unitKey, Block.empty))),
        TyFun(groundArgs.[0], TyFun(groundArgs.[1], TyConst(RuntimeNames.unitKey, Block.empty)))
        "tupled fun",
        2,
        FTFun(FTTuple(Block.ofList [ d 0; d 1 ]), d 0),
        TyFun(TyTuple(Block.ofList [ groundArgs.[0]; groundArgs.[1] ]), groundArgs.[0])
        "record of typar", 1, FTRecord(kRec, Block.singleton (d 0)), TyRecord(kRec, Block.singleton groundArgs.[0])
        "union of typar", 1, FTUnion(kUnion, Block.singleton (d 0)), TyUnion(kUnion, Block.singleton groundArgs.[0])
        "nested generic intrinsic",
        1,
        FTConst(RuntimeNames.arrayKey 1, Block.singleton (FTUnion(kUnion, Block.singleton (d 0)))),
        TyConst(RuntimeNames.arrayKey 1, Block.singleton (TyUnion(kUnion, Block.singleton groundArgs.[0])))
        "unknown head",
        0,
        FTUnknown(UnknownReason.UndefinedName "Unresolved.Head"),
        TyUnknown(UnknownReason.UndefinedName "Unresolved.Head")
    ]

/// Slice `groundArgs` to the template's arity — the declaring substitution a use site
/// would mint fresh.
let private argsForArity (arity: int) : SemType[] = Array.sub groundArgs 0 arity

let private ftInt: FrozenType = FTConst(RuntimeNames.intKey, Block.empty)
let private ftUnit: FrozenType = FTConst(RuntimeNames.unitKey, Block.empty)

/// A stand-in for one applied argument, carrying only what the group open reads: whether it
/// is a literal tuple, and if so its elements.
type private Arg =
    | Atom of string
    | Tup of Arg list

let private asTuple (a: Arg) : Arg list voption =
    match a with
    | Tup elems -> ValueSome elems
    | Atom _ -> ValueNone

let private openGroups (widths: int list) (args: Arg list) =
    SymbolKeyOps.openArgGroups asTuple (Block.ofList widths) args

let private opened (flat: Arg list) (residual: Arg list) : SymbolKeyOps.OpenedArgGroups<Arg> voption =
    ValueSome { Flat = flat; Residual = residual }

[<Tests>]
let tests =
    testList
        "ExternalSignature instantiation oracle"
        [
            test "instantiateDeclaring template args ≡ hand-written expected (no method scope)" {
                for name, arity, template, expected in declaringTemplates do
                    let args = argsForArity arity

                    let viaTemplate =
                        instantiateDeclaring (MeasuredThaw.noneOver (TypeStore())) template args

                    Expect.equal viaTemplate expected (sprintf "instantiateDeclaring for '%s'" name)
            }

            test "substituteDeclaring is the frozen sibling of instantiateDeclaring" {
                // Codegen substitutes declaring args in frozen-space directly; it must agree
                // with `toFrozen ∘ instantiateDeclaring`, the path inference takes.
                for name, arity, template, _ in declaringTemplates do
                    let frozenArgs = argsForArity arity |> Array.map toFrozen |> Block.ofArray
                    let viaSubstitute = substituteDeclaring frozenArgs template

                    let viaInstantiate =
                        toFrozen (
                            instantiateDeclaring (MeasuredThaw.noneOver (TypeStore())) template (argsForArity arity)
                        )

                    Expect.equal
                        viaSubstitute
                        viaInstantiate
                        (sprintf "substituteDeclaring ≡ instantiateDeclaring for '%s'" name)
            }

            test "method placeholders freshen to one shared TyVar per index at the given level" {
                // Method index 0 baked twice and index 1 once — the shape a generic method
                // (`Dictionary.TryGetValue<…>`-style) produces. The var for index 0 must be
                // shared across `Parameters` and `Return`.
                let signature: ExternalSignature =
                    TestHelpers.mkSignature
                        1<_>
                        2<_>
                        (FTTuple(Block.ofList [ d 0; m 0 ]))
                        (FTTuple(Block.ofList [ m 0; m 1 ]))

                let m: ExternalMember =
                    { TestHelpers.mkMember "genericMethod" with
                        Signature = signature
                    }

                let level = 7
                let store = TypeStore()

                let result =
                    ExternalSymbols.instantiateSignature (MeasuredThaw.noneOver store) m (argsForArity 1) level

                match result with
                | TyFun(TyTuple ins, TyTuple outs) ->
                    let declaring = ins.[0]
                    let m0a = ins.[1]
                    let m0b = outs.[0]
                    let m1 = outs.[1]

                    Expect.equal declaring (TyConst(RuntimeNames.intKey, Block.empty)) "declaring arg substituted"

                    let asTyVar label (t: SemType) =
                        match t with
                        | TyVar tv -> tv
                        | other -> failtestf "%s: expected a fresh TyVar, got %A" label other

                    let v0a = asTyVar "m0a" m0a
                    let v0b = asTyVar "m0b" m0b
                    let v1 = asTyVar "m1" m1

                    Expect.isTrue (v0a = v0b) "same method index 0 → same TyVar"
                    Expect.isFalse (v0a = v1) "distinct method index → distinct TyVar"
                    Expect.equal (store.Level(UnionFind.find store v0a)) level "fresh method var stamped at level"
                    Expect.equal (store.Level(UnionFind.find store v1)) level "fresh method var stamped at level"
                | other -> failtestf "unexpected instantiate result: %A" other
            }

            // --- Member path: hand-written ExternalSignature + instantiateSignature ---

            /// Assert `instantiateSignature` instantiates `signature` to `expected` on ground
            /// args. `expected` is `None` for a generic member: the method scope freshens to
            /// `TyVar`s, so only the arities and the `TyFun` shape are asserted.
            let memberOracle
                name
                isProperty
                (declTyparArity: int<typeSlot>)
                (methodTyparArity: int<typeSlot>)
                (signature: ExternalSignature)
                (expected: SemType option)
                =
                test name {
                    let m: ExternalMember =
                        { TestHelpers.mkMember name with
                            Storage =
                                if isProperty then
                                    MemberStorage.Property
                                else
                                    MemberStorage.Method
                            Signature = signature
                        }

                    let args = argsForArity (int declTyparArity)

                    match expected with
                    | Some exp ->
                        Expect.equal
                            (ExternalSymbols.instantiateSignature (MeasuredThaw.noneOver (TypeStore())) m args 0)
                            exp
                            "instantiateSignature ≡ expected"
                    | None ->
                        Expect.equal m.Signature.DeclaringTyparArity declTyparArity "declaring arity preserved"
                        Expect.equal m.Signature.MethodTyparArity methodTyparArity "method arity preserved"

                        match ExternalSymbols.instantiateSignature (MeasuredThaw.noneOver (TypeStore())) m args 0 with
                        | TyFun _ -> ()
                        | other -> failtestf "expected a TyFun member signature, got %A" other
                }

            testList
                "member signatures instantiate via instantiateSignature"
                [
                    // 0-param method: `unit -> ret`.
                    memberOracle
                        "nullary method"
                        false
                        0<_>
                        0<_>
                        (TestHelpers.mkSignature
                            0<_>
                            0<_>
                            (FTConst(RuntimeNames.unitKey, Block.empty))
                            (FTConst(RuntimeNames.intKey, Block.empty)))
                        (Some(
                            TyFun(TyConst(RuntimeNames.unitKey, Block.empty), TyConst(RuntimeNames.intKey, Block.empty))
                        ))

                    // 1-param method over a declaring typar: `'T0 -> bool`.
                    memberOracle
                        "unary method over declaring typar"
                        false
                        1<_>
                        0<_>
                        (TestHelpers.mkSignature 1<_> 0<_> (d 0) (FTConst(RuntimeNames.boolKey, Block.empty)))
                        (Some(TyFun(groundArgs.[0], TyConst(RuntimeNames.boolKey, Block.empty))))

                    // N≥2 params: one tupled arg.
                    memberOracle
                        "binary method tupled"
                        false
                        2<_>
                        0<_>
                        (TestHelpers.mkSignature
                            2<_>
                            0<_>
                            (FTTuple(Block.ofList [ d 0; d 1 ]))
                            (FTConst(RuntimeNames.unitKey, Block.empty)))
                        (Some(
                            TyFun(
                                TyTuple(Block.ofList [ groundArgs.[0]; groundArgs.[1] ]),
                                TyConst(RuntimeNames.unitKey, Block.empty)
                            )
                        ))

                    // Property: bare value type, no parameters / leading `->`. It is the EMPTY
                    // argument-group list that drops the `->`, not the `Storage` flag.
                    memberOracle
                        "property bare value"
                        true
                        1<_>
                        0<_>
                        (ExternalSignature.value (1<_>, 0<_>, FTClass(kRec, Block.singleton (d 0))))
                        (Some(TyClass(kRec, Block.singleton groundArgs.[0])))

                    // Ctor-shaped: `(p1 * p2) -> declType`.
                    memberOracle
                        "ctor tupled to declaring type"
                        false
                        1<_>
                        0<_>
                        (TestHelpers.mkSignature
                            1<_>
                            0<_>
                            (FTTuple(Block.ofList [ d 0; FTConst(RuntimeNames.intKey, Block.empty) ]))
                            (FTRecord(kRec, Block.singleton (d 0))))
                        (Some(
                            TyFun(
                                TyTuple(Block.ofList [ groundArgs.[0]; TyConst(RuntimeNames.intKey, Block.empty) ]),
                                TyRecord(kRec, Block.singleton groundArgs.[0])
                            )
                        ))

                    // Generic method: bakes a method typar (no structural expected).
                    memberOracle
                        "generic method bakes method typar"
                        false
                        1<_>
                        1<_>
                        (TestHelpers.mkSignature 1<_> 1<_> (m 0) (FTTuple(Block.ofList [ d 0; m 0 ])))
                        None
                ]

            // The signature's SOURCE argument groups are the only thing that says how a use
            // site applies a member: the key interns the flat parameter vector, in which the
            // tupled and curried spellings below are indistinguishable.
            testList
                "argument groups drive how a member is applied"
                [
                    test "a group's width is its own tupled domain, not the member's flat arity" {
                        let tupled =
                            TestHelpers.mkSignature 0<_> 0<_> (FTTuple(Block.ofList [ ftInt; ftInt ])) ftInt

                        let curried = ExternalSignature.ofGroups (0<_>, 0<_>, [ ftInt; ftInt ], ftInt)

                        Expect.equal
                            (ExternalSignature.argGroupWidths tupled)
                            (Block.ofList [ 2 ])
                            "`M(a, b)` is one 2-wide group"

                        Expect.equal
                            (ExternalSignature.argGroupWidths curried)
                            (Block.ofList [ 1; 1 ])
                            "`M a b` is two 1-wide groups"

                        Expect.equal
                            (ExternalSignature.argSigOf tupled)
                            (ExternalSignature.argSigOf curried)
                            "and both flatten to the same parameter vector, which is why the key cannot tell them apart"

                        Expect.equal
                            (ExternalSignature.argGroupWidths (TestHelpers.mkSignature 0<_> 0<_> ftUnit ftInt))
                            (Block.ofList [ 0 ])
                            "`M: unit -> r` is one 0-wide group: an argument that erases"

                        Expect.equal
                            (ExternalSignature.argGroupWidths (ExternalSignature.value (0<_>, 0<_>, ftInt)))
                            Block.empty
                            "and a value member has no group at all"
                    }

                    test "each group consumes ONE argument, opened to its own width" {
                        let a, b, c = Atom "a", Atom "b", Atom "c"

                        Expect.equal
                            (openGroups [ 2 ] [ Tup [ a; b ] ])
                            (opened [ a; b ] [])
                            "a 2-wide group opens its one literal tuple"

                        Expect.equal
                            (openGroups [ 1; 1 ] [ a; b ])
                            (opened [ a; b ] [])
                            "two 1-wide groups take their arguments as they stand"

                        Expect.equal
                            (openGroups [ 0 ] [ a ])
                            (opened [] [])
                            "a 0-wide group consumes its `()` and contributes nothing"

                        Expect.equal
                            (openGroups [ 1 ] [ a; b; c ])
                            (opened [ a ] [ b; c ])
                            "arguments past the groups are residual, applied to the member's RESULT"
                    }

                    // Each of these is a miscompile if group boundaries are guessed from the
                    // arguments' shapes against a flat arity instead of read off the widths.
                    test "a group takes its argument WHOLE at width 1, tuple-shaped or not" {
                        let a, b, c = Atom "a", Atom "b", Atom "c"

                        // `member Pair: 'a -> 'b -> r` applied `Pair (a, b) c`: `'a` is ONE
                        // parameter that happens to be given a tuple, and `c` fills `'b`.
                        Expect.equal
                            (openGroups [ 1; 1 ] [ Tup [ a; b ]; c ])
                            (opened [ Tup [ a; b ]; c ] [])
                            "a tuple argument in a 1-wide group stays one position"

                        // `member Run: unit -> int -> r` applied `Run () a`: the flat arity is
                        // 1, but it is the SECOND group that holds the parameter.
                        Expect.equal
                            (openGroups [ 0; 1 ] [ Atom "()"; a ])
                            (opened [ a ] [])
                            "a leading `()` group does not swallow the next group's argument"

                        // `member Go: unit -> unit -> r` applied `Go () ()`: no parameters, but
                        // still TWO applications to consume.
                        Expect.equal
                            (openGroups [ 0; 0 ] [ Atom "()"; Atom "()" ])
                            (opened [] [])
                            "both `()` groups are consumed"
                    }

                    test "a group that cannot be opened is `ValueNone`, never a partial open" {
                        let a, b, c = Atom "a", Atom "b", Atom "c"

                        Expect.equal
                            (openGroups [ 1; 1 ] [ a ])
                            ValueNone
                            "under-application: a group with no argument to consume"

                        Expect.equal
                            (openGroups [ 2 ] [ a ])
                            ValueNone
                            "a 2-wide group whose argument is not a literal tuple"

                        Expect.equal
                            (openGroups [ 2 ] [ Tup [ a; b; c ] ])
                            ValueNone
                            "a literal tuple of the wrong width"
                    }
                ]
        ]
