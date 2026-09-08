module XParsec.FSharp.SemanticAnalysis.Tests.MeasuresTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// The `m`, `s` and `kg` declarations every fixture below references; `analyse` prepends it.
let private unitsPrelude =
    "[<Measure>] type m\n[<Measure>] type s\n[<Measure>] type kg\n"

// Returns the `PassContext` too: the carrier and measure live on the per-file
// `TypeStore`'s union-find state, not on the `TypeVar` node.
let private analyse (input: string) =
    let lexed, file = parseFile (unitsPrelude + input)
    Pipeline.analyseSemWithContextFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

/// The types of the file's `let`s in source order; the measure prelude's declarations are
/// not among them.
let private letTypes (tast: TastFile) : SemType list =
    [
        for d in tast.Decls do
            match d with
            | TDecl.Let(m, _, _) -> m.Ty
            | _ -> ()
    ]

let private declType (tast: TastFile) : SemType =
    match letTypes tast with
    | [ ty ] -> ty
    | other -> failwithf "expected a single let, got %A" other

let private hasMeasureMismatch (tast: TastFile) =
    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Measure mismatch")

/// Read the (carrier, measure) from a measure-bearing `TyVar`, via `UnionFind.find`
/// so a stale non-root pointer still reaches the authoritative Link/Units.
let private measuredOf (store: TypeStore) (ty: SemType) : SemType * MeasureTerm =
    match ty with
    | TyVar tv ->
        let root = UnionFind.find store tv

        let carrier =
            match store.Link root with
            | ValueSome c -> c
            | ValueNone -> failwithf "expected TyVar with Link, got %A" ty

        let units =
            match store.Units root with
            | ValueSome u -> u
            | ValueNone -> failwithf "expected TyVar with Units, got %A" ty

        carrier, units
    | _ -> failwithf "expected TyVar, got %A" ty

/// The prelude carries no `namespace` or `module` header, so each measure keys under the
/// global namespace at arity 0.
let private measure (parts: (string * int) list) : MeasureTerm =
    MeasureTerm.OfList
        [
            for (n, e) in parts -> MeasureAtom.Named(SymbolKeyOps.typeKeyOf "" n), Rational.ofInt e
        ]

[<Tests>]
let tests =
    testList
        "Measures"
        [
            test "measured literal carries its measure" {
                let ctx, tast = analyse "let x = 1.0<m>"
                let carrier, units = measuredOf ctx.Store (declType tast)
                Expect.equal carrier BuiltinTypes.tyFloat "carrier float"
                Expect.equal units (measure [ "m", 1 ]) "units = m"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "dimensionless and measured do not unify" {
                let _, tast = analyse "let r = 1.0 + 1.0<m>"
                Expect.isTrue (hasMeasureMismatch tast) "measure mismatch emitted"
            }

            test "same-measure addition works" {
                let ctx, tast = analyse "let r = 1.0<m> + 2.0<m>"
                let carrier, units = measuredOf ctx.Store (declType tast)
                Expect.equal carrier BuiltinTypes.tyFloat "carrier float"
                Expect.equal units (measure [ "m", 1 ]) "units = m"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "different-measure addition fails" {
                let _, tast = analyse "let r = 1.0<m> + 2.0<kg>"
                Expect.isTrue (hasMeasureMismatch tast) "measure mismatch on m vs kg"
            }

            test "type annotation with measure" {
                let ctx, tast = analyse "let f (x : float<m>) = x"

                match declType tast with
                | TyFun(arg, ret) ->
                    let argCarrier, argUnits = measuredOf ctx.Store arg
                    let retCarrier, retUnits = measuredOf ctx.Store ret
                    Expect.equal argCarrier BuiltinTypes.tyFloat "arg carrier float"
                    Expect.equal argUnits (measure [ "m", 1 ]) "arg units = m"
                    Expect.equal retCarrier BuiltinTypes.tyFloat "ret carrier float"
                    Expect.equal retUnits (measure [ "m", 1 ]) "ret units = m"
                | other -> failtestf "expected TyFun, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "measure normalisation: m s / s = m" {
                let ctx, tast = analyse "let r = 1.0<m s / s>"
                let _, units = measuredOf ctx.Store (declType tast)
                Expect.equal units (measure [ "m", 1 ]) "units cancel to m"
            }

            test "measure normalisation: m^2 / m = m" {
                let ctx, tast = analyse "let r = 1.0<m^2 / m>"
                let _, units = measuredOf ctx.Store (declType tast)
                Expect.equal units (measure [ "m", 1 ]) "exponent cancellation"
            }

            test "measured multiplication" {
                let ctx, tast = analyse "let r = 3.0<m> * 4.0<m>"
                let _, units = measuredOf ctx.Store (declType tast)
                Expect.equal units (measure [ "m", 2 ]) "m * m = m^2"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "measured division" {
                let ctx, tast = analyse "let v = 100.0<m> / 5.0<s>"
                let _, units = measuredOf ctx.Store (declType tast)
                Expect.equal units (measure [ "m", 1; "s", -1 ]) "m / s"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "speed example: d / t : float<m/s>" {
                // `speed` divides two annotated params, and the measure survives
                // through the application: `v : float<m/s>`.
                let ctx, tast =
                    analyse "let speed (d : float<m>) (t : float<s>) = d / t\nlet v = speed 100.0<m> 5.0<s>"

                match letTypes tast with
                | [ _; vTy ] ->
                    let carrier, units = measuredOf ctx.Store vTy
                    Expect.equal carrier BuiltinTypes.tyFloat "v carrier float"
                    Expect.equal units (measure [ "m", 1; "s", -1 ]) "v : float<m/s>"
                | other -> failwithf "expected two lets, got %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "Rational construction reduces to lowest terms" {
                let r = Rational.create (bigint 4, bigint 8)
                Expect.equal r.Numerator (bigint 1) "numerator reduced"
                Expect.equal r.Denominator (bigint 2) "denominator reduced"
            }

            test "Rational negative denominator is canonicalised" {
                let r = Rational.create (bigint 1, bigint -2)
                Expect.equal r.Numerator (bigint -1) "sign moves to numerator"
                Expect.equal r.Denominator (bigint 2) "denominator positive"
            }

            test "Rational equality and comparison agree on unreduced inputs" {
                let a = Rational.create (bigint 2, bigint 4)
                let b = Rational.create (bigint 1, bigint 2)
                Expect.equal a b "2/4 equals 1/2"
                Expect.equal (a.GetHashCode()) (b.GetHashCode()) "and hashes the same"
                Expect.equal (compare a b) 0 "and compares as 0"

                let c = Rational.create (bigint -6, bigint -8)
                Expect.equal c (Rational.create (bigint 3, bigint 4)) "-6/-8 equals 3/4"

                Expect.equal
                    (c.GetHashCode())
                    ((Rational.create (bigint 3, bigint 4)).GetHashCode())
                    "and hashes the same"

                Expect.equal (compare c (Rational.create (bigint 3, bigint 4))) 0 "and compares as 0"

                let third = Rational.create (bigint 2, bigint 6)
                Expect.isTrue (compare third b < 0) "1/3 sorts below 1/2"

                let asSet = Set.ofList [ a; b; third ]
                let asKeys = [ a, 1; b, 2; third, 3 ] |> Map.ofList
                Expect.equal asSet.Count 2 "a sorted collection collapses 2/4 with 1/2"
                Expect.equal asKeys.Count 2 "a keyed collection collapses 2/4 with 1/2"
            }

            // The only construction the private constructor cannot police, so the
            // representation puts `0/1` at the zero-initialised struct.
            test "the default Rational is canonical" {
                let d = Unchecked.defaultof<Rational>
                Expect.equal d.Numerator (bigint 0) "default numerator is 0"
                Expect.equal d.Denominator (bigint 1) "default denominator is 1, not 0"
                Expect.equal d Rational.Zero "the default equals Zero"
                Expect.equal (d.GetHashCode()) (Rational.Zero.GetHashCode()) "and hashes as Zero"

                Expect.isTrue
                    (Array.zeroCreate<Rational> 3 |> Array.forall (fun r -> r = Rational.Zero))
                    "an array of them starts canonical"
            }

            test "MeasureTerm equality is structural after normalisation" {
                let a = measure [ "m", 1; "s", -1 ]
                let b = measure [ "s", -1; "m", 1 ]
                Expect.equal a b "sort order doesn't matter for equality"
            }

            test "MeasureTerm.Empty is dimensionless" {
                Expect.isTrue MeasureTerm.Empty.IsDimensionless "empty is dimensionless"
                Expect.isTrue MeasureTerm.empty.IsDimensionless "module alias same"
            }

            test "MeasureTerm.mul combines exponents" {
                let mPerS = measure [ "m", 1; "s", -1 ]
                let s = measure [ "s", 1 ]
                let result = MeasureTerm.mul mPerS s
                Expect.equal result (measure [ "m", 1 ]) "m/s * s = m"
            }
        ]
