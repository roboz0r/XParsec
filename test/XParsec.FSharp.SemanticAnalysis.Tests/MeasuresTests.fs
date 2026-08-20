module XParsec.FSharp.SemanticAnalysis.Tests.MeasuresTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Returns the `PassContext` too: the carrier and measure live on the per-file
// `TypeStore`'s union-find state, not on the `TypeVar` node.
let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSemWithContext realProvider.Value (LexedFile.ofText lexed) file

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | EqList [ TDecl.Let(_, _, _, ty) ] -> ty
    | _ -> failwithf "expected single TDecl.Let, got %A" tast.Decls

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

let private measure (parts: (string * int) list) : MeasureTerm =
    MeasureTerm.ofList [ for (n, e) in parts -> n, Rational.ofInt e ]

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

                match tast.Decls with
                | EqList [ _; TDecl.Let(_, _, _, vTy) ] ->
                    let carrier, units = measuredOf ctx.Store vTy
                    Expect.equal carrier BuiltinTypes.tyFloat "v carrier float"
                    Expect.equal units (measure [ "m", 1; "s", -1 ]) "v : float<m/s>"
                | other -> failwithf "expected two decls, got %A" other

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
