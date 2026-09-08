module XParsec.FSharp.Codegen.Clr.Tests.MeasureErasureTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

let private claim = SymbolKeyOps.typeKeyOf "Test" "Claim`2"
let private carrier = SymbolKeyOps.typeKeyOf "Test" "Carrier`1"
let private payload = FTConst(SymbolKeyOps.typeKeyOf "Test" "Payload", Block.empty)

/// `type Claim<[<Measure>] 'u, 'a> = Carrier<'a>`.
let private abbrev =
    ExternalTypeShape.Abbrev
        {
            Typars = TyparList.ofSeq [ "'u", TyparKind.Measure; "'a", TyparKind.Type ]
            Body = FTConst(carrier, Block.singleton (FTTypar(TyparScope.Type claim, 0<typeSlot>)))
        }

let private lookup (key: TypeKey) =
    if key = claim then ValueSome abbrev else ValueNone

/// The measure argument a measure-generic declaration writes as `<'u>`.
let private typarUnits =
    let owner = SymbolKeyOps.typeKeyOf "Test" "Owner`1"
    MeasureTerm.OfList [ MeasureAtom.Typar(TyparScope.Type owner, 0<measureSlot>), Rational.ofInt 1 ]

[<Tests>]
let tests =
    testList
        "MeasureErasure"
        [
            test "expanding a claim drops the measure argument" {
                let applied = FTConst(claim, Block.ofList [ FTMeasure typarUnits; payload ])

                Expect.equal
                    (MeasureErasure.erase lookup applied)
                    (FTConst(carrier, Block.singleton payload))
                    "the type-slot argument fills the body's typar"
            }
        ]
