module XParsec.FSharp.SemanticAnalysis.Tests.SecondaryCtorParamTypesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

let private diagnostics (tast: TastFile) =
    tast.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq

/// The declared types of a class's single secondary ctor, in parameter order.
let private secondaryCtorParamTypes (tast: TastFile) : SemType list =
    match diagnostics tast with
    | [] -> ()
    | diags -> failtestf "expected clean analysis, got diagnostics: %A" diags

    let typeDecl =
        tast.Decls
        |> EqArray.toList
        |> List.tryPick (
            function
            | TDecl.Type t -> Some t
            | _ -> None
        )
        |> Option.defaultWith (fun () -> failwithf "expected a TDecl.Type, got %A" tast.Decls)

    match typeDecl.Kind with
    | TTypeKind.Class c ->
        match EqArray.toList c.SecondaryCtors with
        | [ sc ] -> sc.Params |> EqArray.toList |> List.map snd
        | other -> failwithf "expected exactly one secondary ctor, got %d" other.Length
    | other -> failtestf "expected TTypeKind.Class, got %A" other

/// Asked of the VERDICT, not of a message substring: the walker's restriction is that the
/// shape is not implemented, and that is what the diagnostic classifies itself as.
let private isCtorArgShapeError (tast: TastFile) =
    tast.Diagnostics
    |> Seq.exists (fun d ->
        match d.Kind with
        | Kind.NotYetSupported feature -> feature.Contains "constructor argument pattern"
        | _ -> false
    )

// Primary and secondary ctors share ONE parameter walker
// (`MemberRegistration.ctorParamsOfPat`), which collects and resolves each parameter in
// order: every parameter-shaped arm must add exactly one entry, or a later annotation lands
// on an earlier param. That walker only handles a bare identifier with an optional
// annotation — which is sound ONLY because ctor args are restricted to exactly that shape.
// These tests pin both halves of that contract: the shapes the walker handles, and the
// restriction that keeps richer shapes away from it.
[<Tests>]
let tests =
    testList
        "SecondaryCtorParamTypes"
        [
            test "annotated secondary-ctor params link to their own param, in order" {
                let tast =
                    analyse "type C(x: int) =\n    new(a: int, b: string) = C(a)\n    member this.X = x"

                Expect.equal
                    (secondaryCtorParamTypes tast)
                    [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]
                    "a : int, b : string"
            }

            // An attributed or parenthesised ctor arg is REJECTED, for both ctor kinds.
            // This is what makes the walker's narrow set of arms complete rather than
            // lossy: were these shapes ever admitted, they would reach the walker, match
            // no arm, and silently fail to advance the cursor. Lift the restriction and
            // the walker must be taught the new shapes in the same commit.
            test "attributed param is rejected on a primary ctor" {
                let tast = analyse "type C([<System.Obsolete>] x: int) =\n    member this.X = x"
                Expect.isTrue (isCtorArgShapeError tast) "attributed primary-ctor arg is rejected"
            }

            test "attributed param is rejected on a secondary ctor" {
                let tast =
                    analyse "type C(x: int) =\n    new([<System.Obsolete>] a: int) = C(a)\n    member this.X = x"

                Expect.isTrue (isCtorArgShapeError tast) "attributed secondary-ctor arg is rejected"
            }

            test "parenthesised annotated param is rejected on a secondary ctor" {
                let tast =
                    analyse "type C(x: int) =\n    new((a): int, b: string) = C(a)\n    member this.X = x"

                Expect.isTrue (isCtorArgShapeError tast) "non-simple secondary-ctor arg pattern is rejected"
            }
        ]
