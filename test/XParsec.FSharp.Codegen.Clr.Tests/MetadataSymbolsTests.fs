module XParsec.FSharp.Codegen.Clr.Tests.MetadataSymbolsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

// P2 gate (symbol-resolution-plan §6 / phase table): the MetadataLoadContext-backed
// referenced-assembly provider resolves a BCL generic type to a `Class` shape and
// its members to target-agnostic `SemType` signatures (the §6.1 mapping). This is
// the substrate the `hash` milestone (M) needs: it types
// `EqualityComparer<'T>.Default.GetHashCode`.

let private provider = MetadataSymbols.provider

let private eqComparer = "System.Collections.Generic.EqualityComparer`1"

[<Tests>]
let tests =
    testList
        "MetadataSymbols"
        [
            test "EqualityComparer`1 resolves as a non-interface Class with an origin" {
                match provider.TryLookupType eqComparer with
                | ValueSome(ExternalTypeShape.Class(arity, isInterface, origin)) ->
                    Expect.equal arity 1 "one declared typar"
                    Expect.isFalse isInterface "a class, not an interface"
                    Expect.equal origin.Namespace "System.Collections.Generic" "origin namespace"
                    Expect.isTrue origin.Assembly.IsSome "origin carries the defining assembly"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            test "a generic interface resolves with isInterface = true" {
                match provider.TryLookupType "System.Collections.Generic.IEnumerable`1" with
                | ValueSome(ExternalTypeShape.Class(arity, isInterface, _)) ->
                    Expect.equal arity 1 "one typar"
                    Expect.isTrue isInterface "IEnumerable`1 is an interface"
                | other -> failtestf "expected an interface Class shape, got %A" other
            }

            test "a non-generic type resolves with arity 0" {
                match provider.TryLookupType "System.Object" with
                | ValueSome(ExternalTypeShape.Class(arity, isInterface, _)) ->
                    Expect.equal arity 0 "System.Object is non-generic"
                    Expect.isFalse isInterface "System.Object is a class"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            test "Default resolves as a static property typed EqualityComparer<'T>" {
                match provider.TryLookupMember(eqComparer, "Default") with
                | ValueSome m ->
                    Expect.isTrue m.IsStatic "Default is static"
                    Expect.isTrue m.IsProperty "Default is a property"
                    Expect.equal m.Origin.DeclaringType (Some eqComparer) "member origin names the declaring type"

                    // Instantiated at `'T = int`, the property type is
                    // `EqualityComparer<int>` (the §7.3 per-use substitution).
                    match m.BuildSignature [| TyConst "int" |] with
                    | TyClass(name, [ TyConst "int" ]) -> Expect.equal name eqComparer "Default : EqualityComparer<int>"
                    | other -> failtestf "unexpected Default signature %A" other
                | ValueNone -> failtest "Default did not resolve"
            }

            test "GetHashCode resolves as an instance method typed 'T -> int" {
                match provider.TryLookupMember(eqComparer, "GetHashCode") with
                | ValueSome m ->
                    Expect.isFalse m.IsStatic "GetHashCode(T) is an instance method"
                    Expect.isFalse m.IsProperty "a method, not a property"

                    // Instantiated at `'T = int`: `int -> int`.
                    match m.BuildSignature [| TyConst "int" |] with
                    | TyFun(TyConst "int", TyConst "int") -> ()
                    | other -> failtestf "expected int -> int, got %A" other
                | ValueNone -> failtest "GetHashCode did not resolve"
            }

            test "an unknown type misses" {
                Expect.isTrue (provider.TryLookupType "No.Such.Type`9" |> ValueOption.isNone) "unknown type miss"
            }

            test "the metadata layer resolves no values" {
                // F#-style module values / operators are not a metadata surface;
                // they fall through to lower-priority sources in the composite.
                Expect.isTrue (provider.TryLookup "op_Addition" |> ValueOption.isNone) "no value surface"
            }
        ]
