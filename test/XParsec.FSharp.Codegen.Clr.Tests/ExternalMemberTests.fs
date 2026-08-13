module XParsec.FSharp.Codegen.Clr.Tests.ExternalMemberTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Member access on an EXTERNAL type: each access node freezes carrying the `SymbolKey`
// the metadata-backed provider resolved for it, and the emitted call runs.

let private eqComparer = "System.Collections.Generic.EqualityComparer`1"

let private analyseWith (provider: IExternalSymbolProvider) (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSem provider (Hashing.originSourceOfText lexed) file

/// `analyseWith` keeping the `PassContext`, so a test can zonk a live `TyVar` against
/// the per-file `TypeStore`.
let private analyseWithCtx (provider: IExternalSymbolProvider) (input: string) : PassContext * TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText lexed) file

let private errors (tast: TastFile) : Diagnostic list = tast.Diagnostics |> Diagnostic.errors

/// The home assembly of the type `decl` names. A `SymbolKey` is a NOMINAL identity and
/// carries no home, so the physical location has to be read back off the shape the
/// provider resolves for that key.
let private declAssembly (provider: IExternalSymbolProvider) (decl: TypeKey) : string option =
    match (provider :> IExternalSymbolStore).TryLookupType(SymbolKey.Type decl) with
    | ValueSome(ExternalTypeShape.Class info) ->
        match info.Origin.Home.AssemblyOption with
        | ValueSome a -> Some a
        | ValueNone -> None
    | ValueSome other ->
        failtestf "expected %s to resolve to a Class shape, got %A" (SymbolKeyOps.typeMetaName decl) other
    | ValueNone ->
        failtestf "the declaring type %s did not resolve through the provider" (SymbolKeyOps.typeMetaName decl)

[<Tests>]
let tests =
    testList
        "ExternalMember"
        [
            test "EqualityComparer<int>.Default.GetHashCode 5 type-checks + freezes carrying its key" {
                // Vesper.Core supplies the `type int = (# "System.Int32" #)` relationship the
                // metadata tail canonicalizes `GetHashCode`'s `System.Int32` return through.
                let provider = ClrSymbolProviders.build [ vesperCorePackage ]

                let ctx, tast =
                    analyseWithCtx
                        provider
                        "let h = System.Collections.Generic.EqualityComparer<int>.Default.GetHashCode 5"

                Expect.isEmpty (errors tast) "no type errors through the metadata-backed provider"

                // `GetHashCode 5` is `App(ExternalMember(GetHashCode), 5)`; the
                // GetHashCode access's object argument is the `Default` static access.
                let value =
                    match tast.Decls with
                    | EqList [ TDecl.Let(value = v) ] -> v
                    | _ -> failtestf "expected a single let binding, got %A" tast.Decls

                match value with
                | TExpr.App(TExpr.ExternalMember(ValueSome inner, ghKey, "GetHashCode", MemberStorage.Method, _, ghTy, _),
                            TExpr.Const(TConstValue.Integral(IntWidth.Int32, 5L), _, _),
                            resultTy,
                            _) ->
                    match Unification.zonk ctx.Store ghTy with
                    | TyFun(TyConst(k1, _), TyConst(k2, _)) when
                        SymbolKeyOps.simpleName k1 = DisplayName "int"
                        && SymbolKeyOps.simpleName k2 = DisplayName "int"
                        ->
                        ()
                    | other -> failtestf "GetHashCode should be typed int -> int, got %A" other

                    match Unification.zonk ctx.Store resultTy with
                    | TyConst(key, _) when SymbolKeyOps.simpleName key = DisplayName "int" -> ()
                    | other -> failtestf "the application should be typed int, got %A" other

                    match ghKey with
                    | SymbolKey.Member {
                                           Decl = decl
                                           Name = "GetHashCode"
                                           ArgSig = argSig
                                           Kind = MemberKind.Method
                                       } ->
                        Expect.isSome
                            (declAssembly provider decl)
                            "GetHashCode's declaring type is homed in the defining assembly"

                        Expect.equal
                            decl
                            (SymbolKeyOps.typeKeyOfArity "System.Collections.Generic" "EqualityComparer" 1)
                            "GetHashCode declaring type"

                        Expect.equal
                            (SymbolKeyOps.typeSegmentName decl)
                            "EqualityComparer`1"
                            "the arity is spelled only when the metadata name is RENDERED"

                        Expect.equal
                            (EqArray.toList argSig)
                            [ FrozenType.FTTypar(TyparAxis.Declaring, 0) ]
                            "GetHashCode(T) argSig is the declaring typar"
                    | other -> failtestf "unexpected GetHashCode key %A" other

                    match inner with
                    | TExpr.ExternalMember(ValueNone, defKey, "Default", MemberStorage.Property, _, defTy, _) ->
                        match Unification.zonk ctx.Store defTy with
                        | TyClass(name, args) when
                            args.Length = 1
                            && (
                                match args.[0] with
                                | TyConst(key, _) -> SymbolKeyOps.simpleName key = DisplayName "int"
                                | _ -> false
                            )
                            ->
                            Expect.equal name eqComparer "Default : EqualityComparer<int>"
                        | other -> failtestf "Default should be typed EqualityComparer<int>, got %A" other

                        match defKey with
                        | SymbolKey.Member {
                                               Decl = decl
                                               Name = "Default"
                                               ArgSig = argSig
                                               Kind = MemberKind.Property
                                           } ->
                            Expect.equal
                                decl
                                (SymbolKeyOps.typeKeyOfArity "System.Collections.Generic" "EqualityComparer" 1)
                                "Default declaring type"

                            Expect.isTrue argSig.IsEmpty "Default is a property: empty argSig"
                        | other -> failtestf "unexpected Default key %A" other
                    | other -> failtestf "expected a static `Default` ExternalMember object argument, got %A" other
                | other -> failtestf "expected App(ExternalMember GetHashCode, 5), got %A" other
            }

            test "the frozen key matches the provider's own resolved member key" {
                // The interned key must equal what the provider resolves directly:
                // elaboration stamps the resolver's verdict rather than re-deriving a key.
                let provider = ClrSymbolProviders.build [ vesperCorePackage ]

                let expected =
                    match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey eqComparer 0, "GetHashCode") with
                    | ValueSome m -> SymbolKey.Member m.Key
                    | ValueNone -> failtest "provider did not resolve GetHashCode"

                let ctx, tast =
                    analyseWithCtx
                        provider
                        "let h = System.Collections.Generic.EqualityComparer<int>.Default.GetHashCode 5"

                let frozen =
                    match tast.Decls with
                    | EqList [ TDecl.Let(value = TExpr.App(TExpr.ExternalMember(key = k), _, _, _)) ] -> k
                    | _ -> failtestf "expected App(ExternalMember …), got %A" tast.Decls

                Expect.equal frozen expected "frozen key = provider's resolved key"
            }

            test "short name under `open` type-checks + freezes carrying its key" {
                let provider = ClrSymbolProviders.build [ vesperCorePackage ]

                let ctx, tast =
                    analyseWithCtx
                        provider
                        "open System.Collections.Generic\nlet h = EqualityComparer<int>.Default.GetHashCode 5"

                Expect.isEmpty (errors tast) "no type errors for the short-name form under its open"

                let value =
                    tast.Decls
                    |> EqArray.tryFind (
                        function
                        | TDecl.Let _ -> true
                        | _ -> false
                    )
                    |> ValueOption.map (
                        function
                        | TDecl.Let(value = v) -> v
                        | _ -> failwith "unreachable"
                    )

                match value with
                | ValueSome(TExpr.App(TExpr.ExternalMember(ValueSome inner,
                                                           ghKey,
                                                           "GetHashCode",
                                                           MemberStorage.Method,
                                                           _,
                                                           ghTy,
                                                           _),
                                      TExpr.Const(TConstValue.Integral(IntWidth.Int32, 5L), _, _),
                                      resultTy,
                                      _)) ->
                    match Unification.zonk ctx.Store ghTy with
                    | TyFun(TyConst(k1, _), TyConst(k2, _)) when
                        SymbolKeyOps.simpleName k1 = DisplayName "int"
                        && SymbolKeyOps.simpleName k2 = DisplayName "int"
                        ->
                        ()
                    | other -> failtestf "GetHashCode should be typed int -> int, got %A" other

                    match Unification.zonk ctx.Store resultTy with
                    | TyConst(key, _) when SymbolKeyOps.simpleName key = DisplayName "int" -> ()
                    | other -> failtestf "the application should be typed int, got %A" other

                    match ghKey with
                    | SymbolKey.Member {
                                           Decl = decl
                                           Name = "GetHashCode"
                                           ArgSig = argSig
                                           Kind = MemberKind.Method
                                       } ->
                        Expect.isSome
                            (declAssembly provider decl)
                            "GetHashCode's declaring type is homed in the defining assembly"

                        Expect.equal
                            decl
                            (SymbolKeyOps.typeKeyOfArity "System.Collections.Generic" "EqualityComparer" 1)
                            "GetHashCode declaring type"

                        Expect.equal
                            (SymbolKeyOps.typeSegmentName decl)
                            "EqualityComparer`1"
                            "the arity is spelled only when the metadata name is RENDERED"

                        Expect.equal
                            (EqArray.toList argSig)
                            [ FrozenType.FTTypar(TyparAxis.Declaring, 0) ]
                            "GetHashCode(T) argSig is the declaring typar"
                    | other -> failtestf "unexpected GetHashCode key %A" other

                    match inner with
                    | TExpr.ExternalMember(ValueNone, _, "Default", MemberStorage.Property, _, _, _) -> ()
                    | other -> failtestf "expected a static `Default` ExternalMember object argument, got %A" other
                | other -> failtestf "expected App(ExternalMember GetHashCode, 5), got %A" other
            }

            test "short-name key equals the fully-qualified form's resolved key" {
                let provider = ClrSymbolProviders.build [ vesperCorePackage ]

                let expected =
                    match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey eqComparer 0, "GetHashCode") with
                    | ValueSome m -> SymbolKey.Member m.Key
                    | ValueNone -> failtest "provider did not resolve GetHashCode"

                let ctx, tast =
                    analyseWithCtx
                        provider
                        "open System.Collections.Generic\nlet h = EqualityComparer<int>.Default.GetHashCode 5"

                let frozen =
                    tast.Decls
                    |> EqArray.tryFind (
                        function
                        | TDecl.Let(value = TExpr.App(TExpr.ExternalMember _, _, _, _)) -> true
                        | _ -> false
                    )
                    |> ValueOption.map (
                        function
                        | TDecl.Let(value = TExpr.App(TExpr.ExternalMember(key = k), _, _, _)) -> k
                        | _ -> failwith "unreachable"
                    )

                Expect.equal frozen (ValueSome expected) "frozen key (short name) = provider's resolved key"
            }

            // Both member refs are minted from the interned key on one
            // `EqualityComparer`1<int>` TypeSpec. `Int32.GetHashCode` is the identity
            // function, which is why the expected output is the argument itself.
            test "an emitted call to a metadata-resolved member runs (EqualityComparer<int>.Default.GetHashCode 5 = 5)" {
                let src =
                    "printfn \"%d\" (System.Collections.Generic.EqualityComparer<int>.Default.GetHashCode 5)"

                let _, artifact = compileSource "P4ExternalMemberFq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "5" "GetHashCode of int 5 is 5"

                // The BCL comparer rides `System.Private.CoreLib`, not FSharp.Core.
                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "BCL member call pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            // A type ANNOTATION naming an external type must resolve to the same `TyClass`
            // a value of that type carries, so the two unify. An annotation that dropped
            // its type arguments would clash with the object argument's `TyClass`.
            test "a type annotation resolves an external type, so the short form unifies with the object argument" {
                let provider = ClrSymbolProviders.build [ vesperCorePackage ]

                let tast =
                    analyseWith
                        provider
                        "open System.Collections.Generic\nlet d : EqualityComparer<int> = EqualityComparer<int>.Default"

                Expect.isEmpty
                    (errors tast)
                    "the annotated external type unifies with the resolved Default object argument"
            }

            test "a fully-qualified type annotation resolves to the external TyClass (not a fresh TyVar)" {
                let provider = ClrSymbolProviders.build [ vesperCorePackage ]

                // An UNresolved annotation would be a fresh `TyVar`, which unifies with
                // `5 : int` silently. So the observable is inverted: an error is required,
                // and its message must name the resolved type, not an anonymous variable.
                let tast =
                    analyseWith provider "let d : System.Collections.Generic.EqualityComparer<int> = 5"

                let errs = errors tast
                Expect.isNonEmpty errs "the resolved external annotation rejects the int RHS"

                Expect.isTrue
                    (errs |> List.exists (fun d -> d.Message.Contains "EqualityComparer"))
                    (sprintf
                        "the mismatch names the resolved external type, got %A"
                        (errs |> List.map (fun d -> d.Message)))
            }

            // `Equals` is modelled tupled (`(int*int) -> bool`), so the declaring typar has
            // to be recovered from the tuple ELEMENT: recovering it from the whole tuple
            // mis-encodes the call as `EqualityComparer<int*int>`.
            test "a 2-arg external instance method (EqualityComparer<int>.Default.Equals) emits + runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections.Generic"
                            "printfn \"%d\" (if EqualityComparer<int>.Default.Equals(1, 1) then 1 else 0)" // 1
                            "printfn \"%d\" (if EqualityComparer<int>.Default.Equals(1, 2) then 1 else 0)" // 0
                        ]

                let _, artifact = compileSource "P4ExternalEquals2Arg" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "Equals(1,1)=true, Equals(1,2)=false"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "2-arg BCL member call pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            // `String.Concat` is heavily overloaded, so the pick has to narrow by arity and
            // then by betterness: `(string,string)` beats `(object,object)`.
            test "a 2-arg external static method with overloads (String.Concat) resolves + runs" {
                let _, artifact =
                    compileSource "P4ExternalConcat2Arg" "printfn \"%s\" (System.String.Concat(\"a\", \"b\"))"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "ab" "String.Concat(\"a\", \"b\") = \"ab\""
            }

            // Overload resolution reads the argument's TYPE, so a tuple-VALUED expression
            // picks the same 2-parameter overload a syntactic `("a", "b")` does.
            test "a tuple-VALUED argument at a 2-param external method emits + runs" {
                let src =
                    String.concat "\n" [ "let t = (\"a\", \"b\")"; "printfn \"%s\" (System.String.Concat t)" ]

                let _, artifact = compileSource "P4ExternalConcatTupleVar" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "ab" "String.Concat t = \"ab\""
            }

            // Destructuring a tuple-VALUED argument must not reorder or duplicate it: the
            // object argument evaluates first, the argument exactly once. Both operands
            // print, so the printed order IS the evaluation order.
            test "an instance member keeps object-argument-before-argument order over a tuple VALUE" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Text"
                            "let objArg () : StringBuilder ="
                            "    printfn \"R\""
                            "    StringBuilder(\"xy\")"
                            "let arg () : string * string ="
                            "    printfn \"A\""
                            "    (\"x\", \"z\")"
                            "printfn \"%s\" ((objArg ()).Replace(arg ()).ToString())"
                        ]

                let _, artifact = compileSource "P4ExternalTupleValueOrder" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "R\nA\nzy" "object argument, then argument, then `zy`"
            }

            test "the short-name form under `open` emits and runs" {
                let src =
                    "open System.Collections.Generic\nprintfn \"%d\" (EqualityComparer<int>.Default.GetHashCode 42)"

                let _, artifact = compileSource "P4ExternalMemberShort" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "GetHashCode of int 42 is 42"
            }

            // With no `<>` to mark where the type prefix ends, `System.Console.Out` folds
            // into a single LongIdent, so the type / static-member split has to be probed
            // for rather than read off the syntax.
            test "non-generic external static property resolves + freezes carrying its key" {
                let provider = ClrSymbolProviders.build []
                let ctx, tast = analyseWithCtx provider "let w = System.Console.Out"

                Expect.isEmpty (errors tast) "System.Console.Out resolves through the metadata provider"

                let value =
                    match tast.Decls with
                    | EqList [ TDecl.Let(value = v) ] -> v
                    | _ -> failtestf "expected a single let binding, got %A" tast.Decls

                match value with
                | TExpr.ExternalMember(ValueNone, key, "Out", MemberStorage.Property, _, ty, _) ->
                    match Unification.zonk ctx.Store ty with
                    | TyClass("System.IO.TextWriter", args) when args.IsEmpty -> ()
                    | other -> failtestf "Out should be typed System.IO.TextWriter, got %A" other

                    match key with
                    | SymbolKey.Member {
                                           Decl = decl
                                           Name = "Out"
                                           ArgSig = argSig
                                           Kind = MemberKind.Property
                                       } ->
                        Expect.isSome
                            (declAssembly provider decl)
                            "Out's declaring type is homed in the defining assembly"

                        Expect.equal decl (SymbolKeyOps.typeKeyOfArity "System" "Console" 0) "Out declaring type"
                        Expect.isTrue argSig.IsEmpty "Out is a property: empty argSig"
                    | other -> failtestf "unexpected Out key %A" other
                | other -> failtestf "expected a static `Out` ExternalMember, got %A" other
            }

            test "the short form under `open` resolves the same non-generic static member" {
                let provider = ClrSymbolProviders.build []
                let tast = analyseWith provider "open System\nlet w = Console.Out"

                Expect.isEmpty (errors tast) "Console.Out resolves under `open System`"

                let value =
                    tast.Decls
                    |> EqArray.tryFind (
                        function
                        | TDecl.Let _ -> true
                        | _ -> false
                    )
                    |> ValueOption.map (
                        function
                        | TDecl.Let(value = v) -> v
                        | _ -> failwith "unreachable"
                    )

                match value with
                | ValueSome(TExpr.ExternalMember(ValueNone,
                                                 SymbolKey.Member {
                                                                      Decl = decl
                                                                      Name = "Out"
                                                                      ArgSig = EqList []
                                                                      Kind = MemberKind.Property
                                                                  },
                                                 "Out",
                                                 MemberStorage.Property,
                                                 _,
                                                 _,
                                                 _)) when SymbolKeyOps.typeMetaName decl = "System.Console" -> ()
                | other -> failtestf "expected the same keyed Console.Out ExternalMember, got %A" other
            }

            // `System.Math.PI` is a const field, which is not modelled. The F# is valid, so
            // the unmatched tail must fall through without a "no accessible member" error.
            test "a non-member tail on a resolved external type does not error" {
                let provider = ClrSymbolProviders.build []
                let tast = analyseWith provider "let p = System.Math.PI"
                Expect.isEmpty (errors tast) "System.Math.PI (a field) falls through silently, no false error"
            }

            test "a non-generic external static property emits and runs" {
                let src = "printfn \"%d\" System.Environment.ProcessorCount"
                let _, artifact = compileSource "NonGenericStaticProp" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                let n = output.Replace("\r", "").Trim()

                match System.Int32.TryParse n with
                | true, v -> Expect.isTrue (v > 0) (sprintf "ProcessorCount is a positive int, got %d" v)
                | false, _ -> failtestf "expected a numeric ProcessorCount, got %A" n
            }

            // Taking the property path instead would emit `call get_Empty`, which
            // `MissingMethodException`s at JIT since `String` has no such accessor: a clean
            // run is the proof that `ldsfld` was emitted.
            test "a genuine external static field (String.Empty) emits and runs" {
                // Printed directly (`%s`) to isolate the `ldsfld`: chaining an intrinsic
                // like `.Length` off it would bring in a second emission path.
                let src = "printfn \"[%s]\" System.String.Empty"
                let _, artifact = compileSource "ExternalStaticField" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "[]" "String.Empty is the empty string"
            }
        ]
