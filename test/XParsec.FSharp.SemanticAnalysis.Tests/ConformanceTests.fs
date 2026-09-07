module XParsec.FSharp.SemanticAnalysis.Tests.ConformanceTests

open Vesper

// Sig/impl conformance over the two ANALYSED halves: every verdict is taken by resolved
// identity, so a `[<CompiledName>]`, a `ModuleSuffix` module and a shadowed attribute are
// settled before the comparison.

open System.IO

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.Codegen.Common.Tests

/// Every analysed unit's findings, or a test failure citing the halves that did not parse.
let private analysedDiagnostics
    (what: string)
    (units: AssemblyFiles.SourceUnit list)
    : AssemblyFiles.AnchoredDiagnostic list =
    let analysed =
        AnalysedAssembly.analyse
            Pipeline.analyseFileFor
            realProvider.Value
            (AssemblySources.synthetic "TestAsm" "clr" Set.empty units)

    analysed.Units
    |> List.collect (
        function
        | AssemblyAnalysis.UnitOutcome.Analysed u -> AssemblyFiles.fileDiagnostics u.File
        | AssemblyAnalysis.UnitOutcome.Failed(leading, rest) ->
            failtestf
                "%s did not parse: %s"
                what
                (leading :: rest |> List.map (fun e -> e.Id.Name) |> String.concat ", ")
    )

/// Every conformance verdict the in-assembly route reports for one `.fsi` / `.fs` pair.
let conformAnalysed (sigSrc: string) (implSrc: string) : string list =
    [
        AssemblyFiles.SourceUnit.paired
            (AssemblyFiles.SourceFile.ofText "pair.fsi" sigSrc)
            (AssemblyFiles.SourceFile.ofText "pair.fs" implSrc)
    ]
    |> analysedDiagnostics "the pair"
    |> List.choose (fun a ->
        match a.Diagnostic.Kind with
        | Kind.Conformance _ -> Some a.Diagnostic.Message
        | _ -> None
    )

/// The error-severity findings of one analysed `.fsi` / `.fs` pair, whatever pass reported
/// them, paired with the file each was anchored in.
let private pairErrors (sigSrc: string) (implSrc: string) : (string * string) list =
    [
        AssemblyFiles.SourceUnit.paired
            (AssemblyFiles.SourceFile.ofText "pair.fsi" sigSrc)
            (AssemblyFiles.SourceFile.ofText "pair.fs" implSrc)
    ]
    |> analysedDiagnostics "the pair"
    |> List.filter (fun a -> a.Diagnostic.Severity = Severity.Error)
    |> List.map (fun a -> AssemblyFileId.toStored a.Path, a.Diagnostic.Message)

/// The error-severity findings of ONE analysed implementation, so a fixture that fails for an
/// unrelated reason says so rather than passing a conformance assertion vacuously.
let private analysedErrors (implSrc: string) : string list =
    [
        AssemblyFiles.SourceUnit.ofImplementation (AssemblyFiles.SourceFile.ofText "solo.fs" implSrc)
    ]
    |> analysedDiagnostics "the implementation"
    |> List.filter (fun a -> a.Diagnostic.Severity = Severity.Error)
    |> List.map (fun a -> a.Diagnostic.Message)

let theOne (what: string) (msgs: string list) : string =
    match msgs with
    | [ m ] -> m
    | other -> failtestf "expected exactly one %s, got %A" what other

/// One implementation analysed as the sole unit of a synthetic clr-target assembly.
let private analysedSolo (implSrc: string) : AnalysedAssembly =
    AnalysedAssembly.analyse
        Pipeline.analyseFileFor
        realProvider.Value
        (AssemblySources.synthetic
            "TestAsm"
            "clr"
            Set.empty
            [
                AssemblyFiles.SourceUnit.ofImplementation (AssemblyFiles.SourceFile.ofText "solo.fs" implSrc)
            ])

/// The obligations one analysed implementation records for the gate's discharge.
let private analysedFrozenImports (implSrc: string) : ImportObligation list =
    match (analysedSolo implSrc).Units with
    | [ AssemblyAnalysis.UnitOutcome.Analysed u ] -> u.File.Imports
    | _ -> failtest "the implementation did not analyse"

[<Tests>]
let analysedConformanceTests =
    testList
        "AnalysedConformance"
        [
            test "extern in the .fsi with a non-intrinsic impl → the repr is owed" {
                let m =
                    conformAnalysed "namespace V\n\ntype foo = extern" "namespace V\n\ntype foo = int"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "the finding names the identity, not a spelling"
                Expect.stringContains m "no intrinsic representation" "the extern is unanswered"
            }

            test "an intrinsic in the .fs with no extern in the .fsi is reported" {
                let m =
                    conformAnalysed "namespace V\n\ntype foo = int" "namespace V\n\ntype foo = (# \"System.Int32\" #)"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "names the identity"
                Expect.stringContains m "not declared 'extern'" "a repr the contract never declares"
            }

            test "extern class ↔ (# class repr #) conforms" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\ntype Base = extern class"
                        "namespace V\n\ntype Base = (# class \"System.Attribute\" #)")
                    "a heritable extern paired with its tagged intrinsic conforms"
            }

            test "extern class with an untagged repr → heritability disagrees" {
                let m =
                    conformAnalysed
                        "namespace V\n\ntype Base = extern class"
                        "namespace V\n\ntype Base = (# \"System.Attribute\" #)"
                    |> theOne "finding"

                Expect.stringContains m "V.Base" "names the identity"
                Expect.stringContains m "heritability" "the tag disagrees across the pair"
            }

            test "bare extern with a (# class repr #) impl → heritability disagrees" {
                let m =
                    conformAnalysed
                        "namespace V\n\ntype Base = extern"
                        "namespace V\n\ntype Base = (# class \"System.Attribute\" #)"
                    |> theOne "finding"

                Expect.stringContains m "heritability" "the sig is opaque, the impl heritable"
            }

            test "a union declared in the .fsi and absent from the .fs is missing" {
                let m =
                    conformAnalysed "namespace V\n\ntype Bar = | BarCase" "namespace V\n\ntype Other = | OtherCase"
                    |> theOne "finding"

                Expect.stringContains m "V.Bar" "names the union owing a definition"
                Expect.stringContains m "not defined in the implementation" "the FS0240 analogue"
            }

            test "a private implementation type behind a public signature declaration is missing" {
                // fsc's FS0034: the accessibility in the signature exceeds the implementation's.
                // The signatureless surface omits the private type, so presence reports it.
                let m =
                    conformAnalysed "namespace V\n\ntype Bar = | BarCase" "namespace V\n\ntype private Bar = | BarCase"
                    |> theOne "finding"

                Expect.stringContains m "V.Bar" "names the type the signature publishes"
                Expect.stringContains m "not defined in the implementation" "the private type is not published"
            }

            test "a type defined in the .fs and absent from the .fsi is hidden, not drift" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\ntype Bar = | BarCase"
                        "namespace V\n\ntype Bar = | BarCase\n\ntype Hidden = | H")
                    "F# hides an implementation type the signature omits"
            }

            test "an abbreviation declared in the .fsi and absent from the .fs is missing" {
                let m =
                    conformAnalysed "namespace V\n\ntype alias = int" "namespace V\n\ntype other = int"
                    |> theOne "finding"

                Expect.stringContains m "V.alias" "refers to the abbreviation owing a definition"
                Expect.stringContains m "not defined in the implementation" "the FS0240 analogue"
            }

            test "a matching abbreviation pair conforms" {
                Expect.isEmpty
                    (conformAnalysed "namespace V\n\ntype alias = int" "namespace V\n\ntype alias = int")
                    "the implementation's abbreviation matches the one the signature publishes"
            }

            test "kind drift: a class-published sig over a record impl is a mismatch" {
                let m =
                    conformAnalysed
                        "namespace V\n\ntype foo =\n    member P: int"
                        "namespace V\n\ntype foo = { X: int }"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "names the identity"
                Expect.stringContains m "declared as a class" "the signature's committed family"
                Expect.stringContains m "defined as a record" "the implementation's actual family"
            }

            test "kind drift: a class-published sig over an interface impl is a mismatch" {
                let m =
                    conformAnalysed
                        "namespace V\n\ntype foo =\n    member P: int"
                        "namespace V\n\ntype foo =\n    abstract M: int"
                    |> theOne "finding"

                Expect.stringContains m "declared as a class" "the signature's committed family"
                Expect.stringContains m "defined as an interface" "the implementation's actual family"
            }

            test "matching families conform: record, union, enum, interface and class pairs" {
                let pairs =
                    [
                        "type R = { X: int }", "type R = { X: int }"
                        "type U = | A | B", "type U = | A | B"
                        "type E =\n    | A = 1\n    | B = 2", "type E =\n    | A = 1\n    | B = 2"
                        "type I =\n    abstract M: int", "type I =\n    abstract M: int"
                        "type C =\n    new: unit -> C\n    member P: int", "type C() =\n    member _.P = 1"
                    ]

                for sigDecl, implDecl in pairs do
                    Expect.isEmpty
                        (conformAnalysed ("namespace V\n\n" + sigDecl) ("namespace V\n\n" + implDecl))
                        (sprintf "the pair agrees on its family: %s" sigDecl)
            }

            test "an opaque sig type demands no family of its implementation" {
                Expect.isEmpty
                    (conformAnalysed "namespace V\n\ntype T" "namespace V\n\ntype T = { X: int }")
                    "an opaque `type T` hides the representation, so any concrete family satisfies it"
            }

            test "a delegate is refused on both halves and takes no pairing verdict" {
                let src = "namespace V\n\ntype Handler = delegate of int -> int"

                Expect.equal
                    (pairErrors src src)
                    [
                        "pair.fsi", "not yet supported: `delegate` type declarations"
                        "pair.fs", "not yet supported: `delegate` type declarations"
                    ]
                    "each half is refused where it is written"

                Expect.isEmpty (conformAnalysed src src) "a construct neither half models owes no pairing verdict"
            }

            test "a delegate the .fs omits is refused, not reported as missing" {
                Expect.isEmpty
                    (conformAnalysed "namespace V\n\ntype Handler = delegate of int -> int" "namespace V\n\ntype T")
                    "the signature published a gap, not an identity the implementation owes"
            }

            test "a val with no matching let is missing" {
                let m =
                    conformAnalysed "namespace V\n\nval foo: int -> int" "namespace V\n\nlet bar (x: int) = x"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "refers to the value owing a definition"
                Expect.stringContains m "not defined in the implementation" "the value-granularity analogue"
            }

            test "a matching val/let pair, operator included, conforms" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\nval foo: int -> int\n\nval inline (+++): int -> int -> int"
                        "namespace V\n\nlet foo (x: int) = x\n\nlet inline (+++) (a: int) (b: int) = a")
                    "a val paired with its let — plain and operator — conforms"
            }

            test "a [<CompiledName>]'d pair sharing a source name conforms" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\n[<CompiledName(\"Foo\")>]\nval foo: int -> int"
                        "namespace V\n\n[<CompiledName(\"Foo\")>]\nlet foo (x: int) = x")
                    "a value's identity is the name its source writes, whatever it emits as"
            }

            test "a [<CompiledName>]'d let does not satisfy a val of that compiled name" {
                // `fsc` refuses this pair with FS0193 "Module 'V.M' requires a value 'foo'":
                // the halves are matched by the name each writes, not by what they emit as.
                let m =
                    conformAnalysed
                        "namespace V\n\nval foo: int -> int"
                        "namespace V\n\n[<CompiledName(\"foo\")>]\nlet bar (x: int) = x"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "refers to the value owing a definition"
                Expect.stringContains m "not defined in the implementation" "the value-granularity analogue"
            }

            // ---- The EMITTED name across the pair ----
            //
            // A reference resolves through the signature's surface while the implementation
            // emits under its own declaration, so a `[<CompiledName>]` on one half alone
            // would have a consumer call a method that is never emitted.

            test "a [<CompiledName>] written on the implementation alone is reported" {
                let m =
                    conformAnalysed
                        "namespace V\n\nval foo: int -> int"
                        "namespace V\n\n[<CompiledName(\"Foo\")>]\nlet foo (x: int) = x"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "names the value whose halves disagree"
                Expect.stringContains m "'foo'" "the name the signature publishes it under"
                Expect.stringContains m "'Foo'" "the method the implementation emits"
            }

            test "a [<CompiledName>] written on the signature alone is reported" {
                let m =
                    conformAnalysed
                        "namespace V\n\n[<CompiledName(\"Foo\")>]\nval foo: int -> int"
                        "namespace V\n\nlet foo (x: int) = x"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "names the value whose halves disagree"
            }

            test "the halves disagreeing on WHICH compiled name is reported" {
                // ONE finding: the emitted-name check subsumes the FS1200 argument
                // divergence for this attribute, so `divergentAttributes` skips it.
                let emitted =
                    conformAnalysed
                        "namespace V\n\n[<CompiledName(\"Foo\")>]\nval foo: int -> int"
                        "namespace V\n\n[<CompiledName(\"Bar\")>]\nlet foo (x: int) = x"
                    |> theOne "finding"

                Expect.stringContains emitted "'Foo'" "the signature's emitted name"
                Expect.stringContains emitted "'Bar'" "the implementation's"
            }

            // ---- Attribute ARGUMENTS across the pair (fsc's FS1200) ----

            test "one attribute written on both halves with differing arguments is reported" {
                let m =
                    conformAnalysed
                        "namespace V\n\n[<Experimental(\"one\")>]\nval foo: int -> int"
                        "namespace V\n\n[<Experimental(\"other\")>]\nlet foo (x: int) = x"
                    |> theOne "finding"

                Expect.stringContains m "V.foo" "names the declaration"
                Expect.stringContains m "ExperimentalAttribute" "names the attribute"
                Expect.stringContains m "the signature's (.fsi) arguments are the ones compiled" "which copy ships"
            }

            test "the divergence is a warning, not an error" {
                Expect.isEmpty
                    (pairErrors
                        "namespace V\n\n[<Experimental(\"one\")>]\nval foo: int -> int"
                        "namespace V\n\n[<Experimental(\"other\")>]\nlet foo (x: int) = x")
                    "fsc compiles the pair, so this compiler must too"
            }

            test "matching arguments conform, whatever their spelling" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\n[<CompilerMessage(\"m\", 0x1)>]\nval foo: int -> int"
                        "namespace V\n\n[<CompilerMessage(\"m\", 1)>]\nlet foo (x: int) = x")
                    "the two halves write one attribute with one folded argument list"
            }

            test "a folded argument conforms however it was computed" {
                // fsc accepts `A ||| B` against `B ||| A`.
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\n[<CompilerMessage(\"m\", 1 ||| 2)>]\nval foo: int -> int"
                        "namespace V\n\n[<CompilerMessage(\"m\", 2 ||| 1)>]\nlet foo (x: int) = x")
                    "both halves fold to one argument list"
            }

            // ---- `[<Literal>]` values across the pair (fsc's FS0034) ----

            test "a literal written on both halves with one value conforms, whatever its spelling" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\n[<Literal>]\nval Mask: int = 0x3"
                        "namespace V\n\n[<Literal>]\nlet Mask = 1 ||| 2")
                    "both halves denote 3"
            }

            test "a literal whose value differs across the pair is reported" {
                let m =
                    conformAnalysed
                        "namespace V\n\n[<Literal>]\nval Mask: int = 3"
                        "namespace V\n\n[<Literal>]\nlet Mask = 4"
                    |> theOne "finding"

                Expect.stringContains m "V.Mask" "names the value"
                Expect.stringContains m "differing constant values: 3 in the signature" "the values disagree"
                Expect.stringContains m "4 in the implementation" "the implementation's value is shown"
            }

            test "a literal on the implementation alone is reported" {
                let m =
                    conformAnalysed "namespace V\n\nval Mask: int" "namespace V\n\n[<Literal>]\nlet Mask = 3"
                    |> theOne "finding"

                Expect.stringContains m "is not [<Literal>] in the signature (.fsi) but is" "one-sided"
            }

            test "named arguments conform in either order" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\n[<CompilerMessage(\"m\", 1, IsError = true, IsHidden = false)>]\nval foo: int -> int"
                        "namespace V\n\n[<CompilerMessage(\"m\", 1, IsHidden = false, IsError = true)>]\nlet foo (x: int) = x")
                    "ordering a named argument differently is not a divergence"
            }

            test "a named argument with a differing value is reported" {
                let m =
                    conformAnalysed
                        "namespace V\n\n[<CompilerMessage(\"m\", 1, IsError = true)>]\nval foo: int -> int"
                        "namespace V\n\n[<CompilerMessage(\"m\", 1, IsError = false)>]\nlet foo (x: int) = x"
                    |> theOne "finding"

                Expect.stringContains m "CompilerMessageAttribute" "names the attribute"
            }

            test "an attribute on one half alone takes no verdict" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\nval foo: int -> int"
                        "namespace V\n\n[<Experimental(\"only here\")>]\nlet foo (x: int) = x")
                    "F# takes the attribute from whichever half writes it"
            }

            // ---- Attribute ARGUMENTS across a TYPE declaration pair (fsc's FS1200) ----

            test "a type attribute written on both halves with differing arguments is reported, every kind" {
                let decls =
                    [
                        "type R = { X: int }", "type R = { X: int }", "V.R"
                        "type U = | A | B", "type U = | A | B", "V.U"
                        "type E =\n    | A = 1\n    | B = 2", "type E =\n    | A = 1\n    | B = 2", "V.E"
                        "type I =\n    abstract M: int", "type I =\n    abstract M: int", "V.I"
                        "type C =\n    new: unit -> C\n    member P: int", "type C() =\n    member _.P = 1", "V.C"
                        "type A = int", "type A = int", "V.A"
                    ]

                for sigDecl, implDecl, name in decls do
                    let m =
                        conformAnalysed
                            ("namespace V\n\n[<Experimental(\"one\")>]\n" + sigDecl)
                            ("namespace V\n\n[<Experimental(\"other\")>]\n" + implDecl)
                        |> theOne (sprintf "finding for %s" sigDecl)

                    Expect.stringContains m name "names the type declaration"
                    Expect.stringContains m "ExperimentalAttribute" "names the attribute"
                    Expect.stringContains m "the signature's (.fsi) arguments are the ones compiled" "which copy ships"
            }

            test "a type attribute divergence is a warning, not an error" {
                Expect.isEmpty
                    (pairErrors
                        "namespace V\n\n[<Experimental(\"one\")>]\ntype R = { X: int }"
                        "namespace V\n\n[<Experimental(\"other\")>]\ntype R = { X: int }")
                    "fsc compiles the pair, so this compiler must too"
            }

            test "matching type attribute arguments conform, every kind" {
                let decls =
                    [
                        "type R = { X: int }", "type R = { X: int }"
                        "type U = | A | B", "type U = | A | B"
                        "type E =\n    | A = 1\n    | B = 2", "type E =\n    | A = 1\n    | B = 2"
                        "type I =\n    abstract M: int", "type I =\n    abstract M: int"
                        "type C =\n    new: unit -> C\n    member P: int", "type C() =\n    member _.P = 1"
                        "type A = int", "type A = int"
                    ]

                for sigDecl, implDecl in decls do
                    Expect.isEmpty
                        (conformAnalysed
                            ("namespace V\n\n[<Experimental(\"same\")>]\n" + sigDecl)
                            ("namespace V\n\n[<Experimental(\"same\")>]\n" + implDecl))
                        (sprintf "the two halves write one attribute with one folded argument list: %s" sigDecl)
            }

            test "a type attribute on one half alone takes no verdict" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\ntype R = { X: int }"
                        "namespace V\n\n[<Experimental(\"only here\")>]\ntype R = { X: int }")
                    "F# takes the attribute from whichever half writes it"
            }

            test "a type's posture attributes conform when both halves write them" {
                Expect.isEmpty
                    (conformAnalysed
                        "namespace V\n\n[<RequireQualifiedAccess; Struct>]\ntype U = | A | B"
                        "namespace V\n\n[<Struct; RequireQualifiedAccess>]\ntype U = | A | B")
                    "argument-less attributes in either order carry one folded value each"
            }

            // ---- `[<Import>]`, read by resolved identity ----

            test "an [<Import>] binding whose body is not nativeOnly is an error" {
                let m =
                    analysedErrors
                        "namespace V\n\nmodule M =\n\n    [<Import(\"served\", \"./Asset.mjs\")>]\n    let served (x: int) : int = x"
                    |> theOne "error"

                Expect.stringContains m "nativeOnly" "the attribute is the implementation"
            }

            test "an [<Import>] binding with the nativeOnly body and matching selector conforms" {
                Expect.isEmpty
                    (analysedErrors
                        "namespace V\n\nmodule M =\n\n    [<Import(\"served\", \"./Asset.mjs\")>]\n    let served (x: int) : int = nativeOnly")
                    "the sentinel body and the declared selector satisfy every analysed verdict"
            }

            test "a nativeOnly body without [<Import>] is an error" {
                let m =
                    analysedErrors "namespace V\n\nmodule M =\n\n    let served (x: int) : int = nativeOnly"
                    |> theOne "error"

                Expect.stringContains m "[<Import>]" "demanding the declaration that serves it"
            }

            test "a SHADOWING nativeOnly does not satisfy the body check" {
                // The body resolves to the local binding, whose identity is not
                // `Vesper.CompilerMarkers.nativeOnly`, so the sentinel check reports it.
                let m =
                    analysedErrors
                        "namespace V\n\nmodule M =\n\n    let nativeOnly = 42\n\n    [<Import(\"served\", \"./Asset.mjs\")>]\n    let served : int = nativeOnly"
                    |> theOne "error"

                Expect.stringContains m "nativeOnly" "the shadowing value is not the sentinel"
                Expect.stringContains m "[<Import>]" "reported against the import binding"
            }

            test "an [<Import>] selector that is not the emitted name is an error" {
                let errors =
                    analysedErrors
                        "namespace V\n\nmodule M =\n\n    [<Import(\"other\", \"./Asset.mjs\")>]\n    let served (x: int) : int = nativeOnly"

                Expect.isTrue
                    (errors |> List.exists (fun e -> e.Contains "'other'" && e.Contains "selector"))
                    (sprintf "the selector must equal the emitted name; got %A" errors)
            }

            test "a SHADOWING Import declaration is not the compiler's [<Import>]" {
                // Resolving the attribute reaches the local declaration, whose identity is
                // not `Vesper.ImportAttribute`, so the check accepts it.
                Expect.isEmpty
                    (analysedErrors
                        "namespace V\n\ntype ImportAttribute(selector: string, path: string) =\n    inherit Attribute()\n\nmodule M =\n\n    [<Import(\"served\", \"./Asset.mjs\")>]\n    let served (x: int) : int = x")
                    "a locally-declared marker of the same spelling is a different attribute"
            }

            // ---- The per-unit obligation record, for the backend's discharge ----

            test "a well-formed [<Import>] records a per-unit obligation" {
                match
                    analysedFrozenImports
                        "namespace V\n\nmodule M =\n\n    [<Import(\"served\", \"./Asset.mjs\")>]\n    let served (x: int) : int = nativeOnly"
                with
                | [ o ] ->
                    Expect.equal o.Binding "served" "the binding's source name"
                    Expect.equal o.Selector "served" "the selector as written"
                    Expect.equal o.Path "./Asset.mjs" "the path as written"
                | other -> failtestf "expected exactly one obligation, got %d" (List.length other)
            }

            test "a binding without [<Import>] records no obligation" {
                Expect.isEmpty
                    (analysedFrozenImports "namespace V\n\nmodule M =\n\n    let plain (x: int) : int = x")
                    "an ordinary binding owes the gate nothing"
            }

            test "a target with no module system refuses the obligation at the gate" {
                // The CLR driver gates through `RuntimeModules.unsupported`, so an
                // `[<Import>]` binding on that target is a positioned source error.
                let analysed =
                    analysedSolo
                        "namespace V\n\nmodule M =\n\n    [<Import(\"served\", \"./Asset.mjs\")>]\n    let served (x: int) : int = nativeOnly"

                match AnalysedAssembly.gate RuntimeModules.unsupported analysed with
                | Ok _ -> failtest "the gate must refuse the undischargeable import"
                | Error diags ->
                    let d = Expect.wantSome (List.tryExactlyOne diags) "one finding"

                    Expect.stringContains
                        d.Diagnostic.Message
                        "no runtime module system"
                        "the target serves no [<Import>]"

                    Expect.equal (AssemblyFileId.toStored d.Path) "solo.fs" "positioned in the declaring .fs"
                    Expect.isGreaterThan d.Line 1 "at the binding, not the file head"
            }
        ]

// ---- Semantic typar-order conformance ---------------------------------
// The `.fs`-inferred scheme vs the `.fsi`-declared one, both under the binding's
// `ModuleFunction` scope: positional, so `=` fails on a typar-ORDER difference.

let private vesperSrcDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src")

/// Every `Vesper.*` package that builds for `target` → (dir name, its resolved manifest). A
/// package that publishes no `manifest.<target>.toml` does not build for it and is absent here.
let private packageManifests (target: string) : (string * ReferencedProject.ManifestPath) list =
    Directory.GetDirectories(vesperSrcDir, "Vesper.*")
    |> Array.choose (fun d ->
        match ReferencedProject.resolveManifest target d with
        | Ok mp -> Some(Path.GetFileName d, mp)
        | Error _ -> None
    )
    |> Array.sortBy fst
    |> List.ofArray

let private manifestOf (target: string) (package: string) : ReferencedProject.ManifestPath =
    packageManifests target
    |> List.tryFind (fun (p, _) -> p = package)
    |> Option.map snd
    |> Option.defaultWith (fun () -> failtestf "%s %s manifest not found" package target)

/// A contract provider that publishes exactly `declared` and nothing else — the `.fsi` side
/// of one `checkFile` run.
let private contractProvider (declared: ExternalSymbol list) : IExternalSymbolProvider =
    TestHelpers.providerOfValues declared

/// Run the `.fs` through the real frozen self-host pipeline, so a generic binding's
/// typar order is inference's own rather than a hand-built `FrozenType`.
let private frozenOf (src: string) : FrozenPools =
    let lexed, file = parseFile src

    Pipeline.analyseFor
        {
            Name = AssemblyName "M"
            Target = "clr"
        }
        realProvider.Value
        (LexedFile.ofText lexed)
        file

/// The binding `f` of the implicit namespace, the scope its typars are written under.
let private fKey = SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") "f"

/// `f`'s typar `i`.
let private fTypar (i: int) : FrozenType =
    FTTypar(TyparScope.ModuleFunction fKey, i)

/// `val f: 'a -> 'b -> 'b` — the `.fsi` appearance-order scheme (`'a` = index 0).
let private fScheme: FrozenType = FTFun(fTypar 0, FTFun(fTypar 1, fTypar 1))

[<Tests>]
let typarConformanceTests =
    testList
        "TyparConformance"
        [
            // ---- Kernel: structural equality under one scope ----

            test "schemesAgree: same order → agree" {
                let declared = FTFun(fTypar 0, fTypar 1)
                let inferred = FTFun(fTypar 0, fTypar 1)
                Expect.isTrue (ConformanceTypars.schemesAgree declared inferred) "order agrees"
            }

            test "schemesAgree: swapped typar order → disagree" {
                let declared = FTFun(fTypar 0, fTypar 1)
                let inferred = FTFun(fTypar 1, fTypar 0)
                Expect.isFalse (ConformanceTypars.schemesAgree declared inferred) "reversed order disagrees"
            }

            // ---- Driver over the real frozen pipeline ----

            test "declared `<'b,'a>` reorder vs `.fsi` appearance order → TyparMismatch" {
                // `.fs` declares `<'b,'a>`, so `'b` = Method 0, `'a` = Method 1 ⇒ the
                // inferred scheme is `'a -> 'b -> 'b` = `M1 -> M0 -> M0`, the REVERSE
                // positional skeleton of the `.fsi`'s `'a -> 'b -> 'b` = `D0 -> D1 -> D1`.
                let contract =
                    contractProvider
                        [
                            ExternalSymbols.scheme
                                (SymbolKeyOps.inNamespace "")
                                "f"
                                fScheme
                                (FunctionScheme.unconstrained 2<typeSlot>)
                        ]

                let tast = frozenOf "let f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let mismatches = ConformanceTypars.checkFile contract tast
                Expect.equal (List.length mismatches) 1 "one typar-order mismatch"
                Expect.equal mismatches.Head.Name "f" "the mismatch names f"
            }

            test "appearance-order impl conforms to `.fsi` appearance order → no mismatch" {
                // No explicit `<…>`: the canonical order IS appearance order, matching the
                // `.fsi`. The very same binding+contract that fails above now conforms.
                let contract =
                    contractProvider
                        [
                            ExternalSymbols.scheme
                                (SymbolKeyOps.inNamespace "")
                                "f"
                                fScheme
                                (FunctionScheme.unconstrained 2<typeSlot>)
                        ]

                let tast = frozenOf "let f (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                Expect.isEmpty (ConformanceTypars.checkFile contract tast) "appearance-order impl conforms"
            }

            test "a binding the contract does not publish is skipped (presence is a separate check)" {
                // An empty contract: a private/unpublished binding has no declared scheme to
                // compare — typar-order is not the presence check's job.
                let tast = frozenOf "let f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty (ConformanceTypars.checkFile (contractProvider []) tast) "unpublished binding skipped"
            }

            // ---- `inline`: the declared typars ARE the splice's substitution slots ----

            test "an inline binding's typar order is checked: `<'b,'a>` reorder → TyparMismatch" {
                let contract =
                    contractProvider
                        [
                            ExternalSymbols.scheme
                                (SymbolKeyOps.inNamespace "")
                                "f"
                                fScheme
                                (FunctionScheme.unconstrained 2<typeSlot>)
                        ]

                let tast = frozenOf "let inline f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let mismatches = ConformanceTypars.checkFile contract tast
                Expect.equal (List.length mismatches) 1 "one typar-order mismatch"
                Expect.equal mismatches.Head.Name "f" "the mismatch names f"
            }

            test "an inline binding in appearance order conforms → no mismatch" {
                let contract =
                    contractProvider
                        [
                            ExternalSymbols.scheme
                                (SymbolKeyOps.inNamespace "")
                                "f"
                                fScheme
                                (FunctionScheme.unconstrained 2<typeSlot>)
                        ]

                let tast = frozenOf "let inline f (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                Expect.isEmpty (ConformanceTypars.checkFile contract tast) "appearance-order inline impl conforms"
            }

            test "an inline body folding two declared typars into one → TyparMismatch" {
                // The miscompile species: the contract's `'a -> 'b -> 'a` is TWO substitution
                // slots, and a homogeneous body offers one, so a call site would bind the
                // second argument at the first's type. Nothing else catches this.
                let contract =
                    contractProvider
                        [
                            ExternalSymbols.scheme
                                (SymbolKeyOps.inNamespace "")
                                "f"
                                (FTFun(fTypar 0, FTFun(fTypar 1, fTypar 0)))
                                (FunctionScheme.unconstrained 2<typeSlot>)
                        ]

                let tast = frozenOf "let inline f (x: 'a) (y: 'a) : 'a = x"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let mismatches = ConformanceTypars.checkFile contract tast
                Expect.equal (List.length mismatches) 1 "the folded body disagrees with the contract"
                Expect.equal mismatches.Head.Name "f" "the mismatch names f"
            }
        ]

// ---- The units a package COMPILES ------------------------------------------
// Pairing is what carries the conformance and typar checks INTO the assembly pipeline.

/// Each unit as `(signature, implementation)` relative paths, the signature `""` when the
/// body carries none.
let private unitPaths (units: AssemblyFiles.AssemblyUnit list) : (string * string) list =
    [
        for u in units do
            match u with
            | AssemblyFiles.AssemblyUnit.Analysable u ->
                yield
                    (match u.Signature with
                     | ValueSome s -> s.Id.Name
                     | ValueNone -> ""),
                    u.Implementation.Id.Name
            | AssemblyFiles.AssemblyUnit.Faulted(leading, _) ->
                failtestf "the manifest read did not deliver %s" leading.Id.Name
    ]

let private unitsOf (mp: ReferencedProject.ManifestPath) : AssemblyFiles.AssemblyUnit list =
    let sources =
        AssemblySources.ofManifest mp
        |> PackageFaults.okOrFail (sprintf "AssemblySources.ofManifest %s" mp.Path)

    sources.Units

[<Tests>]
let manifestUnitsTests =
    testList
        "ManifestUnits"
        [
            for target in [ "clr"; "js" ] do
                for package, manifestPath in packageManifests target do
                    test $"{package} ({target}): every body compiles under the signature file it pairs with" {
                        let manifest =
                            ReferencedProject.loadManifest manifestPath |> PackageFaults.okOrFail package

                        let units = unitsOf manifestPath

                        Expect.equal
                            (unitPaths units |> List.map snd)
                            (ReferencedProject.implementationFiles manifest)
                            "one unit per implementation entry, in manifest order"

                        let paired = unitPaths units |> List.filter (fst >> (<>) "") |> List.sort

                        let expected =
                            [
                                for u in manifest.Units do
                                    match u.Signature with
                                    | ValueSome s -> yield s.Relative, u.Implementation.Relative
                                    | ValueNone -> ()
                            ]
                            |> List.sort

                        Expect.equal paired expected "the manifest parse's pairing, and no other"
                    }

            // The loop above passes vacuously on a package whose bodies pair with nothing, so
            // one real corpus pair is pinned below: `.fsi` first, at the `.fs`'s own position.
            test "Vesper.Core (clr): prim-types-min is compiled as one two-halved unit" {
                let paths = unitPaths (unitsOf (manifestOf "clr" "Vesper.Core"))

                Expect.equal
                    (List.head paths)
                    ("prim-types-min.fsi", "prim-types-min.clr.fs")
                    "the first body of the package carries its contract"

                Expect.isEmpty
                    (paths |> List.filter (fun (s, _) -> s = ""))
                    "every Vesper.Core body pairs with a signature file"
            }
        ]
