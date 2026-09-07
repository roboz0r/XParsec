module XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyseParsed (input: string) (lexed, file) =
    let ctx = PassContext(realProvider.Value, LexedFile.ofText lexed, testCompiling)

    NameResolution.run ctx file
    Unification.run ctx file
    ctx

let analyse (input: string) = analyseParsed input (parseFile input)

/// `analyse` for a source the parser can only complete by PATCHING it: what the passes
/// then say about the patched tree is the assertion.
let analyseRecovered (input: string) =
    analyseParsed input (parseRecoveredFile input)

let unitFt = FTConst(RuntimeNames.unitKey, Block.empty)
let intFt = FTConst(RuntimeNames.intKey, Block.empty)
let stringFt = FTConst(RuntimeNames.stringKey, Block.empty)

// A candidate `static C.M(paramFts)`; candidates differ only in their parameters.
let overloadMember (paramFts: FrozenType list) (methodTyparArity: int) : ExternalMember =
    let parameters =
        match paramFts with
        | [] -> unitFt
        | [ p ] -> p
        | many -> FTTuple(Block.ofList many)

    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf
              (SymbolKeyOps.qualifiedTypeKeyOf "C" 0)
              "M"
              (Block.ofList paramFts)
              methodTyparArity
              MemberKind.Method
      ) with
        IsStatic = true
        Signature = TestHelpers.mkSignature 0 methodTyparArity parameters unitFt
    }

let classMember (paramTy: SemType) : ExternalMember = overloadMember [ toFrozen paramTy ] 0

// A candidate `Box<'a>.M(paramFt)` — instance, one declaring typar.
let boxMember (paramFt: FrozenType) : ExternalMember =
    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf
              (SymbolKeyOps.qualifiedTypeKeyOf "Box" 1)
              "M"
              (Block.singleton paramFt)
              0
              MemberKind.Method
      ) with
        IsStatic = false
        Signature = TestHelpers.mkSignature 1 0 paramFt unitFt
    }

let chosenParamsWith (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
    UnificationInferOverload.memberParamTypes (analyse "") typeArgs m

let pickWith
    (ctx: PassContext)
    (typeArgs: SemType[])
    (candidates: Block<ExternalMember>)
    (args: SemType list)
    : ExternalMember voption =
    UnificationInferOverload.pickBestOverload ctx typeArgs candidates args

let chosenParams (m: ExternalMember) : SemType list =
    UnificationInferOverload.memberParamTypes (analyse "") [||] m

let grandBaseTy = TyClass("GrandBase", Block.empty)
let baseTy = TyClass("Base", Block.empty)
let derivedTy = TyClass("Derived", Block.empty)

// A FRESH ctx per call: `PassContext` is mutable and these tests run in parallel.
let overloadCtx () = analyse "let _ = 0"

let hierCtx () =
    analyse
        "type GrandBase() =\n    member this.G = 1\ntype Base() =\n    inherit GrandBase()\n    member this.B = 1\ntype Derived() =\n    inherit Base()\n    member this.D = 1"

let keyOfLet (input: string) (name: string) =
    NodeKey.ofSource (input.IndexOf("let " + name + " ") + 4) NodeKind.PatIdent

let errors (ctx: PassContext) =
    ctx.Diagnostics |> Diagnostic.errors |> Seq.toList

// The domain of `let f (x: …) = …`: the parameter annotation as translation canonicalised
// it, with no call type-checking in the loop.
let unionDomainOf (input: string) : SemType =
    let ctx = analyse input
    let patKey = NodeKey.ofSource (input.IndexOf "f ") NodeKind.PatIdent

    match typeOf ctx patKey with
    | TyFun(dom, _) -> dom
    | other -> failtestf "expected f : _ -> _, got %A" other

let subsumeCtx () = analyse "let x = 1"
let intTy = BuiltinTypes.tyInt
let strTy = BuiltinTypes.tyString
let boolTy = BuiltinTypes.tyBool

let checkConstraintKind ctx kind ty =
    let c: SemanticConstraint =
        {
            Kind = kind
            DeclKey = NodeKey.ofSource 0 NodeKind.PatIdent
        }

    UnificationConstraintCheck.checkConstraint ctx c ty

let hasUnionExhaustivenessWarning (ctx: PassContext) =
    ctx.Diagnostics
    |> Seq.exists (fun d -> d.Severity = Severity.Warning && d.Message.Contains "anonymous union")

// Every pass plus the final freeze; the assertions read a decl tree, so re-author one
// from the pools the freeze produces.
let freezeDecls (input: string) : Pooled.TastFile =
    let lexed, file = parseFile input
    TastUnpool.ofPools (Pipeline.analyseFor testCompiling realProvider.Value (LexedFile.ofText lexed) file)

let frozenLetTy (file: Pooled.TastFile) : FrozenType =
    file.Decls
    |> Block.toList
    |> List.tryPick (fun d ->
        match d with
        // `let f x = …` and `let v = …` both freeze to `Let`.
        | TDeclG.Let(binding = m) -> Some m.Ty
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failtest "expected a frozen `let` decl")
