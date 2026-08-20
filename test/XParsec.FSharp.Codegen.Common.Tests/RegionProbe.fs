/// Region verdicts read off a compiled front end. The Regions pass asks the TARGET how it lays
/// out every type it stamps, so a verdict over primitives is a fact about one backend: each
/// suite passes its own provider stack, and a compile composing no platform states none of them.
module XParsec.FSharp.Codegen.Common.Tests.RegionProbe

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes

/// One compile, queried many ways: the escape axis off the context, the representation axis
/// off what the pass returned, and both keyed by a binding pattern found in the CST.
type Probe =
    {
        Ctx: PassContext
        Regions: RegionVerdicts
        File: ImplementationFile<SyntaxToken>
    }

let analyse (provider: IExternalSymbolProvider) (input: string) : Probe =
    match ParseChain.parseUnrecovered Set.empty input with
    | Result.Error ds -> failwithf "parse failed: %A" (ds |> List.map (fun d -> d.Message))
    | Result.Ok parsed ->
        let ctx, regions, _ =
            Pipeline.analyseSemWithRegions provider (LexedFile.ofText parsed.Lexed) parsed.File

        {
            Ctx = ctx
            Regions = regions
            File = parsed.File
        }

let private moduleElems (file: ImplementationFile<SyntaxToken>) : ModuleElems<SyntaxToken> voption =
    match file with
    | ImplementationFile.AnonymousModule elems -> ValueSome elems
    | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> ValueSome elems
    | _ -> ValueNone

/// The bindings of the `i`th module element, if it is a `let` group.
let private letBindingsAt (elems: ModuleElems<SyntaxToken>) (i: int) : ImmutableArray<Binding<SyntaxToken>> voption =
    match elems.[i] with
    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) -> ValueSome bindings
    | _ -> ValueNone

/// The binding-pattern `NodeKey` of a module-level binding named `name`.
let patternKeyOf (p: Probe) (name: string) : NodeKey =
    let tryBindings (bindings: ImmutableArray<Binding<SyntaxToken>>) =
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < bindings.Length do
            match bindings.[i].pattern with
            | Pat.NamedSimple t when p.Ctx.NameOf t = name -> found <- ValueSome(CstKeys.ofPat bindings.[i].pattern)
            | _ -> ()

            i <- i + 1

        found

    let tryElems (elems: ModuleElems<SyntaxToken>) =
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < elems.Length do
            match letBindingsAt elems i with
            | ValueSome bindings -> found <- tryBindings bindings
            | ValueNone -> ()

            i <- i + 1

        found

    match moduleElems p.File |> ValueOption.bind tryElems with
    | ValueSome k -> k
    | ValueNone -> failwithf "binding %s not found at module level" name

/// The binding-pattern `NodeKey` of the first `let` named `name` reachable from `e`, through
/// binding RHSs, let bodies and lambda bodies — e.g. `let f x = x + 1` inside a function.
let rec private findLetKey (ctx: PassContext) (name: string) (e: Expr<SyntaxToken>) : NodeKey voption =
    match e with
    | Expr.LetOrUse(bindings = bs; body = body) ->
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < bs.Length do
            match bs.[i].pattern with
            | Pat.NamedSimple t when ctx.NameOf t = name -> found <- ValueSome(CstKeys.ofPat bs.[i].pattern)
            | _ -> found <- findLetKey ctx name bs.[i].expr

            i <- i + 1

        match found with
        | ValueSome _ -> found
        | ValueNone ->
            match body with
            | ValueSome b -> findLetKey ctx name b
            | ValueNone -> ValueNone
    | Expr.Fun(expr = body) -> findLetKey ctx name body
    | _ -> ValueNone

/// The first module-level binding, whose RHS and argument patterns the nested lookups walk.
let firstBinding (p: Probe) : Binding<SyntaxToken> =
    match moduleElems p.File |> ValueOption.bind (fun elems -> letBindingsAt elems 0) with
    | ValueSome bs -> bs.[0]
    | ValueNone -> failwith "expected a module-level let"

/// The binding-pattern `NodeKey` of a binding NESTED under the first module-level binding's RHS.
let nestedKeyOf (p: Probe) (name: string) : NodeKey =
    match findLetKey p.Ctx name (firstBinding p).expr with
    | ValueSome key -> key
    | ValueNone -> failwithf "binding %s not found" name

/// `None` = no entry, which is what a binding whose type the target lays out flat gets: no
/// region was ever minted for it.
let escapeAt (p: Probe) (key: NodeKey) : EscapeState option =
    match p.Ctx.Bindings.Escape.TryGetValue key with
    | ValueSome s -> Some s
    | ValueNone -> None

let reprAt (p: Probe) (key: NodeKey) : RegionRepr option =
    match p.Regions.Repr.TryGetValue key with
    | ValueSome r -> Some r
    | ValueNone -> None

/// The region a binding's type variable carries, `None` when none was stamped.
let regionAt (p: Probe) (key: NodeKey) : RegionId option =
    match p.Ctx.Bindings.TypeVar.TryGetValue key with
    | ValueSome tv ->
        let root = UnionFind.find p.Ctx.Store tv

        if (p.Ctx.Store.Region root.Id).Raw >= 0 then
            Some(p.Ctx.Store.Region root.Id)
        else
            None
    | ValueNone -> None

let escapeOf (p: Probe) (name: string) : EscapeState option = escapeAt p (patternKeyOf p name)

let escapeOfNested (p: Probe) (name: string) : EscapeState option = escapeAt p (nestedKeyOf p name)

let reprOf (p: Probe) (name: string) : RegionRepr option = reprAt p (patternKeyOf p name)

let reprOfNested (p: Probe) (name: string) : RegionRepr option = reprAt p (nestedKeyOf p name)
