module XParsec.FSharp.SemanticAnalysis.Tests.CitationTests

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Expecto
open XParsec.FSharp.SemanticAnalysis

// A citation is prose, so deleting the member it names does not break it. This guards that
// one bug class: a backticked `T.X` for a guarded `T` must name a field, case, or module
// member that exists. The valid set is reflected off `T`, so there is no allowlist to update.

/// A bare `module M` as the `System.Type` it compiles to — a module has no `typeof<_>`.
/// A miss fails loudly: answering `null` would drop the module's members from the valid set
/// and report every citation of one as dangling.
let private moduleType (name: string) : Type =
    let fullName = "XParsec.FSharp.SemanticAnalysis." + name

    match typeof<FrozenPools>.Assembly.GetType fullName with
    | null -> failtestf "no type %s — this lookup, not the prose, is stale" fullName
    | t -> t

/// The guarded citation prefixes, each paired with the type whose members DEFINE its valid
/// set. A function, not a value: a stale-lookup failure raised from a module initializer
/// would surface as a type-init error at test DISCOVERY, not as this test's own message.
let private guarded () : (string * Type) list =
    [
        "FrozenPools", typeof<FrozenPools>
        "ExprPayload", typeof<ExprPayload>
        "PatPayload", typeof<PatPayload>
        "DeclPayload", typeof<DeclPayload>
        "ExprRow", typeof<ExprRow>
        "PatRow", typeof<PatRow>
        "DeclRow", typeof<DeclRow>
        "BoundVarNaming", typeof<BoundVarNaming>
        "TastWalk", moduleType "TastWalk"
        "TastPools", moduleType "TastPools"
        "TastPoolShapes", moduleType "TastPoolShapes"
        "TastUnpool", moduleType "TastUnpool"
        "TastPoolBuilder", moduleType "TastPoolBuilder"
        "TastConvert", moduleType "TastConvert"
        "TastLower", moduleType "TastLower"
        "Inline", moduleType "Inline"
        "ArgGroups", moduleType "ArgGroups"
        // `BoundVarKey` abbreviates `BoundVarKeyG<NodeKey>`; an abbreviation still clashes
        // with the module's own name, so the module compiles with the `Module` suffix.
        "BoundVarKey", moduleType "BoundVarKeyModule"
        // Likewise `Anchor`: the type owns the bare name, so its module takes the suffix.
        "Anchor", moduleType "AnchorModule"
    ]

let private isFSharpModule (t: Type) =
    t.GetCustomAttributes(typeof<CompilationMappingAttribute>, false)
    |> Array.exists (fun a -> (a :?> CompilationMappingAttribute).SourceConstructFlags = SourceConstructFlags.Module)

/// The name a module was DECLARED under: F# compiles a `module X` that shares its name with
/// a type as `XModule` (`CompilationRepresentationFlags.ModuleSuffix`).
let private moduleSourceName (t: Type) =
    if t.Name.EndsWith("Module", StringComparison.Ordinal) then
        t.Name.Substring(0, t.Name.Length - "Module".Length)
    else
        t.Name

/// The companion module of `t`, or `ValueNone` when `t` declares none (`ExprRow`, `PatRow`,
/// `DeclRow`). A companion compiled under an unexpected name fails loudly: answering "no
/// members" would report every citation of one as dangling, blaming the prose for this bug.
let private companionModule (t: Type) : Type voption =
    let asm = t.Assembly

    // A module has no companion: the search below would match it against itself and read
    // that as a stale lookup.
    if isFSharpModule t then
        ValueNone
    else

        match asm.GetType(t.FullName + "Module") with
        | null ->
            match
                asm.GetTypes()
                |> Array.tryFind (fun m -> isFSharpModule m && moduleSourceName m = t.Name)
            with
            | Some m ->
                failtestf
                    "the companion module of %s is compiled as %s, not %sModule — this lookup, not the prose, is stale"
                    t.Name
                    m.FullName
                    t.FullName
            | None -> ValueNone
        | m -> ValueSome m

/// The names an F# ENTITY spells behind its own qualifier: its values, functions, and the
/// types nested in it. An active pattern compiles under its whole bracketed name
/// (`|EApp|_|`) but prose cites the CASE (`EApp`), so the cases are unpacked out of it.
let private ownMembers (t: Type) : string[] =
    let unpackActivePattern (name: string) =
        if name.StartsWith("|", StringComparison.Ordinal) then
            name.Split('|') |> Array.filter (fun s -> s <> "" && s <> "_")
        else
            [| name |]

    // Non-public too: prose cites a `private` helper by name like any other.
    let anyVisibility =
        BindingFlags.Public
        ||| BindingFlags.NonPublic
        ||| BindingFlags.Static
        ||| BindingFlags.Instance

    Array.concat
        [
            t.GetMethods anyVisibility
            |> Array.collect (fun x -> unpackActivePattern x.Name)
            t.GetProperties anyVisibility |> Array.map (fun x -> x.Name)
            t.GetNestedTypes anyVisibility |> Array.map (fun x -> x.Name)
        ]

/// The names a `` `T.X` `` citation may carry: `T`'s record fields, its union cases, and the
/// members of its companion module — or its own, when `T` IS a module. As prose spells them:
/// `FrozenPools.ExprToks`, `ExprPayload.Lambda`, `TastWalk.declBoundVars`.
let private validMembers (t: Type) : Set<string> =
    // A non-public representation would otherwise reflect as an empty field/case set.
    let anyVisibility = BindingFlags.Public ||| BindingFlags.NonPublic

    let recordFields =
        if Reflection.FSharpType.IsRecord(t, anyVisibility) then
            Reflection.FSharpType.GetRecordFields(t, anyVisibility)
            |> Array.map (fun p -> p.Name)
        else
            [||]

    let unionCases =
        if Reflection.FSharpType.IsUnion(t, anyVisibility) then
            Reflection.FSharpType.GetUnionCases(t, anyVisibility)
            |> Array.map (fun c -> c.Name)
        else
            [||]

    let selfMembers = if isFSharpModule t then ownMembers t else [||]

    let companionMembers =
        match companionModule t with
        | ValueNone -> [||]
        | ValueSome m -> ownMembers m

    Set.ofArray (Array.concat [ recordFields; unionCases; selfMembers; companionMembers ])

/// Only BACKTICKED citations: an unquoted `T.` is real code the compiler already checks. Two
/// look-aheads exclude the shapes that do not cite a member: a file (`TastPools.fs`) and a column
/// family (`FrozenPools.Expr*`, the parallel columns rather than one of them).
let private citationRegex (guards: (string * Type) list) =
    guards
    |> List.map (fst >> Regex.Escape)
    |> String.concat "|"
    |> sprintf "`(%s)\\.(?!fs`)([A-Za-z_][A-Za-z0-9_]*)(?![A-Za-z0-9_*])"
    |> Regex

[<Tests>]
let tests =
    testList
        "Citations"
        [
            test "every backticked citation of a guarded type names a member that exists" {
                let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
                let srcRoot = Path.Combine(repoRoot, "src")

                let guards = guarded ()
                let citation = citationRegex guards

                let members =
                    guards |> List.map (fun (prefix, t) -> prefix, validMembers t) |> Map.ofList

                let dangling =
                    Directory.EnumerateFiles(srcRoot, "*.fs", SearchOption.AllDirectories)
                    |> Seq.filter (fun f ->
                        let rel = Path.GetRelativePath(srcRoot, f).Replace('\\', '/')
                        not (rel.Contains "/obj/" || rel.Contains "/bin/")
                    )
                    |> Seq.collect (fun f ->
                        File.ReadAllLines f
                        |> Array.mapi (fun i line -> (i + 1, line))
                        |> Array.collect (fun (lineNo, line) ->
                            citation.Matches line
                            |> Seq.map (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
                            |> Seq.filter (fun (prefix, name) -> not (Set.contains name members.[prefix]))
                            |> Seq.map (fun (prefix, name) ->
                                sprintf "%s:%d cites `%s.%s`" (Path.GetRelativePath(repoRoot, f)) lineNo prefix name
                            )
                            |> Seq.toArray
                        )
                    )
                    |> List.ofSeq

                Expect.isEmpty
                    dangling
                    (sprintf
                        "citation(s) of a member that does not exist — it was renamed or deleted and the \
                         prose that explains it was left behind:\n%s"
                        (String.concat "\n" dangling))
            }
        ]
