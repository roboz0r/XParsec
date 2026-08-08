module XParsec.FSharp.SemanticAnalysis.Tests.CitationTests

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Expecto
open XParsec.FSharp.SemanticAnalysis

// The pool columns, the node payloads/rows they carry, and the modules that fill and unpool
// them are cited BY NAME throughout the SemanticAnalysis and codegen sources — the layout
// rationale in `TastPoolTypes.fs`, the ordering coupling in `TastPools.fs`, the layer
// arithmetic in `TastPoolBuilder.fs`. A citation is prose, so deleting what it names does
// not break it, and both halves of that have happened: `TastPoolBuilder`'s read-surface
// header went on naming `FrozenPools.ExprShapes` for the whole life of the branch that
// deleted that column, and six citations went on naming a `TastWalk` function after its
// last caller was gone.
//
// This is the guard for that one bug class, and deliberately only that one: a citation of
// `T.X` for a guarded `T` must name a field, case, or module member that exists. It is
// cheap because the valid set is reflected off `T` rather than restated, so a renamed or
// deleted member fails here with no allowlist to update.

/// A bare `module M`, as the `System.Type` it compiles to. A module has no `typeof<_>`, and
/// it is the case the guard exists FOR — the citation that went stale for a whole branch
/// named a module member — so it is resolved by name rather than left out.
///
/// A miss is LOUD, and must be: answering `null` would silently drop the module's members
/// from the valid set and report every citation of one as dangling.
let private moduleType (name: string) : Type =
    let fullName = "XParsec.FSharp.SemanticAnalysis." + name

    match typeof<FrozenPools>.Assembly.GetType fullName with
    | null -> failtestf "no type %s — this lookup, not the prose, is stale" fullName
    | t -> t

/// The guarded citation prefixes, each paired with the type whose members DEFINE its valid
/// set. Adding a guard is one line — nothing here restates what an entity carries, and a
/// module is a row like any other because a module IS a type.
///
/// A function, not a value: `moduleType` fails when a lookup goes stale, and a failure
/// raised from a module initializer surfaces as a type-init error at test DISCOVERY rather
/// than as this test failing with its own message.
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
        // Under its compiled name: `BoundVarKey` is the `BoundVarKeyG<NodeKey>` abbreviation,
        // which erases, and an abbreviation of the module's own name is still a clash — so
        // the module takes the `Module` suffix and no type answers to the bare name.
        "BoundVarKey", moduleType "BoundVarKeyModule"
        // Likewise: `Anchor` is the type, so its module takes the suffix. Guarding it is
        // what keeps a citation of a position convention honest now that the convention IS
        // the type's surface and nothing else.
        "Anchor", moduleType "AnchorModule"
    ]

let private isFSharpModule (t: Type) =
    t.GetCustomAttributes(typeof<CompilationMappingAttribute>, false)
    |> Array.exists (fun a -> (a :?> CompilationMappingAttribute).SourceConstructFlags = SourceConstructFlags.Module)

/// The name a module was DECLARED under: F# compiles `module X` that shares its name with
/// a type as `XModule` (`CompilationRepresentationFlags.ModuleSuffix`), which is the form
/// every guarded companion here takes.
let private moduleSourceName (t: Type) =
    if t.Name.EndsWith("Module", StringComparison.Ordinal) then
        t.Name.Substring(0, t.Name.Length - "Module".Length)
    else
        t.Name

/// The companion module of `t`, or `ValueNone` when `t` genuinely declares none
/// (`ExprRow`/`PatRow`/`DeclRow`).
///
/// A companion that exists but fails to resolve is a LOUD failure rather than an empty
/// member set: answering "no members" to a moved or renamed module would shrink the valid
/// set to the type's own fields and report every module-member citation as dangling —
/// blaming the prose for a fault in this lookup. So a miss is confirmed against the
/// assembly's actual module set before it is believed.
let private companionModule (t: Type) : Type voption =
    let asm = t.Assembly

    // A module has no companion: its members are its OWN (`ownMembers`), and the search
    // below would otherwise match it against itself and read that as a stale lookup.
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
/// types nested in it. Both the module rows and the companion modules of the type rows go
/// through this, which is what keeps a module an ordinary row instead of a derivation.
///
/// An active pattern is compiled under its whole bracketed name (`|EApp|_|`), but prose
/// cites the CASE (`TastAccessor.EApp`), so the cases are unpacked out of it.
let private ownMembers (t: Type) : string[] =
    let unpackActivePattern (name: string) =
        if name.StartsWith("|", StringComparison.Ordinal) then
            name.Split('|') |> Array.filter (fun s -> s <> "" && s <> "_")
        else
            [| name |]

    // Non-public members too, for the reason `validMembers` states of the record/union
    // representation: prose cites a helper by name whether or not it is `private`, and a
    // visibility filter here would report every citation of one as dangling.
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

/// The names a `` `T.X` `` citation may carry: `T`'s record fields, its union case names,
/// and — whether `T` is itself a module or merely has a companion one — that module's own
/// members. These are the things prose spells with the same `T.` qualifier
/// (`FrozenPools.ExprToks`, `ExprPayload.Lambda`, `FrozenPools.typarArity`,
/// `TastWalk.declBoundVars`). Derived off the type for every guard alike, so a guard is a row
/// in the table and never a derivation of its own.
let private validMembers (t: Type) : Set<string> =
    // Reflect a non-public representation too: an accessibility that hid the shape would
    // otherwise yield an empty set, and prose is cited by name regardless of visibility.
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

/// Only BACKTICKED citations: an unquoted `T.` is real code, which the compiler already
/// checks. Two look-aheads carve out the forms that share this shape without naming a
/// member:
///
///   * a FILE (`TastPools.fs`) — a module's prose names its own file as often as its
///     members, and the file exists whether or not a member called `fs` does;
///   * a column FAMILY (`FrozenPools.Expr*`) — that names the parallel columns, not one of
///     them, so the wildcard must not be read as a name.
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

                // The offenders are IN the message: a citation is prose, so the only way to
                // act on this failure is to be told which line to read.
                Expect.isEmpty
                    dangling
                    (sprintf
                        "citation(s) of a member that does not exist — it was renamed or deleted and the \
                         prose that explains it was left behind:\n%s"
                        (String.concat "\n" dangling))
            }
        ]
