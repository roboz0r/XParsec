namespace XParsec.FSharp.SemanticAnalysis

/// What the declarations of a module PATH state, over this compilation and the assemblies it
/// references. A path carries one declaration per assembly (FS0248), and a reader either
/// merges them — `open` — or selects one by home — the class a declaration emits as.
module ModuleDeclarations =

    /// Record what `m`'s declaration in this file states. Idempotent: every pass re-enters the
    /// same scopes.
    let note (types: PassContextTypes) (m: ModuleKey) (facts: ModuleFacts) : unit = types.Modules.[m] <- facts

    let declaredInFile (types: PassContextTypes) : EqDict<ModuleKey, ModuleFacts> = EqDict.ofSeq types.Modules

    /// Every declaration of `m` in scope at `at`: this file's own, homed in `home`, where its
    /// name has entered by `at`, then each referenced surface's.
    let reaching
        (types: PassContextTypes)
        (scope: IScopeContents)
        (home: SymbolHome)
        (at: int)
        (m: ModuleKey)
        : EqArray<ModuleDeclaration> =
        let referenced = scope.DeclarationsOf m

        match
            types.Modules.TryGetValue m, ScopeResolution.tryLocalSubContainer types.LocalContainers m.Container m.Name
        with
        | (true, own), ValueSome local when local.VisibleFrom <= at ->
            EqArray.append (EqArray.singleton { Home = home; Facts = own }) referenced
        | _ -> referenced

    /// Is an `open` of `container` written at `at` refused? A namespace carries no
    /// `[<RequireQualifiedAccess>]` marker.
    let requiresQualifiedAccess
        (types: PassContextTypes)
        (scope: IScopeContents)
        (home: SymbolHome)
        (at: int)
        (container: ModuleContainer)
        : bool =
        match container with
        | ModuleContainer.InNamespace _ -> false
        | ModuleContainer.InModule m -> ModuleDeclaration.anyRefusesOpen (reaching types scope home at m)

    /// Does an earlier file of `assembly` already declare `m`? A second declaration of a path
    /// in one assembly is FS0248. An earlier file's view is homed in the file; a package view
    /// is homed in its assembly, and a package's contract of ITSELF counts as a reference.
    let declaredEarlierInAssembly (scope: IScopeContents) (assembly: AssemblyName) (m: ModuleKey) : bool =
        scope.DeclarationsOf m
        |> EqArray.exists (fun declaration ->
            match declaration.Home with
            | SymbolHome.InFile file -> file.Assembly = ValueSome assembly
            | SymbolHome.InAssembly _
            | SymbolHome.Unstamped -> false
        )
