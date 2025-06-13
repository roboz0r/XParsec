module XParsec.CLArgs.Git

open System

open XParsec
open XParsec.Parsers
open XParsec.CLArgs

type GitInit =
    | Quiet
    | Bare
    | Template of string
    | SeparateGitDir of string
    | ObjectFormat of string
    | RefFormat of string
    | InitialBranch of string
    | Shared
    | Directory of string

let init =
    [
        CLFlag("quiet", Quiet, Some 'q', "Suppress all output")
        CLFlag("bare", Bare, None, "Create a bare repository")
        CLSetting("template", Template, None, RequireEqualsAssignment, "Use the specified template directory")
        CLSetting(
            "separate-git-dir",
            SeparateGitDir,
            None,
            RequireEqualsAssignment,
            "Use the specified directory for the git repository"
        )
        CLSetting("object-format", ObjectFormat, None, RequireEqualsAssignment, "Set the object format")
        CLSetting("ref-format", RefFormat, None, RequireEqualsAssignment, "Set the reference format")
        CLSetting("initial-branch", InitialBranch, Some 'b', RequireEqualsAssignment, "Set the initial branch name")
        CLFlag("shared", Shared, None, "Create a shared repository")
        CLDefaultSetting("directory", Directory, "Set the working directory")
    ]
    |> CLParser.command [ "init" ] "Create an empty Git repository or reinitialize an existing one" id
