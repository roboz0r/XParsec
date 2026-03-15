---
name: xparsec-dev
description: Use this when you need to build the project, run tests, or format F# code using Fantomas in the XParsec repository.
---

# XParsec Everyday Development Tooling

You are operating on a Windows machine in the `XParsec` repository. To perform development tasks safely, you MUST use the local wrapper script (`./claude_tools.cmd`).

- **DO NOT** use raw `dotnet` commands. They will trigger permission blocks or flood your context window.
- **DO** use the **serena** MCP to search within the repository and navigate the codebase efficiently.

## Available Actions

### 1. Building the Project
To compile the entire repository:
```bash
./claude_tools.cmd -Action Build
```

To compile a specific library project and isolate compilation errors, use the `-SourceProject` parameter:
```bash
./claude_tools.cmd -Action Build -SourceProject "XParsec.FSharp"
```

**Valid source projects are:**
- `XParsec`
- `XParsec.FSharp`

### 2. Running Tests

To run a test suite, you must use the `Test` action and specify the exact test project.
*Note: If you want to focus a specific test, do not use CLI filters. Instead, use your file editing tools to change the test definition in the source code from `test` to `ftest` (focused) before running.*

Example running the FSharp tests:

```bash
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Tests"
```

**Fixing Failing Golden/Snapshot Tests:**
If a test fails because the AST or lexed output changed intentionally, you can force the test runner to overwrite the golden files by adding the `-UpdateSnapshots` flag:
```bash
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Tests" -UpdateSnapshots
```

**Valid test projects are:**

- `XParsec.FSharp.Tests`
- `XParsec.Tests`

### 3. Formatting F# Code

To format the F# code across the repository using Fantomas:

```bash
./claude_tools.cmd -Action Format
```

## Logging and Debugging

Every time you run `claude_tools.cmd`, the **complete, unfiltered output** of the underlying command is automatically written to `claude_tools_output.log` in the repository root.

- To save your context window, the script restricts test outputs in the terminal to the last 30 lines (the summary).
- If tests fail or code fails to compile and the truncation hides the actual stack trace or compiler error, **DO NOT run the command again.**
- Instead, immediately use your native `Read` tool to open `claude_tools_output.log` to investigate the failure.
