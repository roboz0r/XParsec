module Test

type Env = { ShowAll: bool; Verbose: bool }

// Record update expression with trailing semicolon (before closing })
// From NicePrint.fs line 2773: { denv with showMemberContainers=true; }

let withAll e =
    { e with ShowAll = true; Verbose = true; }

let processEnv denv f =
    f { denv with ShowAll = true; } 42
