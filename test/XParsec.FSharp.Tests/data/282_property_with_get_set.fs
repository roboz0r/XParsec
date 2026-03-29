module Test

type DiagnosticsThreadStatics =
    static member BuildPhase
        with get () = 42
        and set v = ()

    static member Name
        with get () = "hello"
        and set (v: string) = ()
