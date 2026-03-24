module Test

// Exception with member overrides
exception WrappedError of exn * string with
    override this.Message =
        match this :> exn with
        | WrappedError(_, msg) -> msg
        | _ -> "unknown"

// Simple exception (should still work)
exception SimpleError of string
