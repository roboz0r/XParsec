// decimal — the width NEITHER backend supports, and the one that proves "rejected" is a
// real conformance state rather than the absence of one. There is no clause for it in
// either contract, so `+` falls to the SRTP trait call, fails to resolve (decimal is not
// a nominal with its own `static member (+)`), and the program is REJECTED. This program
// therefore has no golden: it is never run anywhere.
//
// It exists because a width with no clause and no program satisfies the clause-set guard
// vacuously — the guard can only check a width the manifest names. Naming it here is what
// stops a decimal clause being added later that emits garbage nobody judges.
let x = 1.5M + 2.5M
ignore x
