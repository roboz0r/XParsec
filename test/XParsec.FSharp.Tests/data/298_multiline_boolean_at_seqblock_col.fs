module Test

// Multi-line boolean expression where the string literal RHS appears on the
// next line after a line comment within an inner paren context.
// From PrettyNaming.fs lines 719-733

let isInfixOp1 (s: string) =
    let trimmed = s.TrimStart(' ')
    s <> trimmed
    && (s = ":="
        || s.StartsWith "|")

// This version has a mid-expression line comment, causing the RHS to be
// on the next line inside nested parens. PrettyNaming.fs line 720:
//   && ((s = // COLON_EQUALS
//       ":=")
let isInfixOp2 (s: string) =
    let trimmed = s.TrimStart(' ')
    s <> trimmed
    && ((s = // COLON_EQUALS
        ":=")
        || s.StartsWith "|"
        || s.StartsWith "&")
