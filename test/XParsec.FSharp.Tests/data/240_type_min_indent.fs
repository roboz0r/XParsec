module TypeMinIndentation

// ============================================================
// TYPE DEFINITION BODY
// Must be at `type_col + 1` minimum.
// Exception: DU `|` can appear at `type_col` (permitted undentation).
// ============================================================

// DU cases at type_col + 1
type DU1 =
 | X
 | Y

// DU `|` at type_col (permitted undentation)
type DU2 =
| X
| Y

// DU `|` at type_col (inside module)
module Nested =
    type DU3 =
    | X
    | Y

// Record `{` at type_col + 1
type R1 =
 { X: int; Y: int }

// Type alias at type_col + 1
type Alias1 =
 int

// Members at type_col + 1
type A() =
 member _.X = 1

// Member body at member_col + 1
module MemberNested =
    type B() =
        member _.X =
         1            // Depends on `member` indent + 1
