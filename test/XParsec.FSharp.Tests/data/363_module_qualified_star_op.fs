// Module-qualified `(*)` (multiply) operator used as a first-class value in a
// tuple literal. The tricky bit is the lexer: `(*` normally opens a block
// comment, so `Module.(*)` needs to be parsed as `Module.` followed by
// parenthesized-operator `(*)`, NOT `Module.` followed by comment `(*` ... .
//
// Analogous to the `(=)` and `Module.(=)` cases handled by bugs 11 and 17,
// but for the star operator.
//
// Affects: TypedTreeOps.fs (line 10843), ilwrite.fs (possibly).

let checkedBinOps () =
    EvalArithBinOp (Checked.(*), Checked.(*), Checked.(*), Checked.(*))
