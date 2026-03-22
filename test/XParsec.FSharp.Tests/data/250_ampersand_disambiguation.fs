module AmpersandDisambiguation

// && is both infix (boolean and) and prefix (native address-of).
// Disambiguation: at lhsParser position (no LHS operand) it is prefix.
// In the Pratt infix loop (after a LHS expression) it is infix.

let f a b = a && b      // infix: boolean and
let g a b = a &&b       // infix: boolean and (no space before operand)
let h a = && a           // prefix: native address-of
