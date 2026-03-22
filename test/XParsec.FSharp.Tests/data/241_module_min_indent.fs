module ModuleMinIndentation

// ============================================================
// MODULE BODY
// Must be at `module_col + 1` minimum.
// ============================================================

module M =
 let x = 1           // Depends on `module` indent + 1

module Outer =
    module Inner =
     let x = 1       // Depends on inner `module` indent + 1
