module DelimiterMinIndentation

// ============================================================
// ALL DELIMITER TYPES: ( ), [ ], { }, [| |], begin/end
// Content inside delimiters: same offside line as enclosing context.
// Delimiters do NOT suppress the enclosing offside line.
// Closing delimiter: can undent past the enclosing offside line.
// ============================================================

// --- Parentheses ---

module ParenTests =

    // Content at let_col + 1 (minimum)
    let parenContent =
        (
     1
        )

    // Closing ) can undent to module_col
    let parenClose =
        (
            1
   )

// --- Square brackets ---

module BracketTests =

    // Content at let_col + 1 (minimum)
    let bracketContent =
        [
     1
        ]

    // Closing ] can undent past let_col
    let bracketClose =
        [
            1
   ]

// --- Braces ---

type R = { X: int }

module BraceTests =

    // Content at let_col + 1 (minimum)
    let braceContent =
        {
     X = 1
        }

    // Closing } can undent past let_col
    let braceClose =
        {
            X = 1
   }

// --- Array brackets ---

module ArrayTests =

    // Content at let_col + 1 (minimum)
    let arrayContent =
        [|
     1
        |]

    // Closing |] can undent past let_col
    let arrayClose =
        [|
            1
   |]

// --- begin/end ---

module BeginEndTests =

    // Content at let_col + 1 (minimum)
    let beginContent =
        begin
     1
        end

    // end can undent past let_col
    let beginClose =
        begin
            1
   end
