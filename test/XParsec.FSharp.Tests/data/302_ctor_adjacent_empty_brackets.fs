module Test

// DU constructor applied to empty list [] with no space (adjacent brackets)
// From InnerLambdasToTopLevelFuncs.fs line 56: TreeNode[]

type Tree<'T> =
    | TreeNode of Tree<'T> list
    | LeafNode of 'T

let emptyTR = TreeNode[]

let singleLeaf x = TreeNode[ LeafNode x ]
