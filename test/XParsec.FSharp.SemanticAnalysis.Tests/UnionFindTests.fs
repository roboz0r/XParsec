module XParsec.FSharp.SemanticAnalysis.Tests.UnionFindTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

[<Tests>]
let tests =
    testList
        "UnionFind"
        [
            test "fresh TypeVar is its own root" {
                let store = TypeStore()
                let tv = store.NewTypeVar()
                Expect.isTrue ((UnionFind.find store tv).Id = tv) "self-root"
            }

            test "union puts two vars in the same class" {
                let store = TypeStore()
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                UnionFind.union store a b
                Expect.isTrue (UnionFind.inSameClass store a b) "a and b unified"
            }

            test "union is transitive" {
                let store = TypeStore()
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                UnionFind.union store a b
                UnionFind.union store b c
                Expect.isTrue (UnionFind.inSameClass store a c) "a and c transitively unified"
            }

            test "unrelated vars stay in different classes" {
                let store = TypeStore()
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                UnionFind.union store a b
                Expect.isFalse (UnionFind.inSameClass store a c) "c isolated"
                Expect.isFalse (UnionFind.inSameClass store b c) "c isolated"
            }

            test "path compression flattens chains" {
                let store = TypeStore()
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                let d = store.NewTypeVar()
                // Wire by hand to bypass rank-based union's auto-balancing.
                store.SetParent(b, ValueSome a)
                store.SetParent(c, ValueSome b)
                store.SetParent(d, ValueSome c)

                let root = UnionFind.find store d
                Expect.isTrue (root.Id = a) "found a"
                Expect.equal (store.Parent d) (ValueSome a) "d compressed"
                Expect.equal (store.Parent c) (ValueSome a) "c compressed"
                Expect.equal (store.Parent b) (ValueSome a) "b compressed"
            }

            test "rank-based union: smaller tree hangs off larger" {
                let store = TypeStore()
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                UnionFind.union store a b
                UnionFind.union store a c

                let rootC = UnionFind.find store c
                Expect.isTrue (rootC.Id = a) "c's root is a"
                Expect.equal (store.Rank a) 1 "rank stays 1"
            }
        ]
