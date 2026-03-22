module Test

open System

// Simple object expression
let disposable =
    { new IDisposable with
        member _.Dispose() = () }

// Object expression with base call arguments
let comparer =
    { new System.Collections.Generic.Comparer<int>() with
        member _.Compare(x, y) = x - y }

// Object expression with interface implementations
let combined =
    { new IDisposable with
        member _.Dispose() = ()
      interface IComparable with
        member _.CompareTo(_) = 0 }

// Object expression with multiple members
let multi =
    { new IDisposable with
        member _.Dispose() = ()
        member x.ToString() = "hello" }

// Inline object expression in function call
let f (d: IDisposable) = ()
let test = f { new IDisposable with member _.Dispose() = () }
