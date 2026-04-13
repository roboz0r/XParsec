// Object expression with multiple `interface X with` clauses and a
// member body containing deeply-nested if/elif/else returning values.
// seq.fs uses this idiom in its enumerator implementations.

open System
open System.Collections
open System.Collections.Generic

let makeEnumerator<'U> (items: 'U array) : IEnumerator<'U> =
    let mutable index = -1
    let finalIndex = items.Length - 1

    { new IEnumerator<'U> with
        member _.Current = items.[index]
      interface IEnumerator with
          member _.Current = box items.[index]

          member _.MoveNext() =
              if index = -2 then
                  false
              elif index = -1 then
                  index <- 0
                  true
              else
                  if index = Int32.MaxValue then
                      invalidOp "overflow"

                  if index = finalIndex then
                      false
                  else
                      index <- index + 1
                      true

          member _.Reset() =
              index <- -1
      interface IDisposable with
          member _.Dispose() =
              ()
    }
