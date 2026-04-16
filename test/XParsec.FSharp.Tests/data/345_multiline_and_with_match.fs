// Parenthesized expression whose right side of `&&` is an unparenthesized
// multi-line `match`, whose clause bodies are themselves inline nested
// `match` expressions. Also exercises `elif` with multi-line condition.
// Affects: LowerStateMachines.fs, TypedTreeBasics.fs
type V<'T> = N | S of 'T
type R = { NLR: int; TryDeref: V<int> }

let nonLocalRefEq (a: int) (b: int) = a = b
let nonLocalRefDefinitelyNotEq (a: int) (b: int) = a <> b

let eq (x: R) (y: R) =
    if x.NLR = y.NLR then
        true
    elif not (nonLocalRefDefinitelyNotEq x.NLR y.NLR) &&
        (// paths may be equal
         nonLocalRefEq x.NLR y.NLR ||
         // otherwise dereference
         (not (nonLocalRefDefinitelyNotEq x.NLR y.NLR) &&
            match x.TryDeref with
            | S v1 -> match y.TryDeref with S v2 -> v1 = v2 | _ -> false
            | _ -> match y.TryDeref with N -> true | _ -> false)) then
        true
    else
        false
