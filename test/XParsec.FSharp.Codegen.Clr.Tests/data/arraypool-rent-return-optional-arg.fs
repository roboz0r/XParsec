open System.Buffers
let pool = ArrayPool<char>.Shared
let a = pool.Rent(256)
let b = ArrayPool<char>.Shared.Rent(512)
let ok = a.Length >= 256 && b.Length >= 512
pool.Return(a)
ArrayPool<char>.Shared.Return(b)
printfn "%s" (if ok then "ok" else "no")
