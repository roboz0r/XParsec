module Test

type Validator(bytes: byte[], length) =
    do
        if length < 0 || length > bytes.Length then
            raise (System.ArgumentOutOfRangeException("length"))

        if bytes.Length = 0 then
            raise (System.ArgumentException("bytes"))

    member _.Length = length
