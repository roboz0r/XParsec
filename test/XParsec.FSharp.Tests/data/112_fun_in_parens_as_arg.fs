module M

let pLessThan =
    parser {
        let! state = getUserState
        return! nextFn (fun t -> t.Token = Token.OpLessThan && check "<" t state) "<"
    }
