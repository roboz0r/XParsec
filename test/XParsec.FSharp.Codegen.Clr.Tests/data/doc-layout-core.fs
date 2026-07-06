//#include _layout-core.fs
let listDoc =
    LGroup(
        LCat
            [
                LText "["
                LNest(
                    2,
                    LCat
                        [
                            LLine ""
                            LText "1"
                            LText ";"
                            LLine " "
                            LText "2"
                            LText ";"
                            LLine " "
                            LText "3"
                        ]
                )
                LLine ""
                LText "]"
            ],
        false
    )

let appDoc = LGroup(LCat [ LText "Some"; LLine " "; LText "1" ], true)
printfn "%s" (layout listDoc 80)
printfn "%s" (layout listDoc 0)
printfn "%s" (layout listDoc 5)
printfn "%s" (layout appDoc 80)
