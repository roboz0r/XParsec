type FrameKind =
    | Root
    | Group
    | Nest
    | CaseCollect

type Frame =
    val Kind: FrameKind
    val NestIndent: int
    val mutable Kids: LDoc list

    new(kind: FrameKind, nestIndent: int) =
        {
            Kind = kind
            NestIndent = nestIndent
            Kids = []
        }

type SemFrame =
    val Name: string
    val mutable Count: int
    val mutable ChildAppShaped: bool

    new(name: string) =
        {
            Name = name
            Count = 0
            ChildAppShaped = false
        }
