namespace XParsec.FSharp.Codegen.Clr

open Vesper
open XParsec.FSharp.SemanticAnalysis

[<AutoOpen>]
module internal TyparMarkers =

    /// A declaring type's own typars as self-describing nodes, in declaration order:
    /// position `i` encodes as `!i`. This is the instantiation that names a generic type
    /// from inside its own bodies (`Box\`1<!0>`).
    let declaringMarkers (key: TypeKey) (count: int<typeSlot>) : FrozenType list =
        [
            for i in 0 .. int count - 1 -> FTTypar(TyparScope.Type key, TyparIndex.typeSlot i)
        ]

/// One scope's typars in a `TyparFrame`.
type FrameScope =
    {
        Scope: TyparScope
        Count: int<typeSlot>
    }

/// The typar slots a synthesised generic owner declares, in slot order: the enclosing type's,
/// then the enclosing member's or module function's, then each enclosing lifted local's,
/// innermost last. A closure class declares them as class typars, a lifted local as method ones.
/// The frame's slots are the owner's `Types`.
type TyparFrame =
    {
        Scopes: Block<FrameScope>
    }

    member x.Count: int<typeSlot> =
        x.Scopes |> Block.fold (fun n s -> n + s.Count) 0<typeSlot>

    /// The slot the scope's typar `0` occupies, `ValueNone` for a scope outside the frame.
    member x.TryOffset(scope: TyparScope) : int<typeSlot> voption =
        let mutable offset = 0<typeSlot>
        let mutable found = ValueNone

        for s in x.Scopes do
            if found.IsNone then
                if s.Scope = scope then
                    found <- ValueSome offset
                else
                    offset <- offset + s.Count

        found

    /// The frame's own leaves in slot order: the instantiation denoting the owner from inside
    /// the bodies its scopes are visible in.
    member x.Instantiation: FrozenType list =
        [
            for s in x.Scopes do
                for i in 0 .. int s.Count - 1 -> FTTypar(s.Scope, TyparIndex.typeSlot i)
        ]

    /// The frame with `scope`'s typars appended.
    member x.Push(scope: FrameScope) : TyparFrame =
        {
            Scopes = Block.append x.Scopes (Block.singleton scope)
        }

[<RequireQualifiedAccess>]
module TyparFrame =
    let empty: TyparFrame = { Scopes = Block.empty }

    let private ofScope (scope: TyparScope) (count: int<typeSlot>) : TyparFrame =
        empty.Push { Scope = scope; Count = count }

    /// A type's constructor body: every typar is the type's.
    let ofType (key: TypeKey) (count: int<typeSlot>) : TyparFrame = ofScope (TyparScope.Type key) count

    /// A module function's body: every typar is the function's own.
    let ofFunction (key: BindingKey) (count: int<typeSlot>) : TyparFrame =
        ofScope (TyparScope.ModuleFunction key) count

    /// A member body: the owner's `declaring` typars, then the member's `own`.
    let ofMember (owner: TypeKey) (declaring: int<typeSlot>) (own: int<typeSlot>) : TyparFrame =
        (ofType owner declaring).Push
            {
                Scope = TyparScope.Member owner
                Count = own
            }

/// How a signature encoder resolves an `FTTypar` leaf to a generic parameter slot.
[<RequireQualifiedAccess>]
type TyparSlots =
    /// A nominal's member or a module function: a type's typar is `!i`, a member's or a
    /// module function's own `!!i`. A local's has no slot.
    | Declared
    /// A closure class's own emission: every scope of the frame is a class typar at its
    /// offset.
    | ClosureClass of TyparFrame
    /// A lifted local's own emission: every scope of the frame is a method typar at its
    /// offset.
    | LiftedMethod of TyparFrame
