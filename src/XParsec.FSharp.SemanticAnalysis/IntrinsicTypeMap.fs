namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open Vesper

/// One intrinsic declaration: the `.fsi` identity and what its target's `.fs` binds.
type IntrinsicBinding =
    {
        Canon: TypeKey
        Platform: IntrinsicPlatform
    }

/// Every `(canon, platform)` declaration in scope, one entry per canon. Order is PRECEDENCE
/// across sources — `shadow` puts the nearer source's declarations first; within one source
/// it is that source's own enumeration order.
[<CustomEquality; NoComparison>]
type IntrinsicTypeMap =
    private
        {
            Entries: Block<IntrinsicBinding>
            // Indexes over `Entries`, excluded from equality: a `Dictionary` compares by
            // reference. Read-only after construction.
            ByCanon: Dictionary<TypeKey, IntrinsicPlatform>
            ByPlatform: Dictionary<PlatformTypeId, Block<TypeKey>>
        }

    override this.Equals(o: obj) : bool =
        match o with
        | :? IntrinsicTypeMap as other -> this.Entries = other.Entries
        | _ -> false

    override this.GetHashCode() : int = this.Entries.GetHashCode()

    interface IEquatable<IntrinsicTypeMap> with
        member this.Equals(other: IntrinsicTypeMap) = this.Entries = other.Entries

[<RequireQualifiedAccess>]
module IntrinsicTypeMap =

    /// A canon whose platform type id IS its own name (JS `string` → `"string"`) reconciles
    /// nothing: a reader of that id already holds the canon.
    let private isSelfNamed (canon: TypeKey) (id: PlatformTypeId) : bool = id.Value = canon.Name

    /// Entries in precedence order, a canon's FIRST declaration winning.
    let ofSeq (entries: IntrinsicBinding seq) : IntrinsicTypeMap =
        let byCanon = Dictionary<TypeKey, IntrinsicPlatform>()
        let kept = ResizeArray<IntrinsicBinding>()

        for entry in entries do
            if byCanon.TryAdd(entry.Canon, entry.Platform) then
                kept.Add entry

        // An `Unsupported` canon has no platform type id to key under.
        let buckets = Dictionary<PlatformTypeId, ResizeArray<TypeKey>>()

        for entry in kept do
            match entry.Platform with
            | IntrinsicPlatform.Bound id when not (isSelfNamed entry.Canon id) ->
                match buckets.TryGetValue id with
                | true, canons -> canons.Add entry.Canon
                | _ ->
                    let canons = ResizeArray<TypeKey>()
                    canons.Add entry.Canon
                    buckets.[id] <- canons
            | _ -> ()

        let byPlatform = Dictionary<PlatformTypeId, Block<TypeKey>>(buckets.Count)

        for KeyValue(id, canons) in buckets do
            byPlatform.[id] <- Block.ofResizeArray canons

        {
            Entries = Block.ofResizeArray kept
            ByCanon = byCanon
            ByPlatform = byPlatform
        }

    /// The axis of a scope declaring no intrinsics.
    let empty: IntrinsicTypeMap = ofSeq []

    /// The declarations themselves, in precedence order.
    let entries (map: IntrinsicTypeMap) : Block<IntrinsicBinding> = map.Entries

    let isEmpty (map: IntrinsicTypeMap) : bool = map.Entries.IsEmpty

    /// The binding `canon` is declared with, `Unsupported` included.
    let tryPlatform (canon: TypeKey) (map: IntrinsicTypeMap) : IntrinsicPlatform voption =
        match map.ByCanon.TryGetValue canon with
        | true, platform -> ValueSome platform
        | _ -> ValueNone

    /// The platform type id `canon` binds on the compiling target: `int` → `"System.Int32"`.
    /// `ValueNone` for a canon declared unsupported there, or not declared at all.
    let tryPlatformTypeId (canon: TypeKey) (map: IntrinsicTypeMap) : PlatformTypeId voption =
        match map.ByCanon.TryGetValue canon with
        | true, IntrinsicPlatform.Bound id -> ValueSome id
        | _ -> ValueNone

    /// Every canon `id` stands for, nearest declaration first: `"number"` →
    /// `[int; float; float32]` on JS.
    let canonsOf (id: PlatformTypeId) (map: IntrinsicTypeMap) : Block<TypeKey> =
        match map.ByPlatform.TryGetValue id with
        | true, canons -> canons
        | _ -> Block.empty

    /// The canon `id` reconciles to: `"System.Exception"` → `exn`. The LEADING canon where
    /// an id reconciles to several.
    let tryCanon (id: PlatformTypeId) (map: IntrinsicTypeMap) : TypeKey voption =
        match map.ByPlatform.TryGetValue id with
        | true, canons when not canons.IsEmpty -> ValueSome canons.[0]
        | _ -> ValueNone

    /// The canons sharing `canon`'s platform type id, `canon` included: JS `int` →
    /// `[int; float; float32]`. Empty for a canon whose id is its own name, which stands for
    /// nothing but itself, so JS `char` and `string` both binding `"string"` are NOT a family.
    let familyOf (canon: TypeKey) (map: IntrinsicTypeMap) : Block<TypeKey> =
        match tryPlatformTypeId canon map with
        | ValueSome id when not (isSelfNamed canon id) -> canonsOf id map
        | _ -> Block.empty

    /// `shadow near far`: `near`'s declarations, then `far`'s for each canon `near` leaves
    /// undeclared. The unit is the DECLARATION, so a local `int` hides the provider's `int`
    /// and says nothing about its `float`.
    let shadow (near: IntrinsicTypeMap) (far: IntrinsicTypeMap) : IntrinsicTypeMap =
        if near.Entries.IsEmpty then far
        elif far.Entries.IsEmpty then near
        else ofSeq (Seq.append near.Entries far.Entries)

    /// The axis a file's own `(# … #)` bindings declare.
    let ofBindings (bindings: IReadOnlyDictionary<TypeKey, IntrinsicBindingInfo>) : IntrinsicTypeMap =
        ofSeq (
            seq {
                for KeyValue(canon, binding) in bindings ->
                    {
                        Canon = canon
                        Platform = IntrinsicPlatform.Bound binding.TypeId
                    }
            }
        )
