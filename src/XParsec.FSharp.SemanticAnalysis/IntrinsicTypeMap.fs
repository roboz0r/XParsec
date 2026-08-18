namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic

/// One intrinsic declaration: the `.fsi` identity and the repr its target's `.fs` binds.
type IntrinsicReprEntry =
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
            Entries: EqArray<IntrinsicReprEntry>
            // Indexes over `Entries`, excluded from equality: a `Dictionary` compares by
            // reference. Read-only after construction.
            ByCanon: Dictionary<TypeKey, IntrinsicPlatform>
            ByPlatform: Dictionary<string, EqArray<TypeKey>>
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

    /// A canon whose platform spelling IS its own name (JS `string` → `"string"`) reconciles
    /// nothing: a reader of that name already holds the canon.
    let private isSelfRepr (canon: TypeKey) (platform: string) : bool = platform = canon.Name

    /// Entries in precedence order, a canon's FIRST declaration winning.
    let ofSeq (entries: IntrinsicReprEntry seq) : IntrinsicTypeMap =
        let byCanon = Dictionary<TypeKey, IntrinsicPlatform>()
        let kept = ResizeArray<IntrinsicReprEntry>()

        for entry in entries do
            if byCanon.TryAdd(entry.Canon, entry.Platform) then
                kept.Add entry

        // An `Unsupported` canon has no platform name to key under.
        let buckets = Dictionary<string, ResizeArray<TypeKey>>(StringComparer.Ordinal)

        for entry in kept do
            match entry.Platform with
            | IntrinsicPlatform.Repr platform when not (isSelfRepr entry.Canon platform) ->
                match buckets.TryGetValue platform with
                | true, canons -> canons.Add entry.Canon
                | _ ->
                    let canons = ResizeArray<TypeKey>()
                    canons.Add entry.Canon
                    buckets.[platform] <- canons
            | _ -> ()

        let byPlatform =
            Dictionary<string, EqArray<TypeKey>>(buckets.Count, StringComparer.Ordinal)

        for KeyValue(platform, canons) in buckets do
            byPlatform.[platform] <- EqArray.ofResizeArray canons

        {
            Entries = EqArray.ofResizeArray kept
            ByCanon = byCanon
            ByPlatform = byPlatform
        }

    /// The axis of a scope declaring no intrinsics.
    let empty: IntrinsicTypeMap = ofSeq []

    /// The declarations themselves, in precedence order.
    let entries (map: IntrinsicTypeMap) : EqArray<IntrinsicReprEntry> = map.Entries

    let isEmpty (map: IntrinsicTypeMap) : bool = map.Entries.IsEmpty

    /// The repr `canon` is declared with, `Unsupported` included.
    let tryRepr (canon: TypeKey) (map: IntrinsicTypeMap) : IntrinsicPlatform voption =
        match map.ByCanon.TryGetValue canon with
        | true, platform -> ValueSome platform
        | _ -> ValueNone

    /// The platform spelling `canon` binds on the compiling target: `int` → `"System.Int32"`.
    /// `ValueNone` for a canon declared unsupported there, or not declared at all.
    let tryPlatformRepr (canon: TypeKey) (map: IntrinsicTypeMap) : string voption =
        match map.ByCanon.TryGetValue canon with
        | true, IntrinsicPlatform.Repr platform -> ValueSome platform
        | _ -> ValueNone

    /// Every canon `platform` stands for, nearest declaration first: `"number"` →
    /// `[int; float; float32]` on JS.
    let canonsOf (platform: string) (map: IntrinsicTypeMap) : EqArray<TypeKey> =
        match map.ByPlatform.TryGetValue platform with
        | true, canons -> canons
        | _ -> EqArray.empty

    /// The canon `platform` reconciles to: `"System.Exception"` → `exn`. The LEADING canon
    /// where a repr reconciles to several.
    let tryCanon (platform: string) (map: IntrinsicTypeMap) : TypeKey voption =
        match map.ByPlatform.TryGetValue platform with
        | true, canons when not canons.IsEmpty -> ValueSome canons.[0]
        | _ -> ValueNone

    /// The canons sharing `canon`'s repr, `canon` included: JS `int` → `[int; float; float32]`.
    /// Empty for a canon whose repr is its own name, which stands for nothing but itself, so
    /// JS `char` and `string` both repr'ing `"string"` are NOT a family.
    let familyOf (canon: TypeKey) (map: IntrinsicTypeMap) : EqArray<TypeKey> =
        match tryPlatformRepr canon map with
        | ValueSome platform when not (isSelfRepr canon platform) -> canonsOf platform map
        | _ -> EqArray.empty

    /// `shadow near far`: `near`'s declarations, then `far`'s for each canon `near` leaves
    /// undeclared. The unit is the DECLARATION, so a local `int` hides the provider's `int`
    /// and says nothing about its `float`.
    let shadow (near: IntrinsicTypeMap) (far: IntrinsicTypeMap) : IntrinsicTypeMap =
        if near.Entries.IsEmpty then far
        elif far.Entries.IsEmpty then near
        else ofSeq (Seq.append near.Entries far.Entries)

    /// The axis a file's own `(# … #)` bindings declare.
    let ofReprKeys (reprKeys: IReadOnlyDictionary<TypeKey, IntrinsicReprInfo>) : IntrinsicTypeMap =
        ofSeq (
            seq {
                for KeyValue(canon, repr) in reprKeys ->
                    {
                        Canon = canon
                        Platform = IntrinsicPlatform.Repr repr.Platform
                    }
            }
        )
