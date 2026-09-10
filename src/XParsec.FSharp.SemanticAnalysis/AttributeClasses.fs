namespace XParsec.FSharp.SemanticAnalysis

open Vesper

/// One constructor parameter of an attribute class. `Name` is `ValueNone` where the
/// declaration carries no parameter names (a referenced assembly's member); `Declared` is
/// `ValueNone` for a local parameter written without a type annotation.
type AttributeCtorParam =
    {
        Name: string voption
        Declared: FrozenType voption
    }

/// The identity an attribute records for the constructor it selected.
[<RequireQualifiedAccess>]
type AttributeCtorIdentity =
    | Key of MemberKey
    /// A local constructor with the parameter `param` written without a type annotation. An
    /// attribute argument checks against a declared type, so the constructor is refused at
    /// the use.
    | Unannotated of param: string

/// One constructor of an attribute class, as the argument check selects among them.
type AttributeCtor =
    {
        Identity: AttributeCtorIdentity
        Params: Block<AttributeCtorParam>
    }

/// The declarations of an attribute class the argument check reads.
type IAttributeClassSource =
    /// The constructors `attrKey` declares, in declaration order.
    abstract Ctors: attrKey: TypeKey -> Block<AttributeCtor>

    /// The property or field `name` of `attrKey` with its declared type; `ValueNone` where
    /// the class declares neither.
    abstract TrySettable: attrKey: TypeKey * name: string -> TAttributeMember voption

[<RequireQualifiedAccess>]
module AttributeClasses =

    /// The constructors among `members`, a type's published member list.
    let ctorsOfMembers (members: Block<ExternalMember>) : Block<AttributeCtor> =
        members
        |> Block.filter (fun m -> m.Name = ".ctor")
        |> Block.map (fun m ->
            {
                Identity = AttributeCtorIdentity.Key m.Key
                Params =
                    ExternalSignature.argSigOf m.Signature
                    |> Block.map (fun t ->
                        {
                            Name = ValueNone
                            Declared = ValueSome t
                        }
                    )
            }
        )

    /// The property or field among `members`, the overload set published under one name.
    let settableOfMembers (name: string) (members: Block<ExternalMember>) : TAttributeMember voption =
        members
        |> Block.tryFind (fun m -> m.Storage.IsValueMember)
        |> ValueOption.map (fun m ->
            match m.Storage with
            | MemberStorage.Field -> TAttributeMember.Field(name, m.Signature.Return)
            | MemberStorage.Property
            | MemberStorage.Method -> TAttributeMember.Property(name, m.Signature.Return)
        )

    /// The classes a referenced assembly publishes, read through `store`.
    let ofStore (store: IExternalSymbolStore) : IAttributeClassSource =
        { new IAttributeClassSource with
            member _.Ctors attrKey =
                ctorsOfMembers (store.TryLookupMembers(attrKey, ".ctor"))

            member _.TrySettable(attrKey, name) =
                settableOfMembers name (store.TryLookupMembers(attrKey, name))
        }

    /// `first`'s answer where it declares the class, else `second`'s.
    let firstDeclaring (first: IAttributeClassSource) (second: IAttributeClassSource) : IAttributeClassSource =
        { new IAttributeClassSource with
            member _.Ctors attrKey =
                match first.Ctors attrKey with
                | BlockEmpty -> second.Ctors attrKey
                | ctors -> ctors

            member _.TrySettable(attrKey, name) =
                match first.TrySettable(attrKey, name) with
                | ValueSome target -> ValueSome target
                | ValueNone -> second.TrySettable(attrKey, name)
        }
