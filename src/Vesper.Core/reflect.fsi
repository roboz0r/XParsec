namespace Vesper

// CLR-ONLY. JS has no runtime representation of a type.

/// <summary>Represents type declarations: class types, interface types, array types, value
/// types, enumeration types, type parameters, generic type definitions, and open or closed
/// constructed generic types.</summary>
///
/// <category>Basic Types</category>
type Type = extern

// FSharp.Core/prim-types.fsi:3679
// Both bindings are inline templates; the module emits no method.
[<AutoOpen>]
module TypeIntrinsics =

    /// <summary>Generate a Type runtime representation of a static type.</summary>
    val inline typeof<'T> : Type

    /// <summary>Generate a Type representation for a type definition. If the
    /// input type is a generic type instantiation then return the generic type definition
    /// associated with all such instantiations.</summary>
    val inline typedefof<'T> : Type
