namespace Vesper

// TODO: Add compiler recognised attributes to this file, so that we can use them in the core library without referencing FSharp.Core

/// <summary>The base type for attributes: an attribute is any type that inherits this.
/// The target provides the underlying mechanism, so the base itself is extern.</summary>
///
/// <category>Basic Types</category>
[<AbstractClass>]
type Attribute = extern class
