namespace Vesper

// TODO: Add compiler recognised attributes to this file, so that we can use them in the core library without referencing FSharp.Core

/// <summary>The base type for attributes. A Vesper attribute is any type that
/// inherits this; the target provides the underlying attribute mechanism (if
/// any), so the base itself is platform-provided.</summary>
///
/// <category>Basic Types</category>
[<AbstractClass>]
type Attribute = extern
