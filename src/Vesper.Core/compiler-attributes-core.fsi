namespace Vesper

// The attribute types `ops-bitwise.fsi` writes, in a file compiled ahead of it. `AutoOpen`'s
// own mask is therefore written numerically; every other `[<AttributeUsage>]` mask in
// `compiler-attributes.fsi` is written with `|||`.

/// <summary>Specifies the application elements on which it is valid to apply an attribute.</summary>
type AttributeTargets =
    | Assembly = 1
    | Module = 2
    | Class = 4
    | Struct = 8
    | Enum = 16
    | Constructor = 32
    | Method = 64
    | Property = 128
    | Field = 256
    | Event = 512
    | Interface = 1024
    | Parameter = 2048
    | Delegate = 4096
    | ReturnValue = 8192
    | GenericParameter = 16384
    | All = 32767

/// <summary>Specifies the usage of another attribute class.</summary>
type AttributeUsageAttribute =
    inherit Attribute

    /// <summary>Initializes a new instance of the attribute with the specified list of
    /// <c>AttributeTargets</c>.</summary>
    new: validOn: AttributeTargets -> AttributeUsageAttribute

    /// <summary>Gets the set of values identifying which application elements the indicated
    /// attribute can be applied to.</summary>
    member ValidOn: AttributeTargets

    /// <summary>Gets or sets a value indicating whether more than one instance of the
    /// indicated attribute can be specified for a single application element.</summary>
    member AllowMultiple: bool with get, set

    /// <summary>Gets or sets a value indicating whether the indicated attribute is inherited
    /// by derived classes and overriding members.</summary>
    member Inherited: bool with get, set

// FSharp.Core/prim-types.fsi:99
/// <summary>Adding this attribute to class definition makes it sealed, which means it may not
/// be extended or implemented.</summary>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type SealedAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute.</summary>
    /// <returns>The created attribute.</returns>
    new: unit -> SealedAttribute

    /// <summary>Creates an instance of the attribute</summary>
    ///
    /// <param name="value">Indicates whether the class is sealed.</param>
    ///
    /// <returns>SealedAttribute</returns>
    new: value: bool -> SealedAttribute

    /// <summary>The value of the attribute, indicating whether the type is sealed or not.</summary>
    member Value: bool


// FSharp.Core/prim-types.fsi:926
/// <summary>Indicates a construct is automatically opened when brought into scope through
/// an assembly reference or then opening of the containing namespace or module.</summary>
///
/// <remarks>When applied to an assembly, this attribute must be given a string
/// argument, and this indicates a valid module or namespace in that assembly. Source
/// code files compiled with a reference to this assembly are processed in an environment
/// where the given path is automatically opened.
///
/// When applied to a type or module within an assembly, then the attribute must not be given any arguments, and
/// the type or module is implicitly opened when its enclosing namespace or module is opened.
/// </remarks>
[<AttributeUsage(LanguagePrimitives.EnumOfValue<int, AttributeTargets> 13, AllowMultiple = true)>] // Class ||| Struct ||| Assembly
[<Sealed>]
type AutoOpenAttribute =
    inherit Attribute

    /// <summary>Creates an attribute used to mark a module as 'automatically opened' when the enclosing namespace is opened</summary>
    /// <returns>AutoOpenAttribute</returns>
    new: unit -> AutoOpenAttribute

    /// <summary>Creates an attribute used to mark a namespace or module path to be 'automatically opened' when an assembly is referenced</summary>
    ///
    /// <param name="path">The namespace or module to be automatically opened when an assembly is referenced
    /// or an enclosing module opened.</param>
    ///
    /// <returns>AutoOpenAttribute</returns>
    new: path: string -> AutoOpenAttribute

    /// <summary>Indicates the namespace or module to be automatically opened when an assembly is referenced
    /// or an enclosing module opened.</summary>
    member Path: string
