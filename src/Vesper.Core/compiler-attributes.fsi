namespace Vesper

open System

/// <summary>Splices the argument unevaluated at its one use site — evaluated at most
/// once, on demand. This is how <c>&amp;&amp;</c> / <c>||</c> short-circuit without being
/// special-cased. Using it twice, or under a lambda or loop, fails compilation.</summary>
[<AttributeUsage(AttributeTargets.Parameter, AllowMultiple = false)>]
[<Sealed>]
type CallAtMostOnceAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> CallAtMostOnceAttribute

/// <summary>Adding this attribute to a record or union type confirms the automatic
/// generation of overrides for 'Equals' and 'GetHashCode' for the type.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type StructuralEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> StructuralEqualityAttribute

/// <summary>Adding this attribute to a record, union or struct type confirms the automatic
/// generation of an 'IComparable' implementation for the type.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type StructuralComparisonAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> StructuralComparisonAttribute

/// <summary>Adding this attribute to a type disables the automatic generation of
/// equality / hashing overrides; the type uses reference equality.</summary>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
[<Sealed>]
type ReferenceEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> ReferenceEqualityAttribute

/// <summary>Adding this attribute to a type indicates equality is an abnormal
/// operation: the type does not satisfy the 'equality' constraint, so generic
/// equality cannot be instantiated at it.</summary>
[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Interface
                 ||| AttributeTargets.Delegate
                 ||| AttributeTargets.Struct
                 ||| AttributeTargets.Enum,
                 AllowMultiple = false)>]
[<Sealed>]
type NoEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> NoEqualityAttribute

/// <summary>Adding this attribute to a type indicates it has a user-defined
/// implementation of equality.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type CustomEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> CustomEqualityAttribute

/// <summary>Adding this attribute to a type indicates it has a user-defined
/// implementation of comparison.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type CustomComparisonAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> CustomComparisonAttribute

/// <summary>Adding this attribute to a type indicates comparison is an abnormal
/// operation: the type does not satisfy the 'comparison' constraint, so generic
/// comparison cannot be instantiated at it.</summary>
[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Interface
                 ||| AttributeTargets.Delegate
                 ||| AttributeTargets.Struct
                 ||| AttributeTargets.Enum,
                 AllowMultiple = false)>]
[<Sealed>]
type NoComparisonAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> NoComparisonAttribute

/// <summary>Declares that a module-level value IS a global of the target runtime (JS
/// <c>undefined</c>): no definition is emitted for it, and a reference emits the bare name
/// from any file with no import. The body must be one zero-operand template.</summary>
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
[<Sealed>]
type GlobalAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> GlobalAttribute
