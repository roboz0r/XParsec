namespace Vesper

open System

/// <summary>Adding this attribute to a parameter of an inline function marks the
/// argument as call-at-most-once: the compiler splices it unevaluated at its
/// single linear use site (call-by-name for one use) rather than binding it
/// eagerly, so it is evaluated at most once and on demand. This is the mechanism
/// behind the short-circuiting of <c>&amp;&amp;</c> / <c>||</c> without those
/// operators being special-cased in the compiler. The parameter must be used at
/// most once in the body, not under a lambda or loop, or compilation fails.</summary>
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
/// equality / hashing overrides; the type uses reference equality (State, not
/// Data).</summary>
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

/// <summary>Adding this attribute to a module-level value declares that the value IS a
/// global of the target runtime (JS <c>undefined</c>), not a definition of our own. Two
/// consequences: no definition is emitted for the binding — one would restate the
/// global, and being self-referential could not even initialise — and a reference emits
/// the bare name from any file, with no import. The binding's body must be exactly one
/// zero-operand intrinsic template naming that global, or compilation fails. A JS-target
/// marker; the CLR backend ignores it.</summary>
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
[<Sealed>]
type GlobalAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> GlobalAttribute
