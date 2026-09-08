namespace Vesper

/// <summary>Language primitives associated with the F# language</summary>
module LanguagePrimitives =

    /// <summary>Build an enum value from an underlying value</summary>
    ///
    /// <param name="value">The input value.</param>
    ///
    /// <returns>The value as an enumeration.</returns>
    val inline EnumOfValue<'T, 'Enum> : value: 'T -> 'Enum when 'Enum: enum<'T>

    /// <summary>Get the underlying value for an enum value</summary>
    ///
    /// <param name="value">The input enum.</param>
    ///
    /// <returns>The enumeration as a value.</returns>
    val inline EnumToValue<'Enum, 'T> : value: 'Enum -> 'T when 'Enum: enum<'T>
