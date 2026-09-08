namespace Vesper

#nowarn "42"

module LanguagePrimitives =

    // Both conversions are identity: an enum and its underlying value share a representation.
    let inline EnumOfValue (value: 'T) : 'Enum = (# "" value : 'Enum #)

    let inline EnumToValue (value: 'Enum) : 'T = (# "" value : 'T #)
