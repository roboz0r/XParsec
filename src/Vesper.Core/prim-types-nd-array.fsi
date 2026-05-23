namespace Vesper

open System

/// <summary>Two dimensional arrays, typically zero-based.</summary>
///
/// <remarks>Use the values in the <c>Array2D</c> module
/// to manipulate values of this type, or the notation <c>arr.[x,y]</c> to get/set array
/// values.
///
/// Non-zero-based arrays can also be created using methods on the System.Array type.</remarks>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,]`` = (# "!0[0 ... , 0 ... ]" #)

/// <summary>Three dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <remarks>Use the values in the <c>Array3D</c> module
/// to manipulate values of this type, or the notation <c>arr.[x1,x2,x3]</c> to get and set array
/// values.</remarks>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,]`` = (# "!0[0 ...,0 ...,0 ...]" #)

/// <summary>Four dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <remarks>Use the values in the <c>Array4D</c> module
/// to manipulate values of this type, or the notation <c>arr.[x1,x2,x3,x4]</c> to get and set array
/// values.</remarks>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,]`` = (# "!0[0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Five dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,]`` = (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Six dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,]`` = (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Seven dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,]`` = (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Eight dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,]`` = (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Nine dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,]`` = (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Ten dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Eleven dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twelve dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Thirteen dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Fourteen dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Fifteen dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Sixteen dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Seventeen dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Eighteen dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Nineteen dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-one dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-two dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-three dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-four dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-five dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-six dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-seven dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-eight dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Twenty-nine dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Thirty dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Thirty-one dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)

/// <summary>Thirty-two dimensional arrays, typically zero-based. Non-zero-based arrays
/// can be created using methods on the System.Array type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,]`` =
    (# "!0[0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...,0 ...]" #)
