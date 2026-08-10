namespace Vesper

/// <summary>Single dimensional, zero-based arrays, written <c>int array</c>, <c>string array</c> etc.</summary>
///
/// <remarks>Declared after <c>capabilities.fsi</c> because it names <c>seq&lt;'T&gt;</c>: an
/// array IS iterable, supplied by the target rather than by any Vesper code. The CLI gives
/// <c>T[]</c> an <c>IEnumerable&lt;T&gt;</c>, and a JS array carries <c>Symbol.iterator</c>.</remarks>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[]`` = extern with
    interface Vesper.Collections.seq<'T>

/// <summary>Single dimensional, zero-based arrays, written <c>int array</c>, <c>string array</c> etc.</summary>
///
/// <category>Basic Types</category>
type 'T array = 'T[]
