/*
// `when 'a : unmanaged` instantiated at a scalar, an enum, a struct record of scalars and a
// tuple of scalars. The CLR lays a tuple out as a `ValueTuple`, so it holds there; JS lays
// out no value type, so the record refuses it.
type Colour =
    | Red = 1
    | Green = 2

[<Struct>]
type Point = { X: int; Y: int }

let onlyUnmanaged<'a when 'a: unmanaged> (x: 'a) = x

let i = onlyUnmanaged 42
let e = onlyUnmanaged Colour.Red
let p = onlyUnmanaged { X = 1; Y = 2 }
let t = onlyUnmanaged (1, 2)
ignore i
ignore e
ignore p
ignore t
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
[Struct]
public readonly struct Point : IEquatable<Point>, IStructuralFormattable
{
	private readonly int X@;

	private readonly int Y@;

	public int X => X@;

	public int Y => Y@;

	public Point(int X, int Y)
	{
		X@ = X;
		Y@ = Y;
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(X@);
		hashCode.Add(Y@);
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		if (obj is Point)
		{
			return Equals((Point)obj);
		}
		return false;
	}

	public bool Equals(Point other)
	{
		if (X@ == other.X@)
		{
			return Y@ == other.Y@;
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		sink.BeginRecord();
		sink.Field("X");
		sink.Child(X@);
		sink.Field("Y");
		sink.Child(Y@);
		sink.EndRecord();
	}
}
public enum Colour
{
	Red = 1,
	Green
}
public static class Program
{
	public static readonly int i;

	public static readonly Colour e;

	public static readonly Point p;

	public static readonly (int, int) t;

	static Program()
	{
		i = onlyUnmanaged(42);
		e = onlyUnmanaged(Colour.Red);
		p = onlyUnmanaged(new Point(1, 2));
		t = onlyUnmanaged((1, 2));
	}

	public static T0 onlyUnmanaged<T0>(T0 x)
	{
		return x;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
