/*
// A `[<Struct>]` record is laid out as a value on the CLR and erased to an ordinary object
// on JS, so `when 'a : struct` holds at it exactly where it holds at `int`.
[<Struct>]
type Point = { X: int; Y: int }

let onlyStruct<'a when 'a: struct> (x: 'a) = x

let p = onlyStruct { X = 1; Y = 2 }
ignore p
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
public static class Program
{
	public static readonly Point p;

	static Program()
	{
		p = onlyStruct(new Point(1, 2));
	}

	public static T0 onlyStruct<T0>(T0 x)
	{
		return x;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
