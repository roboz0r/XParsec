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
public struct Point : IEquatable<Point>, IStructuralFormattable
{
	public int X;

	public int Y;

	public Point(int X, int Y)
	{
		this.X = X;
		this.Y = Y;
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(X);
		hashCode.Add(Y);
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is Point) ? obj : null);
		if (obj2 != null)
		{
			Point other = (Point)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(Point other)
	{
		if (X == other.X)
		{
			return Y == other.Y;
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		sink.BeginRecord();
		sink.Field("X");
		sink.Child(X);
		sink.Field("Y");
		sink.Child(Y);
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

	public static T0 onlyStruct<T0>(T0 arg0)
	{
		return arg0;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
