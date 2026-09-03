/*
// A record with INSTANCE members — a method (`Sum`, `AddN n`) and a property
// (`Doubled`) — reached by dot-access. Instance-member dispatch on a record object argument
// resolves on the same nominal-member path as a class or union; a field read (`v.X`)
// still lowers to a plain field access, so both forms must agree across the backends.
type Vec =
    {
        X: int
        Y: int
    }

    member this.Sum() = this.X + this.Y
    member this.AddN(n: int) = this.X + this.Y + n
    member this.Doubled = this.X * 2

let v = { X = 3; Y = 4 }
printfn "%d" (v.Sum())
printfn "%d" (v.AddN 10)
printfn "%d" v.Doubled
printfn "%d" v.X
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public sealed class Vec : IEquatable<Vec>, IStructuralFormattable
{
	public int X;

	public int Y;

	public int Doubled
	{
		get
		{
			int x = X;
			int num = 2;
			int num2 = x;
			int num3 = num;
			return num2 * num3;
		}
	}

	public Vec(int X, int Y)
	{
		this.X = X;
		this.Y = Y;
	}

	public int Sum()
	{
		int x = X;
		int y = Y;
		int num = x;
		int num2 = y;
		return num + num2;
	}

	public int AddN(int arg0)
	{
		int x = X;
		int y = Y;
		int num = x;
		int num2 = y;
		int num3 = num + num2;
		int num4 = num3;
		return num4 + arg0;
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
		if (obj is Vec other)
		{
			return Equals(other);
		}
		return false;
	}

	public bool Equals(Vec other)
	{
		if (other != null && X == other.X)
		{
			return Y == other.Y;
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		sink.BeginRecord();
		sink.Field("X");
		sink.Child((object)X);
		sink.Field("Y");
		sink.Child((object)Y);
		sink.EndRecord();
	}
}
public static class Program
{
	public static readonly Vec v;

	static Program()
	{
		v = new Vec(3, 4);
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(v.Sum());
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(v.AddN(10));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		((Formatter)(ref val3)).AppendFormatted<int>(v.Doubled);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		((Formatter)(ref val4)).AppendFormatted<int>(v.X);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		return 0;
	}
}
