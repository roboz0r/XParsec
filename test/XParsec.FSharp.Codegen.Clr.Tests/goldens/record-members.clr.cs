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
			return x * 2;
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
		return x + y;
	}

	public int AddN(int arg0)
	{
		int x = X;
		int y = Y;
		int num = x + y;
		return num + arg0;
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
		sink.Child(X);
		sink.Field("Y");
		sink.Child(Y);
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
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(v.Sum());
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(v.AddN(10));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(v.Doubled);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(v.X);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		return 0;
	}
}
