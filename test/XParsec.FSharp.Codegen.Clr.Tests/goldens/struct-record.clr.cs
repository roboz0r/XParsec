/*
// A [<Struct>] record. On the CLR it emits as a System.ValueType-based value type
// (sealed, base-chain-free ctor, value-type-shaped structural equality); on JS, which
// has no value-type concept, it emits as an ordinary reference object. Both must agree
// on every observable result below: construction, field reads, structural equality, and
// a `{ r with … }` copy-update.
[<Struct>]
type P = { X: int; Y: int }

let a = { X = 3; Y = 4 }
let b = { X = 3; Y = 4 }
let c = { X = 5; Y = 4 }

printfn "%d" a.X
printfn "%d" a.Y
printfn "%b" (a = b)
printfn "%b" (a = c)

let d = { a with X = 10 }
printfn "%d" (d.X + d.Y)
*/

using System;
using System.Collections.Generic;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
[Struct]
public struct P : IEquatable<P>, IStructuralFormattable
{
	private readonly int X@;

	private readonly int Y@;

	public int X => X@;

	public int Y => Y@;

	public P(int X, int Y)
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
		object obj2 = ((obj is P) ? obj : null);
		if (obj2 != null)
		{
			P other = (P)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(P other)
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
	public static readonly P a;

	public static readonly P b;

	public static readonly P c;

	public static P d;

	static Program()
	{
		a = new P(3, 4);
		b = new P(3, 4);
		c = new P(5, 4);
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(a.X);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(a.Y);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendBool(EqualityComparer<P>.Default.Equals(a, b), 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendBool(EqualityComparer<P>.Default.Equals(a, c), 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		d = new P(10, a.Y);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		int x = d.X;
		int y = d.Y;
		formatter5.AppendFormatted(x + y);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		return 0;
	}
}
