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
	public int X;

	public int Y;

	public P(int X, int Y)
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
		sink.Child((object)X);
		sink.Field("Y");
		sink.Child((object)Y);
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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(a.X);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(a.Y);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		P x = a;
		P y = b;
		((Formatter)(ref val3)).AppendBool(EqualityComparer<P>.Default.Equals(x, y), 0);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		P x2 = a;
		P y2 = c;
		((Formatter)(ref val4)).AppendBool(EqualityComparer<P>.Default.Equals(x2, y2), 0);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		P p = a;
		d = new P(10, p.Y);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		int x3 = d.X;
		int y3 = d.Y;
		int num = x3;
		int num2 = y3;
		((Formatter)(ref val5)).AppendFormatted<int>(num + num2);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
