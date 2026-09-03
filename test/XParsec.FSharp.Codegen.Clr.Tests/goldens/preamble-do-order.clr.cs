/*
// The preamble runs in DECLARATION order, once per construction: each `do` sees the
// `let`s above it and none below. The side effects are printed, so a backend that hoisted
// the `let`s over the `do`s (or ran the `do`s once, at type level) reorders this output.
type Ordered(n: int) =
    let a = n + 1
    do printfn "ctor a=%d" a
    let b = a * 3
    do printfn "ctor b=%d" b
    member this.B() = b

let o1 = Ordered(1)
printfn "b=%d" (o1.B())
let o2 = Ordered(5)
printfn "b=%d" (o2.B())
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public class Ordered
{
	internal int n;

	internal readonly int a;

	internal readonly int b;

	public Ordered(int n)
	{
		this.n = n;
		int num = this.n;
		int num2 = 1;
		int num3 = num;
		int num4 = num2;
		a = num3 + num4;
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(7, 1, Console.Out);
		((Formatter)(ref val)).AppendLiteral("ctor a=");
		((Formatter)(ref val)).AppendFormatted<int>(a);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		int num5 = a;
		int num6 = 3;
		int num7 = num5;
		int num8 = num6;
		b = num7 * num8;
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(7, 1, Console.Out);
		((Formatter)(ref val2)).AppendLiteral("ctor b=");
		((Formatter)(ref val2)).AppendFormatted<int>(b);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
	}

	public int B()
	{
		return b;
	}
}
public static class Program
{
	public static readonly Ordered o1;

	public static Ordered o2;

	static Program()
	{
		o1 = new Ordered(1);
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(2, 1, Console.Out);
		((Formatter)(ref val)).AppendLiteral("b=");
		((Formatter)(ref val)).AppendFormatted<int>(o1.B());
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		o2 = new Ordered(5);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(2, 1, Console.Out);
		((Formatter)(ref val2)).AppendLiteral("b=");
		((Formatter)(ref val2)).AppendFormatted<int>(o2.B());
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
