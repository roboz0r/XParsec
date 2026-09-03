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
	internal readonly int n;

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
		Formatter formatter = new Formatter(7, 1, Console.Out);
		formatter.AppendLiteral("ctor a=");
		formatter.AppendFormatted(a);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		int num5 = a;
		int num6 = 3;
		int num7 = num5;
		int num8 = num6;
		b = num7 * num8;
		Formatter formatter2 = new Formatter(7, 1, Console.Out);
		formatter2.AppendLiteral("ctor b=");
		formatter2.AppendFormatted(b);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
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
		Formatter formatter = new Formatter(2, 1, Console.Out);
		formatter.AppendLiteral("b=");
		formatter.AppendFormatted(o1.B());
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		o2 = new Ordered(5);
		Formatter formatter2 = new Formatter(2, 1, Console.Out);
		formatter2.AppendLiteral("b=");
		formatter2.AppendFormatted(o2.B());
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
