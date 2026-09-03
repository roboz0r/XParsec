/*
// The same duality for an inline binding that has a DECLARING MODULE. That is the shape
// with an exportable identity, so it is the one a consumer can splice — and it is also
// the one both backends place on a named module class rather than the anonymous program one, so
// "emitted as an ordinary module function" means something different here than it does
// for a top-level binding, and has to produce the same numbers.
module Scale =

    let inline twice x = x * 2

    // An inline binding calling an inline SIBLING: the sibling reference is what a
    // published template has to rewrite to an exportable identity, and what the emitted
    // ordinary function resolves through the module's own bound variable table. The two forms of
    // the same body must agree, and only a program that runs it can say so.
    let inline quadruple x = twice (twice x)

printfn "%d" (Scale.twice 21)
printfn "%d" (Scale.quadruple 10)

let q = Scale.quadruple
printfn "%d" (q 3)
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Scale
{
	public static int twice(int arg0)
	{
		int num = 2;
		int num2 = num;
		return arg0 * num2;
	}

	public static int quadruple(int arg0)
	{
		int num = 2;
		int num2 = num;
		int num3 = arg0 * num2;
		int num4 = num3;
		int num5 = 2;
		int num6 = num4;
		int num7 = num5;
		return num6 * num7;
	}
}
public static class Program
{
	public static int q(int arg0)
	{
		int num = 2;
		int num2 = num;
		int num3 = arg0 * num2;
		int num4 = num3;
		int num5 = 2;
		int num6 = num4;
		int num7 = num5;
		return num6 * num7;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		int num = 21;
		int num2 = num;
		int num3 = 2;
		int num4 = num2;
		int num5 = num3;
		formatter.AppendFormatted(num4 * num5);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		int num6 = 10;
		int num7 = num6;
		int num8 = num7;
		int num9 = 2;
		int num10 = num8;
		int num11 = num9;
		int num12 = num10 * num11;
		int num13 = num12;
		int num14 = 2;
		int num15 = num13;
		int num16 = num14;
		formatter2.AppendFormatted(num15 * num16);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(q(3));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		return 0;
	}
}
