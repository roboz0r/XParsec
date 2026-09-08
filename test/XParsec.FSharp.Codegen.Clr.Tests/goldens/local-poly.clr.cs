/*
// A body-local `let` is generalised at its own `let`, so one local serves two types in
// the same body. F# forbids explicit typars on a local (FS0665), so the polymorphism is
// always implicit. No `inline` here: this is the control for `inline/inline-local-poly.fs`,
// so a failure here is generalisation or closure emission, not splicing.

// The local used at two types, the outer function monomorphic.
let twoUses (x: int) =
    let g y = y
    (g x, g "a")

// The local's own typar is generalised while the outer function's `'b`, captured through
// `x`, is not.
let entangled (x: 'b) =
    let g y = (y, x)
    (g 1, g "c")

// The local used at ONE type: the shape that does not need a generic local at all.
let oneUse (x: int) =
    let g y = y
    (g x, g 2)

match twoUses 1 with
| a1, a2 -> printfn "%d %s" a1 a2

match entangled "outer" with
| (c1, c1x), (c2, c2x) -> printfn "%d %s %s %s" c1 c1x c2 c2x

match oneUse 3 with
| o1, o2 -> printfn "%d %d" o1 o2
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static (int, string) twoUses(int x)
	{
		return (g@0(x), g@0("a"));
	}

	public static ((int, M0), (string, M0)) entangled<M0>(M0 x)
	{
		return (g@1(x, 1), g@1(x, "c"));
	}

	public static (int, int) oneUse(int x)
	{
		return (g@2(x), g@2(2));
	}

	internal static T0 g@0<T0>(T0 y)
	{
		return y;
	}

	internal static (T1, T0) g@1<T0, T1>(T0 x, T1 y)
	{
		return (y, x);
	}

	internal static T0 g@2<T0>(T0 y)
	{
		return y;
	}

	public static int Main(string[] args)
	{
		(int, string) tuple = twoUses(1);
		int item = tuple.Item1;
		string item2 = tuple.Item2;
		Formatter formatter = new Formatter(1, 2, Console.Out);
		formatter.AppendFormatted(item);
		formatter.AppendLiteral(" ");
		formatter.AppendFormatted(item2);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		((int, string), (string, string)) tuple2 = entangled("outer");
		(int, string) item3 = tuple2.Item1;
		int item4 = item3.Item1;
		string item5 = item3.Item2;
		(string, string) item6 = tuple2.Item2;
		string item7 = item6.Item1;
		string item8 = item6.Item2;
		Formatter formatter2 = new Formatter(3, 4, Console.Out);
		formatter2.AppendFormatted(item4);
		formatter2.AppendLiteral(" ");
		formatter2.AppendFormatted(item5);
		formatter2.AppendLiteral(" ");
		formatter2.AppendFormatted(item7);
		formatter2.AppendLiteral(" ");
		formatter2.AppendFormatted(item8);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		(int, int) tuple3 = oneUse(3);
		int item9 = tuple3.Item1;
		int item10 = tuple3.Item2;
		Formatter formatter3 = new Formatter(1, 2, Console.Out);
		formatter3.AppendFormatted(item9);
		formatter3.AppendLiteral(" ");
		formatter3.AppendFormatted(item10);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		return 0;
	}
}
