/*
// A body-local `let` inside an inline function is generalised at its own `let`, so one
// local serves two types inside the same body. F# forbids explicit typars on a local
// (FS0665), so the local's polymorphism is always implicit, and a splice has to
// re-generalise the local rather than instantiate it once for the whole body. Each
// program below is accepted by F# and prints the components of a two-typed pair.

// The local used at two types, the outer function monomorphic.
let inline twoUses (x: int) =
    let g y = y
    (g x, g "a")

// An annotated `'a` on a local is a generalisable placeholder, not a declaration.
let inline annotated (x: int) =
    let g (y: 'a) : 'a = y
    (g x, g "b")

// The local's own typar is generalised while the outer function's `'b`, captured through
// `x`, is not: `g`'s two uses agree on `x`'s type and differ on `y`'s.
let inline entangled (x: 'b) =
    let g y = (y, x)
    (g 1, g "c")

// The inline body served across a module boundary.
module M =
    let inline served (x: int) =
        let g y = y
        (g x, g "e")

module N =
    let consume () = M.served 5

match twoUses 1 with
| a1, a2 -> printfn "%d %s" a1 a2

match annotated 2 with
| b1, b2 -> printfn "%d %s" b1 b2

match entangled "outer" with
| (c1, c1x), (c2, c2x) -> printfn "%d %s %s %s" c1 c1x c2 c2x

match N.consume () with
| e1, e2 -> printfn "%d %s" e1 e2
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class M
{
	public static (int, string) served(int x)
	{
		return (Program.g@3(x), Program.g@3("e"));
	}
}
public static class N
{
	public static (int, string) consume()
	{
		return (Program.local@4(5), Program.local@4("e"));
	}
}
public static class Program
{
	public static (int, string) twoUses(int x)
	{
		return (g@0(x), g@0("a"));
	}

	public static (int, string) annotated(int x)
	{
		return (g@1(x), g@1("b"));
	}

	public static ((int, M0), (string, M0)) entangled<M0>(M0 x)
	{
		return (g@2(x, 1), g@2(x, "c"));
	}

	internal static T0 g@0<T0>(T0 y)
	{
		return y;
	}

	internal static T0 g@1<T0>(T0 y)
	{
		return y;
	}

	internal static (T1, T0) g@2<T0, T1>(T0 x, T1 y)
	{
		return (y, x);
	}

	internal static T0 g@3<T0>(T0 y)
	{
		return y;
	}

	internal static T0 local@4<T0>(T0 arg0)
	{
		return arg0;
	}

	internal static T0 local@5<T0>(T0 arg0)
	{
		return arg0;
	}

	internal static T0 local@6<T0>(T0 arg0)
	{
		return arg0;
	}

	internal static (T0, string) local@7<T0>(T0 arg0)
	{
		return (arg0, "outer");
	}

	public static int Main(string[] args)
	{
		(int, string) tuple = (local@5(1), local@5("a"));
		int item = tuple.Item1;
		string item2 = tuple.Item2;
		Formatter formatter = new Formatter(1, 2, Console.Out);
		formatter.AppendFormatted(item);
		formatter.AppendLiteral(" ");
		formatter.AppendFormatted(item2);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		(int, string) tuple2 = (local@6(2), local@6("b"));
		int item3 = tuple2.Item1;
		string item4 = tuple2.Item2;
		Formatter formatter2 = new Formatter(1, 2, Console.Out);
		formatter2.AppendFormatted(item3);
		formatter2.AppendLiteral(" ");
		formatter2.AppendFormatted(item4);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		((int, string), (string, string)) tuple3 = (local@7(1), local@7("c"));
		(int, string) item5 = tuple3.Item1;
		int item6 = item5.Item1;
		string item7 = item5.Item2;
		(string, string) item8 = tuple3.Item2;
		string item9 = item8.Item1;
		string item10 = item8.Item2;
		Formatter formatter3 = new Formatter(3, 4, Console.Out);
		formatter3.AppendFormatted(item6);
		formatter3.AppendLiteral(" ");
		formatter3.AppendFormatted(item7);
		formatter3.AppendLiteral(" ");
		formatter3.AppendFormatted(item9);
		formatter3.AppendLiteral(" ");
		formatter3.AppendFormatted(item10);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		(int, string) tuple4 = N.consume();
		int item11 = tuple4.Item1;
		string item12 = tuple4.Item2;
		Formatter formatter4 = new Formatter(1, 2, Console.Out);
		formatter4.AppendFormatted(item11);
		formatter4.AppendLiteral(" ");
		formatter4.AppendFormatted(item12);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		return 0;
	}
}
