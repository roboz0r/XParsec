/*
// nativeint — a pointer-width integer. It has a CLR representation
// (`System.IntPtr`) and no decided JS one, so this program is the asymmetry the
// manifest states: it RUNS on a target that can represent the width, and must be
// REJECTED on one that cannot.
let addN (a: nativeint) (b: nativeint) : nativeint = a + b
let subN (a: nativeint) (b: nativeint) : nativeint = a - b
let mulN (a: nativeint) (b: nativeint) : nativeint = a * b
let divN (a: nativeint) (b: nativeint) : nativeint = a / b
let remN (a: nativeint) (b: nativeint) : nativeint = a % b

printfn "%d" (int (addN 10n 20n))
printfn "%d" (int (subN 10n 20n))
printfn "%d" (int (mulN 6n 7n))
printfn "%d" (int (divN 100n 7n))
printfn "%d" (int (remN 100n 7n))
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static nint addN(nint a, nint b)
	{
		return a + b;
	}

	public static nint subN(nint a, nint b)
	{
		return a - b;
	}

	public static nint mulN(nint a, nint b)
	{
		return a * b;
	}

	public static nint divN(nint a, nint b)
	{
		return a / b;
	}

	public static nint remN(nint a, nint b)
	{
		return a % b;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted((int)addN(10, 20));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted((int)subN(10, 20));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted((int)mulN(6, 7));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted((int)divN(100, 7));
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted((int)remN(100, 7));
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		return 0;
	}
}
