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
	public static nint addN(nint arg0, nint arg1)
	{
		return arg0 + arg1;
	}

	public static nint subN(nint arg0, nint arg1)
	{
		return arg0 - arg1;
	}

	public static nint mulN(nint arg0, nint arg1)
	{
		return arg0 * arg1;
	}

	public static nint divN(nint arg0, nint arg1)
	{
		return arg0 / arg1;
	}

	public static nint remN(nint arg0, nint arg1)
	{
		return arg0 % arg1;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		nint num = addN(10, 20);
		nint num2 = num;
		formatter.AppendFormatted((int)num2);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		nint num3 = subN(10, 20);
		nint num4 = num3;
		formatter2.AppendFormatted((int)num4);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		nint num5 = mulN(6, 7);
		nint num6 = num5;
		formatter3.AppendFormatted((int)num6);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		nint num7 = divN(100, 7);
		nint num8 = num7;
		formatter4.AppendFormatted((int)num8);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		nint num9 = remN(100, 7);
		nint num10 = num9;
		formatter5.AppendFormatted((int)num10);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
