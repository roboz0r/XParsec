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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		nint num = addN(10, 20);
		nint num2 = num;
		((Formatter)(ref val)).AppendFormatted<int>((int)num2);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		nint num3 = subN(10, 20);
		nint num4 = num3;
		((Formatter)(ref val2)).AppendFormatted<int>((int)num4);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		nint num5 = mulN(6, 7);
		nint num6 = num5;
		((Formatter)(ref val3)).AppendFormatted<int>((int)num6);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		nint num7 = divN(100, 7);
		nint num8 = num7;
		((Formatter)(ref val4)).AppendFormatted<int>((int)num8);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		nint num9 = remN(100, 7);
		nint num10 = num9;
		((Formatter)(ref val5)).AppendFormatted<int>((int)num10);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
