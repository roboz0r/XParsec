/*
// unativeint — the unsigned pointer-width integer; the `nativeint` asymmetry
// again, with the unsigned `/` and `%` clauses.
let addN (a: unativeint) (b: unativeint) : unativeint = a + b
let subN (a: unativeint) (b: unativeint) : unativeint = a - b
let mulN (a: unativeint) (b: unativeint) : unativeint = a * b
let divN (a: unativeint) (b: unativeint) : unativeint = a / b
let remN (a: unativeint) (b: unativeint) : unativeint = a % b

printfn "%d" (int (addN 10un 20un))
printfn "%d" (int (subN 30un 10un))
printfn "%d" (int (mulN 6un 7un))
printfn "%d" (int (divN 100un 7un))
printfn "%d" (int (remN 100un 7un))
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static nuint addN(nuint arg0, nuint arg1)
	{
		return arg0 + arg1;
	}

	public static nuint subN(nuint arg0, nuint arg1)
	{
		return arg0 - arg1;
	}

	public static nuint mulN(nuint arg0, nuint arg1)
	{
		return arg0 * arg1;
	}

	public static nuint divN(nuint arg0, nuint arg1)
	{
		return arg0 / arg1;
	}

	public static nuint remN(nuint arg0, nuint arg1)
	{
		return arg0 % arg1;
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		nuint num = addN(10u, 20u);
		nuint num2 = num;
		((Formatter)(ref val)).AppendFormatted<int>((int)num2);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		nuint num3 = subN(30u, 10u);
		nuint num4 = num3;
		((Formatter)(ref val2)).AppendFormatted<int>((int)num4);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		nuint num5 = mulN(6u, 7u);
		nuint num6 = num5;
		((Formatter)(ref val3)).AppendFormatted<int>((int)num6);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		nuint num7 = divN(100u, 7u);
		nuint num8 = num7;
		((Formatter)(ref val4)).AppendFormatted<int>((int)num8);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		nuint num9 = remN(100u, 7u);
		nuint num10 = num9;
		((Formatter)(ref val5)).AppendFormatted<int>((int)num10);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
