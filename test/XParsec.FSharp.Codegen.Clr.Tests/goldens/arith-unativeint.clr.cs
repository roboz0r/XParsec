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
		Formatter formatter = new Formatter(0, 1, Console.Out);
		nuint num = addN(10u, 20u);
		nuint num2 = num;
		formatter.AppendFormatted((int)num2);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		nuint num3 = subN(30u, 10u);
		nuint num4 = num3;
		formatter2.AppendFormatted((int)num4);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		nuint num5 = mulN(6u, 7u);
		nuint num6 = num5;
		formatter3.AppendFormatted((int)num6);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		nuint num7 = divN(100u, 7u);
		nuint num8 = num7;
		formatter4.AppendFormatted((int)num8);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		nuint num9 = remN(100u, 7u);
		nuint num10 = num9;
		formatter5.AppendFormatted((int)num10);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
