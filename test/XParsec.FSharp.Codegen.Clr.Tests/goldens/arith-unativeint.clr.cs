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
	public static nuint addN(nuint a, nuint b)
	{
		return a + b;
	}

	public static nuint subN(nuint a, nuint b)
	{
		return a - b;
	}

	public static nuint mulN(nuint a, nuint b)
	{
		return a * b;
	}

	public static nuint divN(nuint a, nuint b)
	{
		return a / b;
	}

	public static nuint remN(nuint a, nuint b)
	{
		return a % b;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		nuint num = addN(10u, 20u);
		formatter.AppendFormatted((int)num);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		nuint num2 = subN(30u, 10u);
		formatter2.AppendFormatted((int)num2);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		nuint num3 = mulN(6u, 7u);
		formatter3.AppendFormatted((int)num3);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		nuint num4 = divN(100u, 7u);
		formatter4.AppendFormatted((int)num4);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		nuint num5 = remN(100u, 7u);
		formatter5.AppendFormatted((int)num5);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		return 0;
	}
}
