/*
// `=` at the primitives the language prescribes equality for. JS does not lower it
// uniformly: `===` at int / bool / float, but a `structuralEquals` call at string.
printfn "%b" (1 = 1)
printfn "%b" (1 = 2)
printfn "%b" ("a" = "a")
printfn "%b" ("a" = "b")
printfn "%b" (true = false)
printfn "%b" (1.5 = 1.5)
*/

using System;
using System.Collections.Generic;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		int num = 1;
		int num2 = 1;
		formatter.AppendBool(num == num2, 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		int num3 = 1;
		int num4 = 2;
		formatter2.AppendBool(num3 == num4, 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		string x = "a";
		string y = "a";
		formatter3.AppendBool(EqualityComparer<string>.Default.Equals(x, y), 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		string x2 = "a";
		string y2 = "b";
		formatter4.AppendBool(EqualityComparer<string>.Default.Equals(x2, y2), 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		bool flag = true;
		bool flag2 = false;
		formatter5.AppendBool(flag == flag2, 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		double num5 = 1.5;
		double num6 = 1.5;
		formatter6.AppendBool(num5 == num6, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
