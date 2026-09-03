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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		int num = 1;
		int num2 = 1;
		((Formatter)(ref val)).AppendBool(num == num2, 0);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		int num3 = 1;
		int num4 = 2;
		((Formatter)(ref val2)).AppendBool(num3 == num4, 0);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		string x = "a";
		string y = "a";
		((Formatter)(ref val3)).AppendBool(EqualityComparer<string>.Default.Equals(x, y), 0);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		string x2 = "a";
		string y2 = "b";
		((Formatter)(ref val4)).AppendBool(EqualityComparer<string>.Default.Equals(x2, y2), 0);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		bool flag = true;
		bool flag2 = false;
		((Formatter)(ref val5)).AppendBool(flag == flag2, 0);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		double num5 = 1.5;
		double num6 = 1.5;
		((Formatter)(ref val6)).AppendBool(num5 == num6, 0);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
