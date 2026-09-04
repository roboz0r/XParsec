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
		formatter.AppendBool(1 == 1, 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendBool(1 == 2, 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendBool(EqualityComparer<string>.Default.Equals("a", "a"), 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendBool(EqualityComparer<string>.Default.Equals("a", "b"), 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendBool(1 == 0, 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendBool(1.5 == 1.5, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		return 0;
	}
}
