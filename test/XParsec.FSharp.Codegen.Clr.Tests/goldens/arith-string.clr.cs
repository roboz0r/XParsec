/*
// string — the declared `(+)` concatenation, the one non-numeric operand the
// arithmetic contract admits.
printfn "%s" ("ab" + "cd")
printfn "%s" ("" + "x")
printfn "%s" ("a" + "b" + "c")
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted("ab" + "cd");
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted("" + "x");
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		string text = "a" + "b";
		formatter3.AppendFormatted(text + "c");
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		return 0;
	}
}
