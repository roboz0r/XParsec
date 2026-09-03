/*
// `<` at a primitive, in both directions.
printfn "%b" (1 < 2)
printfn "%b" (2 < 1)
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
		int num = 1;
		int num2 = 2;
		formatter.AppendBool(num < num2, 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		int num3 = 2;
		int num4 = 1;
		formatter2.AppendBool(num3 < num4, 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
