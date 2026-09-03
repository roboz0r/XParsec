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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		int num = 1;
		int num2 = 2;
		((Formatter)(ref val)).AppendBool(num < num2, 0);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		int num3 = 2;
		int num4 = 1;
		((Formatter)(ref val2)).AppendBool(num3 < num4, 0);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
