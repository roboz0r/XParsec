/*
// int16 — the sign-extending 16-bit wrap (30000s + 10000s = 40000 truncates to
// -25536s). Reported through `int (…)`, as for sbyte: a negative int16 literal has
// no `TConstValue` representation.
printfn "%d" (int (30000s + 10000s))
printfn "%d" (int (0s - 30000s))
printfn "%d" (int (300s * 300s))
printfn "%d" (int (30000s / 3s))
printfn "%d" (int (30000s % 7s))
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
		short num = (short)(-25536);
		formatter.AppendFormatted((int)num);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		short num2 = (short)(-30000);
		formatter2.AppendFormatted((int)num2);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		short num3 = (short)24464;
		formatter3.AppendFormatted((int)num3);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		short num4 = (short)10000;
		formatter4.AppendFormatted((int)num4);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		short num5 = (short)5;
		formatter5.AppendFormatted((int)num5);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
