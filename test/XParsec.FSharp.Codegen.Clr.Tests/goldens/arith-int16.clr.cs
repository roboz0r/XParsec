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
		formatter.AppendFormatted(-25536);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted((int)(short)(-30000));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(24464);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(10000);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(5);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		return 0;
	}
}
