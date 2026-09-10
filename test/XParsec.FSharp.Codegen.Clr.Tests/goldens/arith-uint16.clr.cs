/*
// uint16 — the mod-65536 wrap (10us - 20us = 65526us, never -10).
printfn "%d" (int (60000us + 10000us))
printfn "%d" (int (10us - 20us))
printfn "%d" (int (300us * 300us))
printfn "%d" (int (60000us / 3us))
printfn "%d" (int (60000us % 7us))
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
		formatter.AppendFormatted(4464);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(65526);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(24464);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(20000);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(3);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		return 0;
	}
}
