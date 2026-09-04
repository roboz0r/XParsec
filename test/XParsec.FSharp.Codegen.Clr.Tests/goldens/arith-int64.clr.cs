/*
// int64 — magnitudes beyond int32, so a 32-bit opcode would visibly wrap. The
// value is reported with `%O` (stringification): `%d` types its argument as int32,
// so an int64 cannot be passed to it, and no int64 -> string conversion exists in the
// contract.
printfn "%O" (1000000000000L + 1L)
printfn "%O" (1000000000000L - 1L)
printfn "%O" (1000000000000L * 3L)
printfn "%O" (3000000000000L / 3L)
printfn "%O" (1000000000001L % 10L)
printfn "%O" (-7L / 2L)
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
		formatter.AppendFormatted(1000000000000L + 1L);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(1000000000000L - 1L);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(1000000000000L * 3L);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(3000000000000L / 3L);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(1000000000001L % 10L);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(-7L / 2L);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		return 0;
	}
}
