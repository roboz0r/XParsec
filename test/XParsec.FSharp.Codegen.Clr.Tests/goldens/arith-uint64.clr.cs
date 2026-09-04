/*
// uint64 — the unsigned 64-bit width, at small magnitudes; `arith-uint64-wide.fs`
// carries the ones a 32-bit fold would truncate. `0UL - 1UL` is the row that pins
// the wrap (18446744073709551615, never -1). Reported with `%O`, as for int64.
printfn "%O" (10UL + 3UL)
printfn "%O" (10UL - 3UL)
printfn "%O" (10UL * 3UL)
printfn "%O" (10UL / 3UL)
printfn "%O" (10UL % 3UL)
printfn "%O" (0UL - 1UL)
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
		formatter.AppendFormatted(13uL);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(7uL);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(30uL);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(10uL / 3uL);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(10uL % 3uL);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted((ulong)(-1L));
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		return 0;
	}
}
