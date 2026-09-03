/*
// uint64 at magnitudes ABOVE 2^32 — the width's whole reason for existing, and the
// rows no narrower model can fake. A backend that carries a uint64 LITERAL in 32 bits
// (or, on JS, as a `number` rather than a BigInt) yields a truncated value from the
// first row on. The last row's dividend is beyond int64's range as well, so it also
// needs the UNSIGNED quotient: read signed it is -1, and -1 / 2 is 0.
printfn "%O" (10000000000UL + 1UL)
printfn "%O" (10000000000UL - 1UL)
printfn "%O" (10000000000UL * 3UL)
printfn "%O" (10000000000UL / 3UL)
printfn "%O" (10000000000UL % 3UL)
printfn "%O" (18446744073709551615UL / 2UL)
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
		formatter.AppendFormatted(10000000001uL);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(9999999999uL);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(30000000000uL);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(10000000000uL / 3uL);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(10000000000uL % 3uL);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(ulong.MaxValue / 2uL);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
