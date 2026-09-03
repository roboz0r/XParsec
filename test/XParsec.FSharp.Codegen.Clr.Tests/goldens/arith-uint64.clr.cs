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
		ulong num = 10uL;
		ulong num2 = 3uL;
		ulong num3 = num;
		ulong num4 = num2;
		formatter.AppendFormatted(num3 + num4);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		ulong num5 = 10uL;
		ulong num6 = 3uL;
		ulong num7 = num5;
		ulong num8 = num6;
		formatter2.AppendFormatted(num7 - num8);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		ulong num9 = 10uL;
		ulong num10 = 3uL;
		ulong num11 = num9;
		ulong num12 = num10;
		formatter3.AppendFormatted(num11 * num12);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		ulong num13 = 10uL;
		ulong num14 = 3uL;
		ulong num15 = num13;
		ulong num16 = num14;
		formatter4.AppendFormatted(num15 / num16);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		ulong num17 = 10uL;
		ulong num18 = 3uL;
		ulong num19 = num17;
		ulong num20 = num18;
		formatter5.AppendFormatted(num19 % num20);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		ulong num21 = 0uL;
		ulong num22 = 1uL;
		ulong num23 = num21;
		ulong num24 = num22;
		formatter6.AppendFormatted(num23 - num24);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
