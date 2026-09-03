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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		ulong num = 10uL;
		ulong num2 = 3uL;
		ulong num3 = num;
		ulong num4 = num2;
		((Formatter)(ref val)).AppendFormatted<ulong>(num3 + num4);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		ulong num5 = 10uL;
		ulong num6 = 3uL;
		ulong num7 = num5;
		ulong num8 = num6;
		((Formatter)(ref val2)).AppendFormatted<ulong>(num7 - num8);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		ulong num9 = 10uL;
		ulong num10 = 3uL;
		ulong num11 = num9;
		ulong num12 = num10;
		((Formatter)(ref val3)).AppendFormatted<ulong>(num11 * num12);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		ulong num13 = 10uL;
		ulong num14 = 3uL;
		ulong num15 = num13;
		ulong num16 = num14;
		((Formatter)(ref val4)).AppendFormatted<ulong>(num15 / num16);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		ulong num17 = 10uL;
		ulong num18 = 3uL;
		ulong num19 = num17;
		ulong num20 = num18;
		((Formatter)(ref val5)).AppendFormatted<ulong>(num19 % num20);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		ulong num21 = 0uL;
		ulong num22 = 1uL;
		ulong num23 = num21;
		ulong num24 = num22;
		((Formatter)(ref val6)).AppendFormatted<ulong>(num23 - num24);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
