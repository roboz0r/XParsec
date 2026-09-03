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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		long num = 1000000000000L;
		long num2 = 1L;
		long num3 = num;
		long num4 = num2;
		((Formatter)(ref val)).AppendFormatted<long>(num3 + num4);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		long num5 = 1000000000000L;
		long num6 = 1L;
		long num7 = num5;
		long num8 = num6;
		((Formatter)(ref val2)).AppendFormatted<long>(num7 - num8);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		long num9 = 1000000000000L;
		long num10 = 3L;
		long num11 = num9;
		long num12 = num10;
		((Formatter)(ref val3)).AppendFormatted<long>(num11 * num12);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		long num13 = 3000000000000L;
		long num14 = 3L;
		long num15 = num13;
		long num16 = num14;
		((Formatter)(ref val4)).AppendFormatted<long>(num15 / num16);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		long num17 = 1000000000001L;
		long num18 = 10L;
		long num19 = num17;
		long num20 = num18;
		((Formatter)(ref val5)).AppendFormatted<long>(num19 % num20);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		long num21 = -7L;
		long num22 = 2L;
		long num23 = num21;
		long num24 = num22;
		((Formatter)(ref val6)).AppendFormatted<long>(num23 / num24);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
