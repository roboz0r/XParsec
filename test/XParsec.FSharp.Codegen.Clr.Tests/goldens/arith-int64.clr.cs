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
		long num = 1000000000000L;
		long num2 = 1L;
		long num3 = num;
		long num4 = num2;
		formatter.AppendFormatted(num3 + num4);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		long num5 = 1000000000000L;
		long num6 = 1L;
		long num7 = num5;
		long num8 = num6;
		formatter2.AppendFormatted(num7 - num8);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		long num9 = 1000000000000L;
		long num10 = 3L;
		long num11 = num9;
		long num12 = num10;
		formatter3.AppendFormatted(num11 * num12);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		long num13 = 3000000000000L;
		long num14 = 3L;
		long num15 = num13;
		long num16 = num14;
		formatter4.AppendFormatted(num15 / num16);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		long num17 = 1000000000001L;
		long num18 = 10L;
		long num19 = num17;
		long num20 = num18;
		formatter5.AppendFormatted(num19 % num20);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		long num21 = -7L;
		long num22 = 2L;
		long num23 = num21;
		long num24 = num22;
		formatter6.AppendFormatted(num23 / num24);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
