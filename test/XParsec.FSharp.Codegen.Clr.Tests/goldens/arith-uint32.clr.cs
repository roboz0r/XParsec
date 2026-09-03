/*
// uint32 — the mod-2^32 wrap and the UNSIGNED `/` and `%`. `int (…)` reinterprets
// the bits (it does not widen), and `%u` reads them back unsigned, so the pair is
// a lossless report of a uint32 value through the int32-typed `%d`/`%u` hole.
//
// The last two rows are the ones a 32-bit-masking report cannot fake: `0u - 1u` is
// 4294967295, and DIVIDING that must use the unsigned quotient. A backend that
// leaves -1 on the wire divides a negative and lands nowhere near.
printfn "%u" (int (4000000000u + 1u))
printfn "%u" (int (4000000000u + 1000000000u))
printfn "%u" (int (0u - 1u))
printfn "%u" (int (100000u * 100000u))
printfn "%u" (int (4000000000u / 2u))
printfn "%u" (int (4000000000u % 3u))
printfn "%u" (int ((0u - 1u) / 2u))
printfn "%u" (int ((0u - 1u) % 10u))
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
		uint num = 4000000000u;
		uint num2 = 1u;
		uint num3 = num;
		uint num4 = num2;
		uint num5 = num3 + num4;
		uint num6 = num5;
		formatter.AppendUnsigned(num6, 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		uint num7 = 4000000000u;
		uint num8 = 1000000000u;
		uint num9 = num7;
		uint num10 = num8;
		uint num11 = num9 + num10;
		uint num12 = num11;
		formatter2.AppendUnsigned(num12, 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		uint num13 = 0u;
		uint num14 = 1u;
		uint num15 = num13;
		uint num16 = num14;
		uint num17 = num15 - num16;
		uint num18 = num17;
		formatter3.AppendUnsigned(num18, 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		uint num19 = 100000u;
		uint num20 = 100000u;
		uint num21 = num19;
		uint num22 = num20;
		uint num23 = num21 * num22;
		uint num24 = num23;
		formatter4.AppendUnsigned(num24, 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		uint num25 = 4000000000u;
		uint num26 = 2u;
		uint num27 = num25;
		uint num28 = num26;
		uint num29 = num27 / num28;
		uint num30 = num29;
		formatter5.AppendUnsigned(num30, 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		uint num31 = 4000000000u;
		uint num32 = 3u;
		uint num33 = num31;
		uint num34 = num32;
		uint num35 = num33 % num34;
		uint num36 = num35;
		formatter6.AppendUnsigned(num36, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		uint num37 = 0u;
		uint num38 = 1u;
		uint num39 = num37;
		uint num40 = num38;
		uint num41 = num39 - num40;
		uint num42 = 2u;
		uint num43 = num41;
		uint num44 = num42;
		uint num45 = num43 / num44;
		uint num46 = num45;
		formatter7.AppendUnsigned(num46, 0);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		uint num47 = 0u;
		uint num48 = 1u;
		uint num49 = num47;
		uint num50 = num48;
		uint num51 = num49 - num50;
		uint num52 = 10u;
		uint num53 = num51;
		uint num54 = num52;
		uint num55 = num53 % num54;
		uint num56 = num55;
		formatter8.AppendUnsigned(num56, 0);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
