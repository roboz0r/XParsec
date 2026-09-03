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
		uint num = 4000000001u;
		formatter.AppendUnsigned(num, 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		uint num2 = 705032704u;
		formatter2.AppendUnsigned(num2, 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		uint num3 = (uint)(-1);
		formatter3.AppendUnsigned(num3, 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		uint num4 = 1410065408u;
		formatter4.AppendUnsigned(num4, 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		uint num5 = 4000000000u / 2u;
		formatter5.AppendUnsigned(num5, 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		uint num6 = 4000000000u % 3u;
		formatter6.AppendUnsigned(num6, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		uint num7 = (uint)(-1);
		uint num8 = num7 / 2;
		formatter7.AppendUnsigned(num8, 0);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		uint num9 = (uint)(-1);
		uint num10 = num9 % 10;
		formatter8.AppendUnsigned(num10, 0);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
