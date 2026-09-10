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
		formatter.AppendUnsigned(4000000001uL, 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendUnsigned(705032704uL, 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendUnsigned((uint)(-1), 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendUnsigned(1410065408uL, 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendUnsigned(2000000000uL, 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendUnsigned(1uL, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendUnsigned((uint)(-1) / 2u, 0);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		formatter8.AppendUnsigned((uint)(-1) % 10u, 0);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		return 0;
	}
}
