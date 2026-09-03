/*
// float32 (IEEE 32) — the single-precision width. `%f` types its argument as
// float, so a float32 is printed with `%O` (shortest round-trip stringification).
//
// The last row is the whole point: `0.1f + 0.2f` rounds to the float32 nearest 0.3
// and prints "0.3". Computing it in DOUBLE precision instead prints
// 0.30000000000000004 — close, but not this width's result. The first four rows are
// exactly representable and cannot tell the two apart.
printfn "%O" (1.5f + 2.5f)
printfn "%O" (3.0f - 1.5f)
printfn "%O" (1.5f * 2.0f)
printfn "%O" (7.5f / 2.5f)
printfn "%O" (0.1f + 0.2f)
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
		float num = 1.5f;
		float num2 = 2.5f;
		float num3 = num;
		float num4 = num2;
		formatter.AppendFormatted(num3 + num4);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		float num5 = 3f;
		float num6 = 1.5f;
		float num7 = num5;
		float num8 = num6;
		formatter2.AppendFormatted(num7 - num8);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		float num9 = 1.5f;
		float num10 = 2f;
		float num11 = num9;
		float num12 = num10;
		formatter3.AppendFormatted(num11 * num12);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		float num13 = 7.5f;
		float num14 = 2.5f;
		float num15 = num13;
		float num16 = num14;
		formatter4.AppendFormatted(num15 / num16);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		float num17 = 0.1f;
		float num18 = 0.2f;
		float num19 = num17;
		float num20 = num18;
		formatter5.AppendFormatted(num19 + num20);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
