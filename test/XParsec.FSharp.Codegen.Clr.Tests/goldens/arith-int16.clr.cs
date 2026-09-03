/*
// int16 — the sign-extending 16-bit wrap (30000s + 10000s = 40000 truncates to
// -25536s). Reported through `int (…)`, as for sbyte: a negative int16 literal has
// no `TConstValue` representation.
printfn "%d" (int (30000s + 10000s))
printfn "%d" (int (0s - 30000s))
printfn "%d" (int (300s * 300s))
printfn "%d" (int (30000s / 3s))
printfn "%d" (int (30000s % 7s))
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
		short num = 30000;
		short num2 = 10000;
		short num3 = num;
		short num4 = num2;
		short num5 = (short)(num3 + num4);
		short num6 = num5;
		formatter.AppendFormatted((int)num6);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		short num7 = 0;
		short num8 = 30000;
		short num9 = num7;
		short num10 = num8;
		short num11 = (short)(num9 - num10);
		short num12 = num11;
		formatter2.AppendFormatted((int)num12);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		short num13 = 300;
		short num14 = 300;
		short num15 = num13;
		short num16 = num14;
		short num17 = (short)(num15 * num16);
		short num18 = num17;
		formatter3.AppendFormatted((int)num18);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		short num19 = 30000;
		short num20 = 3;
		short num21 = num19;
		short num22 = num20;
		short num23 = (short)(num21 / num22);
		short num24 = num23;
		formatter4.AppendFormatted((int)num24);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		short num25 = 30000;
		short num26 = 7;
		short num27 = num25;
		short num28 = num26;
		short num29 = (short)(num27 % num28);
		short num30 = num29;
		formatter5.AppendFormatted((int)num30);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
