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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		short num = 30000;
		short num2 = 10000;
		short num3 = num;
		short num4 = num2;
		short num5 = (short)(num3 + num4);
		short num6 = num5;
		((Formatter)(ref val)).AppendFormatted<int>((int)num6);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		short num7 = 0;
		short num8 = 30000;
		short num9 = num7;
		short num10 = num8;
		short num11 = (short)(num9 - num10);
		short num12 = num11;
		((Formatter)(ref val2)).AppendFormatted<int>((int)num12);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		short num13 = 300;
		short num14 = 300;
		short num15 = num13;
		short num16 = num14;
		short num17 = (short)(num15 * num16);
		short num18 = num17;
		((Formatter)(ref val3)).AppendFormatted<int>((int)num18);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		short num19 = 30000;
		short num20 = 3;
		short num21 = num19;
		short num22 = num20;
		short num23 = (short)(num21 / num22);
		short num24 = num23;
		((Formatter)(ref val4)).AppendFormatted<int>((int)num24);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		short num25 = 30000;
		short num26 = 7;
		short num27 = num25;
		short num28 = num26;
		short num29 = (short)(num27 % num28);
		short num30 = num29;
		((Formatter)(ref val5)).AppendFormatted<int>((int)num30);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
