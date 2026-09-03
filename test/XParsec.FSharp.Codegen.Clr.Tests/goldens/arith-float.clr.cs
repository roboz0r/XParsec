/*
// float (IEEE 64) — `%f` is the exact reporting channel (6 fractional digits, as
// F#'s default). The last row is the one that would expose an integer-division
// clause hiding under `/`.
printfn "%f" (1.5 + 2.5)
printfn "%f" (3.0 - 1.5)
printfn "%f" (1.5 * 2.0)
printfn "%f" (7.5 / 2.5)
printfn "%f" (7.5 % 2.0)
printfn "%f" (1.0 / 3.0)
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
		double num = 1.5;
		double num2 = 2.5;
		double num3 = num;
		double num4 = num2;
		((Formatter)(ref val)).AppendFormatted<double>(num3 + num4, "F6");
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		double num5 = 3.0;
		double num6 = 1.5;
		double num7 = num5;
		double num8 = num6;
		((Formatter)(ref val2)).AppendFormatted<double>(num7 - num8, "F6");
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		double num9 = 1.5;
		double num10 = 2.0;
		double num11 = num9;
		double num12 = num10;
		((Formatter)(ref val3)).AppendFormatted<double>(num11 * num12, "F6");
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		double num13 = 7.5;
		double num14 = 2.5;
		double num15 = num13;
		double num16 = num14;
		((Formatter)(ref val4)).AppendFormatted<double>(num15 / num16, "F6");
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		double num17 = 7.5;
		double num18 = 2.0;
		double num19 = num17;
		double num20 = num18;
		((Formatter)(ref val5)).AppendFormatted<double>(num19 % num20, "F6");
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		double num21 = 1.0;
		double num22 = 3.0;
		double num23 = num21;
		double num24 = num22;
		((Formatter)(ref val6)).AppendFormatted<double>(num23 / num24, "F6");
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
