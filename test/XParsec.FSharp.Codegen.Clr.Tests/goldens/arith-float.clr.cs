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
		Formatter formatter = new Formatter(0, 1, Console.Out);
		double num = 1.5;
		double num2 = 2.5;
		double num3 = num;
		double num4 = num2;
		formatter.AppendFormatted(num3 + num4, "F6");
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		double num5 = 3.0;
		double num6 = 1.5;
		double num7 = num5;
		double num8 = num6;
		formatter2.AppendFormatted(num7 - num8, "F6");
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		double num9 = 1.5;
		double num10 = 2.0;
		double num11 = num9;
		double num12 = num10;
		formatter3.AppendFormatted(num11 * num12, "F6");
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		double num13 = 7.5;
		double num14 = 2.5;
		double num15 = num13;
		double num16 = num14;
		formatter4.AppendFormatted(num15 / num16, "F6");
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		double num17 = 7.5;
		double num18 = 2.0;
		double num19 = num17;
		double num20 = num18;
		formatter5.AppendFormatted(num19 % num20, "F6");
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		double num21 = 1.0;
		double num22 = 3.0;
		double num23 = num21;
		double num24 = num22;
		formatter6.AppendFormatted(num23 / num24, "F6");
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
