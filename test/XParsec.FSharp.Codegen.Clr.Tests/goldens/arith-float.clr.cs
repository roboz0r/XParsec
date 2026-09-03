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
		formatter.AppendFormatted(1.5 + 2.5, "F6");
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(3.0 - 1.5, "F6");
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(1.5 * 2.0, "F6");
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(7.5 / 2.5, "F6");
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(7.5 % 2.0, "F6");
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(1.0 / 3.0, "F6");
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
