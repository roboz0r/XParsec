/*
// Integer `/` must TRUNCATE. A target whose `/` is true division leaves 3.333…
// where 3 belongs, and the fraction survives into the next operation — so
// `x / y * y` is the probe no report-site conversion can launder: it is 9 at every
// integral width, and 10 wherever the quotient kept its fraction.
printfn "%d" (int (10uy / 3uy * 3uy))
printfn "%d" (int (7uy / 2uy * 2uy))
printfn "%d" (int (10us / 3us * 3us))
printfn "%d" (int (10y / 3y * 3y))
printfn "%d" (int (10s / 3s * 3s))
printfn "%u" (int (10u / 3u * 3u))
printfn "%d" (10 / 3 * 3)
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
		formatter.AppendFormatted(9);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(6);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(9);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(9);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(9);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendUnsigned(9uL, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendFormatted(10 / 3 * 3);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		return 0;
	}
}
