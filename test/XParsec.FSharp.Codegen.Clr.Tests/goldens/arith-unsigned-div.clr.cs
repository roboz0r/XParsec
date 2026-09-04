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
		byte b = (byte)3;
		byte b2 = (byte)(b * 3);
		formatter.AppendFormatted((int)b2);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		byte b3 = (byte)3;
		byte b4 = (byte)(b3 * 2);
		formatter2.AppendFormatted((int)b4);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		ushort num = (ushort)3;
		ushort num2 = (ushort)(num * 3);
		formatter3.AppendFormatted((int)num2);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		sbyte b5 = (sbyte)3;
		sbyte b6 = (sbyte)(b5 * 3);
		formatter4.AppendFormatted((int)b6);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		short num3 = (short)3;
		short num4 = (short)(num3 * 3);
		formatter5.AppendFormatted((int)num4);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		uint num5 = 10u / 3u;
		uint num6 = num5 * 3;
		formatter6.AppendUnsigned(num6, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		int num7 = 10 / 3;
		formatter7.AppendFormatted(num7 * 3);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		return 0;
	}
}
