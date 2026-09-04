/*
// byte — the mod-256 wrap. `int (…)` is the reporting channel (`%d` types its
// argument as int32), and it is a WIDENING conversion here: it cannot repair a
// missing 8-bit mask, so a backend whose `+` leaves 300 on the wire prints 300.
printfn "%d" (int (10uy + 20uy))
printfn "%d" (int (200uy + 100uy))
printfn "%d" (int (10uy - 20uy))
printfn "%d" (int (20uy * 20uy))
printfn "%d" (int (200uy / 3uy))
printfn "%d" (int (200uy % 7uy))
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
		byte b = (byte)30;
		formatter.AppendFormatted((int)b);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		byte b2 = (byte)44;
		formatter2.AppendFormatted((int)b2);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		byte b3 = (byte)246;
		formatter3.AppendFormatted((int)b3);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		byte b4 = (byte)144;
		formatter4.AppendFormatted((int)b4);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		byte b5 = (byte)66;
		formatter5.AppendFormatted((int)b5);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		byte b6 = (byte)4;
		formatter6.AppendFormatted((int)b6);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		return 0;
	}
}
