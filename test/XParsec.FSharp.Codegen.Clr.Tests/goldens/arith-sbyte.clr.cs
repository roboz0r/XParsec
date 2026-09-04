/*
// sbyte — the sign-extending 8-bit wrap (100y + 100y = 200 truncates to -56y).
// The result must be reported through `int (…)`: a NEGATIVE sbyte literal has no
// `TConstValue` representation, so `-56y` cannot be written as an expected value,
// but `int (100y + 100y)` prints -56 and reads the same truncation.
printfn "%d" (int (100y + 100y))
printfn "%d" (int (0y - 100y))
printfn "%d" (int (100y * 2y))
printfn "%d" (int (100y / 3y))
printfn "%d" (int (100y % 7y))
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
		sbyte b = (sbyte)(-56);
		formatter.AppendFormatted((int)b);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		sbyte b2 = (sbyte)(-100);
		formatter2.AppendFormatted((int)b2);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		sbyte b3 = (sbyte)(-56);
		formatter3.AppendFormatted((int)b3);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		sbyte b4 = (sbyte)33;
		formatter4.AppendFormatted((int)b4);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		sbyte b5 = (sbyte)2;
		formatter5.AppendFormatted((int)b5);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		return 0;
	}
}
