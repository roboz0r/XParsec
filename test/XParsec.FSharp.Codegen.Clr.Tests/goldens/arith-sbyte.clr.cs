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
		sbyte b = 100;
		sbyte b2 = 100;
		sbyte b3 = b;
		sbyte b4 = b2;
		sbyte b5 = (sbyte)(b3 + b4);
		sbyte b6 = b5;
		formatter.AppendFormatted((int)b6);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		sbyte b7 = 0;
		sbyte b8 = 100;
		sbyte b9 = b7;
		sbyte b10 = b8;
		sbyte b11 = (sbyte)(b9 - b10);
		sbyte b12 = b11;
		formatter2.AppendFormatted((int)b12);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		sbyte b13 = 100;
		sbyte b14 = 2;
		sbyte b15 = b13;
		sbyte b16 = b14;
		sbyte b17 = (sbyte)(b15 * b16);
		sbyte b18 = b17;
		formatter3.AppendFormatted((int)b18);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		sbyte b19 = 100;
		sbyte b20 = 3;
		sbyte b21 = b19;
		sbyte b22 = b20;
		sbyte b23 = (sbyte)(b21 / b22);
		sbyte b24 = b23;
		formatter4.AppendFormatted((int)b24);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		sbyte b25 = 100;
		sbyte b26 = 7;
		sbyte b27 = b25;
		sbyte b28 = b26;
		sbyte b29 = (sbyte)(b27 % b28);
		sbyte b30 = b29;
		formatter5.AppendFormatted((int)b30);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
