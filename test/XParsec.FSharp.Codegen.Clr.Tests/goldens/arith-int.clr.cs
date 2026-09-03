/*
// int32 — the five binary clauses at the native width, plus precedence and
// left-associativity (a break in operator routing or the Pratt RHS shows up here
// first), plus the two overflow rows that pin the 32-bit wrap.
printfn "%d" (2 + 3)
printfn "%d" (10 - 4)
printfn "%d" (6 * 7)
printfn "%d" (100 / 7)
printfn "%d" (100 % 7)
printfn "%d" (3 - 10)
printfn "%d" (7 - 3 - 2)
printfn "%d" (100 / 5 / 2)
printfn "%d" (2 + 3 * 4)
printfn "%d" ((10 + 5) * 2 - 3)
printfn "%d" (100 - 2 * 3 + 1)
printfn "%d" (-7 / 2)
printfn "%d" (-7 % 2)
printfn "%d" (2000000000 + 2000000000)
printfn "%d" (100000 * 100000)
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
		formatter.AppendFormatted(2 + 3);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(10 - 4);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(6 * 7);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(100 / 7);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(100 % 7);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(3 - 10);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		int num = 7 - 3;
		formatter7.AppendFormatted(num - 2);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		int num2 = 100 / 5;
		formatter8.AppendFormatted(num2 / 2);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		int num3 = 3 * 4;
		formatter9.AppendFormatted(2 + num3);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		int num4 = 10 + 5;
		int num5 = num4 * 2;
		formatter10.AppendFormatted(num5 - 3);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		int num6 = 2 * 3;
		int num7 = 100 - num6;
		formatter11.AppendFormatted(num7 + 1);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		Formatter formatter12 = new Formatter(0, 1, Console.Out);
		formatter12.AppendFormatted(-7 / 2);
		formatter12.AppendLiteral("\n");
		formatter12.Flush();
		ValueTuple valueTuple12 = default(ValueTuple);
		Formatter formatter13 = new Formatter(0, 1, Console.Out);
		formatter13.AppendFormatted(-7 % 2);
		formatter13.AppendLiteral("\n");
		formatter13.Flush();
		ValueTuple valueTuple13 = default(ValueTuple);
		Formatter formatter14 = new Formatter(0, 1, Console.Out);
		formatter14.AppendFormatted(unchecked(2000000000 + 2000000000));
		formatter14.AppendLiteral("\n");
		formatter14.Flush();
		ValueTuple valueTuple14 = default(ValueTuple);
		Formatter formatter15 = new Formatter(0, 1, Console.Out);
		formatter15.AppendFormatted(unchecked(100000 * 100000));
		formatter15.AppendLiteral("\n");
		formatter15.Flush();
		ValueTuple valueTuple15 = default(ValueTuple);
		return 0;
	}
}
