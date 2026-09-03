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
		int num = 2;
		int num2 = 3;
		int num3 = num;
		int num4 = num2;
		formatter.AppendFormatted(num3 + num4);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		int num5 = 10;
		int num6 = 4;
		int num7 = num5;
		int num8 = num6;
		formatter2.AppendFormatted(num7 - num8);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		int num9 = 6;
		int num10 = 7;
		int num11 = num9;
		int num12 = num10;
		formatter3.AppendFormatted(num11 * num12);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		int num13 = 100;
		int num14 = 7;
		int num15 = num13;
		int num16 = num14;
		formatter4.AppendFormatted(num15 / num16);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		int num17 = 100;
		int num18 = 7;
		int num19 = num17;
		int num20 = num18;
		formatter5.AppendFormatted(num19 % num20);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		int num21 = 3;
		int num22 = 10;
		int num23 = num21;
		int num24 = num22;
		formatter6.AppendFormatted(num23 - num24);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		int num25 = 7;
		int num26 = 3;
		int num27 = num25;
		int num28 = num26;
		int num29 = num27 - num28;
		int num30 = 2;
		int num31 = num29;
		int num32 = num30;
		formatter7.AppendFormatted(num31 - num32);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		int num33 = 100;
		int num34 = 5;
		int num35 = num33;
		int num36 = num34;
		int num37 = num35 / num36;
		int num38 = 2;
		int num39 = num37;
		int num40 = num38;
		formatter8.AppendFormatted(num39 / num40);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		int num41 = 2;
		int num42 = 3;
		int num43 = 4;
		int num44 = num42;
		int num45 = num43;
		int num46 = num44 * num45;
		int num47 = num41;
		int num48 = num46;
		formatter9.AppendFormatted(num47 + num48);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		int num49 = 10;
		int num50 = 5;
		int num51 = num49;
		int num52 = num50;
		int num53 = num51 + num52;
		int num54 = 2;
		int num55 = num53;
		int num56 = num54;
		int num57 = num55 * num56;
		int num58 = 3;
		int num59 = num57;
		int num60 = num58;
		formatter10.AppendFormatted(num59 - num60);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		int num61 = 100;
		int num62 = 2;
		int num63 = 3;
		int num64 = num62;
		int num65 = num63;
		int num66 = num64 * num65;
		int num67 = num61;
		int num68 = num66;
		int num69 = num67 - num68;
		int num70 = 1;
		int num71 = num69;
		int num72 = num70;
		formatter11.AppendFormatted(num71 + num72);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		Formatter formatter12 = new Formatter(0, 1, Console.Out);
		int num73 = -7;
		int num74 = 2;
		int num75 = num73;
		int num76 = num74;
		formatter12.AppendFormatted(num75 / num76);
		formatter12.AppendLiteral("\n");
		formatter12.Flush();
		ValueTuple valueTuple12 = default(ValueTuple);
		Formatter formatter13 = new Formatter(0, 1, Console.Out);
		int num77 = -7;
		int num78 = 2;
		int num79 = num77;
		int num80 = num78;
		formatter13.AppendFormatted(num79 % num80);
		formatter13.AppendLiteral("\n");
		formatter13.Flush();
		ValueTuple valueTuple13 = default(ValueTuple);
		Formatter formatter14 = new Formatter(0, 1, Console.Out);
		int num81 = 2000000000;
		int num82 = 2000000000;
		int num83 = num81;
		int num84 = num82;
		formatter14.AppendFormatted(num83 + num84);
		formatter14.AppendLiteral("\n");
		formatter14.Flush();
		ValueTuple valueTuple14 = default(ValueTuple);
		Formatter formatter15 = new Formatter(0, 1, Console.Out);
		int num85 = 100000;
		int num86 = 100000;
		int num87 = num85;
		int num88 = num86;
		formatter15.AppendFormatted(num87 * num88);
		formatter15.AppendLiteral("\n");
		formatter15.Flush();
		ValueTuple valueTuple15 = default(ValueTuple);
		return 0;
	}
}
