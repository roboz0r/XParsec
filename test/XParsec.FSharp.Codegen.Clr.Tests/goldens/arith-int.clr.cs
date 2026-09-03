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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		int num = 2;
		int num2 = 3;
		int num3 = num;
		int num4 = num2;
		((Formatter)(ref val)).AppendFormatted<int>(num3 + num4);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		int num5 = 10;
		int num6 = 4;
		int num7 = num5;
		int num8 = num6;
		((Formatter)(ref val2)).AppendFormatted<int>(num7 - num8);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		int num9 = 6;
		int num10 = 7;
		int num11 = num9;
		int num12 = num10;
		((Formatter)(ref val3)).AppendFormatted<int>(num11 * num12);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		int num13 = 100;
		int num14 = 7;
		int num15 = num13;
		int num16 = num14;
		((Formatter)(ref val4)).AppendFormatted<int>(num15 / num16);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		int num17 = 100;
		int num18 = 7;
		int num19 = num17;
		int num20 = num18;
		((Formatter)(ref val5)).AppendFormatted<int>(num19 % num20);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		int num21 = 3;
		int num22 = 10;
		int num23 = num21;
		int num24 = num22;
		((Formatter)(ref val6)).AppendFormatted<int>(num23 - num24);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		int num25 = 7;
		int num26 = 3;
		int num27 = num25;
		int num28 = num26;
		int num29 = num27 - num28;
		int num30 = 2;
		int num31 = num29;
		int num32 = num30;
		((Formatter)(ref val7)).AppendFormatted<int>(num31 - num32);
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		int num33 = 100;
		int num34 = 5;
		int num35 = num33;
		int num36 = num34;
		int num37 = num35 / num36;
		int num38 = 2;
		int num39 = num37;
		int num40 = num38;
		((Formatter)(ref val8)).AppendFormatted<int>(num39 / num40);
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter val9 = default(Formatter);
		((Formatter)(ref val9))..ctor(0, 1, Console.Out);
		int num41 = 2;
		int num42 = 3;
		int num43 = 4;
		int num44 = num42;
		int num45 = num43;
		int num46 = num44 * num45;
		int num47 = num41;
		int num48 = num46;
		((Formatter)(ref val9)).AppendFormatted<int>(num47 + num48);
		((Formatter)(ref val9)).AppendLiteral("\n");
		((Formatter)(ref val9)).Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter val10 = default(Formatter);
		((Formatter)(ref val10))..ctor(0, 1, Console.Out);
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
		((Formatter)(ref val10)).AppendFormatted<int>(num59 - num60);
		((Formatter)(ref val10)).AppendLiteral("\n");
		((Formatter)(ref val10)).Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter val11 = default(Formatter);
		((Formatter)(ref val11))..ctor(0, 1, Console.Out);
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
		((Formatter)(ref val11)).AppendFormatted<int>(num71 + num72);
		((Formatter)(ref val11)).AppendLiteral("\n");
		((Formatter)(ref val11)).Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		Formatter val12 = default(Formatter);
		((Formatter)(ref val12))..ctor(0, 1, Console.Out);
		int num73 = -7;
		int num74 = 2;
		int num75 = num73;
		int num76 = num74;
		((Formatter)(ref val12)).AppendFormatted<int>(num75 / num76);
		((Formatter)(ref val12)).AppendLiteral("\n");
		((Formatter)(ref val12)).Flush();
		ValueTuple valueTuple12 = default(ValueTuple);
		Formatter val13 = default(Formatter);
		((Formatter)(ref val13))..ctor(0, 1, Console.Out);
		int num77 = -7;
		int num78 = 2;
		int num79 = num77;
		int num80 = num78;
		((Formatter)(ref val13)).AppendFormatted<int>(num79 % num80);
		((Formatter)(ref val13)).AppendLiteral("\n");
		((Formatter)(ref val13)).Flush();
		ValueTuple valueTuple13 = default(ValueTuple);
		Formatter val14 = default(Formatter);
		((Formatter)(ref val14))..ctor(0, 1, Console.Out);
		int num81 = 2000000000;
		int num82 = 2000000000;
		int num83 = num81;
		int num84 = num82;
		((Formatter)(ref val14)).AppendFormatted<int>(num83 + num84);
		((Formatter)(ref val14)).AppendLiteral("\n");
		((Formatter)(ref val14)).Flush();
		ValueTuple valueTuple14 = default(ValueTuple);
		Formatter val15 = default(Formatter);
		((Formatter)(ref val15))..ctor(0, 1, Console.Out);
		int num85 = 100000;
		int num86 = 100000;
		int num87 = num85;
		int num88 = num86;
		((Formatter)(ref val15)).AppendFormatted<int>(num87 * num88);
		((Formatter)(ref val15)).AppendLiteral("\n");
		((Formatter)(ref val15)).Flush();
		ValueTuple valueTuple15 = default(ValueTuple);
		return 0;
	}
}
