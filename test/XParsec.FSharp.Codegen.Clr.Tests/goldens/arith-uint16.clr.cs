/*
// uint16 — the mod-65536 wrap (10us - 20us = 65526us, never -10).
printfn "%d" (int (60000us + 10000us))
printfn "%d" (int (10us - 20us))
printfn "%d" (int (300us * 300us))
printfn "%d" (int (60000us / 3us))
printfn "%d" (int (60000us % 7us))
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
		ushort num = 60000;
		ushort num2 = 10000;
		ushort num3 = num;
		ushort num4 = num2;
		ushort num5 = (ushort)(num3 + num4);
		ushort num6 = num5;
		((Formatter)(ref val)).AppendFormatted<int>((int)num6);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		ushort num7 = 10;
		ushort num8 = 20;
		ushort num9 = num7;
		ushort num10 = num8;
		ushort num11 = (ushort)(num9 - num10);
		ushort num12 = num11;
		((Formatter)(ref val2)).AppendFormatted<int>((int)num12);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		ushort num13 = 300;
		ushort num14 = 300;
		ushort num15 = num13;
		ushort num16 = num14;
		ushort num17 = (ushort)(num15 * num16);
		ushort num18 = num17;
		((Formatter)(ref val3)).AppendFormatted<int>((int)num18);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		ushort num19 = 60000;
		ushort num20 = 3;
		ushort num21 = num19;
		ushort num22 = num20;
		ushort num23 = (ushort)((uint)num21 / (uint)num22);
		ushort num24 = num23;
		((Formatter)(ref val4)).AppendFormatted<int>((int)num24);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		ushort num25 = 60000;
		ushort num26 = 7;
		ushort num27 = num25;
		ushort num28 = num26;
		ushort num29 = (ushort)((uint)num27 % (uint)num28);
		ushort num30 = num29;
		((Formatter)(ref val5)).AppendFormatted<int>((int)num30);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
