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
		Formatter formatter = new Formatter(0, 1, Console.Out);
		ushort num = 60000;
		ushort num2 = 10000;
		ushort num3 = num;
		ushort num4 = num2;
		ushort num5 = (ushort)(num3 + num4);
		ushort num6 = num5;
		formatter.AppendFormatted((int)num6);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		ushort num7 = 10;
		ushort num8 = 20;
		ushort num9 = num7;
		ushort num10 = num8;
		ushort num11 = (ushort)(num9 - num10);
		ushort num12 = num11;
		formatter2.AppendFormatted((int)num12);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		ushort num13 = 300;
		ushort num14 = 300;
		ushort num15 = num13;
		ushort num16 = num14;
		ushort num17 = (ushort)(num15 * num16);
		ushort num18 = num17;
		formatter3.AppendFormatted((int)num18);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		ushort num19 = 60000;
		ushort num20 = 3;
		ushort num21 = num19;
		ushort num22 = num20;
		ushort num23 = (ushort)((uint)num21 / (uint)num22);
		ushort num24 = num23;
		formatter4.AppendFormatted((int)num24);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		ushort num25 = 60000;
		ushort num26 = 7;
		ushort num27 = num25;
		ushort num28 = num26;
		ushort num29 = (ushort)((uint)num27 % (uint)num28);
		ushort num30 = num29;
		formatter5.AppendFormatted((int)num30);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		return 0;
	}
}
