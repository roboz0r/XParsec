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
		byte b = 10;
		byte b2 = 20;
		byte b3 = b;
		byte b4 = b2;
		byte b5 = (byte)(b3 + b4);
		byte b6 = b5;
		formatter.AppendFormatted((int)b6);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		byte b7 = 200;
		byte b8 = 100;
		byte b9 = b7;
		byte b10 = b8;
		byte b11 = (byte)(b9 + b10);
		byte b12 = b11;
		formatter2.AppendFormatted((int)b12);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		byte b13 = 10;
		byte b14 = 20;
		byte b15 = b13;
		byte b16 = b14;
		byte b17 = (byte)(b15 - b16);
		byte b18 = b17;
		formatter3.AppendFormatted((int)b18);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		byte b19 = 20;
		byte b20 = 20;
		byte b21 = b19;
		byte b22 = b20;
		byte b23 = (byte)(b21 * b22);
		byte b24 = b23;
		formatter4.AppendFormatted((int)b24);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		byte b25 = 200;
		byte b26 = 3;
		byte b27 = b25;
		byte b28 = b26;
		byte b29 = (byte)((uint)b27 / (uint)b28);
		byte b30 = b29;
		formatter5.AppendFormatted((int)b30);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		byte b31 = 200;
		byte b32 = 7;
		byte b33 = b31;
		byte b34 = b32;
		byte b35 = (byte)((uint)b33 % (uint)b34);
		byte b36 = b35;
		formatter6.AppendFormatted((int)b36);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
