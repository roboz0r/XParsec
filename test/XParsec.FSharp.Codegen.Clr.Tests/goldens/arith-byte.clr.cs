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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		byte b = 10;
		byte b2 = 20;
		byte b3 = b;
		byte b4 = b2;
		byte b5 = (byte)(b3 + b4);
		byte b6 = b5;
		((Formatter)(ref val)).AppendFormatted<int>((int)b6);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		byte b7 = 200;
		byte b8 = 100;
		byte b9 = b7;
		byte b10 = b8;
		byte b11 = (byte)(b9 + b10);
		byte b12 = b11;
		((Formatter)(ref val2)).AppendFormatted<int>((int)b12);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		byte b13 = 10;
		byte b14 = 20;
		byte b15 = b13;
		byte b16 = b14;
		byte b17 = (byte)(b15 - b16);
		byte b18 = b17;
		((Formatter)(ref val3)).AppendFormatted<int>((int)b18);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		byte b19 = 20;
		byte b20 = 20;
		byte b21 = b19;
		byte b22 = b20;
		byte b23 = (byte)(b21 * b22);
		byte b24 = b23;
		((Formatter)(ref val4)).AppendFormatted<int>((int)b24);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		byte b25 = 200;
		byte b26 = 3;
		byte b27 = b25;
		byte b28 = b26;
		byte b29 = (byte)((uint)b27 / (uint)b28);
		byte b30 = b29;
		((Formatter)(ref val5)).AppendFormatted<int>((int)b30);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		byte b31 = 200;
		byte b32 = 7;
		byte b33 = b31;
		byte b34 = b32;
		byte b35 = (byte)((uint)b33 % (uint)b34);
		byte b36 = b35;
		((Formatter)(ref val6)).AppendFormatted<int>((int)b36);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
