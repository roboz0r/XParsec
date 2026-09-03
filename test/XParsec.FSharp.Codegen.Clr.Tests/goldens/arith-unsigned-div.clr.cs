/*
// Integer `/` must TRUNCATE. A target whose `/` is true division leaves 3.333…
// where 3 belongs, and the fraction survives into the next operation — so
// `x / y * y` is the probe no report-site conversion can launder: it is 9 at every
// integral width, and 10 wherever the quotient kept its fraction.
printfn "%d" (int (10uy / 3uy * 3uy))
printfn "%d" (int (7uy / 2uy * 2uy))
printfn "%d" (int (10us / 3us * 3us))
printfn "%d" (int (10y / 3y * 3y))
printfn "%d" (int (10s / 3s * 3s))
printfn "%u" (int (10u / 3u * 3u))
printfn "%d" (10 / 3 * 3)
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
		byte b2 = 3;
		byte b3 = b;
		byte b4 = b2;
		byte b5 = (byte)((uint)b3 / (uint)b4);
		byte b6 = 3;
		byte b7 = b5;
		byte b8 = b6;
		byte b9 = (byte)(b7 * b8);
		byte b10 = b9;
		((Formatter)(ref val)).AppendFormatted<int>((int)b10);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		byte b11 = 7;
		byte b12 = 2;
		byte b13 = b11;
		byte b14 = b12;
		byte b15 = (byte)((uint)b13 / (uint)b14);
		byte b16 = 2;
		byte b17 = b15;
		byte b18 = b16;
		byte b19 = (byte)(b17 * b18);
		byte b20 = b19;
		((Formatter)(ref val2)).AppendFormatted<int>((int)b20);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		ushort num = 10;
		ushort num2 = 3;
		ushort num3 = num;
		ushort num4 = num2;
		ushort num5 = (ushort)((uint)num3 / (uint)num4);
		ushort num6 = 3;
		ushort num7 = num5;
		ushort num8 = num6;
		ushort num9 = (ushort)(num7 * num8);
		ushort num10 = num9;
		((Formatter)(ref val3)).AppendFormatted<int>((int)num10);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		sbyte b21 = 10;
		sbyte b22 = 3;
		sbyte b23 = b21;
		sbyte b24 = b22;
		sbyte b25 = (sbyte)(b23 / b24);
		sbyte b26 = 3;
		sbyte b27 = b25;
		sbyte b28 = b26;
		sbyte b29 = (sbyte)(b27 * b28);
		sbyte b30 = b29;
		((Formatter)(ref val4)).AppendFormatted<int>((int)b30);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		short num11 = 10;
		short num12 = 3;
		short num13 = num11;
		short num14 = num12;
		short num15 = (short)(num13 / num14);
		short num16 = 3;
		short num17 = num15;
		short num18 = num16;
		short num19 = (short)(num17 * num18);
		short num20 = num19;
		((Formatter)(ref val5)).AppendFormatted<int>((int)num20);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		uint num21 = 10u;
		uint num22 = 3u;
		uint num23 = num21;
		uint num24 = num22;
		uint num25 = num23 / num24;
		uint num26 = 3u;
		uint num27 = num25;
		uint num28 = num26;
		uint num29 = num27 * num28;
		uint num30 = num29;
		((Formatter)(ref val6)).AppendUnsigned((ulong)num30, 0);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		int num31 = 10;
		int num32 = 3;
		int num33 = num31;
		int num34 = num32;
		int num35 = num33 / num34;
		int num36 = 3;
		int num37 = num35;
		int num38 = num36;
		((Formatter)(ref val7)).AppendFormatted<int>(num37 * num38);
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		return 0;
	}
}
