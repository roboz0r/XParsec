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
		Formatter formatter = new Formatter(0, 1, Console.Out);
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
		formatter.AppendFormatted((int)b10);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
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
		formatter2.AppendFormatted((int)b20);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
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
		formatter3.AppendFormatted((int)num10);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
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
		formatter4.AppendFormatted((int)b30);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
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
		formatter5.AppendFormatted((int)num20);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
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
		formatter6.AppendUnsigned(num30, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		int num31 = 10;
		int num32 = 3;
		int num33 = num31;
		int num34 = num32;
		int num35 = num33 / num34;
		int num36 = 3;
		int num37 = num35;
		int num38 = num36;
		formatter7.AppendFormatted(num37 * num38);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		return 0;
	}
}
