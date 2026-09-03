/*
// The bitwise family per width. Each width now DECLARES `&&& ||| ^^^ ~~~ <<< >>>` on
// itself, so each has its own body — where a single width-blind opcode used to serve
// them all. These are the rows that tell the two apart.
//
// The complement and the right shift carry the weight. `~~~` leaves an unsigned width
// unless masked back (`~~~0uy` is 255, not -1), and `>>>` must zero-fill on an unsigned
// width and sign-extend on a signed one — the same `shr.un`/`shr` split CIL makes, and
// which JS spells `>>>`/`>>`.
//
// Negative operands are written `(0 - n)`: a negative literal at a narrow width has no
// `TConstValue` representation (see `arith-sbyte.fs`). Reported through `int (…)` /
// `%O`, as the arith programs are.

// int32 — the reference width.
printfn "%d" (13 &&& 11)
printfn "%d" (13 ||| 11)
printfn "%d" (13 ^^^ 11)
printfn "%d" (~~~0)
printfn "%d" (1 <<< 4)
printfn "%d" (208 >>> 2)
printfn "%d" ((0 - 16) >>> 2)

// byte — unsigned 8, so `~~~` masks back and `>>>` zero-fills.
printfn "%d" (int (13uy &&& 11uy))
printfn "%d" (int (13uy ||| 11uy))
printfn "%d" (int (13uy ^^^ 11uy))
printfn "%d" (int (~~~0uy))
printfn "%d" (int (200uy >>> 4))
printfn "%d" (int (3uy <<< 2))

// sbyte — signed 8, so `>>>` sign-extends.
printfn "%d" (int (~~~0y))
printfn "%d" (int ((0y - 16y) >>> 2))
printfn "%d" (int (3y <<< 2))

// int16 / uint16.
printfn "%d" (int (~~~0s))
printfn "%d" (int ((0s - 16s) >>> 2))
printfn "%d" (int (~~~0us))
printfn "%d" (int (60000us >>> 4))

// uint32 — the top-bit-set rows, which a signed read gets wrong.
printfn "%u" (int (~~~0u))
printfn "%u" (int (4000000000u >>> 8))
printfn "%u" (int (4000000000u &&& 4278190080u))

// int64 / uint64.
printfn "%O" (13L &&& 11L)
printfn "%O" (~~~0L)
printfn "%O" ((0L - 16L) >>> 2)
printfn "%O" (1L <<< 40)
printfn "%O" (13UL ||| 3UL)
printfn "%O" (~~~0UL)
printfn "%O" ((0UL - 1UL) >>> 8)
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
		formatter.AppendFormatted(0xD & 0xB);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(0xD | 0xB);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(0xD ^ 0xB);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(~0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(1 << 4);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(208 >> 2);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		int num = -16;
		formatter7.AppendFormatted(num >> 2);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		byte b = 9;
		formatter8.AppendFormatted((int)b);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		byte b2 = 15;
		formatter9.AppendFormatted((int)b2);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		byte b3 = 6;
		formatter10.AppendFormatted((int)b3);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		byte b4 = byte.MaxValue;
		formatter11.AppendFormatted((int)b4);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		Formatter formatter12 = new Formatter(0, 1, Console.Out);
		byte b5 = 12;
		formatter12.AppendFormatted((int)b5);
		formatter12.AppendLiteral("\n");
		formatter12.Flush();
		ValueTuple valueTuple12 = default(ValueTuple);
		Formatter formatter13 = new Formatter(0, 1, Console.Out);
		byte b6 = 12;
		formatter13.AppendFormatted((int)b6);
		formatter13.AppendLiteral("\n");
		formatter13.Flush();
		ValueTuple valueTuple13 = default(ValueTuple);
		Formatter formatter14 = new Formatter(0, 1, Console.Out);
		sbyte b7 = -1;
		formatter14.AppendFormatted((int)b7);
		formatter14.AppendLiteral("\n");
		formatter14.Flush();
		ValueTuple valueTuple14 = default(ValueTuple);
		Formatter formatter15 = new Formatter(0, 1, Console.Out);
		sbyte b8 = (sbyte)(-16);
		sbyte b9 = (sbyte)(b8 >> 2);
		formatter15.AppendFormatted((int)b9);
		formatter15.AppendLiteral("\n");
		formatter15.Flush();
		ValueTuple valueTuple15 = default(ValueTuple);
		Formatter formatter16 = new Formatter(0, 1, Console.Out);
		sbyte b10 = 12;
		formatter16.AppendFormatted((int)b10);
		formatter16.AppendLiteral("\n");
		formatter16.Flush();
		ValueTuple valueTuple16 = default(ValueTuple);
		Formatter formatter17 = new Formatter(0, 1, Console.Out);
		short num2 = -1;
		formatter17.AppendFormatted((int)num2);
		formatter17.AppendLiteral("\n");
		formatter17.Flush();
		ValueTuple valueTuple17 = default(ValueTuple);
		Formatter formatter18 = new Formatter(0, 1, Console.Out);
		short num3 = (short)(-16);
		short num4 = (short)(num3 >> 2);
		formatter18.AppendFormatted((int)num4);
		formatter18.AppendLiteral("\n");
		formatter18.Flush();
		ValueTuple valueTuple18 = default(ValueTuple);
		Formatter formatter19 = new Formatter(0, 1, Console.Out);
		ushort num5 = ushort.MaxValue;
		formatter19.AppendFormatted((int)num5);
		formatter19.AppendLiteral("\n");
		formatter19.Flush();
		ValueTuple valueTuple19 = default(ValueTuple);
		Formatter formatter20 = new Formatter(0, 1, Console.Out);
		ushort num6 = 3750;
		formatter20.AppendFormatted((int)num6);
		formatter20.AppendLiteral("\n");
		formatter20.Flush();
		ValueTuple valueTuple20 = default(ValueTuple);
		Formatter formatter21 = new Formatter(0, 1, Console.Out);
		uint num7 = uint.MaxValue;
		formatter21.AppendUnsigned(num7, 0);
		formatter21.AppendLiteral("\n");
		formatter21.Flush();
		ValueTuple valueTuple21 = default(ValueTuple);
		Formatter formatter22 = new Formatter(0, 1, Console.Out);
		uint num8 = 4000000000u >> 8;
		formatter22.AppendUnsigned(num8, 0);
		formatter22.AppendLiteral("\n");
		formatter22.Flush();
		ValueTuple valueTuple22 = default(ValueTuple);
		Formatter formatter23 = new Formatter(0, 1, Console.Out);
		uint num9 = 3992977408u;
		formatter23.AppendUnsigned(num9, 0);
		formatter23.AppendLiteral("\n");
		formatter23.Flush();
		ValueTuple valueTuple23 = default(ValueTuple);
		Formatter formatter24 = new Formatter(0, 1, Console.Out);
		formatter24.AppendFormatted(0xDL & 0xBL);
		formatter24.AppendLiteral("\n");
		formatter24.Flush();
		ValueTuple valueTuple24 = default(ValueTuple);
		Formatter formatter25 = new Formatter(0, 1, Console.Out);
		formatter25.AppendFormatted(~0L);
		formatter25.AppendLiteral("\n");
		formatter25.Flush();
		ValueTuple valueTuple25 = default(ValueTuple);
		Formatter formatter26 = new Formatter(0, 1, Console.Out);
		long num10 = -16L;
		formatter26.AppendFormatted(num10 >> 2);
		formatter26.AppendLiteral("\n");
		formatter26.Flush();
		ValueTuple valueTuple26 = default(ValueTuple);
		Formatter formatter27 = new Formatter(0, 1, Console.Out);
		formatter27.AppendFormatted(1L << 40);
		formatter27.AppendLiteral("\n");
		formatter27.Flush();
		ValueTuple valueTuple27 = default(ValueTuple);
		Formatter formatter28 = new Formatter(0, 1, Console.Out);
		formatter28.AppendFormatted(15uL);
		formatter28.AppendLiteral("\n");
		formatter28.Flush();
		ValueTuple valueTuple28 = default(ValueTuple);
		Formatter formatter29 = new Formatter(0, 1, Console.Out);
		formatter29.AppendFormatted(ulong.MaxValue);
		formatter29.AppendLiteral("\n");
		formatter29.Flush();
		ValueTuple valueTuple29 = default(ValueTuple);
		Formatter formatter30 = new Formatter(0, 1, Console.Out);
		ulong num11 = (ulong)(-1L);
		formatter30.AppendFormatted(num11 >> 8);
		formatter30.AppendLiteral("\n");
		formatter30.Flush();
		ValueTuple valueTuple30 = default(ValueTuple);
		return 0;
	}
}
