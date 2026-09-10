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
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(0xD | 0xB);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(0xD ^ 0xB);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(~0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(1 << 4);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(208 >> 2);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendFormatted(-16 >> 2);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		formatter8.AppendFormatted(0xD & 0xB);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		formatter9.AppendFormatted(0xD | 0xB);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		formatter10.AppendFormatted(0xD ^ 0xB);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		formatter11.AppendFormatted(255);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		Formatter formatter12 = new Formatter(0, 1, Console.Out);
		formatter12.AppendFormatted(200 >>> 4);
		formatter12.AppendLiteral("\n");
		formatter12.Flush();
		Formatter formatter13 = new Formatter(0, 1, Console.Out);
		formatter13.AppendFormatted(12);
		formatter13.AppendLiteral("\n");
		formatter13.Flush();
		Formatter formatter14 = new Formatter(0, 1, Console.Out);
		formatter14.AppendFormatted(~0);
		formatter14.AppendLiteral("\n");
		formatter14.Flush();
		Formatter formatter15 = new Formatter(0, 1, Console.Out);
		formatter15.AppendFormatted((sbyte)(-16) >> 2);
		formatter15.AppendLiteral("\n");
		formatter15.Flush();
		Formatter formatter16 = new Formatter(0, 1, Console.Out);
		formatter16.AppendFormatted(12);
		formatter16.AppendLiteral("\n");
		formatter16.Flush();
		Formatter formatter17 = new Formatter(0, 1, Console.Out);
		formatter17.AppendFormatted(~0);
		formatter17.AppendLiteral("\n");
		formatter17.Flush();
		Formatter formatter18 = new Formatter(0, 1, Console.Out);
		formatter18.AppendFormatted((short)(-16) >> 2);
		formatter18.AppendLiteral("\n");
		formatter18.Flush();
		Formatter formatter19 = new Formatter(0, 1, Console.Out);
		formatter19.AppendFormatted(65535);
		formatter19.AppendLiteral("\n");
		formatter19.Flush();
		Formatter formatter20 = new Formatter(0, 1, Console.Out);
		formatter20.AppendFormatted(60000 >>> 4);
		formatter20.AppendLiteral("\n");
		formatter20.Flush();
		Formatter formatter21 = new Formatter(0, 1, Console.Out);
		formatter21.AppendUnsigned(4294967295uL, 0);
		formatter21.AppendLiteral("\n");
		formatter21.Flush();
		Formatter formatter22 = new Formatter(0, 1, Console.Out);
		formatter22.AppendUnsigned(15625000uL, 0);
		formatter22.AppendLiteral("\n");
		formatter22.Flush();
		Formatter formatter23 = new Formatter(0, 1, Console.Out);
		formatter23.AppendUnsigned(3992977408uL, 0);
		formatter23.AppendLiteral("\n");
		formatter23.Flush();
		Formatter formatter24 = new Formatter(0, 1, Console.Out);
		formatter24.AppendFormatted(0xDL & 0xBL);
		formatter24.AppendLiteral("\n");
		formatter24.Flush();
		Formatter formatter25 = new Formatter(0, 1, Console.Out);
		formatter25.AppendFormatted(~0L);
		formatter25.AppendLiteral("\n");
		formatter25.Flush();
		Formatter formatter26 = new Formatter(0, 1, Console.Out);
		formatter26.AppendFormatted(-16L >> 2);
		formatter26.AppendLiteral("\n");
		formatter26.Flush();
		Formatter formatter27 = new Formatter(0, 1, Console.Out);
		formatter27.AppendFormatted(1L << 40);
		formatter27.AppendLiteral("\n");
		formatter27.Flush();
		Formatter formatter28 = new Formatter(0, 1, Console.Out);
		formatter28.AppendFormatted(15uL);
		formatter28.AppendLiteral("\n");
		formatter28.Flush();
		Formatter formatter29 = new Formatter(0, 1, Console.Out);
		formatter29.AppendFormatted(ulong.MaxValue);
		formatter29.AppendLiteral("\n");
		formatter29.Flush();
		Formatter formatter30 = new Formatter(0, 1, Console.Out);
		formatter30.AppendFormatted((ulong)(-1L) >> 8);
		formatter30.AppendLiteral("\n");
		formatter30.Flush();
		return 0;
	}
}
