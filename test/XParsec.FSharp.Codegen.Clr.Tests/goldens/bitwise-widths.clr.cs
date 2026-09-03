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
		int num = 13;
		int num2 = 11;
		int num3 = num;
		int num4 = num2;
		formatter.AppendFormatted(num3 & num4);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		int num5 = 13;
		int num6 = 11;
		int num7 = num5;
		int num8 = num6;
		formatter2.AppendFormatted(num7 | num8);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		int num9 = 13;
		int num10 = 11;
		int num11 = num9;
		int num12 = num10;
		formatter3.AppendFormatted(num11 ^ num12);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		int num13 = 0;
		int num14 = num13;
		formatter4.AppendFormatted(~num14);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		int num15 = 1;
		int num16 = 4;
		int num17 = num15;
		int num18 = num16;
		formatter5.AppendFormatted(num17 << num18);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		int num19 = 208;
		int num20 = 2;
		int num21 = num19;
		int num22 = num20;
		formatter6.AppendFormatted(num21 >> num22);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		int num23 = 0;
		int num24 = 16;
		int num25 = num23;
		int num26 = num24;
		int num27 = num25 - num26;
		int num28 = 2;
		int num29 = num27;
		int num30 = num28;
		formatter7.AppendFormatted(num29 >> num30);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		byte b = 13;
		byte b2 = 11;
		byte b3 = b;
		byte b4 = b2;
		byte b5 = (byte)(b3 & b4);
		byte b6 = b5;
		formatter8.AppendFormatted((int)b6);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		byte b7 = 13;
		byte b8 = 11;
		byte b9 = b7;
		byte b10 = b8;
		byte b11 = (byte)(b9 | b10);
		byte b12 = b11;
		formatter9.AppendFormatted((int)b12);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		byte b13 = 13;
		byte b14 = 11;
		byte b15 = b13;
		byte b16 = b14;
		byte b17 = (byte)(b15 ^ b16);
		byte b18 = b17;
		formatter10.AppendFormatted((int)b18);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		byte b19 = 0;
		byte b20 = b19;
		byte b21 = (byte)(~b20);
		byte b22 = b21;
		formatter11.AppendFormatted((int)b22);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		Formatter formatter12 = new Formatter(0, 1, Console.Out);
		byte b23 = 200;
		int num31 = 4;
		byte b24 = b23;
		int num32 = num31;
		byte b25 = (byte)((uint)b24 >> num32);
		byte b26 = b25;
		formatter12.AppendFormatted((int)b26);
		formatter12.AppendLiteral("\n");
		formatter12.Flush();
		ValueTuple valueTuple12 = default(ValueTuple);
		Formatter formatter13 = new Formatter(0, 1, Console.Out);
		byte b27 = 3;
		int num33 = 2;
		byte b28 = b27;
		int num34 = num33;
		byte b29 = (byte)(b28 << num34);
		byte b30 = b29;
		formatter13.AppendFormatted((int)b30);
		formatter13.AppendLiteral("\n");
		formatter13.Flush();
		ValueTuple valueTuple13 = default(ValueTuple);
		Formatter formatter14 = new Formatter(0, 1, Console.Out);
		sbyte b31 = 0;
		sbyte b32 = b31;
		sbyte b33 = (sbyte)(~b32);
		sbyte b34 = b33;
		formatter14.AppendFormatted((int)b34);
		formatter14.AppendLiteral("\n");
		formatter14.Flush();
		ValueTuple valueTuple14 = default(ValueTuple);
		Formatter formatter15 = new Formatter(0, 1, Console.Out);
		sbyte b35 = 0;
		sbyte b36 = 16;
		sbyte b37 = b35;
		sbyte b38 = b36;
		sbyte b39 = (sbyte)(b37 - b38);
		int num35 = 2;
		sbyte b40 = b39;
		int num36 = num35;
		sbyte b41 = (sbyte)(b40 >> num36);
		sbyte b42 = b41;
		formatter15.AppendFormatted((int)b42);
		formatter15.AppendLiteral("\n");
		formatter15.Flush();
		ValueTuple valueTuple15 = default(ValueTuple);
		Formatter formatter16 = new Formatter(0, 1, Console.Out);
		sbyte b43 = 3;
		int num37 = 2;
		sbyte b44 = b43;
		int num38 = num37;
		sbyte b45 = (sbyte)(b44 << num38);
		sbyte b46 = b45;
		formatter16.AppendFormatted((int)b46);
		formatter16.AppendLiteral("\n");
		formatter16.Flush();
		ValueTuple valueTuple16 = default(ValueTuple);
		Formatter formatter17 = new Formatter(0, 1, Console.Out);
		short num39 = 0;
		short num40 = num39;
		short num41 = (short)(~num40);
		short num42 = num41;
		formatter17.AppendFormatted((int)num42);
		formatter17.AppendLiteral("\n");
		formatter17.Flush();
		ValueTuple valueTuple17 = default(ValueTuple);
		Formatter formatter18 = new Formatter(0, 1, Console.Out);
		short num43 = 0;
		short num44 = 16;
		short num45 = num43;
		short num46 = num44;
		short num47 = (short)(num45 - num46);
		int num48 = 2;
		short num49 = num47;
		int num50 = num48;
		short num51 = (short)(num49 >> num50);
		short num52 = num51;
		formatter18.AppendFormatted((int)num52);
		formatter18.AppendLiteral("\n");
		formatter18.Flush();
		ValueTuple valueTuple18 = default(ValueTuple);
		Formatter formatter19 = new Formatter(0, 1, Console.Out);
		ushort num53 = 0;
		ushort num54 = num53;
		ushort num55 = (ushort)(~num54);
		ushort num56 = num55;
		formatter19.AppendFormatted((int)num56);
		formatter19.AppendLiteral("\n");
		formatter19.Flush();
		ValueTuple valueTuple19 = default(ValueTuple);
		Formatter formatter20 = new Formatter(0, 1, Console.Out);
		ushort num57 = 60000;
		int num58 = 4;
		ushort num59 = num57;
		int num60 = num58;
		ushort num61 = (ushort)((uint)num59 >> num60);
		ushort num62 = num61;
		formatter20.AppendFormatted((int)num62);
		formatter20.AppendLiteral("\n");
		formatter20.Flush();
		ValueTuple valueTuple20 = default(ValueTuple);
		Formatter formatter21 = new Formatter(0, 1, Console.Out);
		uint num63 = 0u;
		uint num64 = num63;
		uint num65 = ~num64;
		uint num66 = num65;
		formatter21.AppendUnsigned(num66, 0);
		formatter21.AppendLiteral("\n");
		formatter21.Flush();
		ValueTuple valueTuple21 = default(ValueTuple);
		Formatter formatter22 = new Formatter(0, 1, Console.Out);
		uint num67 = 4000000000u;
		int num68 = 8;
		uint num69 = num67;
		int num70 = num68;
		uint num71 = num69 >> num70;
		uint num72 = num71;
		formatter22.AppendUnsigned(num72, 0);
		formatter22.AppendLiteral("\n");
		formatter22.Flush();
		ValueTuple valueTuple22 = default(ValueTuple);
		Formatter formatter23 = new Formatter(0, 1, Console.Out);
		uint num73 = 4000000000u;
		uint num74 = 4278190080u;
		uint num75 = num73;
		uint num76 = num74;
		uint num77 = num75 & num76;
		uint num78 = num77;
		formatter23.AppendUnsigned(num78, 0);
		formatter23.AppendLiteral("\n");
		formatter23.Flush();
		ValueTuple valueTuple23 = default(ValueTuple);
		Formatter formatter24 = new Formatter(0, 1, Console.Out);
		long num79 = 13L;
		long num80 = 11L;
		long num81 = num79;
		long num82 = num80;
		formatter24.AppendFormatted(num81 & num82);
		formatter24.AppendLiteral("\n");
		formatter24.Flush();
		ValueTuple valueTuple24 = default(ValueTuple);
		Formatter formatter25 = new Formatter(0, 1, Console.Out);
		long num83 = 0L;
		long num84 = num83;
		formatter25.AppendFormatted(~num84);
		formatter25.AppendLiteral("\n");
		formatter25.Flush();
		ValueTuple valueTuple25 = default(ValueTuple);
		Formatter formatter26 = new Formatter(0, 1, Console.Out);
		long num85 = 0L;
		long num86 = 16L;
		long num87 = num85;
		long num88 = num86;
		long num89 = num87 - num88;
		int num90 = 2;
		long num91 = num89;
		int num92 = num90;
		formatter26.AppendFormatted(num91 >> num92);
		formatter26.AppendLiteral("\n");
		formatter26.Flush();
		ValueTuple valueTuple26 = default(ValueTuple);
		Formatter formatter27 = new Formatter(0, 1, Console.Out);
		long num93 = 1L;
		int num94 = 40;
		long num95 = num93;
		int num96 = num94;
		formatter27.AppendFormatted(num95 << num96);
		formatter27.AppendLiteral("\n");
		formatter27.Flush();
		ValueTuple valueTuple27 = default(ValueTuple);
		Formatter formatter28 = new Formatter(0, 1, Console.Out);
		ulong num97 = 13uL;
		ulong num98 = 3uL;
		ulong num99 = num97;
		ulong num100 = num98;
		formatter28.AppendFormatted(num99 | num100);
		formatter28.AppendLiteral("\n");
		formatter28.Flush();
		ValueTuple valueTuple28 = default(ValueTuple);
		Formatter formatter29 = new Formatter(0, 1, Console.Out);
		ulong num101 = 0uL;
		ulong num102 = num101;
		formatter29.AppendFormatted(~num102);
		formatter29.AppendLiteral("\n");
		formatter29.Flush();
		ValueTuple valueTuple29 = default(ValueTuple);
		Formatter formatter30 = new Formatter(0, 1, Console.Out);
		ulong num103 = 0uL;
		ulong num104 = 1uL;
		ulong num105 = num103;
		ulong num106 = num104;
		ulong num107 = num105 - num106;
		int num108 = 8;
		ulong num109 = num107;
		int num110 = num108;
		formatter30.AppendFormatted(num109 >> num110);
		formatter30.AppendLiteral("\n");
		formatter30.Flush();
		ValueTuple valueTuple30 = default(ValueTuple);
		return 0;
	}
}
