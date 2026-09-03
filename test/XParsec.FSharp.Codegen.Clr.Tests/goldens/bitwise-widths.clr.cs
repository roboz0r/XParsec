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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		int num = 13;
		int num2 = 11;
		int num3 = num;
		int num4 = num2;
		((Formatter)(ref val)).AppendFormatted<int>(num3 & num4);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		int num5 = 13;
		int num6 = 11;
		int num7 = num5;
		int num8 = num6;
		((Formatter)(ref val2)).AppendFormatted<int>(num7 | num8);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		int num9 = 13;
		int num10 = 11;
		int num11 = num9;
		int num12 = num10;
		((Formatter)(ref val3)).AppendFormatted<int>(num11 ^ num12);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		int num13 = 0;
		int num14 = num13;
		((Formatter)(ref val4)).AppendFormatted<int>(~num14);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		int num15 = 1;
		int num16 = 4;
		int num17 = num15;
		int num18 = num16;
		((Formatter)(ref val5)).AppendFormatted<int>(num17 << num18);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		int num19 = 208;
		int num20 = 2;
		int num21 = num19;
		int num22 = num20;
		((Formatter)(ref val6)).AppendFormatted<int>(num21 >> num22);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		int num23 = 0;
		int num24 = 16;
		int num25 = num23;
		int num26 = num24;
		int num27 = num25 - num26;
		int num28 = 2;
		int num29 = num27;
		int num30 = num28;
		((Formatter)(ref val7)).AppendFormatted<int>(num29 >> num30);
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		byte b = 13;
		byte b2 = 11;
		byte b3 = b;
		byte b4 = b2;
		byte b5 = (byte)(b3 & b4);
		byte b6 = b5;
		((Formatter)(ref val8)).AppendFormatted<int>((int)b6);
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter val9 = default(Formatter);
		((Formatter)(ref val9))..ctor(0, 1, Console.Out);
		byte b7 = 13;
		byte b8 = 11;
		byte b9 = b7;
		byte b10 = b8;
		byte b11 = (byte)(b9 | b10);
		byte b12 = b11;
		((Formatter)(ref val9)).AppendFormatted<int>((int)b12);
		((Formatter)(ref val9)).AppendLiteral("\n");
		((Formatter)(ref val9)).Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter val10 = default(Formatter);
		((Formatter)(ref val10))..ctor(0, 1, Console.Out);
		byte b13 = 13;
		byte b14 = 11;
		byte b15 = b13;
		byte b16 = b14;
		byte b17 = (byte)(b15 ^ b16);
		byte b18 = b17;
		((Formatter)(ref val10)).AppendFormatted<int>((int)b18);
		((Formatter)(ref val10)).AppendLiteral("\n");
		((Formatter)(ref val10)).Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter val11 = default(Formatter);
		((Formatter)(ref val11))..ctor(0, 1, Console.Out);
		byte b19 = 0;
		byte b20 = b19;
		byte b21 = (byte)(~b20);
		byte b22 = b21;
		((Formatter)(ref val11)).AppendFormatted<int>((int)b22);
		((Formatter)(ref val11)).AppendLiteral("\n");
		((Formatter)(ref val11)).Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		Formatter val12 = default(Formatter);
		((Formatter)(ref val12))..ctor(0, 1, Console.Out);
		byte b23 = 200;
		int num31 = 4;
		byte b24 = b23;
		int num32 = num31;
		byte b25 = (byte)((uint)b24 >> num32);
		byte b26 = b25;
		((Formatter)(ref val12)).AppendFormatted<int>((int)b26);
		((Formatter)(ref val12)).AppendLiteral("\n");
		((Formatter)(ref val12)).Flush();
		ValueTuple valueTuple12 = default(ValueTuple);
		Formatter val13 = default(Formatter);
		((Formatter)(ref val13))..ctor(0, 1, Console.Out);
		byte b27 = 3;
		int num33 = 2;
		byte b28 = b27;
		int num34 = num33;
		byte b29 = (byte)(b28 << num34);
		byte b30 = b29;
		((Formatter)(ref val13)).AppendFormatted<int>((int)b30);
		((Formatter)(ref val13)).AppendLiteral("\n");
		((Formatter)(ref val13)).Flush();
		ValueTuple valueTuple13 = default(ValueTuple);
		Formatter val14 = default(Formatter);
		((Formatter)(ref val14))..ctor(0, 1, Console.Out);
		sbyte b31 = 0;
		sbyte b32 = b31;
		sbyte b33 = (sbyte)(~b32);
		sbyte b34 = b33;
		((Formatter)(ref val14)).AppendFormatted<int>((int)b34);
		((Formatter)(ref val14)).AppendLiteral("\n");
		((Formatter)(ref val14)).Flush();
		ValueTuple valueTuple14 = default(ValueTuple);
		Formatter val15 = default(Formatter);
		((Formatter)(ref val15))..ctor(0, 1, Console.Out);
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
		((Formatter)(ref val15)).AppendFormatted<int>((int)b42);
		((Formatter)(ref val15)).AppendLiteral("\n");
		((Formatter)(ref val15)).Flush();
		ValueTuple valueTuple15 = default(ValueTuple);
		Formatter val16 = default(Formatter);
		((Formatter)(ref val16))..ctor(0, 1, Console.Out);
		sbyte b43 = 3;
		int num37 = 2;
		sbyte b44 = b43;
		int num38 = num37;
		sbyte b45 = (sbyte)(b44 << num38);
		sbyte b46 = b45;
		((Formatter)(ref val16)).AppendFormatted<int>((int)b46);
		((Formatter)(ref val16)).AppendLiteral("\n");
		((Formatter)(ref val16)).Flush();
		ValueTuple valueTuple16 = default(ValueTuple);
		Formatter val17 = default(Formatter);
		((Formatter)(ref val17))..ctor(0, 1, Console.Out);
		short num39 = 0;
		short num40 = num39;
		short num41 = (short)(~num40);
		short num42 = num41;
		((Formatter)(ref val17)).AppendFormatted<int>((int)num42);
		((Formatter)(ref val17)).AppendLiteral("\n");
		((Formatter)(ref val17)).Flush();
		ValueTuple valueTuple17 = default(ValueTuple);
		Formatter val18 = default(Formatter);
		((Formatter)(ref val18))..ctor(0, 1, Console.Out);
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
		((Formatter)(ref val18)).AppendFormatted<int>((int)num52);
		((Formatter)(ref val18)).AppendLiteral("\n");
		((Formatter)(ref val18)).Flush();
		ValueTuple valueTuple18 = default(ValueTuple);
		Formatter val19 = default(Formatter);
		((Formatter)(ref val19))..ctor(0, 1, Console.Out);
		ushort num53 = 0;
		ushort num54 = num53;
		ushort num55 = (ushort)(~num54);
		ushort num56 = num55;
		((Formatter)(ref val19)).AppendFormatted<int>((int)num56);
		((Formatter)(ref val19)).AppendLiteral("\n");
		((Formatter)(ref val19)).Flush();
		ValueTuple valueTuple19 = default(ValueTuple);
		Formatter val20 = default(Formatter);
		((Formatter)(ref val20))..ctor(0, 1, Console.Out);
		ushort num57 = 60000;
		int num58 = 4;
		ushort num59 = num57;
		int num60 = num58;
		ushort num61 = (ushort)((uint)num59 >> num60);
		ushort num62 = num61;
		((Formatter)(ref val20)).AppendFormatted<int>((int)num62);
		((Formatter)(ref val20)).AppendLiteral("\n");
		((Formatter)(ref val20)).Flush();
		ValueTuple valueTuple20 = default(ValueTuple);
		Formatter val21 = default(Formatter);
		((Formatter)(ref val21))..ctor(0, 1, Console.Out);
		uint num63 = 0u;
		uint num64 = num63;
		uint num65 = ~num64;
		uint num66 = num65;
		((Formatter)(ref val21)).AppendUnsigned((ulong)num66, 0);
		((Formatter)(ref val21)).AppendLiteral("\n");
		((Formatter)(ref val21)).Flush();
		ValueTuple valueTuple21 = default(ValueTuple);
		Formatter val22 = default(Formatter);
		((Formatter)(ref val22))..ctor(0, 1, Console.Out);
		uint num67 = 4000000000u;
		int num68 = 8;
		uint num69 = num67;
		int num70 = num68;
		uint num71 = num69 >> num70;
		uint num72 = num71;
		((Formatter)(ref val22)).AppendUnsigned((ulong)num72, 0);
		((Formatter)(ref val22)).AppendLiteral("\n");
		((Formatter)(ref val22)).Flush();
		ValueTuple valueTuple22 = default(ValueTuple);
		Formatter val23 = default(Formatter);
		((Formatter)(ref val23))..ctor(0, 1, Console.Out);
		uint num73 = 4000000000u;
		uint num74 = 4278190080u;
		uint num75 = num73;
		uint num76 = num74;
		uint num77 = num75 & num76;
		uint num78 = num77;
		((Formatter)(ref val23)).AppendUnsigned((ulong)num78, 0);
		((Formatter)(ref val23)).AppendLiteral("\n");
		((Formatter)(ref val23)).Flush();
		ValueTuple valueTuple23 = default(ValueTuple);
		Formatter val24 = default(Formatter);
		((Formatter)(ref val24))..ctor(0, 1, Console.Out);
		long num79 = 13L;
		long num80 = 11L;
		long num81 = num79;
		long num82 = num80;
		((Formatter)(ref val24)).AppendFormatted<long>(num81 & num82);
		((Formatter)(ref val24)).AppendLiteral("\n");
		((Formatter)(ref val24)).Flush();
		ValueTuple valueTuple24 = default(ValueTuple);
		Formatter val25 = default(Formatter);
		((Formatter)(ref val25))..ctor(0, 1, Console.Out);
		long num83 = 0L;
		long num84 = num83;
		((Formatter)(ref val25)).AppendFormatted<long>(~num84);
		((Formatter)(ref val25)).AppendLiteral("\n");
		((Formatter)(ref val25)).Flush();
		ValueTuple valueTuple25 = default(ValueTuple);
		Formatter val26 = default(Formatter);
		((Formatter)(ref val26))..ctor(0, 1, Console.Out);
		long num85 = 0L;
		long num86 = 16L;
		long num87 = num85;
		long num88 = num86;
		long num89 = num87 - num88;
		int num90 = 2;
		long num91 = num89;
		int num92 = num90;
		((Formatter)(ref val26)).AppendFormatted<long>(num91 >> num92);
		((Formatter)(ref val26)).AppendLiteral("\n");
		((Formatter)(ref val26)).Flush();
		ValueTuple valueTuple26 = default(ValueTuple);
		Formatter val27 = default(Formatter);
		((Formatter)(ref val27))..ctor(0, 1, Console.Out);
		long num93 = 1L;
		int num94 = 40;
		long num95 = num93;
		int num96 = num94;
		((Formatter)(ref val27)).AppendFormatted<long>(num95 << num96);
		((Formatter)(ref val27)).AppendLiteral("\n");
		((Formatter)(ref val27)).Flush();
		ValueTuple valueTuple27 = default(ValueTuple);
		Formatter val28 = default(Formatter);
		((Formatter)(ref val28))..ctor(0, 1, Console.Out);
		ulong num97 = 13uL;
		ulong num98 = 3uL;
		ulong num99 = num97;
		ulong num100 = num98;
		((Formatter)(ref val28)).AppendFormatted<ulong>(num99 | num100);
		((Formatter)(ref val28)).AppendLiteral("\n");
		((Formatter)(ref val28)).Flush();
		ValueTuple valueTuple28 = default(ValueTuple);
		Formatter val29 = default(Formatter);
		((Formatter)(ref val29))..ctor(0, 1, Console.Out);
		ulong num101 = 0uL;
		ulong num102 = num101;
		((Formatter)(ref val29)).AppendFormatted<ulong>(~num102);
		((Formatter)(ref val29)).AppendLiteral("\n");
		((Formatter)(ref val29)).Flush();
		ValueTuple valueTuple29 = default(ValueTuple);
		Formatter val30 = default(Formatter);
		((Formatter)(ref val30))..ctor(0, 1, Console.Out);
		ulong num103 = 0uL;
		ulong num104 = 1uL;
		ulong num105 = num103;
		ulong num106 = num104;
		ulong num107 = num105 - num106;
		int num108 = 8;
		ulong num109 = num107;
		int num110 = num108;
		((Formatter)(ref val30)).AppendFormatted<ulong>(num109 >> num110);
		((Formatter)(ref val30)).AppendLiteral("\n");
		((Formatter)(ref val30)).Flush();
		ValueTuple valueTuple30 = default(ValueTuple);
		return 0;
	}
}
