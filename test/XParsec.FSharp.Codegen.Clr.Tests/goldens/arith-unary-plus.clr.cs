/*
// Prefix plus (`~+`) — the identity, at every width that declares it. It spans several
// widths, so it pins no single (width, operator) pair and carries no `width`; the
// per-width rows put `~+` in the matrix.
//
// Each row routes through a FUNCTION PARAMETER rather than `+5` directly: a literal with
// a sign is a constant, and the point here is that the operator resolves to the width's
// own declared member and yields the operand unchanged — including at the unsigned widths,
// where `~-` is undefined and rejected.
let plusI (x: int) = +x
let plusB (x: byte) = +x
let plusS (x: sbyte) = +x
let plusH (x: uint16) = +x
let plusU (x: uint32) = +x
let plusL (x: int64) = +x
let plusW (x: uint64) = +x
let plusF (x: float) = +x
let plusG (x: bigint) = +x

printfn "%d" (plusI 42)
printfn "%d" (plusI (-42))
printfn "%d" (int (plusB 200uy))
printfn "%d" (int (plusS (-128y)))
printfn "%d" (int (plusH 65535us))
printfn "%O" (plusU 4000000000u)
printfn "%O" (plusL (-9223372036854775808L))
printfn "%O" (plusW 18446744073709551615UL)
printfn "%f" (plusF 2.5)
printfn "%O" (plusG (bigint 1000000007))
*/

using System;
using System.Numerics;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static int plusI(int arg0)
	{
		return arg0;
	}

	public static byte plusB(byte arg0)
	{
		return arg0;
	}

	public static sbyte plusS(sbyte arg0)
	{
		return arg0;
	}

	public static ushort plusH(ushort arg0)
	{
		return arg0;
	}

	public static uint plusU(uint arg0)
	{
		return arg0;
	}

	public static long plusL(long arg0)
	{
		return arg0;
	}

	public static ulong plusW(ulong arg0)
	{
		return arg0;
	}

	public static double plusF(double arg0)
	{
		return arg0;
	}

	public static BigInteger plusG(BigInteger arg0)
	{
		return arg0;
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(plusI(42));
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(plusI(-42));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		byte b = plusB(200);
		byte b2 = b;
		((Formatter)(ref val3)).AppendFormatted<int>((int)b2);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		sbyte b3 = plusS(sbyte.MinValue);
		sbyte b4 = b3;
		((Formatter)(ref val4)).AppendFormatted<int>((int)b4);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		ushort num = plusH(ushort.MaxValue);
		ushort num2 = num;
		((Formatter)(ref val5)).AppendFormatted<int>((int)num2);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		((Formatter)(ref val6)).AppendFormatted<uint>(plusU(4000000000u));
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		((Formatter)(ref val7)).AppendFormatted<long>(plusL(long.MinValue));
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		((Formatter)(ref val8)).AppendFormatted<ulong>(plusW(ulong.MaxValue));
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter val9 = default(Formatter);
		((Formatter)(ref val9))..ctor(0, 1, Console.Out);
		((Formatter)(ref val9)).AppendFormatted<double>(plusF(2.5), "F6");
		((Formatter)(ref val9)).AppendLiteral("\n");
		((Formatter)(ref val9)).Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter val10 = default(Formatter);
		((Formatter)(ref val10))..ctor(0, 1, Console.Out);
		int num3 = 1000000007;
		((Formatter)(ref val10)).AppendFormatted<BigInteger>(plusG(num3));
		((Formatter)(ref val10)).AppendLiteral("\n");
		((Formatter)(ref val10)).Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		return 0;
	}
}
