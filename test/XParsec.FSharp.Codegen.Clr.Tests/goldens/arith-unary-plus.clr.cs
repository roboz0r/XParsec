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
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(plusI(42));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(plusI(-42));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		byte b = plusB(200);
		byte b2 = b;
		formatter3.AppendFormatted((int)b2);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		sbyte b3 = plusS(sbyte.MinValue);
		sbyte b4 = b3;
		formatter4.AppendFormatted((int)b4);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		ushort num = plusH(ushort.MaxValue);
		ushort num2 = num;
		formatter5.AppendFormatted((int)num2);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(plusU(4000000000u));
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendFormatted(plusL(long.MinValue));
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		formatter8.AppendFormatted(plusW(ulong.MaxValue));
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		formatter9.AppendFormatted(plusF(2.5), "F6");
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		int num3 = 1000000007;
		formatter10.AppendFormatted(plusG(num3));
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		return 0;
	}
}
