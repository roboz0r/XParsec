/*
// Unary negation (`~-`) per SIGNED width — the only widths it is defined on (see
// `arith-negate-unsigned.fs` for the rejection). A literal `-5` is a negative CONSTANT,
// not the operator, so each width routes through a function parameter to reach the
// contract's own negation clause.
//
// Each width is negated twice: once at an ordinary value, and once at its MINIMUM, where
// the negation overflows and must wrap back onto itself. The minimum is the row that
// needs the width mask — a bare CIL `neg` (or a bare JS `-`) computes on a wider stack
// and yields +128 where -128y belongs.
let negI (x: int) = -x
let negL (x: int64) = -x
let negF (x: float) = -x
let negG (x: float32) = -x
let negS (x: sbyte) = -x
let negH (x: int16) = -x

printfn "%d" (negI 5)
printfn "%d" (negI (-5))
printfn "%d" (negI (-2147483648))
printfn "%O" (negL 1000000000000L)
printfn "%O" (negL (-9223372036854775808L))
printfn "%f" (negF 2.5)
printfn "%O" (negG 2.5f)
printfn "%d" (int (negS 100y))
printfn "%d" (int (negS (-128y)))
printfn "%d" (int (negH 30000s))
printfn "%d" (int (negH (-32768s)))
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static int negI(int x)
	{
		return -x;
	}

	public static long negL(long x)
	{
		return -x;
	}

	public static double negF(double x)
	{
		return 0.0 - x;
	}

	public static float negG(float x)
	{
		return 0f - x;
	}

	public static sbyte negS(sbyte x)
	{
		return (sbyte)(-x);
	}

	public static short negH(short x)
	{
		return (short)(-x);
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(negI(5));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(negI(-5));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(negI(int.MinValue));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(negL(1000000000000L));
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(negL(long.MinValue));
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(negF(2.5), "F6");
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendFormatted(negG(2.5f));
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		sbyte b = negS(100);
		formatter8.AppendFormatted((int)b);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		sbyte b2 = negS(sbyte.MinValue);
		formatter9.AppendFormatted((int)b2);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		short num = negH(30000);
		formatter10.AppendFormatted((int)num);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		short num2 = negH(short.MinValue);
		formatter11.AppendFormatted((int)num2);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		return 0;
	}
}
