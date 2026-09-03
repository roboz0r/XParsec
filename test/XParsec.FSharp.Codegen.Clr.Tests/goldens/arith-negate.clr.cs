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
	public static int negI(int arg0)
	{
		return -arg0;
	}

	public static long negL(long arg0)
	{
		return -arg0;
	}

	public static double negF(double arg0)
	{
		return 0.0 - arg0;
	}

	public static float negG(float arg0)
	{
		return 0f - arg0;
	}

	public static sbyte negS(sbyte arg0)
	{
		sbyte b = arg0;
		sbyte b2 = b;
		return (sbyte)(-b2);
	}

	public static short negH(short arg0)
	{
		short num = arg0;
		short num2 = num;
		return (short)(-num2);
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(negI(5));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(negI(-5));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(negI(int.MinValue));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(negL(1000000000000L));
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(negL(long.MinValue));
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(negF(2.5), "F6");
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendFormatted(negG(2.5f));
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		sbyte b = negS(100);
		sbyte b2 = b;
		formatter8.AppendFormatted((int)b2);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		sbyte b3 = negS(sbyte.MinValue);
		sbyte b4 = b3;
		formatter9.AppendFormatted((int)b4);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		short num = negH(30000);
		short num2 = num;
		formatter10.AppendFormatted((int)num2);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		short num3 = negH(short.MinValue);
		short num4 = num3;
		formatter11.AppendFormatted((int)num4);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		return 0;
	}
}
