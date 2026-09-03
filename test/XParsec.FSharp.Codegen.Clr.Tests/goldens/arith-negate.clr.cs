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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(negI(5));
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(negI(-5));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		((Formatter)(ref val3)).AppendFormatted<int>(negI(int.MinValue));
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		((Formatter)(ref val4)).AppendFormatted<long>(negL(1000000000000L));
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		((Formatter)(ref val5)).AppendFormatted<long>(negL(long.MinValue));
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		((Formatter)(ref val6)).AppendFormatted<double>(negF(2.5), "F6");
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		((Formatter)(ref val7)).AppendFormatted<float>(negG(2.5f));
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		sbyte b = negS(100);
		sbyte b2 = b;
		((Formatter)(ref val8)).AppendFormatted<int>((int)b2);
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter val9 = default(Formatter);
		((Formatter)(ref val9))..ctor(0, 1, Console.Out);
		sbyte b3 = negS(sbyte.MinValue);
		sbyte b4 = b3;
		((Formatter)(ref val9)).AppendFormatted<int>((int)b4);
		((Formatter)(ref val9)).AppendLiteral("\n");
		((Formatter)(ref val9)).Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter val10 = default(Formatter);
		((Formatter)(ref val10))..ctor(0, 1, Console.Out);
		short num = negH(30000);
		short num2 = num;
		((Formatter)(ref val10)).AppendFormatted<int>((int)num2);
		((Formatter)(ref val10)).AppendLiteral("\n");
		((Formatter)(ref val10)).Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter val11 = default(Formatter);
		((Formatter)(ref val11))..ctor(0, 1, Console.Out);
		short num3 = negH(short.MinValue);
		short num4 = num3;
		((Formatter)(ref val11)).AppendFormatted<int>((int)num4);
		((Formatter)(ref val11)).AppendLiteral("\n");
		((Formatter)(ref val11)).Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		return 0;
	}
}
