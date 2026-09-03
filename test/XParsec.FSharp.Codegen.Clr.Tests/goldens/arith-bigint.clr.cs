/*
// bigint — arbitrary precision, so the rows that matter are the ones NO fixed width holds:
// `big * big` runs past 2^64 and must still be exact on both backends (BCL `BigInteger` on
// the CLR, the JS `BigInt` primitive). Like `decimal`, its operator bodies are BCL calls on
// the CLR rather than mnemonics; unlike `decimal` it has a JS repr, so both targets run it.
//
// The signed `/` and `%` rows are the agreement that is not free: both must truncate toward
// zero and take the sign of the DIVIDEND. `%O` rather than `%A` — `%A` renders a JS bigint
// with the `L` suffix it uses for `int64`, which the CLR has no reason to print.
let a = bigint 1000000007
let b = bigint 1000000009
let big = a * b
printfn "%O" big
printfn "%O" (big * big)
printfn "%O" (bigint 6 + bigint 7)
printfn "%O" (bigint 6 - bigint 7)
printfn "%O" (bigint 20 / bigint 6)
printfn "%O" (bigint 20 % bigint 6)
printfn "%O" (bigint (-20) / bigint 6)
printfn "%O" (bigint (-20) % bigint 6)
printfn "%O" (-(bigint 5))
*/

using System;
using System.Numerics;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly BigInteger a;

	public static readonly BigInteger b;

	public static readonly BigInteger big;

	static Program()
	{
		int num = 1000000007;
		a = num;
		int num2 = 1000000009;
		b = num2;
		BigInteger bigInteger = a;
		BigInteger bigInteger2 = b;
		BigInteger left = bigInteger;
		BigInteger right = bigInteger2;
		big = BigInteger.Multiply(left, right);
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<BigInteger>(big);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		BigInteger bigInteger = big;
		BigInteger bigInteger2 = big;
		BigInteger left = bigInteger;
		BigInteger right = bigInteger2;
		((Formatter)(ref val2)).AppendFormatted<BigInteger>(BigInteger.Multiply(left, right));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		int num = 6;
		BigInteger bigInteger3 = num;
		int num2 = 7;
		BigInteger bigInteger4 = num2;
		BigInteger left2 = bigInteger3;
		BigInteger right2 = bigInteger4;
		((Formatter)(ref val3)).AppendFormatted<BigInteger>(BigInteger.Add(left2, right2));
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		int num3 = 6;
		BigInteger bigInteger5 = num3;
		int num4 = 7;
		BigInteger bigInteger6 = num4;
		BigInteger left3 = bigInteger5;
		BigInteger right3 = bigInteger6;
		((Formatter)(ref val4)).AppendFormatted<BigInteger>(BigInteger.Subtract(left3, right3));
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		int num5 = 20;
		BigInteger bigInteger7 = num5;
		int num6 = 6;
		BigInteger bigInteger8 = num6;
		BigInteger dividend = bigInteger7;
		BigInteger divisor = bigInteger8;
		((Formatter)(ref val5)).AppendFormatted<BigInteger>(BigInteger.Divide(dividend, divisor));
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		int num7 = 20;
		BigInteger bigInteger9 = num7;
		int num8 = 6;
		BigInteger bigInteger10 = num8;
		BigInteger dividend2 = bigInteger9;
		BigInteger divisor2 = bigInteger10;
		((Formatter)(ref val6)).AppendFormatted<BigInteger>(BigInteger.Remainder(dividend2, divisor2));
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		int num9 = -20;
		BigInteger bigInteger11 = num9;
		int num10 = 6;
		BigInteger bigInteger12 = num10;
		BigInteger dividend3 = bigInteger11;
		BigInteger divisor3 = bigInteger12;
		((Formatter)(ref val7)).AppendFormatted<BigInteger>(BigInteger.Divide(dividend3, divisor3));
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		int num11 = -20;
		BigInteger bigInteger13 = num11;
		int num12 = 6;
		BigInteger bigInteger14 = num12;
		BigInteger dividend4 = bigInteger13;
		BigInteger divisor4 = bigInteger14;
		((Formatter)(ref val8)).AppendFormatted<BigInteger>(BigInteger.Remainder(dividend4, divisor4));
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter val9 = default(Formatter);
		((Formatter)(ref val9))..ctor(0, 1, Console.Out);
		int num13 = 5;
		BigInteger bigInteger15 = num13;
		BigInteger value = bigInteger15;
		((Formatter)(ref val9)).AppendFormatted<BigInteger>(BigInteger.Negate(value));
		((Formatter)(ref val9)).AppendLiteral("\n");
		((Formatter)(ref val9)).Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		return 0;
	}
}
