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
		a = 1000000007;
		b = 1000000009;
		big = BigInteger.Multiply(a, b);
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(big);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(BigInteger.Multiply(big, big));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		BigInteger left = 6;
		BigInteger right = 7;
		formatter3.AppendFormatted(BigInteger.Add(left, right));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		BigInteger left2 = 6;
		BigInteger right2 = 7;
		formatter4.AppendFormatted(BigInteger.Subtract(left2, right2));
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		BigInteger dividend = 20;
		BigInteger divisor = 6;
		formatter5.AppendFormatted(BigInteger.Divide(dividend, divisor));
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		BigInteger dividend2 = 20;
		BigInteger divisor2 = 6;
		formatter6.AppendFormatted(BigInteger.Remainder(dividend2, divisor2));
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		BigInteger dividend3 = -20;
		BigInteger divisor3 = 6;
		formatter7.AppendFormatted(BigInteger.Divide(dividend3, divisor3));
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		BigInteger dividend4 = -20;
		BigInteger divisor4 = 6;
		formatter8.AppendFormatted(BigInteger.Remainder(dividend4, divisor4));
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		BigInteger value = 5;
		formatter9.AppendFormatted(BigInteger.Negate(value));
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		return 0;
	}
}
