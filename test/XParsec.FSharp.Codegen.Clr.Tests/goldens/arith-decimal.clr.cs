/*
// decimal — the one width whose operator bodies are BCL CALLS (`Decimal.Add`, …) rather
// than a CIL mnemonic, so this program is what judges that a spliced call computes the
// same value a spliced opcode does. It runs on the CLR only: JS has no decimal repr, so
// the type is unrepresentable there and the same six declarations are unreachable.
//
// The last two rows are why the width exists: `0.1M + 0.2M` is EXACTLY `0.3` (the float
// row of this corpus is not), and multiplying keeps the operand's scale.
let a = 1.5M
let b = 2.5M
printfn "%M" (a + b)
printfn "%M" (10M - 4M)
printfn "%M" (6M * 7M)
printfn "%M" (10M / 4M)
printfn "%M" (10M % 3M)
printfn "%M" (-a)
printfn "%M" (0.1M + 0.2M)
printfn "%M" (1.50M * 2M)
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly decimal a;

	public static readonly decimal b;

	static Program()
	{
		a = 1.5m;
		b = 2.5m;
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		decimal num = a;
		decimal num2 = b;
		decimal d = num;
		decimal d2 = num2;
		((Formatter)(ref val)).AppendFormatted<decimal>(decimal.Add(d, d2));
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		decimal num3 = 10m;
		decimal num4 = 4m;
		decimal d3 = num3;
		decimal d4 = num4;
		((Formatter)(ref val2)).AppendFormatted<decimal>(decimal.Subtract(d3, d4));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		decimal num5 = 6m;
		decimal num6 = 7m;
		decimal d5 = num5;
		decimal d6 = num6;
		((Formatter)(ref val3)).AppendFormatted<decimal>(decimal.Multiply(d5, d6));
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		decimal num7 = 10m;
		decimal num8 = 4m;
		decimal d7 = num7;
		decimal d8 = num8;
		((Formatter)(ref val4)).AppendFormatted<decimal>(decimal.Divide(d7, d8));
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		decimal num9 = 10m;
		decimal num10 = 3m;
		decimal d9 = num9;
		decimal d10 = num10;
		((Formatter)(ref val5)).AppendFormatted<decimal>(decimal.Remainder(d9, d10));
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		decimal num11 = a;
		decimal d11 = num11;
		((Formatter)(ref val6)).AppendFormatted<decimal>(decimal.Negate(d11));
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		decimal num12 = 0.1m;
		decimal num13 = 0.2m;
		decimal d12 = num12;
		decimal d13 = num13;
		((Formatter)(ref val7)).AppendFormatted<decimal>(decimal.Add(d12, d13));
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		decimal num14 = 1.50m;
		decimal num15 = 2m;
		decimal d14 = num14;
		decimal d15 = num15;
		((Formatter)(ref val8)).AppendFormatted<decimal>(decimal.Multiply(d14, d15));
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
