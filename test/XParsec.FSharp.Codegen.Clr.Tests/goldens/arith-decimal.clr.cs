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
		Formatter formatter = new Formatter(0, 1, Console.Out);
		decimal num = a;
		decimal num2 = b;
		decimal d = num;
		decimal d2 = num2;
		formatter.AppendFormatted(decimal.Add(d, d2));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		decimal num3 = 10m;
		decimal num4 = 4m;
		decimal d3 = num3;
		decimal d4 = num4;
		formatter2.AppendFormatted(decimal.Subtract(d3, d4));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		decimal num5 = 6m;
		decimal num6 = 7m;
		decimal d5 = num5;
		decimal d6 = num6;
		formatter3.AppendFormatted(decimal.Multiply(d5, d6));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		decimal num7 = 10m;
		decimal num8 = 4m;
		decimal d7 = num7;
		decimal d8 = num8;
		formatter4.AppendFormatted(decimal.Divide(d7, d8));
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		decimal num9 = 10m;
		decimal num10 = 3m;
		decimal d9 = num9;
		decimal d10 = num10;
		formatter5.AppendFormatted(decimal.Remainder(d9, d10));
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		decimal num11 = a;
		decimal d11 = num11;
		formatter6.AppendFormatted(decimal.Negate(d11));
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		decimal num12 = 0.1m;
		decimal num13 = 0.2m;
		decimal d12 = num12;
		decimal d13 = num13;
		formatter7.AppendFormatted(decimal.Add(d12, d13));
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		decimal num14 = 1.50m;
		decimal num15 = 2m;
		decimal d14 = num14;
		decimal d15 = num15;
		formatter8.AppendFormatted(decimal.Multiply(d14, d15));
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
