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
		formatter.AppendFormatted(decimal.Add(a, b));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(decimal.Subtract(10m, 4m));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(decimal.Multiply(6m, 7m));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(decimal.Divide(10m, 4m));
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(decimal.Remainder(10m, 3m));
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(decimal.Negate(a));
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendFormatted(decimal.Add(0.1m, 0.2m));
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		formatter8.AppendFormatted(decimal.Multiply(1.50m, 2m));
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		return 0;
	}
}
