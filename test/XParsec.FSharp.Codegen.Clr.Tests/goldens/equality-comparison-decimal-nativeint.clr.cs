/*
// `=` and `<` at the three widths the contract declares equatable and comparable but JS
// binds no repr for. Neither operator carries a static-optimization clause at these
// widths, so both lower to the generic `Comparer` / `EqualityComparer` base rather than a
// CIL mnemonic — this program is what judges that the base compares as the opcodes do.
// CLR only: the same eight rows are unreachable on JS, which represents none of the three.
printfn "%b" (1.5M = 1.5M)
printfn "%b" (1.5M = 2.5M)
printfn "%b" (1.5M < 2.5M)
printfn "%b" (1n = 1n)
printfn "%b" (1n = 2n)
printfn "%b" (1n < 2n)
printfn "%b" (1un = 1un)
printfn "%b" (2un < 1un)
*/

using System;
using System.Collections.Generic;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		decimal x = 1.5m;
		decimal y = 1.5m;
		formatter.AppendBool(EqualityComparer<decimal>.Default.Equals(x, y), 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		decimal x2 = 1.5m;
		decimal y2 = 2.5m;
		formatter2.AppendBool(EqualityComparer<decimal>.Default.Equals(x2, y2), 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		decimal x3 = 1.5m;
		decimal y3 = 2.5m;
		formatter3.AppendBool(Comparer<decimal>.Default.Compare(x3, y3) < 0, 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		nint x4 = 1;
		nint y4 = 1;
		formatter4.AppendBool(EqualityComparer<nint>.Default.Equals(x4, y4), 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		nint x5 = 1;
		nint y5 = 2;
		formatter5.AppendBool(EqualityComparer<nint>.Default.Equals(x5, y5), 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		nint x6 = 1;
		nint y6 = 2;
		formatter6.AppendBool(Comparer<nint>.Default.Compare(x6, y6) < 0, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		nuint x7 = 1u;
		nuint y7 = 1u;
		formatter7.AppendBool(EqualityComparer<nuint>.Default.Equals(x7, y7), 0);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		nuint x8 = 2u;
		nuint y8 = 1u;
		formatter8.AppendBool(Comparer<nuint>.Default.Compare(x8, y8) < 0, 0);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
