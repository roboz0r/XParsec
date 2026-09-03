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
		formatter.AppendBool(EqualityComparer<decimal>.Default.Equals(1.5m, 1.5m), 0);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendBool(EqualityComparer<decimal>.Default.Equals(1.5m, 2.5m), 0);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendBool(Comparer<decimal>.Default.Compare(1.5m, 2.5m) < 0, 0);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendBool(EqualityComparer<nint>.Default.Equals(1, 1), 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendBool(EqualityComparer<nint>.Default.Equals(1, 2), 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendBool(Comparer<nint>.Default.Compare(1, 2) < 0, 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendBool(EqualityComparer<nuint>.Default.Equals(1u, 1u), 0);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		formatter8.AppendBool(Comparer<nuint>.Default.Compare(2u, 1u) < 0, 0);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
