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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		decimal x = 1.5m;
		decimal y = 1.5m;
		((Formatter)(ref val)).AppendBool(EqualityComparer<decimal>.Default.Equals(x, y), 0);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		decimal x2 = 1.5m;
		decimal y2 = 2.5m;
		((Formatter)(ref val2)).AppendBool(EqualityComparer<decimal>.Default.Equals(x2, y2), 0);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		decimal x3 = 1.5m;
		decimal y3 = 2.5m;
		((Formatter)(ref val3)).AppendBool(Comparer<decimal>.Default.Compare(x3, y3) < 0, 0);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		nint x4 = 1;
		nint y4 = 1;
		((Formatter)(ref val4)).AppendBool(EqualityComparer<nint>.Default.Equals(x4, y4), 0);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		nint x5 = 1;
		nint y5 = 2;
		((Formatter)(ref val5)).AppendBool(EqualityComparer<nint>.Default.Equals(x5, y5), 0);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		nint x6 = 1;
		nint y6 = 2;
		((Formatter)(ref val6)).AppendBool(Comparer<nint>.Default.Compare(x6, y6) < 0, 0);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		nuint x7 = 1u;
		nuint y7 = 1u;
		((Formatter)(ref val7)).AppendBool(EqualityComparer<nuint>.Default.Equals(x7, y7), 0);
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		nuint x8 = 2u;
		nuint y8 = 1u;
		((Formatter)(ref val8)).AppendBool(Comparer<nuint>.Default.Compare(x8, y8) < 0, 0);
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
