/*
// float32 (IEEE 32) — the single-precision width. `%f` types its argument as
// float, so a float32 is printed with `%O` (shortest round-trip stringification).
//
// The last row is the whole point: `0.1f + 0.2f` rounds to the float32 nearest 0.3
// and prints "0.3". Computing it in DOUBLE precision instead prints
// 0.30000000000000004 — close, but not this width's result. The first four rows are
// exactly representable and cannot tell the two apart.
printfn "%O" (1.5f + 2.5f)
printfn "%O" (3.0f - 1.5f)
printfn "%O" (1.5f * 2.0f)
printfn "%O" (7.5f / 2.5f)
printfn "%O" (0.1f + 0.2f)
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(1.5f + 2.5f);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(3f - 1.5f);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(1.5f * 2f);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(7.5f / 2.5f);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(0.1f + 0.2f);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		return 0;
	}
}
