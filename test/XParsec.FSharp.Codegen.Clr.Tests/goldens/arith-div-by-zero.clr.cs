/*
// Integer division by zero must FAULT, not produce a value — and a fault is a
// conformance property exactly as much as a computed value is. This program has no golden:
// it declares its expected runtime error in the manifest instead.
//
// `1 / 0` is the int32 row, the one width whose clause already throws on the CLR (CIL
// `div` raises `DivideByZeroException`). The width-by-width family belongs here too,
// but only once every clause can throw.
printfn "%d" (1 / 0)
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
		int num = 1;
		int num2 = 0;
		int num3 = num;
		int num4 = num2;
		formatter.AppendFormatted(num3 / num4);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		return 0;
	}
}
