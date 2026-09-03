/*
// An instance `let` reading a primary-ctor parameter, read back through a member.
// The initialiser can only be evaluated after the ctor params are in place, so this
// program dies (or reads zero) on any backend that runs the preamble too early.
type Boxed(n: int) =
    let m = n + 1
    member this.M() = m * 10

let b = Boxed(4)
printfn "%d" (b.M())
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public class Boxed
{
	internal int n;

	internal readonly int m;

	public Boxed(int n)
	{
		this.n = n;
		int num = this.n;
		int num2 = 1;
		int num3 = num;
		int num4 = num2;
		m = num3 + num4;
	}

	public int M()
	{
		int num = m;
		int num2 = 10;
		int num3 = num;
		int num4 = num2;
		return num3 * num4;
	}
}
public static class Program
{
	public static readonly Boxed b;

	static Program()
	{
		b = new Boxed(4);
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(b.M());
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		return 0;
	}
}
