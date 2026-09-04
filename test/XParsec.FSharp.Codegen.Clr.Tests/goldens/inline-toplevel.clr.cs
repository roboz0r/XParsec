/*
// A TOP-LEVEL `let inline` is two things at once, and this pins both halves against
// each other: F# emits it as an ordinary module function AND splices its body at the
// use sites that can take one, so a saturated call, a call through another inline, and
// a first-class reference must all compute the same number.
//
// It is also the program that catches an inline binding emitted BADLY. The emitted
// function is reachable from the module surface even when every use here splices, so a
// malformed one is not dead weight: on JS it is a module-level definition whose dangling
// reference fails to load before a single line is printed, and on the CLR a static method
// whose body cannot be built.
let inline double x = x * 2

let inline applySum f x y = f (x + y)

printfn "%d" (double 21)
printfn "%d" (applySum double 20 1)

// A first-class reference — no call arguments to saturate, so the compiler has to produce a
// function VALUE for the binding rather than a spliced expression.
let d = double
printfn "%d" (d 7)
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public sealed class <closure>$0 : Fun<int, int>
{
	public static readonly <closure>$0 instance = new <closure>$0();

	public int Invoke(int arg0)
	{
		return arg0 * 2;
	}
}
public static class Program
{
	public static int @double(int x)
	{
		return x * 2;
	}

	public static T0 applySum<T0>(Fun<int, T0> f, int x, int y)
	{
		return f.Invoke(x + y);
	}

	public static int d(int arg0)
	{
		return arg0 * 2;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(21 * 2);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		Fun<int, int> instance = <closure>$0.instance;
		formatter2.AppendFormatted(instance.Invoke(20 + 1));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(d(7));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		return 0;
	}
}
