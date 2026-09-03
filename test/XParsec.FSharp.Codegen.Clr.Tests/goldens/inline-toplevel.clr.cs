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
		int num = 2;
		int num2 = num;
		return arg0 * num2;
	}
}
public static class Program
{
	public static int @double(int arg0)
	{
		int num = 2;
		int num2 = num;
		return arg0 * num2;
	}

	public static T0 applySum<T0>(Fun<int, T0> arg0, int arg1, int arg2)
	{
		return arg0.Invoke(arg1 + arg2);
	}

	public static int d(int arg0)
	{
		int num = 2;
		int num2 = num;
		return arg0 * num2;
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		int num = 21;
		int num2 = num;
		int num3 = 2;
		int num4 = num2;
		int num5 = num3;
		((Formatter)(ref val)).AppendFormatted<int>(num4 * num5);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		Fun<int, int> instance = <closure>$0.instance;
		int num6 = 20;
		int num7 = 1;
		int num8 = num6;
		int num9 = num7;
		int num10 = num8;
		int num11 = num9;
		((Formatter)(ref val2)).AppendFormatted<int>(instance.Invoke(num10 + num11));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		((Formatter)(ref val3)).AppendFormatted<int>(d(7));
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		return 0;
	}
}
