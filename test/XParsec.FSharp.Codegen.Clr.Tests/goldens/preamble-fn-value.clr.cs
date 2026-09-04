/*
// A function-valued `let` used FIRST-CLASS: passed out of the class as a value, not just
// applied in place. It closes over the ctor param, so the value that leaves the member has
// to carry the instance with it — a backend that compiled the `let` to a method and only
// knew how to call it directly cannot hand it to `twice`.
let twice (f: int -> int) (x: int) = f (f x)

type Adder(k: int) =
    let add (x: int) = x + k
    member this.Twice(n: int) = twice add n

let a = Adder(3)
printfn "%d" (a.Twice 10)
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public class Adder
{
	internal readonly int k;

	internal readonly Fun<int, int> add;

	public Adder(int k)
	{
		this.k = k;
		add = new <closure>$0(this);
	}

	public int Twice(int arg0)
	{
		return Program.twice(add, arg0);
	}
}
public sealed class <closure>$0 : Fun<int, int>
{
	private readonly Adder capture0;

	public <closure>$0(Adder arg0)
	{
		capture0 = arg0;
	}

	public int Invoke(int arg0)
	{
		int k = capture0.k;
		return arg0 + k;
	}
}
public static class Program
{
	public static readonly Adder a;

	static Program()
	{
		a = new Adder(3);
	}

	public static int twice(Fun<int, int> arg0, int arg1)
	{
		return arg0.Invoke(arg0.Invoke(arg1));
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(a.Twice(10));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		return 0;
	}
}
