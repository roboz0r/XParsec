/*
// A `let rec` in the preamble: the function-valued `let` is in scope in its own body, and
// stays callable from a member afterwards. Both the ctor-time call (`value`) and the
// member-time call go through whatever storage the backend chose for it.
type Factorial(n: int) =
    let rec fact k = if k <= 1 then 1 else k * fact (k - 1)

    let value = fact n
    member this.Value() = value
    member this.Of(k: int) = fact k

let f = Factorial(5)
printfn "%d" (f.Value())
printfn "%d" (f.Of 6)
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public class Factorial
{
	internal int n;

	internal readonly Fun<int, int> fact;

	internal readonly int value;

	public Factorial(int n)
	{
		this.n = n;
		fact = new <closure>$0(this);
		value = fact.Invoke(this.n);
	}

	public int Value()
	{
		return value;
	}

	public int Of(int arg0)
	{
		return fact.Invoke(arg0);
	}
}
public sealed class <closure>$0 : Fun<int, int>
{
	public Factorial capture0;

	public <closure>$0(Factorial arg0)
	{
		capture0 = arg0;
	}

	public int Invoke(int arg0)
	{
		int num = 1;
		int result;
		if (arg0 <= num)
		{
			result = 1;
		}
		else
		{
			Fun<int, int> fact = capture0.fact;
			int num2 = 1;
			int num3 = num2;
			int num4 = fact.Invoke(arg0 - num3);
			int num5 = num4;
			result = arg0 * num5;
		}
		return result;
	}
}
public static class Program
{
	public static readonly Factorial f;

	static Program()
	{
		f = new Factorial(5);
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(f.Value());
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(f.Of(6));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
