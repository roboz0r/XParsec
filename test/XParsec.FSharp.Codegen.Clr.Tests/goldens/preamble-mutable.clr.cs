/*
// A `let mutable` is ONE storage location on the instance, shared by the function-valued
// `let` that writes it and the member that reads it. Two calls are what makes that
// observable: a backend that gave the closure its own copy (a ref cell captured by value,
// a field re-initialised per call) prints 6 twice and never accumulates.
type Counter(step: int) =
    let mutable count = 0
    let bump (k: int) = count <- count + k * step

    member this.Bump(k: int) =
        let _ = bump k
        count

let c = Counter(2)
printfn "%d" (c.Bump 3)
printfn "%d" (c.Bump 4)
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public class Counter
{
	internal readonly int step;

	internal int count;

	internal readonly Fun<int, ValueTuple> bump;

	public Counter(int step)
	{
		this.step = step;
		count = 0;
		bump = new <closure>$0(this);
	}

	public int Bump(int k)
	{
		ValueTuple valueTuple = bump.Invoke(k);
		return count;
	}
}
public sealed class <closure>$0 : Fun<int, ValueTuple>
{
	private readonly Counter capture0;

	public <closure>$0(Counter arg0)
	{
		capture0 = arg0;
	}

	public ValueTuple Invoke(int k)
	{
		Counter counter = capture0;
		int count = capture0.count;
		int step = capture0.step;
		int num = k * step;
		counter.count = count + num;
		return default(ValueTuple);
	}
}
public static class Program
{
	public static readonly Counter c;

	static Program()
	{
		c = new Counter(2);
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(c.Bump(3));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(c.Bump(4));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		return 0;
	}
}
