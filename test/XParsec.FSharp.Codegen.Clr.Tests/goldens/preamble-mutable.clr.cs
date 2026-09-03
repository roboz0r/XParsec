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
	internal int step;

	internal int count;

	internal readonly Fun<int, ValueTuple> bump;

	public Counter(int step)
	{
		this.step = step;
		count = 0;
		bump = new <closure>$0(this);
	}

	public int Bump(int arg0)
	{
		ValueTuple valueTuple = bump.Invoke(arg0);
		return count;
	}
}
public sealed class <closure>$0 : Fun<int, ValueTuple>
{
	public Counter capture0;

	public <closure>$0(Counter arg0)
	{
		capture0 = arg0;
	}

	public ValueTuple Invoke(int arg0)
	{
		Counter counter = capture0;
		int count = capture0.count;
		int step = capture0.step;
		int num = step;
		int num2 = arg0 * num;
		int num3 = count;
		int num4 = num2;
		counter.count = num3 + num4;
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
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(c.Bump(3));
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(c.Bump(4));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
