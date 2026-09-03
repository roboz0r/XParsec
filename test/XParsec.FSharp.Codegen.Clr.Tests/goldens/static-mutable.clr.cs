/*
// A `static let mutable` is ONE storage location on the TYPE, shared by every instance and
// every access — a write through one instance is visible to a read through another. Two
// instances writing then both reading is what makes the sharing observable: a backend that
// gave the field per-instance storage (or dropped the store) would print 3 then 4, never 7
// twice. The CLR stores to a private static field (`stsfld`); JS assigns a property on the
// emitted class object. The read is an instance method (a closure re-reading the cell) — a
// JS nullary/unit member snapshots its value at module load, a separate limitation.
type Counter() =
    static let mutable total = 0
    member _.Add(k: int) = total <- total + k
    member _.Get() = total

let a = Counter()
let b = Counter()
a.Add 3
b.Add 4
printfn "%d" (a.Get())
printfn "%d" (b.Get())
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public class Counter
{
	internal static int total = 0;

	public void Add(int arg0)
	{
		int num = total;
		int num2 = num;
		total = num2 + arg0;
		ValueTuple valueTuple = default(ValueTuple);
	}

	public int Get()
	{
		return total;
	}
}
public static class Program
{
	public static readonly Counter a;

	public static readonly Counter b;

	static Program()
	{
		a = new Counter();
		b = new Counter();
	}

	public static int Main(string[] args)
	{
		a.Add(3);
		ValueTuple valueTuple = default(ValueTuple);
		b.Add(4);
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(a.Get());
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(b.Get());
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		return 0;
	}
}
