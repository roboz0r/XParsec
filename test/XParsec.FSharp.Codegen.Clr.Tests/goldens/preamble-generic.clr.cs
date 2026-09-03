/*
// Instance `let`s on a GENERIC class: the preamble storage is typed by the type parameter, and
// a `let` closing over a `'T`-typed ctor param has to keep it at each instantiation. Two
// instantiations, so a backend that erased the preamble to one shared slot is caught.
type Cell<'T>(x: 'T, n: int) =
    let k = n * 2 + 1
    let get () = x
    member this.K() = k
    member this.X() = get ()

let s = Cell<string>("hi", 3)
let i = Cell<int>(9, 10)
printfn "%d %s" (s.K()) (s.X())
printfn "%d %d" (i.K()) (i.X())
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public class Cell<T>
{
	internal readonly T x;

	internal readonly int n;

	internal readonly int k;

	internal readonly Fun<ValueTuple, T> get;

	public Cell(T x, int n)
	{
		this.x = x;
		this.n = n;
		int num = this.n;
		int num2 = 2;
		int num3 = num;
		int num4 = num2;
		int num5 = num3 * num4;
		int num6 = 1;
		int num7 = num5;
		int num8 = num6;
		k = num7 + num8;
		get = new <closure>$0<T>(this);
	}

	public int K()
	{
		return k;
	}

	public T X()
	{
		return get.Invoke(default(ValueTuple));
	}
}
public sealed class <closure>$0<T0> : Fun<ValueTuple, T0>
{
	private readonly Cell<T0> capture0;

	public <closure>$0(Cell<T0> arg0)
	{
		capture0 = arg0;
	}

	public T0 Invoke(ValueTuple arg0)
	{
		return capture0.x;
	}
}
public static class Program
{
	public static readonly Cell<string> s;

	public static readonly Cell<int> i;

	static Program()
	{
		s = new Cell<string>("hi", 3);
		i = new Cell<int>(9, 10);
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(1, 2, Console.Out);
		formatter.AppendFormatted(s.K());
		formatter.AppendLiteral(" ");
		formatter.AppendFormatted(s.X());
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(1, 2, Console.Out);
		formatter2.AppendFormatted(i.K());
		formatter2.AppendLiteral(" ");
		formatter2.AppendFormatted(i.X());
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
