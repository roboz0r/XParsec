/*
// `when 'a : (new : unit -> 'a)` instantiated at a class declaring a parameterless
// constructor, which both targets accept.
type Counter() =
    member _.Count = 0

let construct<'a when 'a: (new: unit -> 'a)> (x: 'a) = x

let c = construct (Counter())
ignore c
*/

using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public class Counter
{
	public int Count => 0;
}
public static class Program
{
	public static readonly Counter c;

	static Program()
	{
		c = construct(new Counter());
	}

	public static T0 construct<T0>(T0 x) where T0 : new()
	{
		return x;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
