/*
// `when 'a : not null` at bare primitives, which carry no `null` member on either target.
let notNull<'a when 'a: not null> (x: 'a) = x

let i = notNull 42
let s = notNull "x"
ignore i
ignore s
*/

using System;
using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly int i;

	public static readonly string s;

	public static readonly int value$8;

	public static string value$9;

	static Program()
	{
		i = notNull(42);
		s = notNull("x");
		value$8 = i;
	}

	public static T0 notNull<T0>(T0 arg0)
	{
		return arg0;
	}

	public static int Main(string[] args)
	{
		ValueTuple valueTuple = default(ValueTuple);
		value$9 = s;
		ValueTuple valueTuple2 = default(ValueTuple);
		return 0;
	}
}
