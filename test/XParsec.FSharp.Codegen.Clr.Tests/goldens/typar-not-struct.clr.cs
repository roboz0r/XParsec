/*
// `when 'a : not struct` at a reference primitive.
let onlyRef<'a when 'a: not struct> (x: 'a) = x

let s = onlyRef "x"
ignore s
*/

using System;
using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly string s;

	public static readonly string value$5;

	static Program()
	{
		s = onlyRef("x");
		value$5 = s;
	}

	public static T0 onlyRef<T0>(T0 arg0)
	{
		return arg0;
	}

	public static int Main(string[] args)
	{
		ValueTuple valueTuple = default(ValueTuple);
		return 0;
	}
}
