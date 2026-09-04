/*
// `when 'a : not struct` at a reference primitive.
let onlyRef<'a when 'a: not struct> (x: 'a) = x

let s = onlyRef "x"
ignore s
*/

using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly string s;

	static Program()
	{
		s = onlyRef("x");
	}

	public static T0 onlyRef<T0>(T0 arg0)
	{
		return arg0;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
