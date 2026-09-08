/*
// `when 'a : not null` at bare primitives, which carry no `null` member on either target.
let notNull<'a when 'a: not null> (x: 'a) = x

let i = notNull 42
let s = notNull "x"
ignore i
ignore s
*/

using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly int i;

	public static readonly string s;

	static Program()
	{
		i = notNull(42);
		s = notNull("x");
	}

	public static a notNull<a>(a x)
	{
		return x;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
