/*
// `when 'a : null` at a union that HAS a `null` member.
let onlyNull<'a when 'a: null> (x: 'a) = x

let f (s: string | null) = onlyNull s
ignore f
*/

using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static T0 onlyNull<T0>(T0 x)
	{
		return x;
	}

	public static string f(string s)
	{
		return onlyNull(s);
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
