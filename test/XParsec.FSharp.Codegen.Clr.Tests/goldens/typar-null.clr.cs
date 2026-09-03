/*
// `when 'a : null` at a union that HAS a `null` member.
let onlyNull<'a when 'a: null> (x: 'a) = x

let f (s: string | null) = onlyNull s
ignore f
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public sealed class <closure>$0 : Fun<string, string>
{
	public static readonly <closure>$0 instance = new <closure>$0();

	public string Invoke(string arg0)
	{
		return Program.f(arg0);
	}
}
public static class Program
{
	public static T0 onlyNull<T0>(T0 arg0)
	{
		return arg0;
	}

	public static string f(string arg0)
	{
		return onlyNull(arg0);
	}

	public static int Main(string[] args)
	{
		Fun<string, string> instance = <closure>$0.instance;
		ValueTuple valueTuple = default(ValueTuple);
		return 0;
	}
}
