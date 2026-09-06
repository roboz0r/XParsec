/*
// A measured float is a float at runtime: the measure is checked by the front end and
// erased by every backend, so a measured value prints exactly as its carrier does, and
// measured arithmetic splices the carrier's operator.
[<Measure>]
type m

[<Measure>]
type s

let d = 100.0<m>
let t: float<s> = 8.0<s>
let v: float<m / s> = d / t
printfn "%f" d
printfn "%f" t
printfn "%f" v
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly double d;

	public static readonly double t;

	public static readonly double v;

	static Program()
	{
		d = 100.0;
		t = 8.0;
		v = d / t;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(d, "F6");
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(t, "F6");
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(v, "F6");
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		return 0;
	}
}
