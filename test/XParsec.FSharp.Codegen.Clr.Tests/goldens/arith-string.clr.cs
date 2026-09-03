/*
// string — the declared `(+)` concatenation, the one non-numeric operand the
// arithmetic contract admits.
printfn "%s" ("ab" + "cd")
printfn "%s" ("" + "x")
printfn "%s" ("a" + "b" + "c")
*/

using System;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		string text = "ab";
		string text2 = "cd";
		string text3 = text;
		string text4 = text2;
		((Formatter)(ref val)).AppendFormatted<string>(text3 + text4);
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		string text5 = "";
		string text6 = "x";
		string text7 = text5;
		string text8 = text6;
		((Formatter)(ref val2)).AppendFormatted<string>(text7 + text8);
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		string text9 = "a";
		string text10 = "b";
		string text11 = text9;
		string text12 = text10;
		string text13 = text11 + text12;
		string text14 = "c";
		string text15 = text13;
		string text16 = text14;
		((Formatter)(ref val3)).AppendFormatted<string>(text15 + text16);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		return 0;
	}
}
