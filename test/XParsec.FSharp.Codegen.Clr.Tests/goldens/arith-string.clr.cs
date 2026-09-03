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
		Formatter formatter = new Formatter(0, 1, Console.Out);
		string text = "ab";
		string text2 = "cd";
		string text3 = text;
		string text4 = text2;
		formatter.AppendFormatted(text3 + text4);
		formatter.AppendLiteral("\n");
		formatter.Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		string text5 = "";
		string text6 = "x";
		string text7 = text5;
		string text8 = text6;
		formatter2.AppendFormatted(text7 + text8);
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		string text9 = "a";
		string text10 = "b";
		string text11 = text9;
		string text12 = text10;
		string text13 = text11 + text12;
		string text14 = "c";
		string text15 = text13;
		string text16 = text14;
		formatter3.AppendFormatted(text15 + text16);
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		return 0;
	}
}
