/*
// `when 'a : struct` instantiated at primitives the CLR represents as value types and JS
// represents as a `number` and a `boolean`.
let onlyStruct<'a when 'a: struct> (x: 'a) = x

let i = onlyStruct 42
let b = onlyStruct true
ignore i
ignore b
*/

using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public static class Program
{
	public static readonly int i;

	public static readonly bool b;

	static Program()
	{
		i = onlyStruct(42);
		b = onlyStruct(x: true);
	}

	public static a onlyStruct<a>(a x) where a : struct
	{
		return x;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
