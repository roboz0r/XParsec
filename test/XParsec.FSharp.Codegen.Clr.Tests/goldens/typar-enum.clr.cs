/*
// `when 'a : enum<int>` instantiated at an enum whose underlying type is `int`.
type Colour =
    | Red = 1
    | Green = 2

let onlyEnum<'a when 'a: enum<int>> (x: 'a) = x

let c = onlyEnum Colour.Green
ignore c
*/

using System.Reflection;

[assembly: AssemblyVersion("1.0.0.0")]
public enum Colour
{
	Red = 1,
	Green
}
public static class Program
{
	public static readonly Colour c;

	static Program()
	{
		c = onlyEnum(Colour.Green);
	}

	public static a onlyEnum<a>(a x)
	{
		return x;
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
