/*
// `when 'a : null` at a class that DECLARES `null` inhabits it. `[<AllowNullLiteral>]` is the
// only way a bare nominal satisfies the constraint.
[<AllowNullLiteral>]
type Node(v: int) =
    member this.V = v

let onlyNull<'a when 'a: null> (x: 'a) = x

let f (n: Node) = onlyNull n
ignore f
*/

using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
[AllowNullLiteral]
public class Node
{
	internal readonly int v;

	public int V => v;

	public Node(int v)
	{
		this.v = v;
	}
}
public static class Program
{
	public static a onlyNull<a>(a x) where a : class
	{
		return x;
	}

	public static Node f(Node n)
	{
		return onlyNull(n);
	}

	public static int Main(string[] args)
	{
		return 0;
	}
}
