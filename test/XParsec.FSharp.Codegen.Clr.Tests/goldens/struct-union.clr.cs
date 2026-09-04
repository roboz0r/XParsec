/*
// A [<Struct>] union. On the CLR it emits as a sealed System.ValueType-based value type;
// on JS, which has no value-type concept, it emits as the same reference object a plain
// union does. Both backends must produce every result below.
[<Struct>]
type Shape =
    | Empty
    | Point of x: int
    | Pair of a: int * b: int

let describe (s: Shape) : int =
    match s with
    | Empty -> 0
    | Point x -> x
    | Pair(a, b) -> a + b

let s0 = Empty
let s1 = Point 3
let s2 = Pair(4, 5)

printfn "%d" (describe s0)
printfn "%d" (describe s1)
printfn "%d" (describe s2)
printfn "%b" (s1 = Point 3)
printfn "%b" (s1 = Empty)
printfn "%b" (Point 3 = s1)
printfn "%b" (s2 = Pair(4, 5))
printfn "%b" (s2 = Pair(4, 6))
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.InteropServices;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
[Struct]
public readonly struct Shape : IEquatable<Shape>, IStructuralFormattable
{
	internal struct Payload
	{
		internal Shape$Data _data;
	}

	public readonly struct Payload_Point
	{
		private readonly Payload _payload;

		public int x => _payload._data.Point._x;

		internal Payload_Point(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Pair
	{
		private readonly Payload _payload;

		public int a => _payload._data.Pair._a;

		public int b => _payload._data.Pair._b;

		internal Payload_Pair(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	internal Shape(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static Shape Empty()
	{
		return new Shape(0, default(Payload));
	}

	public static Shape Point(int arg0)
	{
		Payload payload = default(Payload);
		payload._data.Point._x = arg0;
		return new Shape(1, payload);
	}

	public static Shape Pair(int arg0, int arg1)
	{
		Payload payload = default(Payload);
		payload._data.Pair._a = arg0;
		payload._data.Pair._b = arg1;
		return new Shape(2, payload);
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Point_0()
	{
		return _payload._data.Point._x;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Pair_0()
	{
		return _payload._data.Pair._a;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Pair_1()
	{
		return _payload._data.Pair._b;
	}

	public Payload_Point GetPayload_Point()
	{
		return new Payload_Point(_payload);
	}

	public Payload_Pair GetPayload_Pair()
	{
		return new Payload_Pair(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		switch (_tag)
		{
		case 1:
			hashCode.Add(_payload._data.Point._x);
			break;
		case 2:
			hashCode.Add(_payload._data.Pair._a);
			hashCode.Add(_payload._data.Pair._b);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is Shape) ? obj : null);
		if (obj2 != null)
		{
			Shape other = (Shape)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(Shape other)
	{
		if (_tag == other._tag)
		{
			switch (_tag)
			{
			case 1:
				return _payload._data.Point._x == other._payload._data.Point._x;
			case 2:
				break;
			default:
				return true;
			}
			if (_payload._data.Pair._a == other._payload._data.Pair._a)
			{
				return _payload._data.Pair._b == other._payload._data.Pair._b;
			}
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		switch (_tag)
		{
		case 0:
			sink.BeginCase("Empty");
			sink.EndCase();
			break;
		case 1:
			sink.BeginCase("Point");
			sink.Child(_payload._data.Point._x);
			sink.EndCase();
			break;
		case 2:
			sink.BeginCase("Pair");
			sink.Child(_payload._data.Pair._a);
			sink.Child(_payload._data.Pair._b);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct Shape$Data
{
	internal struct Data_Point
	{
		internal int _x;
	}

	internal struct Data_Pair
	{
		internal int _a;

		internal int _b;
	}

	[FieldOffset(0)]
	internal Data_Point Point;

	[FieldOffset(0)]
	internal Data_Pair Pair;
}
public static class Program
{
	public static readonly Shape s0;

	public static readonly Shape s1;

	public static readonly Shape s2;

	static Program()
	{
		s0 = Shape.Empty();
		s1 = Shape.Point(3);
		s2 = Shape.Pair(4, 5);
	}

	public static int describe(Shape arg0)
	{
		Shape shape = arg0;
		int result;
		if (shape.Tag == 0)
		{
			result = 0;
		}
		else if (shape.Tag == 1)
		{
			int x = shape._payload._data.Point._x;
			result = x;
		}
		else
		{
			if (shape.Tag != 2)
			{
				throw new Exception("The match cases were incomplete");
			}
			int a = shape._payload._data.Pair._a;
			int b = shape._payload._data.Pair._b;
			result = a + b;
		}
		return result;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(describe(s0));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(describe(s1));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(describe(s2));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		Shape y = Shape.Point(3);
		formatter4.AppendBool(EqualityComparer<Shape>.Default.Equals(s1, y), 0);
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		Shape y2 = Shape.Empty();
		formatter5.AppendBool(EqualityComparer<Shape>.Default.Equals(s1, y2), 0);
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		Shape x = Shape.Point(3);
		formatter6.AppendBool(EqualityComparer<Shape>.Default.Equals(x, s1), 0);
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		Shape y3 = Shape.Pair(4, 5);
		formatter7.AppendBool(EqualityComparer<Shape>.Default.Equals(s2, y3), 0);
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		Shape y4 = Shape.Pair(4, 6);
		formatter8.AppendBool(EqualityComparer<Shape>.Default.Equals(s2, y4), 0);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		return 0;
	}
}
