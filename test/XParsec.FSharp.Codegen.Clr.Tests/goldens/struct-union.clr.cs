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
			sink.Child((object)_payload._data.Point._x);
			sink.EndCase();
			break;
		case 2:
			sink.BeginCase("Pair");
			sink.Child((object)_payload._data.Pair._a);
			sink.Child((object)_payload._data.Pair._b);
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
			int num = a;
			int num2 = b;
			int num3 = num;
			int num4 = num2;
			result = num3 + num4;
		}
		return result;
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(describe(s0));
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(describe(s1));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		((Formatter)(ref val3)).AppendFormatted<int>(describe(s2));
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		Shape x = s1;
		Shape y = Shape.Point(3);
		((Formatter)(ref val4)).AppendBool(EqualityComparer<Shape>.Default.Equals(x, y), 0);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		Shape x2 = s1;
		Shape y2 = Shape.Empty();
		((Formatter)(ref val5)).AppendBool(EqualityComparer<Shape>.Default.Equals(x2, y2), 0);
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		Shape x3 = Shape.Point(3);
		Shape y3 = s1;
		((Formatter)(ref val6)).AppendBool(EqualityComparer<Shape>.Default.Equals(x3, y3), 0);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		Shape x4 = s2;
		Shape y4 = Shape.Pair(4, 5);
		((Formatter)(ref val7)).AppendBool(EqualityComparer<Shape>.Default.Equals(x4, y4), 0);
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		Shape x5 = s2;
		Shape y5 = Shape.Pair(4, 6);
		((Formatter)(ref val8)).AppendBool(EqualityComparer<Shape>.Default.Equals(x5, y5), 0);
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		return 0;
	}
}
