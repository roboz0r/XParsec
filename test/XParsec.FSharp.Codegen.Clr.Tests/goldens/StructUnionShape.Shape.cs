/*
[<Struct>]
type Shape =
    | Empty
    | Point of x: int
    | Pair of a: int * b: int
*/

using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Vesper;

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
		if (obj is Shape)
		{
			return Equals((Shape)obj);
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
