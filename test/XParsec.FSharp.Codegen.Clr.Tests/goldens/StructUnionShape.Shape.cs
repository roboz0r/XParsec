/*
[<Struct>]
type Shape =
    | Empty
    | Point of x: int
    | Pair of a: int * b: int
*/

using System;
using System.Collections.Generic;
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

	public Shape(int _tag, Payload _payload)
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

	public int Get_Point_0()
	{
		return _payload._data.Point._x;
	}

	public int Get_Pair_0()
	{
		return _payload._data.Pair._a;
	}

	public int Get_Pair_1()
	{
		return _payload._data.Pair._b;
	}

	public Payload_Point Get_Point()
	{
		return new Payload_Point(_payload);
	}

	public Payload_Pair Get_Pair()
	{
		return new Payload_Pair(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		if (_tag != 1)
		{
			if (_tag == 2)
			{
				hashCode.Add(_payload._data.Pair._a);
				hashCode.Add(_payload._data.Pair._b);
			}
		}
		else
		{
			hashCode.Add(_payload._data.Point._x);
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is Shape) ? obj : null);
		if (obj2 != null)
		{
			Shape shape = (Shape)obj2;
			if (_tag == shape._tag)
			{
				if (_tag != 1)
				{
					if (_tag != 2 || (EqualityComparer<int>.Default.Equals(_payload._data.Pair._a, shape._payload._data.Pair._a) && EqualityComparer<int>.Default.Equals(_payload._data.Pair._b, shape._payload._data.Pair._b)))
					{
						goto IL_00f2;
					}
				}
				else if (EqualityComparer<int>.Default.Equals(_payload._data.Point._x, shape._payload._data.Point._x))
				{
					goto IL_00f2;
				}
			}
		}
		return false;
		IL_00f2:
		return true;
	}

	public bool Equals(Shape other)
	{
		if (_tag == other._tag)
		{
			if (_tag != 1)
			{
				if (_tag != 2 || (EqualityComparer<int>.Default.Equals(_payload._data.Pair._a, other._payload._data.Pair._a) && EqualityComparer<int>.Default.Equals(_payload._data.Pair._b, other._payload._data.Pair._b)))
				{
					goto IL_00de;
				}
			}
			else if (EqualityComparer<int>.Default.Equals(_payload._data.Point._x, other._payload._data.Point._x))
			{
				goto IL_00de;
			}
		}
		return false;
		IL_00de:
		return true;
	}

	public void Format(IFormatSink sink)
	{
		if (_tag != 0)
		{
			if (_tag != 1)
			{
				sink.BeginCase("Pair");
				sink.Child((object)_payload._data.Pair._a);
				sink.Child((object)_payload._data.Pair._b);
				sink.EndCase();
			}
			else
			{
				sink.BeginCase("Point");
				sink.Child((object)_payload._data.Point._x);
				sink.EndCase();
			}
		}
		else
		{
			sink.BeginCase("Empty");
			sink.EndCase();
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
