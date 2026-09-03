/*
[<Struct>]
type GShape<'T> =
    | Val of v: 'T
    | Pt of x: int * y: int
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Vesper;

[Struct]
public readonly struct GShape<T> : IEquatable<GShape<T>>, IStructuralFormattable
{
	internal struct Payload
	{
		internal GShape$Data$1 _data;

		internal T _val0;
	}

	public readonly struct Payload_Val
	{
		private readonly Payload _payload;

		public T v => _payload._val0;

		internal Payload_Val(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Pt
	{
		private readonly Payload _payload;

		public int x => _payload._data.Pt._x;

		public int y => _payload._data.Pt._y;

		internal Payload_Pt(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	internal GShape(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static GShape<T> Val(T arg0)
	{
		Payload payload = default(Payload);
		payload._val0 = arg0;
		return new GShape<T>(0, payload);
	}

	public static GShape<T> Pt(int arg0, int arg1)
	{
		Payload payload = default(Payload);
		payload._data.Pt._x = arg0;
		payload._data.Pt._y = arg1;
		return new GShape<T>(1, payload);
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public T Get_Val_0()
	{
		return _payload._val0;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Pt_0()
	{
		return _payload._data.Pt._x;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Pt_1()
	{
		return _payload._data.Pt._y;
	}

	public Payload_Val GetPayload_Val()
	{
		return new Payload_Val(_payload);
	}

	public Payload_Pt GetPayload_Pt()
	{
		return new Payload_Pt(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		switch (_tag)
		{
		case 0:
			hashCode.Add(_payload._val0);
			break;
		case 1:
			hashCode.Add(_payload._data.Pt._x);
			hashCode.Add(_payload._data.Pt._y);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is GShape<T>) ? obj : null);
		if (obj2 != null)
		{
			GShape<T> other = (GShape<T>)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(GShape<T> other)
	{
		if (_tag == other._tag)
		{
			switch (_tag)
			{
			case 0:
				return EqualityComparer<T>.Default.Equals(_payload._val0, other._payload._val0);
			case 1:
				break;
			default:
				return true;
			}
			if (_payload._data.Pt._x == other._payload._data.Pt._x)
			{
				return _payload._data.Pt._y == other._payload._data.Pt._y;
			}
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		switch (_tag)
		{
		case 0:
			sink.BeginCase("Val");
			sink.Child(_payload._val0);
			sink.EndCase();
			break;
		case 1:
			sink.BeginCase("Pt");
			sink.Child(_payload._data.Pt._x);
			sink.Child(_payload._data.Pt._y);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct GShape$Data$1
{
	internal struct Data_Pt
	{
		internal int _x;

		internal int _y;
	}

	[FieldOffset(0)]
	internal Data_Pt Pt;
}
