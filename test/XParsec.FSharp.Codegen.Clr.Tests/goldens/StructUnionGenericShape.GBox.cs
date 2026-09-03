/*
[<Struct>]
type GBox<'T> =
    | Val of v: 'T
    | Num of n: int
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Vesper;

[Struct]
public readonly struct GBox<T> : IEquatable<GBox<T>>, IStructuralFormattable
{
	internal struct Payload
	{
		internal GBox$Data$1 _data;

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

	public readonly struct Payload_Num
	{
		private readonly Payload _payload;

		public int n => _payload._data.Num._n;

		internal Payload_Num(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	internal GBox(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static GBox<T> Val(T arg0)
	{
		Payload payload = default(Payload);
		payload._val0 = arg0;
		return new GBox<T>(0, payload);
	}

	public static GBox<T> Num(int arg0)
	{
		Payload payload = default(Payload);
		payload._data.Num._n = arg0;
		return new GBox<T>(1, payload);
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public T Get_Val_0()
	{
		return _payload._val0;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Num_0()
	{
		return _payload._data.Num._n;
	}

	public Payload_Val GetPayload_Val()
	{
		return new Payload_Val(_payload);
	}

	public Payload_Num GetPayload_Num()
	{
		return new Payload_Num(_payload);
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
			hashCode.Add(_payload._data.Num._n);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is GBox<T>) ? obj : null);
		if (obj2 != null)
		{
			GBox<T> other = (GBox<T>)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(GBox<T> other)
	{
		if (_tag == other._tag)
		{
			return _tag switch
			{
				0 => EqualityComparer<T>.Default.Equals(_payload._val0, other._payload._val0), 
				1 => _payload._data.Num._n == other._payload._data.Num._n, 
				_ => true, 
			};
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
			sink.BeginCase("Num");
			sink.Child(_payload._data.Num._n);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct GBox$Data$1
{
	internal struct Data_Num
	{
		internal int _n;
	}

	[FieldOffset(0)]
	internal Data_Num Num;
}
