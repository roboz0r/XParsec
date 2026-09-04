/*
[<Struct>]
type Mixed =
    | I of x: int
    | S of x: string
*/

using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Vesper;

[Struct]
public readonly struct Mixed : IEquatable<Mixed>, IStructuralFormattable
{
	internal struct Payload
	{
		internal Mixed$Data _data;

		internal object _ref0;
	}

	public readonly struct Payload_I
	{
		private readonly Payload _payload;

		public int x => _payload._data.I._x;

		internal Payload_I(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_S
	{
		private readonly Payload _payload;

		public string x => (string)_payload._ref0;

		internal Payload_S(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	internal Mixed(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static Mixed I(int _x)
	{
		Payload payload = default(Payload);
		payload._data.I._x = _x;
		return new Mixed(0, payload);
	}

	public static Mixed S(string _x)
	{
		Payload payload = default(Payload);
		payload._ref0 = _x;
		return new Mixed(1, payload);
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_I_0()
	{
		return _payload._data.I._x;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public string Get_S_0()
	{
		return (string)_payload._ref0;
	}

	public Payload_I GetPayload_I()
	{
		return new Payload_I(_payload);
	}

	public Payload_S GetPayload_S()
	{
		return new Payload_S(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		switch (_tag)
		{
		case 0:
			hashCode.Add(_payload._data.I._x);
			break;
		case 1:
			hashCode.Add((string)_payload._ref0);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		if (obj is Mixed)
		{
			return Equals((Mixed)obj);
		}
		return false;
	}

	public bool Equals(Mixed other)
	{
		if (_tag == other._tag)
		{
			return _tag switch
			{
				0 => _payload._data.I._x == other._payload._data.I._x, 
				1 => string.Equals((string)_payload._ref0, (string)other._payload._ref0), 
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
			sink.BeginCase("I");
			sink.Child(_payload._data.I._x);
			sink.EndCase();
			break;
		case 1:
			sink.BeginCase("S");
			sink.Child(_payload._ref0);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct Mixed$Data
{
	internal struct Data_I
	{
		internal int _x;
	}

	[FieldOffset(0)]
	internal Data_I I;
}
