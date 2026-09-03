/*
[<Struct>]
type Readers =
    | X of a: int * b: int
    | X_0 of c: string
*/

using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Vesper;

[Struct]
public readonly struct Readers : IEquatable<Readers>, IStructuralFormattable
{
	internal struct Payload
	{
		internal Readers$Data _data;

		internal object _ref0;
	}

	public readonly struct Payload_X
	{
		private readonly Payload _payload;

		public int a => _payload._data.X._a;

		public int b => _payload._data.X._b;

		internal Payload_X(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_X_0
	{
		private readonly Payload _payload;

		public string c => (string)_payload._ref0;

		internal Payload_X_0(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	internal Readers(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static Readers X(int arg0, int arg1)
	{
		Payload payload = default(Payload);
		payload._data.X._a = arg0;
		payload._data.X._b = arg1;
		return new Readers(0, payload);
	}

	public static Readers X_0(string arg0)
	{
		Payload payload = default(Payload);
		payload._ref0 = arg0;
		return new Readers(1, payload);
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_X_0()
	{
		return _payload._data.X._a;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_X_1()
	{
		return _payload._data.X._b;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public string Get_X_0_0()
	{
		return (string)_payload._ref0;
	}

	public Payload_X GetPayload_X()
	{
		return new Payload_X(_payload);
	}

	public Payload_X_0 GetPayload_X_0()
	{
		return new Payload_X_0(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		switch (_tag)
		{
		case 0:
			hashCode.Add(_payload._data.X._a);
			hashCode.Add(_payload._data.X._b);
			break;
		case 1:
			hashCode.Add((string)_payload._ref0);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is Readers) ? obj : null);
		if (obj2 != null)
		{
			Readers other = (Readers)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(Readers other)
	{
		if (_tag == other._tag)
		{
			switch (_tag)
			{
			case 0:
				break;
			case 1:
				return string.Equals((string)_payload._ref0, (string)other._payload._ref0);
			default:
				return true;
			}
			if (_payload._data.X._a == other._payload._data.X._a)
			{
				return _payload._data.X._b == other._payload._data.X._b;
			}
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		switch (_tag)
		{
		case 0:
			sink.BeginCase("X");
			sink.Child((object)_payload._data.X._a);
			sink.Child((object)_payload._data.X._b);
			sink.EndCase();
			break;
		case 1:
			sink.BeginCase("X_0");
			sink.Child(_payload._ref0);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct Readers$Data
{
	internal struct Data_X
	{
		internal int _a;

		internal int _b;
	}

	[FieldOffset(0)]
	internal Data_X X;
}
