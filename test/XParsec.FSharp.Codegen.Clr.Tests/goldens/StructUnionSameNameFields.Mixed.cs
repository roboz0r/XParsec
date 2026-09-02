/*
[<Struct>]
type Mixed =
    | I of x: int
    | S of x: string
*/

using System;
using System.Collections.Generic;
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

	public Mixed(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static Mixed I(int arg0)
	{
		Payload payload = default(Payload);
		payload._data.I._x = arg0;
		return new Mixed(0, payload);
	}

	public static Mixed S(string arg0)
	{
		Payload payload = default(Payload);
		payload._ref0 = arg0;
		return new Mixed(1, payload);
	}

	public int Get_I_0()
	{
		return _payload._data.I._x;
	}

	public string Get_S_0()
	{
		return (string)_payload._ref0;
	}

	public Payload_I Get_I()
	{
		return new Payload_I(_payload);
	}

	public Payload_S Get_S()
	{
		return new Payload_S(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		if (_tag != 0)
		{
			if (_tag == 1)
			{
				hashCode.Add((string)_payload._ref0);
			}
		}
		else
		{
			hashCode.Add(_payload._data.I._x);
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is Mixed) ? obj : null);
		if (obj2 != null)
		{
			Mixed mixed = (Mixed)obj2;
			if (_tag == mixed._tag)
			{
				if (_tag != 0)
				{
					if (_tag != 1 || EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)mixed._payload._ref0))
					{
						goto IL_00af;
					}
				}
				else if (EqualityComparer<int>.Default.Equals(_payload._data.I._x, mixed._payload._data.I._x))
				{
					goto IL_00af;
				}
			}
		}
		return false;
		IL_00af:
		return true;
	}

	public bool Equals(Mixed other)
	{
		if (_tag == other._tag)
		{
			if (_tag != 0)
			{
				if (_tag != 1 || EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)other._payload._ref0))
				{
					goto IL_009b;
				}
			}
			else if (EqualityComparer<int>.Default.Equals(_payload._data.I._x, other._payload._data.I._x))
			{
				goto IL_009b;
			}
		}
		return false;
		IL_009b:
		return true;
	}

	public void Format(IFormatSink sink)
	{
		if (_tag != 0)
		{
			sink.BeginCase("S");
			sink.Child((object)(string)_payload._ref0);
			sink.EndCase();
		}
		else
		{
			sink.BeginCase("I");
			sink.Child((object)_payload._data.I._x);
			sink.EndCase();
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
