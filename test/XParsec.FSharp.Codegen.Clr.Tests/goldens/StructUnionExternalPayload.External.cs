/*
[<Struct>]
type External =
    | Scalars of x: int * y: bool
    | Nested of inner: Inner
    | Text of s: string
    | Id of id: System.Guid
    | Stamp of at: System.DateTime
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Vesper;

[Struct]
public readonly struct External : IEquatable<External>, IStructuralFormattable
{
	internal struct Payload
	{
		internal External$Data _data;

		internal object _ref0;

		internal Guid _val0;

		internal DateTime _val1;
	}

	public readonly struct Payload_Scalars
	{
		private readonly Payload _payload;

		public int x => _payload._data.Scalars._x;

		public bool y => _payload._data.Scalars._y;

		internal Payload_Scalars(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Nested
	{
		private readonly Payload _payload;

		public Inner inner => _payload._data.Nested._inner;

		internal Payload_Nested(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Text
	{
		private readonly Payload _payload;

		public string s => (string)_payload._ref0;

		internal Payload_Text(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Id
	{
		private readonly Payload _payload;

		public Guid id => _payload._val0;

		internal Payload_Id(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Stamp
	{
		private readonly Payload _payload;

		public DateTime at => _payload._val1;

		internal Payload_Stamp(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	internal External(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static External Scalars(int arg0, bool arg1)
	{
		Payload payload = default(Payload);
		payload._data.Scalars._x = arg0;
		payload._data.Scalars._y = arg1;
		return new External(0, payload);
	}

	public static External Nested(Inner arg0)
	{
		Payload payload = default(Payload);
		payload._data.Nested._inner = arg0;
		return new External(1, payload);
	}

	public static External Text(string arg0)
	{
		Payload payload = default(Payload);
		payload._ref0 = arg0;
		return new External(2, payload);
	}

	public static External Id(Guid arg0)
	{
		Payload payload = default(Payload);
		payload._val0 = arg0;
		return new External(3, payload);
	}

	public static External Stamp(DateTime arg0)
	{
		Payload payload = default(Payload);
		payload._val1 = arg0;
		return new External(4, payload);
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Scalars_0()
	{
		return _payload._data.Scalars._x;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public bool Get_Scalars_1()
	{
		return _payload._data.Scalars._y;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public Inner Get_Nested_0()
	{
		return _payload._data.Nested._inner;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public string Get_Text_0()
	{
		return (string)_payload._ref0;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public Guid Get_Id_0()
	{
		return _payload._val0;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public DateTime Get_Stamp_0()
	{
		return _payload._val1;
	}

	public Payload_Scalars GetPayload_Scalars()
	{
		return new Payload_Scalars(_payload);
	}

	public Payload_Nested GetPayload_Nested()
	{
		return new Payload_Nested(_payload);
	}

	public Payload_Text GetPayload_Text()
	{
		return new Payload_Text(_payload);
	}

	public Payload_Id GetPayload_Id()
	{
		return new Payload_Id(_payload);
	}

	public Payload_Stamp GetPayload_Stamp()
	{
		return new Payload_Stamp(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		switch (_tag)
		{
		case 0:
			hashCode.Add(_payload._data.Scalars._x);
			hashCode.Add(_payload._data.Scalars._y);
			break;
		case 1:
			hashCode.Add(_payload._data.Nested._inner);
			break;
		case 2:
			hashCode.Add((string)_payload._ref0);
			break;
		case 3:
			hashCode.Add(_payload._val0);
			break;
		case 4:
			hashCode.Add(_payload._val1);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is External) ? obj : null);
		if (obj2 != null)
		{
			External other = (External)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(External other)
	{
		if (_tag == other._tag)
		{
			switch (_tag)
			{
			case 0:
				break;
			case 1:
				return EqualityComparer<Inner>.Default.Equals(_payload._data.Nested._inner, other._payload._data.Nested._inner);
			case 2:
				return string.Equals((string)_payload._ref0, (string)other._payload._ref0);
			case 3:
				return EqualityComparer<Guid>.Default.Equals(_payload._val0, other._payload._val0);
			case 4:
				return EqualityComparer<DateTime>.Default.Equals(_payload._val1, other._payload._val1);
			default:
				return true;
			}
			if (_payload._data.Scalars._x == other._payload._data.Scalars._x)
			{
				return _payload._data.Scalars._y == other._payload._data.Scalars._y;
			}
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		switch (_tag)
		{
		case 0:
			sink.BeginCase("Scalars");
			sink.Child(_payload._data.Scalars._x);
			sink.Child(_payload._data.Scalars._y);
			sink.EndCase();
			break;
		case 1:
			sink.BeginCase("Nested");
			sink.Child(_payload._data.Nested._inner);
			sink.EndCase();
			break;
		case 2:
			sink.BeginCase("Text");
			sink.Child(_payload._ref0);
			sink.EndCase();
			break;
		case 3:
			sink.BeginCase("Id");
			sink.Child(_payload._val0);
			sink.EndCase();
			break;
		case 4:
			sink.BeginCase("Stamp");
			sink.Child(_payload._val1);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct External$Data
{
	internal struct Data_Scalars
	{
		internal int _x;

		internal bool _y;
	}

	internal struct Data_Nested
	{
		internal Inner _inner;
	}

	[FieldOffset(0)]
	internal Data_Scalars Scalars;

	[FieldOffset(0)]
	internal Data_Nested Nested;
}
