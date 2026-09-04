/*
[<Struct>]
type Storage =
    | Scalars of x: int * y: bool
    | Nested of inner: Inner
    | Text of s: string
    | Labelled of t: Tagged
    | Id of id: System.Guid
    | Both of k: int * name: string
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Vesper;

[Struct]
public readonly struct Storage : IEquatable<Storage>, IStructuralFormattable
{
	internal struct Payload
	{
		internal Storage$Data _data;

		internal object _ref0;

		internal Tagged _val0;

		internal Guid _val1;
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

	public readonly struct Payload_Labelled
	{
		private readonly Payload _payload;

		public Tagged t => _payload._val0;

		internal Payload_Labelled(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Id
	{
		private readonly Payload _payload;

		public Guid id => _payload._val1;

		internal Payload_Id(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Both
	{
		private readonly Payload _payload;

		public int k => _payload._data.Both._k;

		public string name => (string)_payload._ref0;

		internal Payload_Both(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	internal Storage(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static Storage Scalars(int _x, bool _y)
	{
		Payload payload = default(Payload);
		payload._data.Scalars._x = _x;
		payload._data.Scalars._y = _y;
		return new Storage(0, payload);
	}

	public static Storage Nested(Inner _inner)
	{
		Payload payload = default(Payload);
		payload._data.Nested._inner = _inner;
		return new Storage(1, payload);
	}

	public static Storage Text(string _s)
	{
		Payload payload = default(Payload);
		payload._ref0 = _s;
		return new Storage(2, payload);
	}

	public static Storage Labelled(Tagged _t)
	{
		Payload payload = default(Payload);
		payload._val0 = _t;
		return new Storage(3, payload);
	}

	public static Storage Id(Guid _id)
	{
		Payload payload = default(Payload);
		payload._val1 = _id;
		return new Storage(4, payload);
	}

	public static Storage Both(int _k, string _name)
	{
		Payload payload = default(Payload);
		payload._data.Both._k = _k;
		payload._ref0 = _name;
		return new Storage(5, payload);
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
	public Tagged Get_Labelled_0()
	{
		return _payload._val0;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public Guid Get_Id_0()
	{
		return _payload._val1;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public int Get_Both_0()
	{
		return _payload._data.Both._k;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public string Get_Both_1()
	{
		return (string)_payload._ref0;
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

	public Payload_Labelled GetPayload_Labelled()
	{
		return new Payload_Labelled(_payload);
	}

	public Payload_Id GetPayload_Id()
	{
		return new Payload_Id(_payload);
	}

	public Payload_Both GetPayload_Both()
	{
		return new Payload_Both(_payload);
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
		case 5:
			hashCode.Add(_payload._data.Both._k);
			hashCode.Add((string)_payload._ref0);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		if (obj is Storage)
		{
			return Equals((Storage)obj);
		}
		return false;
	}

	public bool Equals(Storage other)
	{
		if (_tag == other._tag)
		{
			switch (_tag)
			{
			case 0:
				if (_payload._data.Scalars._x == other._payload._data.Scalars._x)
				{
					return _payload._data.Scalars._y == other._payload._data.Scalars._y;
				}
				break;
			case 1:
				return EqualityComparer<Inner>.Default.Equals(_payload._data.Nested._inner, other._payload._data.Nested._inner);
			case 2:
				return string.Equals((string)_payload._ref0, (string)other._payload._ref0);
			case 3:
				return EqualityComparer<Tagged>.Default.Equals(_payload._val0, other._payload._val0);
			case 4:
				return EqualityComparer<Guid>.Default.Equals(_payload._val1, other._payload._val1);
			case 5:
				if (_payload._data.Both._k == other._payload._data.Both._k)
				{
					return string.Equals((string)_payload._ref0, (string)other._payload._ref0);
				}
				break;
			default:
				return true;
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
			sink.BeginCase("Labelled");
			sink.Child(_payload._val0);
			sink.EndCase();
			break;
		case 4:
			sink.BeginCase("Id");
			sink.Child(_payload._val1);
			sink.EndCase();
			break;
		case 5:
			sink.BeginCase("Both");
			sink.Child(_payload._data.Both._k);
			sink.Child(_payload._ref0);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct Storage$Data
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

	internal struct Data_Both
	{
		internal int _k;
	}

	[FieldOffset(0)]
	internal Data_Scalars Scalars;

	[FieldOffset(0)]
	internal Data_Nested Nested;

	[FieldOffset(0)]
	internal Data_Both Both;
}
