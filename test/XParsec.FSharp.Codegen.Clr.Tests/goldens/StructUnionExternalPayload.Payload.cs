/*
[<Struct>]
type Payload =
    | Scalars of x: int * y: bool
    | Nested of inner: Inner
    | Text of s: string
    | Id of id: System.Guid
    | Stamp of at: System.DateTime
*/

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using Vesper;

[Struct]
public readonly struct Payload : IEquatable<Payload>, IStructuralFormattable
{
	internal struct Payload
	{
		internal Payload$Data _data;

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

	public Payload(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static global::Payload Scalars(int arg0, bool arg1)
	{
		Payload payload = default(Payload);
		payload._data.Scalars._x = arg0;
		payload._data.Scalars._y = arg1;
		return new global::Payload(0, payload);
	}

	public static global::Payload Nested(Inner arg0)
	{
		Payload payload = default(Payload);
		payload._data.Nested._inner = arg0;
		return new global::Payload(1, payload);
	}

	public static global::Payload Text(string arg0)
	{
		Payload payload = default(Payload);
		payload._ref0 = arg0;
		return new global::Payload(2, payload);
	}

	public static global::Payload Id(Guid arg0)
	{
		Payload payload = default(Payload);
		payload._val0 = arg0;
		return new global::Payload(3, payload);
	}

	public static global::Payload Stamp(DateTime arg0)
	{
		Payload payload = default(Payload);
		payload._val1 = arg0;
		return new global::Payload(4, payload);
	}

	public int Get_Scalars_0()
	{
		return _payload._data.Scalars._x;
	}

	public bool Get_Scalars_1()
	{
		return _payload._data.Scalars._y;
	}

	public Inner Get_Nested_0()
	{
		return _payload._data.Nested._inner;
	}

	public string Get_Text_0()
	{
		return (string)_payload._ref0;
	}

	public Guid Get_Id_0()
	{
		return _payload._val0;
	}

	public DateTime Get_Stamp_0()
	{
		return _payload._val1;
	}

	public Payload_Scalars Get_Scalars()
	{
		return new Payload_Scalars(_payload);
	}

	public Payload_Nested Get_Nested()
	{
		return new Payload_Nested(_payload);
	}

	public Payload_Text Get_Text()
	{
		return new Payload_Text(_payload);
	}

	public Payload_Id Get_Id()
	{
		return new Payload_Id(_payload);
	}

	public Payload_Stamp Get_Stamp()
	{
		return new Payload_Stamp(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		if (_tag != 0)
		{
			if (_tag != 1)
			{
				if (_tag != 2)
				{
					if (_tag != 3)
					{
						if (_tag == 4)
						{
							hashCode.Add(_payload._val1);
						}
					}
					else
					{
						hashCode.Add(_payload._val0);
					}
				}
				else
				{
					hashCode.Add((string)_payload._ref0);
				}
			}
			else
			{
				hashCode.Add(_payload._data.Nested._inner);
			}
		}
		else
		{
			hashCode.Add(_payload._data.Scalars._x);
			hashCode.Add(_payload._data.Scalars._y);
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is global::Payload) ? obj : null);
		if (obj2 != null)
		{
			global::Payload payload = (global::Payload)obj2;
			if (_tag == payload._tag)
			{
				if (_tag != 0)
				{
					if (_tag != 1)
					{
						if (_tag != 2)
						{
							if (_tag != 3)
							{
								if (_tag != 4 || EqualityComparer<DateTime>.Default.Equals(_payload._val1, payload._payload._val1))
								{
									goto IL_019e;
								}
							}
							else if (EqualityComparer<Guid>.Default.Equals(_payload._val0, payload._payload._val0))
							{
								goto IL_019e;
							}
						}
						else if (EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)payload._payload._ref0))
						{
							goto IL_019e;
						}
					}
					else if (EqualityComparer<Inner>.Default.Equals(_payload._data.Nested._inner, payload._payload._data.Nested._inner))
					{
						goto IL_019e;
					}
				}
				else if (EqualityComparer<int>.Default.Equals(_payload._data.Scalars._x, payload._payload._data.Scalars._x) && EqualityComparer<bool>.Default.Equals(_payload._data.Scalars._y, payload._payload._data.Scalars._y))
				{
					goto IL_019e;
				}
			}
		}
		return false;
		IL_019e:
		return true;
	}

	public bool Equals(global::Payload other)
	{
		if (_tag == other._tag)
		{
			if (_tag != 0)
			{
				if (_tag != 1)
				{
					if (_tag != 2)
					{
						if (_tag != 3)
						{
							if (_tag != 4 || EqualityComparer<DateTime>.Default.Equals(_payload._val1, other._payload._val1))
							{
								goto IL_018a;
							}
						}
						else if (EqualityComparer<Guid>.Default.Equals(_payload._val0, other._payload._val0))
						{
							goto IL_018a;
						}
					}
					else if (EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)other._payload._ref0))
					{
						goto IL_018a;
					}
				}
				else if (EqualityComparer<Inner>.Default.Equals(_payload._data.Nested._inner, other._payload._data.Nested._inner))
				{
					goto IL_018a;
				}
			}
			else if (EqualityComparer<int>.Default.Equals(_payload._data.Scalars._x, other._payload._data.Scalars._x) && EqualityComparer<bool>.Default.Equals(_payload._data.Scalars._y, other._payload._data.Scalars._y))
			{
				goto IL_018a;
			}
		}
		return false;
		IL_018a:
		return true;
	}

	public void Format(IFormatSink sink)
	{
		if (_tag != 0)
		{
			if (_tag != 1)
			{
				if (_tag != 2)
				{
					if (_tag != 3)
					{
						sink.BeginCase("Stamp");
						sink.Child((object)_payload._val1);
						sink.EndCase();
					}
					else
					{
						sink.BeginCase("Id");
						sink.Child((object)_payload._val0);
						sink.EndCase();
					}
				}
				else
				{
					sink.BeginCase("Text");
					sink.Child((object)(string)_payload._ref0);
					sink.EndCase();
				}
			}
			else
			{
				sink.BeginCase("Nested");
				sink.Child((object)_payload._data.Nested._inner);
				sink.EndCase();
			}
		}
		else
		{
			sink.BeginCase("Scalars");
			sink.Child((object)_payload._data.Scalars._x);
			sink.Child((object)_payload._data.Scalars._y);
			sink.EndCase();
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct Payload$Data
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
