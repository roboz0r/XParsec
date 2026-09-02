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

	public Storage(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static Storage Scalars(int arg0, bool arg1)
	{
		Payload payload = default(Payload);
		payload._data.Scalars._x = arg0;
		payload._data.Scalars._y = arg1;
		return new Storage(0, payload);
	}

	public static Storage Nested(Inner arg0)
	{
		Payload payload = default(Payload);
		payload._data.Nested._inner = arg0;
		return new Storage(1, payload);
	}

	public static Storage Text(string arg0)
	{
		Payload payload = default(Payload);
		payload._ref0 = arg0;
		return new Storage(2, payload);
	}

	public static Storage Labelled(Tagged arg0)
	{
		Payload payload = default(Payload);
		payload._val0 = arg0;
		return new Storage(3, payload);
	}

	public static Storage Id(Guid arg0)
	{
		Payload payload = default(Payload);
		payload._val1 = arg0;
		return new Storage(4, payload);
	}

	public static Storage Both(int arg0, string arg1)
	{
		Payload payload = default(Payload);
		payload._data.Both._k = arg0;
		payload._ref0 = arg1;
		return new Storage(5, payload);
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

	public Tagged Get_Labelled_0()
	{
		return _payload._val0;
	}

	public Guid Get_Id_0()
	{
		return _payload._val1;
	}

	public int Get_Both_0()
	{
		return _payload._data.Both._k;
	}

	public string Get_Both_1()
	{
		return (string)_payload._ref0;
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

	public Payload_Labelled Get_Labelled()
	{
		return new Payload_Labelled(_payload);
	}

	public Payload_Id Get_Id()
	{
		return new Payload_Id(_payload);
	}

	public Payload_Both Get_Both()
	{
		return new Payload_Both(_payload);
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
						if (_tag != 4)
						{
							if (_tag == 5)
							{
								hashCode.Add(_payload._data.Both._k);
								hashCode.Add((string)_payload._ref0);
							}
						}
						else
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
		object obj2 = ((obj is Storage) ? obj : null);
		if (obj2 != null)
		{
			Storage storage = (Storage)obj2;
			if (_tag == storage._tag)
			{
				if (_tag != 0)
				{
					if (_tag != 1)
					{
						if (_tag != 2)
						{
							if (_tag != 3)
							{
								if (_tag != 4)
								{
									if (_tag != 5 || (EqualityComparer<int>.Default.Equals(_payload._data.Both._k, storage._payload._data.Both._k) && EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)storage._payload._ref0)))
									{
										goto IL_0217;
									}
								}
								else if (EqualityComparer<Guid>.Default.Equals(_payload._val1, storage._payload._val1))
								{
									goto IL_0217;
								}
							}
							else if (EqualityComparer<Tagged>.Default.Equals(_payload._val0, storage._payload._val0))
							{
								goto IL_0217;
							}
						}
						else if (EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)storage._payload._ref0))
						{
							goto IL_0217;
						}
					}
					else if (EqualityComparer<Inner>.Default.Equals(_payload._data.Nested._inner, storage._payload._data.Nested._inner))
					{
						goto IL_0217;
					}
				}
				else if (EqualityComparer<int>.Default.Equals(_payload._data.Scalars._x, storage._payload._data.Scalars._x) && EqualityComparer<bool>.Default.Equals(_payload._data.Scalars._y, storage._payload._data.Scalars._y))
				{
					goto IL_0217;
				}
			}
		}
		return false;
		IL_0217:
		return true;
	}

	public bool Equals(Storage other)
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
							if (_tag != 4)
							{
								if (_tag != 5 || (EqualityComparer<int>.Default.Equals(_payload._data.Both._k, other._payload._data.Both._k) && EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)other._payload._ref0)))
								{
									goto IL_0203;
								}
							}
							else if (EqualityComparer<Guid>.Default.Equals(_payload._val1, other._payload._val1))
							{
								goto IL_0203;
							}
						}
						else if (EqualityComparer<Tagged>.Default.Equals(_payload._val0, other._payload._val0))
						{
							goto IL_0203;
						}
					}
					else if (EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)other._payload._ref0))
					{
						goto IL_0203;
					}
				}
				else if (EqualityComparer<Inner>.Default.Equals(_payload._data.Nested._inner, other._payload._data.Nested._inner))
				{
					goto IL_0203;
				}
			}
			else if (EqualityComparer<int>.Default.Equals(_payload._data.Scalars._x, other._payload._data.Scalars._x) && EqualityComparer<bool>.Default.Equals(_payload._data.Scalars._y, other._payload._data.Scalars._y))
			{
				goto IL_0203;
			}
		}
		return false;
		IL_0203:
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
						if (_tag != 4)
						{
							sink.BeginCase("Both");
							sink.Child((object)_payload._data.Both._k);
							sink.Child((object)(string)_payload._ref0);
							sink.EndCase();
						}
						else
						{
							sink.BeginCase("Id");
							sink.Child((object)_payload._val1);
							sink.EndCase();
						}
					}
					else
					{
						sink.BeginCase("Labelled");
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
