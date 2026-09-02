/*
[<Struct>]
type Holder<'T> =
    | Val of v: 'T
    | Text of s: string
    | Rec of r: Node
*/

using System;
using System.Collections.Generic;
using Vesper;

[Struct]
public readonly struct Holder<T> : IEquatable<Holder<T>>, IStructuralFormattable
{
	internal struct Payload
	{
		internal object _ref0;

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

	public readonly struct Payload_Text
	{
		private readonly Payload _payload;

		public string s => (string)_payload._ref0;

		internal Payload_Text(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	public readonly struct Payload_Rec
	{
		private readonly Payload _payload;

		public Node r => (Node)_payload._ref0;

		internal Payload_Rec(Payload _payload)
		{
			this._payload = _payload;
		}
	}

	private readonly int _tag;

	internal readonly Payload _payload;

	public int Tag => _tag;

	public Holder(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static Holder<T> Val(T arg0)
	{
		Payload payload = default(Payload);
		payload._val0 = arg0;
		return new Holder<T>(0, payload);
	}

	public static Holder<T> Text(string arg0)
	{
		Payload payload = default(Payload);
		payload._ref0 = arg0;
		return new Holder<T>(1, payload);
	}

	public static Holder<T> Rec(Node arg0)
	{
		Payload payload = default(Payload);
		payload._ref0 = arg0;
		return new Holder<T>(2, payload);
	}

	public T Get_Val_0()
	{
		return _payload._val0;
	}

	public string Get_Text_0()
	{
		return (string)_payload._ref0;
	}

	public Node Get_Rec_0()
	{
		return (Node)_payload._ref0;
	}

	public Payload_Val Get_Val()
	{
		return new Payload_Val(_payload);
	}

	public Payload_Text Get_Text()
	{
		return new Payload_Text(_payload);
	}

	public Payload_Rec Get_Rec()
	{
		return new Payload_Rec(_payload);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(_tag);
		if (_tag != 0)
		{
			if (_tag != 1)
			{
				if (_tag == 2)
				{
					hashCode.Add((Node)_payload._ref0);
				}
			}
			else
			{
				hashCode.Add((string)_payload._ref0);
			}
		}
		else
		{
			hashCode.Add(_payload._val0);
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is Holder<T>) ? obj : null);
		if (obj2 != null)
		{
			Holder<T> holder = (Holder<T>)obj2;
			if (_tag == holder._tag)
			{
				if (_tag != 0)
				{
					if (_tag != 1)
					{
						if (_tag != 2 || EqualityComparer<Node>.Default.Equals((Node)_payload._ref0, (Node)holder._payload._ref0))
						{
							goto IL_00db;
						}
					}
					else if (EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)holder._payload._ref0))
					{
						goto IL_00db;
					}
				}
				else if (EqualityComparer<T>.Default.Equals(_payload._val0, holder._payload._val0))
				{
					goto IL_00db;
				}
			}
		}
		return false;
		IL_00db:
		return true;
	}

	public bool Equals(Holder<T> other)
	{
		if (_tag == other._tag)
		{
			if (_tag != 0)
			{
				if (_tag != 1)
				{
					if (_tag != 2 || EqualityComparer<Node>.Default.Equals((Node)_payload._ref0, (Node)other._payload._ref0))
					{
						goto IL_00c7;
					}
				}
				else if (EqualityComparer<string>.Default.Equals((string)_payload._ref0, (string)other._payload._ref0))
				{
					goto IL_00c7;
				}
			}
			else if (EqualityComparer<T>.Default.Equals(_payload._val0, other._payload._val0))
			{
				goto IL_00c7;
			}
		}
		return false;
		IL_00c7:
		return true;
	}

	public void Format(IFormatSink sink)
	{
		if (_tag != 0)
		{
			if (_tag != 1)
			{
				sink.BeginCase("Rec");
				sink.Child((object)(Node)_payload._ref0);
				sink.EndCase();
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
			sink.BeginCase("Val");
			sink.Child((object)_payload._val0);
			sink.EndCase();
		}
	}
}
