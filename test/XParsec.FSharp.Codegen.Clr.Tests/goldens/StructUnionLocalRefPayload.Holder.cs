/*
[<Struct>]
type Holder<'T> =
    | Val of v: 'T
    | Text of s: string
    | Rec of r: Node
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
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

	internal Holder(int _tag, Payload _payload)
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

	[EditorBrowsable(EditorBrowsableState.Never)]
	public T Get_Val_0()
	{
		return _payload._val0;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public string Get_Text_0()
	{
		return (string)_payload._ref0;
	}

	[EditorBrowsable(EditorBrowsableState.Never)]
	public Node Get_Rec_0()
	{
		return (Node)_payload._ref0;
	}

	public Payload_Val GetPayload_Val()
	{
		return new Payload_Val(_payload);
	}

	public Payload_Text GetPayload_Text()
	{
		return new Payload_Text(_payload);
	}

	public Payload_Rec GetPayload_Rec()
	{
		return new Payload_Rec(_payload);
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
			hashCode.Add((string)_payload._ref0);
			break;
		case 2:
			hashCode.Add((Node)_payload._ref0);
			break;
		}
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		object obj2 = ((obj is Holder<T>) ? obj : null);
		if (obj2 != null)
		{
			Holder<T> other = (Holder<T>)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(Holder<T> other)
	{
		if (_tag == other._tag)
		{
			return _tag switch
			{
				0 => EqualityComparer<T>.Default.Equals(_payload._val0, other._payload._val0), 
				1 => string.Equals((string)_payload._ref0, (string)other._payload._ref0), 
				2 => EqualityComparer<Node>.Default.Equals((Node)_payload._ref0, (Node)other._payload._ref0), 
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
			sink.BeginCase("Text");
			sink.Child(_payload._ref0);
			sink.EndCase();
			break;
		case 2:
			sink.BeginCase("Rec");
			sink.Child(_payload._ref0);
			sink.EndCase();
			break;
		}
	}
}
