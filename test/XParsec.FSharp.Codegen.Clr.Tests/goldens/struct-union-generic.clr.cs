/*
// A generic [<Struct>] union: the 'T payload field is an ordinary generic value-type
// field on the CLR, and an ordinary property on JS. Construction, matching and
// structural equality must agree across the two backends.
[<Struct>]
type G<'T> =
    | Val of v: 'T
    | Num of n: int

let pick (g: G<int>) : int =
    match g with
    | Val v -> v
    | Num n -> n * 10

let a: G<int> = Val 3
let b: G<int> = Num 4

printfn "%d" (pick a)
printfn "%d" (pick b)
printfn "%b" (a = Val 3)
printfn "%b" (a = b)

let s: G<string> = Val "hi"

let text (g: G<string>) : string =
    match g with
    | Val v -> v
    | Num _ -> "num"

printfn "%s" (text s)
printfn "%b" (s = Val "hi")
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.InteropServices;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
[Struct]
public readonly struct G<T> : IEquatable<G<T>>, IStructuralFormattable
{
	internal struct Payload
	{
		internal G$Data$1 _data;

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

	internal G(int _tag, Payload _payload)
	{
		this._tag = _tag;
		this._payload = _payload;
	}

	public static G<T> Val(T arg0)
	{
		Payload payload = default(Payload);
		payload._val0 = arg0;
		return new G<T>(0, payload);
	}

	public static G<T> Num(int arg0)
	{
		Payload payload = default(Payload);
		payload._data.Num._n = arg0;
		return new G<T>(1, payload);
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
		object obj2 = ((obj is G<T>) ? obj : null);
		if (obj2 != null)
		{
			G<T> other = (G<T>)obj2;
			return Equals(other);
		}
		return false;
	}

	public bool Equals(G<T> other)
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
			sink.Child((object)_payload._val0);
			sink.EndCase();
			break;
		case 1:
			sink.BeginCase("Num");
			sink.Child((object)_payload._data.Num._n);
			sink.EndCase();
			break;
		}
	}
}
[StructLayout(LayoutKind.Explicit)]
internal struct G$Data$1
{
	internal struct Data_Num
	{
		internal int _n;
	}

	[FieldOffset(0)]
	internal Data_Num Num;
}
public static class Program
{
	public static readonly G<int> a;

	public static readonly G<int> b;

	public static G<string> s;

	static Program()
	{
		a = G<int>.Val(3);
		b = G<int>.Num(4);
	}

	public static int pick(G<int> arg0)
	{
		G<int> g = arg0;
		int result;
		if (g.Tag == 0)
		{
			int val = g._payload._val0;
			result = val;
		}
		else
		{
			if (g.Tag != 1)
			{
				throw new Exception("The match cases were incomplete");
			}
			int n = g._payload._data.Num._n;
			int num = n;
			int num2 = 10;
			int num3 = num;
			int num4 = num2;
			result = num3 * num4;
		}
		return result;
	}

	public static string text(G<string> arg0)
	{
		G<string> g = arg0;
		object result;
		if (g.Tag == 0)
		{
			string val = g._payload._val0;
			result = val;
		}
		else
		{
			if (g.Tag != 1)
			{
				throw new Exception("The match cases were incomplete");
			}
			result = "num";
		}
		return (string)result;
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(pick(a));
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(pick(b));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		G<int> x = a;
		G<int> y = G<int>.Val(3);
		((Formatter)(ref val3)).AppendBool(EqualityComparer<G<int>>.Default.Equals(x, y), 0);
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		G<int> x2 = a;
		G<int> y2 = b;
		((Formatter)(ref val4)).AppendBool(EqualityComparer<G<int>>.Default.Equals(x2, y2), 0);
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		s = G<string>.Val("hi");
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		((Formatter)(ref val5)).AppendFormatted<string>(text(s));
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		G<string> x3 = s;
		G<string> y3 = G<string>.Val("hi");
		((Formatter)(ref val6)).AppendBool(EqualityComparer<G<string>>.Default.Equals(x3, y3), 0);
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		return 0;
	}
}
