/*
// Reference unions across the emitter's regimes: a single-case union (no discriminant),
// a two-case type-tested union, and a four-case tagged union, exercising construction,
// matching and structural equality on both backends.
type Meters = | M of int

type Shape =
    | Dot
    | Line of len: int

type Quad =
    | Q0
    | Q1 of int
    | Q2 of int * int
    | Q3 of int * int * int

let metersValue (m: Meters) : int =
    match m with
    | M v -> v

let describeShape (s: Shape) : int =
    match s with
    | Dot -> 0
    | Line len -> len

let describeQuad (q: Quad) : int =
    match q with
    | Q0 -> 0
    | Q1 x -> x
    | Q2(a, b) -> a + b
    | Q3(a, b, c) -> a + b + c

printfn "%d" (metersValue (M 7))
printfn "%d" (describeShape Dot)
printfn "%d" (describeShape (Line 4))
printfn "%d" (describeQuad Q0)
printfn "%d" (describeQuad (Q1 5))
printfn "%d" (describeQuad (Q2(2, 3)))
printfn "%d" (describeQuad (Q3(1, 2, 3)))
printfn "%b" (M 7 = M 7)
printfn "%b" (M 7 = M 8)
printfn "%b" (Dot = Dot)
printfn "%b" (Line 4 = Line 4)
printfn "%b" (Line 4 = Line 5)
printfn "%b" (Line 4 = Dot)
printfn "%b" (Q0 = Q0)
printfn "%b" (Q2(2, 3) = Q2(2, 3))
printfn "%b" (Q2(2, 3) = Q3(1, 2, 3))
*/

using System;
using System.Collections.Generic;
using System.Reflection;
using Vesper;

[assembly: AssemblyVersion("1.0.0.0")]
public sealed class Meters : IEquatable<Meters>, IStructuralFormattable
{
	public readonly int item;

	internal Meters(int item)
	{
		this.item = item;
	}

	public static Meters M(int arg0)
	{
		return new Meters(arg0);
	}

	public override int GetHashCode()
	{
		HashCode hashCode = default(HashCode);
		hashCode.Add(item);
		return hashCode.ToHashCode();
	}

	public override bool Equals(object obj)
	{
		if (obj is Meters other)
		{
			return Equals(other);
		}
		return false;
	}

	public bool Equals(Meters other)
	{
		if (other != null)
		{
			return item == other.item;
		}
		return false;
	}

	public void Format(IFormatSink sink)
	{
		sink.BeginCase("M");
		sink.Child((object)item);
		sink.EndCase();
	}
}
public abstract class Shape : IEquatable<Shape>, IStructuralFormattable
{
	public sealed class Dot : Shape
	{
		public override int GetHashCode()
		{
			HashCode hashCode = default(HashCode);
			hashCode.Add(0);
			return hashCode.ToHashCode();
		}

		public override bool Equals(Shape other)
		{
			return Equals(other as Dot);
		}

		public bool Equals(Dot other)
		{
			if (other != null)
			{
				return true;
			}
			return false;
		}

		public override void Format(IFormatSink sink)
		{
			sink.BeginCase("Dot");
			sink.EndCase();
		}
	}

	public sealed class Line : Shape
	{
		public readonly int _len;

		public Line(int _len)
		{
			this._len = _len;
		}

		public override int GetHashCode()
		{
			HashCode hashCode = default(HashCode);
			hashCode.Add(1);
			hashCode.Add(_len);
			return hashCode.ToHashCode();
		}

		public override bool Equals(Shape other)
		{
			return Equals(other as Line);
		}

		public bool Equals(Line other)
		{
			if (other != null)
			{
				return _len == other._len;
			}
			return false;
		}

		public override void Format(IFormatSink sink)
		{
			sink.BeginCase("Line");
			sink.Child((object)_len);
			sink.EndCase();
		}
	}

	private static readonly Shape _unique_Dot = new Dot();

	internal Shape()
	{
	}

	public static Shape Dot()
	{
		return _unique_Dot;
	}

	public static Shape Line(int arg0)
	{
		return new Line(arg0);
	}

	public abstract override int GetHashCode();

	public override bool Equals(object obj)
	{
		return Equals(obj as Shape);
	}

	public abstract bool Equals(Shape other);

	public abstract void Format(IFormatSink sink);
}
public abstract class Quad : IEquatable<Quad>, IStructuralFormattable
{
	public sealed class Q0 : Quad
	{
		public Q0()
			: base(0)
		{
		}

		public override int GetHashCode()
		{
			HashCode hashCode = default(HashCode);
			hashCode.Add(0);
			return hashCode.ToHashCode();
		}

		public override bool Equals(Quad other)
		{
			return Equals(other as Q0);
		}

		public bool Equals(Q0 other)
		{
			if (other != null)
			{
				return true;
			}
			return false;
		}

		public override void Format(IFormatSink sink)
		{
			sink.BeginCase("Q0");
			sink.EndCase();
		}
	}

	public sealed class Q1 : Quad
	{
		public readonly int item;

		public Q1(int item)
			: base(1)
		{
			this.item = item;
		}

		public override int GetHashCode()
		{
			HashCode hashCode = default(HashCode);
			hashCode.Add(1);
			hashCode.Add(item);
			return hashCode.ToHashCode();
		}

		public override bool Equals(Quad other)
		{
			return Equals(other as Q1);
		}

		public bool Equals(Q1 other)
		{
			if (other != null)
			{
				return item == other.item;
			}
			return false;
		}

		public override void Format(IFormatSink sink)
		{
			sink.BeginCase("Q1");
			sink.Child((object)item);
			sink.EndCase();
		}
	}

	public sealed class Q2 : Quad
	{
		public readonly int item1;

		public readonly int item2;

		public Q2(int item1, int item2)
			: base(2)
		{
			this.item1 = item1;
			this.item2 = item2;
		}

		public override int GetHashCode()
		{
			HashCode hashCode = default(HashCode);
			hashCode.Add(2);
			hashCode.Add(item1);
			hashCode.Add(item2);
			return hashCode.ToHashCode();
		}

		public override bool Equals(Quad other)
		{
			return Equals(other as Q2);
		}

		public bool Equals(Q2 other)
		{
			if (other != null && item1 == other.item1)
			{
				return item2 == other.item2;
			}
			return false;
		}

		public override void Format(IFormatSink sink)
		{
			sink.BeginCase("Q2");
			sink.Child((object)item1);
			sink.Child((object)item2);
			sink.EndCase();
		}
	}

	public sealed class Q3 : Quad
	{
		public readonly int item1;

		public readonly int item2;

		public readonly int item3;

		public Q3(int item1, int item2, int item3)
			: base(3)
		{
			this.item1 = item1;
			this.item2 = item2;
			this.item3 = item3;
		}

		public override int GetHashCode()
		{
			HashCode hashCode = default(HashCode);
			hashCode.Add(3);
			hashCode.Add(item1);
			hashCode.Add(item2);
			hashCode.Add(item3);
			return hashCode.ToHashCode();
		}

		public override bool Equals(Quad other)
		{
			return Equals(other as Q3);
		}

		public bool Equals(Q3 other)
		{
			if (other != null && item1 == other.item1 && item2 == other.item2)
			{
				return item3 == other.item3;
			}
			return false;
		}

		public override void Format(IFormatSink sink)
		{
			sink.BeginCase("Q3");
			sink.Child((object)item1);
			sink.Child((object)item2);
			sink.Child((object)item3);
			sink.EndCase();
		}
	}

	private readonly int _tag;

	private static readonly Quad _unique_Q0 = new Q0();

	public int Tag => _tag;

	internal Quad(int _tag)
	{
		this._tag = _tag;
	}

	public static Quad Q0()
	{
		return _unique_Q0;
	}

	public static Quad Q1(int arg0)
	{
		return new Q1(arg0);
	}

	public static Quad Q2(int arg0, int arg1)
	{
		return new Q2(arg0, arg1);
	}

	public static Quad Q3(int arg0, int arg1, int arg2)
	{
		return new Q3(arg0, arg1, arg2);
	}

	public abstract override int GetHashCode();

	public override bool Equals(object obj)
	{
		return Equals(obj as Quad);
	}

	public abstract bool Equals(Quad other);

	public abstract void Format(IFormatSink sink);
}
public static class Program
{
	public static int metersValue(Meters arg0)
	{
		return arg0.item;
	}

	public static int describeShape(Shape arg0)
	{
		int result;
		if (arg0 is Shape.Dot)
		{
			result = 0;
		}
		else
		{
			if (!(arg0 is Shape.Line { _len: var len }))
			{
				throw new Exception("The match cases were incomplete");
			}
			result = len;
		}
		return result;
	}

	public static int describeQuad(Quad arg0)
	{
		int result;
		if (arg0.Tag == 0)
		{
			result = 0;
		}
		else if (arg0.Tag == 1)
		{
			Quad.Q1 q = (Quad.Q1)arg0;
			int item = q.item;
			result = item;
		}
		else if (arg0.Tag == 2)
		{
			Quad.Q2 q2 = (Quad.Q2)arg0;
			int item2 = q2.item1;
			int item3 = q2.item2;
			int num = item2;
			int num2 = item3;
			int num3 = num;
			int num4 = num2;
			result = num3 + num4;
		}
		else
		{
			if (arg0.Tag != 3)
			{
				throw new Exception("The match cases were incomplete");
			}
			Quad.Q3 q3 = (Quad.Q3)arg0;
			int item4 = q3.item1;
			int item5 = q3.item2;
			int item6 = q3.item3;
			int num5 = item4;
			int num6 = item5;
			int num7 = num5;
			int num8 = num6;
			int num9 = num7 + num8;
			int num10 = item6;
			int num11 = num9;
			int num12 = num10;
			result = num11 + num12;
		}
		return result;
	}

	public static int Main(string[] args)
	{
		Formatter val = default(Formatter);
		((Formatter)(ref val))..ctor(0, 1, Console.Out);
		((Formatter)(ref val)).AppendFormatted<int>(metersValue(Meters.M(7)));
		((Formatter)(ref val)).AppendLiteral("\n");
		((Formatter)(ref val)).Flush();
		ValueTuple valueTuple = default(ValueTuple);
		Formatter val2 = default(Formatter);
		((Formatter)(ref val2))..ctor(0, 1, Console.Out);
		((Formatter)(ref val2)).AppendFormatted<int>(describeShape(Shape.Dot()));
		((Formatter)(ref val2)).AppendLiteral("\n");
		((Formatter)(ref val2)).Flush();
		ValueTuple valueTuple2 = default(ValueTuple);
		Formatter val3 = default(Formatter);
		((Formatter)(ref val3))..ctor(0, 1, Console.Out);
		((Formatter)(ref val3)).AppendFormatted<int>(describeShape(Shape.Line(4)));
		((Formatter)(ref val3)).AppendLiteral("\n");
		((Formatter)(ref val3)).Flush();
		ValueTuple valueTuple3 = default(ValueTuple);
		Formatter val4 = default(Formatter);
		((Formatter)(ref val4))..ctor(0, 1, Console.Out);
		((Formatter)(ref val4)).AppendFormatted<int>(describeQuad(Quad.Q0()));
		((Formatter)(ref val4)).AppendLiteral("\n");
		((Formatter)(ref val4)).Flush();
		ValueTuple valueTuple4 = default(ValueTuple);
		Formatter val5 = default(Formatter);
		((Formatter)(ref val5))..ctor(0, 1, Console.Out);
		((Formatter)(ref val5)).AppendFormatted<int>(describeQuad(Quad.Q1(5)));
		((Formatter)(ref val5)).AppendLiteral("\n");
		((Formatter)(ref val5)).Flush();
		ValueTuple valueTuple5 = default(ValueTuple);
		Formatter val6 = default(Formatter);
		((Formatter)(ref val6))..ctor(0, 1, Console.Out);
		((Formatter)(ref val6)).AppendFormatted<int>(describeQuad(Quad.Q2(2, 3)));
		((Formatter)(ref val6)).AppendLiteral("\n");
		((Formatter)(ref val6)).Flush();
		ValueTuple valueTuple6 = default(ValueTuple);
		Formatter val7 = default(Formatter);
		((Formatter)(ref val7))..ctor(0, 1, Console.Out);
		((Formatter)(ref val7)).AppendFormatted<int>(describeQuad(Quad.Q3(1, 2, 3)));
		((Formatter)(ref val7)).AppendLiteral("\n");
		((Formatter)(ref val7)).Flush();
		ValueTuple valueTuple7 = default(ValueTuple);
		Formatter val8 = default(Formatter);
		((Formatter)(ref val8))..ctor(0, 1, Console.Out);
		Meters x = Meters.M(7);
		Meters y = Meters.M(7);
		((Formatter)(ref val8)).AppendBool(EqualityComparer<Meters>.Default.Equals(x, y), 0);
		((Formatter)(ref val8)).AppendLiteral("\n");
		((Formatter)(ref val8)).Flush();
		ValueTuple valueTuple8 = default(ValueTuple);
		Formatter val9 = default(Formatter);
		((Formatter)(ref val9))..ctor(0, 1, Console.Out);
		Meters x2 = Meters.M(7);
		Meters y2 = Meters.M(8);
		((Formatter)(ref val9)).AppendBool(EqualityComparer<Meters>.Default.Equals(x2, y2), 0);
		((Formatter)(ref val9)).AppendLiteral("\n");
		((Formatter)(ref val9)).Flush();
		ValueTuple valueTuple9 = default(ValueTuple);
		Formatter val10 = default(Formatter);
		((Formatter)(ref val10))..ctor(0, 1, Console.Out);
		Shape x3 = Shape.Dot();
		Shape y3 = Shape.Dot();
		((Formatter)(ref val10)).AppendBool(EqualityComparer<Shape>.Default.Equals(x3, y3), 0);
		((Formatter)(ref val10)).AppendLiteral("\n");
		((Formatter)(ref val10)).Flush();
		ValueTuple valueTuple10 = default(ValueTuple);
		Formatter val11 = default(Formatter);
		((Formatter)(ref val11))..ctor(0, 1, Console.Out);
		Shape x4 = Shape.Line(4);
		Shape y4 = Shape.Line(4);
		((Formatter)(ref val11)).AppendBool(EqualityComparer<Shape>.Default.Equals(x4, y4), 0);
		((Formatter)(ref val11)).AppendLiteral("\n");
		((Formatter)(ref val11)).Flush();
		ValueTuple valueTuple11 = default(ValueTuple);
		Formatter val12 = default(Formatter);
		((Formatter)(ref val12))..ctor(0, 1, Console.Out);
		Shape x5 = Shape.Line(4);
		Shape y5 = Shape.Line(5);
		((Formatter)(ref val12)).AppendBool(EqualityComparer<Shape>.Default.Equals(x5, y5), 0);
		((Formatter)(ref val12)).AppendLiteral("\n");
		((Formatter)(ref val12)).Flush();
		ValueTuple valueTuple12 = default(ValueTuple);
		Formatter val13 = default(Formatter);
		((Formatter)(ref val13))..ctor(0, 1, Console.Out);
		Shape x6 = Shape.Line(4);
		Shape y6 = Shape.Dot();
		((Formatter)(ref val13)).AppendBool(EqualityComparer<Shape>.Default.Equals(x6, y6), 0);
		((Formatter)(ref val13)).AppendLiteral("\n");
		((Formatter)(ref val13)).Flush();
		ValueTuple valueTuple13 = default(ValueTuple);
		Formatter val14 = default(Formatter);
		((Formatter)(ref val14))..ctor(0, 1, Console.Out);
		Quad x7 = Quad.Q0();
		Quad y7 = Quad.Q0();
		((Formatter)(ref val14)).AppendBool(EqualityComparer<Quad>.Default.Equals(x7, y7), 0);
		((Formatter)(ref val14)).AppendLiteral("\n");
		((Formatter)(ref val14)).Flush();
		ValueTuple valueTuple14 = default(ValueTuple);
		Formatter val15 = default(Formatter);
		((Formatter)(ref val15))..ctor(0, 1, Console.Out);
		Quad x8 = Quad.Q2(2, 3);
		Quad y8 = Quad.Q2(2, 3);
		((Formatter)(ref val15)).AppendBool(EqualityComparer<Quad>.Default.Equals(x8, y8), 0);
		((Formatter)(ref val15)).AppendLiteral("\n");
		((Formatter)(ref val15)).Flush();
		ValueTuple valueTuple15 = default(ValueTuple);
		Formatter val16 = default(Formatter);
		((Formatter)(ref val16))..ctor(0, 1, Console.Out);
		Quad x9 = Quad.Q2(2, 3);
		Quad y9 = Quad.Q3(1, 2, 3);
		((Formatter)(ref val16)).AppendBool(EqualityComparer<Quad>.Default.Equals(x9, y9), 0);
		((Formatter)(ref val16)).AppendLiteral("\n");
		((Formatter)(ref val16)).Flush();
		ValueTuple valueTuple16 = default(ValueTuple);
		return 0;
	}
}
