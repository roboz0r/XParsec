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

	public static Meters M(int item)
	{
		return new Meters(item);
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
		sink.Child(item);
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
			sink.Child(_len);
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

	public static Shape Line(int _len)
	{
		return new Line(_len);
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
			sink.Child(item);
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
			sink.Child(item1);
			sink.Child(item2);
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
			sink.Child(item1);
			sink.Child(item2);
			sink.Child(item3);
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

	public static Quad Q1(int item)
	{
		return new Q1(item);
	}

	public static Quad Q2(int item1, int item2)
	{
		return new Q2(item1, item2);
	}

	public static Quad Q3(int item1, int item2, int item3)
	{
		return new Q3(item1, item2, item3);
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
	public static int metersValue(Meters m)
	{
		return m.item;
	}

	public static int describeShape(Shape s)
	{
		int result;
		if (s is Shape.Dot)
		{
			result = 0;
		}
		else
		{
			if (!(s is Shape.Line { _len: var len }))
			{
				throw new Exception("The match cases were incomplete");
			}
			result = len;
		}
		return result;
	}

	public static int describeQuad(Quad q)
	{
		int result;
		if (q.Tag == 0)
		{
			result = 0;
		}
		else if (q.Tag == 1)
		{
			Quad.Q1 q2 = (Quad.Q1)q;
			int item = q2.item;
			result = item;
		}
		else if (q.Tag == 2)
		{
			Quad.Q2 q3 = (Quad.Q2)q;
			int item2 = q3.item1;
			int item3 = q3.item2;
			result = item2 + item3;
		}
		else
		{
			if (q.Tag != 3)
			{
				throw new Exception("The match cases were incomplete");
			}
			Quad.Q3 q4 = (Quad.Q3)q;
			int item4 = q4.item1;
			int item5 = q4.item2;
			int item6 = q4.item3;
			result = item4 + item5 + item6;
		}
		return result;
	}

	public static int Main(string[] args)
	{
		Formatter formatter = new Formatter(0, 1, Console.Out);
		formatter.AppendFormatted(metersValue(Meters.M(7)));
		formatter.AppendLiteral("\n");
		formatter.Flush();
		Formatter formatter2 = new Formatter(0, 1, Console.Out);
		formatter2.AppendFormatted(describeShape(Shape.Dot()));
		formatter2.AppendLiteral("\n");
		formatter2.Flush();
		Formatter formatter3 = new Formatter(0, 1, Console.Out);
		formatter3.AppendFormatted(describeShape(Shape.Line(4)));
		formatter3.AppendLiteral("\n");
		formatter3.Flush();
		Formatter formatter4 = new Formatter(0, 1, Console.Out);
		formatter4.AppendFormatted(describeQuad(Quad.Q0()));
		formatter4.AppendLiteral("\n");
		formatter4.Flush();
		Formatter formatter5 = new Formatter(0, 1, Console.Out);
		formatter5.AppendFormatted(describeQuad(Quad.Q1(5)));
		formatter5.AppendLiteral("\n");
		formatter5.Flush();
		Formatter formatter6 = new Formatter(0, 1, Console.Out);
		formatter6.AppendFormatted(describeQuad(Quad.Q2(2, 3)));
		formatter6.AppendLiteral("\n");
		formatter6.Flush();
		Formatter formatter7 = new Formatter(0, 1, Console.Out);
		formatter7.AppendFormatted(describeQuad(Quad.Q3(1, 2, 3)));
		formatter7.AppendLiteral("\n");
		formatter7.Flush();
		Formatter formatter8 = new Formatter(0, 1, Console.Out);
		Meters x = Meters.M(7);
		Meters y = Meters.M(7);
		formatter8.AppendBool(EqualityComparer<Meters>.Default.Equals(x, y), 0);
		formatter8.AppendLiteral("\n");
		formatter8.Flush();
		Formatter formatter9 = new Formatter(0, 1, Console.Out);
		Meters x2 = Meters.M(7);
		Meters y2 = Meters.M(8);
		formatter9.AppendBool(EqualityComparer<Meters>.Default.Equals(x2, y2), 0);
		formatter9.AppendLiteral("\n");
		formatter9.Flush();
		Formatter formatter10 = new Formatter(0, 1, Console.Out);
		Shape x3 = Shape.Dot();
		Shape y3 = Shape.Dot();
		formatter10.AppendBool(EqualityComparer<Shape>.Default.Equals(x3, y3), 0);
		formatter10.AppendLiteral("\n");
		formatter10.Flush();
		Formatter formatter11 = new Formatter(0, 1, Console.Out);
		Shape x4 = Shape.Line(4);
		Shape y4 = Shape.Line(4);
		formatter11.AppendBool(EqualityComparer<Shape>.Default.Equals(x4, y4), 0);
		formatter11.AppendLiteral("\n");
		formatter11.Flush();
		Formatter formatter12 = new Formatter(0, 1, Console.Out);
		Shape x5 = Shape.Line(4);
		Shape y5 = Shape.Line(5);
		formatter12.AppendBool(EqualityComparer<Shape>.Default.Equals(x5, y5), 0);
		formatter12.AppendLiteral("\n");
		formatter12.Flush();
		Formatter formatter13 = new Formatter(0, 1, Console.Out);
		Shape x6 = Shape.Line(4);
		Shape y6 = Shape.Dot();
		formatter13.AppendBool(EqualityComparer<Shape>.Default.Equals(x6, y6), 0);
		formatter13.AppendLiteral("\n");
		formatter13.Flush();
		Formatter formatter14 = new Formatter(0, 1, Console.Out);
		Quad x7 = Quad.Q0();
		Quad y7 = Quad.Q0();
		formatter14.AppendBool(EqualityComparer<Quad>.Default.Equals(x7, y7), 0);
		formatter14.AppendLiteral("\n");
		formatter14.Flush();
		Formatter formatter15 = new Formatter(0, 1, Console.Out);
		Quad x8 = Quad.Q2(2, 3);
		Quad y8 = Quad.Q2(2, 3);
		formatter15.AppendBool(EqualityComparer<Quad>.Default.Equals(x8, y8), 0);
		formatter15.AppendLiteral("\n");
		formatter15.Flush();
		Formatter formatter16 = new Formatter(0, 1, Console.Out);
		Quad x9 = Quad.Q2(2, 3);
		Quad y9 = Quad.Q3(1, 2, 3);
		formatter16.AppendBool(EqualityComparer<Quad>.Default.Equals(x9, y9), 0);
		formatter16.AppendLiteral("\n");
		formatter16.Flush();
		return 0;
	}
}
