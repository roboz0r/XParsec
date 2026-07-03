using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Text;

namespace Vesper;

// The `%A` structural formatter (reflection-free).
//
// Target: `%A v` produces copy-pasteable Vesper source. Layout: a group-based
// pretty-printer — each composite is a group rendered ALL-flat (if its flat form
// fits the width budget) or ALL-broken (every soft break in it becomes a
// newline). No node is ever half-broken, so there is no stair-shape (the failure
// mode of F#'s greedy `squashToAux`, sformat.fs:666).
//
// The implemented interfaces (`IStructuralFormattable` / `IFormatSink`) are owned
// by `Vesper.Core` so a synthesised record/DU `Format` implements a Core type
// (every program links Core) without leaking a `Vesper.Printf` dependency into
// every record-bearing program. `RuntimeFormatState` below implements the
// Core-owned `Vesper.IFormatSink`, bound at C# build time through the committed
// `refs/Vesper.Core.dll` reference (see the .csproj).

/// <summary>The recorded layout document. A group is rendered all-flat or
/// all-broken; nesting governs the indent that broken lines hang at.</summary>
internal abstract class Doc
{
    internal abstract int FlatWidth { get; }
}

internal sealed class DocText : Doc
{
    internal readonly string S;
    internal DocText(string s) => S = s;
    internal override int FlatWidth => S.Length;
}

/// <summary>A soft break. <see cref="Flat"/> is what it renders to when its
/// group is flat (" " or ""); broken it is a newline + indent.</summary>
internal sealed class DocLine : Doc
{
    internal readonly string Flat;
    internal DocLine(string flat) => Flat = flat;
    internal override int FlatWidth => Flat.Length;
}

// Composite nodes compute FlatWidth eagerly in their constructors: the Doc tree
// is built strictly bottom-up (`RuntimeFormatState.PopWrap`), so every child's
// FlatWidth is already a resolved field read by the time a parent is built. This
// keeps the tree immutable (no lazy cache) and the render pass allocation- and
// mutation-free.

internal sealed class DocCat : Doc
{
    internal readonly List<Doc> Kids;
    internal override int FlatWidth { get; }

    internal DocCat(List<Doc> kids)
    {
        Kids = kids;
        int w = 0;
        foreach (Doc k in kids)
        {
            w += k.FlatWidth;
        }
        FlatWidth = w;
    }
}

internal sealed class DocNest : Doc
{
    internal readonly int Indent;
    internal readonly Doc Inner;
    internal override int FlatWidth { get; }

    internal DocNest(int indent, Doc inner)
    {
        Indent = indent;
        Inner = inner;
        FlatWidth = inner.FlatWidth;
    }
}

internal sealed class DocGroup : Doc
{
    internal readonly Doc Inner;
    /// <summary>When true, the group is wrapped in parentheses (a DU application
    /// in argument position). The parens count toward the flat width.</summary>
    internal readonly bool Parens;
    internal override int FlatWidth { get; }

    internal DocGroup(Doc inner, bool parens)
    {
        Inner = inner;
        Parens = parens;
        FlatWidth = inner.FlatWidth + (parens ? 2 : 0);
    }
}

/// <summary>
/// The concrete <see cref="IFormatSink"/>: builds a <c>Doc</c> via a frame stack,
/// runs the reflection-free dispatcher for children, and lays the document out to
/// a string. Carries the visited-set (cycle detection) and depth counter.
/// </summary>
public sealed class RuntimeFormatState : IFormatSink
{
    // F#'s FormatOptions.Default limits (sformat.fs:432), adopted as-is.
    private const int DefaultPrintWidth = 80;
    private const int PrintDepth = 100;
    private const int PrintLength = 100;

    private readonly int _width; // 0 ⇒ never break (always flat); else the column budget.

    // F#'s PrintSize: a global "node" budget (sformat.fs `countNodes`). Each leaf
    // (primitive / string / char / bool / ToString fallback) spends one unit;
    // composites (records, DUs, tuples, collections) don't spend directly — their
    // children do. When it reaches 0, further values render as "..." (the `%.NA`
    // truncation). Default 10000 ⇒ effectively unbounded for normal values.
    private int _size;

    // The kind of an open layout scope. Root is the implicit outermost frame.
    // CaseCollect gathers a union case's payload child Docs so EndCase can pick the
    // nullary / single / tuple form from the observed count (popped by hand there).
    private enum FrameKind { Root, Group, Nest, CaseCollect }

    // Bookkeeping for one open BeginRecord / BeginCase scope (the semantic protocol).
    // A record tracks its field count (first field opens `{ `, the rest prefix `;` +
    // a soft break). A case tracks its payload count — the arity, fixed at EndCase —
    // and whether its lone payload was itself application-shaped (a single-payload
    // case parenthesises such a child, `Some (Some 3)`).
    private sealed class SemFrame
    {
        internal readonly bool IsCase;
        internal readonly string Name;
        internal int Count;
        internal bool ChildAppShaped;
        internal SemFrame(bool isCase, string name) { IsCase = isCase; Name = name; }
    }

    // Doc-building frames. Each frame collects children; closing a group/nest pops
    // and wraps. The root frame (index 0) holds the whole document.
    private sealed class Frame
    {
        internal readonly List<Doc> Kids = new();
        internal readonly FrameKind Kind;
        internal readonly int NestIndent;
        internal readonly bool Parens;
        internal Frame(FrameKind kind, int nestIndent, bool parens) { Kind = kind; NestIndent = nestIndent; Parens = parens; }
    }

    private readonly List<Frame> _frames = new();
    private readonly HashSet<object> _visited = new(ReferenceEqualityComparer.Instance);
    private int _depth;

    // The semantic-frame stack for the BeginRecord / BeginCase protocol (top = last).
    private readonly List<SemFrame> _semFrames = new();

    // The application-shapedness of the value Dispatch most recently completed (true
    // iff it was a union case with >= 1 payload). Child reads it to mark a case
    // payload for single-payload parenthesisation; every non-case terminal in
    // Dispatch resets it to false, so it always reflects the just-dispatched value.
    private bool _lastAppShaped;

    /// <summary>Create a layout state with the given column budget (0 ⇒ never
    /// break) and node budget (F# PrintSize; nodes past it render as "...").</summary>
    public RuntimeFormatState(int width, int printSize)
    {
        _width = width;
        _size = printSize;
        _frames.Add(new Frame(FrameKind.Root, 0, false));
    }

    private Frame Top => _frames[_frames.Count - 1];

    private void Add(Doc d) => Top.Kids.Add(d);

    /// <inheritdoc />
    public void Text(string s) => Add(new DocText(s));

    /// <inheritdoc />
    public void Line() => Add(new DocLine(" "));

    /// <inheritdoc />
    public void SoftBreak() => Add(new DocLine(""));

    /// <inheritdoc />
    public void BeginGroup() => _frames.Add(new Frame(FrameKind.Group, 0, false));

    /// <inheritdoc />
    public void EndGroup() => PopWrap(FrameKind.Group);

    /// <inheritdoc />
    public void BeginNest(int indent) => _frames.Add(new Frame(FrameKind.Nest, indent, false));

    /// <inheritdoc />
    public void EndNest() => PopWrap(FrameKind.Nest);

    // ---- the semantic protocol (BeginRecord/Field/BeginCase/Child/…) ----
    // The record/union layout policy that once lived in `StructuralFormatRecipe`
    // realised here, lowering the semantic frames into the same Doc builders. Two
    // invariants: the field label is added at `Field`, before `Child` recurses (so a
    // nested record's first `Field` cannot clobber it); and a single-payload case
    // parenthesises its child iff the child is application-shaped (`ChildAppShaped`).

    /// <inheritdoc />
    public void BeginRecord()
    {
        _semFrames.Add(new SemFrame(false, ""));
        BeginGroup();
    }

    /// <inheritdoc />
    public void Field(string name)
    {
        SemFrame rf = _semFrames[_semFrames.Count - 1];
        if (rf.Count == 0)
        {
            // First field opens `{ name = ` (outside the hang); the fields hang at +2
            // when the group breaks.
            Add(new DocText("{ " + name + " = "));
            BeginNest(2);
        }
        else
        {
            Add(new DocText(";"));
            Add(new DocLine(" "));
            Add(new DocText(name + " = "));
        }
        rf.Count++;
    }

    /// <inheritdoc />
    public void EndRecord()
    {
        SemFrame rf = _semFrames[_semFrames.Count - 1];
        _semFrames.RemoveAt(_semFrames.Count - 1);
        if (rf.Count == 0)
        {
            // A field-less record is a bare `{ }`.
            Add(new DocText("{ }"));
            PopWrap(FrameKind.Group);
        }
        else
        {
            PopWrap(FrameKind.Nest);
            Add(new DocText(" }"));
            PopWrap(FrameKind.Group);
        }
        _lastAppShaped = false;
    }

    /// <inheritdoc />
    public void BeginCase(string name)
    {
        _semFrames.Add(new SemFrame(true, name));
        _frames.Add(new Frame(FrameKind.CaseCollect, 0, false));
    }

    /// <inheritdoc />
    public void Child(object? value)
    {
        Dispatch(value);
        // In a record the label was emitted at Field; here we only record, for a
        // case, the payload count and (for the single-payload arm) the child's
        // application-shapedness. At the top level (the `Print` entry) there is no open
        // semantic frame, so this is a bare dispatch.
        if (_semFrames.Count > 0)
        {
            SemFrame sf = _semFrames[_semFrames.Count - 1];
            if (sf.IsCase)
            {
                sf.Count++;
                sf.ChildAppShaped = _lastAppShaped;
            }
        }
    }

    /// <inheritdoc />
    public void EndCase()
    {
        SemFrame cf = _semFrames[_semFrames.Count - 1];
        _semFrames.RemoveAt(_semFrames.Count - 1);

        // Harvest the case's payload child Docs (source order) from its CaseCollect
        // layout frame, popped by hand.
        Frame collect = Top;
        _frames.RemoveAt(_frames.Count - 1);
        List<Doc> kids = collect.Kids;

        Doc caseDoc;
        if (cf.Count == 0)
        {
            // Nullary case: a bare identifier (`None`).
            caseDoc = new DocText(cf.Name);
        }
        else if (cf.Count == 1)
        {
            Doc child = kids.Count == 1 ? kids[0] : new DocCat(kids);
            // The lone payload parenthesises iff it is itself a payload-bearing case.
            // The parent decides this (the child's Doc is already built), so we wrap
            // here rather than bake parens into the child.
            Doc payload = cf.ChildAppShaped ? new DocGroup(child, true) : child;
            caseDoc = new DocGroup(new DocCat(new List<Doc> { new DocText(cf.Name + " "), payload }), false);
        }
        else
        {
            // Multi-field payload: a parenthesised tuple whose parens already
            // disambiguate, so components stay in normal position.
            var tupleKids = new List<Doc>();
            bool first = true;
            foreach (Doc k in kids)
            {
                if (!first)
                {
                    tupleKids.Add(new DocText(","));
                    tupleKids.Add(new DocLine(" "));
                }
                first = false;
                tupleKids.Add(k);
            }
            Doc tuple = new DocGroup(
                new DocCat(new List<Doc> { new DocText("("), new DocNest(1, new DocCat(tupleKids)), new DocText(")") }),
                false);
            caseDoc = new DocGroup(new DocCat(new List<Doc> { new DocText(cf.Name + " "), tuple }), false);
        }

        Add(caseDoc);
        // A case is application-shaped iff it carries a payload; that mark is what an
        // enclosing single-payload case reads to parenthesise this one.
        _lastAppShaped = cf.Count >= 1;
    }

    private void PopWrap(FrameKind expected)
    {
        Frame f = Top;
        if (f.Kind != expected)
        {
            throw new InvalidOperationException(
                $"Vesper.RuntimeFormatState: mismatched layout scope (expected {expected}, got {f.Kind}).");
        }

        _frames.RemoveAt(_frames.Count - 1);
        Doc inner = f.Kids.Count == 1 ? f.Kids[0] : new DocCat(f.Kids);
        Doc wrapped = f.Kind switch
        {
            FrameKind.Group => new DocGroup(inner, false),
            FrameKind.Nest => new DocNest(f.NestIndent, inner),
            _ => inner,
        };
        Add(wrapped);
    }

    /// <summary>Resolution order (reflection-free). Cycle + depth guard first,
    /// then our own types, then BCL shapes, then a ToString fallback.</summary>
    private void Dispatch(object? value)
    {
        // Default: the value about to be dispatched is not application-shaped. Only a
        // payload-bearing union case sets it true at its EndCase, and each composite
        // resets it as its last act, so on return this reflects exactly `value`.
        _lastAppShaped = false;

        if (value is null)
        {
            Text("null");
            return;
        }

        if (_depth >= PrintDepth)
        {
            Text("...");
            return;
        }

        // Global node budget exhausted (`%.NA`): truncate. Checked before the
        // value is classified, mirroring F#'s `exceededPrintSize` guard at the
        // head of `objL` (sformat.fs:1024).
        if (_size <= 0)
        {
            Text("...");
            return;
        }

        Type t = value.GetType();
        bool tracked = false;
        if (!t.IsValueType)
        {
            if (!_visited.Add(value))
            {
                Text("...");
                return;
            }
            tracked = true;
        }

        _depth++;
        try
        {
            switch (value)
            {
                case IStructuralFormattable structural:
                    // The synthesised body drives the semantic protocol; a
                    // payload-bearing case sets _lastAppShaped at its EndCase, which the
                    // enclosing Child reads for single-payload parenthesisation.
                    structural.Format(this);
                    break;

                // Leaf cases spend one unit of the node budget (`countNodes 1` in
                // F#'s sformat.fs); composite cases (tuple / enumerable / our own
                // structural types) don't — their leaf children do.
                case string s:
                    _size--;
                    Text(QuoteString(s));
                    break;

                case char c:
                    _size--;
                    Text(QuoteChar(c));
                    break;

                case bool b:
                    _size--;
                    Text(b ? "true" : "false");
                    break;

                case ITuple tuple:
                    FormatTuple(tuple);
                    break;

                // Numbers / other ISpanFormattable primitives: invariant, with the
                // float `.0` fixup so the result is unambiguously a float in source.
                case ISpanFormattable spanFormattable:
                    _size--;
                    Text(FormatPrimitive(spanFormattable));
                    break;

                case IEnumerable enumerable:
                    FormatEnumerable(enumerable);
                    break;

                default:
                    _size--;
                    Text(value.ToString() ?? "null");
                    break;
            }
        }
        finally
        {
            _depth--;
            if (tracked)
            {
                _visited.Remove(value);
            }
        }
    }

    private void FormatTuple(ITuple tuple)
    {
        // `(a, b)` flat; broken hangs the components under the open paren (indent 1).
        BeginGroup();
        Text("(");
        BeginNest(1);
        for (int i = 0; i < tuple.Length; i++)
        {
            if (i > 0)
            {
                Text(",");
                Line();
            }
            Dispatch(tuple[i]);
        }
        EndNest();
        Text(")");
        EndGroup();
        // A tuple is never application-shaped, so a single-payload case wrapping it
        // must not add an extra pair of parens.
        _lastAppShaped = false;
    }

    private void FormatEnumerable(IEnumerable enumerable)
    {
        // `[1; 2; 3]` flat; broken puts the opening/closing brackets on their own
        // lines with the elements nested (indent 2), `;`-separated.
        BeginGroup();
        Text("[");
        BeginNest(2);
        SoftBreak();
        int i = 0;
        foreach (object? item in enumerable)
        {
            if (i > 0)
            {
                Text(";");
                Line();
            }
            // Truncate on either the per-collection length cap (PrintLength) or the
            // exhausted global node budget (`%.NA`/PrintSize). Breaking here keeps
            // the trailing `...` single, mirroring F#'s `boundedUnfoldL` stopShort
            // (sformat.fs) — without it the per-element `_size <= 0` guard in
            // Dispatch would emit one `...` per remaining element.
            if (i >= PrintLength || _size <= 0)
            {
                Text("...");
                break;
            }
            Dispatch(item);
            i++;
        }
        EndNest();
        SoftBreak();
        Text("]");
        EndGroup();
        // A list is never application-shaped (see FormatTuple).
        _lastAppShaped = false;
    }

    /// <summary>Lay the recorded document out to a string.</summary>
    public string Finish()
    {
        if (_frames.Count != 1 || Top.Kind != FrameKind.Root)
        {
            throw new InvalidOperationException("Vesper.RuntimeFormatState: unbalanced layout scopes at Finish.");
        }

        Doc root = Top.Kids.Count == 1 ? Top.Kids[0] : new DocCat(Top.Kids);
        var sb = new StringBuilder();
        // The whole document is an implicit group so the top level can break.
        Render(new DocGroup(root, false), indent: 0, broken: false, column: 0, sb);
        return sb.ToString();
    }

    /// <returns>the column after rendering <paramref name="doc"/>.</returns>
    private int Render(Doc doc, int indent, bool broken, int column, StringBuilder sb)
    {
        switch (doc)
        {
            case DocText dt:
                sb.Append(dt.S);
                return column + dt.S.Length;

            case DocLine dl:
                if (broken)
                {
                    sb.Append('\n');
                    sb.Append(' ', indent);
                    return indent;
                }
                sb.Append(dl.Flat);
                return column + dl.Flat.Length;

            case DocNest dn:
                return Render(dn.Inner, indent + dn.Indent, broken, column, sb);

            case DocCat dc:
            {
                int col = column;
                foreach (Doc k in dc.Kids)
                {
                    col = Render(k, indent, broken, col, sb);
                }
                return col;
            }

            case DocGroup dg:
            {
                int col = column;
                if (dg.Parens)
                {
                    sb.Append('(');
                    col++;
                }

                // All-or-nothing: the group is flat iff its entire flat rendering
                // fits the remaining budget from the current column. width 0 ⇒
                // an unbounded budget ⇒ always flat (the `%0A` "never break" mode).
                bool groupBroken = _width != 0 && col + dg.Inner.FlatWidth > _width;
                col = Render(dg.Inner, indent, groupBroken, col, sb);

                if (dg.Parens)
                {
                    sb.Append(')');
                    col++;
                }
                return col;
            }

            default:
                throw new InvalidOperationException("Vesper.RuntimeFormatState: unknown Doc node.");
        }
    }

    // ---- atom rendering (copy-pasteable source forms) ----

    private static string FormatPrimitive(ISpanFormattable value)
    {
        // Floats must read back as floats: a finite double/single whose shortest
        // round-trip has no '.', 'e'/'E' would paste back as an int — append ".0".
        // float32 additionally takes the `f` suffix (`3.0f`, `nanf`); double none.
        if (value is double d)
        {
            return FixFloat(d.ToString(null, CultureInfo.InvariantCulture), double.IsFinite(d), "");
        }
        if (value is float f)
        {
            return FixFloat(f.ToString(null, CultureInfo.InvariantCulture), float.IsFinite(f), "f");
        }

        // Every non-`int32` integral / decimal literal carries a type suffix so the
        // rendered atom reads back at its *source* type (`5L`, `5uy`, `1.5M`), not as
        // a bare `int`. The canonical spellings are the ones the XParsec.FSharp lexer
        // accepts — each suffix below maps 1:1 to a lexer token in
        // `Lexing.getIntTokenFromSpan` (`y`→SByte, `uy`→Byte, `s`→Int16, `us`→UInt16,
        // `u`→UInt32, `L`→Int64, `UL`→UInt64, `n`→NativeInt, `un`→UNativeInt,
        // `M`→Decimal), so the output re-lexes at the same type (round-trip).
        // The invariant string is the same `ISpanFormattable` rendering for all; only
        // the suffix differs. `int32` and any other `ISpanFormattable` (DateTime,
        // Guid, …) take no suffix.
        string s = value.ToString(null, CultureInfo.InvariantCulture);

        string suffix = value switch
        {
            sbyte => "y",
            byte => "uy",
            short => "s",
            ushort => "us",
            uint => "u",
            long => "L",
            ulong => "UL",
            IntPtr => "n",
            UIntPtr => "un",
            decimal => "M",
            _ => "",
        };

        return s + suffix;
    }

    private static string FixFloat(string s, bool finite, string suffix)
    {
        if (!finite)
        {
            // Non-finite floats have no numeric source form; use the F# spellings
            // (float32 keeps its suffix: `nanf` / `infinityf` / `-infinityf`).
            return s switch
            {
                "NaN" => "nan" + suffix,
                "Infinity" => "infinity" + suffix,
                "-Infinity" => "-infinity" + suffix,
                _ => s,
            };
        }

        foreach (char c in s)
        {
            if (c == '.' || c == 'e' || c == 'E')
            {
                return s + suffix;
            }
        }
        return s + ".0" + suffix;
    }

    private static string QuoteString(string s)
    {
        var sb = new StringBuilder(s.Length + 2);
        sb.Append('"');
        foreach (char c in s)
        {
            AppendEscaped(sb, c, '"');
        }
        sb.Append('"');
        return sb.ToString();
    }

    private static string QuoteChar(char c)
    {
        var sb = new StringBuilder(4);
        sb.Append('\'');
        AppendEscaped(sb, c, '\'');
        sb.Append('\'');
        return sb.ToString();
    }

    private static void AppendEscaped(StringBuilder sb, char c, char quote)
    {
        switch (c)
        {
            case '\\': sb.Append("\\\\"); break;
            case '\n': sb.Append("\\n"); break;
            case '\r': sb.Append("\\r"); break;
            case '\t': sb.Append("\\t"); break;
            case '\0': sb.Append("\\0"); break;
            default:
                if (c == quote)
                {
                    sb.Append('\\').Append(c);
                }
                else
                {
                    sb.Append(c);
                }
                break;
        }
    }
}

/// <summary>Entry points for <c>%A</c>. <see cref="Print"/> is the standalone
/// "render to string" used by tests and by the printf handler's structural hole.</summary>
public static class StructuralPrinter
{
    /// <summary>Render <paramref name="value"/> as copy-pasteable Vesper source.</summary>
    /// <param name="value">the value to format.</param>
    /// <param name="widthBudget">column budget before a group breaks; 0 ⇒ never
    /// break (the <c>%0A</c> mode).</param>
    /// <param name="sizeBudget">F# PrintSize: max leaf nodes before truncating with
    /// <c>...</c> (the <c>%.NA</c> mode). Defaults to F#'s 10000 (plain <c>%A</c>).</param>
    public static string Print(object? value, int widthBudget, int sizeBudget = 10000)
    {
        var state = new RuntimeFormatState(widthBudget, sizeBudget);
        state.Child(value);
        return state.Finish();
    }
}
