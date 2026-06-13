using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Text;

namespace Vesper;

// Step 1 of vesper-printf-percentA-plan: a C# prototype of the `%A` structural
// formatter. The interfaces (`IStructuralFormattable` / `IFormatSink`) live here
// for now; at integration they migrate to self-hosted `Vesper.Core` — the
// synthetic-csproj test topology (self-host `Vesper.Core.dll`, then compile this
// C# against its path) lets the engine implement a Core-owned interface, so a
// synthesized record/DU implements `Vesper.IStructuralFormattable` without
// leaking a `Vesper.Printf` dependency into every record-bearing program.
//
// Target (D-A): `%A v` produces copy-pasteable Vesper source. Layout (D-B):
// a group-based pretty-printer — each composite is a group rendered ALL-flat (if
// its flat form fits the width budget) or ALL-broken (every soft break in it
// becomes a newline). No node is ever half-broken, so there is no stair-shape
// (the failure mode of F#'s greedy `squashToAux`, sformat.fs:666).

/// <summary>
/// Implemented by every compiler-synthesized record / DU / anonymous record. The
/// synthesized body declares the type's structure through <see cref="IFormatSink"/>.
/// </summary>
public interface IStructuralFormattable
{
    /// <summary>Declare this value's structure into <paramref name="sink"/>.</summary>
    void Format(IFormatSink sink);
}

/// <summary>
/// The declarative layout surface the synthesized <c>Format</c> calls. Tokens are
/// recorded into a <c>Doc</c> tree, then laid out group-by-group. <see cref="FormatChild"/>
/// takes <see cref="object"/> (value children box once — fine on the <c>%A</c>
/// heavy path; F#'s reflection-based <c>%A</c> boxes everything anyway).
/// </summary>
public interface IFormatSink
{
    /// <summary>A literal run that never breaks (labels, punctuation, brackets).</summary>
    void Text(string s);

    /// <summary>A soft break: a single space when its group is flat, a newline +
    /// current indent when broken. Use between items that read with a space.</summary>
    void Line();

    /// <summary>A soft break with no flat alternative: nothing when flat, a
    /// newline + current indent when broken. Use just inside an opening bracket
    /// and just before a closing one (so the closer dedents cleanly).</summary>
    void SoftBreak();

    /// <summary>Open a group: its soft breaks all flatten or all break together.</summary>
    void BeginGroup();

    /// <summary>Close the current group.</summary>
    void EndGroup();

    /// <summary>Open an indentation scope: soft breaks inside indent by
    /// <paramref name="indent"/> extra columns.</summary>
    void BeginNest(int indent);

    /// <summary>Close the current indentation scope.</summary>
    void EndNest();

    /// <summary>Open a DU application (<c>Case payload</c>): parenthesized iff this
    /// value sits in argument position (see <see cref="FormatArg"/>), so
    /// <c>Some (Some 3)</c> round-trips. A negative literal is a single atom token,
    /// not an application, so <c>Some -3</c> is correct without parens.</summary>
    void BeginApplication();

    /// <summary>Close the current DU application.</summary>
    void EndApplication();

    /// <summary>Recurse into a child in normal position (record field, list
    /// element, tuple component).</summary>
    void FormatChild(object? value);

    /// <summary>Recurse into a child in DU-argument position: if the child renders
    /// as an application it is parenthesized.</summary>
    void FormatArg(object? value);
}

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

    // Doc-building frames. Each frame collects children; closing a group/nest pops
    // and wraps. The root frame (index 0) holds the whole document.
    private sealed class Frame
    {
        internal readonly List<Doc> Kids = new();
        internal readonly char Kind; // 'r' root, 'g' group, 'n' nest, 'a' application
        internal readonly int NestIndent;
        internal readonly bool Parens;
        internal Frame(char kind, int nestIndent, bool parens) { Kind = kind; NestIndent = nestIndent; Parens = parens; }
    }

    private readonly List<Frame> _frames = new();
    private readonly HashSet<object> _visited = new(ReferenceEqualityComparer.Instance);
    private int _depth;
    // Set by FormatArg for the immediately-dispatched value; consumed by the first
    // BeginApplication it produces (so only a top-level application parenthesizes).
    private bool _argPending;

    /// <summary>Create a layout state with the given column budget (0 ⇒ never break).</summary>
    public RuntimeFormatState(int width)
    {
        _width = width;
        _frames.Add(new Frame('r', 0, false));
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
    public void BeginGroup() => _frames.Add(new Frame('g', 0, false));

    /// <inheritdoc />
    public void EndGroup() => PopWrap('g');

    /// <inheritdoc />
    public void BeginNest(int indent) => _frames.Add(new Frame('n', indent, false));

    /// <inheritdoc />
    public void EndNest() => PopWrap('n');

    /// <inheritdoc />
    public void BeginApplication()
    {
        bool parens = _argPending;
        _argPending = false;
        _frames.Add(new Frame('a', 0, parens));
    }

    /// <inheritdoc />
    public void EndApplication() => PopWrap('a');

    private void PopWrap(char expected)
    {
        Frame f = Top;
        if (f.Kind != expected)
        {
            throw new InvalidOperationException(
                $"Vesper.RuntimeFormatState: mismatched layout scope (expected '{expected}', got '{f.Kind}').");
        }

        _frames.RemoveAt(_frames.Count - 1);
        Doc inner = f.Kids.Count == 1 ? f.Kids[0] : new DocCat(f.Kids);
        Doc wrapped = f.Kind switch
        {
            'g' => new DocGroup(inner, false),
            'a' => new DocGroup(inner, f.Parens),
            'n' => new DocNest(f.NestIndent, inner),
            _ => inner,
        };
        Add(wrapped);
    }

    /// <inheritdoc />
    public void FormatChild(object? value)
    {
        _argPending = false;
        Dispatch(value);
    }

    /// <inheritdoc />
    public void FormatArg(object? value)
    {
        _argPending = true;
        // finally so a throwing child (e.g. a user ToString in the fallback arm)
        // can't strand _argPending=true onto an unrelated later value — matching
        // Dispatch's own depth/visited cleanup.
        try
        {
            Dispatch(value);
        }
        finally
        {
            _argPending = false;
        }
    }

    /// <summary>Resolution order (reflection-free; vesper-printf-percentA-plan
    /// "Dispatcher"). Cycle + depth guard first, then our own types, then BCL
    /// shapes, then a ToString fallback.</summary>
    private void Dispatch(object? value)
    {
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
                    // Propagate arg position so the structural value's own
                    // BeginApplication can parenthesize (only DUs open one).
                    structural.Format(this);
                    break;

                case string s:
                    Text(QuoteString(s));
                    break;

                case char c:
                    Text(QuoteChar(c));
                    break;

                case bool b:
                    Text(b ? "true" : "false");
                    break;

                case ITuple tuple:
                    FormatTuple(tuple);
                    break;

                // Numbers / other ISpanFormattable primitives: invariant, with the
                // float `.0` fixup so the result is unambiguously a float in source.
                case ISpanFormattable spanFormattable:
                    Text(FormatPrimitive(spanFormattable));
                    break;

                case IEnumerable enumerable:
                    FormatEnumerable(enumerable);
                    break;

                default:
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
            FormatChild(tuple[i]);
        }
        EndNest();
        Text(")");
        EndGroup();
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
            if (i >= PrintLength)
            {
                Text("...");
                break;
            }
            FormatChild(item);
            i++;
        }
        EndNest();
        SoftBreak();
        Text("]");
        EndGroup();
    }

    /// <summary>Lay the recorded document out to a string.</summary>
    public string Finish()
    {
        if (_frames.Count != 1 || Top.Kind != 'r')
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
        string s = value.ToString(null, CultureInfo.InvariantCulture);

        // Floats must read back as floats: a finite double/single whose shortest
        // round-trip has no '.', 'e'/'E' would paste back as an int — append ".0".
        if (value is double d)
        {
            return FixFloat(s, double.IsFinite(d));
        }
        if (value is float f)
        {
            return FixFloat(s, float.IsFinite(f));
        }
        return s;
    }

    private static string FixFloat(string s, bool finite)
    {
        if (!finite)
        {
            // Non-finite floats have no source form; use the F# spellings.
            return s switch
            {
                "NaN" => "nan",
                "Infinity" => "infinity",
                "-Infinity" => "-infinity",
                _ => s,
            };
        }

        foreach (char c in s)
        {
            if (c == '.' || c == 'e' || c == 'E')
            {
                return s;
            }
        }
        return s + ".0";
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
    public static string Print(object? value, int widthBudget)
    {
        var state = new RuntimeFormatState(widthBudget);
        state.FormatChild(value);
        return state.Finish();
    }
}
