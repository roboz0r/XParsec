# Specification: `IStructuralFormattable` for Zero-Reflection `%A` Formatting

## 1. Motivation

In legacy F#, the `%A` formatter relies on heavy runtime reflection (`FSharpValue.GetRecordFields`, etc.) to traverse arbitrary structural types. This results in significant memory allocations, poor performance, and fundamental incompatibility with Native AOT compilation due to metadata trimming limits.

This specification defines a compiler-driven, interface-based approach to structural formatting. It guarantees:

1. **Zero Runtime Reflection** for language-defined types.
2. **Zero Boxing** for value types and primitives.
3. **100% Native AOT Compatibility**.
4. **Safe Cycle Detection** for mutable, self-referential structures.

---

## 2. Core Runtime Types

The formatting engine consists of an interface implemented by language types, and an opaque `ref struct` context passed during traversal.

### 2.1. `IStructuralFormattable`

All compiler-generated structural types (Records, Discriminated Unions, Anonymous Records) **must** implement this interface.

```csharp
namespace Language.Runtime.Formatting;

/// <summary>
/// Implemented by types that support reflection-free structural formatting.
/// </summary>
public interface IStructuralFormattable
{
    void Format(ref FormatContext context);
}
```

### 2.2. `FormatContext`

A `ref struct` that hides the underlying buffer (e.g., `StringBuilder` or `ArrayPool` buffer) and tracking state. It provides the strictly constrained API that compiler-generated IL is allowed to call.

```csharp
namespace Language.Runtime.Formatting;

/// <summary>
/// An opaque struct passed by reference to accumulate formatted output.
/// </summary>
public ref struct FormatContext
{
    // Opaque state (Reference type holding StringBuilder, Hashset, Indent levels)
    private readonly RuntimeFormatState _state;

    internal FormatContext(RuntimeFormatState state) => _state = state;

    // --- Literal Appending ---
    public void Append(string text) => _state.Append(text);
    public void Append(ReadOnlySpan<char> text) => _state.Append(text);
    public void AppendLine() => _state.AppendLine();

    // --- High-Performance Primitive Appending ---
    public void AppendPrimitive<T>(T value) where T : ISpanFormattable 
        => _state.AppendSpanFormattable(value);

    // --- Recursive Dispatch ---
    public void FormatChild<T>(T value) 
        => PrettyPrinter.FormatValue(value, ref this);

    // --- Layout Management ---
    public void PushIndent() => _state.PushIndent();
    public void PopIndent() => _state.PopIndent();
}
```

---

## 3. Compiler Code Generation Rules

Whenever the compiler emits a Record, Discriminated Union, or Anonymous Record, it must emit a `Format(ref FormatContext)` method.

### 3.1. Records

For a record, the compiler emits instructions to print the braces, fields, and semicolons.

**Source Code:**

```fsharp
type Point = { X: int; Y: string }
```

**Emitted IL Equivalent:**

```csharp
public void Format(ref FormatContext ctx)
{
    ctx.Append("{ X = ");
    ctx.AppendPrimitive(this.X); // Route to ISpanFormattable
    
    ctx.Append("; Y = ");
    ctx.FormatChild(this.Y);     // Route to String/Child formatter
    
    ctx.Append(" }");
}
```

*Note: If the compiler detects a primitive that implements `ISpanFormattable`, it MUST emit a call to `AppendPrimitive`. For all other types, it MUST emit `FormatChild`.*

### 3.2. Discriminated Unions (DUs)

For DUs, the compiler emits a switch over the internal union tags.

**Source Code:**

```fsharp
type Result<'T> = 
    | Ok of 'T
    | Error of string * int
```

**Emitted IL Equivalent:**

```csharp
public void Format(ref FormatContext ctx)
{
    switch (this.Tag)
    {
        case Tags.Ok:
            ctx.Append("Ok ");
            ctx.FormatChild(this.Item); // Generic 'T
            break;

        case Tags.Error:
            ctx.Append("Error (");
            ctx.FormatChild(this.ErrorItem1); // string
            ctx.Append(", ");
            ctx.AppendPrimitive(this.ErrorItem2); // int
            ctx.Append(")");
            break;
    }
}
```

---

## 4. The Runtime Dispatcher (`PrettyPrinter`)

Because the language does not control the BCL (Base Class Library), standard `.NET` types (`int[]`, `ValueTuple`, `List<T>`) do not implement `IStructuralFormattable`.

When compiled code calls `ctx.FormatChild<T>(value)`, it is routed to the runtime dispatcher.

### 4.1 Dispatch Resolution Order

The dispatcher must resolve the formatting strategy in the following exact order to guarantee performance and safety:

```csharp
internal static class PrettyPrinter
{
    public static void FormatValue<T>(T value, ref FormatContext ctx)
    {
        // 1. Null Check
        if (value is null) {
            ctx.Append("null");
            return;
        }

        // 2. Cycle Detection (Reference Types Only)
        if (!typeof(T).IsValueType && !ctx._state.TryVisit(value)) {
            ctx.Append("...");
            return;
        }

        // 3. Fast Path: Types we own (JIT devirtualizes this!)
        if (value is IStructuralFormattable structural) {
            structural.Format(ref ctx);
            return;
        }

        // 4. Primitive / String Fallback
        if (value is string s) {
            ctx.Append("\""); ctx.Append(s); ctx.Append("\"");
            return;
        }

        // 5. ValueTuples (Via ITuple)
        if (value is System.Runtime.CompilerServices.ITuple tuple) {
            FormatTuple(tuple, ref ctx);
            return;
        }

        // 6. Arrays and Collections
        if (value is System.Collections.IEnumerable enumerable) {
            FormatEnumerable(enumerable, ref ctx);
            return;
        }

        // 7. Ultimate Fallback
        ctx.Append(value.ToString());
    }
}
```

### 4.2 Formatting Tuples without Reflection

Tuples are handled via the BCL `ITuple` interface.

```csharp
private static void FormatTuple(ITuple tuple, ref FormatContext ctx)
{
    ctx.Append("(");
    for (int i = 0; i < tuple.Length; i++)
    {
        if (i > 0) ctx.Append(", ");
        FormatValue(tuple[i], ref ctx); // Recursively format
    }
    ctx.Append(")");
}
```

---

## 5. State Management & Cycle Detection

The hidden `RuntimeFormatState` maintains the structural integrity of the print operation.

### 5.1 Cycle Detection Requirements

1. The state must contain a `HashSet<object> Visited { get; }` initialized with `ReferenceEqualityComparer.Instance`.
2. Cycle detection **must not** box value types. The dispatcher must check `typeof(T).IsValueType` before attempting to add the object to the `Visited` set.
3. If an object is already in the `Visited` set, the dispatcher must output `...` and return immediately.

### 5.2 Depth Limits (Stack Overflow Prevention)

Even without cycles, an incredibly deep tree can cause a `StackOverflowException`.
The state must maintain a `Depth` counter. If `Depth > 100`, it should append `...` and abort deeper traversal.

---

## 6. Entry Points

When a user invokes `%A`, it acts as the entry point to instantiate the state and begin formatting.

```csharp
// The implementation of `sprintf "%A" myData`
public static string PrintA<T>(T value)
{
    var state = new RuntimeFormatState();
    var ctx = new FormatContext(state);
    
    ctx.FormatChild(value);
    
    return state.ToString(); // Returns the accumulated buffer
}
```

---

## 7. AOT & Trimming Guarantees

By adhering to this specification:

* **No `[DynamicallyAccessedMembers]` attributes are required.**
* The .NET linker statically observes `FormatChild(this.Friend)` and guarantees that `Friend.Format()` is preserved during compilation.
* Code that doesn't use `%A` printing will have the `.Format()` methods successfully trimmed away by the .NET ILCompiler if they implement the interface explicitly, resulting in minimal binary sizes.
