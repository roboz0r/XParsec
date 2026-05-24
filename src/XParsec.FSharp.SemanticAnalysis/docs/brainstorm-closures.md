# Zero-Cost Stack-Bound Closures

**Target Runtime:** .NET 9+ (Requires `allows ref struct` and ref struct interface implementation).

## 1. Motivation

In standard .NET functional languages, closures are implemented as reference types (`System.Delegate` / `Func<T, TResult>`), resulting in heap allocations. In heavy functional pipelines, this causes severe Garbage Collection (GC) pressure.

This specification defines a compiler architecture where closures are emitted as `readonly struct` or `readonly ref struct` types implementing a unified interface. When combined with JIT monomorphization, this achieves zero-allocation, heavily inlined, C++-speed functional composition.

## 2. Core Interface Definitions

The compiler relies on a fundamental interface representing a function.

```csharp
// Emitted into the core library of the language
public interface IFunction<TIn, TOut>
{
    TOut Invoke(TIn arg);
}
```

*Note: Depending on arity, the compiler will generate `IFunction<T1, T2, TOut>`, etc.*

## 3. Closure Emission Strategy (Anonymous Functions)

When the compiler encounters an anonymous function (e.g., `fun x -> x + y`), it performs **Capture Analysis** to determine the emitted representation.

### 3.1. Pure Closures (No Captures)

If the closure captures zero variables from the enclosing scope, it is pure.

* **Emission:** `readonly struct`
* **Optimization:** The compiler emits a single `static readonly` boxed instance globally to ensure zero allocations if the function is ever forced to the heap.

### 3.2. Standard Closures (Captures Heap-Safe Types)

If the closure captures only standard types (`int`, `string`, classes, normal structs).

* **Emission:** `readonly struct`
* **Fields:** Captured variables are emitted as `private readonly` fields.

### 3.3. Stack-Bound Closures (Captures `ref struct`)

If the closure captures a `ref struct` (e.g., `ReadOnlySpan<T>`).

* **Emission:** `readonly ref struct`
* **Constraint:** This closure is permanently bound to the stack and cannot be boxed.

```csharp
// Example IL/C# emission for: let f = fun x -> span[x]
internal readonly ref struct Closure_Line42 : IFunction<int, int>
{
    private readonly ReadOnlySpan<int> _span; // Allowed in .NET 9+
    public Closure_Line42(ReadOnlySpan<int> span) => _span = span;
    public int Invoke(int arg) => _span[arg];
}
```

## 4. Higher-Order Functions & Callee Signatures

To preserve value semantics and trigger JIT devirtualization, the compiler translates standard functional type signatures (`A -> B`) into constrained generics.

**F# Source:**

```fsharp
let map (f: A -> B) (list: List<A>) : List<B> = ...
```

**Compiler Emission Rules for Arguments:**
When `A -> B` appears as a parameter, the compiler desugars it into:

1. A generic type parameter `TClosure`.
2. An `in` passing modifier (to prevent copying large struct payloads).
3. An `allows ref struct` constraint (to allow `Span`-capturing closures).
4. An `IFunction<A, B>` constraint.

**Emitted C#/IL:**

```csharp
public static List<B> Map<A, B, TClosure>(in TClosure f, List<A> list) 
    where TClosure : allows ref struct, IFunction<A, B>
{
    // Inner loop:
    B result = f.Invoke(item); // Devirtualized and Inlined by RyuJIT
}
```

## 5. Return Types and the Caller Stack

When a function returns a closure, the compiler must prevent premature boxing.

**F# Source:**

```fsharp
let makeAdder x = fun y -> x + y
```

### 5.1 Concrete Opaque Returns

The compiler must infer the exact generated struct type and use it as the return type. The .NET CLR will write the struct directly onto the caller's stack frame.

```csharp
// The compiler uses the generated type name, NOT the interface
public static Closure_MakeAdder MakeAdder(int x) => new Closure_MakeAdder(x);
```

### 5.2 The Branching / Union Fallback

If a function branches and returns two structurally different closures, the compiler must resolve the type union:

* **If both are standard structs:** The compiler emits a return type of `IFunction<A, B>`. This triggers a `box` instruction, moving the closure to the heap.
* **If either is a `ref struct`:** Boxing is illegal. The compiler must either throw a compilation error, or (as an advanced optimization) synthesize a `UnionStruct` containing the fields of both, plus a boolean tag to dispatch the `Invoke` call.

## 6. Type Erasure, Boxing, and Escape Analysis

Because `A -> B` is a syntactic alias, its runtime representation changes based on where it is used. The compiler relies on the CLR for escape analysis where possible.

### 6.1 Safe Escape (Boxing)

If a user assigns a standard closure to a persistent data structure (e.g., `type Record = { func: A -> B }`), the compiler resolves `A -> B` to the boxed interface `IFunction<A,B>`.

* **Action:** The compiler emits a `box` instruction. The performance drops to standard delegate levels, but the program remains valid.

### 6.2 Unsafe Escape (Compile Error)

If a user attempts to assign a *Stack-Bound Closure* (Section 3.3) to a heap-allocated location, the .NET CLR rules take over.

* **Action:** The C#/IL compiler will detect a `ref struct` being boxed or used as a generic type argument that lacks `allows ref struct`. It will emit a fatal compilation error.
* **Language UX:** The compiler should surface this to the user as: *"Error: Closure captures stack-allocated state (Span) and cannot escape the current scope."*

## 7. Interoperability with standard .NET (Delegates)

To maintain compatibility with the broader .NET ecosystem (e.g., LINQ `IEnumerable.Select`), the compiler provides automatic bridging.

When an `A -> B` closure is passed to an external .NET method expecting `System.Func<A, B>`:

1. The compiler generates an anonymous method that wraps the struct.
2. An allocation occurs (instantiating the `Func`).

```csharp
// F# Source: list.Select(fun x -> x + 1)
Closure_Inline closure = new Closure_Inline();
// Compiler auto-bridges to standard BCL delegate:
list.Select(new Func<int, int>(closure.Invoke)); 
```

## 8. Summary of Performance Guarantees

If implemented according to this specification:

1. **Local Pipelines:** `list |> map f |> filter g` will execute with 0 bytes of memory allocated, compiled down to native machine loops by the JIT.
2. **Span safety:** Users can safely parse high-performance byte buffers (`Span<byte>`) using higher-order functional combinators.
3. **Graceful Degradation:** Storing functions in lists or records will automatically fallback to standard heap-allocation without requiring syntax changes from the user.
