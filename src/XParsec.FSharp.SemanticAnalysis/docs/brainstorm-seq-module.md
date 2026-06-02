# Specification: High-Performance, Zero-Allocation `Seq` Module

**Target Runtime:** .NET 9+
**Objective:** Provide a lazy, strictly non-allocating sequence processing module that relies on struct-chaining, duck-typed enumeration, and JIT devirtualization to achieve C++-style loop fusion (deforestation) without hardcoded AST rewrite rules.

## 1. Motivation

In standard F#, `seq<'T>` is an alias for `System.Collections.Generic.IEnumerable<'T>`. Because interfaces are reference types in .NET, any function returning `seq<'T>` inherently forces the underlying state machine onto the heap.
Pipelining operations (`xs |> Seq.map f |> Seq.filter g`) allocates an object per step.

This specification redesigns the `Seq` module to emit deeply nested **concrete generic structs** that resolve at compile-time, allowing the RyuJIT compiler to inline the entire pipeline into a single, stack-based `while` loop.

## 2. Core Architectural Principles

1. **No Interface Return Types:** Functions in the `Seq` module must return compiler-generated concrete struct types, never `IEnumerable<'T>`.
2. **Duck-Typed Enumeration:** Sequences must expose a public `GetEnumerator()` method that returns a concrete struct enumerator. The compiler will lower `for..in` loops directly to these methods, ignoring `IEnumerable`.
3. **Explicit Interface Fallback:** All sequence structs must implement `IEnumerable<'T>` explicitly. This serves as an escape hatch; if the sequence is passed to a standard C# API, it will transparently box itself.
4. **Closure Integration:** Sequence combinators must accept closures using the `in TClosure where TClosure : Fun<TIn, TOut>` pattern established in the closure specification.

## 3. Core Types & Interfaces

To allow the compiler to chain operations without losing type information, we define a marker interface used *only* as a generic constraint (never as a variable type).

```csharp
// Emitted into the core library
// TEnumerator is tracked so the compiler knows exactly what to call.
public interface IStructSeq<T, out TEnumerator> : IEnumerable<T>
    where TEnumerator : struct, IEnumerator<T>
{
    new TEnumerator GetEnumerator(); 
}
```

## 4. Sequence Types (The Data Structures)

For every operation in the `Seq` module, the compiler library defines a corresponding struct pair: a `Sequence` and an `Enumerator`.

### 4.1 Example: The Map Sequence

When the user uses `Seq.map`, the compiler instantiates these structs:

```csharp
// 1. The Sequence Struct
public readonly struct MapSeq<TSourceSeq, TEnum, TIn, TOut, TFunc> 
    : IStructSeq<TOut, MapSeq<...>.Enumerator>
    where TSourceSeq : IStructSeq<TIn, TEnum>
    where TEnum : struct, IEnumerator<TIn>
    where TFunc : allows ref struct, Fun<TIn, TOut>
{
    internal readonly TSourceSeq _source;
    internal readonly TFunc _f;

    public MapSeq(in TSourceSeq source, in TFunc f)
    {
        _source = source;
        _f = f;
    }

    // Duck-typed entry point
    public Enumerator GetEnumerator() => new Enumerator(_source.GetEnumerator(), _f);

    // Fallback C# Interop
    IEnumerator<TOut> IEnumerable<TOut>.GetEnumerator() => GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

    // 2. The Enumerator Struct
    public struct Enumerator : IEnumerator<TOut>
    {
        private TEnum _sourceEnum;
        private readonly TFunc _f;

        internal Enumerator(TEnum sourceEnum, in TFunc f)
        {
            _sourceEnum = sourceEnum;
            _f = f;
        }

        public bool MoveNext() => _sourceEnum.MoveNext();
        // Devirtualized, inline function call!
        public TOut Current => _f.Invoke(_sourceEnum.Current); 
        
        public void Dispose() => _sourceEnum.Dispose();
        object IEnumerator.Current => Current;
        public void Reset() => throw new NotSupportedException();
    }
}
```

## 5. The `Seq` Module Combinators (Public API)

The public-facing `Seq` module consists of highly generic inline functions that construct these structs.

*Note: While the generic signatures look terrifying in C#, they are fully inferred by the language's type checker and remain completely invisible to the user.*

```csharp
public static class Seq
{
    // seq |> Seq.map f
    public static MapSeq<TSourceSeq, TEnum, TIn, TOut, TFunc> Map<TSourceSeq, TEnum, TIn, TOut, TFunc>(
        in TFunc f, 
        in TSourceSeq source)
        // Constraints elided for brevity
    {
        return new MapSeq<...>(source, f); // Zero allocations!
    }

    // Terminal Operation (forces evaluation)
    // seq |> Seq.fold f seed
    public static TState Fold<TSourceSeq, TEnum, TState, TItem, TFunc>(
        in TFunc f, 
        TState seed, 
        in TSourceSeq source)
    {
        TState state = seed;
        // Duck-typed loop against the concrete struct
        foreach (var item in source) 
        {
            state = f.Invoke(state, item);
        }
        return state;
    }
}
```

## 6. Pipeline Resolution (Compiler Behavior)

**User Code:**

```fsharp
let data = ... // some struct array or list wrapper
let result = 
    data 
    |> Seq.filter (fun x -> x > 0)
    |> Seq.map (fun x -> x * 2)
    |> Seq.fold (+) 0
```

**What the Compiler Generates:**

1. Generates `Closure_Filter` and `Closure_Map` structs.
2. Infers the types and generates a chained type signature: `MapSeq<FilterSeq<ArraySeq<int>,...>,...>`
3. Lowers the `fold` operation into a `foreach` loop over this massive struct.

**What the JIT Compiler Does:**

1. The JIT sees a `foreach` loop over `MapSeq`.
2. It expands `MapSeq.MoveNext()`, which directly calls `FilterSeq.MoveNext()`.
3. It devirtualizes the closure calls inside `Current`.
4. **Final Machine Code:** The JIT emits a single, raw CPU loop over the original `data` array. Branch prediction handles the filter. No intermediate memory is allocated, and the closures are erased into inline assembly.

## 7. Interoperability & Escape Hatches

### 7.1 Entering the Pipeline

To start a zero-allocation pipeline from standard .NET collections, the module must provide bridging wrappers.

* `List |> Seq.ofList` -> Wraps the list in a `ListSeq` struct.
* `Array |> Seq.ofArray` -> Wraps the array in an `ArraySeq` struct.

### 7.2 Exiting the Pipeline (Safe Boxing)

Because every sequence struct implements `IEnumerable<T>`, the pipeline can be passed to standard .NET APIs.

**User Code:**

```fsharp
let mySeq = data |> Seq.map (fun x -> x * 2)
System.IO.File.WriteAllLines("out.txt", mySeq) // BCL method expects IEnumerable<string>
```

**Compiler Action:**
The compiler detects the type mismatch (struct vs interface) and emits a `box` instruction. The struct state machine is allocated onto the heap, perfectly satisfying the .NET BCL requirement without requiring any explicit casting syntax from the user.

## 8. Summary of Benefits over F# `seq`

1. **True Deforestation:** Combinators act as zero-cost abstractions.
2. **Predictable Performance:** Users know that `List.map` allocates immediately, while `Seq.map` builds a zero-allocation struct tree evaluated only upon termination.
3. **No AST Magic:** The compiler requires no special heuristics to rewrite `map >> filter`. It simply relies on the standard .NET JIT inlining mechanisms.
