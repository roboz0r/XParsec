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

---

## 9. Status & sequencing (where this sits in the roadmap)

> **Status:** Forward-looking design — the *destination*, not the next step. The
> currently-shipping sequence type is the interface-based `src/Vesper.Seq/seq.clr.fs`
> (`seq<'T>` = `IEnumerable<'T>`, walked through the `for … in` `Interface` path).
> This zero-allocation struct redesign sits **on top of** several larger unbuilt
> capabilities; completing it is not a near-term next commit.

### 9.1 What this design depends on (bottom-up)

This spec is the *consumer* that would naturally force the remaining `for … in`
gaps (`get-enumerator-gaps.md`) — but it sits above them, so driving those gaps "by
completing the Seq module" inverts the dependency order. The real prerequisites, in
build order:

1. **Unboxed struct method dispatch** — the keystone, and bigger than all the
   `for … in` gaps combined. The entire struct-chaining mechanism is calls on struct
   *fields* by address (`_source.GetEnumerator()`, `_f.Invoke(_sourceEnum.Current)`).
   `[<Struct>]` codegen today is the **boxed** path; unboxed dispatch is deferred. Nothing in §4–6 works without it.
2. **Struct / ref-struct closures** — §2.4 / §4's `in TFunc where TFunc : Fun<…>`
   needs the *struct*-closure shape. Current closures are **reference-type**
   `Vesper.Fun<_,_>` subclasses (`function-representation-plan.md`); the struct and
   `allows ref struct` shapes are explicitly "further out" (`brainstorm-closures.md`).
3. **Generic struct interface impls** — every sequence struct implements
   `IEnumerable<'T>` explicitly as the §7.2 escape hatch. This is the same
   "Free type parameter 'T not declared" impl gap tracked as `for … in` Gap 3.
4. **Byref-like type modelling** — `allows ref struct` (§3, ref-struct closures, the
   ref-struct enumerator `Dispose` of `for … in` Gap 2) all need a byref-like
   predicate that `SemType` does not have today.

### 9.2 Mapping to the `for … in` remaining-work gaps

Two of the three open `for … in` gaps are exactly what this design exercises, which
is why Seq is the right *eventual* forcing function — but each is gated as above:

| `get-enumerator-gaps.md` item | exercised by | gated behind |
|---|---|---|
| **value-type source** (a struct `MapSeq` passed to `Seq.fold` / `foreach`) | §5 `Fold`, §6 | unboxed struct dispatch (#1) — it's the same "address a struct receiver" capability one level up, so it falls out of, or right after, that work |
| **generic struct interface impls** | §7.2 escape hatch | the impl/upcast gap (#3) |
| **ref-struct `Dispose()`** (non-`IDisposable`) | ref-struct enumerators | byref-like modelling (#4), co-blocked with `allows ref struct` |

### 9.3 Recommended path

- **Now:** don't pre-build the `for … in` gaps in isolation (they're consumer-gated;
  value-type source is explicitly deferred for lack of a site). Let the interface-based
  `Vesper.Seq` and `Vesper.Set` pull `for … in` features as they hit real walls — the
  landed `Interface` + `Pattern` paths already cover them.
- **When chasing the zero-alloc design:** go bottom-up. **Unboxed struct method
  dispatch first** (#1). Then drive value-type source + generic struct interface impls
  with a **thin vertical slice** — `ArraySeq`/`ListSeq` → `map` → `fold`, struct
  enumerators, *reference-type closures for now* (don't wait on struct closures) —
  which gives a real running test forcing those gaps. Defer ref-struct `Dispose` and
  `allows ref struct` together until byref-like types are modelled.
- The full §4–6 generic struct tree with struct closures and JIT devirtualization is
  the north star, layered on once #1–#4 are in place.
