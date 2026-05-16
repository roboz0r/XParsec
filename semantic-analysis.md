Here is a comprehensive draft specification document. You can use this as the architectural blueprint for your compiler’s frontend. It synthesizes all the concepts we discussed into a structured, engineering-focused format.

---

# Semantic Analysis Specification: "F#2" Multi-Target Compiler

## 1. Abstract & Scope
This document outlines the Semantic Analysis phase for the F#2 compiler. This phase acts as the bridge between the Parser (Raw AST) and the Backend Emitters. 

Unlike traditional compilers, F#2 utilizes a **Multi-Target Pluggable Environment** and a **3-Axis Constraint Solver**. The output of this phase is a **Target-Decorated Typed-AST (HIR - High-level IR)**, where all names are resolved, types are inferred, memory regions are calculated, and target-specific semantics (e.g., Rust lifetimes, .NET structs) are locked in.

**Inputs:** Raw AST (Indentation-aware, desugared).
**Outputs:** Fully Typed, Region-Annotated AST.
**Errors Handled:** Type mismatches, missing cases in pattern matching, illegal memory escapes, unresolvable symbols.

---

## 2. Core Data Structures: The `TypeVar` Graph
To support F#'s ad-hoc polymorphism (SRTP, IWSAMs) and multi-axis inference, the compiler uses **Algorithm J**. Type metadata is stored as a highly mutable Union-Find (Disjoint-Set) graph.

```fsharp
/// Represents the 3-axis state of any expression in the AST
type TypeVar = {
    // Axis 1: Standard Type Unification (Points to concrete type or another TypeVar)
    mutable Link: Type option 
    
    // Axis 2: Units of Measure (Abelian Group Algebraic terms)
    mutable Units: MeasureTerm list 
    
    // Axis 3: Allocation / Escape Region
    mutable RegionId: RegionId 
    
    // F#-Specific Ad-Hoc Polymorphism Deferred Bounds
    mutable InterfaceBounds: TargetInterface list // For .NET IWSAMs / Rust Traits
    mutable SRTPBounds: MemberSignature list      // For F# 'inline' constraints
}

/// Represents the Allocation Region Reachability Graph
type RegionGraph = {
    Nodes: Map<RegionId, ScopeLevel>
    Edges: List<RegionConstraint> // Represents R_A >= R_B (A outlives B)
}
```

---

## 3. Phase Pipeline Overview
Semantic Analysis is executed in a strictly ordered pipeline per compilation unit:

1. **Target Environment Initialization**
2. **Top-to-Bottom Pass: Name Resolution & Constraint Gathering**
3. **Unification (Solving Axes 1 & 2: Types & Units)**
4. **Reachability Analysis (Solving Axis 3: Regions)**
5. **Context-Sensitive Validation**
6. **Target-Specific Specialization**

---

## 4. Detailed Phase Specifications

### Phase 4.1: Target Environment Initialization
Before traversing the AST, the compiler must populate the global environment. F#2 uses an `IExternalSymbolProvider` interface.
*   **The Internal Resolver:** Initializes the file based on the `.fsproj` top-to-bottom file order.
*   **The External Resolver (Target-Dependent):**
    *   *If Target = .NET:* Loads `FSharp.Core.dll` and user `.dll` references. Maps CLI metadata to F#2 `TypeVar` nodes.
    *   *If Target = JS:* Parses `lib.dom.d.ts` and `package.json`. Maps TypeScript structural interfaces to F#2 `TypeVar` nodes.
    *   *If Target = Rust:* Parses `Cargo.toml` dependencies. Maps Rust traits to F#2 `InterfaceBounds`.

### Phase 4.2: Name Resolution & Constraint Gathering
The compiler performs a single top-to-bottom walk of the Raw AST.
*   **Scoping & Shadowing:** Implemented as an immutable stack of HashMaps (Environments). Pushing a new scope allows identical variable names to legally shadow outer scopes.
*   **Node Annotation:** Every AST Node (variables, functions, literals) is assigned a fresh `TypeVar`.
*   **Constraint Generation:**
    *   `let x = 5` generates: `Unify(x.Type, Int)`
    *   `x + y` generates: `DeferBound(x, HasMember(+)); Unify(x.Type, y.Type)`
    *   `return x` generates: `AddRegionEdge(ReturnRegion >= x.Region)`

### Phase 4.3: Unification (Types & Units)
Executes Algorithm J over the gathered constraints.
*   **Type Unification:** Resolves all equalities. Triggers "On-Unified" callbacks to resolve deferred Operator Overloads and SRTPs once concrete types are discovered.
*   **Unit Unification:** Sorts and cancels unit exponents (e.g., evaluating `<kg * m / s>` vs `<m * kg / s>`).
*   **Generalization:** Any `TypeVar` remaining completely unconstrained at a `let` boundary is generalized into a polymorphic generic `'a`.

### Phase 4.4: Reachability Analysis (Allocation & Escapes)
Instead of equality, this step solves for Partial Ordering (Inequality).
1.  Compile the `RegionGraph` from the AST dataflow.
2.  Compute the **Least Upper Bound** for every allocation's lifespan.
3.  Assign an `EscapeState` to every `TypeVar`:
    *   `LocalStack` (Does not escape the function).
    *   `CallerStack` (Escapes one level up).
    *   `HeapShared` (Captured by long-lived data or async threads).

### Phase 4.5: Context-Sensitive Validation
With types and dataflow fully resolved, F#-specific rules are enforced:
*   **Pattern Match Exhaustiveness:** Construct a decision tree for every `match` statement. Yield compiler errors if any union cases or numeric ranges are unhandled.
*   **The Value Restriction:** Yield an error if a generalized type `'a` is applied to a mutable reference or a non-simple value (preventing generic type-unsafety).
*   **Immutability Enforcement:** Yield an error if the `<-` operator is used on a `TypeVar` not marked with the `mutable` metadata flag.

### Phase 4.6: Target-Specific Specialization
The finalized Typed-AST is handed back to the active Target Plugin for final lowering decisions based on the 3-Axis data.

*   **.NET Target Actions:**
    *   Convert `LocalStack` closures to `ref struct` / function pointers.
    *   Map IWSAM bounds directly to .NET 7 interfaces.
*   **Rust Target Actions:**
    *   Convert `LocalStack` references to Rust borrows `&T`.
    *   Convert `HeapShared` allocations to `Rc<T>` or `Arc<T>`.
*   **JS Target Actions:**
    *   Apply structural duck-typing for external interop types.
    *   Inline `LocalStack` allocations to prevent GC pressure.

---

## 5. Extensibility & Future-Proofing
By completely isolating the **Constraint Generation** (F# Syntax) from the **Constraint Solving** (Algorithm J / Reachability Graph), and abstracting the **Environment Provider**, the compiler architecture inherently supports:
1.  Adding new target backends (e.g., WebAssembly, Python) simply by implementing a new `IExternalSymbolProvider` and `Specialization` pass.
2.  Adding new language features (e.g., GADTs, Higher-Kinded Types) strictly by modifying the `TypeVar` data structure and Unification step, without breaking the parser or backend emitters.

---

### Suggested Next Steps for Implementation
1.  **Phase 0:** Define the internal `Raw AST` structure in code.
2.  **Phase 1:** Implement the `TypeVar` class and a basic Union-Find Unification algorithm (ignoring Regions and Units for the first iteration).
3.  **Phase 2:** Build a mock `IExternalSymbolProvider` that just returns `.NET Core` primitives (`int`, `string`) to test the unification engine.