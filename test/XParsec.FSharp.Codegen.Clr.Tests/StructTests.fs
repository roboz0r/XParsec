module XParsec.FSharp.Codegen.Clr.Tests.StructTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `[<Struct>]` value-type emission. These tests reflect over the emitted PE so
// a runtime fault (bad IL, wrong base type, lost mutation) surfaces through
// `loadAssembly` / `Activator.CreateInstance`.
//
// The boxed-interface path is the one `Vesper.Set`'s struct enumerator needs:
// construct a struct, coerce it to an interface (box), dispatch through the
// boxed reference (callvirt). Value-type method dispatch on an unboxed local is
// also covered below (address-based `call` on the receiver).

[<Tests>]
let structTests =
    let declaredInstance =
        BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    testList
        "Struct"
        [
            test "a `[<Struct>]` type emits as a System.ValueType-based value type" {
                let _, artifact =
                    compileSource
                        "StructShape"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.X = x"
                                "let p = SPoint(3, 4)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                Expect.isNotNull ty "the assembly contains the struct type SPoint"
                Expect.isTrue ty.IsValueType "SPoint emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"

                let fields = ty.GetFields(BindingFlags.Public ||| BindingFlags.Instance)
                let names = fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                Expect.equal names (Set.ofList [ "x"; "y" ]) "both ctor-param backing fields are present"
            }

            test "a struct ctor stores ctor params + a member reads one back (boxed dispatch)" {
                let _, artifact =
                    compileSource
                        "StructCtorRead"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Holder(v: int) ="
                                "    interface System.IComparable with"
                                "        member this.CompareTo(o: obj) = v"
                                "let h = Holder(7)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"
                Expect.isTrue ty.IsValueType "Holder is a value type"

                // Reflection boxes the constructed struct; the interface dispatch is
                // a normal callvirt on the boxed reference, and the method reads the
                // ctor-param backing field through the byref `this`.
                let boxed = Activator.CreateInstance(ty, [| box 7 |])
                let cmp = boxed :?> IComparable
                Expect.equal (cmp.CompareTo(null)) 7 "CompareTo returns the stored ctor-param field"
            }

            test "a struct `val mutable` field mutates through a boxed method and persists" {
                let _, artifact =
                    compileSource
                        "StructMutable"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Counter ="
                                "    val mutable N: int"
                                "    member this.Bump() = this.N <- this.N + 1"
                                "    member this.Get() = this.N"
                                "let c = Counter()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                Expect.isTrue ty.IsValueType "Counter is a value type"

                let fld = ty.GetField("N", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.isNotNull fld "the val field N is emitted"
                Expect.isFalse fld.IsInitOnly "a mutable val field is writable"

                // A boxed value type: `Invoke` mutates the box's interior through the
                // byref `this`, so the mutation survives across calls.
                let boxed = Activator.CreateInstance ty
                let bump = ty.GetMethod("Bump", declaredInstance, null, [||], null)
                let get = ty.GetMethod("Get", declaredInstance, null, [||], null)
                bump.Invoke(boxed, [||]) |> ignore
                bump.Invoke(boxed, [||]) |> ignore
                Expect.equal (get.Invoke(boxed, [||]) :?> int) 2 "two Bump()s leave N = 2"
            }

            test "a method call on an unboxed struct local dispatches by address" {
                // `p.Sum()` on a `let`-bound struct value needs
                // the receiver *address* (`ldloca` + `constrained. callvirt`), not a
                // by-value `callvirt` (invalid IL on an unboxed value type).
                let _, artifact =
                    compileSource
                        "StructUnboxedCall"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.Sum() = x + y"
                                "    static member SumOf() : int ="
                                "        let p = SPoint(3, 4)"
                                "        p.Sum()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull sumOf "SumOf emitted as a static method"
                Expect.equal (sumOf.Invoke(null, [||]) :?> int) 7 "p.Sum() on an unboxed local returns 7"
            }

            test "a property get on an unboxed struct local dispatches by address" {
                let _, artifact =
                    compileSource
                        "StructUnboxedProp"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.X = x"
                                "    static member XOf() : int ="
                                "        let p = SPoint(5, 6)"
                                "        p.X"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let xOf = ty.GetMethod("XOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull xOf "XOf emitted as a static method"
                Expect.equal (xOf.Invoke(null, [||]) :?> int) 5 "p.X on an unboxed local returns 5"
            }

            test "a mutating method on an unboxed struct local persists (in-place addressing)" {
                // This only passes if the receiver is addressed in place (`ldloca`
                // the slot) — a spill-to-temp copy per call would mutate a throwaway
                // and `Get()` would read the un-mutated original.
                let _, artifact =
                    compileSource
                        "StructUnboxedMutate"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Counter ="
                                "    val mutable N: int"
                                "    member this.Bump() = this.N <- this.N + 1"
                                "    member this.Get() = this.N"
                                "    static member Run() : int ="
                                "        let c = Counter()"
                                "        c.Bump()"
                                "        c.Bump()"
                                "        c.Get()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                let run = ty.GetMethod("Run", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull run "Run emitted as a static method"
                Expect.equal (run.Invoke(null, [||]) :?> int) 2 "two Bump()s on the same local leave N = 2"
            }

            // A parameterless struct construction (`Counter()`)
            // lowers to `ldloca; initobj; ldloc` on a scratch local, not a `newobj`
            // against the synthesised parameterless `.ctor`. The field reads back as
            // its zero-init default, proving `initobj` produced a usable zeroed value.
            test "a parameterless struct construction zero-inits its fields via initobj" {
                let _, artifact =
                    compileSource
                        "StructInitObj"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Counter ="
                                "    val mutable N: int"
                                "    member this.Get() = this.N"
                                "    static member Fresh() : int ="
                                "        let c = Counter()"
                                "        c.Get()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Counter"
                let fresh = ty.GetMethod("Fresh", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull fresh "Fresh emitted as a static method"
                Expect.equal (fresh.Invoke(null, [||]) :?> int) 0 "an initobj-constructed Counter has N = 0"
            }

            test "a struct upcast `:>` to an interface boxes (round-trips through the interface)" {
                let _, artifact =
                    compileSource
                        "StructUpcast"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Holder(v: int) ="
                                "    interface System.IComparable with"
                                "        member this.CompareTo(o: obj) = v"
                                "    static member AsCmp(h: Holder) : System.IComparable = h :> System.IComparable"
                                "let h = Holder(9)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Holder"

                // `AsCmp` upcasts the struct to the interface (`box`); dispatching
                // through the returned reference proves the `:>` box round-trips.
                let asCmp = ty.GetMethod("AsCmp", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull asCmp "AsCmp emitted as a static method"

                let h = Activator.CreateInstance(ty, [| box 9 |])
                let cmp = asCmp.Invoke(null, [| h |]) :?> IComparable
                Expect.equal (cmp.CompareTo(null)) 9 "the boxed struct keeps its field value through `:>`"
            }

            // The natural shape `SumOf(a, b)` that the unboxed-dispatch test above had
            // to sidestep. A tupled static member now flattens to two scalar params,
            // so it can take its own args and forward them to the struct ctor.
            test "a two-parameter static member on a struct binds both args (SumOf(3,4) returns 7)" {
                let _, artifact =
                    compileSource
                        "StructStaticAdd2"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type SPoint(x: int, y: int) ="
                                "    member this.Sum() = x + y"
                                "    static member SumOf(a: int, b: int) : int ="
                                "        let p = SPoint(a, b)"
                                "        p.Sum()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "SPoint"
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.isNotNull sumOf "SumOf emitted as a static method"
                Expect.equal (sumOf.GetParameters().Length) 2 "SumOf has two scalar parameters (tuple flattened)"
                Expect.equal (sumOf.Invoke(null, [| box 3; box 4 |]) :?> int) 7 "SPoint(3,4).Sum() returns 7"
            }

            // A secondary ctor of the explicit field-init form
            // `new(args) = { f = e; … }`. Unlike a chain-form `new`, it stores
            // directly into the declared `val` fields (no primary-`.ctor` chain).
            test "a struct secondary ctor with an explicit field-init block initialises val fields" {
                let _, artifact =
                    compileSource
                        "StructFieldInit"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Pair ="
                                "    val mutable A: int"
                                "    val mutable B: int"
                                "    new(a: int, b: int) = { A = a; B = b }"
                                "    member this.Sum() = this.A + this.B"
                                "    static member SumOf(a: int, b: int) : int ="
                                "        let p = Pair(a, b)"
                                "        p.Sum()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Pair"
                Expect.isTrue ty.IsValueType "Pair is a value type"

                // The two-arg secondary ctor must exist alongside the (empty) primary.
                let ctor = ty.GetConstructor [| typeof<int>; typeof<int> |]
                Expect.isNotNull ctor "the two-arg secondary ctor is emitted"

                // Construct directly: the field-init block stored both args.
                let boxed = Activator.CreateInstance(ty, [| box 3; box 4 |])
                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal (fieldA.GetValue boxed :?> int) 3 "field A initialised from the first ctor param"
                Expect.equal (fieldB.GetValue boxed :?> int) 4 "field B initialised from the second ctor param"

                // And through a member that constructs + reads back.
                let sumOf = ty.GetMethod("SumOf", BindingFlags.Public ||| BindingFlags.Static)
                Expect.equal (sumOf.Invoke(null, [| box 3; box 4 |]) :?> int) 7 "Pair(3,4).Sum() returns 7"
            }

            test "a field-init ctor runs its let-preamble before storing fields" {
                // The `let d = a + a` preamble binds a local the field-init block
                // then reads — proving lets execute ahead of the `stfld` stores.
                let _, artifact =
                    compileSource
                        "StructFieldInitLet"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type LetPair ="
                                "    val mutable A: int"
                                "    val mutable B: int"
                                "    new(a: int) = let d = a + a in { A = a; B = d }"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "LetPair"

                let boxed = Activator.CreateInstance(ty, [| box 5 |])
                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal (fieldA.GetValue boxed :?> int) 5 "A = a"
                Expect.equal (fieldB.GetValue boxed :?> int) 10 "B = the let-bound a + a"
            }

            // An immutable `val x: T` (no `mutable`) emits as
            // `InitOnly`. Validation forbids `this.x <- …` on it, so it is only ever
            // written by a ctor — here the field-init secondary ctor's `stfld`, which
            // InitOnly permits. The field still round-trips its ctor-stored value.
            test "an immutable struct val field emits as InitOnly and is set by a field-init ctor" {
                let _, artifact =
                    compileSource
                        "StructInitOnly"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Ro ="
                                "    val A: int"
                                "    val mutable B: int"
                                "    new(a: int, b: int) = { A = a; B = b }"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Ro"

                let fieldA = ty.GetField("A", BindingFlags.Public ||| BindingFlags.Instance)
                let fieldB = ty.GetField("B", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.isTrue fieldA.IsInitOnly "an immutable val field is InitOnly"
                Expect.isFalse fieldB.IsInitOnly "a mutable val field stays writable"

                // The InitOnly field is still written by the field-init ctor.
                let boxed = Activator.CreateInstance(ty, [| box 3; box 4 |])
                Expect.equal (fieldA.GetValue boxed :?> int) 3 "InitOnly field initialised from the ctor"
                Expect.equal (fieldB.GetValue boxed :?> int) 4 "mutable field initialised from the ctor"
            }

            // The shape `Vesper.Set`'s hand-written enumerator needs: a field from a
            // ctor param plus a bool-literal field (`new(s) = { stack = s; started = false }`).
            test "a field-init ctor mixes a param-sourced field and a bool-literal field" {
                let _, artifact =
                    compileSource
                        "StructFieldInitMixed"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Iter ="
                                "    val mutable Cur: int"
                                "    val mutable Started: bool"
                                "    new(c: int) = { Cur = c; Started = false }"
                                "    member this.IsStarted() = this.Started"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Iter"

                let boxed = Activator.CreateInstance(ty, [| box 11 |])
                let fieldCur = ty.GetField("Cur", BindingFlags.Public ||| BindingFlags.Instance)

                let fieldStarted =
                    ty.GetField("Started", BindingFlags.Public ||| BindingFlags.Instance)

                Expect.equal (fieldCur.GetValue boxed :?> int) 11 "Cur initialised from the ctor param"
                Expect.equal (fieldStarted.GetValue boxed :?> bool) false "Started initialised from the bool literal"
            }

            // Generic structs. `SetIterator<'T>` is generic, so
            // the value-type flag must ride the generic self-`TypeSpec` (base type,
            // ctor field `MemberRef`s, signature encoding). These prove a generic
            // value type constructs, reflects as a generic value type, and reads a
            // ctor-param field back through a member.
            test "a generic `[<Struct>]` type emits as a generic value type" {
                let _, artifact =
                    compileSource
                        "GenericStructShape"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Box<'T>(value: 'T) ="
                                "    member this.Value = value"
                                "let b = Box<int>(42)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Box`1"
                Expect.isNotNull ty "the assembly contains the generic struct type Box`1"
                Expect.isTrue ty.IsValueType "Box`1 emits as a value type"
                Expect.isTrue ty.IsSealed "a value type is sealed"
                Expect.isTrue ty.IsGenericTypeDefinition "Box`1 is a generic type definition"

                let inst = ty.MakeGenericType [| typeof<int> |]
                let boxed = Activator.CreateInstance(inst, [| box 42 |])
                let valField = inst.GetField("value", BindingFlags.Public ||| BindingFlags.Instance)
                Expect.equal (valField.GetValue boxed :?> int) 42 "the ctor-param field stores the generic value"
            }

            test "a generic struct dispatches a member that reads a generic ctor-param field (boxed)" {
                // Boxed dispatch through the generic self-`TypeSpec`: the ctor stores
                // `value` into the open `Box\`1<!0>::value` field and `Get()` reads it
                // back. A `CLASS`-tagged self-`TypeSpec` would fault "value type
                // mismatch" before `Get()` ever runs.
                let _, artifact =
                    compileSource
                        "GenericStructMember"
                        (String.concat
                            "\n"
                            [ "[<Struct>]"; "type Box<'T>(value: 'T) ="; "    member this.Get() = value" ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let inst = (asm.GetType "Box`1").MakeGenericType [| typeof<int> |]

                let boxed = Activator.CreateInstance(inst, [| box 99 |])
                let get = inst.GetMethod("Get", declaredInstance, null, [||], null)
                Expect.equal (get.Invoke(boxed, [||]) :?> int) 99 "Get() reads the generic ctor-param field"
            }

            // The `SetIterator<'T>` shape: a generic struct with a `val mutable`
            // field plus an explicit field-init secondary ctor, boxed to an
            // interface — the full prerequisite for phase-6's hand-written enumerator.
            test "a generic struct with a val field + field-init ctor round-trips boxed to an interface" {
                let _, artifact =
                    compileSource
                        "GenericStructIter"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type Cell<'T> ="
                                "    val mutable Item: 'T"
                                "    val mutable Started: bool"
                                "    new(x: 'T) = { Item = x; Started = false }"
                                "    member this.Get() = this.Item"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let openTy = asm.GetType "Cell`1"
                Expect.isTrue openTy.IsValueType "Cell`1 is a value type"
                let ty = openTy.MakeGenericType [| typeof<int> |]

                let ctor =
                    openTy.GetConstructors() |> Array.find (fun c -> c.GetParameters().Length = 1)

                Expect.isNotNull ctor "the one-arg field-init secondary ctor is emitted"

                let boxed = Activator.CreateInstance(ty, [| box 7 |])
                let itemFld = ty.GetField("Item", BindingFlags.Public ||| BindingFlags.Instance)

                let startedFld =
                    ty.GetField("Started", BindingFlags.Public ||| BindingFlags.Instance)

                Expect.equal (itemFld.GetValue boxed :?> int) 7 "Item initialised from the generic ctor param"
                Expect.equal (startedFld.GetValue boxed :?> bool) false "Started initialised from the bool literal"

                let get = ty.GetMethod("Get", declaredInstance, null, [||], null)
                Expect.equal (get.Invoke(boxed, [||]) :?> int) 7 "Get() reads the val field back"
            }

            // B-3-alt — the struct *enumerator* shape. A generic struct that IS the
            // enumerator: it implements
            // `IEnumerator<'T>` (generic `Current`) + the non-generic `IEnumerator`
            // (`Current : obj`, `MoveNext`, `Reset`) + `IDisposable`, mutating its
            // own `val mutable` state through the byref `this` across `MoveNext`
            // calls on the *boxed* struct. This is `SetTree.SetIterator<'T>` minus
            // the AVL stack — the object expression `mkIEnumerator` is replaced by.
            test "a generic struct enumerator implements the three IEnumerator interfaces and advances boxed" {
                let _, artifact =
                    compileSource
                        "StructEnumerator"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type OnceEnum<'T> ="
                                "    val mutable Item: 'T"
                                "    val mutable Started: bool"
                                "    new(x: 'T) = { Item = x; Started = false }"
                                "    interface System.Collections.Generic.IEnumerator<'T> with"
                                "        member this.Current = this.Item"
                                "    interface System.Collections.IEnumerator with"
                                "        member this.Current = box this.Item"
                                "        member this.MoveNext() ="
                                "            if this.Started then"
                                "                false"
                                "            else"
                                "                this.Started <- true"
                                "                true"
                                "        member this.Reset() = ()"
                                "    interface System.IDisposable with"
                                "        member this.Dispose() = ()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let openTy = asm.GetType "OnceEnum`1"
                Expect.isTrue openTy.IsValueType "OnceEnum`1 is a value type"
                let ty = openTy.MakeGenericType [| typeof<int> |]

                // Box the struct and drive it through the generic `IEnumerator<int>`.
                // The mutation in `MoveNext` must survive across calls (byref `this`
                // into the box), so the second `MoveNext` reports the end.
                let boxed = Activator.CreateInstance(ty, [| box 42 |])
                let e = boxed :?> System.Collections.Generic.IEnumerator<int>
                Expect.isTrue (e.MoveNext()) "first MoveNext starts the single-element enumeration"
                Expect.equal e.Current 42 "Current yields the ctor-stored item through IEnumerator<int>"
                Expect.isFalse (e.MoveNext()) "second MoveNext reports the end (Started mutation persisted)"

                // The non-generic `IEnumerator.Current` boxes the item.
                let boxed2 = Activator.CreateInstance(ty, [| box 7 |])
                let ng = boxed2 :?> System.Collections.IEnumerator
                Expect.isTrue (ng.MoveNext()) "non-generic MoveNext advances"
                Expect.equal (ng.Current :?> int) 7 "non-generic Current boxes the item"
            }

            // NOTE (B-3-alt limitation): a struct enumerator must inline its advance
            // / read logic *directly* in the interface members (as the test above
            // does), not factor it into a public member the interface forwards to.
            // Two gaps block the forwarded shape and keep `set.fs`'s `SetIterator`
            // on the inlined form:
            //   * same name (`member this.MoveNext() = this.MoveNext()`) resolves the
            //     inner call to the interface method ⇒ infinite recursion;
            //   * distinct name (`… = this.Advance()`) calls the helper by *value*
            //     ⇒ the mutation to `this` happens on a copy and is lost.
            // Both are separate from this phase's struct-enumerator support; the
            // inlined form is the supported shape.

            // The full B-3-alt wiring: a class whose `GetEnumerator()` *constructs*
            // the struct enumerator and returns it `:>`-upcast (boxed) to the
            // interface — exactly `Set<'T>.GetEnumerator()` ⇒ `SetTree.mkIEnumerator`.
            // Phase 5's codegen test stored a ctor-supplied enumerator; here the
            // enumerator is built in-method from a struct value type.
            test "a class GetEnumerator constructs a struct enumerator and returns it boxed (yields the element)" {
                let _, artifact =
                    compileSource
                        "StructEnumeratorSeq"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type OnceEnum<'T> ="
                                "    val mutable Item: 'T"
                                "    val mutable Started: bool"
                                "    new(x: 'T) = { Item = x; Started = false }"
                                "    interface System.Collections.Generic.IEnumerator<'T> with"
                                "        member this.Current = this.Item"
                                "    interface System.Collections.IEnumerator with"
                                "        member this.Current = box this.Item"
                                "        member this.MoveNext() ="
                                "            if this.Started then"
                                "                false"
                                "            else"
                                "                this.Started <- true"
                                "                true"
                                "        member this.Reset() = ()"
                                "    interface System.IDisposable with"
                                "        member this.Dispose() = ()"
                                "type OnceSeq<'T>(x: 'T) ="
                                "    interface System.Collections.Generic.IEnumerable<'T> with"
                                "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> ="
                                "            OnceEnum(x) :> System.Collections.Generic.IEnumerator<'T>"
                                "    interface System.Collections.IEnumerable with"
                                "        member this.GetEnumerator() : System.Collections.IEnumerator ="
                                "            OnceEnum(x) :> System.Collections.IEnumerator"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let seqTy = (asm.GetType "OnceSeq`1").MakeGenericType [| typeof<int> |]

                let s =
                    Activator.CreateInstance(seqTy, [| box 99 |]) :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal
                    (s |> Seq.toList)
                    [ 99 ]
                    "enumerating the seq via its struct enumerator yields the single element"
            }

            // The `set.fs` construction shape: an *explicit* type application at the
            // construction site (`SetIterator<'T>(s.Tree)` → here `OnceEnum<'T>(x)`).
            // The secondary-ctor type args ground from both the explicit `<'T>` and
            // the value arg; without the local-secondary-ctor inference path the
            // result type's arg leaks as a free `TyVar` (`?ungrounded-operator`).
            test "a class GetEnumerator constructs the struct enumerator with explicit type args (Set<'T> shape)" {
                let _, artifact =
                    compileSource
                        "StructEnumeratorTypeApp"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type OnceEnum<'T> ="
                                "    val mutable Item: 'T"
                                "    val mutable Started: bool"
                                "    new(x: 'T) = { Item = x; Started = false }"
                                "    interface System.Collections.Generic.IEnumerator<'T> with"
                                "        member this.Current = this.Item"
                                "    interface System.Collections.IEnumerator with"
                                "        member this.Current = box this.Item"
                                "        member this.MoveNext() ="
                                "            if this.Started then"
                                "                false"
                                "            else"
                                "                this.Started <- true"
                                "                true"
                                "        member this.Reset() = ()"
                                "    interface System.IDisposable with"
                                "        member this.Dispose() = ()"
                                "type OnceSeq<'T>(x: 'T) ="
                                "    interface System.Collections.Generic.IEnumerable<'T> with"
                                "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> ="
                                "            OnceEnum<'T>(x) :> System.Collections.Generic.IEnumerator<'T>"
                                "    interface System.Collections.IEnumerable with"
                                "        member this.GetEnumerator() : System.Collections.IEnumerator ="
                                "            OnceEnum<'T>(x) :> System.Collections.IEnumerator"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let seqTy = (asm.GetType "OnceSeq`1").MakeGenericType [| typeof<int> |]

                let s =
                    Activator.CreateInstance(seqTy, [| box 5 |]) :?> System.Collections.Generic.IEnumerable<int>

                Expect.equal (s |> Seq.toList) [ 5 ] "explicit-type-app construction enumerates to the single element"
            }

            // A `[<Struct>]` declared in a *referenced package*
            // and consumed by name. The contract `.fsi` publishes a `struct … end`
            // value type; a consumer compiled against that contract must encode the
            // referenced type as `ELEMENT_TYPE_VALUETYPE` (0x11), not `CLASS` (0x12) —
            // a wrong tag faults the loader "value type mismatch". This is the
            // cross-package twin of the project-local struct-signature path: it stands
            // up a synthetic producer package (manifest + `.fsi`, no DLL needed — the
            // emitted *signature* is the encoder's decisive output) and reads the
            // consumer's emitted MethodDef signature straight off the metadata.
            test "a struct declared in a referenced package encodes as VALUETYPE in a consumer signature" {
                // The package directory name IS the package identity (`buildClosure`
                // resolves `depends-on` against it), so it must equal the manifest
                // `name` — hence `Vesper.PointPkg`, not a descriptive slug.
                let outDir = tmpDir "Vesper.PointPkg"
                let manifestPath = System.IO.Path.Combine(outDir, "manifest.toml")
                let fsiPath = System.IO.Path.Combine(outDir, "point.fsi")

                // `depends-on = []`: the `int` field type resolves from the
                // `Vesper.Core` manifest we pass explicitly in the flat stack below,
                // so no sibling-package closure resolution is triggered.
                System.IO.File.WriteAllText(
                    manifestPath,
                    "[core]\nname = \"Vesper.PointPkg\"\nnamespace = \"Vesper\"\ndepends-on = []\nfiles = [\"point.fsi\"]\n"
                )

                System.IO.File.WriteAllText(
                    fsiPath,
                    "namespace Vesper\n\ntype Point =\n    struct\n        val X: int\n        val Y: int\n    end\n"
                )

                let provider = SymbolProviders.buildContract [ vesperCoreManifest; manifestPath ]

                // Identity over the referenced struct: forces `Point` into the
                // emitted method's signature (return + param) without constructing it.
                let src =
                    "namespace App\n\nopen Vesper\n\nmodule Consumer =\n    let echo (p: Point) : Point = p\n"

                let project = ProjectInfo.library "StructXPkgConsumer"
                let lexed, file = parseFile src
                let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

                if not (List.isEmpty errors) then
                    failwithf "consumer failed to analyse: %A" (errors |> List.map (fun d -> d.Message))

                let artifact = Codegen.compile provider project tast
                let bytes = Codegen.toBytes artifact

                // Find `echo`'s declaring (module) type without hard-coding the
                // emitted module-type name.
                let declType =
                    match peMethodNames bytes |> List.filter (fun (_, m) -> m = "echo") with
                    | (t, _) :: _ -> t
                    | [] -> failwithf "no `echo` method emitted; methods: %A" (peMethodNames bytes)

                let elem = peMethodReturnElementType bytes declType "echo"

                // 0x11 = ELEMENT_TYPE_VALUETYPE, 0x12 = ELEMENT_TYPE_CLASS.
                Expect.notEqual elem 0x12uy "the referenced struct must NOT encode as ELEMENT_TYPE_CLASS"
                Expect.equal elem 0x11uy "the referenced struct encodes as ELEMENT_TYPE_VALUETYPE"
            }

            // G4 — `set.fs`'s `SetIterator.MoveNext` shape: an infix operator
            // (`t.Height = 1`) *inside a struct interface member body*, where `t` is
            // a class instance popped from a list-typed `val` field (`match
            // this.Stack with | t :: rest -> …`). Desugar only walked the type's
            // *own* members, never `interface … with member …` bodies, so the `=`
            // node got no `DesugaredForm.OpName` entry and Freeze threw
            // `InfixApp … missing DesugaredForm entry`. None of the enumerator tests
            // above caught it: their interface members contain no infix operator.
            // This drives the boxed enumerator over the true `=` branch.
            test "an infix operator inside a struct interface member resolves (SetIterator.MoveNext shape)" {
                let _, artifact =
                    compileSource
                        "StructIfaceInfix"
                        (String.concat
                            "\n"
                            [
                                "type List<'T> ="
                                "    | ([]): List<'T>"
                                "    | (::): Head: 'T * Tail: List<'T> -> List<'T>"
                                "and 'T list = List<'T>"
                                "type Node(h: int) ="
                                "    member this.Height = h"
                                "[<Struct>]"
                                "type Iter ="
                                "    val mutable Stack: Node list"
                                "    val mutable Hit: bool"
                                "    new(n: Node) = { Stack = n :: []; Hit = false }"
                                "    interface System.Collections.IEnumerator with"
                                "        member this.Current = box 0"
                                "        member this.MoveNext() ="
                                "            match this.Stack with"
                                "            | [] -> false"
                                "            | t :: rest ->"
                                "                if t.Height = 1 then"
                                "                    this.Stack <- rest"
                                "                    this.Hit <- true"
                                "                    true"
                                "                else"
                                "                    false"
                                "        member this.Reset() = ()"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Iter"
                Expect.isTrue ty.IsValueType "Iter is a value type"

                // Box the struct and drive `MoveNext` through `IEnumerator`. The
                // ctor seeds a single-element stack whose `Node.Height = 1`, so the
                // first `MoveNext` takes the `=`-true branch (pops the stack, sets
                // `Hit`) and returns true — proving the interface-body `=` both
                // froze and ran. A second call hits the `[]` arm and returns false.
                let nodeTy = asm.GetType "Node"
                let node = Activator.CreateInstance(nodeTy, [| box 1 |])
                let boxed = Activator.CreateInstance(ty, [| node |])
                let e = boxed :?> System.Collections.IEnumerator
                Expect.isTrue (e.MoveNext()) "first MoveNext takes the `t.Height = 1` true branch"
                Expect.isFalse (e.MoveNext()) "second MoveNext hits the empty-stack arm"
            }

            // Regression: a chained property access `this.field.Prop` where `field`
            // is a `val`/ctor-param *instance field* (not a member) and `Prop`'s
            // type differs from the field's. Freeze's `recoverFieldStepTy` resolves
            // each intermediate chain segment's type; its `TyClass` arm used to scan
            // only the class's *members*, missing `val`/ctor-param fields — so it
            // fell back to the chain's *final* type and mis-typed the receiver
            // (`this.Stack : List<int>` collapsed to `bool`, the type of `.IsEmpty`).
            // Codegen then routed the receiver through the wrong member-ref path and
            // faulted. This is exactly set.fs's `not this.stack.IsEmpty` shape.
            test "a chained property on a struct val field types the receiver as the field, not the property" {
                let _, artifact =
                    compileSource
                        "StructFieldChainProp"
                        (String.concat
                            "\n"
                            [
                                "type List<'T> ="
                                "    | ([]): List<'T>"
                                "    | (::): Head: 'T * Tail: List<'T> -> List<'T>"
                                "    member this.IsEmpty = match this with | [] -> true | _ -> false"
                                "and 'T list = List<'T>"
                                "[<Struct>]"
                                "type Wrap ="
                                "    val mutable Stack: int list"
                                "    new(n: int) = { Stack = (if n = 0 then [] else n :: []) }"
                                "    member this.NotEmpty() = not this.Stack.IsEmpty"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "Wrap"
                Expect.isTrue ty.IsValueType "Wrap is a value type"

                let notEmpty = ty.GetMethod("NotEmpty", declaredInstance, null, [||], null)

                // `Wrap(0)` seeds `[]` ⇒ `this.Stack.IsEmpty` is true ⇒ `NotEmpty()` false.
                let emptyWrap = Activator.CreateInstance(ty, [| box 0 |])
                Expect.isFalse (notEmpty.Invoke(emptyWrap, [||]) :?> bool) "empty stack ⇒ NotEmpty() is false"

                // `Wrap(5)` seeds `5 :: []` ⇒ `IsEmpty` false ⇒ `NotEmpty()` true.
                let fullWrap = Activator.CreateInstance(ty, [| box 5 |])
                Expect.isTrue (notEmpty.Invoke(fullWrap, [||]) :?> bool) "non-empty stack ⇒ NotEmpty() is true"
            }

            // PP1 (ref-struct-emit / printf-port-steps): a `[<Struct; IsByRefLike>]`
            // type emits the `IsByRefLikeAttribute` marker so the CLR confines it
            // to the stack. The runtime surfaces this directly as
            // `Type.IsByRefLike`; this is the first custom attribute the backend
            // ever emits.
            test "a `[<Struct; IsByRefLike>]` type emits a byref-like value type" {
                let _, artifact =
                    compileSource
                        "RefStructShape"
                        (String.concat
                            "\n"
                            [
                                "[<Struct; IsByRefLike>]"
                                "type RPoint(x: int, y: int) ="
                                "    member this.X = x"
                                "let p = RPoint(3, 4)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "RPoint"
                Expect.isNotNull ty "the assembly contains the type RPoint"
                Expect.isTrue ty.IsValueType "RPoint emits as a value type"
                Expect.isTrue ty.IsByRefLike "RPoint is byref-like (ref struct)"
            }

            // The marker is opt-in: a plain `[<Struct>]` stays non-byref-like, so
            // the new attribute can't leak onto every value type.
            test "a plain `[<Struct>]` type is not byref-like" {
                let _, artifact =
                    compileSource
                        "PlainStructNotRefLike"
                        (String.concat
                            "\n"
                            [
                                "[<Struct>]"
                                "type NPoint(x: int, y: int) ="
                                "    member this.X = x"
                                "let p = NPoint(3, 4)"
                            ])

                let asm = loadAssembly (Codegen.toBytes artifact)
                let ty = asm.GetType "NPoint"
                Expect.isTrue ty.IsValueType "NPoint is a value type"
                Expect.isFalse ty.IsByRefLike "a plain [<Struct>] is not byref-like"
            }

            // PP2 (printf-port-steps): a ref struct with a `Span<char>` field, a ctor
            // that fills it from a `char[]`, and members that read `.Length` and
            // `.Slice` it, round-trip through compile + run. Exercises three things the
            // backend lacked: an external generic *value-type* TypeSpec parent (the
            // `Span`1<char>` ctor / member refs must be tagged `VALUETYPE`, not
            // `CLASS`), and address-based (`ldloca` + non-virtual `call`) dispatch of
            // an external struct receiver's method (`Slice`) and property (`Length`) —
            // a by-value `callvirt` is verifier-illegal on a ref struct. (The
            // `ref T`-returning indexer `chars.[i]` needs byref support and is tracked
            // separately.)
            test "PP2: ref struct with a Span<char> field — ctor, Length, Slice round-trip" {
                runsLines
                    [ "5"; "3" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "[<Struct; IsByRefLike>]"
                            "type SpanView(chars: Span<char>) ="
                            "    member this.Len = chars.Length"
                            "    member this.Tail = chars.Slice(2, 3)"
                            "let arr = [| 'h'; 'e'; 'l'; 'l'; 'o' |]"
                            "let v = SpanView(Span<char>(arr))"
                            "printfn \"%d\" v.Len"
                            "printfn \"%d\" v.Tail.Length"
                        ])
            }

            // PP2b (printf-port-steps): the byref-return indexer `chars.[i]`, the one
            // piece PP2a deferred. `Span<T>`'s only element accessor is
            // `get_Item(i) : T&` (a managed-pointer return, no by-value form), so this
            // exercises the whole byref stack: resolving a by-ref-returning BCL member
            // (`tryBuildType` no longer drops `T&`; the indexer surfaces as
            // `get_Item` carrying a `FTConst("&", [elem])` return), encoding
            // `ELEMENT_TYPE_BYREF` at the member-ref return seam, and dereferencing the
            // result (`call get_Item` → `ldobj <elem>`). `At 1 = 'e'` is the checkpoint
            // assertion. (The 1-arg `Slice(int)` the plan also lists is blocked on an
            // orthogonal gap — folded-`LongIdent` instance-method overload resolution,
            // not byref — see printf-port-steps.md PP2b.)
            test "PP2b: ref struct Span<char> byref indexer read — chars.[i]" {
                runsLines
                    [ "e"; "o" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "[<Struct; IsByRefLike>]"
                            "type SpanView(chars: Span<char>) ="
                            "    member this.At(i) = chars.[i]"
                            "let arr = [| 'h'; 'e'; 'l'; 'l'; 'o' |]"
                            "let v = SpanView(Span<char>(arr))"
                            "printfn \"%c\" (v.At 1)"
                            "printfn \"%c\" (v.At 4)"
                        ])
            }

            // PP3 (printf-port-steps): `ArrayPool<char>.Shared.Rent/Return` — generic
            // external static-property access (`.Shared` on the constructed generic
            // `ArrayPool<char>`) plus instance method calls on the result. `Rent(int)`
            // is the plan's stated checkpoint; `Return(arr)` is the load-bearing
            // addition — it omits `Return`'s trailing optional `clearArray = false`
            // parameter. Optional-argument omission is the real PP3 gap (the generic
            // two-axis signature build already landed): the provider surfaces the
            // member's `OptionalDefaults`, Unification permits the under-applied arity
            // and records the omitted constant, and Freeze synthesises it as a literal
            // so codegen sees the full tupled call. Both the bound-receiver form
            // (`pool.Return`) and the chained static form (`ArrayPool<char>.Shared.Return`,
            // as `Formatter.cs` writes it) are exercised.
            test "PP3: ArrayPool<char>.Shared Rent + Return (omitted optional arg)" {
                runsLines
                    [ "ok" ]
                    (String.concat
                        "\n"
                        [
                            "open System.Buffers"
                            "let pool = ArrayPool<char>.Shared"
                            "let a = pool.Rent(256)"
                            "let b = ArrayPool<char>.Shared.Rent(512)"
                            "let ok = a.Length >= 256 && b.Length >= 512"
                            "pool.Return(a)"
                            "ArrayPool<char>.Shared.Return(b)"
                            "printfn \"%s\" (if ok then \"ok\" else \"no\")"
                        ])
            }

            // PP4 (printf-port-steps): the `null` literal *pattern* (`match x with
            // null -> …`), the explicit-null-match shape the `Formatter` port uses
            // to replace C#'s `?.` / `??`. It binds nothing and lowers to a non-null
            // test (`ldloc; brtrue` skips the arm), so a null scrutinee falls to the
            // body and any other value to the next arm. New front-to-back: a
            // `TPatG.Null` case threaded through Freeze + the pattern emitter.
            test "PP4: a `null` literal pattern matches a null reference, binds nothing" {
                runsLines
                    [ "null"; "value" ]
                    (String.concat
                        "\n"
                        [
                            "let describe (s: string) ="
                            "    match s with"
                            "    | null -> \"null\""
                            "    | _ -> \"value\""
                            "printfn \"%s\" (describe null)"
                            "printfn \"%s\" (describe \"hi\")"
                        ])
            }

            // PP4 checkpoint: the `Formatter` field block declares + initialises
            // cleanly. Mirrors `Formatter.cs`'s fields front-to-back — `const int`
            // ported as module-level `let`s, `static readonly Provider` as a
            // `static let` (a static field + the type `.cctor`), the nullable ref
            // fields (`TextWriter?`, `char[]?`) as their underlying ref types with
            // explicit `null` handling (the `?.` replacement), the `Span<char>`
            // buffer (PP2a) and the `int` position. Two construction shapes — a
            // write-through sink (a real writer + a pooled buffer) and a string sink
            // (both null) — prove each field declares and initialises, and the
            // null-matching members prove the explicit-null handling.
            // PP5a (printf-port-steps): `Span<char>` passed as a *by-value argument*
            // plus the buffer-copy method surface every `Formatter` body uses —
            // `string.CopyTo(Span<char>)`, `Span<char>.CopyTo(Span<char>)`,
            // `Span<char>.Fill(char)`, `Span<char>.ToString()`, and
            // `string.TryCopyTo(Span<char>) : bool`. Rides PP2a (Span as a value), but
            // each call passes / produces a `Span<char>` in *argument* position (where
            // PP2a only proved the receiver/field/return), so the member-ref parent for
            // a Span parameter must encode `VALUETYPE`. The slices use the 2-arg
            // `Slice(int,int)` form throughout — the 1-arg `Slice(int)` folds into a
            // LongIdent head and hits the orthogonal overload-resolution gap PP2b noted.
            test "PP5a: Span<char> by-value args — string.CopyTo, span CopyTo, Fill, ToString, TryCopyTo" {
                runsLines
                    [ "ab---cd"; "ab---cdab"; "true"; "false" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "[<Struct; IsByRefLike>]"
                            "type Buf(chars: Span<char>) ="
                            // 'ab' at 0..1, '---' filled at 2..4, 'cd' at 5..6
                            "    member this.Build() ="
                            "        \"ab\".CopyTo(chars.Slice(0, 2))"
                            "        chars.Slice(2, 3).Fill('-')"
                            "        \"cd\".CopyTo(chars.Slice(5, 2))"
                            "        chars.Slice(0, 7).ToString()"
                            // span→span copy: duplicate 'ab' into 7..8
                            "    member this.Dup() ="
                            "        chars.Slice(0, 2).CopyTo(chars.Slice(7, 2))"
                            "        chars.Slice(0, 9).ToString()"
                            "    member this.Fits() = \"xy\".TryCopyTo(chars.Slice(0, 2))"
                            "    member this.Overflows() = \"toolong\".TryCopyTo(chars.Slice(0, 2))"
                            "let arr = [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |]"
                            "let b = Buf(Span<char>(arr))"
                            "printfn \"%s\" (b.Build())"
                            "printfn \"%s\" (b.Dup())"
                            "printfn \"%b\" (b.Fits())"
                            "printfn \"%b\" (b.Overflows())"
                        ])
            }

            test "PP4: the Formatter field block declares + initialises (static let, nullable refs, Span)" {
                runsLines
                    [ "true"; "5"; "5"; "false"; "-1"; "true" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.IO"
                            "open System.Globalization"
                            "let GuessedLengthPerHole = 11"
                            "let MinimumArrayPoolLength = 256"
                            "[<Struct; IsByRefLike>]"
                            "type FieldBlock ="
                            "    static let provider : IFormatProvider = CultureInfo.InvariantCulture"
                            "    val mutable private Writer: TextWriter"
                            "    val mutable private Pool: char[]"
                            "    val mutable private Chars: Span<char>"
                            "    val mutable private Pos: int"
                            "    new(w: TextWriter, buf: char[]) = { Writer = w; Pool = buf; Chars = Span<char>(buf); Pos = 0 }"
                            "    member this.HasWriter = match this.Writer with null -> false | _ -> true"
                            "    member this.Cap = this.Chars.Length"
                            "    member this.PoolLen = match this.Pool with null -> -1 | arr -> arr.Length"
                            "    member this.ProviderOk = match box provider with null -> false | _ -> true"
                            "let arr = [| 'h'; 'e'; 'l'; 'l'; 'o' |]"
                            "let a = FieldBlock(Console.Out, arr)"
                            "let b = FieldBlock(null, null)"
                            "printfn \"%b\" a.HasWriter"
                            "printfn \"%d\" a.Cap"
                            "printfn \"%d\" a.PoolLen"
                            "printfn \"%b\" b.HasWriter"
                            "printfn \"%d\" b.PoolLen"
                            "printfn \"%b\" a.ProviderOk"
                        ])
            }

            // PP5b (printf-port-steps): unsigned literals + the `uint` cast.
            test "PP5b: hex uint32 literal reads back; radix literals" {
                runsLines
                    [ "true"; "true"; "true"; "true" ]
                    (String.concat
                        "\n"
                        [
                            "let MaxChars = 0x3FFFFFDFu"
                            "let hexI = 0xFF"
                            "let octI = 0o17"
                            "let binU = 0b1010u"
                            "printfn \"%b\" (MaxChars = 1073741791u)"
                            "printfn \"%b\" (hexI = 255)"
                            "printfn \"%b\" (octI = 15)"
                            "printfn \"%b\" (binU = 10u)"
                        ])
            }

            test "PP5b: uint cast feeding Math.Max/Min/Clamp" {
                runsLines
                    [ "true" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "let MaxChars = 0x3FFFFFDFu"
                            "let grow (pos: int) (additional: int) (cap: int) ="
                            "    let needed = uint pos + uint additional"
                            "    let doubled = uint cap * 2u"
                            "    let size = Math.Max(needed, doubled)"
                            "    Math.Min(Math.Clamp(size, 256u, MaxChars), MaxChars)"
                            "printfn \"%b\" (grow 10 20 8 = 256u)"
                        ])
            }

            // PP5c (printf-port-steps): the hole-formatting dispatch
            // `AppendFormatted<'T>` via `IFormattable`. `box value` once, then a
            // 3-way match: `:? IFormattable` (the invariant-culture
            // `ToString(null, provider)` — value types like int / float), the
            // `null` literal pattern (PP4), and the `o.ToString()` fallback for a
            // non-`IFormattable` reference (string). The member is generic and
            // *called within the defining assembly at three distinct types* (int,
            // float, string), which forced the inference fix: a project-local
            // generic member's own method typars are now freshened per call site
            // (`Engine.instantiateMemberCall`) instead of the one prototype typar
            // grounding to the first call's type — without it the member emitted
            // as a single mono method specialised to `int` and the `float` call
            // passed a wrong-typed argument (`InvalidProgramException` at JIT).
            test "PP5c: AppendFormatted<'T> IFormattable dispatch (int/float/ref), same-assembly multi-instantiation" {
                runsLines
                    [ "42"; "3.14"; "hi" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.Globalization"
                            "[<Struct; IsByRefLike>]"
                            "type Holder(seed: int) ="
                            "    static let provider : IFormatProvider = CultureInfo.InvariantCulture"
                            "    member this.AppendFormatted(value: 'T) : string ="
                            "        let o = box value"
                            "        match o with"
                            "        | :? IFormattable as f -> f.ToString(null, provider)"
                            "        | null -> \"\""
                            "        | _ -> o.ToString()"
                            "let h = Holder(0)"
                            "printfn \"%s\" (h.AppendFormatted 42)"
                            "printfn \"%s\" (h.AppendFormatted 3.14)"
                            "printfn \"%s\" (h.AppendFormatted \"hi\")"
                        ])
            }

            // Regression for the same inference fix at module scope: a project-local
            // *generic free function* called at two distinct types must stay one
            // generic method, not ground to the first call. (A generic free function
            // that *captures a module value* is lifted to a generic closure, a
            // separate still-open gap — this one captures nothing.)
            test "PP5c: generic free function, same-assembly multi-instantiation" {
                runsLines
                    [ "42"; "3.14" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.Globalization"
                            "let format (value: 'T) : string ="
                            "    let o = box value"
                            "    match o with"
                            "    | :? IFormattable as f -> f.ToString(null, CultureInfo.InvariantCulture)"
                            "    | null -> \"\""
                            "    | _ -> o.ToString()"
                            "printfn \"%s\" (format 42)"
                            "printfn \"%s\" (format 3.14)"
                        ])
            }

            // PP5d (printf-port-steps): byref `out`/`ref` *arguments* — the
            // genuinely-new compiler feature. `&local` (managed address-of) is the
            // byref intrinsic `TyConst("&", [T])` (`inferPrefix`), lowered at Freeze
            // to an `ldloca` of the operand's slot; the member-ref encodes the
            // parameter with the `ELEMENT_TYPE_BYREF` prefix (`mintMemberRef`), so a
            // BCL method writes back through the local. `Int32.TryParse(string,
            // out int)` is the clean non-`Span` `out` probe: the local must be a
            // function-local mutable (a module-level mutable is a static field, not a
            // slot-addressable local).
            test "PP5d: Int32.TryParse(s, &r) writes the out local through a byref arg" {
                runsLines
                    [ "123"; "-1" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "let parse (s: string) : int ="
                            "    let mutable r = 0"
                            "    let ok = Int32.TryParse(s, &r)"
                            "    if ok then r else -1"
                            "printfn \"%d\" (parse \"123\")"
                            "printfn \"%d\" (parse \"oops\")"
                        ])
            }

            // PP5d part 4 (printf-port-steps): the `ISpanFormattable.TryFormat`
            // span fast-path — the no-alloc branch the `Formatter` prefers over the
            // `IFormattable.ToString` fallback. `box value` once, then
            // `:? ISpanFormattable as sf -> sf.TryFormat(dest, &cw,
            // ReadOnlySpan<char>.Empty, provider)` writes the formatted chars
            // straight into the `Span<char>` buffer (no intermediate string),
            // reading the count back through the `&cw` out arg (PP5d's byref-arg
            // machinery) and slicing the dest to materialise the result. The two
            // earlier-deferred ingredients now compile: the empty
            // `ReadOnlySpan<char>.Empty` format arg (the `TODO TypeApp` blocker is
            // gone — an external generic-type static-property receiver resolves
            // through `ExternalAccess`) and the `Span<char>` by-value `dest`
            // (PP5a). `int` / `float` implement `ISpanFormattable` (the fast path);
            // `string` implements neither, so it falls to the `o.ToString()` arm —
            // and every fast-path result is byte-identical to the `IFormattable`
            // path PP5c delivers.
            test "PP5d part 4: AppendFormatted span fast-path via ISpanFormattable.TryFormat" {
                runsLines
                    [ "42"; "3.14"; "hi" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.Buffers"
                            "open System.Globalization"
                            "[<Struct; IsByRefLike>]"
                            "type Holder(seed: int) ="
                            "    static let provider : IFormatProvider = CultureInfo.InvariantCulture"
                            "    member this.AppendFormatted(value: 'T) : string ="
                            "        let buf = ArrayPool<char>.Shared.Rent(64)"
                            "        let dest = Span<char>(buf)"
                            "        let o = box value"
                            "        let result ="
                            "            match o with"
                            "            | :? ISpanFormattable as sf ->"
                            "                let mutable cw = 0"
                            "                if sf.TryFormat(dest, &cw, ReadOnlySpan<char>.Empty, provider) then"
                            "                    dest.Slice(0, cw).ToString()"
                            "                else"
                            "                    \"\""
                            "            | :? IFormattable as f -> f.ToString(null, provider)"
                            "            | null -> \"\""
                            "            | _ -> o.ToString()"
                            "        ArrayPool<char>.Shared.Return(buf)"
                            "        result"
                            "let h = Holder(0)"
                            "printfn \"%s\" (h.AppendFormatted 42)"
                            "printfn \"%s\" (h.AppendFormatted 3.14)"
                            "printfn \"%s\" (h.AppendFormatted \"hi\")"
                        ])
            }

            // PP5f (printf-port-steps): the integration rung — the realisable
            // `Formatter` core (`Handler`, a string-sink ref struct) ported
            // front-to-back, proving the prior rungs compose AND the load-bearing
            // codegen fix this rung surfaced. Stitches: the field block + ctor
            // (PP4 / PP2a / PP3 — `ArrayPool.Rent`), `string.TryCopyTo` /
            // `Span.CopyTo` / `Span.ToString` (PP5a), the single generic
            // `AppendFormatted<'T>` `IFormattable` hole dispatch with the `null`
            // pattern (PP5c / PP4), and a real grow (`GrowThenCopyString` → `Grow` →
            // `GrowCore`) exercised by the 400-char case. **The point of the test:**
            // every body mutates `this` through *self-calls* (`AppendFormatted` →
            // `this.AppendLiteral`, `AppendLiteral` → `this.GrowThenCopyString` →
            // `this.Grow` → `this.GrowCore`); these only persist because of the
            // struct self-receiver in-place addressing fix this rung landed
            // (`EmitMember.loadStructReceiverAddr` + the `thisKey`-as-`SelfKey`
            // wiring in `Emit.buildMember`). Without it a struct self-call addresses
            // a *defensive copy* of `this` — the mutation is lost (an immutable F#
            // local would copy per `fsc`; the repo addresses in place, and now does
            // so for self-calls too). See `docs/overload-resolution-bug.md` for the
            // gaps that scoped this down (the four overloaded `AppendFormatted`, the
            // write-through `Flush`, the dedicated `Append*` members) and
            // printf-port-steps.md PP5f. The few deviations below (int grow vs PP5b
            // `uint`, `TryCopyTo` vs `string.CopyTo`, `while` vs `for … in 1..n`,
            // string sink only) each dodge one of those gaps and are byte-equivalent
            // for the holes exercised.
            test "PP5f: Formatter core — literal, generic hole, grow, string sink" {
                runsLines
                    [
                        "x=42, pi=3.14" // literal + AppendFormatted int/float (invariant)
                        "400" // 200 × "ab" forced repeated grows (GrowThenCopyString)
                    ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.Buffers"
                            "open System.Globalization"
                            "[<Struct; IsByRefLike>]"
                            "type Handler ="
                            "    static let MinimumArrayPoolLength = 256"
                            "    static let MaxChars = 0x3FFFFFDF"
                            "    static let provider : IFormatProvider = CultureInfo.InvariantCulture"
                            "    val mutable private Pool: char[]"
                            "    val mutable private Chars: Span<char>"
                            "    val mutable private Pos: int"
                            "    new(literalLength: int, formattedCount: int) ="
                            "        let buf = ArrayPool<char>.Shared.Rent(Math.Max(256, literalLength + formattedCount * 11))"
                            "        { Pool = buf; Chars = Span<char>(buf); Pos = 0 }"
                            "    member this.AppendLiteral(value: string) ="
                            "        if value.TryCopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos)) then"
                            "            this.Pos <- this.Pos + value.Length"
                            "        else"
                            "            this.GrowThenCopyString(value)"
                            "    member this.AppendFormatted(value: 'T) ="
                            "        let o = box value"
                            "        let s ="
                            "            match o with"
                            "            | :? IFormattable as f -> f.ToString(null, provider)"
                            "            | null -> \"\""
                            "            | _ -> o.ToString()"
                            "        this.AppendLiteral(s)"
                            "    member this.ToStringAndClear() : string ="
                            "        let result = this.Chars.Slice(0, this.Pos).ToString()"
                            "        this.Clear()"
                            "        result"
                            "    member private this.Clear() ="
                            "        let toReturn = this.Pool"
                            "        this.Pool <- null"
                            "        this.Pos <- 0"
                            "        match toReturn with | null -> () | arr -> ArrayPool<char>.Shared.Return(arr)"
                            "    member private this.GrowThenCopyString(value: string) ="
                            "        this.Grow(value.Length)"
                            // string.CopyTo(Span) mis-resolves to the 4-param overload
                            // (overload-resolution-bug.md); TryCopyTo (the bool form)
                            // resolves and is equivalent once the buffer has grown.
                            "        let _ok = value.TryCopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos))"
                            "        this.Pos <- this.Pos + value.Length"
                            "    member private this.Grow(additionalChars: int) ="
                            "        this.GrowCore(this.Pos + additionalChars)"
                            // The real Formatter clamps growth in `uint` (PP5b) to
                            // dodge overflow at huge sizes; the `uint`→`int` array-size
                            // conversion has no recipe in the test stack
                            // (overload-resolution-bug.md), so this checkpoint uses int
                            // arithmetic — equivalent for the small grow it exercises.
                            "    member private this.GrowCore(requiredMinCapacity: int) ="
                            "        let newCapacity = Math.Max(requiredMinCapacity, Math.Min(this.Chars.Length * 2, MaxChars))"
                            "        let arraySize = Math.Max(newCapacity, MinimumArrayPoolLength)"
                            "        let newArray = ArrayPool<char>.Shared.Rent(arraySize)"
                            "        this.Chars.Slice(0, this.Pos).CopyTo(Span<char>(newArray))"
                            "        let toReturn = this.Pool"
                            "        this.Pool <- newArray"
                            "        this.Chars <- Span<char>(newArray)"
                            "        match toReturn with | null -> () | arr -> ArrayPool<char>.Shared.Return(arr)"
                            "let basic () ="
                            "    let mutable f = Handler(0, 2)"
                            "    f.AppendLiteral(\"x=\")"
                            "    f.AppendFormatted(42)"
                            "    f.AppendLiteral(\", pi=\")"
                            "    f.AppendFormatted(3.14)"
                            "    f.ToStringAndClear()"
                            "let grown () ="
                            "    let mutable f = Handler(0, 1)"
                            "    let mutable i = 0"
                            "    while i < 200 do"
                            "        f.AppendLiteral(\"ab\")"
                            "        i <- i + 1"
                            "    let s = f.ToStringAndClear()"
                            "    s.Length"
                            "printfn \"%s\" (basic())"
                            "printfn \"%d\" (grown())"
                        ])
            }

            // overload-resolution-bug.md Gap A: same-name overloaded *generic*
            // members. Each `Fmt` overload carries its own method typar `'T`;
            // `Elaborate.methodTypeParams` recovered them by *name* (`Array.tryFind`),
            // so all three overloads took the first one's typars and the others' own
            // `'T` was never generalised → froze as `?ungrounded-operator`. Fixed by
            // matching the exact overload through its registration `DeclKey`. The
            // backend emits all four `AppendFormatted` overloads of `formatter.fs`
            // this way, so this unblocks PP6.
            test "Gap A: same-name overloaded generic members each generalise their own 'T" {
                runsLines
                    [ "42" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.Globalization"
                            "[<Struct; IsByRefLike>]"
                            "type Box(seed: int) ="
                            "    static let provider : IFormatProvider = CultureInfo.InvariantCulture"
                            "    member this.Fmt(value: 'T) : string ="
                            "        let o = box value"
                            "        match o with"
                            "        | :? IFormattable as f -> f.ToString(null, provider)"
                            "        | null -> \"\""
                            "        | _ -> o.ToString()"
                            "    member this.Fmt(value: 'T, format: string) : string ="
                            "        let o = box value"
                            "        match o with"
                            "        | :? IFormattable as f -> f.ToString(format, provider)"
                            "        | null -> \"\""
                            "        | _ -> o.ToString()"
                            "    member this.Fmt(value: 'T, pad: int) : string ="
                            "        let o = box value"
                            "        match o with"
                            "        | :? IFormattable as f -> f.ToString(null, provider)"
                            "        | null -> \"\""
                            "        | _ -> o.ToString()"
                            "let b = Box(0)"
                            "printfn \"%s\" (b.Fmt 42)"
                        ])
            }

            // overload-resolution-bug.md Gap B: external *instance* method overload
            // resolution on a *variable/property* receiver. `w.Write("hi")` parses
            // with `fn = LongIdent [w; Write]` (the parser folds the dot when the
            // head is a plain ident), which bypassed the `DotLookup`-only arg-aware
            // probe and fell to the single-pick field walk — grabbing the widest
            // `Write(string, object, object, object)` overload for one `string` arg.
            // Fixed by routing the folded-LongIdent value receiver through the same
            // `pickBestOverload` path. (A *literal* receiver `"ab".CopyTo` already
            // worked — it stays a `DotLookup`.) Unblocks `formatter.fs`'s `Flush`.
            test "Gap B: external instance overload pick on a folded-LongIdent receiver" {
                runsLines [ "hi" ] (String.concat "\n" [ "open System"; "let w = Console.Out"; "w.Write(\"hi\")" ])
            }

            // overload-resolution-bug.md Gap C: binary `+` on `string` must lower
            // to `System.String.Concat`, not a numeric `add` on two string
            // references (a garbage pointer → AccessViolation). The `(+)` operator
            // (Vesper.Core/ops-platform.fs) gained a `when ^T : string` static-opt
            // clause mirroring FSharp.Core. Independent correctness bug; the
            // Formatter never uses string `+`, but it crashed an early PP5f probe.
            test "Gap C: binary + on string concatenates (String.Concat), not numeric add" {
                runsLines
                    [ "xy" ]
                    (String.concat "\n" [ "let f (a: string) (b: string) = a + b"; "printfn \"%s\" (f \"x\" \"y\")" ])
            }

            // overload-resolution-bug.md Gap D: the parameterless `Span<char>()`
            // ctor (`default(Span<char>)`). An *external* value type with 0 ctor
            // args now lowers to `initobj` (the same path a project-local struct
            // takes), not a missing `newobj` ctor recipe.
            test "Gap D: parameterless Span<char>() (external value-type default ctor)" {
                runsLines
                    [ "0" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "let g () ="
                            "    let s = Span<char>()"
                            "    s.Length"
                            "printfn \"%d\" (g())"
                        ])
            }

            // overload-resolution-bug.md Gap E: the `int` (`ToInt32`) conversion
            // *from* a `uint` — the signed sibling of PP5b's `uint`/`uint32`, now
            // in Vesper.Core/ops-platform.fs. `GrowCore` narrows its clamped `uint`
            // size back to the `int` array length.
            test "Gap E: int conversion of a uint (Math.Clamp narrowing)" {
                runsLines
                    [ "50" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "let h (x: int) = int (Math.Clamp(uint x, 0u, 100u))"
                            "printfn \"%d\" (h 50)"
                        ])
            }

            // overload-resolution-bug.md Gap F: `for i in a..b do` over an integer
            // range. Freeze lowers a unit-step range source to a counted `ForTo`
            // loop (F#'s own lowering) instead of trying to walk a non-existent
            // range enumerable (`Emit: unsupported expression: Range`).
            test "Gap F: for i in 1..n range loop (lowers to counted ForTo)" {
                runsLines
                    [ "15" ]
                    (String.concat
                        "\n"
                        [
                            "let s () ="
                            "    let mutable acc = 0"
                            "    for i in 1..5 do"
                            "        acc <- acc + i"
                            "    acc"
                            "printfn \"%d\" (s())"
                        ])
            }

            // PP7a regression guard 1: chained string concatenation. `a + b` (one
            // `+`) already lowered to `String.Concat` (overload-resolution-bug Gap C),
            // but `a + b + c` = `(a + b) + c` mis-dispatched: the *outer* `+`'s `^T`
            // was pinned (first) to the inner `+`-App's still-abstract result type, so
            // the operator failed the inline splice gate and fell to its numeric `add`
            // base — `add` on two string references (an AccessViolation, not a wrong
            // string). Fixed in `InlineExpansion.deriveInlineTypeArgs`: a ground
            // sibling operand (here `c : string`, and the `string` return position)
            // upgrades the abstract candidate so `^T` grounds to `string` and the
            // `when ^T : string` clause fires. `surround` exercises the literal-edged
            // form (`"(" + s + ")"`) the `%A` engine's parens wrapping uses.
            test "Chained string concat a + b + c lowers to String.Concat (not numeric add)" {
                runsLines
                    [ "abc"; "(x)" ]
                    (String.concat
                        "\n"
                        [
                            "let join3 (a: string) (b: string) (c: string) : string = a + b + c"
                            "let surround (s: string) : string = \"(\" + s + \")\""
                            "printfn \"%s\" (join3 \"a\" \"b\" \"c\")"
                            "printfn \"%s\" (surround \"x\")"
                        ])
            }

            // PP7a regression guard 2: escape sequences in a string-literal *value*.
            // The parser folds a `\n` / `\t` / `\"` fragment into a `StringPart.Text`
            // carrying its raw 2-char span; `FreezeLiterals.foldStringParts` used to
            // append that span verbatim, so a `"\n"` value emitted a literal
            // backslash-n. Now it decodes an escape-sequence-token fragment to the
            // char it denotes. The `%A` broken layout depends on real `"\n"` newlines.
            test "string literal escape sequences decode in the emitted value" {
                runsLines
                    [ "a"; "b"; "x\ty"; "q\"r" ]
                    (String.concat
                        "\n"
                        [
                            "let nl : string = \"a\\nb\""
                            "let tab : string = \"x\\ty\""
                            "let quo : string = \"q\\\"r\""
                            "printfn \"%s\" nl"
                            "printfn \"%s\" tab"
                            "printfn \"%s\" quo"
                        ])
            }

            // PP7a (printf-port-steps): the `%A` engine's layout core — the `Doc`
            // DU, the recursive `flatWidth`, and the recursive `Render` — ported
            // pure, with NO `IFormatSink` / dispatch (those are PP7b/PP7c). This
            // isolates the recursion / DU / string-building surface `StructuralFormat.cs`
            // needs before stitching the whole engine. Deviations from the C# (each
            // sanctioned by the plan's capability survey): `flatWidth` is recomputed
            // recursively instead of cached eagerly per node (the trees are small,
            // fully immutable); `render` accumulates functionally — it returns a
            // record `{ Txt; Col }` of the rendered text + resulting column rather
            // than appending to a `StringBuilder` (the StringBuilder probe is PP7c;
            // a record sidesteps the wide-tuple convention and the mixed ref/value
            // ValueTuple path), and the broken-line indent is built by a recursive
            // `spaces` concat rather than `Append(char, int)`. The all-or-nothing
            // group rule is the load-bearing logic: a group is flat iff
            // `col + inner.flatWidth <= width` (`width = 0` ⇒ always flat — the `%0A`
            // "never break" mode), so a composite renders all-flat or all-broken,
            // never half-broken.
            //
            // The driver hand-builds the exact `Doc` shapes `RuntimeFormatState`
            // would record for `[1; 2; 3]` (a group over text / soft-break / cat /
            // nest) and a parenthesised DU application `(Some 1)` (a `parens=true`
            // group), and asserts the laid-out string flat (width 80), never-break
            // (width 0), and broken (width 5 forces the group open, hanging the
            // elements at the nest indent).
            //
            // Two backend gaps surfaced + fixed building this probe (regression
            // guards immediately below): chained string concat `a + b + c`
            // mis-dispatched to numeric `add` on string refs (an AccessViolation) —
            // `InlineExpansion.deriveInlineTypeArgs` now lets a ground sibling
            // operand pin the operator's `^T`; and a string literal's `\n` escape
            // emitted a literal backslash-n — `FreezeLiterals.foldStringParts` now
            // decodes escape-sequence fragments. A third gap is routed around
            // (see the `listKids`/`elems` `let`-binding note below), not yet fixed.
            test "PP7a: Doc layout core — flatWidth + Render (flat / never-break / broken)" {
                runsLines
                    [
                        "[1; 2; 3]" // width 80: fits ⇒ all-flat
                        "[1; 2; 3]" // width 0: never-break ⇒ all-flat
                        "[" // width 5: group broken ⇒ all soft breaks become newlines
                        "  1;"
                        "  2;"
                        "  3"
                        "]"
                        "(Some 1)" // a parens group (DU application) renders its `(`/`)`
                    ]
                    // The DU + cases are named `LDoc`/`LText`/… (not the C# port's
                    // `Doc`/`DocText`/…) on purpose: the still-C# `StructuralFormat.cs`
                    // already defines `Vesper.Doc`/`Vesper.DocGroup`/… and the default
                    // test stack resolves `Vesper.Printf`, so a local `DocGroup(…)`
                    // binds the external C# class (no ctor recipe) instead of the
                    // local union case. PP7e drops the C# and frees the names.
                    (String.concat
                        "\n"
                        [
                            "type LDoc ="
                            "    | LText of string"
                            "    | LLine of string"
                            "    | LCat of LDoc list"
                            "    | LNest of int * LDoc"
                            "    | LGroup of LDoc * bool"
                            "let rec flatWidth (d: LDoc) : int ="
                            "    match d with"
                            "    | LText s -> s.Length"
                            "    | LLine flat -> flat.Length"
                            "    | LCat kids -> catWidth kids"
                            "    | LNest (_, inner) -> flatWidth inner"
                            "    | LGroup (inner, parens) -> flatWidth inner + (if parens then 2 else 0)"
                            "and catWidth (kids: LDoc list) : int ="
                            "    match kids with"
                            "    | [] -> 0"
                            "    | k :: rest -> flatWidth k + catWidth rest"
                            "let rec spaces (n: int) : string ="
                            "    if n <= 0 then \"\" else \" \" + spaces (n - 1)"
                            // `render` mirrors the C# `Render` — it produces the laid-out
                            // text and the resulting column. The C# threads a
                            // `StringBuilder` + an `int` return; here `render` returns a
                            // record `{ Txt; Col }`. (A `(string * int)` tuple return is
                            // the natural F# shape, but the project's wide-tuple-→-record
                            // convention applies, and a record sidesteps the mixed
                            // ref/value-field path cleanly.)
                            "type R = { Txt: string; Col: int }"
                            "let rec render (d: LDoc) (indent: int) (broken: bool) (col: int) (width: int) : R ="
                            "    match d with"
                            "    | LText s -> { Txt = s; Col = col + s.Length }"
                            "    | LLine flat ->"
                            "        if broken then { Txt = \"\\n\" + spaces indent; Col = indent }"
                            "        else { Txt = flat; Col = col + flat.Length }"
                            "    | LNest (i, inner) -> render inner (indent + i) broken col width"
                            "    | LCat kids -> renderCat kids indent broken col width"
                            "    | LGroup (inner, parens) ->"
                            "        let openCol = if parens then col + 1 else col"
                            "        let groupBroken = width <> 0 && openCol + flatWidth inner > width"
                            "        let r = render inner indent groupBroken openCol width"
                            "        if parens then { Txt = \"(\" + r.Txt + \")\"; Col = r.Col + 1 }"
                            "        else r"
                            "and renderCat (kids: LDoc list) (indent: int) (broken: bool) (col: int) (width: int) : R ="
                            "    match kids with"
                            "    | [] -> { Txt = \"\"; Col = col }"
                            "    | k :: rest ->"
                            "        let r1 = render k indent broken col width"
                            "        let r2 = renderCat rest indent broken r1.Col width"
                            "        { Txt = r1.Txt + r2.Txt; Col = r2.Col }"
                            "let layout (d: LDoc) (width: int) : string ="
                            "    let r = render (LGroup(d, false)) 0 false 0 width"
                            "    r.Txt"
                            // List literals are now passed *directly* as union-case
                            // arguments (`LCat [ … ]`, even nested): the gap #3 that
                            // mis-lowered such a cons-list to empty is fixed (see
                            // docs/printf-port-steps.md PP7a; `peelCtorArgs` only
                            // collapses round-paren grouping, never a `[ … ]` literal).
                            "let listDoc = LGroup(LCat [ LText \"[\"; LNest(2, LCat [ LLine \"\"; LText \"1\"; LText \";\"; LLine \" \"; LText \"2\"; LText \";\"; LLine \" \"; LText \"3\" ]); LLine \"\"; LText \"]\" ], false)"
                            "let appDoc = LGroup(LCat [ LText \"Some\"; LLine \" \"; LText \"1\" ], true)"
                            "printfn \"%s\" (layout listDoc 80)"
                            "printfn \"%s\" (layout listDoc 0)"
                            "printfn \"%s\" (layout listDoc 5)"
                            "printfn \"%s\" (layout appDoc 80)"
                        ])
            }

            // PP7a gap #3 (now FIXED): a cons-list *literal* passed DIRECTLY as a
            // union-case argument — `LCat [ a; b; c ]` — used to lower the list to
            // empty (so a `Doc list` field read back `[]` and folds over it
            // returned 0). The cause: `peelCtorArgs`/`peelOneArg` matched the
            // `[ … ]` literal's `EnclosedBlock` and unwrapped it to its inner
            // `Sequential`, dropping the `ParenKind.List` literal lowering. Fixed
            // by only collapsing round-paren / begin-end grouping (`ValueParen`)
            // into the argument list. The probe above let-binds every list to dodge
            // this; here we pass the literal inline and assert the field round-trips.
            test "PP7a gap #3: list literal as a direct union-case argument round-trips" {
                runsLines
                    [ "3"; "6" ]
                    (String.concat
                        "\n"
                        [
                            "type Bag = | Items of int list"
                            "let rec sumList (xs: int list) : int ="
                            "    match xs with"
                            "    | [] -> 0"
                            "    | h :: t -> h + sumList t"
                            "let count (b: Bag) : int ="
                            "    match b with"
                            "    | Items xs -> sumList xs"
                            // inline literal as the sole ctor arg (the formerly-broken shape)
                            "let direct = Items [ 1; 1; 1 ]"
                            "let total = Items [ 1; 2; 3 ]"
                            "printfn \"%d\" (count direct)"
                            "printfn \"%d\" (count total)"
                        ])
            }

            // PP7b (printf-port-steps): `RuntimeFormatState : IFormatSink` — the
            // concrete sink + frame stack, the second StructuralFormat.cs rung. This
            // probe stitches the PP7a layout core (the `LDoc` DU + `flatWidth` +
            // `render`/`layout`) to a *class* `LSink` implementing the Core-owned
            // `Vesper.IFormatSink` — all 11 members — plus the frame-stack
            // push / `PopWrap` (collapse a frame's children to a single child or a
            // `LCat`, wrap in `LGroup`/`LNest`) and `Finish`. It exercises every
            // sink capability PP7b owns:
            //   * a reference class implementing an external Core interface, members
            //     forwarding to private helpers (the forwarding the struct enumerator
            //     CANNOT do — but a *class* `this` is a reference, so the mutation is
            //     not lost on a copy);
            //   * `val mutable` fields holding a frame stack as a Vesper cons-list
            //     used as a stack (push = cons, pop = head/tail; `PopWrap` reverses
            //     the per-frame child accumulator) — the plan's recommended deviation
            //     over a BCL `List<Doc>`, avoiding mutable-collection support;
            //   * the `_argPending` parens propagation (`FormatArg` → the next
            //     `BeginApplication` parenthesizes; `FormatChild` clears it);
            //   * a minimal `Dispatch` (`:? IStructuralFormattable` recurse + a
            //     `ToString` fallback for leaves) — enough to drive `FormatChild` /
            //     `FormatArg` recursion. The full type-test/atom surface (numeric
            //     suffixes, quoting, `ITuple`, `IEnumerable`) is PP7c.
            //
            // The drivers are *classes* (`MyList` / `MyOpt`), not DUs: a hand-written
            // union interface impl is unsupported front-to-back
            // (project_union_interface_impls_unsupported); `RuntimeFormatState` is a
            // class for the same reason, and these mirror it. `MyList` records the
            // `[a; b; c]` enumerable shape and `MyOpt` the `Some payload`
            // application — together they drive group / nest / soft-break / the
            // parenthesised application, asserted flat (80), broken (5), and nested
            // (`Some (Some 1)`, the inner `Some` parenthesised because it is in
            // argument position).
            test "PP7b: RuntimeFormatState : IFormatSink — sink + frame stack" {
                runsSelfHostLines
                    [
                        "[1; 2; 3]" // width 80: the list group fits ⇒ flat
                        "[" // width 5: the group breaks ⇒ soft breaks become newlines
                        "  1;"
                        "  2;"
                        "  3"
                        "]"
                        "Some 1" // top-level application: not parenthesised
                        "Some (Some 1)" // the inner Some is in arg position ⇒ parens
                    ]
                    (String.concat
                        "\n"
                        [
                            // ---- PP7a layout core (LDoc + flatWidth + render + layout) ----
                            "type LDoc ="
                            "    | LText of string"
                            "    | LLine of string"
                            "    | LCat of LDoc list"
                            "    | LNest of int * LDoc"
                            "    | LGroup of LDoc * bool"
                            "let rec flatWidth (d: LDoc) : int ="
                            "    match d with"
                            "    | LText s -> s.Length"
                            "    | LLine flat -> flat.Length"
                            "    | LCat kids -> catWidth kids"
                            "    | LNest (_, inner) -> flatWidth inner"
                            "    | LGroup (inner, parens) -> flatWidth inner + (if parens then 2 else 0)"
                            "and catWidth (kids: LDoc list) : int ="
                            "    match kids with"
                            "    | [] -> 0"
                            "    | k :: rest -> flatWidth k + catWidth rest"
                            "let rec spaces (n: int) : string ="
                            "    if n <= 0 then \"\" else \" \" + spaces (n - 1)"
                            "type R = { Txt: string; Col: int }"
                            "let rec render (d: LDoc) (indent: int) (broken: bool) (col: int) (width: int) : R ="
                            "    match d with"
                            "    | LText s -> { Txt = s; Col = col + s.Length }"
                            "    | LLine flat ->"
                            "        if broken then { Txt = \"\\n\" + spaces indent; Col = indent }"
                            "        else { Txt = flat; Col = col + flat.Length }"
                            "    | LNest (i, inner) -> render inner (indent + i) broken col width"
                            "    | LCat kids -> renderCat kids indent broken col width"
                            "    | LGroup (inner, parens) ->"
                            "        let openCol = if parens then col + 1 else col"
                            "        let groupBroken = width <> 0 && openCol + flatWidth inner > width"
                            "        let r = render inner indent groupBroken openCol width"
                            "        if parens then { Txt = \"(\" + r.Txt + \")\"; Col = r.Col + 1 }"
                            "        else r"
                            "and renderCat (kids: LDoc list) (indent: int) (broken: bool) (col: int) (width: int) : R ="
                            "    match kids with"
                            "    | [] -> { Txt = \"\"; Col = col }"
                            "    | k :: rest ->"
                            "        let r1 = render k indent broken col width"
                            "        let r2 = renderCat rest indent broken r1.Col width"
                            "        { Txt = r1.Txt + r2.Txt; Col = r2.Col }"
                            "let layout (d: LDoc) (width: int) : string ="
                            "    let r = render (LGroup(d, false)) 0 false 0 width"
                            "    r.Txt"
                            // Reverse a frame's child accumulator (built by consing, so
                            // reversed) back into recording order. Hand-written to avoid a
                            // dependency on a `List.rev` external static.
                            "let rec revOnto (xs: LDoc list) (acc: LDoc list) : LDoc list ="
                            "    match xs with"
                            "    | [] -> acc"
                            "    | h :: t -> revOnto t (h :: acc)"
                            // ---- the sink + frame stack (RuntimeFormatState) ----
                            "type FrameKind ="
                            "    | Root"
                            "    | Group"
                            "    | Nest"
                            "    | Application"
                            // A layout frame: its child accumulator (`Kids`, consed ⇒
                            // reversed) plus the scope kind / nest indent / parens flag.
                            "type Frame ="
                            "    val Kind: FrameKind"
                            "    val NestIndent: int"
                            "    val Parens: bool"
                            "    val mutable Kids: LDoc list"
                            "    new(kind: FrameKind, nestIndent: int, parens: bool) ="
                            "        { Kind = kind; NestIndent = nestIndent; Parens = parens; Kids = [] }"
                            "type LSink ="
                            "    val Width: int"
                            "    val mutable Frames: Frame list"
                            "    val mutable ArgPending: bool"
                            "    new(width: int) ="
                            "        let root = Frame(Root, 0, false)"
                            "        { Width = width; Frames = [ root ]; ArgPending = false }"
                            "    member private this.Add(d: LDoc) ="
                            "        match this.Frames with"
                            "        | top :: _ -> top.Kids <- d :: top.Kids"
                            "        | [] -> ()"
                            "    member private this.Push(f: Frame) = this.Frames <- f :: this.Frames"
                            "    member private this.PopWrap(expected: FrameKind) ="
                            "        match this.Frames with"
                            "        | f :: rest ->"
                            "            this.Frames <- rest"
                            "            let kids = revOnto f.Kids []"
                            "            let inner ="
                            "                match kids with"
                            "                | [ single ] -> single"
                            "                | _ -> LCat kids"
                            "            let wrapped ="
                            "                match f.Kind with"
                            "                | Group -> LGroup(inner, false)"
                            "                | Application -> LGroup(inner, f.Parens)"
                            "                | Nest -> LNest(f.NestIndent, inner)"
                            "                | Root -> inner"
                            "            this.Add(wrapped)"
                            "        | [] -> ()"
                            "    member private this.Dispatch(value: obj) ="
                            "        match value with"
                            "        | :? Vesper.IStructuralFormattable as s -> s.Format(this :> Vesper.IFormatSink)"
                            "        | null -> this.Add(LText \"null\")"
                            "        | _ -> this.Add(LText(value.ToString()))"
                            "    member this.Finish() : string ="
                            "        match this.Frames with"
                            "        | [ root ] ->"
                            "            let kids = revOnto root.Kids []"
                            "            let docRoot ="
                            "                match kids with"
                            "                | [ single ] -> single"
                            "                | _ -> LCat kids"
                            "            layout docRoot this.Width"
                            "        | _ -> failwith \"Vesper.LSink: unbalanced layout scopes at Finish.\""
                            "    interface Vesper.IFormatSink with"
                            "        member this.Text(s: string) = this.Add(LText s)"
                            "        member this.Line() = this.Add(LLine \" \")"
                            "        member this.SoftBreak() = this.Add(LLine \"\")"
                            "        member this.BeginGroup() = this.Push(Frame(Group, 0, false))"
                            "        member this.EndGroup() = this.PopWrap(Group)"
                            "        member this.BeginNest(indent: int) = this.Push(Frame(Nest, indent, false))"
                            "        member this.EndNest() = this.PopWrap(Nest)"
                            "        member this.BeginApplication() ="
                            "            let parens = this.ArgPending"
                            "            this.ArgPending <- false"
                            "            this.Push(Frame(Application, 0, parens))"
                            "        member this.EndApplication() = this.PopWrap(Application)"
                            "        member this.FormatChild(value: obj) ="
                            "            this.ArgPending <- false"
                            "            this.Dispatch(value)"
                            "        member this.FormatArg(value: obj) ="
                            "            this.ArgPending <- true"
                            "            this.Dispatch(value)"
                            "            this.ArgPending <- false"
                            // ---- the drivers (hand-written IStructuralFormattable on CLASSES) ----
                            "let rec emitElems (sink: Vesper.IFormatSink) (xs: int list) (first: bool) : unit ="
                            "    match xs with"
                            "    | [] -> ()"
                            "    | h :: t ->"
                            "        if not first then"
                            "            sink.Text \";\""
                            "            sink.Line()"
                            "        sink.FormatChild(box h)"
                            "        emitElems sink t false"
                            "type MyList(xs: int list) ="
                            "    interface Vesper.IStructuralFormattable with"
                            "        member this.Format(sink: Vesper.IFormatSink) ="
                            "            sink.BeginGroup()"
                            "            sink.Text \"[\""
                            "            sink.BeginNest 2"
                            "            sink.SoftBreak()"
                            "            emitElems sink xs true"
                            "            sink.EndNest()"
                            "            sink.SoftBreak()"
                            "            sink.Text \"]\""
                            "            sink.EndGroup()"
                            "type MyOpt(payload: obj, isSome: bool) ="
                            "    interface Vesper.IStructuralFormattable with"
                            "        member this.Format(sink: Vesper.IFormatSink) ="
                            "            if isSome then"
                            "                sink.BeginApplication()"
                            "                sink.Text \"Some \""
                            "                sink.FormatArg payload"
                            "                sink.EndApplication()"
                            "            else"
                            "                sink.Text \"None\""
                            "let print (v: obj) (width: int) : string ="
                            "    let sink = LSink(width)"
                            "    (sink :> Vesper.IFormatSink).FormatChild(v)"
                            "    sink.Finish()"
                            "printfn \"%s\" (print (MyList([ 1; 2; 3 ]) :> obj) 80)"
                            "printfn \"%s\" (print (MyList([ 1; 2; 3 ]) :> obj) 5)"
                            "printfn \"%s\" (print (MyOpt(box 1, true) :> obj) 80)"
                            "printfn \"%s\" (print (MyOpt((MyOpt(box 1, true) :> obj), true) :> obj) 80)"
                        ])
            }

            // A `val`-field reference class whose only constructor is a parameterless
            // `new() = { … }` (the form `RuntimeFormatState` would take if it dropped
            // its width/size params). The backend used to synthesise an empty primary
            // `.ctor()` ALONGSIDE the `new()` — two identical `.ctor()` rows — and
            // construction (`S()`) bound the empty one, leaving every field
            // uninitialised. Fixed: the val-field form (parser `pcOpt = ValueNone`)
            // emits NO synthesised primary; the secondaries are the only ctors and
            // `EmitConstruct.buildNew` resolves `S()` to the `new()` secondary.
            test "a val-field class with only a parameterless new() initialises its fields" {
                runsLines
                    [ "42"; "7" ]
                    (String.concat
                        "\n"
                        [
                            "type S ="
                            "    val mutable X: int"
                            "    val mutable Y: int"
                            "    new() = { X = 42; Y = 7 }"
                            "    member this.Sum = this.X + this.Y"
                            "let s = S()"
                            "printfn \"%d\" s.X"
                            "printfn \"%d\" s.Y"
                        ])
            }
        ]
