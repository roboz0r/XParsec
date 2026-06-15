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
        ]
