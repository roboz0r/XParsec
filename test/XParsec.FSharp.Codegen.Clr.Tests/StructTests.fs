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

// ---- shared embedded-source fragments for the self-hosted `IFormatSink` tests ----
// `PP7b` and `PP7c` below each self-host a hand-written `LSink : IFormatSink` and
// assert its `%A`-style output. The layout engine, the frame / semantic-frame types,
// the frame-stack plumbing, and — load-bearing — the semantic `BeginCase`/`Child`/
// `EndCase` protocol are byte-identical between the two sinks, so they live here once.
// Each test splices in only its own `LSink` header + ctor, its `Dispatch` variant, and
// its drivers. Keeping the protocol single-sourced means a sink-protocol change edits
// one fragment, not two hand-copied blobs.

/// `LDoc` + the width-driven layout engine (`flatWidth`/`render`/`layout`) and the
/// `revOnto` frame-child un-reverser (children accumulate by consing; this flips
/// them back to recording order without a `List.rev` dependency).
let private lLayoutCore: string list =
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
        "let rec revOnto (xs: LDoc list) (acc: LDoc list) : LDoc list ="
        "    match xs with"
        "    | [] -> acc"
        "    | h :: t -> revOnto t (h :: acc)"
    ]

/// `FrameKind` / `Frame` / `SemFrame` declarations. A `SemFrame`'s payload `Count`
/// fixes the union-case arity at `EndCase`; `ChildAppShaped` carries the one bit the
/// count cannot settle — whether a lone payload is itself application-shaped, so it
/// parenthesises (`Some (Some 1)` but not `Some 1`).
let private lFrameSemTypes: string list =
    [
        "type FrameKind ="
        "    | Root"
        "    | Group"
        "    | Nest"
        "    | CaseCollect"
        "type Frame ="
        "    val Kind: FrameKind"
        "    val NestIndent: int"
        "    val mutable Kids: LDoc list"
        "    new(kind: FrameKind, nestIndent: int) ="
        "        { Kind = kind; NestIndent = nestIndent; Kids = [] }"
        "type SemFrame ="
        "    val Name: string"
        "    val mutable Count: int"
        "    val mutable ChildAppShaped: bool"
        "    new(name: string) = { Name = name; Count = 0; ChildAppShaped = false }"
    ]

/// `LSink.Add` / `.Push` / `.PopWrap` — the frame-stack plumbing shared by both sinks.
/// `CaseCollect` (like `Root`) unwraps bare here; `EndCase` pops it by hand.
let private lSinkFramePlumbing: string list =
    [
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
        "                | Nest -> LNest(f.NestIndent, inner)"
        "                | Root -> inner"
        "                | CaseCollect -> inner"
        "            this.Add(wrapped)"
        "        | [] -> ()"
    ]

/// `LSink.Finish` + the full `IFormatSink` interface impl, including the semantic
/// `BeginCase`/`Child`/`EndCase` protocol both sinks exercise. Record ops are no-op
/// stubs — only union cases drive these sinks (`MyOpt`).
let private lSinkFinishAndProtocol: string list =
    [
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
        "        member this.BeginGroup() = this.Push(Frame(Group, 0))"
        "        member this.EndGroup() = this.PopWrap(Group)"
        "        member this.BeginNest(indent: int) = this.Push(Frame(Nest, indent))"
        "        member this.EndNest() = this.PopWrap(Nest)"
        "        member this.BeginRecord() = ()"
        "        member this.Field(name: string) = ()"
        "        member this.EndRecord() = ()"
        "        member this.BeginCase(name: string) ="
        "            this.SemFrames <- SemFrame(name) :: this.SemFrames"
        "            this.Push(Frame(CaseCollect, 0))"
        "        member this.Child(value: obj) ="
        "            this.Dispatch(value)"
        "            match this.SemFrames with"
        "            | sf :: _ ->"
        "                sf.Count <- sf.Count + 1"
        "                sf.ChildAppShaped <- this.LastAppShaped"
        "            | [] -> ()"
        "        member this.EndCase() ="
        "            match this.SemFrames with"
        "            | cf :: rest ->"
        "                this.SemFrames <- rest"
        "                let kids ="
        "                    match this.Frames with"
        "                    | f :: fr ->"
        "                        this.Frames <- fr"
        "                        revOnto f.Kids []"
        "                    | [] -> []"
        "                let caseDoc ="
        "                    if cf.Count = 0 then LText cf.Name"
        "                    else"
        "                        let child ="
        "                            match kids with"
        "                            | [ single ] -> single"
        "                            | _ -> LCat kids"
        "                        let payload ="
        "                            if cf.ChildAppShaped then LGroup(child, true) else child"
        "                        LGroup(LCat [ LText(cf.Name + \" \"); payload ], false)"
        "                this.Add(caseDoc)"
        "                this.LastAppShaped <- cf.Count >= 1"
        "            | [] -> ()"
    ]

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

            // A parameterless struct construction lowers to `ldloca; initobj; ldloc`
            // on a scratch local, not a `newobj` against the synthesised parameterless
            // `.ctor`. The field reads back as its zero-init default.
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

            // A secondary ctor of the explicit field-init form `new(args) = { f = e; … }`.
            // Unlike a chain-form `new`, it stores directly into `val` fields (no primary-`.ctor` chain).
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

            // An immutable `val x: T` (no `mutable`) emits as `InitOnly`.
            // `stfld` in a ctor is legal on InitOnly; writes elsewhere are forbidden.
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

            // A generic value type's self-`TypeSpec` (base type, ctor field `MemberRef`s,
            // signature encoding) must carry the VALUETYPE tag throughout.
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

            // A class `GetEnumerator()` constructs the struct enumerator in-method
            // and returns it `:>`-upcast (boxed) to the interface.
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

            // An explicit type application at the construction site (`OnceEnum<'T>(x)`):
            // the secondary-ctor type args must ground from both the explicit `<'T>` and
            // the value arg, not leak as a free `TyVar`.
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

            // A struct declared in a referenced package: the consumer must encode
            // `ELEMENT_TYPE_VALUETYPE` (0x11) not `CLASS` (0x12) — a wrong tag faults
            // the loader. The package directory name IS the package identity
            // (`buildClosure` resolves `depends-on` against it), so it must equal
            // the manifest `name` — hence `Vesper.PointPkg`, not a descriptive slug.
            test "a struct declared in a referenced package encodes as VALUETYPE in a consumer signature" {
                let outDir = tmpDir "Vesper.PointPkg"
                let manifestPath = System.IO.Path.Combine(outDir, "manifest.toml")
                let fsiPath = System.IO.Path.Combine(outDir, "point.fsi")

                System.IO.File.WriteAllText(
                    manifestPath,
                    "[core]\nname = \"Vesper.PointPkg\"\nnamespace = \"Vesper\"\ndepends-on = []\nfiles = [\"point.fsi\"]\n"
                )

                System.IO.File.WriteAllText(
                    fsiPath,
                    "namespace Vesper\n\ntype Point =\n    struct\n        val X: int\n        val Y: int\n    end\n"
                )

                let provider = ClrSymbolProviders.buildContract [ vesperCoreManifest; manifestPath ]

                // Identity function forces `Point` into the emitted signature (return + param).
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

                let declType =
                    match peMethodNames bytes |> List.filter (fun (_, m) -> m = "echo") with
                    | (t, _) :: _ -> t
                    | [] -> failwithf "no `echo` method emitted; methods: %A" (peMethodNames bytes)

                let elem = peMethodReturnElementType bytes declType "echo"

                // ELEMENT_TYPE_VALUETYPE = 0x11, ELEMENT_TYPE_CLASS = 0x12.
                Expect.notEqual elem 0x12uy "the referenced struct must NOT encode as ELEMENT_TYPE_CLASS"
                Expect.equal elem 0x11uy "the referenced struct encodes as ELEMENT_TYPE_VALUETYPE"
            }

            // An infix operator inside an `interface … with member …` body: desugar
            // must walk interface member bodies, not only the type's own members,
            // or the operator gets no `DesugaredForm.OpName` entry and Freeze throws.
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

            // A chained property access `this.field.Prop` where `field` is a
            // `val`/ctor-param instance field: `recoverFieldStepTy` must scan val
            // fields (not just members) when resolving intermediate chain types,
            // otherwise the receiver gets the final property's type instead of the
            // field's type.
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

            // `[<Struct; IsByRefLike>]` emits the `IsByRefLikeAttribute` marker so
            // the CLR confines the type to the stack. The runtime surfaces this as
            // `Type.IsByRefLike`.
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

            // The marker is opt-in: `IsByRefLikeAttribute` must not leak onto plain `[<Struct>]` types.
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

            // An external generic value type (`Span<char>`): ctor/member refs must be
            // tagged `VALUETYPE` not `CLASS`, and dispatch must be address-based
            // (`ldloca` + non-virtual `call`) — a by-value `callvirt` is verifier-illegal
            // on a ref struct.
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

            // `Span<T>`'s element accessor is `get_Item(i) : T&` (byref return, no
            // by-value form). This exercises the full byref stack: resolving
            // `get_Item` as carrying `FTConst("&",[elem])`, encoding
            // `ELEMENT_TYPE_BYREF` in the member-ref, and dereferencing via `ldobj`.
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

            // `ArrayPool<char>.Shared.Return` omits its optional `clearArray = false`
            // trailing parameter. The provider surfaces `OptionalDefaults`; Freeze
            // synthesises the omitted constant so codegen sees the full call.
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

            // The `null` literal pattern binds nothing and lowers to a non-null test
            // (`ldloc; brtrue` skips the arm), so a null scrutinee falls to the body
            // and any other value to the next arm.
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

            // `Span<char>` passed as a by-value argument: the member-ref parent for a
            // Span parameter must encode `VALUETYPE`, not just the receiver/field/return.
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

            // A project-local generic member called at multiple distinct types within
            // the same assembly: method typars must be freshened per call site
            // (`Engine.instantiateMemberCall`), not grounded to the first call's type.
            // Without freshening the member emits as a mono method and the second
            // call passes a wrong-typed arg (`InvalidProgramException` at JIT).
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
                            "        | :? IFormattable as f -> f.ToString(null, CultureInfo.InvariantCulture)"
                            "        | null -> \"\""
                            "        | _ -> o.ToString()"
                            "let h = Holder(0)"
                            "printfn \"%s\" (h.AppendFormatted 42)"
                            "printfn \"%s\" (h.AppendFormatted 3.14)"
                            "printfn \"%s\" (h.AppendFormatted \"hi\")"
                        ])
            }

            // Same typar-freshening invariant at module scope: a generic free function
            // (capturing nothing) called at two distinct types must stay generic,
            // not ground to the first call.
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

            // `&local` (managed address-of) lowers to `ldloca` of the operand's slot;
            // the member-ref encodes the parameter with `ELEMENT_TYPE_BYREF`. The local
            // must be a function-local mutable — a module-level mutable is a static
            // field, not a slot-addressable local.
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

            // `ISpanFormattable.TryFormat` writes formatted chars straight into a
            // `Span<char>` buffer (no intermediate string), reading the count back
            // through the `&cw` out arg. `int`/`float` implement `ISpanFormattable`;
            // `string` falls to the `o.ToString()` arm.
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

            // Every body mutates `this` through self-calls (`AppendFormatted` →
            // `AppendLiteral` → `GrowThenCopyString` → `Grow` → `GrowCore`). These
            // only persist because struct self-calls address `this` in place
            // (`EmitMember.loadStructReceiverAddr`); a defensive copy per call would
            // lose each mutation.
            test "PP5f: Formatter core — literal, generic hole, grow, string sink" {
                runsLines
                    [ "x=42, pi=3.14"; "400" ]
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
                            // `string.CopyTo(Span)` mis-resolves to the 4-param overload
                            // at this call site; `TryCopyTo` (bool form) resolves correctly
                            // and is equivalent once the buffer has grown.
                            "        let _ok = value.TryCopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos))"
                            "        this.Pos <- this.Pos + value.Length"
                            "    member private this.Grow(additionalChars: int) ="
                            "        this.GrowCore(this.Pos + additionalChars)"
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

            // Same-name overloaded generic members: each overload carries its own
            // method typar `'T`. Elaboration must match the exact overload by `DeclKey`,
            // not by name — otherwise all overloads share the first one's typars and
            // the others' `'T` freezes as `?ungrounded-operator`.
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

            // External instance method overload resolution on a variable/property
            // receiver: `w.Write("hi")` parses with `fn = LongIdent [w; Write]`
            // (the parser folds the dot). This must go through `pickBestOverload`,
            // not the single-pick field walk (which would grab the widest overload).
            test "Gap B: external instance overload pick on a folded-LongIdent receiver" {
                runsLines [ "hi" ] (String.concat "\n" [ "open System"; "let w = Console.Out"; "w.Write(\"hi\")" ])
            }

            // Binary `+` on `string` must lower to `String.Concat`, not a numeric
            // `add` on two string references (a garbage pointer → AccessViolation).
            test "Gap C: binary + on string concatenates (String.Concat), not numeric add" {
                runsLines
                    [ "xy" ]
                    (String.concat "\n" [ "let f (a: string) (b: string) = a + b"; "printfn \"%s\" (f \"x\" \"y\")" ])
            }

            // An external value type with 0 ctor args lowers to `initobj`, not `newobj`.
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

            // `for i in a..b do` over an integer range lowers to a counted `ForTo`
            // loop, not a range enumerable walk.
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

            // `a + b + c` = `(a + b) + c`: the outer `+`'s `^T` must be grounded by
            // a sibling ground operand (`c : string` or the `string` return position),
            // not pinned to the inner App's still-abstract result type. Without this,
            // the operator falls to its numeric `add` base — `add` on string refs is
            // an AccessViolation.
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

            // String escape sequences must decode to the char they denote: `FreezeLiterals`
            // must not append the raw 2-char span verbatim.
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

            // The `%A` layout core: a group is all-flat iff
            // `col + inner.flatWidth <= width` (`width = 0` ⇒ always flat), never
            // half-broken. `render` returns a record `{ Txt; Col }` rather than
            // threading a `StringBuilder` (avoids mixed ref/value-field path).
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
                    // The DU cases are named `LDoc`/`LText`/… to avoid clashing with the
                    // external C# `Vesper.Doc`/`Vesper.DocGroup`/… already in scope from
                    // the default test stack; a local `DocGroup(…)` would otherwise bind
                    // the external class (no ctor recipe) instead of the local union case.
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
                            "let listDoc = LGroup(LCat [ LText \"[\"; LNest(2, LCat [ LLine \"\"; LText \"1\"; LText \";\"; LLine \" \"; LText \"2\"; LText \";\"; LLine \" \"; LText \"3\" ]); LLine \"\"; LText \"]\" ], false)"
                            "let appDoc = LGroup(LCat [ LText \"Some\"; LLine \" \"; LText \"1\" ], true)"
                            "printfn \"%s\" (layout listDoc 80)"
                            "printfn \"%s\" (layout listDoc 0)"
                            "printfn \"%s\" (layout listDoc 5)"
                            "printfn \"%s\" (layout appDoc 80)"
                        ])
            }

            // A list literal passed directly as a union-case argument `LCat [ a; b; c ]`:
            // `peelCtorArgs` must only collapse round-paren/begin-end grouping, never
            // unwrap a `[ … ]` literal's `EnclosedBlock` (which would drop the list lowering).
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
                            "let direct = Items [ 1; 1; 1 ]"
                            "let total = Items [ 1; 2; 3 ]"
                            "printfn \"%d\" (count direct)"
                            "printfn \"%d\" (count total)"
                        ])
            }

            // `IFormatSink` class sink + frame stack: a union case drives the semantic
            // protocol (`BeginCase`/`Child`/`EndCase`); the sink decides the
            // single-payload parenthesisation from the child's application-shapedness
            // (`Some (Some 1)` but not `Some 1`). Drivers are classes (`MyList`/`MyOpt`)
            // because union interface impls are unsupported front-to-back.

            // Overloaded instance members on a struct, where one overload self-calls
            // another: `Members` must key to an overload list and pick by argument
            // types at the call site (ECMA-335 §I.10.2). Keying by name only makes
            // every call resolve to the first overload's handle, causing
            // `InvalidProgramException` when the arity or types mismatch.
            test "PP7f: overloaded struct self-call resolves the right overload by arg types" {
                runsSelfHostLines
                    [ "2" ]
                    (String.concat
                        "\n"
                        [
                            "[<Struct; IsByRefLike>]"
                            "type S ="
                            "    val mutable Acc: int"
                            "    new(a: int) = { Acc = a }"
                            "    member this.G(x: 'T) : unit = this.Acc <- this.Acc + 100"
                            "    member this.G(x: 'T, k: string) : unit = this.Acc <- this.Acc + 10"
                            "    member this.G(x: 'T, k: int) : unit = this.Acc <- this.Acc + 1"
                            "    member this.U(y: int) : unit = this.G(y, 0)"
                            "let run () ="
                            "    let mutable s = S(0)"
                            "    s.U 5"
                            "    s.U 5"
                            "    s.Acc"
                            "printfn \"%d\" (run())"
                        ])
            }

            test "PP7f: %u / aligned / zero-padded-float holes match the C# handler" {
                runsDifferentialEq "42" "printfn \"%u\" 42"
                runsDifferentialEq "   42" "printfn \"%5u\" 42"
                runsDifferentialEq "00003.14" "printfn \"%08.2f\" 3.14"
                runsDifferentialEq "42   ,7" "printfn \"%-5u,%d\" 42 7"
            }

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
                        (lLayoutCore
                         @ lFrameSemTypes
                         @ [
                             // This sink omits the depth/size budget (unexercised here); its
                             // `Dispatch` is a bare three-arm switch, unlike PP7c's engine.
                             "type LSink ="
                             "    val Width: int"
                             "    val mutable Frames: Frame list"
                             "    val mutable SemFrames: SemFrame list"
                             "    val mutable LastAppShaped: bool"
                             "    new(width: int) ="
                             "        let root = Frame(Root, 0)"
                             "        { Width = width; Frames = [ root ]; SemFrames = []; LastAppShaped = false }"
                         ]
                         @ lSinkFramePlumbing
                         @ [
                             "    member private this.Dispatch(value: obj) ="
                             "        this.LastAppShaped <- false"
                             "        match value with"
                             "        | :? Vesper.IStructuralFormattable as s -> s.Format(this :> Vesper.IFormatSink)"
                             "        | null -> this.Add(LText \"null\")"
                             "        | _ -> this.Add(LText(value.ToString()))"
                         ]
                         @ lSinkFinishAndProtocol
                         @ [
                             "let rec emitElems (sink: Vesper.IFormatSink) (xs: int list) (first: bool) : unit ="
                             "    match xs with"
                             "    | [] -> ()"
                             "    | h :: t ->"
                             "        if not first then"
                             "            sink.Text \";\""
                             "            sink.Line()"
                             "        sink.Child(box h)"
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
                             "                sink.BeginCase \"Some\""
                             "                sink.Child payload"
                             "                sink.EndCase()"
                             "            else"
                             "                sink.BeginCase \"None\""
                             "                sink.EndCase()"
                             "let print (v: obj) (width: int) : string ="
                             "    let sink = LSink(width)"
                             "    (sink :> Vesper.IFormatSink).Child(v)"
                             "    sink.Finish()"
                             "printfn \"%s\" (print (MyList([ 1; 2; 3 ]) :> obj) 80)"
                             "printfn \"%s\" (print (MyList([ 1; 2; 3 ]) :> obj) 5)"
                             "printfn \"%s\" (print (MyOpt(box 1, true) :> obj) 80)"
                             "printfn \"%s\" (print (MyOpt((MyOpt(box 1, true) :> obj), true) :> obj) 80)"
                         ]))
            }

            test "PP7c probe: numeric type-tests + suffix render" {
                runsSelfHostLines
                    [ "5y"; "5uy"; "5s"; "5us"; "5u"; "5L"; "5UL"; "5M" ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.Globalization"
                            "let provider : IFormatProvider = CultureInfo.InvariantCulture"
                            "let formatPrimitive (value: obj) : string ="
                            "    let s ="
                            "        match value with"
                            "        | :? IFormattable as f -> f.ToString(null, provider)"
                            "        | _ -> value.ToString()"
                            "    let suffix ="
                            "        match value with"
                            "        | :? sbyte -> \"y\""
                            "        | :? byte -> \"uy\""
                            "        | :? int16 -> \"s\""
                            "        | :? uint16 -> \"us\""
                            "        | :? uint32 -> \"u\""
                            "        | :? int64 -> \"L\""
                            "        | :? uint64 -> \"UL\""
                            "        | :? decimal -> \"M\""
                            "        | _ -> \"\""
                            "    s + suffix"
                            "printfn \"%s\" (formatPrimitive (box 5y))"
                            "printfn \"%s\" (formatPrimitive (box 5uy))"
                            "printfn \"%s\" (formatPrimitive (box 5s))"
                            "printfn \"%s\" (formatPrimitive (box 5us))"
                            "printfn \"%s\" (formatPrimitive (box 5u))"
                            "printfn \"%s\" (formatPrimitive (box 5L))"
                            "printfn \"%s\" (formatPrimitive (box 5UL))"
                            "printfn \"%s\" (formatPrimitive (box 5M))"
                        ])
            }

            test "PP7c probe: float fixup + string/char quoting" {
                runsSelfHostLines
                    [
                        "3.0"
                        "3.5"
                        "nan"
                        "3.0f"
                        "nanf"
                        "infinityf"
                        "\"a\\\"b\\nc\""
                        "'c'"
                        "'\\n'"
                    ]
                    (String.concat
                        "\n"
                        [
                            "open System"
                            "open System.Globalization"
                            "let provider : IFormatProvider = CultureInfo.InvariantCulture"
                            // Scan by index: no char enumerator available.
                            "let rec hasDot (s: string) (i: int) : bool ="
                            "    if i >= s.Length then false"
                            "    else"
                            "        let c = s.[i]"
                            "        if c = '.' || c = 'e' || c = 'E' then true"
                            "        else hasDot s (i + 1)"
                            "let fixFloat (s: string) (finite: bool) (suffix: string) : string ="
                            "    if not finite then"
                            "        if s = \"NaN\" then \"nan\" + suffix"
                            "        elif s = \"Infinity\" then \"infinity\" + suffix"
                            "        elif s = \"-Infinity\" then \"-infinity\" + suffix"
                            "        else s"
                            "    elif hasDot s 0 then s + suffix"
                            "    else s + \".0\" + suffix"
                            "let formatFloat (value: obj) : string ="
                            "    match value with"
                            "    | :? double as d -> fixFloat (d.ToString(null, CultureInfo.InvariantCulture)) (Double.IsFinite d) \"\""
                            "    | :? single as f -> fixFloat (f.ToString(null, provider)) (Single.IsFinite f) \"f\""
                            "    | _ -> value.ToString()"
                            "let appendEscaped (acc: string) (c: char) (quote: char) : string ="
                            "    if c = '\\\\' then acc + \"\\\\\\\\\""
                            "    elif c = '\\n' then acc + \"\\\\n\""
                            "    elif c = '\\r' then acc + \"\\\\r\""
                            "    elif c = '\\t' then acc + \"\\\\t\""
                            "    elif c = quote then acc + \"\\\\\" + c.ToString()"
                            "    else acc + c.ToString()"
                            "let rec escapeInto (acc: string) (s: string) (i: int) (quote: char) : string ="
                            "    if i >= s.Length then acc"
                            "    else escapeInto (appendEscaped acc s.[i] quote) s (i + 1) quote"
                            "let quoteString (s: string) : string ="
                            "    \"\\\"\" + escapeInto \"\" s 0 '\\\"' + \"\\\"\""
                            "let quoteChar (c: char) : string ="
                            "    \"'\" + appendEscaped \"\" c '\\'' + \"'\""
                            "printfn \"%s\" (formatFloat (box 3.0))"
                            "printfn \"%s\" (formatFloat (box 3.5))"
                            "printfn \"%s\" (formatFloat (box (0.0 / 0.0)))"
                            "printfn \"%s\" (formatFloat (box 3.0f))"
                            "printfn \"%s\" (formatFloat (box (0.0f / 0.0f)))"
                            "printfn \"%s\" (formatFloat (box (1.0f / 0.0f)))"
                            "printfn \"%s\" (quoteString \"a\\\"b\\nc\")"
                            "printfn \"%s\" (quoteChar 'c')"
                            "printfn \"%s\" (quoteChar '\\n')"
                        ])
            }

            test "PP7c probe: ITuple + IEnumerable walk" {
                runsSelfHostLines
                    [ "(1, 2)"; "[1; 2; 3]" ]
                    (String.concat
                        "\n"
                        [
                            "open System.Collections"
                            "open System.Runtime.CompilerServices"
                            "let formatTuple (o: obj) : string ="
                            "    match o with"
                            "    | :? ITuple as t ->"
                            "        let mutable acc = \"(\""
                            "        for i in 0 .. t.Length - 1 do"
                            "            if i > 0 then acc <- acc + \", \""
                            "            acc <- acc + (t.[i]).ToString()"
                            "        acc + \")\""
                            "    | _ -> \"?\""
                            "let formatEnum (o: obj) : string ="
                            "    match o with"
                            "    | :? IEnumerable as xs ->"
                            "        let mutable acc = \"[\""
                            "        let mutable first = true"
                            "        for item in xs do"
                            "            if not first then acc <- acc + \"; \""
                            "            acc <- acc + item.ToString()"
                            "            first <- false"
                            "        acc + \"]\""
                            "    | _ -> \"?\""
                            "printfn \"%s\" (formatTuple (box (1, 2)))"
                            "printfn \"%s\" (formatEnum (box [| 1; 2; 3 |]))"
                        ])
            }

            // The full `%A` dispatch + atom rendering: the reflection-free `:?` chain,
            // atom helpers (`formatPrimitive`/`fixFloat`/`quoteString`/`quoteChar`),
            // and the depth+size budget. Drives one value per dispatch arm.
            test "PP7c: Dispatch + atom rendering (the %A engine, depth+size budget)" {
                runsSelfHostLines
                    [
                        "42"
                        "true"
                        "3.0"
                        "\"hi\""
                        "'c'"
                        "5L"
                        "1.5M"
                        "(1, \"a\")"
                        "[1; 2; 3]"
                        "Some (Some 1)"
                        "[1; 2; ...]"
                    ]
                    (String.concat
                        "\n"
                        ([
                            "open System"
                            "open System.Collections"
                            "open System.Globalization"
                            "open System.Runtime.CompilerServices"
                         ]
                         @ lLayoutCore
                         @ [
                             // ---- atom rendering ----
                             // `CultureInfo.InvariantCulture` is used inline (not bound to a
                             // module `let`) because a module-level value referenced from a
                             // class member body emits an `ldsfld` that is unresolved in the
                             // member emit environment.
                             "let rec hasDot (s: string) (i: int) : bool ="
                             "    if i >= s.Length then false"
                             "    else"
                             "        let c = s.[i]"
                             "        if c = '.' || c = 'e' || c = 'E' then true"
                             "        else hasDot s (i + 1)"
                             "let fixFloat (s: string) (finite: bool) (suffix: string) : string ="
                             "    if not finite then"
                             "        if s = \"NaN\" then \"nan\" + suffix"
                             "        elif s = \"Infinity\" then \"infinity\" + suffix"
                             "        elif s = \"-Infinity\" then \"-infinity\" + suffix"
                             "        else s"
                             "    elif hasDot s 0 then s + suffix"
                             "    else s + \".0\" + suffix"
                             "let formatPrimitive (value: obj) : string ="
                             "    match value with"
                             "    | :? double as d -> fixFloat (d.ToString(null, CultureInfo.InvariantCulture)) (Double.IsFinite d) \"\""
                             "    | :? single as f -> fixFloat (f.ToString(null, CultureInfo.InvariantCulture)) (Single.IsFinite f) \"f\""
                             "    | _ ->"
                             "        let s ="
                             "            match value with"
                             "            | :? IFormattable as fmt -> fmt.ToString(null, CultureInfo.InvariantCulture)"
                             "            | _ -> value.ToString()"
                             "        let suffix ="
                             "            match value with"
                             "            | :? sbyte -> \"y\""
                             "            | :? byte -> \"uy\""
                             "            | :? int16 -> \"s\""
                             "            | :? uint16 -> \"us\""
                             "            | :? uint32 -> \"u\""
                             "            | :? int64 -> \"L\""
                             "            | :? uint64 -> \"UL\""
                             "            | :? decimal -> \"M\""
                             "            | _ -> \"\""
                             "        s + suffix"
                             "let appendEscaped (acc: string) (c: char) (quote: char) : string ="
                             "    if c = '\\\\' then acc + \"\\\\\\\\\""
                             "    elif c = '\\n' then acc + \"\\\\n\""
                             "    elif c = '\\r' then acc + \"\\\\r\""
                             "    elif c = '\\t' then acc + \"\\\\t\""
                             "    elif c = quote then acc + \"\\\\\" + c.ToString()"
                             "    else acc + c.ToString()"
                             "let rec escapeInto (acc: string) (s: string) (i: int) (quote: char) : string ="
                             "    if i >= s.Length then acc"
                             "    else escapeInto (appendEscaped acc s.[i] quote) s (i + 1) quote"
                             "let quoteString (s: string) : string ="
                             "    \"\\\"\" + escapeInto \"\" s 0 '\\\"' + \"\\\"\""
                             "let quoteChar (c: char) : string ="
                             "    \"'\" + appendEscaped \"\" c '\\'' + \"'\""
                         ]
                         @ lFrameSemTypes
                         @ [
                             // ---- sink + frame stack + dispatch ----
                             "type LSink ="
                             "    val Width: int"
                             "    val mutable Size: int"
                             "    val mutable Depth: int"
                             "    val mutable Frames: Frame list"
                             "    val mutable SemFrames: SemFrame list"
                             "    val mutable LastAppShaped: bool"
                             "    new(width: int, size: int) ="
                             "        let root = Frame(Root, 0)"
                             "        { Width = width; Size = size; Depth = 0; Frames = [ root ]; SemFrames = []; LastAppShaped = false }"
                         ]
                         @ lSinkFramePlumbing
                         @ [
                             "    member private this.FormatTuple(t: ITuple) ="
                             "        this.Push(Frame(Group, 0))"
                             "        this.Add(LText \"(\")"
                             "        this.Push(Frame(Nest, 1))"
                             "        for i in 0 .. t.Length - 1 do"
                             "            if i > 0 then"
                             "                this.Add(LText \",\")"
                             "                this.Add(LLine \" \")"
                             "            this.Dispatch(t.[i])"
                             "        this.PopWrap(Nest)"
                             "        this.Add(LText \")\")"
                             "        this.PopWrap(Group)"
                             "        this.LastAppShaped <- false"
                             "    member private this.FormatEnumerable(xs: IEnumerable) ="
                             "        this.Push(Frame(Group, 0))"
                             "        this.Add(LText \"[\")"
                             "        this.Push(Frame(Nest, 2))"
                             "        this.Add(LLine \"\")"
                             "        let mutable i = 0"
                             "        let mutable truncated = false"
                             "        for item in xs do"
                             "            if not truncated then"
                             "                if i > 0 then"
                             "                    this.Add(LText \";\")"
                             "                    this.Add(LLine \" \")"
                             "                if i >= 100 || this.Size <= 0 then"
                             "                    this.Add(LText \"...\")"
                             "                    truncated <- true"
                             "                else"
                             "                    this.Dispatch(item)"
                             "                    i <- i + 1"
                             "        this.PopWrap(Nest)"
                             "        this.Add(LLine \"\")"
                             "        this.Add(LText \"]\")"
                             "        this.PopWrap(Group)"
                             "        this.LastAppShaped <- false"
                             "    member private this.Dispatch(value: obj) ="
                             "        this.LastAppShaped <- false"
                             "        match value with"
                             "        | null -> this.Add(LText \"null\")"
                             "        | _ ->"
                             "            if this.Depth >= 100 then this.Add(LText \"...\")"
                             "            elif this.Size <= 0 then this.Add(LText \"...\")"
                             "            else"
                             "                this.Depth <- this.Depth + 1"
                             "                this.DispatchInner(value)"
                             "                this.Depth <- this.Depth - 1"
                             "    member private this.DispatchInner(value: obj) ="
                             "        match value with"
                             "        | :? Vesper.IStructuralFormattable as structural ->"
                             "            structural.Format(this :> Vesper.IFormatSink)"
                             "        | :? string as s ->"
                             "            this.Size <- this.Size - 1"
                             "            this.Add(LText(quoteString s))"
                             "        | :? char as c ->"
                             "            this.Size <- this.Size - 1"
                             "            this.Add(LText(quoteChar c))"
                             "        | :? bool as b ->"
                             "            this.Size <- this.Size - 1"
                             "            this.Add(LText(if b then \"true\" else \"false\"))"
                             "        | :? ITuple as t -> this.FormatTuple t"
                             "        | :? IFormattable ->"
                             "            this.Size <- this.Size - 1"
                             "            this.Add(LText(formatPrimitive value))"
                             "        | :? IEnumerable as xs -> this.FormatEnumerable xs"
                             "        | _ ->"
                             "            this.Size <- this.Size - 1"
                             "            this.Add(LText(value.ToString()))"
                         ]
                         @ lSinkFinishAndProtocol
                         @ [
                             "type MyOpt(payload: obj, isSome: bool) ="
                             "    interface Vesper.IStructuralFormattable with"
                             "        member this.Format(sink: Vesper.IFormatSink) ="
                             "            if isSome then"
                             "                sink.BeginCase \"Some\""
                             "                sink.Child payload"
                             "                sink.EndCase()"
                             "            else"
                             "                sink.BeginCase \"None\""
                             "                sink.EndCase()"
                             "let print (v: obj) (width: int) (size: int) : string ="
                             "    let sink = LSink(width, size)"
                             "    (sink :> Vesper.IFormatSink).Child(v)"
                             "    sink.Finish()"
                             "printfn \"%s\" (print (box 42) 80 10000)"
                             "printfn \"%s\" (print (box true) 80 10000)"
                             "printfn \"%s\" (print (box 3.0) 80 10000)"
                             "printfn \"%s\" (print (box \"hi\") 80 10000)"
                             "printfn \"%s\" (print (box 'c') 80 10000)"
                             "printfn \"%s\" (print (box 5L) 80 10000)"
                             "printfn \"%s\" (print (box 1.5M) 80 10000)"
                             "printfn \"%s\" (print (box (1, \"a\")) 80 10000)"
                             "printfn \"%s\" (print (box [| 1; 2; 3 |]) 80 10000)"
                             "printfn \"%s\" (print ((MyOpt((MyOpt(box 1, true) :> obj), true)) :> obj) 80 10000)"
                             "printfn \"%s\" (print (box [| 1; 2; 3; 4; 5 |]) 80 2)"
                         ]))
            }

            // A val-field class whose only ctor is a parameterless `new() = { … }`:
            // no synthesised empty primary `.ctor()` must be emitted alongside it
            // (two identical `.ctor()` rows would leave construction binding the empty
            // one and every field uninitialised).
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
