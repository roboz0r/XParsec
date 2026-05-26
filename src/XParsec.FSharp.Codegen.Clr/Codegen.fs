namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Entry points: the `compile` / `materialise` pair from
// [backend-design-plan](../XParsec.FSharp.SemanticAnalysis/docs/backend-design-plan.md).
// `compile` is pure-ish (deterministic given the same inputs); `materialise`
// is the only side effect.

/// One row in `Codegen`'s deferred `TypeDefinition` lists (`unionTypes` /
/// `recordTypes`). The actual `TypeDefinition` / `InterfaceImpl` /
/// `GenericParam` rows aren't added until every method/field row exists, so
/// the per-type loop accumulates this shape and the trailing pass walks it.
/// `DeclaresIEquatable` (records-plan §B4) and `DeclaresIComparable`
/// (records-plan §B6) decide which `InterfaceImpl` rows pair with the
/// type — see the comment on `unionTypes` for the per-flag detail.
type internal EmittedTypeRow =
    {
        Name: string
        Namespace: string
        Typars: string list
        FirstField: FieldDefinitionHandle
        FirstMethod: MethodDefinitionHandle
        DeclaresIEquatable: bool
        DeclaresIComparable: bool
    }

/// The in-memory assembled PE plus enough to inspect / write it.
type ClrArtifact =
    {
        AssemblyName: string
        OutputPath: string option
        /// The serialised PE image.
        Pe: BlobBuilder
        /// Simple names of every assembly the emitted PE binds against (its
        /// `AssemblyRef` table). Drives `materialiseApp`'s copy: a referenced
        /// assembly with a known source (a `ProjectInfo.References` entry, or a
        /// host-loaded FSharp.Core / Vesper.Printf fallback) is copied beside the
        /// PE; BCL / shared-framework refs have no source and resolve at runtime.
        ReferencedAssemblies: string list
        /// The distinct FSharp.Core constructs the emission referenced
        /// (from `ICodegenProvider.FSharpCoreDependencies`). **Empty ⇒ the PE
        /// has no `FSharp.Core.dll` dependency** (finer-grained than
        /// `ReferencedAssemblies` — the §D3 cut list of *what* pins it).
        FSharpCoreDependencies: string list
    }

module Codegen =

    /// `int Main(string[])` — the synthesised entry point's signature.
    let private mainSignature () : BlobBuilder =
        let sigB = BlobBuilder()

        BlobEncoder(sigB)
            .MethodSignature()
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().SZArray().String())
            )

        sigB

    /// Shared assembly scaffolding: module + assembly rows, a `Main` whose
    /// body comes from `build`, the `<Module>` pseudo-type, and the holder
    /// class. `build` receives the wired context + provider so a hand-written
    /// `Il` body (the `assembleMainEmit` test seam) can reference primitives.
    let private assembleWith
        (project: ProjectInfo)
        (build: MetadataContext -> ClrProvider -> (Il -> unit))
        : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        // No TAST on this hand-written-body seam — the built-in primitive
        // representations are all it can reference. No external references either:
        // these hand-written bodies form no function value (so `Vesper.Fun` is never
        // needed) and no list literal (so `Vesper.List` is never needed); no external
        // member access either, so the symbol provider is the null one.
        let provider =
            ClrProvider(ctx, IntrinsicRepr.defaults, Map.empty, ExternalSymbols.nullProvider)

        let icodegen = provider :> ICodegenProvider

        let bodyOffset =
            Cil.buildBody (fun locals -> icodegen.EncodeLocalSignature locals) ctx.BodyStream (build ctx provider)

        let mainDef =
            ctx.AddMethod(
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                "Main",
                mainSignature (),
                bodyOffset
            )

        // `<Module>` (row 1, empty method range) then the holder class, which
        // claims `Main` onward. No closures on this seam, so the holder owns
        // the (empty) field table from row 1.
        ctx.AddModuleType(mainDef)

        ctx.AddProgramType(
            "",
            project.ModuleName,
            provider.ObjectType,
            MetadataTokens.FieldDefinitionHandle(1),
            mainDef
        )
        |> ignore

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = ctx.Serialize(mainDef)
            // Captured after the body build above ran every encoder/recipe.
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }

    // ---- Shared helpers: abstract (interface) method emission ----

    /// `Public ||| Abstract ||| Virtual ||| HideBySig ||| NewSlot` — an abstract
    /// interface method (no body; `AddMethodWithParamList` is called with
    /// `bodyOffset = -1`).
    let private abstractMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Abstract
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot

    /// Decurry a curried function `SemType` into (parameter types, return type).
    /// `TyFun('A, 'B)` ⇒ `(['A], 'B)`.
    let rec private decurry (t: SemType) : SemType list * SemType =
        match t with
        | TyFun(a, b) ->
            let ps, r = decurry b
            a :: ps, r
        | _ -> [], t

    /// `instance <ret> <name><'C…>(<params…>)` for an abstract interface method,
    /// with the declaring type's typars resolved to `GenericTypeParameter` indices
    /// and the method's own typars to `GenericMethodParameter` indices. Concrete
    /// leaves (a primitive, `unit`, a nested function type) are encoded by the
    /// provider's `EncodeAbstractType` (G5) — the same `encodeType` the executable
    /// path uses, so an abstract signature can reach anything the provider can
    /// encode and the dependency surface it pins is recorded. The signature's
    /// generic-parameter count is the method's own typar count.
    let private abstractMethodSignature
        (provider: ClrProvider)
        (typeParams: string list)
        (m: TAbstractMethod)
        : BlobBuilder =
        let typeIx = typeParams |> List.mapi (fun i n -> n, i) |> Map.ofList
        let methodIx = m.MethodTypeParams |> List.mapi (fun i n -> n, i) |> Map.ofList
        let paramTys, retTy = decurry m.Signature
        let blob = BlobBuilder()

        BlobEncoder(blob)
            .MethodSignature(genericParameterCount = List.length m.MethodTypeParams, isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> provider.EncodeAbstractType(typeIx, methodIx, ret.Type(), retTy)),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        provider.EncodeAbstractType(typeIx, methodIx, pars.AddParameter().Type(), p)
                )
            )

        blob

    // ---- The converged assembler (G9 / rung-2 P3c) ----

    /// One spine emits every declared type and the module's value-bearing
    /// members, regardless of output kind: the declared interfaces (bodyless
    /// abstract methods, rung 1), the declared unions (a monomorphic tagged class
    /// + per-case factory, rung 2), the synthesised closures, and the
    /// module-level static methods (P3b). The *only* Exe/Library difference is
    /// the tail — an executable appends a synthesised `Main` + entry point and
    /// serialises *with* one (`emitEntryPoint = true`); a library appends neither.
    ///
    /// Metadata stays contiguous and only-backward-referencing via a fixed
    /// emission order:
    ///   methods: interface abstract methods → union ctor+factories → closure
    ///            ctor+`Invoke` (leaves-first) → static methods → `Main`;
    ///   fields:  union tag+case fields → closure captures;
    ///   types:   `<Module>` (row 1) → interfaces → unions → closures → `Program`.
    /// Two forward references are *predicted* from the row counts and registered
    /// up front so a body built before its target row resolves: a union's
    /// `TypeDefinition` (a field / factory / local signature `encodeType`s it) and
    /// a static method's `MethodDefinition` (a recursive / cross call `call`s it).
    /// `GenericParam` rows (today only an interface's typars) are collected and
    /// emitted last, sorted by `CodedIndex.TypeOrMethodDef(owner)` then index, as
    /// SRM requires (a method owner can sort before its declaring type).
    let private assemble
        (externalInlines: Map<string, TDecl>)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tast: TastFile)
        (emitEntryPoint: bool)
        : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        // The referenced assemblies' identities, read off their files and keyed by
        // simple name — so the emitted `AssemblyRef` matches the exact artifact, not
        // whatever the host loaded (R4). The provider resolves a type's owning
        // assembly by name (`Vesper.Core` → `Fun`, `Vesper.List` → the cons-list, an
        // explicit `FSharp.Core` overriding the host fallback); each is its own
        // package (package-split-plan PS2).
        let references =
            project.References
            |> List.map (fun path ->
                let an = AssemblyName.GetAssemblyName path
                an.Name, an
            )
            |> Map.ofList

        // This file's intrinsic bindings overlay the defaults (G7). The provider's
        // refs are lazy (G6), so a typar-only / BCL-only build forces no
        // FSharp.Core `AssemblyRef` (and a function-value-free / list-free build
        // forces no `Vesper.Core` / `Vesper.List` ref); a richer signature reuses
        // `encodeType` (G5).
        let provider =
            ClrProvider(ctx, IntrinsicRepr.merge tast.IntrinsicReprTypes, references, symbols)

        let icodegen = provider :> ICodegenProvider
        let encodeLocals locals = icodegen.EncodeLocalSignature locals

        // One body-stream encoder shared by every method: its ctor requires a
        // 4-byte-aligned IL builder, so a fresh encoder per body would throw once
        // the first (tiny) body left the builder unaligned. `AddMethodBody`
        // realigns per body internally, so reuse is correct.
        let bodyStream = ctx.BodyStream

        let lowered = Emit.lowerWith externalInlines tast.Decls

        // Top-level functions emitted as static methods (P3b) — excluded from
        // closure discovery and resolved as direct `call`s at their use sites.
        // A binding from a named `module Foo = …` (R3 deferred) carries a holder
        // so it lands on a real `Foo`/`FooModule` static class.
        let staticFns, staticFnKeys = Emit.collectStaticFns tast.ModuleMembers lowered

        // Emit each named module's functions on a dedicated holder type; order
        // emission so every holder's methods form a *contiguous* `MethodDef` range
        // — each named-holder group first (in first-appearance order), then the
        // holder-less ("Program") functions. This single order drives both handle
        // prediction and body emission, so the predicted handles line up with the
        // rows actually added, and the holders' `TypeDefinition` rows stay ascending
        // by first method.
        let namedHolderGroups =
            staticFns
            |> List.choose (fun fn ->
                match fn.Holder with
                | Some h -> Some(h, fn)
                | None -> None
            )
            |> List.groupBy fst
            |> List.map (fun (h, pairs) -> h, List.map snd pairs)

        let holderlessFns = staticFns |> List.filter (fun fn -> fn.Holder.IsNone)
        let staticFnsEmitOrder = (namedHolderGroups |> List.collect snd) @ holderlessFns

        // Each static fn's typar set, by binding key. A closure walked from a
        // generic static fn's body inherits this on its `Closure.Typars`; an
        // empty list (monomorphic fn / `Main` resident) leaves the closure
        // monomorphic. Computed *before* the `staticMethods` dictionary
        // proper (handles aren't predicted yet at this point); both end up
        // with the same `Typars` for any given key.
        let staticFnTyparsMap = Dictionary<NodeKey, TypeVar list>()

        for fn in staticFns do
            staticFnTyparsMap.[fn.Key] <- Emit.staticFnTypars fn

        let closures, closureByNode =
            Emit.discoverClosures staticFnKeys staticFnTyparsMap lowered

        let ctorHandleByNode = Dictionary<TExpr, EntityHandle>(HashIdentity.Reference)

        // Declared types split by kind (two disjoint `tast.Decls` walks). Both
        // kinds now flow through this one assembler on both paths: the library
        // used to drop unions (G9), and an executable still declares no interfaces
        // in today's sources (so `interfaceDecls` is empty there and the interface
        // block below is a no-op that shifts no prediction).
        let interfaceDecls =
            tast.Decls
            |> List.choose (fun d ->
                match d with
                | TDecl.Type td ->
                    match td.Kind with
                    | TTypeKind.Interface methods -> Some(td, methods)
                    | _ -> None
                | _ -> None
            )

        let unionDecls =
            tast.Decls
            |> List.choose (fun d ->
                match d with
                | TDecl.Type td ->
                    match td.Kind with
                    | TTypeKind.Union(cases, members) -> Some(td, cases, members)
                    | _ -> None
                | _ -> None
            )

        let recordDecls =
            tast.Decls
            |> List.choose (fun d ->
                match d with
                | TDecl.Type td ->
                    match td.Kind with
                    | TTypeKind.Record(fields, members) -> Some(td, fields, members)
                    | _ -> None
                | _ -> None
            )

        // ---- Forward-reference prediction ----

        let interfaceCount = List.length interfaceDecls
        let unionCount = List.length unionDecls

        let interfaceMethodTotal =
            interfaceDecls |> List.sumBy (fun (_, methods) -> List.length methods)

        // A union's `TypeDefinition` lands at row `2 + interfaceCount + i` (after
        // `<Module>` and the interface types). Register it up front so a field /
        // factory / local signature can `encodeType` a `TyUnion name` before the
        // row itself is added. A *generic* union also registers its shape (typars +
        // cases) so the provider can mint `MemberRef`s on its `TypeSpec` — for both
        // its own factory bodies and any external construction / match site (P3d.4).
        unionDecls
        |> List.iteri (fun i (td, cases, _) ->
            provider.RegisterUserType(td.Name, toEntity (MetadataTokens.TypeDefinitionHandle(2 + interfaceCount + i)))

            if not (List.isEmpty td.TypeParams) then
                let shape =
                    [
                        for c in cases ->
                            c.Name,
                            [
                                for fi in 0 .. c.Fields.Length - 1 -> sprintf "%s_%d" c.Name fi, snd c.Fields.[fi]
                            ]
                    ]

                provider.RegisterGenericUnion(td.Name, td.TypeParams, shape)
        )

        // Records sit immediately after unions in the `TypeDefinition` table
        // (records-plan §B2). Same up-front registration shape: predict the
        // `TypeDefinition` handle so a record-typed local / `RecordCons` ctor /
        // member-ref signature can reach the type before its row exists, and
        // register a generic record's field shape so `GenericRecordMemberRef`
        // can mint `MemberRef`s on its `TypeSpec` (`Box\`1<int>::Value`).
        recordDecls
        |> List.iteri (fun i (td, fields, _) ->
            provider.RegisterUserType(
                td.Name,
                toEntity (MetadataTokens.TypeDefinitionHandle(2 + interfaceCount + unionCount + i))
            )

            if not (List.isEmpty td.TypeParams) then
                let shape = [ for f in fields -> f.Name, f.Type ]
                provider.RegisterGenericRecord(td.Name, td.TypeParams, shape)
        )

        let recordCount = List.length recordDecls

        // A *generic* closure (function-representation-plan §Generic closures, C3) is a real generic
        // `TypeDefinition`, sitting immediately after interfaces/unions/records
        // and before the holders. Predict its handle here so any reference inside
        // the closure's own emission (capture-field `MemberRef`s built before the
        // type row exists) and the construction-site `Newobj` both reach it.
        // Monomorphic closures are skipped — their `Def` tokens are used directly
        // (matching the union / record split).
        closures
        |> List.iteri (fun i c ->
            if not (List.isEmpty c.Typars) then
                let handle =
                    toEntity (MetadataTokens.TypeDefinitionHandle(2 + interfaceCount + unionCount + recordCount + i))

                provider.RegisterClosure(c.Name, c.Typars, c.Captures |> List.map snd, c.ParamTy, c.ResultTy, handle)
        )

        let unions = Dictionary<string, Emit.EmittedUnion>()
        let records = Dictionary<string, Emit.EmittedRecord>()

        // One row per emitted union/record `TypeDefinition`, claimed after
        // every method/field row exists. `Typars` drives the metadata arity
        // suffix (`List\`1`) + `GenericParam` rows for a generic union (empty
        // ⇒ monomorphic). The two flags decide which `InterfaceImpl` rows
        // pair with the type's `TypeDefinition` below:
        //   * `DeclaresIEquatable` (records-plan §B4) pairs with the emitted
        //     `Equals(Self)` — `IEquatable<Self>`.
        //   * `DeclaresIComparable` (records-plan §B6) pairs with the
        //     emitted `CompareTo(Self)` + `CompareTo(object)` —
        //     `IComparable<Self>` and the non-generic `IComparable`.
        // Both default to `false` when the corresponding C-Attr verdict is
        // anything other than `Structural`.
        let unionTypes = ResizeArray<EmittedTypeRow>()

        // Per union: the parameterless `.ctor`, one factory per case, one method
        // per augmentation member (P3d.3), and — when the C-Attr verdict is
        // `Structural` (default) — the synthesised `GetHashCode()` +
        // `Equals(object)` + typed `Equals(Self)` triple (C-Eq1 / S4). A
        // `[<ReferenceEquality>]` / `[<NoEquality>]` union skips the triple
        // (records-plan §B4). A `[<StructuralComparison>]` union also gets
        // the synthesised `CompareTo(Self)` + `CompareTo(object)` pair
        // (records-plan §B6).
        let unionMethodTotal =
            unionDecls
            |> List.sumBy (fun (td, cases, members) ->
                let triple =
                    match td.EqualitySupport with
                    | EqualityVerdict.Structural -> 3
                    | _ -> 0

                let pair =
                    match td.ComparisonSupport with
                    | ComparisonVerdict.Structural -> 2
                    | _ -> 0

                1 + List.length cases + List.length members + triple + pair
            )

        // Per record: one ctor taking the fields, one method per augmentation
        // member (empty in v1), and — when the C-Attr verdict is `Structural`
        // — the synthesised `GetHashCode()` + `Equals(object)` + typed
        // `Equals(Self)` triple (records-plan §B4 / brainstorm-structural-equality
        // §8). Mutable records default to `Reference` (no triple); explicit
        // `[<StructuralEquality>]` / `[<ReferenceEquality>]` / `[<NoEquality>]`
        // overrides the default. The verdict is computed in
        // `NameResolution.registerRecordTypeDefn` and projected onto
        // `td.EqualitySupport` by `Freeze`. A `[<StructuralComparison>]`
        // record also gets the synthesised `CompareTo` pair
        // (records-plan §B6).
        let recordMethodTotal =
            recordDecls
            |> List.sumBy (fun (td, _, members) ->
                let triple =
                    match td.EqualitySupport with
                    | EqualityVerdict.Structural -> 3
                    | _ -> 0

                let pair =
                    match td.ComparisonSupport with
                    | ComparisonVerdict.Structural -> 2
                    | _ -> 0

                1 + List.length members + triple + pair
            )

        let closureMethodTotal = 2 * List.length closures

        // Static methods follow the interface, union, record, and closure methods,
        // so a static method's `MethodDefinition` is `staticBase + 1 + i`.
        let staticBase =
            interfaceMethodTotal + unionMethodTotal + recordMethodTotal + closureMethodTotal

        let staticMethods = Dictionary<NodeKey, Emit.StaticMethodRef>()

        staticFnsEmitOrder
        |> List.iteri (fun i fn ->
            staticMethods.[fn.Key] <-
                {
                    Handle = toEntity (MetadataTokens.MethodDefinitionHandle(staticBase + 1 + i))
                    Arity = List.length fn.Params
                    ResultTy = fn.ResultTy
                    Typars = staticFnTyparsMap.[fn.Key]
                    ParamTys = fn.Params |> List.map snd
                }
        )

        // ---- Attribute sets ----

        let closureAttrs =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.AutoLayout
            ||| TypeAttributes.AnsiClass
            ||| TypeAttributes.BeforeFieldInit

        // A monomorphic union is a single sealed reference class (same shape as a
        // closure type — no inheritance in rung 2).
        let unionAttrs = closureAttrs

        // A union case's static factory and a module-level static method (P3b)
        // share attributes (`public static hidebysig`).
        let staticFactoryAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.Static
            ||| MethodAttributes.HideBySig

        let staticMethodAttrs = staticFactoryAttrs

        // A union instance augmentation member (P3d.3) — a plain non-virtual
        // `public hidebysig` instance method (the union is sealed, so `call`
        // dispatch is correct; no `Virtual`/`NewSlot`).
        let instanceMethodAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

        // A synthesised `Object.Equals`/`GetHashCode` *override* (C-Eq1): reuse
        // the base virtual slot (no `NewSlot`), so name + signature matching
        // makes it the override — exactly what F#/C# emit for those two.
        let overrideMethodAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig

        // The typed `IEquatable<Self>::Equals(Self)` (C-Eq1): a *new* virtual slot
        // (`Object` has no `Equals(Self)` to reuse), `Final` since the union is
        // sealed — exactly the shape the closure `Invoke` uses to implicitly
        // implement `Vesper.Fun`. The `InterfaceImpl` row (below) declares the
        // interface; the runtime binds this method to it by name + signature.
        let ifaceEqualsAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.Final

        let ctorAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        // `NewSlot ||| Final` virtual: the closure derives from `System.Object`
        // (no base `Invoke` to reuse) and *implements* the `Vesper.Fun\`2::Invoke`
        // interface slot (R1). Implicit interface implementation — the runtime maps
        // the instantiated interface method to this `Invoke` by name + signature
        // (the `InterfaceImpl` row declares the interface); `Final` because a sealed
        // closure has no further overrides.
        let invokeAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.Final

        // G8: route every emitted method through a real `Param` list (so
        // reflection's `GetParameters` works, not just execution). `Param` rows
        // are a global table referenced by each `MethodDefinition.ParamList`, so
        // they must be added in method order; `addParams` is called immediately
        // before each `AddMethodWithParamList`, returning the method's first
        // `Param` handle (past-the-end for a zero-parameter method).
        let mutable paramCount = 0

        let addParams (names: string list) : ParameterHandle =
            let firstParam = MetadataTokens.ParameterHandle(paramCount + 1)

            names
            |> List.iteri (fun i n ->
                ctx.AddParameter(i + 1, n) |> ignore
                paramCount <- paramCount + 1
            )

            firstParam

        let argNames (n: int) : string list =
            [ for i in 0 .. n - 1 -> sprintf "arg%d" i ]

        // The first emitted method row, claimed by `<Module>`. Set on the first
        // method added of any kind (interface, union, closure, static, `Main`).
        let mutable firstMethod = ValueNone

        let claimFirstMethod (h: MethodDefinitionHandle) =
            if firstMethod.IsNone then
                firstMethod <- ValueSome h

        // ---- Interfaces: bodyless abstract methods + collected generic params ----
        //
        // `GenericParam` rows can't be added inline: SRM requires them globally
        // sorted by `CodedIndex.TypeOrMethodDef(owner)`, and a method owner can
        // sort *before* its declaring type. So collect (owner, index, name) for
        // every type and method typar and emit them sorted, once all handles exist.
        let mutable methodCount = 0
        let interfacePending = ResizeArray<TTypeDecl * MethodDefinitionHandle>()
        let genericParams = ResizeArray<EntityHandle * int * string>()

        for (td, methods) in interfaceDecls do
            let firstIfaceMethod = MetadataTokens.MethodDefinitionHandle(methodCount + 1)

            for m in methods do
                let paramTys, _ = decurry m.Signature

                let methodHandle =
                    ctx.AddMethodWithParamList(
                        abstractMethodAttrs,
                        m.Name,
                        abstractMethodSignature provider td.TypeParams m,
                        -1,
                        addParams (argNames (List.length paramTys))
                    )

                // The method's own typars are owned by this MethodDef (the metadata
                // name drops the F# leading quote: `'C` ⇒ `C`).
                m.MethodTypeParams
                |> List.iteri (fun i n -> genericParams.Add(toEntity methodHandle, i, n.TrimStart('\'')))

                methodCount <- methodCount + 1

            claimFirstMethod firstIfaceMethod
            interfacePending.Add(td, firstIfaceMethod)

        // ---- Unions: tag + per-case fields, parameterless ctor, per-case factory ----
        //
        // Field / method rows are added before the closures' (so each union's
        // `TypeDefinition` range precedes them); the factory bodies reference the
        // `.ctor` + field handles, already added.
        let mutable fieldCount = 0

        for (td, cases, members) in unionDecls do
            let firstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)

            // A generic union (`List<'T>`) is a real generic `TypeDefinition`: its
            // field / factory signatures are encoded in terms of its own generic
            // parameters (`!0`), and every member access goes through a `MemberRef`
            // on the type's `TypeSpec` (P3d.4). `typarMarkers` is the instantiation
            // a member ref *inside* this type uses — the type's own typars.
            let isGeneric = not (List.isEmpty td.TypeParams)
            let typarMarkers = [ for n in td.TypeParams -> TyConst n ]

            let tagField =
                toEntity (ctx.AddField(FieldAttributes.Public, "_tag", provider.FieldSignature(TyConst "int")))

            fieldCount <- fieldCount + 1

            // Per-case field handles (declaration order), uniquely named
            // `<Case>_<index>` so cases don't collide in the flat field table. A
            // generic case field's signature carries the typars (`Head : !0`).
            let caseFields =
                [
                    for c in cases ->
                        let handles =
                            c.Fields
                            |> List.mapi (fun fi (_, fty) ->
                                let sigBlob =
                                    if isGeneric then
                                        provider.GenericFieldSignature(td.TypeParams, fty)
                                    else
                                        provider.FieldSignature fty

                                let h = ctx.AddField(FieldAttributes.Public, sprintf "%s_%d" c.Name fi, sigBlob)
                                fieldCount <- fieldCount + 1
                                toEntity h
                            )

                        c.Name, handles
                ]

            let ctorBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef []))

            let unionCtor =
                ctx.AddMethodWithParamList(
                    ctorAttrs,
                    ".ctor",
                    provider.NullaryCtorSignature(),
                    ctorBodyOffset,
                    addParams []
                )

            claimFirstMethod unionCtor
            methodCount <- methodCount + 1 // the `.ctor` row

            let emittedCases = Dictionary<string, Emit.EmittedCase>()

            cases
            |> List.iteri (fun tag c ->
                let fieldHandles = caseFields |> List.find (fun (n, _) -> n = c.Name) |> snd

                // The factory body's ctor / tag / field tokens: `Def`s for a
                // monomorphic union, `MemberRef`s on `List<!0>` (the type's own
                // typars) for a generic one — the same `emitUnionFactory` shape,
                // different tokens (P3d.4).
                let ctorRef, tagRef, fieldRefs =
                    if isGeneric then
                        icodegen.GenericUnionMemberRef(td.Name, typarMarkers, UnionMember.Ctor),
                        icodegen.GenericUnionMemberRef(td.Name, typarMarkers, UnionMember.Tag),
                        [
                            for fi in 0 .. List.length fieldHandles - 1 ->
                                icodegen.GenericUnionMemberRef(td.Name, typarMarkers, UnionMember.Field(c.Name, fi))
                        ]
                    else
                        toEntity unionCtor, tagField, fieldHandles

                let factoryBody =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildUnionFactory ctorRef tag tagRef fieldRefs))

                let paramTys = c.Fields |> List.map snd

                let factorySig =
                    if isGeneric then
                        provider.GenericStaticMethodSignature(td.TypeParams, paramTys, TyUnion(td.Name, typarMarkers))
                    else
                        provider.StaticMethodSignature(paramTys, TyUnion(td.Name, []))

                let factory =
                    ctx.AddMethodWithParamList(
                        staticFactoryAttrs,
                        c.Name,
                        factorySig,
                        factoryBody,
                        addParams (argNames (List.length paramTys))
                    )

                methodCount <- methodCount + 1 // each case factory row

                emittedCases.[c.Name] <-
                    {
                        Tag = tag
                        Factory = toEntity factory
                        Fields = fieldHandles
                    }
            )

            // Augmentation members (P3d.3) follow the factories within this
            // union's contiguous method range. Predict each method's handle from
            // the running row count *before* building any body, so a member body
            // can reference a sibling member (`this.Length`) or a case factory
            // (`Empty = Nil`). A property is emitted as a `get_<name>` method.
            let emittedMembers = Dictionary<string, Emit.EmittedMember>()

            // A property is emitted (and referenced) as `get_<name>`; a method
            // keeps its name.
            let memberMetaName (mem: TTypeMember) =
                match mem.Kind with
                | TMemberKind.Property -> "get_" + mem.Name
                | TMemberKind.Method -> mem.Name

            members
            |> List.iteri (fun i (mem: TTypeMember) ->
                let handle = MetadataTokens.MethodDefinitionHandle(methodCount + 1 + i)

                emittedMembers.[mem.Name] <-
                    {
                        Handle = toEntity handle
                        IsStatic = mem.IsStatic
                        Arity = List.length mem.Params
                        MetaName = memberMetaName mem
                        ParamTys = mem.Params |> List.map snd
                        RetTy = mem.ReturnTy
                    }
            )

            // Register the union before its member bodies are built, so they
            // resolve their own type's cases + members.
            unions.[td.Name] <-
                {
                    Name = td.Name
                    Typars = td.TypeParams
                    TagField = tagField
                    Cases = emittedCases
                    Members = emittedMembers
                }

            // A generic union's member bodies share the type's generic context, so
            // a typar-typed local (`h : 'T`) encodes to `!0` (R2); a monomorphic
            // body uses the executable local encoder.
            let memberEncodeLocals =
                if isGeneric then
                    fun locals -> provider.EncodeGenericLocalSignature(td.TypeParams, locals)
                else
                    encodeLocals

            for mem in members do
                let bodyOffset =
                    Cil.buildBody
                        memberEncodeLocals
                        bodyStream
                        (IlIr.lower (
                            Emit.buildMember
                                icodegen
                                ctx
                                closureByNode
                                ctorHandleByNode
                                unions
                                records
                                staticMethods
                                mem.ThisKey
                                mem.Params
                                // Member bodies emit straight from `tast.Decls`, never
                                // through `Emit.lower`, so the operator → inline-IL rewrite
                                // is applied here (operators-plan.md, C-Eq1).
                                (Emit.expandBuiltinOps mem.Body)
                        ))

                let methodName = memberMetaName mem
                let paramTys = mem.Params |> List.map snd

                // A generic union's member signatures are written in its own
                // typars (`instance !0 get_Head()`) (R2); a monomorphic union's
                // are concrete.
                let signature =
                    match isGeneric, mem.IsStatic with
                    | true, true -> provider.GenericStaticMethodSignature(td.TypeParams, paramTys, mem.ReturnTy)
                    | true, false -> provider.GenericInstanceMethodSignature(td.TypeParams, paramTys, mem.ReturnTy)
                    | false, true -> provider.StaticMethodSignature(paramTys, mem.ReturnTy)
                    | false, false -> provider.InstanceMethodSignature(paramTys, mem.ReturnTy)

                let attrs =
                    if mem.IsStatic then
                        staticMethodAttrs
                    else
                        instanceMethodAttrs

                ctx.AddMethodWithParamList(
                    attrs,
                    methodName,
                    signature,
                    bodyOffset,
                    addParams (argNames (List.length mem.Params))
                )
                |> ignore

                methodCount <- methodCount + 1 // each member-method row

            // Synthesised structural-equality triple (C-Eq1 / S4) when the
            // C-Attr verdict is `Structural` (default for a union; records-plan
            // §B4). `Equals(object)` + `GetHashCode()` overrides + the typed
            // `IEquatable<Self>::Equals(Self)` walk the cases' fields by the
            // §3.2 rule (`EqualityComparer<F>.Default`, `System.HashCode`). A
            // *generic* union (S4) gets the same triple written in its own
            // `!0`: field/tag access goes through `MemberRef`s on its `TypeSpec`,
            // `isinst`/`other`/the typed-`Equals` param/the interface arg are
            // the type's own `TypeSpec` (`List<!0>`), and a typar-typed field
            // reaches `EqualityComparer<!0>` / `HashCode.Add<!0>` via the
            // ambient type-typar set installed here. These rows land inside
            // this union's contiguous method range, after its members; their
            // count (+3) is in `unionMethodTotal` above. The ambient `!0`
            // mapping must be live while the comparer/hash refs and the
            // `!0`-relative signatures are minted — some lazily, during body
            // build — so it spans the whole block. A `[<ReferenceEquality>]` /
            // `[<NoEquality>]` union skips the triple (and the
            // `IEquatable<Self>` `InterfaceImpl` row that pairs with it).
            let emitsUnionTriple = td.EqualitySupport = EqualityVerdict.Structural

            if emitsUnionTriple then
                provider.SetTypeTypars td.TypeParams

                // `(field handle, field type)` across every case, in declaration
                // order. The flat walk is sound because inactive-case fields are
                // always default (see `Emit.UnionEqualitySupport`). A generic
                // union's field/tag handles are `MemberRef`s on its own
                // `TypeSpec`; a monomorphic union's are `Def`s.
                let allFields =
                    [
                        for c in cases do
                            let handles = caseFields |> List.find (fun (n, _) -> n = c.Name) |> snd

                            for fi in 0 .. c.Fields.Length - 1 ->
                                let fieldHandle =
                                    if isGeneric then
                                        icodegen.GenericUnionMemberRef(
                                            td.Name,
                                            typarMarkers,
                                            UnionMember.Field(c.Name, fi)
                                        )
                                    else
                                        handles.[fi]

                                fieldHandle, Emit.zonk (snd c.Fields.[fi])
                    ]

                let support: Emit.UnionEqualitySupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericUnionSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyUnion(td.Name, typarMarkers)
                        TagField =
                            if isGeneric then
                                icodegen.GenericUnionMemberRef(td.Name, typarMarkers, UnionMember.Tag)
                            else
                                tagField
                        Fields = allFields
                        IntType = TyConst "int"
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                // A generic union's triple bodies declare an `other` local
                // typed in its own `!0` (`List<!0>`), so they encode locals
                // through the generic encoder (`memberEncodeLocals`, =
                // `encodeLocals` for a monomorphic union).
                let getHashCodeBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionGetHashCode support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "GetHashCode",
                    provider.GetHashCodeOverrideSignature(),
                    getHashCodeBody,
                    addParams []
                )
                |> ignore

                methodCount <- methodCount + 1

                let equalsBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionEquals support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "Equals",
                    provider.EqualsOverrideSignature(),
                    equalsBody,
                    addParams [ "obj" ]
                )
                |> ignore

                methodCount <- methodCount + 1

                // The typed `IEquatable<Self>::Equals(Self)` — the boxing-free
                // path `EqualityComparer<Self>.Default` reaches once the union
                // declares `IEquatable<Self>` (the `InterfaceImpl` row is added
                // with the union's `TypeDefinition` below). Same field walk,
                // `Self` operand. For a generic union `Self` is `List<!0>` (the
                // ambient set makes `EqualsTypedSignature` write it).
                let equalsTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionEqualsTyped support))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "Equals",
                    provider.EqualsTypedSignature(TyUnion(td.Name, typarMarkers)),
                    equalsTypedBody,
                    addParams [ "other" ]
                )
                |> ignore

                methodCount <- methodCount + 1

                provider.ClearTypeTypars()

            // Structural-comparison pair for a union whose C-Attr verdict is
            // `Structural` (records-plan §B6). Mirrors the equality
            // triple block above — same ambient `!0` map, same `allFields`
            // walk over every payload field in declaration order. Adds
            // `CompareTo(Self)` then `CompareTo(object)`; the obj-typed entry
            // delegates to the typed one, so the typed handle is captured
            // first. Skipped by default since brainstorm-comparison §9 is
            // opt-in.
            let emitsUnionComparison = td.ComparisonSupport = ComparisonVerdict.Structural

            if emitsUnionComparison then
                provider.SetTypeTypars td.TypeParams

                let allFieldsForCmp =
                    [
                        for c in cases do
                            let handles = caseFields |> List.find (fun (n, _) -> n = c.Name) |> snd

                            for fi in 0 .. c.Fields.Length - 1 ->
                                let fieldHandle =
                                    if isGeneric then
                                        icodegen.GenericUnionMemberRef(
                                            td.Name,
                                            typarMarkers,
                                            UnionMember.Field(c.Name, fi)
                                        )
                                    else
                                        handles.[fi]

                                fieldHandle, Emit.zonk (snd c.Fields.[fi])
                    ]

                let cmpSupport: Emit.UnionComparisonSupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericUnionSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyUnion(td.Name, typarMarkers)
                        TagField =
                            if isGeneric then
                                icodegen.GenericUnionMemberRef(td.Name, typarMarkers, UnionMember.Tag)
                            else
                                tagField
                        Fields = allFieldsForCmp
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                let compareToTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionCompareTo cmpSupport))

                let typedCompareTo =
                    ctx.AddMethodWithParamList(
                        ifaceEqualsAttrs,
                        "CompareTo",
                        provider.CompareToTypedSignature(TyUnion(td.Name, typarMarkers)),
                        compareToTypedBody,
                        addParams [ "other" ]
                    )

                methodCount <- methodCount + 1

                let compareToObjBody =
                    Cil.buildBody
                        memberEncodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildUnionCompareToObj cmpSupport (toEntity typedCompareTo)))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "CompareTo",
                    provider.CompareToOverrideSignature(),
                    compareToObjBody,
                    addParams [ "obj" ]
                )
                |> ignore

                methodCount <- methodCount + 1

                provider.ClearTypeTypars()

            unionTypes.Add(
                {
                    Name = td.Name
                    Namespace = defaultArg td.Namespace ""
                    Typars = td.TypeParams
                    FirstField = firstField
                    FirstMethod = unionCtor
                    DeclaresIEquatable = emitsUnionTriple
                    DeclaresIComparable = emitsUnionComparison
                }
            )

        // ---- Records (records-plan §B2/B4) ----
        //
        // Per record: one ctor + the structural-equality triple (when the record
        // is all-immutable). `recordTypes` carries the (name, namespace, typars,
        // firstField, ctor-handle, declares-IEquatable) tuple used to add the
        // `TypeDefinition` row + `InterfaceImpl` row + `GenericParam` rows below,
        // exactly mirroring the union path's `unionTypes`.
        // Mirrors `unionTypes` — see its comment.
        let recordTypes = ResizeArray<EmittedTypeRow>()

        for (td, fields, members) in recordDecls do
            let firstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)
            let isGeneric = not (List.isEmpty td.TypeParams)
            // C-Attr verdict drives both the triple emission and the
            // `IEquatable<Self>` `InterfaceImpl` row (records-plan §B4). The
            // default for a record was previously `isAllImmutable`; that rule
            // now lives on `info.EqualitySupport` (NameResolution).
            let emitsEqualityTriple = td.EqualitySupport = EqualityVerdict.Structural
            let typarMarkers = [ for n in td.TypeParams -> TyConst n ]

            // One `public` field per record field, in declaration order. A
            // generic record's field signature uses the type's own typars
            // (`Value : !0`); a monomorphic record's is concrete. The handle list
            // is paired with `(name, type)` for `EmittedRecord` so `FieldGet` /
            // `FieldSet` can look up by source field name.
            let fieldHandles =
                fields
                |> List.map (fun f ->
                    let sigBlob =
                        if isGeneric then
                            provider.GenericFieldSignature(td.TypeParams, f.Type)
                        else
                            provider.FieldSignature f.Type

                    let h = ctx.AddField(FieldAttributes.Public, f.Name, sigBlob)
                    fieldCount <- fieldCount + 1
                    f.Name, toEntity h, f.Type
                )

            // The ctor body: chain to `Object::.ctor()`, then store each `ldarg.(i+1)`
            // into its field. The shared `Emit.buildRecordCtor` is structurally
            // identical to a closure ctor (records-plan §B3).
            let ctorBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (Emit.buildRecordCtor provider.ObjectCtorRef [ for (_, h, _) in fieldHandles -> h ]))

            let ctorSig =
                if isGeneric then
                    provider.GenericRecordCtorSignature(td.TypeParams, [ for f in fields -> f.Type ])
                else
                    provider.ClosureCtorSignature [ for f in fields -> f.Type ]

            let recordCtor =
                ctx.AddMethodWithParamList(
                    ctorAttrs,
                    ".ctor",
                    ctorSig,
                    ctorBodyOffset,
                    addParams [ for f in fields -> f.Name ]
                )

            claimFirstMethod recordCtor
            methodCount <- methodCount + 1

            // Register the record before any aug-member body / equality triple
            // body builds, so they resolve their own record (none reference it
            // today, but matches the union shape).
            records.[td.Name] <-
                {
                    Name = td.Name
                    Typars = td.TypeParams
                    Fields = fieldHandles
                    Ctor = toEntity recordCtor
                }

            // v1: records have no augmentation members. `members` is always [];
            // this block is the placeholder slot the union loop fills for P3d.3.
            ignore members

            // Structural-equality triple for a record whose C-Attr verdict is
            // `Structural` (records-plan §B4). The ambient `!0` map is
            // installed for the duration so a typar-typed field reaches
            // `EqualityComparer<!0>` / `HashCode.Add<!0>` and the `isinst` /
            // `other` / typed-`Equals` self resolve to the record's own
            // `TypeSpec` (`Box<!0>`). A `Reference` / `NoEquality` record skips
            // it (BCL `Object.Equals` reference identity; no
            // `IEquatable<Self>`).
            if emitsEqualityTriple then
                provider.SetTypeTypars td.TypeParams

                let memberEncodeLocals =
                    if isGeneric then
                        fun locals -> provider.EncodeGenericLocalSignature(td.TypeParams, locals)
                    else
                        encodeLocals

                let allFields =
                    [
                        for (name, _, fty) in fieldHandles ->
                            let fieldHandle =
                                if isGeneric then
                                    icodegen.GenericRecordMemberRef(td.Name, typarMarkers, RecordMember.Field name)
                                else
                                    let _, h, _ = fieldHandles |> List.find (fun (n, _, _) -> n = name)
                                    h

                            fieldHandle, Emit.zonk fty
                    ]

                let support: Emit.RecordEqualitySupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericRecordSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyRecord(td.Name, typarMarkers)
                        Fields = allFields
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                let getHashCodeBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordGetHashCode support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "GetHashCode",
                    provider.GetHashCodeOverrideSignature(),
                    getHashCodeBody,
                    addParams []
                )
                |> ignore

                methodCount <- methodCount + 1

                let equalsBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordEquals support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "Equals",
                    provider.EqualsOverrideSignature(),
                    equalsBody,
                    addParams [ "obj" ]
                )
                |> ignore

                methodCount <- methodCount + 1

                let equalsTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordEqualsTyped support))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "Equals",
                    provider.EqualsTypedSignature(TyRecord(td.Name, typarMarkers)),
                    equalsTypedBody,
                    addParams [ "other" ]
                )
                |> ignore

                methodCount <- methodCount + 1

                provider.ClearTypeTypars()

            // Structural-comparison pair for a record whose C-Attr verdict is
            // `Structural` (records-plan §B6). Mirrors the equality
            // triple block above — same ambient `!0` map, same field walk in
            // declaration order. Adds `CompareTo(Self)` then `CompareTo(object)`;
            // the obj-typed entry delegates to the typed one. Skipped by
            // default since brainstorm-comparison §9 is opt-in.
            let emitsRecordComparison = td.ComparisonSupport = ComparisonVerdict.Structural

            if emitsRecordComparison then
                provider.SetTypeTypars td.TypeParams

                let memberEncodeLocals =
                    if isGeneric then
                        fun locals -> provider.EncodeGenericLocalSignature(td.TypeParams, locals)
                    else
                        encodeLocals

                let allFieldsForCmp =
                    [
                        for (name, _, fty) in fieldHandles ->
                            let fieldHandle =
                                if isGeneric then
                                    icodegen.GenericRecordMemberRef(td.Name, typarMarkers, RecordMember.Field name)
                                else
                                    let _, h, _ = fieldHandles |> List.find (fun (n, _, _) -> n = name)
                                    h

                            fieldHandle, Emit.zonk fty
                    ]

                let cmpSupport: Emit.RecordComparisonSupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericRecordSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyRecord(td.Name, typarMarkers)
                        Fields = allFieldsForCmp
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                let compareToTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordCompareTo cmpSupport))

                let typedCompareTo =
                    ctx.AddMethodWithParamList(
                        ifaceEqualsAttrs,
                        "CompareTo",
                        provider.CompareToTypedSignature(TyRecord(td.Name, typarMarkers)),
                        compareToTypedBody,
                        addParams [ "other" ]
                    )

                methodCount <- methodCount + 1

                let compareToObjBody =
                    Cil.buildBody
                        memberEncodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildRecordCompareToObj cmpSupport (toEntity typedCompareTo)))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "CompareTo",
                    provider.CompareToOverrideSignature(),
                    compareToObjBody,
                    addParams [ "obj" ]
                )
                |> ignore

                methodCount <- methodCount + 1

                provider.ClearTypeTypars()

            recordTypes.Add(
                {
                    Name = td.Name
                    Namespace = defaultArg td.Namespace ""
                    Typars = td.TypeParams
                    FirstField = firstField
                    FirstMethod = recordCtor
                    DeclaresIEquatable = emitsEqualityTriple
                    DeclaresIComparable = emitsRecordComparison
                }
            )

        // ---- Closures (leaves-first) ----
        //
        // Emit each closure's capture fields, build its ctor + `Invoke` bodies,
        // then add its method rows. Defer the `TypeDefinition` rows (collected
        // here) until every field/method row exists, so the type ranges are
        // contiguous.
        //
        // A *generic* closure (function-representation-plan §Generic closures, C3) installs its typars as the
        // ambient `closureTyparRoots` set around every signature/body emission,
        // so a free `TyVar` in a capture type / `ParamTy` / `ResultTy` resolves
        // to the closure type's `GenericTypeParameter` (`!i`). Capture-field
        // loads inside its own `Invoke` use `MemberRef`s on its self-`TypeSpec`
        // (`<closure>$n<!0, !1, …>::capture_i`), the same shape generic unions
        // use for their own factory bodies (P3d.4).
        let closureTypes =
            ResizeArray<string * TypeVar list * EntityHandle * FieldDefinitionHandle * MethodDefinitionHandle>()

        for c in closures do
            let firstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)
            let captureFields = Dictionary<NodeKey, EntityHandle>()
            let isGenericClosure = not (List.isEmpty c.Typars)

            // The closure's own typars as `SemType` args (TyVars over their
            // union-find roots). Used as the instantiation argument list when
            // minting `MemberRef`s on the closure's *self*-`TypeSpec` from
            // inside its own emission — with `closureTyparRoots = c.Typars`
            // installed, each encodes to `!i` via `closureTyparLeaf`.
            let selfArgs = c.Typars |> List.map TyVar

            if isGenericClosure then
                provider.SetClosureTypars c.Typars

            let fieldHandles =
                c.Captures
                |> List.mapi (fun i (k, ty) ->
                    // Field signature: monomorphic closure → concrete encoding;
                    // generic closure → `!i` for typar-typed captures (the
                    // ambient `closureTyparLeaf` resolves free `TyVar`s).
                    let h =
                        ctx.AddField(FieldAttributes.Public, sprintf "capture%d" i, provider.FieldSignature ty)

                    // Generic closure: the handle that `stfld` (in the ctor) and
                    // `ldfld` (in `Invoke`, via `captureFields`) reference is a
                    // `MemberRef` on the closure's self-`TypeSpec`. Monomorphic
                    // keeps the `Def` token.
                    let handleForUse =
                        if isGenericClosure then
                            icodegen.GenericClosureMemberRef(c.Name, selfArgs, ClosureMember.CaptureField i)
                        else
                            toEntity h

                    captureFields.[k] <- handleForUse
                    fieldCount <- fieldCount + 1
                    handleForUse
                )

            // The closure derives from `System.Object` and implements
            // `Vesper.Fun\`2` (R1) — its ctor chains to `Object::.ctor()`, not the
            // old protected `FSharpFunc\`2::.ctor()`.
            let ctorBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef fieldHandles))

            let invokeBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (
                        Emit.buildClosureInvoke
                            icodegen
                            ctx
                            closureByNode
                            ctorHandleByNode
                            unions
                            records
                            staticMethods
                            c
                            captureFields
                    ))

            let ctorHandle =
                ctx.AddMethodWithParamList(
                    ctorAttrs,
                    ".ctor",
                    provider.ClosureCtorSignature(List.map snd c.Captures),
                    ctorBodyOffset,
                    addParams (argNames (List.length c.Captures))
                )

            ctx.AddMethodWithParamList(
                invokeAttrs,
                "Invoke",
                provider.InvokeSignature(c.ParamTy, c.ResultTy),
                invokeBodyOffset,
                addParams [ "arg0" ]
            )
            |> ignore

            claimFirstMethod ctorHandle

            // Monomorphic closures: the construction-site `Newobj` targets the
            // ctor's `Def` directly (via this dict). Generic closures: the
            // construction site mints a fresh `MemberRef` on the use-site's
            // `TypeSpec` instantiation through `provider.GenericClosureMemberRef`
            // (the dict is left unpopulated; `buildExpr TExpr.Lambda` branches
            // on `closure.Typars`).
            if not isGenericClosure then
                ctorHandleByNode.[c.Node] <- toEntity ctorHandle

            // `Fun\`2<param, result>` interface `TypeSpec` — with the closure's
            // ambient still installed, free `TyVar`s in `ParamTy` / `ResultTy`
            // encode to `!i` (so the `InterfaceImpl` row below is
            // `Fun\`2<!0, !1>`, the type's own view of itself).
            let ifaceSpec = provider.FunInterfaceSpec(c.ParamTy, c.ResultTy)

            // Generic closure: own typars owned by the predicted `TypeDefinition`
            // — collected into the shared sort buffer (the assembler emits
            // `GenericParam` rows globally sorted by owner + index). Clear the
            // ambient now that every signature/spec encoding is done.
            if isGenericClosure then
                let closureHandle =
                    toEntity (
                        MetadataTokens.TypeDefinitionHandle(
                            2 + interfaceCount + unionCount + recordCount + closureTypes.Count
                        )
                    )

                c.Typars
                |> List.iteri (fun i _ -> genericParams.Add(closureHandle, i, sprintf "T%d" i))

                provider.ClearClosureTypars()

            closureTypes.Add(c.Name, c.Typars, ifaceSpec, firstField, ctorHandle)

        // ---- Static methods (P3b) ----
        //
        // Their handles were predicted above, so recursion / cross-calls already
        // resolve; bodies reference the (now complete) closure ctor handles +
        // union factories. Emitted in `staticFnsEmitOrder` (named-holder groups
        // first, then holder-less), tracking each holder's first `MethodDef` so its
        // holder `TypeDefinition` can claim the contiguous range below.
        let mutable holderlessFirstMethod = ValueNone

        let namedHolderFirstMethod =
            Dictionary<string option * string, MethodDefinitionHandle>(HashIdentity.Structural)

        for fn in staticFnsEmitOrder do
            // A *generic* static method (`fold`, R3): install its typar set as the
            // ambient `!!i` context for the duration of its signature / locals /
            // body emission (the body's locals encode through `encodeLocals` =
            // `EncodeLocalSignature` = `encodeType`, so the ambient set reaches
            // them; the recursive self-call mints a `MethodSpec` over its own
            // typars). Empty typars ⇒ the monomorphic path, unchanged.
            let typars = staticMethods.[fn.Key].Typars
            provider.SetMethodTypars typars

            let bodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (
                        Emit.buildStaticMethod
                            icodegen
                            ctx
                            closureByNode
                            ctorHandleByNode
                            unions
                            records
                            staticMethods
                            fn
                    ))

            let signature =
                if List.isEmpty typars then
                    provider.StaticMethodSignature(fn.Params |> List.map snd, fn.ResultTy)
                else
                    provider.GenericStaticFnSignature(List.length typars, fn.Params |> List.map snd, fn.ResultTy)

            let handle =
                ctx.AddMethodWithParamList(
                    staticMethodAttrs,
                    fn.Name,
                    signature,
                    bodyOffset,
                    addParams (argNames (List.length fn.Params))
                )

            // The method's own typars are owned by this `MethodDefinition`
            // (collected here, emitted in the globally-sorted `GenericParam` pass).
            typars
            |> List.iteri (fun i _ -> genericParams.Add(toEntity handle, i, sprintf "T%d" i))

            provider.ClearMethodTypars()
            claimFirstMethod handle

            match fn.Holder with
            | Some h ->
                if not (namedHolderFirstMethod.ContainsKey h) then
                    namedHolderFirstMethod.[h] <- handle
            | None ->
                if holderlessFirstMethod.IsNone then
                    holderlessFirstMethod <- ValueSome handle

        // ---- Main (executable only) ----
        let mainDef =
            if emitEntryPoint then
                let mainBodyOffset =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (
                            Emit.buildMain
                                icodegen
                                ctx
                                closureByNode
                                ctorHandleByNode
                                unions
                                records
                                staticMethods
                                lowered
                        ))

                let handle =
                    ctx.AddMethodWithParamList(
                        MethodAttributes.Public
                        ||| MethodAttributes.Static
                        ||| MethodAttributes.HideBySig,
                        "Main",
                        mainSignature (),
                        mainBodyOffset,
                        addParams [ "args" ]
                    )

                claimFirstMethod handle
                ValueSome handle
            else
                ValueNone

        // ---- TypeDefinition rows, in ascending field/method order ----

        let firstMethodHandle =
            match firstMethod with
            | ValueSome h -> h
            // A degenerate library with no methods at all: `<Module>` points past
            // the end of the (empty) method table.
            | ValueNone -> MetadataTokens.MethodDefinitionHandle(1)

        ctx.AddModuleType(firstMethodHandle)

        // Interfaces have no fields, so their field range is empty and starts at 1
        // (≤ the first union/closure field, which also starts at row 1).
        let emptyFirstField = MetadataTokens.FieldDefinitionHandle(1)

        for (td, firstIfaceMethod) in interfacePending do
            let ns =
                match td.Namespace with
                | Some n -> n
                | None -> ""

            let metaName =
                if List.isEmpty td.TypeParams then
                    td.Name
                else
                    sprintf "%s`%d" td.Name (List.length td.TypeParams)

            let typeHandle =
                ctx.AddInterfaceType(ns, metaName, emptyFirstField, firstIfaceMethod)

            // The type's own typars are owned by this TypeDef (the metadata name
            // drops the F# leading quote: `'A` ⇒ `A`).
            td.TypeParams
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n.TrimStart('\'')))

        for row in unionTypes do
            let metaName =
                if List.isEmpty row.Typars then
                    row.Name
                else
                    sprintf "%s`%d" row.Name (List.length row.Typars)

            let typeHandle =
                ctx.AddClass(unionAttrs, row.Namespace, metaName, provider.ObjectType, row.FirstField, row.FirstMethod)

            // A union whose C-Attr verdict is `Structural` (default) declares
            // `IEquatable<Self>` — the typed `Equals` emitted above implements
            // it. For a generic union the arg is its own `TypeSpec` self
            // (`List<!0>`); the ambient `!0` mapping makes
            // `EquatableInterfaceSpec` write it. The `InterfaceImpl` table is
            // sorted by `Class`: union `TypeDefinition`s are added here in
            // ascending row order, *before* the closure loop adds its own
            // `InterfaceImpl` rows (closures sort after unions), so the table
            // stays ordered. `[<ReferenceEquality>]` / `[<NoEquality>]` unions
            // skip the row (records-plan §B4). A `[<StructuralComparison>]`
            // union additionally declares `IComparable<Self>` + `IComparable`
            // for its emitted `CompareTo` pair (records-plan §B6); the
            // SRM `InterfaceImpl` writer sorts ties on Interface coded index
            // internally, so the order of these `Add` calls on a single class
            // does not need to match the final row order.
            if row.DeclaresIEquatable || row.DeclaresIComparable then
                provider.SetTypeTypars row.Typars

                let selfMarkers = [ for t in row.Typars -> TyConst t ]

                if row.DeclaresIEquatable then
                    ctx.AddInterfaceImplementation(
                        typeHandle,
                        provider.EquatableInterfaceSpec(TyUnion(row.Name, selfMarkers))
                    )

                if row.DeclaresIComparable then
                    ctx.AddInterfaceImplementation(
                        typeHandle,
                        provider.ComparableInterfaceSpec(TyUnion(row.Name, selfMarkers))
                    )

                    ctx.AddInterfaceImplementation(typeHandle, provider.IComparableType)

                provider.ClearTypeTypars()

            // A generic union's typars are owned by this TypeDef (collected here,
            // emitted sorted with the rest — the metadata name drops the F# quote).
            row.Typars
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n.TrimStart('\'')))

        // Records mirror unions: sealed reference class derived from `Object`,
        // declares `IEquatable<Self>` when its triple was emitted (all-immutable),
        // generic typars owned by the `TypeDefinition` (records-plan §B2/B4).
        // These rows land between the union and closure TypeDefinitions so the
        // ascending `InterfaceImpl` / `GenericParam` order holds.
        for row in recordTypes do
            let metaName =
                if List.isEmpty row.Typars then
                    row.Name
                else
                    sprintf "%s`%d" row.Name (List.length row.Typars)

            let typeHandle =
                ctx.AddClass(unionAttrs, row.Namespace, metaName, provider.ObjectType, row.FirstField, row.FirstMethod)

            // Skip the `IEquatable<Self>` declaration for a mutable record (its
            // triple was skipped above, so there's nothing to bind here). A
            // `[<StructuralComparison>]` record additionally declares
            // `IComparable<Self>` + `IComparable` for its emitted `CompareTo`
            // pair (records-plan §B6).
            if row.DeclaresIEquatable || row.DeclaresIComparable then
                provider.SetTypeTypars row.Typars

                let selfMarkers = [ for t in row.Typars -> TyConst t ]

                if row.DeclaresIEquatable then
                    ctx.AddInterfaceImplementation(
                        typeHandle,
                        provider.EquatableInterfaceSpec(TyRecord(row.Name, selfMarkers))
                    )

                if row.DeclaresIComparable then
                    ctx.AddInterfaceImplementation(
                        typeHandle,
                        provider.ComparableInterfaceSpec(TyRecord(row.Name, selfMarkers))
                    )

                    ctx.AddInterfaceImplementation(typeHandle, provider.IComparableType)

                provider.ClearTypeTypars()

            row.Typars
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n.TrimStart('\'')))

        // Each closure derives from `System.Object` and implements its
        // `Vesper.Fun\`2<param, result>` interface (R1). The `InterfaceImpl` table
        // is sorted by `Class`, and these `TypeDefinition`s are added ascending, so
        // adding each row right after its `AddClass` keeps the table ordered. A
        // *generic* closure (function-representation-plan §Generic closures, C3) wears the arity suffix
        // (`<closure>$n\`N`) on its metadata name, matching how generic unions
        // and records mint their suffixed metaName.
        for (name, typars, ifaceSpec, firstField, ctorHandle) in closureTypes do
            let metaName =
                if List.isEmpty typars then
                    name
                else
                    sprintf "%s`%d" name (List.length typars)

            let closureHandle =
                ctx.AddClass(closureAttrs, "", metaName, provider.ObjectType, firstField, ctorHandle)

            ctx.AddInterfaceImplementation(closureHandle, ifaceSpec)

        // A holder owns no fields, so every holder's field range is empty and
        // starts past the last (closure/union) field.
        let holderFirstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)

        // Named-module holders (R3 deferred): one `abstract sealed` static class
        // per `module Foo` (`Vesper.Collections.ListModule`), added in the same
        // first-appearance order their methods were emitted — so their
        // `TypeDefinition` rows stay ascending by first method, and precede the
        // "Program" holder (whose holder-less methods follow theirs).
        for (holderKey, _) in namedHolderGroups do
            let ns, holderName = holderKey

            ctx.AddProgramType(
                defaultArg ns "",
                holderName,
                provider.ObjectType,
                holderFirstField,
                namedHolderFirstMethod.[holderKey]
            )
            |> ignore

        // The anonymous "Program" holder owns the holder-less static methods (and
        // `Main`, when an executable). Emit it only when it owns something — a
        // library of just interfaces / unions / named-module holders has no
        // "Program" type.
        if emitEntryPoint || not (List.isEmpty holderlessFns) then
            // Its method range starts at the first holder-less static method, else
            // `Main`. (The guard above guarantees one of these exists.)
            let programFirstMethod =
                match holderlessFirstMethod, mainDef with
                | ValueSome h, _ -> h
                | _, ValueSome m -> m
                | ValueNone, ValueNone -> firstMethodHandle

            ctx.AddProgramType("", project.ModuleName, provider.ObjectType, holderFirstField, programFirstMethod)
            |> ignore

        // Now every TypeDef / MethodDef handle exists: add the `GenericParam` rows
        // in the order SRM validates — by the owner's `TypeOrMethodDef` coded index,
        // then by parameter index.
        genericParams
        |> Seq.sortBy (fun (owner, index, _) -> (CodedIndex.TypeOrMethodDef owner, index))
        |> Seq.iter (fun (owner, index, name) -> ctx.AddGenericParameter(owner, index, name) |> ignore)

        // An executable serialises with its entry point; a library has none.
        let pe =
            match mainDef with
            | ValueSome m -> ctx.Serialize(m)
            | ValueNone -> ctx.SerializeLibrary()

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = pe
            // Captured after every body build above ran its encoders/recipes, so
            // the reference set + dependency set are complete (empty FSharp.Core
            // deps ⇒ a BCL-only-plus-Vesper PE).
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }

    /// TAST + symbol context → in-memory PE artifact. The `symbols` provider is the
    /// front end's resolution stack, now *read* by codegen (P4): a frozen
    /// `TExpr.ExternalMember`'s `SymbolKey` is minted into a `MemberRef` through it
    /// (`ClrProvider.ExternalMemberRef`). `ProjectInfo.OutputKind` routes to the
    /// executable (`Main` + `Program`) or library (declared types, no entry point)
    /// tail of the one converged assembler.
    /// `compile` plus the cross-package inline bodies (milestone M): a referenced
    /// package's `val inline` whose `.fs` body is *spliced* at each use site rather
    /// than called as a compiled member (`SymbolProviders.inlineBodies`). The map
    /// is threaded to `Emit.lowerWith`, which expands a saturated `External(name)`
    /// call head found in it. `compile` passes an empty map (the pure-local-inline
    /// path); a driver that references a manifest with `impl` bodies uses this.
    let compileWithInlines
        (externalInlines: Map<string, TDecl>)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tast: TastFile)
        : ClrArtifact =
        match project.OutputKind with
        | Library -> assemble externalInlines symbols project tast false
        | Exe -> assemble externalInlines symbols project tast true

    let compile (symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: TastFile) : ClrArtifact =
        compileWithInlines Map.empty symbols project tast

    /// Assemble a hand-written `Main` body that drives the untyped `Il` surface
    /// directly — the testable seam for hand-written bodies (e.g. an `IlIr`
    /// buffer lowered via `IlIr.lower`), independent of any TAST.
    let assembleMainEmit (project: ProjectInfo) (build: Il -> unit) : ClrArtifact =
        assembleWith project (fun _ _ -> build)

    /// The serialised PE bytes.
    let toBytes (artifact: ClrArtifact) : byte[] = artifact.Pe.ToArray()

    /// The only side effect: write the PE to `OutputPath` when one is set.
    let materialise (artifact: ClrArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            use stream = new FileStream(path, FileMode.Create, FileAccess.Write)
            artifact.Pe.WriteContentTo(stream)
        | None -> ()

    /// TFM + shared-framework version to target, derived from the runtime the
    /// codegen host runs on. The emitted `AssemblyRef`s bind against exactly
    /// the assemblies loaded in this process (`project_dotnet_provider_stack`),
    /// so the produced app must run on the same major — we read the version
    /// straight off the host rather than guess.
    let private hostFramework () : string * string =
        let v = System.Environment.Version
        sprintf "net%d.%d" v.Major v.Minor, sprintf "%d.%d.0" v.Major v.Minor

    /// The `runtimeconfig.json` a framework-dependent console app needs beside
    /// its dll — `dotnet <app>.dll` refuses to start without one. Targets the
    /// shared `Microsoft.NETCore.App`; `rollForward: Major` lets it run on a
    /// newer installed runtime than the exact version requested.
    let private runtimeConfigJson (tfm: string) (frameworkVersion: string) : string =
        System.String.Join(
            "\n",
            [
                "{"
                "  \"runtimeOptions\": {"
                sprintf "    \"tfm\": \"%s\"," tfm
                "    \"rollForward\": \"Major\","
                "    \"framework\": {"
                "      \"name\": \"Microsoft.NETCore.App\","
                sprintf "      \"version\": \"%s\"" frameworkVersion
                "    }"
                "  }"
                "}"
                ""
            ]
        )

    /// Materialise a *runnable* framework-dependent app: the PE (via
    /// `materialise`), its `runtimeconfig.json`, and a copy of every referenced
    /// assembly the shared framework does *not* carry (the `Vesper.*` libraries, an
    /// FSharp.Core cold path) into the PE's directory, where the loader's app-base
    /// probe finds it. `System.Private.CoreLib` and the rest of the BCL resolve from
    /// the shared framework automatically. After this, `dotnet <OutputPath>` runs the
    /// program. Requires `OutputPath`.
    let materialiseApp (project: ProjectInfo) (artifact: ClrArtifact) : unit =
        match artifact.OutputPath with
        | None -> failwith "Codegen.materialiseApp: ProjectInfo.OutputPath must be set"
        | Some dllPath ->
            let dir = Path.GetDirectoryName dllPath
            Directory.CreateDirectory dir |> ignore
            materialise artifact

            let tfm, frameworkVersion =
                match project.TargetFramework with
                | Some t ->
                    let v = System.Environment.Version
                    t, sprintf "%d.%d.0" v.Major v.Minor
                | None -> hostFramework ()

            File.WriteAllText(
                Path.Combine(dir, project.AssemblyName + ".runtimeconfig.json"),
                runtimeConfigJson tfm frameworkVersion
            )

            // Copy each assembly the PE actually binds against (`ReferencedAssemblies`)
            // for which we have a source. The source for a simple name is the
            // `ProjectInfo.References` entry that supplied it; for the two
            // host-resolved fallbacks the provider allows — FSharp.Core (the R9
            // cold-printf island) and Vesper.Printf (the happy-path formatter) — the
            // host-loaded copy, unless a reference already overrides it. A name with
            // no source (the BCL) resolves from the shared framework and is skipped.
            // A reference the PE never bound against is absent from the set, so it is
            // not shipped — that is what keeps a happy-path bundle FSharp.Core-free.
            let referenceSources =
                let fromProject =
                    project.References
                    |> List.map (fun path -> AssemblyName.GetAssemblyName(path).Name, path)
                    |> Map.ofList

                let withFallback name (hostPath: unit -> string) m =
                    if Map.containsKey name m then
                        m
                    else
                        Map.add name (hostPath ()) m

                fromProject
                |> withFallback "FSharp.Core" (fun () -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location)
                |> withFallback "Vesper.Printf" (fun () -> typeof<Vesper.PrintfRuntime>.Assembly.Location)

            for refName in artifact.ReferencedAssemblies do
                match Map.tryFind refName referenceSources with
                | Some src ->
                    // The loader probes the app base by *simple name*, so the
                    // destination file is always `<simpleName>.dll` regardless of the
                    // source file name.
                    let dst = Path.Combine(dir, refName + ".dll")

                    if
                        not (
                            System.String.Equals(
                                Path.GetFullPath src,
                                Path.GetFullPath dst,
                                System.StringComparison.OrdinalIgnoreCase
                            )
                        )
                    then
                        File.Copy(src, dst, true)
                | None -> ()
