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

/// The in-memory assembled PE plus enough to inspect / write it.
type ClrArtifact =
    {
        AssemblyName: string
        OutputPath: string option
        /// The serialised PE image.
        Pe: BlobBuilder
        /// The distinct FSharp.Core constructs the emission referenced
        /// (from `ICodegenProvider.FSharpCoreDependencies`). **Empty ⇒ the PE
        /// has no `FSharp.Core.dll` dependency**, so `materialiseApp` skips
        /// copying it; non-empty is the list of constructs still pinning it.
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
    /// class. `build` receives the wired context + provider so callers emit
    /// either from a TAST or from a hand-written op.
    let private assembleWith
        (project: ProjectInfo)
        (build: MetadataContext -> ClrProvider -> (Il -> unit))
        : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        // No TAST on this hand-written-body seam — the built-in primitive
        // representations are all it can reference.
        let provider = ClrProvider(ctx, IntrinsicRepr.defaults)
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

        ctx.AddProgramType(project.ModuleName, provider.ObjectType, MetadataTokens.FieldDefinitionHandle(1), mainDef)
        |> ignore

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = ctx.Serialize(mainDef)
            // Captured after the body build above ran every encoder/recipe.
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
    let private assemble (project: ProjectInfo) (tast: TastFile) (emitEntryPoint: bool) : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        // This file's intrinsic bindings overlay the defaults (G7). The provider's
        // refs are lazy (G6), so a typar-only / BCL-only build forces no
        // FSharp.Core `AssemblyRef`; a richer signature reuses `encodeType` (G5).
        let provider = ClrProvider(ctx, IntrinsicRepr.merge tast.IntrinsicReprTypes)
        let icodegen = provider :> ICodegenProvider
        let encodeLocals locals = icodegen.EncodeLocalSignature locals

        // One body-stream encoder shared by every method: its ctor requires a
        // 4-byte-aligned IL builder, so a fresh encoder per body would throw once
        // the first (tiny) body left the builder unaligned. `AddMethodBody`
        // realigns per body internally, so reuse is correct.
        let bodyStream = ctx.BodyStream

        let lowered = Emit.lower tast.Decls

        // Top-level functions emitted as static methods (P3b) — excluded from
        // closure discovery and resolved as direct `call`s at their use sites.
        let staticFns, staticFnKeys = Emit.collectStaticFns lowered

        let closures, closureByNode = Emit.discoverClosures staticFnKeys lowered
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

        // ---- Forward-reference prediction ----

        let interfaceCount = List.length interfaceDecls

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

        let unions = Dictionary<string, Emit.EmittedUnion>()

        // (name, namespace, typars, firstField, firstMethod) for each emitted
        // union's `TypeDefinition`, claimed after every method/field row exists.
        // `typars` drives the metadata arity suffix (`List\`1`) + `GenericParam`
        // rows for a generic union (empty ⇒ monomorphic).
        let unionTypes =
            ResizeArray<string * string * string list * FieldDefinitionHandle * MethodDefinitionHandle>()

        // Per union: the parameterless `.ctor`, one factory per case, and one
        // method per augmentation member (P3d.3).
        let unionMethodTotal =
            unionDecls
            |> List.sumBy (fun (_, cases, members) -> 1 + List.length cases + List.length members)

        let closureMethodTotal = 2 * List.length closures

        // Static methods follow the interface, union, and closure methods, so a
        // static method's `MethodDefinition` is `staticBase + 1 + i`.
        let staticBase = interfaceMethodTotal + unionMethodTotal + closureMethodTotal

        let staticMethods = Dictionary<NodeKey, Emit.StaticMethodRef>()

        staticFns
        |> List.iteri (fun i fn ->
            staticMethods.[fn.Key] <-
                {
                    Handle = toEntity (MetadataTokens.MethodDefinitionHandle(staticBase + 1 + i))
                    Arity = List.length fn.Params
                    ResultTy = fn.ResultTy
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

        let ctorAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        // Reuse-slot virtual (no `NewSlot`) so `Invoke` overrides the base
        // `FSharpFunc\`2::Invoke` abstract slot by its instantiated signature.
        let invokeAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig

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
                Cil.buildBody encodeLocals bodyStream (Emit.emitClosureCtor provider.ObjectCtorRef [])

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
                    Cil.buildBody encodeLocals bodyStream (Emit.emitUnionFactory ctorRef tag tagRef fieldRefs)

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

            members
            |> List.iteri (fun i (mem: TTypeMember) ->
                let handle = MetadataTokens.MethodDefinitionHandle(methodCount + 1 + i)

                emittedMembers.[mem.Name] <-
                    {
                        Handle = toEntity handle
                        IsStatic = mem.IsStatic
                        Arity = List.length mem.Params
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

            for mem in members do
                let bodyOffset =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (Emit.emitMember
                            icodegen
                            ctx
                            closureByNode
                            ctorHandleByNode
                            unions
                            staticMethods
                            mem.ThisKey
                            mem.Params
                            mem.Body)

                let methodName =
                    match mem.Kind with
                    | TMemberKind.Property -> "get_" + mem.Name
                    | TMemberKind.Method -> mem.Name

                let paramTys = mem.Params |> List.map snd

                let signature =
                    if mem.IsStatic then
                        provider.StaticMethodSignature(paramTys, mem.ReturnTy)
                    else
                        provider.InstanceMethodSignature(paramTys, mem.ReturnTy)

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

            unionTypes.Add(td.Name, (defaultArg td.Namespace ""), td.TypeParams, firstField, unionCtor)

        // ---- Closures (leaves-first) ----
        //
        // Emit each closure's capture fields, build its ctor + `Invoke` bodies,
        // then add its method rows. Defer the `TypeDefinition` rows (collected
        // here) until every field/method row exists, so the type ranges are
        // contiguous.
        let closureTypes =
            ResizeArray<string * EntityHandle * FieldDefinitionHandle * MethodDefinitionHandle>()

        for c in closures do
            let firstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)
            let captureFields = Dictionary<NodeKey, EntityHandle>()

            let fieldHandles =
                c.Captures
                |> List.mapi (fun i (k, ty) ->
                    let h =
                        ctx.AddField(FieldAttributes.Public, sprintf "capture%d" i, provider.FieldSignature ty)

                    captureFields.[k] <- toEntity h
                    fieldCount <- fieldCount + 1
                    toEntity h
                )

            let baseCtor = provider.FSharpFuncCtorRef(c.ParamTy, c.ResultTy)

            let ctorBodyOffset =
                Cil.buildBody encodeLocals bodyStream (Emit.emitClosureCtor baseCtor fieldHandles)

            let invokeBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (Emit.emitClosureInvoke
                        icodegen
                        ctx
                        closureByNode
                        ctorHandleByNode
                        unions
                        staticMethods
                        c
                        captureFields)

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

            ctorHandleByNode.[c.Node] <- toEntity ctorHandle
            closureTypes.Add(c.Name, provider.ClosureBaseSpec(c.ParamTy, c.ResultTy), firstField, ctorHandle)

        // ---- Static methods (P3b), owned by the `Program` holder ----
        //
        // Their handles were predicted above, so recursion / cross-calls already
        // resolve; bodies reference the (now complete) closure ctor handles +
        // union factories.
        let mutable firstStaticMethod = ValueNone

        for fn in staticFns do
            let bodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (Emit.emitStaticMethod icodegen ctx closureByNode ctorHandleByNode unions staticMethods fn)

            let handle =
                ctx.AddMethodWithParamList(
                    staticMethodAttrs,
                    fn.Name,
                    provider.StaticMethodSignature(fn.Params |> List.map snd, fn.ResultTy),
                    bodyOffset,
                    addParams (argNames (List.length fn.Params))
                )

            claimFirstMethod handle

            if firstStaticMethod.IsNone then
                firstStaticMethod <- ValueSome handle

        // ---- Main (executable only) ----
        let mainDef =
            if emitEntryPoint then
                let mainBodyOffset =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (Emit.emitMain icodegen ctx closureByNode ctorHandleByNode unions staticMethods lowered)

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

        for (name, ns, typars, firstField, firstUnionMethod) in unionTypes do
            let metaName =
                if List.isEmpty typars then
                    name
                else
                    sprintf "%s`%d" name (List.length typars)

            let typeHandle =
                ctx.AddClass(unionAttrs, ns, metaName, provider.ObjectType, firstField, firstUnionMethod)

            // A generic union's typars are owned by this TypeDef (collected here,
            // emitted sorted with the rest — the metadata name drops the F# quote).
            typars
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n.TrimStart('\'')))

        for (name, baseSpec, firstField, ctorHandle) in closureTypes do
            ctx.AddClass(closureAttrs, "", name, baseSpec, firstField, ctorHandle) |> ignore

        // The `Program` holder owns the module's static methods (and `Main`, when
        // an executable). Emit it only when it owns something — a library of just
        // interfaces / unions (rung 1, or a union-only library) has no holder.
        if emitEntryPoint || not (List.isEmpty staticFns) then
            let programFirstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)

            // Its method range starts at the first static method, else `Main`. (The
            // guard above guarantees one of these exists.)
            let programFirstMethod =
                match firstStaticMethod, mainDef with
                | ValueSome h, _ -> h
                | _, ValueSome m -> m
                | ValueNone, ValueNone -> firstMethodHandle

            ctx.AddProgramType(project.ModuleName, provider.ObjectType, programFirstField, programFirstMethod)
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
            // the dependency set is complete (empty ⇒ a BCL-only PE).
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }

    /// TAST + symbol context → in-memory PE artifact. The `symbols` provider
    /// is accepted per the shared-inputs posture; slice 1 reads everything it
    /// needs from the TAST and the target `ClrProvider`. `ProjectInfo.OutputKind`
    /// routes to the executable (`Main` + `Program`) or library (declared types,
    /// no entry point) tail of the one converged assembler.
    let compile (_symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: TastFile) : ClrArtifact =
        match project.OutputKind with
        | Library -> assemble project tast false
        | Exe -> assemble project tast true

    /// Assemble a single hand-written `Main` body (a typed `Op` from the empty
    /// stack) into an artifact. The testable seam for the `Cil` body DSL,
    /// independent of any TAST.
    let assembleMainOp (project: ProjectInfo) (op: Op<E, 'out>) : ClrArtifact =
        assembleWith project (fun _ _ -> fun il -> op null null il)

    /// Assemble a hand-written `Main` body that drives the untyped `Il`
    /// surface directly — the testable seam for bodies the typed `Op` CE
    /// doesn't yet cover (e.g. branching, which lives only on the `emit*` path).
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
    /// `materialise`), its `runtimeconfig.json`, and a copy of `FSharp.Core.dll`
    /// (which the shared framework does *not* carry) into the PE's directory,
    /// where the loader's app-base probe finds it. `System.Private.CoreLib`
    /// resolves from the shared framework automatically. After this,
    /// `dotnet <OutputPath>` runs the program. Requires `OutputPath`.
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

            // Copy FSharp.Core only when the PE actually references it. The
            // happy-path printf / arithmetic / interpolation lowerings touch no
            // FSharp.Core construct, so a program built entirely from them ships
            // without it. `FSharpCoreDependencies` is the authoritative signal:
            // every FSharp.Core reference is minted through `ClrProvider`, which
            // records each use-site, so an empty set means nothing in the IL can
            // bind against `FSharp.Core.dll`. (See its non-empty contents for the
            // constructs still pinning the dependency — the §D3 cut list.)
            if not (List.isEmpty artifact.FSharpCoreDependencies) then
                let fsharpCoreSrc =
                    match project.FSharpCorePath with
                    | Some p -> p
                    | None -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location

                let fsharpCoreDst = Path.Combine(dir, "FSharp.Core.dll")

                if
                    not (
                        System.String.Equals(
                            Path.GetFullPath fsharpCoreSrc,
                            Path.GetFullPath fsharpCoreDst,
                            System.StringComparison.OrdinalIgnoreCase
                        )
                    )
                then
                    File.Copy(fsharpCoreSrc, fsharpCoreDst, true)

            // The bootstrap printf runtime, like FSharp.Core, is absent from the
            // shared framework — copy it beside the PE so a fully-applied literal
            // printf (lowered to `Vesper.Formatter` calls) resolves at runtime.
            let vesperPrintfSrc = typeof<Vesper.PrintfRuntime>.Assembly.Location
            let vesperPrintfDst = Path.Combine(dir, "Vesper.Printf.dll")

            if
                not (
                    System.String.Equals(
                        Path.GetFullPath vesperPrintfSrc,
                        Path.GetFullPath vesperPrintfDst,
                        System.StringComparison.OrdinalIgnoreCase
                    )
                )
            then
                File.Copy(vesperPrintfSrc, vesperPrintfDst, true)
