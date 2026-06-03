namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// The converged assembler (G9 / rung-2 P3c): one spine emits every declared
/// type and the module's value-bearing members regardless of output kind —
/// interfaces (bodyless abstract methods), unions/records/classes (via
/// `NominalEmit.emit`), synthesised closures, and module-level static methods.
/// The only Exe/Library difference is the tail (`EmitMain` + `emitEntryPoint`
/// passed to `Finalise`).
///
/// Metadata stays contiguous and only-backward-referencing via a fixed emission
/// order:
///   methods: interface abstract → union/record/class ctor+factories+members →
///            closure ctor+`Invoke` → static methods → `Main`;
///   fields:  type fields → closure captures;
///   types:   `<Module>` → interfaces → unions → records → classes → closures
///            → holders.
/// Two forward references are *predicted* from row counts in the constructor: a
/// nominal type's `TypeDefinition` and a static method's `MethodDefinition`.
/// `GenericParam` rows are collected and emitted last, sorted by
/// `CodedIndex.TypeOrMethodDef(owner)` then index, as SRM requires.
type internal Assembler(symbols: IExternalSymbolProvider, project: ProjectInfo, tast: TastFile) =

    let ctx = MetadataContext()
    do ctx.AddModuleAndAssembly(project.AssemblyName)

    // Referenced assemblies' identities read off their files and keyed by simple
    // name, so an emitted `AssemblyRef` matches the exact artifact, not whatever
    // the host loaded (R4).
    let references =
        project.References
        |> List.map (fun path ->
            let an = AssemblyName.GetAssemblyName path
            an.Name, an
        )
        |> Map.ofList

    let provider =
        ClrProvider(ctx, IntrinsicRepr.merge tast.IntrinsicReprTypes, references, symbols, project.AssemblyName)

    let icodegen = provider :> ICodegenProvider
    let encodeLocals (locals: SemType list) = icodegen.EncodeLocalSignature locals

    // One body-stream encoder shared by every method: a fresh encoder per body
    // would throw once a tiny body left the 4-byte-aligned builder unaligned;
    // `AddMethodBody` realigns per body internally, so reuse is correct.
    let bodyStream = ctx.BodyStream

    let lowered = Emit.lower tast.Decls
    let staticFns, staticFnKeys = Emit.collectStaticFns tast.ModuleMembers lowered

    // Order emission so every holder's methods form a contiguous `MethodDef`
    // range — named-holder groups first (first-appearance order), then the
    // holder-less ("Program") functions. Drives both handle prediction and body
    // emission.
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

    // Each static fn's typar set by binding key; a closure walked from a generic
    // static fn's body inherits this. Computed before the `staticMethods`
    // dictionary (handles aren't predicted yet).
    let staticFnTyparsMap = Dictionary<NodeKey, int>()

    do
        for fn in staticFns do
            staticFnTyparsMap.[fn.Key] <- Emit.staticFnTypars fn

    let closures, closureByNode =
        Emit.discoverClosures staticFnKeys staticFnTyparsMap lowered

    let ctorHandleByNode = Dictionary<TExpr, EntityHandle>(HashIdentity.Reference)

    let partitionedDecls = partitionTypeDecls tast.Decls
    let interfaceDecls = partitionedDecls.Interfaces
    let unionDecls = partitionedDecls.Unions
    let recordDecls = partitionedDecls.Records
    let classDecls = partitionedDecls.Classes

    let typeCounts: TypeDefCounts =
        {
            Interfaces = List.length interfaceDecls
            Unions = List.length unionDecls
            Records = List.length recordDecls
            Classes = List.length classDecls
        }

    let interfaceMethodTotal =
        interfaceDecls |> List.sumBy (fun (_, methods) -> List.length methods)

    // ---- Forward-reference prediction ----
    // Predict each nominal type's `TypeDefinition` handle (after `<Module>` and
    // prior kinds) so a field / factory / local signature can `encodeType` it
    // before the row exists. A *generic* type also registers its shape so the
    // provider can mint `MemberRef`s on its `TypeSpec`.
    do
        unionDecls
        |> List.iteri (fun i (td, cases, _) ->
            // Types are keyed by their nominal `SymbolKey` (which embeds namespace,
            // arity, and home assembly) so same-named overloads (`Choice\`2`…
            // `Choice\`7`) and same-name-different-namespace types don't collide in
            // `userTypes` / `genericUnions`.
            provider.RegisterUserType(td.Key, toEntity (predictTypeDef typeCounts NominalKind.Union i))

            if not td.TypeParams.IsEmpty then
                let shape =
                    [
                        for c in cases ->
                            c.Name,
                            [
                                for fi in 0 .. c.Fields.Length - 1 -> sprintf "%s_%d" c.Name fi, snd c.Fields.[fi]
                            ]
                    ]

                provider.RegisterGenericUnion(td.Key, EqArray.toList td.TypeParams, shape)
        )

    do
        recordDecls
        |> List.iteri (fun i (td, fields, _) ->
            provider.RegisterUserType(td.Key, toEntity (predictTypeDef typeCounts NominalKind.Record i))

            if not td.TypeParams.IsEmpty then
                let shape = [ for f in fields -> f.Name, f.Type ]
                provider.RegisterGenericRecord(td.Key, EqArray.toList td.TypeParams, shape)
        )

    do
        classDecls
        |> List.iteri (fun
                           i
                           (td,
                            _fields,
                            ctorParams,
                            _members,
                            _baseType,
                            _interfaces,
                            _isSealed,
                            _staticLets,
                            _secondaryCtors,
                            _baseCtorCall) ->
            provider.RegisterUserType(td.Key, toEntity (predictTypeDef typeCounts NominalKind.Class i))

            if not td.TypeParams.IsEmpty then
                let shape = [ for p in ctorParams -> p.Name, p.Type ]
                provider.RegisterGenericClass(td.Key, EqArray.toList td.TypeParams, shape)
        )

    // A *generic* closure is a real generic `TypeDefinition` after the nominal
    // types and before the holders; predict its handle so capture-field
    // `MemberRef`s and the construction-site `Newobj` both reach it. Monomorphic
    // closures use their `Def` tokens directly.
    do
        closures
        |> List.iteri (fun i c ->
            if c.Typars > 0 then
                let handle = toEntity (predictTypeDef typeCounts NominalKind.Closure i)
                provider.RegisterClosure(c.Name, c.Typars, c.Captures |> List.map snd, c.ParamTy, c.ResultTy, handle)
        )

    let unions = Dictionary<SymbolKey, Emit.EmittedUnion>()
    let records = Dictionary<SymbolKey, Emit.EmittedRecord>()
    let classes = Dictionary<SymbolKey, Emit.EmittedClass>()

    // Per union: `.ctor` + per-case factory + per member + the optional
    // equality triple (C-Attr `Structural`) + the optional comparison pair.
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

    // Per class: `.ctor` + per member + one virtual method per interface-impl
    // member (B-2, §5.3) + one synthesised `.cctor` when the class has
    // `static let`s + one `.ctor` overload per secondary constructor (B-11).
    // Static-let *fields* add to the field table, not here.
    let classMethodTotal =
        classDecls
        |> List.sumBy (fun (_, _, _, members, _, interfaces, _, staticLets, secondaryCtors, _) ->
            1
            + List.length members
            + List.sumBy (fun (_, ms) -> List.length ms) interfaces
            + (if List.isEmpty staticLets then 0 else 1)
            + List.length secondaryCtors
        )

    let closureMethodTotal = 2 * List.length closures

    // Static methods follow the interface, union, record, class, and closure
    // methods, so a static method's `MethodDefinition` is `staticBase + 1 + i`.
    let staticBase =
        interfaceMethodTotal
        + unionMethodTotal
        + recordMethodTotal
        + classMethodTotal
        + closureMethodTotal

    let staticMethods = Dictionary<NodeKey, Emit.StaticMethodRef>()

    do
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

    let emitCtx: Emit.EmitContext =
        {
            Provider = icodegen
            Ctx = ctx
            ClosureByNode = closureByNode
            CtorHandleByNode = ctorHandleByNode
            Unions = unions
            Records = records
            Classes = classes
            StaticMethods = staticMethods
        }

    // ---- Attribute sets ----
    let closureAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.BeforeFieldInit

    // A user class opts in to `Sealed` via `[<Sealed>]` (B-8); without it the
    // class is open (Phase 2 / B-4 wires inheritance). Unions / records reuse
    // this with `isSealed = true`.
    let classAttrsOf (isSealed: bool) =
        let baseAttrs =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.AutoLayout
            ||| TypeAttributes.AnsiClass
            ||| TypeAttributes.BeforeFieldInit

        if isSealed then
            baseAttrs ||| TypeAttributes.Sealed
        else
            baseAttrs

    let staticFactoryAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Static
        ||| MethodAttributes.HideBySig

    let staticMethodAttrs = staticFactoryAttrs

    // Non-virtual `public hidebysig` instance method (the union/record/class is
    // sealed, so `call` dispatch is correct).
    let instanceMethodAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

    // A synthesised `Object.Equals`/`GetHashCode` override: reuse the base
    // virtual slot (no `NewSlot`) so name + signature matching makes it the
    // override.
    let overrideMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig

    // Typed `IEquatable<Self>::Equals(Self)` / `IComparable<Self>::CompareTo`:
    // a *new* virtual slot (`Object` has none to reuse), `Final` since sealed.
    // The runtime binds it to the `InterfaceImpl` by name + signature.
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

    let cctorAttrs =
        MethodAttributes.Private
        ||| MethodAttributes.Static
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.SpecialName
        ||| MethodAttributes.RTSpecialName

    // A closure derives from `System.Object` and *implements* the
    // `Vesper.Fun\`2::Invoke` interface slot (R1) by name + signature; `Final`
    // because a sealed closure has no further overrides.
    let invokeAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.Final

    // ---- Mutable emission counters ----
    let mutable paramCount = 0
    let mutable firstMethod = ValueNone
    let mutable methodCount = 0
    let mutable fieldCount = 0

    // G8: route every emitted method through a real `Param` list. `Param` rows
    // are a global table referenced by each `MethodDefinition.ParamList`, so
    // they must be added in method order; call this immediately before each
    // `AddMethodWithParamList`. Returns the method's first `Param` handle
    // (past-the-end for a zero-parameter method).
    let addParams (names: string list) : ParameterHandle =
        let firstParam = MetadataTokens.ParameterHandle(paramCount + 1)

        names
        |> List.iteri (fun i n ->
            ctx.AddParameter(i + 1, n) |> ignore
            paramCount <- paramCount + 1
        )

        firstParam

    // The first emitted method row, claimed by `<Module>`.
    let claimFirstMethod (h: MethodDefinitionHandle) =
        if firstMethod.IsNone then
            firstMethod <- ValueSome h

    // ---- Deferred-row accumulators ----
    let interfacePending = ResizeArray<TTypeDecl * MethodDefinitionHandle>()

    // `GenericParam` rows can't be added inline: SRM requires them globally
    // sorted by `CodedIndex.TypeOrMethodDef(owner)`, and a method owner can sort
    // *before* its declaring type. Collect (owner, index, name) and emit sorted
    // once every handle exists.
    let genericParams = ResizeArray<EntityHandle * int * string>()

    let unionTypes = ResizeArray<EmittedTypeRow>()
    let recordTypes = ResizeArray<EmittedTypeRow>()
    let classTypes = ResizeArray<EmittedTypeRow>()
    let closureTypes = ResizeArray<EmittedTypeRow>()

    let mutable holderlessFirstMethod = ValueNone

    let namedHolderFirstMethod =
        Dictionary<string option * string, MethodDefinitionHandle>(HashIdentity.Structural)

    // ---- State exposed to `NominalEmit` and the `Codegen` orchestrator ----
    member _.Provider = provider
    member _.Icodegen = icodegen
    member _.Ctx = ctx
    member _.BodyStream = bodyStream
    member _.EncodeLocals = encodeLocals
    member _.EmitCtx = emitCtx
    member _.CtorAttrs = ctorAttrs
    member _.CctorAttrs = cctorAttrs
    member _.StaticFactoryAttrs = staticFactoryAttrs
    member _.StaticMethodAttrs = staticMethodAttrs
    member _.InstanceMethodAttrs = instanceMethodAttrs
    member _.OverrideMethodAttrs = overrideMethodAttrs
    member _.IfaceEqualsAttrs = ifaceEqualsAttrs
    member _.Unions = unions
    member _.Records = records
    member _.Classes = classes
    member _.UnionDecls = unionDecls
    member _.RecordDecls = recordDecls
    member _.ClassDecls = classDecls
    member _.UnionTypes = unionTypes
    member _.RecordTypes = recordTypes
    member _.ClassTypes = classTypes
    member _.AddParams(names: string list) : ParameterHandle = addParams names
    member _.ClaimFirstMethod(h: MethodDefinitionHandle) = claimFirstMethod h

    /// Record a `GenericParam` row owned by a *method* (a B-12 generic member's
    /// own typar). Like every other generic-param row it can't be added inline —
    /// `Finalise` emits the whole collection sorted by `TypeOrMethodDef(owner)`.
    member _.AddMethodGenericParam(owner: EntityHandle, index: int, name: string) =
        genericParams.Add(owner, index, name)

    member _.FieldCount
        with get () = fieldCount
        and set v = fieldCount <- v

    member _.MethodCount
        with get () = methodCount
        and set v = methodCount <- v

    // ---- Interfaces: bodyless abstract methods + collected generic params ----
    member this.EmitInterfaces() =
        for (td, methods) in interfaceDecls do
            let firstIfaceMethod = MetadataTokens.MethodDefinitionHandle(methodCount + 1)

            for m in methods do
                let paramTys, _ = decurry m.Signature

                let methodHandle =
                    ctx.AddMethodWithParamList(
                        abstractMethodAttrs,
                        m.Name,
                        abstractMethodSignature provider m,
                        -1,
                        addParams (argNames (List.length paramTys))
                    )

                // The method's own typars are owned by this MethodDef (metadata
                // name drops the F# leading quote).
                m.MethodTypeParams
                |> EqArray.iteri (fun i n -> genericParams.Add(toEntity methodHandle, i, n.TrimStart('\'')))

                methodCount <- methodCount + 1

            claimFirstMethod firstIfaceMethod
            interfacePending.Add(td, firstIfaceMethod)

    // ---- Closures (leaves-first) ----
    // A *generic* closure (C3) enters closure-typar mode around every
    // signature/body emission, so the body's `TempTypar(Method, i)` (the enclosing
    // method's typars) re-project onto this closure class's `!i` (frozen-type-plan 2B).
    member this.EmitClosures() =
        for c in closures do
            let firstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)
            let captureFields = Dictionary<NodeKey, EntityHandle>()
            let isGenericClosure = c.Typars > 0
            // This closure's self-instantiation over its own typars: the enclosing
            // method's `TempTypar(Method, i)`, which (in closure mode) encode to the
            // closure class's `!i`.
            let selfArgs = [ for i in 0 .. c.Typars - 1 -> TempTypar(TyparAxis.Method, i) ]

            if isGenericClosure then
                provider.EnterClosureTyparScope()

            let fieldHandles =
                c.Captures
                |> List.mapi (fun i (k, ty) ->
                    let h =
                        ctx.AddField(FieldAttributes.Public, sprintf "capture%d" i, provider.FieldSignature ty)

                    // Generic closure: `stfld` (ctor) and `ldfld` (`Invoke`)
                    // reference a `MemberRef` on the closure's self-`TypeSpec`;
                    // monomorphic keeps the `Def` token.
                    let handleForUse =
                        if isGenericClosure then
                            icodegen.UserClosureMemberRef(c.Name, selfArgs, ClosureMember.CaptureField i)
                        else
                            toEntity h

                    captureFields.[k] <- handleForUse
                    fieldCount <- fieldCount + 1
                    handleForUse
                )

            let ctorBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef fieldHandles))

            let invokeBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureInvoke emitCtx c captureFields))

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
            // ctor's `Def` directly via this dict. Generic closures mint a fresh
            // `MemberRef` at the use site instead (dict left unpopulated).
            if not isGenericClosure then
                ctorHandleByNode.[c.Node] <- toEntity ctorHandle

            // `Fun\`2<param, result>` interface `TypeSpec` — closure ambient
            // still installed, so free `TyVar`s encode to `!i`.
            let ifaceSpec = provider.FunInterfaceSpec(c.ParamTy, c.ResultTy)

            if isGenericClosure then
                let closureHandle =
                    toEntity (predictTypeDef typeCounts NominalKind.Closure closureTypes.Count)

                for i in 0 .. c.Typars - 1 do
                    genericParams.Add(closureHandle, i, sprintf "T%d" i)

                provider.ExitClosureTyparScope()

            // Closures synthesise their typar names (`T0`, …) — the source typar
            // strings aren't retained; only the count survives to codegen.
            let typarNames = [ for i in 0 .. c.Typars - 1 -> sprintf "T%d" i ]

            closureTypes.Add(
                {
                    Name = c.Name
                    Namespace = ""
                    Typars = typarNames
                    FirstField = firstField
                    FirstMethod = ctorHandle
                    Interfaces = [ ifaceSpec ]
                    IsSealed = true
                    BaseType = provider.ObjectType
                }
            )

    // ---- Static methods (P3b) ----
    // Handles were predicted in the constructor, so recursion / cross-calls
    // already resolve. Emitted in `staticFnsEmitOrder`, tracking each holder's
    // first `MethodDef` for the trailing holder `TypeDefinition`.
    member this.EmitStaticMethods() =
        for fn in staticFnsEmitOrder do
            // A *generic* static method (`fold`, R3): its body / signature / locals
            // embed `TempTypar(Method, i)` (freeze-quantified, frozen-type-plan 2B),
            // which the encoder maps to `!!i` directly — no ambient typar window.
            let typarCount = staticMethods.[fn.Key].Typars

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticMethod emitCtx fn))

            let signature =
                if typarCount = 0 then
                    provider.StaticMethodSignature(fn.Params |> List.map snd, fn.ResultTy)
                else
                    provider.GenericStaticFnSignature(typarCount, fn.Params |> List.map snd, fn.ResultTy)

            let handle =
                ctx.AddMethodWithParamList(
                    staticMethodAttrs,
                    fn.Name,
                    signature,
                    bodyOffset,
                    addParams (argNames (List.length fn.Params))
                )

            for i in 0 .. typarCount - 1 do
                genericParams.Add(toEntity handle, i, sprintf "T%d" i)

            claimFirstMethod handle

            match fn.Holder with
            | Some h ->
                if not (namedHolderFirstMethod.ContainsKey h) then
                    namedHolderFirstMethod.[h] <- handle
            | None ->
                if holderlessFirstMethod.IsNone then
                    holderlessFirstMethod <- ValueSome handle

    // ---- Main (executable only) ----
    member this.EmitMain(emitEntryPoint: bool) : MethodDefinitionHandle voption =
        if emitEntryPoint then
            let mainBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildMain emitCtx lowered))

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

    // ---- TypeDefinition rows (ascending field/method order) + serialise ----
    member this.Finalise(mainDef: MethodDefinitionHandle voption, emitEntryPoint: bool) : ClrArtifact =
        let firstMethodHandle =
            match firstMethod with
            | ValueSome h -> h
            // A degenerate library with no methods: `<Module>` points past the
            // end of the empty method table.
            | ValueNone -> MetadataTokens.MethodDefinitionHandle(1)

        ctx.AddModuleType(firstMethodHandle)

        // Interfaces have no fields, so their field range is empty and starts at
        // 1 (<= the first union/closure field, which also starts at row 1).
        let emptyFirstField = MetadataTokens.FieldDefinitionHandle(1)

        for (td, firstIfaceMethod) in interfacePending do
            let ns =
                match td.Namespace with
                | Some n -> n
                | None -> ""

            let metaName = ExternalSymbols.arityName td.Name td.TypeParams.Length

            let typeHandle =
                ctx.AddInterfaceType(ns, metaName, emptyFirstField, firstIfaceMethod)

            td.TypeParams
            |> EqArray.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n.TrimStart('\'')))

        // Union, record, and class `TypeDefinition` rows share one recipe, with
        // pre-minted `InterfaceImpl` entries. The base type is `row.BaseType`:
        // `Object` for unions / records and parent-less classes, the parent's
        // resolved `TypeSpec` for a class with an `inherit` clause (B-4 Step 2.5).
        // Unions first, then records, then classes — keeps the `InterfaceImpl` /
        // `GenericParam` rows ascending (sorted by `Class` / `TypeOrMethodDef`).
        for row in Seq.append unionTypes (Seq.append recordTypes classTypes) do
            let metaName = ExternalSymbols.arityName row.Name (List.length row.Typars)

            let attrs = classAttrsOf row.IsSealed

            let typeHandle =
                ctx.AddClass(attrs, row.Namespace, metaName, row.BaseType, row.FirstField, row.FirstMethod)

            for iface in row.Interfaces do
                ctx.AddInterfaceImplementation(typeHandle, iface)

            row.Typars
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n.TrimStart('\'')))

        // Each closure derives from `System.Object` and implements its
        // `Vesper.Fun\`2<param, result>` interface (R1).
        for row in closureTypes do
            let metaName = ExternalSymbols.arityName row.Name (List.length row.Typars)

            let closureHandle =
                ctx.AddClass(
                    closureAttrs,
                    row.Namespace,
                    metaName,
                    provider.ObjectType,
                    row.FirstField,
                    row.FirstMethod
                )

            for iface in row.Interfaces do
                ctx.AddInterfaceImplementation(closureHandle, iface)

        // A holder owns no fields, so every holder's field range is empty and
        // starts past the last (closure/union) field.
        let holderFirstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)

        // Named-module holders (R3 deferred): one static class per `module Foo`,
        // added in first-appearance order so their `TypeDefinition` rows stay
        // ascending by first method and precede the "Program" holder.
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

        // The anonymous "Program" holder owns the holder-less static methods
        // (and `Main`, when an executable). Emit it only when it owns something.
        if emitEntryPoint || not (List.isEmpty holderlessFns) then
            let programFirstMethod =
                match holderlessFirstMethod, mainDef with
                | ValueSome h, _ -> h
                | _, ValueSome m -> m
                | ValueNone, ValueNone -> firstMethodHandle

            ctx.AddProgramType("", project.ModuleName, provider.ObjectType, holderFirstField, programFirstMethod)
            |> ignore

        // Every handle now exists: add `GenericParam` rows in the order SRM
        // validates — by the owner's `TypeOrMethodDef` coded index, then index.
        genericParams
        |> Seq.sortBy (fun (owner, index, _) -> (CodedIndex.TypeOrMethodDef owner, index))
        |> Seq.iter (fun (owner, index, name) -> ctx.AddGenericParameter(owner, index, name) |> ignore)

        let pe =
            match mainDef with
            | ValueSome m -> ctx.Serialize(m)
            | ValueNone -> ctx.SerializeLibrary()

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = pe
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }
