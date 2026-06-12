namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// The converged assembler over the `AssemblyLayout`: the layout enumerates
/// every ranged-table row as data (handle = position), the constructor
/// registers forward handles and
/// writes the whole field table, the *Bind* phase pre-fills the `EmitContext`
/// registries from the layout, the *Prepare* phase builds every signature/body
/// against resolved handles (body-stream order is free), and `WriteMethods` /
/// `Finalise` walk the layout mechanically — no decisions, no row arithmetic.
/// `GenericParam` rows are collected and emitted last, sorted by
/// `CodedIndex.TypeOrMethodDef(owner)` then index, as SRM requires.
type internal Assembler(symbols: IExternalSymbolProvider, project: ProjectInfo, tast: Frozen.TastFile) =

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
    let encodeLocals (locals: FrozenType list) = icodegen.EncodeLocalSignature locals

    // One body-stream encoder shared by every method: a fresh encoder per body
    // would throw once a tiny body left the 4-byte-aligned builder unaligned;
    // `AddMethodBody` realigns per body internally, so reuse is correct.
    let bodyStream = ctx.BodyStream

    // ---- The assembly layout ----
    // One enumeration of every ranged-table row; every forward handle below is
    // a lookup into the prefix-sum derivation, not arithmetic. The layout also
    // carries the lowering products (lowered decls, holder plan, closures,
    // partition) computed once inside `Layout.build`.
    let layout = Layout.build project tast
    let layoutHandles = Layout.deriveHandles layout

    let lowered = layout.Lowered
    let plan = layout.Plan
    let closures = layout.Closures
    let closureByNode = layout.ClosureByNode

    let ctorHandleByNode =
        Dictionary<Frozen.TExpr, EntityHandle>(HashIdentity.Reference)

    let partitionedDecls = layout.Partitioned
    let interfaceDecls = partitionedDecls.Interfaces
    let unionDecls = partitionedDecls.Unions
    let recordDecls = partitionedDecls.Records
    let classDecls = partitionedDecls.Classes

    // ---- Forward-reference registration ----
    // Register each nominal type's layout-derived `TypeDefinition` handle so a
    // field / factory / local signature can `encodeType` it before the row
    // exists. A *generic* type also registers its shape so the provider can
    // mint `MemberRef`s on its `TypeSpec`.
    do
        unionDecls
        |> List.iter (fun ud ->
            let td = ud.Decl
            // Types are keyed by their nominal `SymbolKey` (which embeds namespace,
            // arity, and home assembly) so same-named overloads (`Choice\`2`…
            // `Choice\`7`) and same-name-different-namespace types don't collide in
            // `userTypes` / `genericUnions`.
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeKey.Nominal td.Key)))

            if not td.TypeParams.IsEmpty then
                let shape =
                    [
                        for c in ud.Cases ->
                            c.Name,
                            [
                                for fi in 0 .. c.Fields.Length - 1 -> sprintf "%s_%d" c.Name fi, snd c.Fields.[fi]
                            ]
                    ]

                provider.RegisterGenericUnion(td.Key, EqArray.toList td.TypeParams, shape)
        )

    do
        recordDecls
        |> List.iter (fun rd ->
            let td = rd.Decl
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeKey.Nominal td.Key)))

            if not td.TypeParams.IsEmpty then
                let shape = [ for f in rd.Fields -> f.Name, f.Type ]
                provider.RegisterGenericRecord(td.Key, EqArray.toList td.TypeParams, shape)
        )

    do
        classDecls
        |> List.iter (fun cd ->
            let td = cd.Decl
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeKey.Nominal td.Key)))

            if cd.IsStruct then
                provider.RegisterUserValueType td.Key

            if not td.TypeParams.IsEmpty then
                // The ctor-param backing fields, the explicit `val [mutable] x: T`
                // instance fields, and the `static let` backing fields must all be in
                // the generic-class registry: a generic struct's field-init ctor and
                // member-body `ldfld`/`stfld` reference the `val` fields by name
                // through a `MemberRef` on the open self-`TypeSpec`, and a generic
                // `static let` read/store (`ldsfld`/`stsfld`) goes through the same
                // `ClassMember.Field` `MemberRef` (G13) — an unregistered field fails
                // resolution ("generic class … has no field").
                let ctorParamFields = [ for p in cd.CtorParams -> p.Name, p.Type ]

                let shape =
                    ctorParamFields
                    @ [ for f in cd.Fields -> f.Name, f.Type ]
                    @ [ for sl in cd.StaticLets -> sl.Name, sl.Type ]

                provider.RegisterGenericClass(td.Key, EqArray.toList td.TypeParams, List.length ctorParamFields, shape)
        )

    // A *generic* closure is a real generic `TypeDefinition` after the nominal
    // types and before the holders; its layout-derived handle lets capture-field
    // `MemberRef`s and the construction-site `Newobj` both reach it. Monomorphic
    // closures use their `Def` tokens directly.
    do
        closures
        |> List.iter (fun c ->
            if c.Typars > 0 then
                let handle = toEntity (layoutHandles.TypeDefOf(TypeKey.Closure c.Name))

                provider.RegisterClosure(
                    c.Name,
                    c.Typars,
                    c.DeclaringTypars,
                    c.Captures |> List.map snd,
                    c.ParamTy,
                    c.ResultTy,
                    handle
                )
        )

    // ---- Field table: the writer's field pass ----
    // Tables are independent (only intra-table order matters), so the whole
    // field table is written up front, straight off the layout; every later
    // phase resolves def handles by `FieldKey` instead of adding rows. A
    // generic closure's capture-field signature encodes inside the ambient
    // closure-typar scope, bracketed per slot.
    let fieldDefHandles = Dictionary<FieldKey, FieldDefinitionHandle>()

    do
        for fs in layout.Fields do
            match fs.ClosureScope with
            | ValueSome d -> provider.EnterClosureTyparScope d
            | ValueNone -> ()

            // A leaked metavar / unresolved head in a field type (e.g. a closure
            // capture whose element typar never grounded) surfaces here as an opaque
            // encoder failure; name the field + type so the front-end grounding gap is
            // pinpointable rather than anonymous.
            let fieldSig =
                try
                    provider.FieldSignature fs.Ty
                with ex ->
                    // Wrap (not `failwithf "%s" ex.Message`) so the original
                    // encoder exception rides as `InnerException` — the stack
                    // pointing at the actual encode failure is preserved.
                    raise (System.Exception(sprintf "While encoding field '%s' : %A" fs.Name fs.Ty, ex))

            let h = ctx.AddField(fs.Attrs, fs.Name, fieldSig)

            if fs.ClosureScope.IsSome then
                provider.ExitClosureTyparScope()

            fieldDefHandles.Add(fs.Key, h)

        if ctx.FieldRowCount <> layoutHandles.TotalFields then
            failwithf
                "Layout: field table has %d rows after the field pass, layout owns %d"
                ctx.FieldRowCount
                layoutHandles.TotalFields

    let unions = Dictionary<SymbolKey, Emit.EmittedUnion>()
    let records = Dictionary<SymbolKey, Emit.EmittedRecord>()
    let classes = Dictionary<SymbolKey, Emit.EmittedClass>()

    // A static fn's call sites resolve through its layout-derived `MethodDef`
    // handle; recursion and cross-calls need no emission-order discipline.
    let staticMethods = Dictionary<NodeKey, Emit.StaticMethodRef>()

    do
        plan.StaticFns
        |> List.iter (fun fn ->
            staticMethods.[fn.Key] <-
                {
                    Handle = toEntity (layoutHandles.MethodDefOf(MethodKey.StaticFn fn.Key))
                    Arity = List.length fn.Params
                    ResultTy = fn.ResultTy
                    Typars = plan.StaticFnTypars.[fn.Key]
                    ParamTys = fn.Params |> List.map snd
                }
        )

    // Module-value bindings resolve to their already-written field rows
    // (module-representation-plan §3) — any body encodes the `ldsfld` token
    // straight off the def handle.
    let moduleValueFields = Dictionary<NodeKey, EntityHandle>()

    do
        plan.ModuleValueFieldOrder
        |> List.iter (fun mv -> moduleValueFields.[mv.Key] <- toEntity fieldDefHandles.[FieldKey.ModuleValue mv.Key])

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
            ModuleValues = moduleValueFields
        }

    // ---- Prepared methods ----
    // The Prepare phase binds every layout method row to its signature + body
    // offset + param names; `WriteMethods` walks `layout.Methods` and writes
    // them mechanically. Keyed adds throw on a duplicate — collisions are bugs
    // that should be loud (§3.1).
    let prepared = Dictionary<MethodKey, PreparedMethod>()

    // ---- Attribute sets ----
    // Method attribute sets live in `MethodAttrSets` (shared with the
    // layout's method enumeration); only the type-level sets remain here.
    let closureAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.BeforeFieldInit

    // A user class opts in to `Sealed` via `[<Sealed>]` (B-8); without it the
    // class is open (Phase 2 / B-4 wires inheritance). Unions / records reuse
    // this with `isSealed = true`. A `[<Struct>]` value type
    // (vesper-set-sprint-phase-6) is always sealed and uses sequential layout
    // (the F# default for value types) instead of auto layout.
    let classAttrsOf (isSealed: bool) (isValueType: bool) =
        let layoutAttr =
            if isValueType then
                TypeAttributes.SequentialLayout
            else
                TypeAttributes.AutoLayout

        let baseAttrs =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| layoutAttr
            ||| TypeAttributes.AnsiClass
            ||| TypeAttributes.BeforeFieldInit

        if isSealed || isValueType then
            baseAttrs ||| TypeAttributes.Sealed
        else
            baseAttrs

    // G8: route every emitted method through a real `Param` list. `Param` rows
    // are a global table referenced by each `MethodDefinition.ParamList`, so
    // they must be added in method order; the writer calls this immediately
    // before each `AddMethodWithParamList`. Returns the method's first `Param`
    // handle (past-the-end for a zero-parameter method).
    let addParams (names: string list) : ParameterHandle =
        let firstParam = ctx.NextParamHandle
        names |> List.iteri (fun i n -> ctx.AddParameter(i + 1, n) |> ignore)
        firstParam

    // `GenericParam` rows can't be added inline: SRM requires them globally
    // sorted by `CodedIndex.TypeOrMethodDef(owner)`, and a method owner can sort
    // *before* its declaring type. Collect (owner, index, name) and emit sorted
    // once every handle exists.
    let genericParams = ResizeArray<EntityHandle * int * string>()

    // The Prepare-minted `InterfaceImpl` / `BaseType` handles per type —
    // everything else a `TypeDefinition` row needs comes from the layout.
    let typeRowExtras = Dictionary<TypeKey, TypeRowExtras>()

    // ---- State exposed to `NominalEmit` and the `Codegen` orchestrator ----
    member _.Provider = provider
    member _.Icodegen = icodegen
    member _.Ctx = ctx
    member _.BodyStream = bodyStream
    member _.EncodeLocals = encodeLocals
    member _.EmitCtx = emitCtx
    member _.Unions = unions
    member _.Records = records
    member _.Classes = classes
    member _.UnionDecls = unionDecls
    member _.RecordDecls = recordDecls
    member _.ClassDecls = classDecls

    /// The layout's prefix-sum handle derivation — the only place a
    /// first-field / first-method / TypeDef / MethodDef handle comes from.
    member _.LayoutHandles = layoutHandles

    /// A field's def-table handle, resolved from the layout's field pass.
    /// Whether a use site routes through a `MemberRef` instead (generic
    /// types/closures, G13) stays the caller's policy.
    member _.FieldDef(key: FieldKey) : FieldDefinitionHandle = fieldDefHandles.[key]

    /// A method's layout-derived def-table handle.
    member _.MethodDef(key: MethodKey) : MethodDefinitionHandle = layoutHandles.MethodDefOf key

    /// Bind a layout method row to its built signature/body; the writer adds
    /// the actual `MethodDef` row in layout order.
    member _.AddPrepared(key: MethodKey, m: PreparedMethod) = prepared.Add(key, m)

    /// Record a type's Prepare-minted `InterfaceImpl` / `BaseType` handles
    /// for `Finalise`'s `TypeDefinition` row.
    member _.AddTypeRowExtras(key: TypeKey, extras: TypeRowExtras) = typeRowExtras.Add(key, extras)

    // ---- Bind: monomorphic closure ctors ----
    // The construction-site `Newobj` targets the ctor's `Def` directly via
    // this dict. Generic closures mint a fresh `MemberRef` at the use site
    // instead (dict left unpopulated).
    member this.BindClosures() =
        for c in closures do
            if c.Typars = 0 then
                ctorHandleByNode.[c.Node] <- toEntity (layoutHandles.MethodDefOf(MethodKey.ClosureCtor c.Name))

    // ---- Prepare: interfaces (bodyless abstract methods) ----
    member this.PrepareInterfaces() =
        for (td, methods) in interfaceDecls do
            methods
            |> List.iteri (fun i m ->
                let paramTys, _ = decurry m.Signature

                this.AddPrepared(
                    MethodKey.InterfaceMethod(td.Key, i),
                    {
                        Signature = abstractMethodSignature provider m
                        BodyOffset = -1
                        ParamNames = argNames (List.length paramTys)
                        // The method's own typars are owned by this MethodDef
                        // (metadata name drops the F# leading quote).
                        MethodTypars = [ for n in m.MethodTypeParams -> n.TrimStart('\'') ]
                    }
                )
            )

    // ---- Prepare: closures (leaves-first) ----
    // A *generic* closure (C3) enters closure-typar mode around every
    // signature/body build, so the body's `FTTypar(Method, i)` (the enclosing
    // method's typars) re-project onto this closure class's `!i`.
    member this.PrepareClosures() =
        for c in closures do
            let captureFields = Dictionary<NodeKey, EntityHandle>()
            let isGenericClosure = c.Typars > 0
            // This closure's self-instantiation over its *own* typars (`!0 … !{n-1}`),
            // used for the capture-field `MemberRef`s on its self-`TypeSpec`. A
            // closure typar `i` is its own declaring typar, so `FTTypar(Declaring, i)`
            // encodes `!i` regardless of the closure-scope offset (only method-axis
            // typars are re-projected). This is offset-independent — the same bytes
            // a static-fn closure emitted before.
            let selfArgs = [ for i in 0 .. c.Typars - 1 -> FTTypar(TyparAxis.Declaring, i) ]

            if isGenericClosure then
                provider.EnterClosureTyparScope c.DeclaringTypars

            let fieldHandles =
                c.Captures
                |> List.mapi (fun i (k, _) ->
                    let h = fieldDefHandles.[FieldKey.ClosureCapture(c.Name, i)]

                    // Generic closure: `stfld` (ctor) and `ldfld` (`Invoke`)
                    // reference a `MemberRef` on the closure's self-`TypeSpec`;
                    // monomorphic keeps the `Def` token.
                    let handleForUse =
                        if isGenericClosure then
                            icodegen.UserClosureMemberRef(c.Name, selfArgs, ClosureMember.CaptureField i)
                        else
                            toEntity h

                    captureFields.[k] <- handleForUse
                    handleForUse
                )

            let ctorBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef fieldHandles))

            let invokeBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureInvoke emitCtx c captureFields))

            this.AddPrepared(
                MethodKey.ClosureCtor c.Name,
                {
                    Signature = provider.ClosureCtorSignature(List.map snd c.Captures)
                    BodyOffset = ctorBodyOffset
                    ParamNames = argNames (List.length c.Captures)
                    MethodTypars = []
                }
            )

            this.AddPrepared(
                MethodKey.ClosureInvoke c.Name,
                {
                    Signature = provider.InvokeSignature(c.ParamTy, c.ResultTy)
                    BodyOffset = invokeBodyOffset
                    ParamNames = [ "arg0" ]
                    MethodTypars = []
                }
            )

            // `Fun\`2<param, result>` interface `TypeSpec` — closure ambient
            // still installed, so free `TyVar`s encode to `!i`.
            let ifaceSpec = provider.FunInterfaceSpec(c.ParamTy, c.ResultTy)

            if isGenericClosure then
                let closureHandle = toEntity (layoutHandles.TypeDefOf(TypeKey.Closure c.Name))

                for i in 0 .. c.Typars - 1 do
                    genericParams.Add(closureHandle, i, sprintf "T%d" i)

                provider.ExitClosureTyparScope()

            typeRowExtras.Add(
                TypeKey.Closure c.Name,
                {
                    Interfaces = [ ifaceSpec ]
                    BaseType = provider.ObjectType
                }
            )

    // ---- Prepare: holder `.cctor`s + static methods (P3b) ----
    member this.PrepareStaticMethods() =
        let prepareStaticFn (fn: Emit.StaticFn) =
            // A *generic* static method (`fold`, R3): its body / signature / locals
            // embed `FTTypar(Method, i)` (freeze-quantified),
            // which the encoder maps to `!!i` directly — no ambient typar window.
            let typarCount = staticMethods.[fn.Key].Typars

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticMethod emitCtx fn))

            let signature =
                if typarCount = 0 then
                    provider.StaticMethodSignature(fn.Params |> List.map snd, fn.ResultTy)
                else
                    provider.GenericStaticFnSignature(typarCount, fn.Params |> List.map snd, fn.ResultTy)

            this.AddPrepared(
                MethodKey.StaticFn fn.Key,
                {
                    Signature = signature
                    BodyOffset = bodyOffset
                    ParamNames = argNames (List.length fn.Params)
                    MethodTypars = [ for i in 0 .. typarCount - 1 -> sprintf "T%d" i ]
                }
            )

        // A holder's `.cctor` initialises its module values in declaration order
        // (the static analogue of the class `static let` cctor — same
        // `buildStaticCctor` recipe, `stsfld` into each field). `mv.Init` comes
        // from the lowered decls, so built-in operators are already expanded.
        let prepareHolderCctor (h: Emit.HolderKey) =
            let lets =
                [
                    for mv in HolderPlan.holderValues plan h -> moduleValueFields.[mv.Key], mv.Init
                ]

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticCctor emitCtx lets))

            this.AddPrepared(
                MethodKey.HolderCctor h,
                {
                    Signature = provider.CctorSignature()
                    BodyOffset = bodyOffset
                    ParamNames = []
                    MethodTypars = []
                }
            )

        for slot in plan.MethodPlan do
            match slot with
            | HolderCctor h -> prepareHolderCctor h
            | HolderFn fn -> prepareStaticFn fn

    // ---- Prepare: Main (executable only; presence is a layout decision) ----
    member this.PrepareMain() =
        if layout.EmitEntryPoint then
            let mainBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildMain emitCtx lowered))

            this.AddPrepared(
                MethodKey.Main,
                {
                    Signature = mainSignature ()
                    BodyOffset = mainBodyOffset
                    ParamNames = [ "args" ]
                    MethodTypars = []
                }
            )

    // ---- Write: the MethodDef table, one mechanical loop ----
    // Per row: `Param` rows, then the `MethodDef` row, then the method-owned
    // `GenericParam` rows (collected; emitted sorted in `Finalise`). The only
    // "decision" is the prepared lookup — everything else came from the layout.
    member this.WriteMethods() =
        for row in layout.Methods do
            let p =
                match prepared.TryGetValue row.Key with
                | true, p -> p
                | _ -> failwithf "Layout: method row '%s' (%A) was never prepared" row.Name row.Key

            let firstParam = addParams p.ParamNames

            let handle =
                ctx.AddMethodWithParamList(row.Attrs, row.Name, p.Signature, p.BodyOffset, firstParam)

            let predicted = layoutHandles.MethodDefOf row.Key

            if handle <> predicted then
                failwithf
                    "Layout: method '%s' predicted MethodDef row %d <> actual %d"
                    row.Name
                    (MetadataTokens.GetRowNumber(toEntity predicted))
                    (MetadataTokens.GetRowNumber(toEntity handle))

            p.MethodTypars
            |> List.iteri (fun i n -> genericParams.Add(toEntity handle, i, n))

        if prepared.Count <> layoutHandles.TotalMethods then
            failwithf
                "Layout: %d methods were prepared but the layout owns %d rows"
                prepared.Count
                layoutHandles.TotalMethods

    // ---- TypeDefinition rows (one walk over the layout) + serialise ----
    member this.Finalise() : ClrArtifact =
        let rowOf (h: EntityHandle) = MetadataTokens.GetRowNumber h

        let verifyTypeHandle (slot: TypeSlot) (actual: TypeDefinitionHandle) =
            let predicted = layoutHandles.TypeDefOf slot.Key

            if predicted <> actual then
                failwithf
                    "Layout: slot '%s' predicted TypeDef row %d <> actual %d"
                    slot.MetaName
                    (rowOf (toEntity predicted))
                    (rowOf (toEntity actual))

        // Union, record, class, and closure `TypeDefinition` rows share one
        // recipe, with the Prepare-minted `InterfaceImpl` / `BaseType` handles.
        // Walking the layout in order keeps the `InterfaceImpl` /
        // `GenericParam` rows ascending (sorted by `Class` / `TypeOrMethodDef`).
        let addNominalRow (slot: TypeSlot) (attrs: TypeAttributes) =
            let extras =
                match typeRowExtras.TryGetValue slot.Key with
                | true, e -> e
                | _ -> failwithf "Layout: type slot '%s' was never prepared" slot.MetaName

            let typeHandle =
                ctx.AddClass(
                    attrs,
                    slot.Namespace,
                    slot.MetaName,
                    extras.BaseType,
                    layoutHandles.FirstFieldOf slot.Key,
                    layoutHandles.FirstMethodOf slot.Key
                )

            verifyTypeHandle slot typeHandle

            for iface in extras.Interfaces do
                ctx.AddInterfaceImplementation(typeHandle, iface)

            slot.Typars
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n))

        for slot in layout.Types do
            match slot.Kind with
            | TypeSlotKind.ModulePseudo ->
                // `<Module>` points at method row 1 — the first real method, or
                // past-the-end of the empty table in a degenerate no-method
                // library (both are the layout's first-method prefix sum).
                ctx.AddModuleType(layoutHandles.FirstMethodOf slot.Key)

            | TypeSlotKind.Interface ->
                // Interfaces have no fields, so the field range is empty (the
                // prefix sum — row 1, every field-bearing kind follows).
                let typeHandle =
                    ctx.AddInterfaceType(
                        slot.Namespace,
                        slot.MetaName,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle

                slot.Typars
                |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n))

            // Unions and records are always sealed (rung 2 forbids
            // inheritance); a class opts in via `[<Sealed>]` / `[<Struct>]`.
            | TypeSlotKind.Union
            | TypeSlotKind.Record -> addNominalRow slot (classAttrsOf true false)

            | TypeSlotKind.Class(isSealed, isValueType) -> addNominalRow slot (classAttrsOf isSealed isValueType)

            // Each closure derives from `System.Object` and implements its
            // `Vesper.Fun\`2<param, result>` interface (R1). Its `GenericParam`
            // rows were collected during `PrepareClosures` (closure-typar
            // ambient), so none are added here.
            | TypeSlotKind.Closure ->
                let extras =
                    match typeRowExtras.TryGetValue slot.Key with
                    | true, e -> e
                    | _ -> failwithf "Layout: closure slot '%s' was never prepared" slot.MetaName

                let closureHandle =
                    ctx.AddClass(
                        closureAttrs,
                        slot.Namespace,
                        slot.MetaName,
                        extras.BaseType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot closureHandle

                for iface in extras.Interfaces do
                    ctx.AddInterfaceImplementation(closureHandle, iface)

            // Named-module holders (R3 deferred): one static class per
            // `module Foo`. A holder owning module values takes its own
            // `FieldList` and drops `BeforeFieldInit` (its `.cctor` runs before
            // first access); a value-less holder's empty field range points past
            // the previous owner's range (the prefix sum).
            | TypeSlotKind.Holder hasCctor ->
                let typeHandle =
                    ctx.AddProgramType(
                        slot.Namespace,
                        slot.MetaName,
                        provider.ObjectType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key,
                        not hasCctor
                    )

                verifyTypeHandle slot typeHandle

            // The anonymous "Program" holder owns the holder-less static
            // methods (and `Main`, when an executable). Its presence is a
            // layout decision (`Layout.build`).
            | TypeSlotKind.Program ->
                let typeHandle =
                    ctx.AddProgramType(
                        slot.Namespace,
                        slot.MetaName,
                        provider.ObjectType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key,
                        true
                    )

                verifyTypeHandle slot typeHandle

        // Every handle now exists: add `GenericParam` rows in the order SRM
        // validates — by the owner's `TypeOrMethodDef` coded index, then index.
        genericParams
        |> Seq.sortBy (fun (owner, index, _) -> (CodedIndex.TypeOrMethodDef owner, index))
        |> Seq.iter (fun (owner, index, name) -> ctx.AddGenericParameter(owner, index, name) |> ignore)

        let pe =
            if layout.EmitEntryPoint then
                ctx.Serialize(layoutHandles.MethodDefOf MethodKey.Main)
            else
                ctx.SerializeLibrary()

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = pe
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }
