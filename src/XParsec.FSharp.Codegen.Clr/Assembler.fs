namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
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
    // the host loaded.
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

    // closure name → its `Closure` record, so the type-layout
    // pass (keyed only by `TypeKey.Closure name`) can branch a value-struct closure
    // onto struct attrs / `System.ValueType` base.
    let closureByName = Dictionary<string, Emit.Closure>()

    do
        for c in closures do
            closureByName.[c.Name] <- c

    let closureIsValueStruct (name: string) : bool =
        match closureByName.TryGetValue name with
        | true, c -> c.IsValueStruct
        | false, _ -> false

    let ctorHandleByNode =
        Dictionary<Frozen.TExpr, EntityHandle>(HashIdentity.Reference)

    // A non-capturing, monomorphic closure's cached singleton field: its
    // construction sites `ldsfld` this instead of `newobj`ing.
    let cachedClosureFieldByNode =
        Dictionary<Frozen.TExpr, EntityHandle>(HashIdentity.Reference)

    // A captureless `Stack` (value-struct) closure's synthetic
    // encodable `FrozenType` (the by-value local + the constrained-slot `MethodSpec`
    // type-argument) and its closure-`TypeDef` handle (`initobj` operand).
    let closureValueTypeByNode =
        Dictionary<Frozen.TExpr, FrozenType>(HashIdentity.Reference)

    let closureTypeDefByNode =
        Dictionary<Frozen.TExpr, EntityHandle>(HashIdentity.Reference)

    let partitionedDecls = layout.Partitioned
    let interfaceDecls = partitionedDecls.Interfaces
    let unionDecls = partitionedDecls.Unions
    let recordDecls = partitionedDecls.Records
    let classDecls = partitionedDecls.Classes

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

            if cd.ValueKind <> ClassValueKind.RefType then
                provider.RegisterUserValueType td.Key

            if not td.TypeParams.IsEmpty then
                // The ctor-param backing fields, the explicit `val [mutable] x: T`
                // instance fields, and the `static let` backing fields must all be in
                // the generic-class registry: a generic struct's field-init ctor and
                // member-body `ldfld`/`stfld` reference the `val` fields by name
                // through a `MemberRef` on the open self-`TypeSpec`, and a generic
                // `static let` read/store (`ldsfld`/`stsfld`) goes through the same
                // `ClassMember.Field` `MemberRef` — an unregistered field fails
                // resolution ("generic class … has no field").
                let ctorParamFields = [ for p in cd.CtorParams -> p.Name, p.Type ]

                let shape =
                    ctorParamFields
                    @ [ for f in cd.Fields -> f.Name, f.Type ]
                    @ [ for sl in cd.StaticLets -> sl.Name, sl.Type ]

                provider.RegisterGenericClass(td.Key, EqArray.toList td.TypeParams, List.length ctorParamFields, shape)
        )

    // Interfaces register their `TypeDef` too, so one Core interface naming another
    // as a member-signature type (`IStructuralFormattable.Format(IFormatSink)`)
    // resolves through `userTypes` like any project-local nominal.
    do
        interfaceDecls
        |> List.iter (fun (td, _) ->
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeKey.Nominal td.Key)))

            // A *generic* interface (`IStructSeq<'E>`) also enters the generic-class
            // registry so a constrained-typar dispatch (`CallVia.Interface`)
            // can mint its abstract slot as a `MemberRef` on the instantiated
            // interface `TypeSpec` (`IStructSeq\`1<!E>::GetEnumerator`) via
            // `UserGenericMemberRef`. An interface has no ctor params or fields, so
            // the field/ctor-arity components are empty — only the typar count (for
            // the self-`TypeSpec`) and the member signature are consulted.
            if not td.TypeParams.IsEmpty then
                provider.RegisterGenericClass(td.Key, EqArray.toList td.TypeParams, 0, [])
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

    // Mint each captureless value-struct closure's
    // synthetic encodable `FrozenType` + `TypeDef` handle NOW, before the field
    // table is written — the module-value field substitution
    // (`substituteVerdictClosures`) must read `closureValueTypeByNode` while encoding a stored binding's
    // `'TFunc` slot, and that slot's field is in the up-front field pass below.
    // `BindClosures` (called later from `Codegen.assemble`) reads these already-minted
    // entries rather than re-minting (`RegisterStackClosureValueType` is single-shot —
    // it fails on a duplicate `<closure>` key).
    do
        for c in closures do
            if c.IsValueStruct then
                let defHandle = toEntity (layoutHandles.TypeDefOf(TypeKey.Closure c.Name))
                let ft = provider.RegisterStackClosureValueType(c.Name, defHandle)
                closureValueTypeByNode.[c.Node] <- ft
                closureTypeDefByNode.[c.Node] <- defHandle

    // The seq→enumerator witness the closure-verdict rewrite needs to
    // rewrite a chained binding's nested `'E` ENUMERATOR slot node-keyed (NOT by
    // arrow shape). For a project-local seq class, its `GetEnumerator` interface-impl
    // member's RETURN type is the enumerator over the class's declaring typars; map
    // each seq class key → that template, then `enumeratorOf` instantiates it by a
    // concrete (already-rewritten) seq nominal's args. This is the structural
    // seq→enumerator relationship the type system defines — the codegen analog of
    // `EmitResolve.tryInterfaceWitness`, computed here from the front-end class decls
    // because `env.Classes` is not yet populated at the up-front field pass.
    let enumeratorTemplateByClass =
        let d = Dictionary<SymbolKey, FrozenType>()

        for cd in classDecls do
            let template =
                cd.Interfaces
                |> List.tryPick (fun (_, members) ->
                    members
                    |> List.tryPick (fun (m: Frozen.TTypeMember) ->
                        if m.Name = "GetEnumerator" then Some m.ReturnTy else None
                    )
                )

            match template with
            | Some t -> d.[cd.Decl.Key] <- t
            | None -> ()

        d

    // Instantiate a seq class's declaring-typar enumerator template by a concrete
    // nominal's args: `FTTypar(Declaring, i) := args.[i]` throughout (the canonical
    // `FrozenTypeBridge.substituteDeclaring` — a `GetEnumerator`-return template
    // carries only the declaring axis, so its loud method-axis arm is unreachable).
    // `ValueNone` when the nominal is not a project-local seq class (no template).
    let enumeratorOf (seqTy: FrozenType) : FrozenType voption =
        match seqTy with
        | FTClass(key, args) ->
            match enumeratorTemplateByClass.TryGetValue key with
            | true, template -> ValueSome(substituteDeclaring (args.AsSpan().ToArray()) template)
            | false, _ -> ValueNone
        | _ -> ValueNone

    // The closure-verdict TAST rewrite, built from backend-neutral inputs
    // (the already-minted value-struct closure types + the front-end's result-typar
    // verdicts + the stored module values + the seq→enumerator witness). It owns
    // `substituteVerdictClosures` / `retypeBody` / `retypeDecl` and the field-slot
    // lookup; see `ClosureVerdictRewrite`. Built here (after the mint `do` above) so
    // the field pass below can consult it.
    let verdict =
        ClosureVerdictRewrite.build
            closureValueTypeByNode
            tast.FunVerdicts
            enumeratorOf
            [ for mv in plan.ModuleValueFieldOrder -> mv.Key, mv.Ty, mv.Init ]

    let retypeBody = verdict.RetypeBody
    let retypeDecl = verdict.RetypeDecl

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
                    match fs.Key with
                    // The cached-singleton field's type is the closure's own reference
                    // type, encoded from its TypeDef handle (no `FrozenType`).
                    | FieldKey.ClosureCached name ->
                        provider.ClosureSelfFieldSignature(toEntity (layoutHandles.TypeDefOf(TypeKey.Closure name)))
                    // A stored module value whose initialiser feeds a
                    // value-struct source lambda into a `'TFunc`-carrying result type —
                    // rewrite the typar-position leaf to the `<closure>$` value-struct so
                    // the field slot matches the value the call returns.
                    | FieldKey.ModuleValue mvKey -> provider.FieldSignature(verdict.ModuleValueSlotType mvKey fs.Ty)
                    | _ -> provider.FieldSignature fs.Ty
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
    // Filled by `PrepareInterfaces` (shared by reference with `emitCtx`), so a call
    // on an interface-typed receiver resolves its slot through `resolveInstanceMember`.
    let interfaces = Dictionary<SymbolKey, Emit.EmittedInterface>()

    // A static fn's call sites resolve through its layout-derived `MethodDef`
    // handle; recursion and cross-calls need no emission-order discipline.
    let staticMethods = Dictionary<NodeKey, Emit.StaticMethodRef>()

    do
        plan.StaticFns
        |> List.iter (fun fn ->
            staticMethods.[fn.Key] <-
                {
                    Handle = toEntity (layoutHandles.MethodDefOf(MethodKey.StaticFn fn.Key))
                    // The flat CLR arg count (the `call` operand count); the spine
                    // split uses `Groups.Length`, which can be smaller (a tupled
                    // group is one application, many flat params).
                    Arity = List.length fn.Params
                    Groups = fn.Groups
                    ResultTy = fn.ResultTy
                    // `plan.StaticFnTypars` is the max method
                    // index over params + result + BODY, so a generic combinator emits
                    // a `MethodSpec` slot for each phantom typar surviving in its body
                    // (`fold`'s `'E`) — the call site solves those from `Constraints`.
                    Typars = plan.StaticFnTypars.[fn.Key]
                    ParamTys = fn.Params |> List.map (fun p -> p.Ty)
                    ReturnsVoid = fn.ReturnsVoid
                    // The frozen typar bounds the call-site
                    // phantom-typar solve (`EmitCall`) reads to recover the phantom
                    // method-typar slots no parameter/result mentions.
                    Constraints = fn.Constraints
                }
        )

    // Module-value bindings resolve to their already-written field rows —
    // any body encodes the `ldsfld` token straight off the def handle.
    let moduleValueFields = Dictionary<NodeKey, EntityHandle>()

    do
        plan.ModuleValueFieldOrder
        |> List.iter (fun mv -> moduleValueFields.[mv.Key] <- toEntity fieldDefHandles.[FieldKey.ModuleValue mv.Key])

    // The trailing top-level values: their `public static` field is written in
    // `Main` (`buildMain` `stsfld`), not a `.cctor`. Same field handles, a separate
    // map so `buildMain` knows to emit the store (vs the cctor-initialised values it
    // skips).
    let mainInitValues = Dictionary<NodeKey, EntityHandle>()

    do
        plan.ProgramMainValues
        |> List.iter (fun mv -> mainInitValues.[mv.Key] <- moduleValueFields.[mv.Key])

    let emitCtx: Emit.EmitContext =
        {
            Provider = icodegen
            Ctx = ctx
            ClosureByNode = closureByNode
            CtorHandleByNode = ctorHandleByNode
            CachedClosureFieldByNode = cachedClosureFieldByNode
            ClosureValueTypeByNode = closureValueTypeByNode
            ClosureTypeDefByNode = closureTypeDefByNode
            Unions = unions
            Records = records
            Classes = classes
            Interfaces = interfaces
            StaticMethods = staticMethods
            ModuleValues = moduleValueFields
            MainInitValues = mainInitValues
        }

    // The Prepare phase binds every layout method row to its signature + body
    // offset + param names; `WriteMethods` walks `layout.Methods` and writes
    // them mechanically. Keyed adds throw on a duplicate — collisions are bugs
    // that should be loud.
    let prepared = Dictionary<MethodKey, PreparedMethod>()

    // Method attribute sets live in `MethodAttrSets` (shared with the
    // layout's method enumeration); only the type-level sets remain here.
    let closureAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.BeforeFieldInit

    // A user class opts in to `Sealed` via `[<Sealed>]`; without it the
    // class is open. Unions / records reuse this with `isSealed = true`.
    // A `[<Struct>]` value type is always sealed and uses sequential layout
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

    // Route every emitted method through a real `Param` list. `Param` rows
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

    /// True when *this* compilation defines the `%A` structural-format interfaces
    /// (`Vesper.IStructuralFormattable` / `IFormatSink`) — i.e. it is `Vesper.Core`.
    /// Then the per-type `Format` synthesis (NominalEmit) is suppressed: a Core
    /// record (the `[<ReferenceEquality>]` `Ref` cell) would otherwise reference
    /// the interface through an `AssemblyRef` to Core *itself*, which `refRequired`
    /// rejects. Core's internal cells need no `%A`; every downstream assembly
    /// resolves the interface externally via `vesperCoreRef`, as before. Read off
    /// the layout (computed once in `Layout.build`) so the `Format`-row reservation
    /// and this body-emission gate share one source of truth.
    member _.DefinesStructuralFormatInterfaces = layout.DefinesStructuralFormatInterfaces

    /// The layout's prefix-sum handle derivation — the only place a
    /// first-field / first-method / TypeDef / MethodDef handle comes from.
    member _.LayoutHandles = layoutHandles

    /// A field's def-table handle, resolved from the layout's field pass.
    /// Whether a use site routes through a `MemberRef` instead (generic
    /// types/closures) stays the caller's policy.
    member _.FieldDef(key: FieldKey) : FieldDefinitionHandle = fieldDefHandles.[key]

    /// A method's layout-derived def-table handle.
    member _.MethodDef(key: MethodKey) : MethodDefinitionHandle = layoutHandles.MethodDefOf key

    /// Bind a layout method row to its built signature/body; the writer adds
    /// the actual `MethodDef` row in layout order.
    member _.AddPrepared(key: MethodKey, m: PreparedMethod) = prepared.Add(key, m)

    /// Record a type's Prepare-minted `InterfaceImpl` / `BaseType` handles
    /// for `Finalise`'s `TypeDefinition` row.
    member _.AddTypeRowExtras(key: TypeKey, extras: TypeRowExtras) = typeRowExtras.Add(key, extras)

    // The construction-site `Newobj` targets the ctor's `Def` directly via
    // this dict. Generic closures mint a fresh `MemberRef` at the use site
    // instead (dict left unpopulated).
    member this.BindClosures() =
        for c in closures do
            if c.Typars = 0 then
                ctorHandleByNode.[c.Node] <- toEntity (layoutHandles.MethodDefOf(MethodKey.ClosureCtor c.Name))

            // A non-capturing, monomorphic closure is cached: the construction site
            // `ldsfld`s its singleton field instead of `newobj`ing.
            if Emit.closureIsCached c then
                cachedClosureFieldByNode.[c.Node] <- toEntity (fieldDefHandles.[FieldKey.ClosureCached c.Name])

            // A captureless `Stack` (value-struct) closure is
            // constructed by-value (`initobj` to a local) and its struct `TypeDef`
            // is the constrained-slot `MethodSpec` type-argument at the call site.
            // Its synthetic value-type `FrozenType` + `TypeDef` handle were already
            // minted in the constructor (before the field pass, so the stored-slot
            // substitution could read them); `RegisterStackClosureValueType` is
            // single-shot, so this only asserts they are present — never re-mints.
            if c.IsValueStruct && not (closureValueTypeByNode.ContainsKey c.Node) then
                failwithf "Emit: value-struct closure '%s' was not pre-minted before the field pass" c.Name

    member this.PrepareInterfaces() =
        for (td, methods) in interfaceDecls do
            // The use-site member table for a call on an interface-typed receiver:
            // each method's `MethodKey.InterfaceMethod` handle keyed by source name, so
            // `resolveInstanceMember` finds the slot and `buildMethodCall` `callvirt`s
            // it. Same `EmittedMember` shape as a class member (overload list).
            let memberTable = Dictionary<string, Emit.EmittedMember list>()

            methods
            |> List.iteri (fun i m ->
                // The post-elision arity (a nullary `unit ->` member drops its sole
                // param) must match `abstractMethodSignature`'s, or the `Param` rows
                // and the signature disagree and the method becomes un-reflectable.
                let paramTys = abstractMethodParamTys m
                let _, retTy = decurry m.Signature

                let handle = toEntity (this.MethodDef(MethodKey.InterfaceMethod(td.Key, i)))

                let em: Emit.EmittedMember =
                    {
                        Handle = handle
                        IsStatic = false
                        Arity = List.length paramTys
                        // The IL method name (a property → its `get_<Name>` getter);
                        // the use-site table below stays keyed by the bare member name.
                        MetaName = if m.IsProperty then "get_" + m.Name else m.Name
                        ParamTys = paramTys
                        RetTy = retTy
                        MethodTyparCount = m.MethodTypeParams.Length
                    }

                memberTable.[m.Name] <-
                    match memberTable.TryGetValue m.Name with
                    | true, existing -> existing @ [ em ]
                    | false, _ -> [ em ]

                this.AddPrepared(
                    MethodKey.InterfaceMethod(td.Key, i),
                    {
                        Signature = abstractMethodSignature provider m
                        BodyOffset = -1
                        ParamNames = argNames (List.length paramTys)
                        // The method's own typars are owned by this MethodDef;
                        // the metadata name drops the F# leading quote.
                        MethodTypars = [ for n in m.MethodTypeParams -> n.TrimStart('\'') ]
                    }
                )
            )

            interfaces.[td.Key] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
                    Members = memberTable
                }

    // A *generic* closure enters closure-typar mode around every signature/body
    // build, so the body's `FTTypar(Method, i)` (the enclosing method's typars)
    // re-project onto this closure class's `!i`.
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

            // A `Stack` closure is a value type — its ctor
            // does NOT chain `System.Object::.ctor` (value types have none and do
            // not chain), so use the struct-ctor builder. A captureless Stack
            // closure's ctor is the trivial `ret`; construction is by-value
            // (`initobj`), so it is never called, but the row stays for layout
            // parity with the heap path.
            let isStack = c.IsValueStruct

            let ctorBodyOffset =
                if isStack then
                    Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStructCtor fieldHandles))
                else
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

            // A flat-2 (`Fun2`) closure's `Invoke` is `Invoke(a, b) : c`;
            // arity-1 keeps the single-arg `Invoke(a) : b`.
            let invokeSignature, invokeParamNames =
                match c.Param2 with
                | ValueSome(_, p2ty, _) -> provider.InvokeSignature2(c.ParamTy, p2ty, c.ResultTy), [ "arg0"; "arg1" ]
                | ValueNone -> provider.InvokeSignature(c.ParamTy, c.ResultTy), [ "arg0" ]

            this.AddPrepared(
                MethodKey.ClosureInvoke c.Name,
                {
                    Signature = invokeSignature
                    BodyOffset = invokeBodyOffset
                    ParamNames = invokeParamNames
                    MethodTypars = []
                }
            )

            // A non-capturing, monomorphic closure caches its single instance: a
            // `.cctor` `newobj`s the ctor once and `stsfld`s the singleton field.
            // Construction sites then `ldsfld` it (`BindClosures`
            // populated `cachedClosureFieldByNode`).
            if Emit.closureIsCached c then
                let ctorHandle = toEntity (layoutHandles.MethodDefOf(MethodKey.ClosureCtor c.Name))
                let cachedField = toEntity (fieldDefHandles.[FieldKey.ClosureCached c.Name])

                let cctorBodyOffset =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildCachedClosureCctor ctorHandle cachedField))

                this.AddPrepared(
                    MethodKey.ClosureCctor c.Name,
                    {
                        Signature = provider.CctorSignature()
                        BodyOffset = cctorBodyOffset
                        ParamNames = []
                        MethodTypars = []
                    }
                )

            // `Fun\`2<param, result>` interface `TypeSpec` — closure ambient
            // still installed, so free `TyVar`s encode to `!i`. A flat-2 (`Fun2`)
            // value-struct closure implements `Fun2`3<a,b,c>` instead.
            let ifaceSpec =
                match c.Param2 with
                | ValueSome(_, p2ty, _) -> provider.Fun2InterfaceSpec(c.ParamTy, p2ty, c.ResultTy)
                | ValueNone -> provider.FunInterfaceSpec(c.ParamTy, c.ResultTy)

            if isGenericClosure then
                let closureHandle = toEntity (layoutHandles.TypeDefOf(TypeKey.Closure c.Name))

                for i in 0 .. c.Typars - 1 do
                    genericParams.Add(closureHandle, i, sprintf "T%d" i)

                provider.ExitClosureTyparScope()

            typeRowExtras.Add(
                TypeKey.Closure c.Name,
                {
                    Interfaces = [ ifaceSpec ]
                    // A `Stack` closure is a value type, so it
                    // derives from `System.ValueType`; the heap closure from `Object`.
                    BaseType =
                        (if isStack then
                             provider.ValueTypeBase
                         else
                             provider.ObjectType)
                }
            )

    member this.PrepareStaticMethods() =
        let prepareStaticFn (fn: Emit.StaticFn) =
            // A *generic* static method: its body / signature / locals embed
            // `FTTypar(Method, i)` (freeze-quantified), which the encoder maps to
            // `!!i` directly — no ambient typar window.
            let typarCount = staticMethods.[fn.Key].Typars

            // Retype the body so a reference to a verdict module value (a
            // stored transformer result, `Var h` / `h.F`) or an inline transformer call
            // dispatches on the `<closure>$` value-struct nominal rather than the frozen
            // arrow. A no-op when there are no verdicts (the green named-struct path).
            let fn = { fn with Body = retypeBody fn.Body }

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticMethod emitCtx fn))

            let paramTys = fn.Params |> List.map (fun p -> p.Ty)

            // A `unit`-returning module function now encodes genuine CLR `void`
            // ("void everywhere"), matching the consumer convention the
            // instance path already used. A generic void static fn reuses the
            // generic-method void encoder with `isInstanceMethod = false`.
            let signature =
                match typarCount = 0, fn.ReturnsVoid with
                | true, false -> provider.StaticMethodSignature(paramTys, fn.ResultTy)
                | true, true -> provider.StaticMethodSignatureVoid(paramTys)
                | false, false -> provider.GenericStaticFnSignature(typarCount, paramTys, fn.ResultTy)
                | false, true -> provider.GenericMethodOnTypeSignatureVoid(typarCount, paramTys, false)

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
                    for mv in HolderPlan.holderValues plan h -> moduleValueFields.[mv.Key], retypeBody mv.Init
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

        // The anonymous "Program" holder's `.cctor`: the same value-store
        // recipe as a named holder's, over the leading-prefix top-level values.
        let prepareProgramCctor () =
            let lets =
                [
                    for mv in plan.ProgramCctorValues -> moduleValueFields.[mv.Key], retypeBody mv.Init
                ]

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticCctor emitCtx lets))

            this.AddPrepared(
                MethodKey.ProgramCctor,
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
            | ProgramCctor -> prepareProgramCctor ()

    member this.PrepareMain() =
        if layout.EmitEntryPoint then
            // Retype the Main decls so a reference to a verdict module
            // value (and its field projections) dispatches on the `<closure>$` value-
            // struct nominal, not the frozen arrow.
            let mainDecls = lowered |> List.map retypeDecl

            let mainBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildMain emitCtx mainDecls))

            this.AddPrepared(
                MethodKey.Main,
                {
                    Signature = mainSignature ()
                    BodyOffset = mainBodyOffset
                    ParamNames = [ "args" ]
                    MethodTypars = []
                }
            )

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
        let addNominalRow (slot: TypeSlot) (attrs: TypeAttributes) (isByRefLike: bool) =
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

            // A `[<IsByRefLike>]` value type carries the `IsByRefLikeAttribute`
            // marker — a parameterless custom attribute (blob = prolog `0x0001`
            // + zero named args = `01 00 00 00`). The CLR reads this to confine
            // the type to the stack; there is no `TypeAttributes` bit.
            if isByRefLike then
                let blob = BlobBuilder()
                blob.WriteUInt16(1us)
                blob.WriteUInt16(0us)

                ctx.AddCustomAttribute(toEntity typeHandle, provider.IsByRefLikeAttrCtor, blob)
                |> ignore

            for iface in extras.Interfaces do
                ctx.AddInterfaceImplementation(typeHandle, iface)

            slot.Typars
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n))

        for slot in layout.Types do
            match slot.Kind with
            | TypeSlotKind.ModulePseudo ->
                // `<Module>` points at method row 1 — the first real method, or
                // past-the-end of the empty table in a degenerate no-method library
                // (both are the layout's first-method prefix sum).
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

            // Unions and records are always sealed (subclassing /
            // inheritance forbidden); a class opts in via `[<Sealed>]` / `[<Struct>]`.
            | TypeSlotKind.Union
            | TypeSlotKind.Record -> addNominalRow slot (classAttrsOf true false) false

            | TypeSlotKind.Class(isSealed, valueKind) ->
                let isValueType = valueKind <> ClassValueKind.RefType
                let isByRefLike = valueKind = ClassValueKind.RefStruct
                addNominalRow slot (classAttrsOf isSealed isValueType) isByRefLike

            // Each closure derives from `System.Object` and implements its
            // `Vesper.Fun\`2<param, result>` interface. Its `GenericParam` rows
            // were collected during `PrepareClosures` (closure-typar ambient),
            // so none are added here.
            | TypeSlotKind.Closure ->
                let extras =
                    match typeRowExtras.TryGetValue slot.Key with
                    | true, e -> e
                    | _ -> failwithf "Layout: closure slot '%s' was never prepared" slot.MetaName

                // A `Stack` closure is a `[<Struct>]` value
                // type (sealed, sequential layout) deriving from `System.ValueType`
                // (set on `extras.BaseType` in `PrepareClosures`); the heap closure
                // keeps the sealed-class `closureAttrs` over `System.Object`.
                let attrs =
                    match slot.Key with
                    | TypeKey.Closure name when closureIsValueStruct name -> classAttrsOf true true
                    | _ -> closureAttrs

                let closureHandle =
                    ctx.AddClass(
                        attrs,
                        slot.Namespace,
                        slot.MetaName,
                        extras.BaseType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot closureHandle

                for iface in extras.Interfaces do
                    ctx.AddInterfaceImplementation(closureHandle, iface)

            // Named-module holders: one static class per `module Foo`. A holder
            // owning module values takes its own `FieldList` and drops
            // `BeforeFieldInit` (its `.cctor` runs before first access); a
            // value-less holder's empty field range points past the previous
            // owner's range (the prefix sum).
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

            // The anonymous "Program" holder owns the holder-less static methods
            // (and `Main`, when an executable) and the top-level value fields.
            // `hasCctor` ⇔ it owns leading-prefix values, dropping `BeforeFieldInit`
            // so its `.cctor` runs before `Main`. Its presence is a layout decision
            // (`Layout.build`).
            | TypeSlotKind.Program hasCctor ->
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
