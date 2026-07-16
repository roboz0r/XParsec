namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open AssemblerScaffold

/// One compilation unit's emission state that the single combined field table forces
/// to straddle the up-front field pass: the value-struct closure mint and the
/// closure-verdict rewrite must PRECEDE the pass (both feed a field's signature), so
/// they are built first and carried here into the post-pass completion (`completeUnit`),
/// which adds the field-derived tables and the `EmitContext`.
type private UnitPrelude =
    {
        Layout: UnitLayout
        CtorHandleByNode: Dictionary<Frozen.TExpr, EntityHandle>
        CachedClosureFieldByNode: Dictionary<Frozen.TExpr, EntityHandle>
        ClosureValueTypeByNode: Dictionary<Frozen.TExpr, FrozenType>
        ClosureTypeDefByNode: Dictionary<Frozen.TExpr, EntityHandle>
        Verdict: ClosureVerdictRewrite.Rewrite
    }

/// The per-unit emission state the Bind / Prepare passes consume. `EmitCtx` is a FRESH
/// `EmitContext` per unit — its NodeKey-keyed tables (`StaticMethods` / `ModuleValues` /
/// `MainInitValues`) and reference-keyed closure tables are file-local, so they never
/// collide across units; its nominal registries, the field-handle map and the ONE combined
/// row space are the SHARED ones on the `Assembler`. The Bind / Prepare passes reach every
/// per-unit table THROUGH `EmitCtx`; `Layout` and `Verdict` are the only two things they
/// need that `EmitContext` deliberately does not carry.
type internal UnitEmit =
    {
        Layout: UnitLayout
        Verdict: ClosureVerdictRewrite.Rewrite
        EmitCtx: Emit.EmitContext
    }

/// The converged assembler over the `AssemblyLayout`: the layout enumerates
/// every ranged-table row as data (handle = position), the constructor
/// registers forward handles and
/// writes the whole field table, the *Bind* phase pre-fills the `EmitContext`
/// registries from the layout, the *Prepare* phase builds every signature/body
/// against resolved handles (body-stream order is free), and `WriteMethods` /
/// `Finalise` walk the layout mechanically — no decisions, no row arithmetic.
/// `GenericParam` rows are collected and emitted last, sorted by
/// `CodedIndex.TypeOrMethodDef(owner)` then index, as SRM requires.
type internal Assembler
    (symbols: IExternalSymbolProvider, project: ProjectInfo, tasts: Frozen.TastFile list, bclReferences: string list) =

    let ctx = MetadataContext()
    do ctx.AddModuleAndAssembly(project.AssemblyName)

    // Referenced assemblies' identities read off their files and keyed by simple
    // name, so an emitted `AssemblyRef` matches the exact artifact, not whatever
    // the host loaded. `bclReferences` (the compilation's own BCL surface — a TFM
    // ref pack + `<Reference>`s, a driver-level input) feeds identity here so a
    // bootstrap `System.Runtime`/`System.Console` `AssemblyRef` binds the ref set,
    // NOT the host's `System.Private.CoreLib`. It is DELIBERATELY absent from
    // `project.References` (which `materialiseApp` copies beside the output): a
    // reference assembly has no IL and must never ship — the shared framework
    // supplies the real one at run time. `project.References` wins a simple-name
    // tie (it is folded last).
    let references =
        bclReferences @ project.References
        |> List.map (fun path ->
            let an = System.Reflection.AssemblyName.GetAssemblyName path
            an.Name, an
        )
        |> Map.ofList

    // The own-compilation intrinsic reprs are the UNION of every unit's `IntrinsicReprKeys`
    // — a `SymbolKey` identifies an intrinsic assembly-wide, so a key repeated across files
    // is the same declaration (a genuine duplicate would already be a front-end
    // duplicate-decl error), making last-wins union safe.
    let intrinsicReprKeys =
        let d = Dictionary<SymbolKey, string>()

        for tast in tasts do
            for kv in tast.IntrinsicReprKeys do
                d.[kv.Key] <- kv.Value

        d

    let provider =
        // Own-compilation intrinsics only; every other primitive's repr is read through the
        // provider (`ClrEnv.TryPrimitiveRepr`), the single source of truth harvested
        // from the dependency closure's `.fs`. No codegen-local repr table backs this up.
        ClrProvider(ctx, intrinsicReprKeys, references, symbols)

    let icodegen = provider :> ICodegenProvider
    let encodeLocals (locals: FrozenType list) = icodegen.EncodeLocalSignature locals

    // The narrow emission-side view of the provider (type/member shapes only). `Layout`
    // reads it to recognise the capability interfaces a nominal implements, and
    // `NominalEmit` re-derives the same co-slots from it when preparing their bodies.
    let codegenSymbols = CodegenSymbols.ofProvider symbols

    // One body-stream encoder shared by every method: a fresh encoder per body
    // would throw once a tiny body left the 4-byte-aligned builder unaligned;
    // `AddMethodBody` realigns per body internally, so reuse is correct.
    let bodyStream = ctx.BodyStream

    // One enumeration of every ranged-table row; every forward handle below is
    // a lookup into the prefix-sum derivation, not arithmetic. The layout also
    // carries the lowering products (lowered decls, holder plan, closures,
    // partition) computed once inside `Layout.build`.
    let layout = Layout.buildMany codegenSymbols project tasts
    let layoutHandles = Layout.deriveHandles layout

    // closure name → its `Closure` record, so the type-layout pass (keyed only by
    // `TypeSlotKey.Closure name`) can branch a value-struct closure onto struct attrs /
    // `System.ValueType` base. SHARED across units (the one `ClosureNamer` keeps names
    // unique assembly-wide); populated per unit in `buildPrelude`.
    let closureByName = Dictionary<string, Emit.Closure>()

    let closureIsValueStruct (name: string) : bool =
        match closureByName.TryGetValue name with
        | true, c -> c.IsValueStruct
        | false, _ -> false

    // The whole field table is written up front, straight off the layout; every later
    // phase resolves def handles by `FieldKey` instead of adding rows. The map is SHARED
    // — it spans the ONE combined field table, so any unit's body resolves a sibling
    // unit's field row through it.
    let fieldDefHandles = Dictionary<FieldKey, FieldDefinitionHandle>()

    // A numeric enum's `static literal` case fields each carry a `Constant` row whose
    // value is the case's underlying integer (boxed to the authored CLR primitive, so
    // SRM picks the matching `ConstantTypeCode`). Keyed by `FieldKey` (SymbolKey-based,
    // shared across units); the field pass attaches the constant as it writes each
    // literal field (ascending field order, which the `Constant` table is also sorted by).
    let enumFieldConstants = Dictionary<FieldKey, obj>()

    // The nominal registries are SHARED: keyed by nominal `SymbolKey`, so a call in one
    // unit's body resolves a type / member defined in another unit through the same
    // tables. `unions`/`records`/`classes`/`interfaces` are filled by the Bind /
    // `PrepareInterfaces` passes; `enums` is filled numeric here (in `buildPrelude`) and
    // struct post-field-pass (in `completeUnit`).
    let unions = Dictionary<SymbolKey, Emit.EmittedUnion>()
    let records = Dictionary<SymbolKey, Emit.EmittedRecord>()
    let classes = Dictionary<SymbolKey, Emit.EmittedClass>()
    let enums = Dictionary<SymbolKey, Emit.EmittedEnum>()
    let interfaces = Dictionary<SymbolKey, Emit.EmittedInterface>()

    // A module value's verdict-rewritten field-slot type, keyed by its `SymbolKey` — the
    // SAME identity `FieldKey.ModuleValue` carries, so the ONE shared field pass reads it
    // straight off the field key (a per-file `NodeKey` would collide here across units too).
    // Accumulated per unit in `buildPrelude` (each unit's own closure-verdict rewrite) and
    // read by the shared field pass — the single spot where the combined field table needs
    // a per-unit datum, surfaced as a lookup so the pass itself stays a plain walk of
    // `layout.Fields`.
    let moduleValueSlotType = Dictionary<SymbolKey, FrozenType>()

    // Per unit, BEFORE the shared field pass: register this unit's nominals with the
    // provider, mint its value-struct closure types, and build its closure-verdict
    // rewrite — all three feed a field's signature, so they must precede the pass. The
    // NodeKey / reference-keyed tables and the `EmitContext` are built post-field-pass in
    // `completeUnit`; the provider registries and the field / enum-constant / registry
    // maps this touches are SHARED.
    let buildPrelude (unit: UnitLayout) : UnitPrelude =
        let partitioned = unit.Partitioned
        let closures = unit.Closures
        let plan = unit.Plan

        for c in closures do
            closureByName.[c.Name] <- c

        // Register each nominal type's layout-derived `TypeDefinition` handle so a field
        // / factory / local signature can `encodeType` it before the row exists. A
        // *generic* type also registers its shape so the provider can mint `MemberRef`s
        // on its `TypeSpec`. Types are keyed by their nominal `SymbolKey` (namespace +
        // arity + home assembly), so overloads (`Choice\`2`…`Choice\`7`) never collide in
        // `userTypes` / `genericUnions`.
        for ud in partitioned.Unions do
            let td = ud.Decl
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            if not td.TypeParams.IsEmpty then
                let shape =
                    [
                        for c in ud.Cases ->
                            c.Name,
                            [
                                for fi in 0 .. c.Fields.Length - 1 -> sprintf "%s_%d" c.Name fi, snd c.Fields.[fi]
                            ]
                    ]

                provider.RegisterGenericUnion(td.TypeKey, EqArray.toList td.TypeParams, shape)

        for rd in partitioned.Records do
            let td = rd.Decl
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            // A `[<Struct>]` record is a project-local value type → `VALUETYPE` (not
            // `CLASS`) in every signature, exactly as a struct class.
            if rd.ValueKind <> ClassValueKind.RefType then
                provider.RegisterUserValueType td.Key

            if not td.TypeParams.IsEmpty then
                let shape = [ for f in rd.Fields -> f.Name, f.Type ]
                provider.RegisterGenericRecord(td.Key, EqArray.toList td.TypeParams, shape)

        for cd in partitioned.Classes do
            let td = cd.Decl
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            if cd.ValueKind <> ClassValueKind.RefType then
                provider.RegisterUserValueType td.Key

            if not td.TypeParams.IsEmpty then
                // The ctor-param backing fields, the explicit `val [mutable] x: T`
                // instance fields, and the instance-`let` / `static let` backing fields
                // must all be in the generic-class registry: a generic struct's
                // field-init ctor and member-body `ldfld`/`stfld` reference the `val`
                // fields by name through a `MemberRef` on the open self-`TypeSpec`, an
                // instance-`let` field is stored by the primary ctor and read from every
                // member body through that same `MemberRef`, and a generic `static let`
                // read/store (`ldsfld`/`stsfld`) goes through the same
                // `ClassMember.Field` `MemberRef` — an unregistered field fails
                // resolution ("generic class … has no field").
                let ctorParamFields = [ for p in cd.CtorParams -> p.Name, p.Type ]

                let shape =
                    ctorParamFields
                    @ [ for f in cd.Fields -> f.Name, f.Type ]
                    @ [ for l in TPreambleEntryG.lets cd.InstancePreamble -> l.Name, l.Type ]
                    @ [ for sl in TPreambleEntryG.lets cd.StaticPreamble -> sl.Name, sl.Type ]

                provider.RegisterGenericClass(td.Key, EqArray.toList td.TypeParams, List.length ctorParamFields, shape)

        // Interfaces register their `TypeDef` too, so one Core interface naming another
        // as a member-signature type (`IStructuralFormattable.Format(IFormatSink)`)
        // resolves through `userTypes` like any project-local nominal. A *generic*
        // interface (`IStructSeq<'E>`) also enters the generic-class registry so a
        // constrained-typar dispatch can mint its abstract slot as a `MemberRef` on the
        // instantiated interface `TypeSpec`.
        for (td, _) in partitioned.Interfaces do
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            if not td.TypeParams.IsEmpty then
                provider.RegisterGenericClass(td.Key, EqArray.toList td.TypeParams, 0, [])

        // Every enum — numeric (`System.Enum` subclass) or string/mixed (`[<Struct>]`
        // wrapper) — registers its layout-derived handle (its case fields are typed as
        // the enum itself, `FTEnum`) and, as a project-local value type (base chain
        // reaches `System.ValueType`), registers as a user value type →
        // `ELEMENT_TYPE_VALUETYPE`.
        for td in
            (partitioned.Enums |> List.map (fun ed -> ed.Decl))
            @ (partitioned.StructEnums |> List.map (fun sed -> sed.Decl)) do
            provider.RegisterUserType(td.Key, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))
            provider.RegisterUserValueType td.Key

        // Register this unit's home-local module functions so a SIBLING unit's cross-file
        // call resolves to the local `MethodDef` (`ClrRecipes.emitExternalCall` probes
        // `env.LocalModuleFns` before minting an `AssemblyRef`-scoped `MemberRef`). The
        // key is the fn's `SymbolKey` — the SAME identity `emitExternalCall` reconstructs
        // from the call's declaring module + name (a named-holder fn's `SymbolKey` is
        // `valueKey (InModule holder) name`), so the two sides meet. Holder-less fns
        // (`None`) are never cross-referenced — they live on the anonymous Program holder —
        // so skip them. For a single unit no `External` call ever targets this table,
        // leaving emission unchanged.
        for fn in plan.StaticFns do
            match fn.Holder with
            | Some _ ->
                let localMethodDef =
                    toEntity (layoutHandles.MethodDefOf(MethodKey.StaticFn fn.SymbolKey))

                provider.RegisterLocalModuleFn(fn.SymbolKey, localMethodDef)
            | None -> ()

        // A *generic* closure is a real generic `TypeDefinition`; its layout-derived
        // handle lets capture-field `MemberRef`s and the construction-site `Newobj` both
        // reach it. Monomorphic closures use their `Def` tokens directly.
        for c in closures do
            if c.Typars > 0 then
                let handle = toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure c.Name))

                provider.RegisterClosure(
                    c.Name,
                    c.Typars,
                    c.DeclaringTypars,
                    c.Captures |> List.map snd,
                    c.ParamTy,
                    c.ResultTy,
                    handle
                )

        let ctorHandleByNode =
            Dictionary<Frozen.TExpr, EntityHandle>(HashIdentity.Reference)

        // A non-capturing, monomorphic closure's cached singleton field: its
        // construction sites `ldsfld` this instead of `newobj`ing.
        let cachedClosureFieldByNode =
            Dictionary<Frozen.TExpr, EntityHandle>(HashIdentity.Reference)

        // A captureless `Stack` (value-struct) closure's synthetic encodable `FrozenType`
        // (the by-value local + the constrained-slot `MethodSpec` type-argument) and its
        // closure-`TypeDef` handle (`initobj` operand). Minted NOW, before the field table
        // is written — the module-value field substitution (`substituteVerdictClosures`)
        // must read `closureValueTypeByNode` while encoding a stored binding's `'TFunc`
        // slot, and that slot's field is in the up-front field pass. `BindClosures` reads
        // these already-minted entries rather than re-minting (`RegisterStackClosure-
        // ValueType` is single-shot — it fails on a duplicate `<closure>` key).
        let closureValueTypeByNode =
            Dictionary<Frozen.TExpr, FrozenType>(HashIdentity.Reference)

        let closureTypeDefByNode =
            Dictionary<Frozen.TExpr, EntityHandle>(HashIdentity.Reference)

        for c in closures do
            if c.IsValueStruct then
                let defHandle = toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure c.Name))
                let ft = provider.RegisterStackClosureValueType(c.Name, defHandle)
                closureValueTypeByNode.[c.Node] <- ft
                closureTypeDefByNode.[c.Node] <- defHandle

        // A numeric enum's case `Constant` values + its `NumericEnum` registry entry.
        // Both must exist before the field pass (which attaches the `Constant` rows). SRM
        // reads the `ConstantTypeCode` off the box's RUNTIME type, so each value is boxed
        // at the width's own .NET primitive — `IntWidth.boxed`.
        for ed in partitioned.Enums do
            for (caseName, v) in ed.Cases do
                let w, bits = TEnumCases.integralValue v
                enumFieldConstants.[FieldKey.EnumCaseField(ed.Decl.Key, caseName)] <- IntWidth.boxed w bits

            let caseValues = Dictionary<string, TConstValue>()

            for (caseName, v) in ed.Cases do
                caseValues.[caseName] <- v

            enums.[ed.Decl.Key] <-
                {
                    Repr = Emit.EmittedEnumRepr.NumericEnum caseValues
                }

        // The seq→enumerator witness the closure-verdict rewrite needs to rewrite a
        // chained binding's nested `'E` ENUMERATOR slot node-keyed (NOT by arrow shape).
        // For a project-local seq class, its `GetEnumerator` interface-impl member's
        // RETURN type is the enumerator over the class's declaring typars; map each seq
        // class key → that template, then `enumeratorOf` instantiates it by a concrete
        // seq nominal's args. Computed from THIS unit's class decls because `env.Classes`
        // is not yet populated at the up-front field pass.
        let enumeratorTemplateByClass =
            let d = Dictionary<SymbolKey, FrozenType>()

            for cd in partitioned.Classes do
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
        // nominal's args (`FrozenTypeBridge.substituteDeclaring`). `ValueNone` when the
        // nominal is not a project-local seq class (no template).
        let enumeratorOf (seqTy: FrozenType) : FrozenType voption =
            match seqTy with
            | FTClass(key, args) ->
                match enumeratorTemplateByClass.TryGetValue(SymbolKey.Type key) with
                | true, template -> ValueSome(substituteDeclaring (args.AsSpan().ToArray()) template)
                | false, _ -> ValueNone
            | _ -> ValueNone

        // The closure-verdict TAST rewrite for THIS unit's bodies, from backend-neutral
        // inputs (this unit's already-minted value-struct closure types + its result-typar
        // verdicts + its stored module values + the seq→enumerator witness). It owns
        // `substituteVerdictClosures` / `retypeBody` / `retypeDecl` and the field-slot
        // lookup; see `ClosureVerdictRewrite`.
        let verdict =
            ClosureVerdictRewrite.build
                closureValueTypeByNode
                unit.FunVerdicts
                enumeratorOf
                [ for mv in plan.AllModuleValues -> mv.Key, mv.Ty, mv.Init ]

        // Surface this unit's module-value slot types into the shared lookup the field
        // pass reads. A non-verdict binding stores its declared type unchanged (the field
        // pass would encode the same), so the pass need not know the verdict itself.
        for mv in plan.AllModuleValues do
            moduleValueSlotType.[mv.SymbolKey] <- verdict.ModuleValueSlotType mv.Key mv.Ty

        {
            Layout = unit
            CtorHandleByNode = ctorHandleByNode
            CachedClosureFieldByNode = cachedClosureFieldByNode
            ClosureValueTypeByNode = closureValueTypeByNode
            ClosureTypeDefByNode = closureTypeDefByNode
            Verdict = verdict
        }

    // Force every unit's prelude BEFORE the field pass, so all units' nominals are
    // registered and value-struct closures minted by the time any field signature is
    // encoded.
    let unitPreludes = layout.Units |> List.map buildPrelude

    // Tables are independent (only intra-table order matters), so the whole field table
    // is written up front, straight off the layout — the ONE combined row space across
    // every unit. Every later phase resolves def handles by `FieldKey`. A generic
    // closure's capture-field signature encodes inside the ambient closure-typar scope,
    // bracketed per slot.
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
                        provider.ClosureSelfFieldSignature(toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure name)))
                    // A stored module value whose initialiser feeds a value-struct source
                    // lambda into a `'TFunc`-carrying result type — the owning unit's
                    // verdict rewrote the slot to the `<closure>$` value-struct, surfaced
                    // through `moduleValueSlotType`. Absent ⇒ the declared type unchanged.
                    | FieldKey.ModuleValue mvKey ->
                        let slotTy =
                            match moduleValueSlotType.TryGetValue mvKey with
                            | true, t -> t
                            | false, _ -> fs.Ty

                        provider.FieldSignature slotTy
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

            // A numeric enum case field: attach its `Constant` row now, in field
            // order, so the `Constant.Parent` column is ascending.
            match enumFieldConstants.TryGetValue fs.Key with
            | true, boxed -> ctx.AddConstant(toEntity h, boxed) |> ignore
            | false, _ -> ()

        if ctx.FieldRowCount <> layoutHandles.TotalFields then
            failwithf
                "Layout: field table has %d rows after the field pass, layout owns %d"
                ctx.FieldRowCount
                layoutHandles.TotalFields

    // Per unit, AFTER the field pass: the field-derived tables (struct-enum registry,
    // static-method refs, module-value field handles) and this unit's `EmitContext`. A
    // FRESH EmitContext per unit keeps its NodeKey-keyed tables (`StaticMethods` /
    // `ModuleValues` / `MainInitValues`) and reference-keyed closure tables from
    // colliding across files; its nominal registries are the SHARED ones.
    let completeUnit (pre: UnitPrelude) : UnitEmit =
        let unit = pre.Layout
        let partitioned = unit.Partitioned
        let plan = unit.Plan

        // String/mixed enums: the per-case `static initonly` field handles (read off the
        // completed field pass) drive `E.A` `ldsfld`, and the case literals + backing
        // field handle drive the `| E.A` pattern's field equality.
        for sed in partitioned.StructEnums do
            let caseFields = Dictionary<string, EntityHandle>()
            let caseLits = Dictionary<string, TEnumLiteral>()

            for (caseName, lit) in sed.Cases do
                caseFields.[caseName] <- toEntity fieldDefHandles.[FieldKey.EnumCaseField(sed.Decl.Key, caseName)]
                caseLits.[caseName] <- lit

            let backingField = toEntity fieldDefHandles.[FieldKey.EnumBackingField sed.Decl.Key]

            enums.[sed.Decl.Key] <-
                {
                    Repr = Emit.EmittedEnumRepr.StructEnum(sed.IsMixed, backingField, caseFields, caseLits)
                }

        // A static fn's call sites resolve through its layout-derived `MethodDef` handle;
        // recursion and cross-calls need no emission-order discipline.
        let staticMethods = Dictionary<NodeKey, Emit.StaticMethodRef>()

        for fn in plan.StaticFns do
            staticMethods.[fn.Key] <-
                {
                    Handle = toEntity (layoutHandles.MethodDefOf(MethodKey.StaticFn fn.SymbolKey))
                    // The flat CLR arg count (the `call` operand count); the spine split
                    // uses `Groups.Length`, which can be smaller (a tupled group is one
                    // application, many flat params).
                    ParamArity = List.length fn.Params
                    Groups = fn.Groups
                    ResultTy = fn.ResultTy
                    // `plan.StaticFnTypars` is the max method index over params + result +
                    // BODY, so a generic combinator emits a `MethodSpec` slot for each
                    // phantom typar surviving in its body (`fold`'s `'E`) — the call site
                    // solves those from `Constraints`.
                    Typars = plan.StaticFnTypars.[fn.Key]
                    ParamTys = fn.Params |> List.map (fun p -> p.Ty)
                    ReturnsVoid = fn.ReturnsVoid
                    // The frozen typar bounds the call-site phantom-typar solve (`EmitCall`)
                    // reads to recover the phantom method-typar slots no parameter/result
                    // mentions.
                    Constraints = fn.Constraints
                }

        // Module-value bindings resolve to their already-written field rows — any body
        // encodes the `ldsfld` token straight off the def handle.
        let moduleValueFields = Dictionary<NodeKey, EntityHandle>()

        for mv in plan.AllModuleValues do
            moduleValueFields.[mv.Key] <- toEntity fieldDefHandles.[FieldKey.ModuleValue mv.SymbolKey]

        // The trailing top-level values: their `public static` field is written in `Main`
        // (`buildMain` `stsfld`), not a `.cctor`. Same field handles, a separate map so
        // `buildMain` knows to emit the store (vs the cctor-initialised values it skips).
        let mainInitValues = Dictionary<NodeKey, EntityHandle>()

        for mv in plan.ProgramMainValues do
            mainInitValues.[mv.Key] <- moduleValueFields.[mv.Key]

        let emitCtx: Emit.EmitContext =
            {
                Provider = icodegen
                Ctx = ctx
                ClosureByNode = unit.ClosureByNode
                CtorHandleByNode = pre.CtorHandleByNode
                CachedClosureFieldByNode = pre.CachedClosureFieldByNode
                ClosureValueTypeByNode = pre.ClosureValueTypeByNode
                ClosureTypeDefByNode = pre.ClosureTypeDefByNode
                Unions = unions
                Records = records
                Classes = classes
                Interfaces = interfaces
                Enums = enums
                StaticMethods = staticMethods
                ModuleValues = moduleValueFields
                MainInitValues = mainInitValues
            }

        {
            Layout = unit
            Verdict = pre.Verdict
            EmitCtx = emitCtx
        }

    let units = unitPreludes |> List.map completeUnit

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

    // A numeric enum's `TypeDefinition`: a sealed `auto ansi` class extending
    // `System.Enum` (the base supplies value-type-ness + equality/hashing/compare).
    // No `BeforeFieldInit` — there is no `.cctor` (the case fields are `literal`,
    // baked into the `Constant` table, not initialised at runtime).
    let enumAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass

    // A string/mixed enum's `[<Struct>]` wrapper: a sealed value type
    // (sequential layout, `System.ValueType` base) with NO `BeforeFieldInit` — its
    // `.cctor` materialises the case singletons and must run before the first case
    // `ldsfld` (precise-init semantics, like a holder owning module values).
    let structEnumAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.SequentialLayout
        ||| TypeAttributes.AnsiClass

    // An interface: `abstract`, no base, no fields.
    let interfaceAttrs =
        TypeAttributes.Interface ||| TypeAttributes.Abstract ||| TypeAttributes.Public

    // A module holder / the anonymous "Program" holder: an `abstract sealed` static
    // class. A holder owning module-value fields has a side-effecting `.cctor`; drop
    // `BeforeFieldInit` so it runs before first member access. This is *first-access*
    // (lazy, per-holder) initialisation — real F# runs file-scope bindings eagerly in
    // file order via startup code, so a side-effecting initialiser could observe a
    // different order; the pure values in this slice's scope can't tell the difference.
    let holderAttrsOf (hasCctor: bool) =
        let baseAttrs =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.AutoLayout

        if hasCctor then
            baseAttrs
        else
            baseAttrs ||| TypeAttributes.BeforeFieldInit

    // Nested visibility REPLACES the 3-bit visibility field rather than adding to it, so
    // a nested type's `Public` becomes `NestedPublic`. Uniformly `NestedPublic`: nothing
    // models `internal` in emission today (every attribute set above hard-codes
    // `Public`), so a narrower nested visibility would REGRESS a `module internal` from
    // the public class it emits today, not fix it. The rule to honour when accessibility
    // lands is that a nested type's visibility is the MINIMUM of its own and its holder
    // chain's — `internal` is assembly-scoped, so anything inside an assembly-scoped
    // module is at most assembly-scoped.
    let nestedAttrsOf (enclosing: TypeSlotKey voption) (attrs: TypeAttributes) =
        match enclosing with
        | ValueNone -> attrs
        | ValueSome _ -> (attrs &&& ~~~TypeAttributes.VisibilityMask) ||| TypeAttributes.NestedPublic

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
    let typeRowExtras = Dictionary<TypeSlotKey, TypeRowExtras>()

    member _.Provider = provider
    member _.Icodegen = icodegen
    member _.Ctx = ctx
    member _.BodyStream = bodyStream
    member _.EncodeLocals = encodeLocals
    member _.Unions = unions
    member _.Records = records
    member _.Classes = classes

    /// The per-unit emission state: a fresh `EmitContext` plus this unit's NodeKey /
    /// reference-keyed tables and its closure-verdict rewrite. The Bind / Prepare passes
    /// iterate these; the nominal registries, the field-handle map and the one combined
    /// row space live on the Assembler and are shared across units.
    member _.Units: UnitEmit list = units

    /// The emission-side symbol view — `NominalEmit` derives a nominal's capability
    /// co-slots from it (`CapabilityCoSlots.required`) exactly as `Layout` did when it
    /// reserved their rows.
    member _.Symbols: ICodegenSymbols = codegenSymbols

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
    member _.AddTypeRowExtras(key: TypeSlotKey, extras: TypeRowExtras) = typeRowExtras.Add(key, extras)

    // The construction-site `Newobj` targets the ctor's `Def` directly via
    // this dict. Generic closures mint a fresh `MemberRef` at the use site
    // instead (dict left unpopulated).
    member this.BindClosures(u: UnitEmit) =
        for c in u.Layout.Closures do
            if c.Typars = 0 then
                u.EmitCtx.CtorHandleByNode.[c.Node] <-
                    toEntity (layoutHandles.MethodDefOf(MethodKey.ClosureCtor c.Name))

            // A non-capturing, monomorphic closure is cached: the construction site
            // `ldsfld`s its singleton field instead of `newobj`ing.
            if Emit.closureIsCached c then
                u.EmitCtx.CachedClosureFieldByNode.[c.Node] <-
                    toEntity (fieldDefHandles.[FieldKey.ClosureCached c.Name])

            // A captureless `Stack` (value-struct) closure is
            // constructed by-value (`initobj` to a local) and its struct `TypeDef`
            // is the constrained-slot `MethodSpec` type-argument at the call site.
            // Its synthetic value-type `FrozenType` + `TypeDef` handle were already
            // minted in `buildPrelude` (before the field pass, so the stored-slot
            // substitution could read them); `RegisterStackClosureValueType` is
            // single-shot, so this only asserts they are present — never re-mints.
            if c.IsValueStruct && not (u.EmitCtx.ClosureValueTypeByNode.ContainsKey c.Node) then
                failwithf "Emit: value-struct closure '%s' was not pre-minted before the field pass" c.Name

    member this.PrepareInterfaces(u: UnitEmit) =
        for (td, methods) in u.Layout.Partitioned.Interfaces do
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
                        ParamArity = List.length paramTys
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

    /// Prepare each string/mixed enum's `.ctor` (stores the wrapped value) and
    /// `.cctor` (constructs every case singleton), and record its `System.ValueType`
    /// base for the `TypeDefinition` row. The per-case field handles + literals were
    /// captured in the `enums` registry (after the field pass); here they drive the
    /// `newobj;stsfld` sequence.
    member this.PrepareStructEnums(u: UnitEmit) =
        for sed in u.Layout.Partitioned.StructEnums do
            let td = sed.Decl

            let fieldTy =
                FTConst(
                    (if sed.IsMixed then
                         RuntimeNames.objKey
                     else
                         RuntimeNames.stringKey),
                    EqArray.empty
                )

            // `caseLits` (the registry's case → literal map) feeds the `| E.A`
            // pattern's field equality, not the `.cctor` — here the literals come
            // straight off `sed.Cases` in declaration order.
            let backingField, caseFields =
                match enums.[td.Key].Repr with
                | Emit.EmittedEnumRepr.StructEnum(_, bf, cf, _) -> bf, cf
                | other -> failwithf "Emit: struct enum '%A' has a non-struct repr %A" td.Key other

            // The single-arg value-type `.ctor(value)` storing the backing field.
            let ctorBody =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStructCtor [ backingField ]))

            this.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.RecordCtorSignature [ fieldTy ]
                    BodyOffset = ctorBody
                    ParamNames = [ "value" ]
                    MethodTypars = []
                }
            )

            // The `.cctor` constructs each case singleton in declaration order.
            let ctorHandle = toEntity (layoutHandles.MethodDefOf(MethodKey.NominalCtor td.Key))

            let cctorCases =
                [
                    for (caseName, lit) in sed.Cases ->
                        caseFields.[caseName], EmitResolve.enumLiteralPush icodegen.TypeToken ctx.UserString lit
                ]

            let cctorBody =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStructEnumCctor ctorHandle cctorCases))

            this.AddPrepared(
                MethodKey.NominalCctor td.Key,
                {
                    Signature = provider.CctorSignature()
                    BodyOffset = cctorBody
                    ParamNames = []
                    MethodTypars = []
                }
            )

            // The wrapper extends `System.ValueType` (value-type-ness); no interfaces.
            this.AddTypeRowExtras(
                TypeSlotKey.Nominal td.Key,
                {
                    Interfaces = []
                    BaseType = provider.ValueTypeBase
                }
            )

    // A *generic* closure enters closure-typar mode around every signature/body
    // build, so the body's `FTTypar(Method, i)` (the enclosing method's typars)
    // re-project onto this closure class's `!i`.
    member this.PrepareClosures(u: UnitEmit) =
        for c in u.Layout.Closures do
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
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureInvoke u.EmitCtx c captureFields))

            this.AddPrepared(
                MethodKey.ClosureCtor c.Name,
                {
                    Signature = provider.ClosureCtorSignature(List.map snd c.Captures)
                    BodyOffset = ctorBodyOffset
                    ParamNames = argNames (List.length c.Captures)
                    MethodTypars = []
                }
            )

            // A flat (`Fun`(N+1)`) closure's `Invoke` takes all `FunArity` flat
            // params (`Invoke(arg0, …, arg{N-1}) : result`); arity-1 reduces to the
            // single-arg `Invoke(arg0) : result`. `InvokeSignatureN` yields bytes
            // identical to the old per-arity encoders for arity 1/2.
            let invokeSignature, invokeParamNames =
                let paramTys = c.ParamTy :: (c.ExtraParams |> List.map (fun (_, ty, _) -> ty))
                let names = [ for i in 0 .. c.FunArity - 1 -> sprintf "arg%d" i ]
                provider.InvokeSignatureN(paramTys, c.ResultTy), names

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
            // still installed, so free `TyVar`s encode to `!i`. A flat (arity ≥2)
            // value-struct closure implements the wider `Fun`(N+1)<a,…,result>`
            // instead (`Fun`3`/`Fun`4`/`Fun`5`).
            let ifaceSpec =
                match c.FunArity with
                | 1 -> provider.FunInterfaceSpec(c.ParamTy, c.ResultTy)
                | _ ->
                    let tys =
                        (c.ParamTy :: (c.ExtraParams |> List.map (fun (_, ty, _) -> ty)))
                        @ [ c.ResultTy ]

                    provider.FlatFunInterfaceSpecN(tys)

            if isGenericClosure then
                let closureHandle = toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure c.Name))

                for i in 0 .. c.Typars - 1 do
                    genericParams.Add(closureHandle, i, sprintf "T%d" i)

                provider.ExitClosureTyparScope()

            typeRowExtras.Add(
                TypeSlotKey.Closure c.Name,
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

    member this.PrepareStaticMethods(u: UnitEmit) =
        let plan = u.Layout.Plan
        let emitCtx = u.EmitCtx
        let staticMethods = emitCtx.StaticMethods
        let moduleValueFields = emitCtx.ModuleValues
        let retypeBody = u.Verdict.RetypeBody

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
                MethodKey.StaticFn fn.SymbolKey,
                {
                    Signature = signature
                    BodyOffset = bodyOffset
                    ParamNames = argNames (List.length fn.Params)
                    MethodTypars = [ for i in 0 .. typarCount - 1 -> sprintf "T%d" i ]
                }
            )

        // A holder's `.cctor` initialises its module values in declaration order
        // (the static analogue of the class `static let` cctor — same
        // `buildStaticCctor` recipe, `stsfld` into each field).
        let prepareHolderCctor (h: Emit.HolderKey) =
            let lets =
                [
                    for mv in HolderPlan.holderValues plan h ->
                        Emit.PreambleStep.Store(moduleValueFields.[mv.Key], retypeBody mv.Init)
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
                    for mv in plan.ProgramCctorValues ->
                        Emit.PreambleStep.Store(moduleValueFields.[mv.Key], retypeBody mv.Init)
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

    /// `Main` belongs to the ENTRY unit only — the one whose layout carries the entry
    /// point. A non-entry unit contributes no `Main` row, so this is a no-op for it.
    member this.PrepareMain(u: UnitEmit) =
        if u.Layout.EmitEntryPoint then
            // Retype the Main decls so a reference to a verdict module
            // value (and its field projections) dispatches on the `<closure>$` value-
            // struct nominal, not the frozen arrow.
            let mainDecls = u.Layout.Lowered |> List.map u.Verdict.RetypeDecl

            let mainBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildMain u.EmitCtx mainDecls))

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

        // The `NestedClass` row of a type the layout nests. Written HERE — while the
        // NESTED type is being written, not its enclosing one — so the table comes out
        // sorted by the nested handle (which is what SRM validates), the `TypeDef` walk
        // being ascending. The enclosing handle is already minted: a node's enclosing
        // type precedes it in the pre-order flattening.
        let addNesting (node: TypeNode) (typeHandle: TypeDefinitionHandle) =
            match node.Enclosing with
            | ValueNone -> ()
            | ValueSome encl -> ctx.AddNestedType(typeHandle, layoutHandles.TypeDefOf encl)

        // Union, record, class, and closure `TypeDefinition` rows share one
        // recipe, with the Prepare-minted `InterfaceImpl` / `BaseType` handles.
        // Walking the layout in order keeps the `InterfaceImpl` /
        // `GenericParam` rows ascending (sorted by `Class` / `TypeOrMethodDef`).
        let addNominalRow (node: TypeNode) (attrs: TypeAttributes) (isByRefLike: bool) =
            let slot = node.Slot

            let extras =
                match typeRowExtras.TryGetValue slot.Key with
                | true, e -> e
                | _ -> failwithf "Layout: type slot '%s' was never prepared" slot.MetaName

            let typeHandle =
                ctx.AddClass(
                    nestedAttrsOf node.Enclosing attrs,
                    slot.Namespace,
                    slot.MetaName,
                    extras.BaseType,
                    layoutHandles.FirstFieldOf slot.Key,
                    layoutHandles.FirstMethodOf slot.Key
                )

            verifyTypeHandle slot typeHandle
            addNesting node typeHandle

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

        for node in layout.Types do
            let slot = node.Slot

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
                        nestedAttrsOf node.Enclosing interfaceAttrs,
                        slot.Namespace,
                        slot.MetaName,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle
                addNesting node typeHandle

                slot.Typars
                |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n))

            // Unions and records are always sealed (subclassing /
            // inheritance forbidden); a class opts in via `[<Sealed>]` / `[<Struct>]`.
            // A record additionally opts into value-type emission via `[<Struct>]`
            // (`System.ValueType` base); it is never byref-like.
            | TypeSlotKind.Union -> addNominalRow node (classAttrsOf true false) false
            | TypeSlotKind.Record valueKind ->
                addNominalRow node (classAttrsOf true (valueKind <> ClassValueKind.RefType)) false

            | TypeSlotKind.Class(isSealed, valueKind) ->
                let isValueType = valueKind <> ClassValueKind.RefType
                let isByRefLike = valueKind = ClassValueKind.RefStruct
                addNominalRow node (classAttrsOf isSealed isValueType) isByRefLike

            // A numeric enum: base = `System.Enum`, no interfaces, no
            // methods. It needs no `TypeRowExtras` (no synthesised eq/comp/format
            // interfaces — `System.Enum` supplies them), so it is written directly
            // rather than through `addNominalRow`.
            | TypeSlotKind.Enum ->
                let typeHandle =
                    ctx.AddClass(
                        nestedAttrsOf node.Enclosing enumAttrs,
                        slot.Namespace,
                        slot.MetaName,
                        provider.EnumBase,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle
                addNesting node typeHandle

            // A string/mixed enum: a `[<Struct>]` value type over
            // `System.ValueType` with a `.ctor` + `.cctor`. Routed through
            // `addNominalRow` (it carries `TypeRowExtras` — the `ValueType` base set
            // in `PrepareStructEnums`); never byref-like.
            | TypeSlotKind.StructEnum _ -> addNominalRow node structEnumAttrs false

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
                    | TypeSlotKey.Closure name when closureIsValueStruct name -> classAttrsOf true true
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

            // Named-module holders: one static class per `module Foo`, nested in its
            // parent module's holder when the module nests. A holder owning module
            // values takes its own `FieldList` and drops `BeforeFieldInit` (its
            // `.cctor` runs before first access); a value-less holder's empty field
            // range points past the previous owner's range (the prefix sum).
            | TypeSlotKind.Holder hasCctor ->
                let typeHandle =
                    ctx.AddProgramType(
                        nestedAttrsOf node.Enclosing (holderAttrsOf hasCctor),
                        slot.Namespace,
                        slot.MetaName,
                        provider.ObjectType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle
                addNesting node typeHandle

            // The anonymous "Program" holder owns the holder-less static methods
            // (and `Main`, when an executable) and the top-level value fields.
            // `hasCctor` ⇔ it owns leading-prefix values, dropping `BeforeFieldInit`
            // so its `.cctor` runs before `Main`. Its presence is a layout decision
            // (`Layout.build`).
            | TypeSlotKind.Program hasCctor ->
                let typeHandle =
                    ctx.AddProgramType(
                        holderAttrsOf hasCctor,
                        slot.Namespace,
                        slot.MetaName,
                        provider.ObjectType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
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
