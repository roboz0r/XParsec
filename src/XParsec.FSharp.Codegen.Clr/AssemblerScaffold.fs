namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// Stateless helpers shared by the converged `Assembler` and the `Codegen`
/// facade: forward-handle prediction, the `tast.Decls` partition, signature
/// builders, and the hand-written-body test seam (`assembleWith`).
module internal AssemblerScaffold =

    /// Forward-handle prediction for the deferred `TypeDefinition` rows.
    /// `<Module>` occupies row 1; the trailing emission loop walks interfaces →
    /// unions → records → classes → closures. The i-th type of `kind` lands at
    /// `2 + (sum of prior-kind counts) + i`.
    let predictTypeDef (counts: TypeDefCounts) (kind: NominalKind) (i: int) : TypeDefinitionHandle =
        let priorRows =
            match kind with
            | NominalKind.Interface -> 0
            | NominalKind.Union -> counts.Interfaces
            | NominalKind.Record -> counts.Interfaces + counts.Unions
            | NominalKind.Class -> counts.Interfaces + counts.Unions + counts.Records
            | NominalKind.Closure -> counts.Interfaces + counts.Unions + counts.Records + counts.Classes

        MetadataTokens.TypeDefinitionHandle(2 + priorRows + i)

    /// Single-walk partition of `tast.Decls` by `TTypeKind`.
    let partitionTypeDecls (decls: EqArray<TDecl>) : PartitionedTypeDecls =
        let interfaces = ResizeArray()
        let unions = ResizeArray()
        let records = ResizeArray()
        let classes = ResizeArray()

        for d in decls do
            match d with
            | TDecl.Type td ->
                match td.Kind with
                | TTypeKind.Interface methods -> interfaces.Add(td, EqArray.toList methods)
                | TTypeKind.Union(cases, members) -> unions.Add(td, EqArray.toList cases, EqArray.toList members)
                | TTypeKind.Record(fields, members) -> records.Add(td, EqArray.toList fields, EqArray.toList members)
                | TTypeKind.Class(fields, ctorParams, members, baseType, _ifaces, isSealed, staticLets, secondaryCtors) ->
                    classes.Add(
                        td,
                        EqArray.toList fields,
                        EqArray.toList ctorParams,
                        EqArray.toList members,
                        baseType,
                        isSealed,
                        EqArray.toList staticLets,
                        EqArray.toList secondaryCtors
                    )
            | _ -> ()

        {
            Interfaces = List.ofSeq interfaces
            Unions = List.ofSeq unions
            Records = List.ofSeq records
            Classes = List.ofSeq classes
        }

    /// `int Main(string[])` — the synthesised entry point's signature.
    let mainSignature () : BlobBuilder =
        let sigB = BlobBuilder()

        BlobEncoder(sigB)
            .MethodSignature()
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().SZArray().String())
            )

        sigB

    /// An abstract interface method (no body).
    let abstractMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Abstract
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot

    /// `TyFun('A, 'B)` ⇒ `(['A], 'B)`.
    let rec decurry (t: SemType) : SemType list * SemType =
        match t with
        | TyFun(a, b) ->
            let ps, r = decurry b
            a :: ps, r
        | _ -> [], t

    let argNames (n: int) : string list =
        [ for i in 0 .. n - 1 -> sprintf "arg%d" i ]

    /// A property is emitted (and referenced) as `get_<name>`; a method keeps
    /// its name.
    let memberMetaName (mem: TTypeMember) : string =
        match mem.Kind with
        | TMemberKind.Property -> "get_" + mem.Name
        | TMemberKind.Method -> mem.Name

    /// `instance <ret> <name><'C…>(<params…>)` for an abstract interface
    /// method, with the declaring type's typars resolved to
    /// `GenericTypeParameter` indices and the method's own typars to
    /// `GenericMethodParameter` indices. Concrete leaves are encoded by the
    /// provider's `EncodeAbstractType` (the same `encodeType` the executable
    /// path uses).
    let abstractMethodSignature
        (provider: ClrProvider)
        (typeParams: EqArray<string>)
        (m: TAbstractMethod)
        : BlobBuilder =
        let typeIx =
            let mutable acc = Map.empty
            typeParams |> EqArray.iteri (fun i n -> acc <- Map.add n i acc)
            acc

        let methodIx =
            let mutable acc = Map.empty
            m.MethodTypeParams |> EqArray.iteri (fun i n -> acc <- Map.add n i acc)
            acc

        let paramTys, retTy = decurry m.Signature
        let blob = BlobBuilder()

        BlobEncoder(blob)
            .MethodSignature(genericParameterCount = m.MethodTypeParams.Length, isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> provider.EncodeAbstractType(typeIx, methodIx, ret.Type(), retTy)),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        provider.EncodeAbstractType(typeIx, methodIx, pars.AddParameter().Type(), p)
                )
            )

        blob

    /// Shared assembly scaffolding for a hand-written `Main` body: module +
    /// assembly rows, a `Main` whose body comes from `build`, the `<Module>`
    /// pseudo-type, and the holder class. The hand-written body forms no
    /// function value / list literal / external member access, so `Vesper.Fun`
    /// / `Vesper.List` are never needed and the symbol provider is the null one.
    let assembleWith (project: ProjectInfo) (build: MetadataContext -> ClrProvider -> (Il -> unit)) : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

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
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }
