namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open Vesper
open XParsec.FSharp.SemanticAnalysis

/// A type's CLR reflection name and its qualifying assembly. `Assembly` is `ValueNone` for
/// a type of the assembly under emission, which II.23.3 spells bare.
type internal ReflectionName =
    {
        FullName: string
        Assembly: string voption
    }

    /// The `Type.AssemblyQualifiedName` spelling: `Ns.Name, Asm`.
    member this.Qualified: string =
        match this.Assembly with
        | ValueSome asm -> this.FullName + ", " + asm
        | ValueNone -> this.FullName

module internal ClrTypeNames =

    /// Every nominal the layout places, keyed to its reflection name: the `+`-joined chain
    /// of metadata names beneath the root's namespace. A module-held type appears under the
    /// module CLASS's name.
    let localNames (layouts: FileLayout list) : Dictionary<TypeKey, string> =
        let names = Dictionary<TypeKey, string>()

        let rec walk (enclosing: string voption) (node: TypeNode) =
            let self =
                match enclosing with
                | ValueSome outer -> outer + "+" + node.Slot.MetaName
                | ValueNone ->
                    match node.Slot.Namespace with
                    | "" -> node.Slot.MetaName
                    | ns -> ns + "." + node.Slot.MetaName

            match node.Slot.Key with
            | TypeSlotKey.Nominal(SymbolKey.Type key) -> names.[key] <- self
            | _ -> ()

            for child in node.Nested do
                walk (ValueSome self) child

        for layout in layouts do
            for root in layout.Roots do
                walk ValueNone root

        names

/// `FrozenType` → its reflection name in a `CustomAttrib` blob: the `Type` element (`0x50`)
/// and the enum SerString of a named enum-typed argument (`0x55`).
type internal ClrTypeNamer(provider: ClrProvider, localNames: IReadOnlyDictionary<TypeKey, string>) =

    /// `fullName` applied to `args`; `ValueNone` where any argument is unspellable.
    let render (fullName: string) (asm: string voption) (args: ReflectionName voption list) : ReflectionName voption =
        if List.exists ValueOption.isNone args then
            ValueNone
        else
            let applied =
                match args with
                | [] -> fullName
                | args ->
                    let rendered =
                        args
                        |> List.map (fun a -> "[" + (ValueOption.get a).Qualified + "]")
                        |> String.concat ","

                    fullName + "[" + rendered + "]"

            ValueSome { FullName = applied; Assembly = asm }

    let assemblyOf (path: ExternalTypePath) : string voption =
        path.Home.AssemblyOption |> ValueOption.map (fun asm -> asm.Name)

    let rec tryName (t: FrozenType) : ReflectionName voption =
        match t with
        // An array's assembly is its element's: `System.Int32[], System.Runtime`.
        | FTArray elem ->
            tryName elem
            |> ValueOption.map (fun e -> { e with FullName = e.FullName + "[]" })
        | FTConst(key, args) ->
            match provider.TryPrimitiveTypeId key with
            | ValueSome id ->
                let platformKey = SymbolKeyOps.qualifiedTypeKeyOf id.Value 0

                match PlatformTypeIds.tryReflectionName id with
                | ValueSome name ->
                    render
                        name
                        (provider.TryExternalTypePath platformKey |> ValueOption.bind assemblyOf)
                        (argNames args)
                | ValueNone -> applied platformKey (argNames args)
            | ValueNone -> applied key (argNames args)
        | FTRecord(key, args)
        | FTUnion(key, args)
        | FTClass(key, args) -> applied key (argNames args)
        | FTEnum key ->
            match ClrAttributeNames.tryBclEnumFullName key with
            | ValueSome name -> render name ValueNone []
            | ValueNone -> applied key []
        // The `System.ValueTuple` family, nesting past arity 7 exactly as the signature
        // encoder does.
        | FTTuple items -> tupleName items 0
        // Every function value implements the curried `Vesper.Fun`2`.
        | FTFun(a, b) -> applied (RuntimeNames.vesperFunKey 2) [ tryName a; tryName b ]
        | _ -> ValueNone

    and argNames (args: Block<FrozenType>) : ReflectionName voption list = [ for a in args -> tryName a ]

    and applied (key: TypeKey) (args: ReflectionName voption list) : ReflectionName voption =
        match localNames.TryGetValue key with
        | true, name -> render name ValueNone args
        | _ ->
            provider.TryExternalTypePath key
            |> ValueOption.bind (fun path -> render path.FullName (assemblyOf path) args)

    and tupleName (items: Block<FrozenType>) (start: int) : ReflectionName voption =
        let remaining = items.Length - start
        let itemName i = tryName items.[start + i]

        if ClrTuples.fitsOneMember remaining then
            applied (ClrTuples.typeKey remaining) [ for i in 0 .. remaining - 1 -> itemName i ]
        else
            applied
                (ClrTuples.typeKey remaining)
                [
                    for i in 0 .. ClrTuples.MaxDirect - 1 -> itemName i
                    yield tupleName items (start + ClrTuples.MaxDirect)
                ]

    /// The SerString a `Type` element or an enum-typed named argument carries; `ValueNone`
    /// where the encoder cannot spell the type.
    member _.TryTypeName(t: FrozenType) : string voption =
        tryName t |> ValueOption.map (fun n -> n.Qualified)
