module XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// Read the emitted PE through `System.Reflection.Metadata` (method/field tokens, the
// AssemblyRef table, raw IL). The `pe*` readers work straight off the bytes; `loadAssembly`
// and `programClassMethods*` reflect over the same bytes through the runtime loader.

open System
open System.Reflection
open System.Runtime.Loader
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

/// Load emitted PE bytes into a FRESH `AssemblyLoadContext`. Loading the same bytes twice
/// gives two assemblies, and mixing them throws "Object of type X cannot be converted to
/// type X", so reflect every member through the ONE `Assembly` this returns.
let loadAssembly (bytes: byte[]) : Assembly =
    let alc = AssemblyLoadContext("xparsec-codegen-test", isCollectible = true)
    use ms = new IO.MemoryStream(bytes)
    alc.LoadFromStream ms

/// Open a PE byte stream as a metadata reader. The caller disposes the `PEReader`; the
/// `MetadataReader` it yields is valid only for that lifetime.
let openPe (bytes: byte[]) : PEReader =
    new PEReader(System.Collections.Immutable.ImmutableArray.Create<byte>(bytes))

/// `Namespace.Name`, or `Name` alone for the empty namespace.
let private qualifiedName (ns: string) (name: string) : string =
    if String.IsNullOrEmpty ns then
        name
    else
        sprintf "%s.%s" ns name

/// The namespace-qualified name of a type-def, so a NESTED type appears as `Inner` rather
/// than `Outer+Inner`.
let private qualifiedTypeName (md: MetadataReader) (td: TypeDefinition) : string =
    qualifiedName (md.GetString td.Namespace) (md.GetString td.Name)

/// The base-type full name of the FIRST type-def whose simple name satisfies `nameMatches`,
/// resolving the handle through a `TypeReference` or a sibling `TypeDefinition`.
/// `System.ValueType` vs `System.Object` distinguishes a struct closure from a heap one.
let peTypeBaseTypeName (bytes: byte[]) (nameMatches: string -> bool) : string voption =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    md.TypeDefinitions
    |> Seq.tryPick (fun tdh ->
        let td = md.GetTypeDefinition tdh

        if nameMatches (md.GetString td.Name) then
            let bt = td.BaseType

            if bt.IsNil then
                Some ValueNone
            else
                match bt.Kind with
                | HandleKind.TypeReference ->
                    let r = md.GetTypeReference(TypeReferenceHandle.op_Explicit bt)
                    Some(ValueSome(qualifiedName (md.GetString r.Namespace) (md.GetString r.Name)))
                | HandleKind.TypeDefinition ->
                    let d = md.GetTypeDefinition(TypeDefinitionHandle.op_Explicit bt)
                    Some(ValueSome(qualifiedTypeName md d))
                | _ -> Some ValueNone
        else
            None
    )
    |> Option.defaultValue ValueNone

/// Every `<closure>$…` type-def in the PE, in type-def order.
let private closureTypeDefs (md: MetadataReader) : TypeDefinition seq =
    md.TypeDefinitions
    |> Seq.map md.GetTypeDefinition
    |> Seq.filter (fun td -> (md.GetString td.Name).StartsWith "<closure>$")

/// The base-type SIMPLE name (`ValueType` / `Object`) of EVERY `<closure>$…` type-def in
/// the PE, one entry per closure, so a test can assert all closures are value types.
/// Throws if a closure's base handle is not a `TypeReference`.
let peClosureBaseTypeNames (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for td in closureTypeDefs md do
            match td.BaseType.Kind with
            | HandleKind.TypeReference ->
                md.GetString (md.GetTypeReference(TypeReferenceHandle.op_Explicit td.BaseType)).Name
            | kind ->
                failwithf
                    "PeInspection: closure '%s' has a %A base handle; expected a TypeReference"
                    (md.GetString td.Name)
                    kind
    ]

/// The name of every `<closure>$…` type-def carrying a `.cctor`, in type-def order: the
/// closures given a cached singleton.
let peClosureCctors (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for td in closureTypeDefs md do
            let hasCctor =
                td.GetMethods()
                |> Seq.exists (fun mdh -> md.GetString (md.GetMethodDefinition mdh).Name = ".cctor")

            if hasCctor then
                md.GetString td.Name
    ]

/// Every method-def outside `<Module>` as `(declaringType, methodName)`, the declaring type
/// spelled `Namespace.Name`.
let peMethodNames (bytes: byte[]) : (string * string) list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for tdHandle in md.TypeDefinitions do
            let td = md.GetTypeDefinition tdHandle

            if md.GetString td.Name <> "<Module>" then
                let qualified = qualifiedTypeName md td

                for mdh in td.GetMethods() do
                    let m = md.GetMethodDefinition mdh
                    yield qualified, md.GetString m.Name
    ]

/// Every type-def in the PE by namespace-qualified name, `<Module>` excluded.
let peTypeDefNames (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for tdHandle in md.TypeDefinitions do
            let td = md.GetTypeDefinition tdHandle

            if md.GetString td.Name <> "<Module>" then
                yield qualifiedTypeName md td
    ]

/// Every TypeRef in the PE by namespace-qualified name: the external types the emitted
/// metadata binds to by name.
let peTypeRefNames (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for trHandle in md.TypeReferences do
            let tr = md.GetTypeReference trHandle
            yield qualifiedName (md.GetString tr.Namespace) (md.GetString tr.Name)
    ]

/// Every AssemblyRef name in the PE: the dependency surface the loader resolves.
let peAssemblyRefs (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for h in md.AssemblyReferences do
            let r = md.GetAssemblyReference h
            md.GetString r.Name
    ]

/// The `InterfaceImpl` rows of the type-def named `declaringType`: one per `: IFace` entry
/// actually emitted, so an inherited interface is excluded where `Type.GetInterfaces` would
/// fold it in. Throws if the type is not found.
let peInterfaceImplCount (bytes: byte[]) (declaringType: string) : int =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let found =
        md.TypeDefinitions
        |> Seq.map md.GetTypeDefinition
        |> Seq.tryFind (fun td -> qualifiedTypeName md td = declaringType)

    match found with
    | Some td -> td.GetInterfaceImplementations().Count
    | None -> failwithf "PeInspection: no type-def '%s'" declaringType

/// Every method on `declaringType` whose name satisfies `nameMatches`, in type-def then
/// method-def order.
let private methodDefsWhere
    (md: MetadataReader)
    (declaringType: string)
    (nameMatches: string -> bool)
    : MethodDefinition seq =
    seq {
        for tdh in md.TypeDefinitions do
            let td = md.GetTypeDefinition tdh

            if qualifiedTypeName md td = declaringType then
                for mdh in td.GetMethods() do
                    let m = md.GetMethodDefinition mdh

                    if nameMatches (md.GetString m.Name) then
                        yield m
    }

/// The FIRST method on `declaringType` whose name satisfies `nameMatches`; throws if none
/// matches.
let private methodDefWhere
    (md: MetadataReader)
    (declaringType: string)
    (nameMatches: string -> bool)
    : MethodDefinition =
    match Seq.tryHead (methodDefsWhere md declaringType nameMatches) with
    | Some m -> m
    | None -> failwithf "PeInspection: no matching method on type '%s'" declaringType

/// The method named `methodName` on `declaringType`; throws if not found.
let private methodDefNamed (md: MetadataReader) (declaringType: string) (methodName: string) : MethodDefinition =
    match Seq.tryHead (methodDefsWhere md declaringType ((=) methodName)) with
    | Some m -> m
    | None -> failwithf "PeInspection: no method '%s' on type '%s'" methodName declaringType

/// Raw IL bytes of a method's body; `[||]` for a body-less method.
let private methodIl (peReader: PEReader) (m: MethodDefinition) : byte[] =
    if m.RelativeVirtualAddress = 0 then
        [||]
    else
        let body = peReader.GetMethodBody m.RelativeVirtualAddress
        let ilReader = body.GetILReader()
        let buf = Array.zeroCreate ilReader.RemainingBytes
        ilReader.ReadBytes(ilReader.RemainingBytes, buf, 0)
        buf

/// Raw IL bytes of the FIRST method on `declaringType` whose name satisfies `nameMatches`.
/// `[||]` for a body-less method; throws if none matches.
let peMethodIlWhere (bytes: byte[]) (declaringType: string) (nameMatches: string -> bool) : byte[] =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()
    methodIl peReader (methodDefWhere md declaringType nameMatches)

/// `peMethodIlWhere` for EVERY matching method, in type-def then method-def order.
let peMethodsIlWhere (bytes: byte[]) (declaringType: string) (nameMatches: string -> bool) : byte[][] =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    methodDefsWhere md declaringType nameMatches
    |> Seq.map (methodIl peReader)
    |> Seq.toArray

/// Raw IL bytes of the method named `methodName` on `declaringType`, for asserting an
/// opcode sequence. `[||]` for an abstract method (no body); throws if not found.
let peMethodIl (bytes: byte[]) (declaringType: string) (methodName: string) : byte[] =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()
    methodIl peReader (methodDefNamed md declaringType methodName)

/// Every CIL opcode by its encoding: the single byte for a one-byte opcode, `0xFExx` for a
/// two-byte one.
let private opCodeTable: Lazy<Collections.Generic.Dictionary<int, Emit.OpCode>> =
    lazy
        (let table = Collections.Generic.Dictionary<int, Emit.OpCode>()

         for f in typeof<Emit.OpCodes>.GetFields(BindingFlags.Public ||| BindingFlags.Static) do
             match f.GetValue null with
             | :? Emit.OpCode as op -> table.[int (uint16 op.Value)] <- op
             | _ -> ()

         table)

/// One decoded instruction of an IL body: its opcode and the offset of its operand (the
/// offset of the next instruction for an operand-less opcode).
[<Struct>]
type IlInstruction = { Op: Emit.OpCode; OperandOffset: int }

/// The instructions of a raw IL body in order. Throws on an operand type the walk does not
/// size.
let ilInstructions (il: byte[]) : IlInstruction list =
    let table = opCodeTable.Value
    let ops = ResizeArray<IlInstruction>()
    let mutable i = 0

    while i < il.Length do
        let code, width =
            if il.[i] = 0xFEuy then
                (0xFE00 ||| int il.[i + 1]), 2
            else
                int il.[i], 1

        let op = table.[code]
        i <- i + width
        ops.Add { Op = op; OperandOffset = i }

        match op.OperandType with
        | Emit.OperandType.InlineNone -> ()
        | Emit.OperandType.ShortInlineBrTarget
        | Emit.OperandType.ShortInlineI
        | Emit.OperandType.ShortInlineVar -> i <- i + 1
        | Emit.OperandType.InlineVar -> i <- i + 2
        | Emit.OperandType.InlineI8
        | Emit.OperandType.InlineR -> i <- i + 8
        | Emit.OperandType.InlineSwitch ->
            let count = BitConverter.ToInt32(il, i)
            i <- i + 4 + 4 * count
        | Emit.OperandType.InlineField
        | Emit.OperandType.InlineMethod
        | Emit.OperandType.InlineTok
        | Emit.OperandType.InlineBrTarget
        | Emit.OperandType.InlineI
        | Emit.OperandType.InlineString
        | Emit.OperandType.InlineSig
        | Emit.OperandType.InlineType
        | Emit.OperandType.ShortInlineR -> i <- i + 4
        | other -> failwithf "ilInstructions: unhandled operand type %A" other

    List.ofSeq ops

/// The opcodes a test asserts on, by name.
module IlOp =
    let Call = Emit.OpCodes.Call
    let Newobj = Emit.OpCodes.Newobj
    let Ldsfld = Emit.OpCodes.Ldsfld
    let Stsfld = Emit.OpCodes.Stsfld
    let Box = Emit.OpCodes.Box
    let Constrained = Emit.OpCodes.Constrained

/// How many instructions of `il` carry opcode `op`.
let ilCountOp (op: Emit.OpCode) (il: byte[]) : int =
    ilInstructions il |> List.sumBy (fun i -> if i.Op = op then 1 else 0)

/// Whether any instruction of `il` carries opcode `op`.
let ilHasOp (op: Emit.OpCode) (il: byte[]) : bool =
    ilInstructions il |> List.exists (fun i -> i.Op = op)

/// Whether `il` dispatches through a `constrained.` prefix: a typar-receiver member call
/// addressing a struct with no box.
let ilHasConstrainedPrefix (il: byte[]) : bool = ilHasOp IlOp.Constrained il

/// Whether `il` contains a `box`.
let ilHasBox (il: byte[]) : bool = ilHasOp IlOp.Box il

/// The name of the member a metadata token designates: a `MethodDef`, `FieldDef` or
/// `MemberRef` row's own name, a `MethodSpec` through the method it instantiates.
let rec private memberNameOfToken (md: MetadataReader) (token: int) : string =
    let h = MetadataTokens.EntityHandle token
    let row = MetadataTokens.GetRowNumber h

    match h.Kind with
    | HandleKind.MethodDefinition ->
        md.GetString (md.GetMethodDefinition(MetadataTokens.MethodDefinitionHandle row)).Name
    | HandleKind.FieldDefinition -> md.GetString (md.GetFieldDefinition(MetadataTokens.FieldDefinitionHandle row)).Name
    | HandleKind.MemberReference -> md.GetString (md.GetMemberReference(MetadataTokens.MemberReferenceHandle row)).Name
    | HandleKind.MethodSpecification ->
        let spec = md.GetMethodSpecification(MetadataTokens.MethodSpecificationHandle row)
        memberNameOfToken md (MetadataTokens.GetToken spec.Method)
    | kind -> sprintf "<%A>" kind

/// The member-bearing instructions of `methodName` on `declaringType`, in body order, as
/// `(mnemonic, member name)`: every `call` / `callvirt` / `newobj` / `ldfld` / `stfld` /
/// `ldflda` / `ldsfld` / `stsfld` / `ldftn` / `ldtoken` with the name of its operand. Throws
/// if not found.
let peMethodMemberOps (bytes: byte[]) (declaringType: string) (methodName: string) : (string * string) list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()
    let il = methodIl peReader (methodDefNamed md declaringType methodName)

    [
        for i in ilInstructions il do
            match i.Op.OperandType with
            | Emit.OperandType.InlineField
            | Emit.OperandType.InlineMethod
            | Emit.OperandType.InlineTok ->
                yield i.Op.Name, memberNameOfToken md (BitConverter.ToInt32(il, i.OperandOffset))
            | _ -> ()
    ]

/// How many local slots the body of `methodName` on `declaringType` declares; `0` for a
/// body-less method. Throws if not found.
let peMethodLocalCount (bytes: byte[]) (declaringType: string) (methodName: string) : int =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()
    let m = methodDefNamed md declaringType methodName

    if m.RelativeVirtualAddress = 0 then
        0
    else
        let body = peReader.GetMethodBody m.RelativeVirtualAddress

        if body.LocalSignature.IsNil then
            0
        else
            // A `LocalVarSig` blob is the `LOCAL_SIG` calling convention followed by the
            // slot count, then one type per slot.
            let localSig = md.GetStandaloneSignature body.LocalSignature
            let mutable reader = md.GetBlobReader localSig.Signature
            reader.ReadSignatureHeader() |> ignore
            reader.ReadCompressedInteger()

/// The return type's `ELEMENT_TYPE_*` tag from a method's MethodDef signature blob. For a
/// nominal return type that is the encoder's value-vs-class decision: `0x11`
/// (ELEMENT_TYPE_VALUETYPE) vs `0x12` (ELEMENT_TYPE_CLASS). Throws if not found.
let peMethodReturnElementType (bytes: byte[]) (declaringType: string) (methodName: string) : byte =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()
    let m = methodDefNamed md declaringType methodName
    let mutable r = md.GetBlobReader m.Signature
    r.ReadSignatureHeader() |> ignore // calling convention (HASTHIS etc.)
    r.ReadCompressedInteger() |> ignore // parameter count
    r.ReadByte() // return type's ELEMENT_TYPE_* tag

/// Every static method on the anonymous "Program" class (where a binding that declares no
/// module lands), minus the synthesised entry point; `[||]` if there is no Program class.
/// Takes the loaded `Assembly` so a caller reflecting types out of the same PE holds ONE.
let programClassMethodsOf (asm: Assembly) : MethodInfo[] =
    match asm.GetType "Program" with
    | null -> [||]
    | program ->
        program.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
        |> Array.filter (fun m -> m.Name <> "Main")

/// `programClassMethodsOf` for a caller that reflects nothing else out of the PE.
let programClassMethods (bytes: byte[]) : MethodInfo[] =
    programClassMethodsOf (loadAssembly bytes)

/// Format a PE byte array as a hex string (`"02 00 01 …"`), capped so a test
/// failure message stays readable.
let formatIlBytes (bytes: byte[]) : string =
    bytes
    |> Array.truncate 64
    |> Array.map (sprintf "%02x")
    |> String.concat " "
    |> fun s ->
        if bytes.Length > 64 then
            s + sprintf " ... (%d bytes total)" bytes.Length
        else
            s
