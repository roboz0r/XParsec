module XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// Read the emitted PE through `System.Reflection.Metadata` (method/field tokens, the
// AssemblyRef table, raw IL). The `pe*` readers work straight off the bytes; `loadAssembly`
// and `programClassMethods*` reflect over the same bytes through the runtime loader.

open System
open System.Reflection
open System.Runtime.Loader
open System.Reflection.Metadata
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

/// The base-type full name of the FIRST type-def whose simple name satisfies `nameMatches`,
/// resolving the handle through a `TypeReference` or a sibling `TypeDefinition`.
/// `System.ValueType` vs `System.Object` distinguishes a struct closure from a heap one.
let peTypeBaseTypeName (bytes: byte[]) (nameMatches: string -> bool) : string voption =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let nameOf (ns: string) (n: string) =
        if System.String.IsNullOrEmpty ns then
            n
        else
            sprintf "%s.%s" ns n

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
                    Some(ValueSome(nameOf (md.GetString r.Namespace) (md.GetString r.Name)))
                | HandleKind.TypeDefinition ->
                    let d = md.GetTypeDefinition(TypeDefinitionHandle.op_Explicit bt)
                    Some(ValueSome(nameOf (md.GetString d.Namespace) (md.GetString d.Name)))
                | _ -> Some ValueNone
        else
            None
    )
    |> Option.defaultValue ValueNone

/// The base-type SIMPLE name (`ValueType` / `Object`) of EVERY `<closure>$…` type-def in
/// the PE, one entry per closure, so a test can assert all closures are value types.
/// `<none>` when the base handle is not a `TypeReference`.
let peClosureBaseTypeNames (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    md.TypeDefinitions
    |> Seq.choose (fun tdh ->
        let td = md.GetTypeDefinition tdh

        if (md.GetString td.Name).StartsWith "<closure>$" then
            match td.BaseType.Kind with
            | HandleKind.TypeReference ->
                Some(md.GetString (md.GetTypeReference(TypeReferenceHandle.op_Explicit td.BaseType)).Name)
            | _ -> Some "<none>"
        else
            None
    )
    |> Seq.toList

/// Every method-def outside `<Module>` as `(declaringType, methodName)`, the declaring type
/// spelled `Namespace.Name`, so a NESTED type appears as `Inner` rather than `Outer+Inner`.
let peMethodNames (bytes: byte[]) : (string * string) list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for tdHandle in md.TypeDefinitions do
            let td = md.GetTypeDefinition tdHandle
            let typeName = md.GetString td.Name

            if typeName <> "<Module>" then
                let ns = md.GetString td.Namespace

                let qualified =
                    if System.String.IsNullOrEmpty ns then
                        typeName
                    else
                        sprintf "%s.%s" ns typeName

                for mdh in td.GetMethods() do
                    let m = md.GetMethodDefinition mdh
                    yield qualified, md.GetString m.Name
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

/// Total `InterfaceImpl` rows across every type-def: one per `: IFace` entry actually emitted,
/// so an inherited interface is excluded where `Type.GetInterfaces` would fold it in.
let peInterfaceImplCount (bytes: byte[]) : int =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    md.TypeDefinitions
    |> Seq.sumBy (fun h -> (md.GetTypeDefinition h).GetInterfaceImplementations().Count)

/// Raw IL bytes of the FIRST method on `declaringType` whose name satisfies `nameMatches`.
/// `[||]` for a body-less method; throws if none matches.
let peMethodIlWhere (bytes: byte[]) (declaringType: string) (nameMatches: string -> bool) : byte[] =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let typeMatches (td: TypeDefinition) =
        let name = md.GetString td.Name
        let ns = md.GetString td.Namespace

        let qualified =
            if System.String.IsNullOrEmpty ns then
                name
            else
                sprintf "%s.%s" ns name

        qualified = declaringType

    let methodHandle =
        md.TypeDefinitions
        |> Seq.tryPick (fun tdh ->
            let td = md.GetTypeDefinition tdh

            if typeMatches td then
                td.GetMethods()
                |> Seq.tryFind (fun mdh -> nameMatches (md.GetString(md.GetMethodDefinition(mdh).Name)))
            else
                None
        )

    match methodHandle with
    | None -> failwithf "peMethodIlWhere: no matching method on type '%s'" declaringType
    | Some mdh ->
        let m = md.GetMethodDefinition mdh

        if m.RelativeVirtualAddress = 0 then
            [||]
        else
            let body = peReader.GetMethodBody m.RelativeVirtualAddress
            let ilReader = body.GetILReader()
            let buf = Array.zeroCreate ilReader.RemainingBytes
            ilReader.ReadBytes(ilReader.RemainingBytes, buf, 0)
            buf

/// `peMethodIlWhere` for EVERY matching method, in type-def then method-def order.
let peMethodsIlWhere (bytes: byte[]) (declaringType: string) (nameMatches: string -> bool) : byte[][] =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let typeMatches (td: TypeDefinition) =
        let name = md.GetString td.Name
        let ns = md.GetString td.Namespace

        let qualified =
            if System.String.IsNullOrEmpty ns then
                name
            else
                sprintf "%s.%s" ns name

        qualified = declaringType

    [|
        for tdh in md.TypeDefinitions do
            let td = md.GetTypeDefinition tdh

            if typeMatches td then
                for mdh in td.GetMethods() do
                    let m = md.GetMethodDefinition mdh

                    if nameMatches (md.GetString m.Name) then
                        if m.RelativeVirtualAddress = 0 then
                            yield [||]
                        else
                            let body = peReader.GetMethodBody m.RelativeVirtualAddress
                            let ilReader = body.GetILReader()
                            let buf = Array.zeroCreate ilReader.RemainingBytes
                            ilReader.ReadBytes(ilReader.RemainingBytes, buf, 0)
                            yield buf
    |]

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

/// Raw IL bytes of the method named `methodName` on `declaringType`, for asserting an
/// opcode sequence. `[||]` for an abstract method (no body); throws if not found.
let peMethodIl (bytes: byte[]) (declaringType: string) (methodName: string) : byte[] =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let typeMatches (td: TypeDefinition) =
        let name = md.GetString td.Name
        let ns = md.GetString td.Namespace

        let qualified =
            if System.String.IsNullOrEmpty ns then
                name
            else
                sprintf "%s.%s" ns name

        qualified = declaringType

    let methodHandle =
        md.TypeDefinitions
        |> Seq.tryPick (fun tdh ->
            let td = md.GetTypeDefinition tdh

            if typeMatches td then
                td.GetMethods()
                |> Seq.tryFind (fun mdh -> md.GetString(md.GetMethodDefinition(mdh).Name) = methodName)
            else
                None
        )

    match methodHandle with
    | None -> failwithf "peMethodIl: no method '%s' on type '%s'" methodName declaringType
    | Some mdh ->
        let m = md.GetMethodDefinition mdh

        if m.RelativeVirtualAddress = 0 then
            [||]
        else
            let body = peReader.GetMethodBody m.RelativeVirtualAddress
            let ilReader = body.GetILReader()
            let buf = Array.zeroCreate ilReader.RemainingBytes
            ilReader.ReadBytes(ilReader.RemainingBytes, buf, 0)
            buf

/// The return type's `ELEMENT_TYPE_*` tag from a method's MethodDef signature blob. For a
/// nominal return type that is the encoder's value-vs-class decision: `0x11`
/// (ELEMENT_TYPE_VALUETYPE) vs `0x12` (ELEMENT_TYPE_CLASS). Throws if not found.
let peMethodReturnElementType (bytes: byte[]) (declaringType: string) (methodName: string) : byte =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let qualifiedOf (td: TypeDefinition) =
        let name = md.GetString td.Name
        let ns = md.GetString td.Namespace

        if System.String.IsNullOrEmpty ns then
            name
        else
            sprintf "%s.%s" ns name

    let methodHandle =
        md.TypeDefinitions
        |> Seq.tryPick (fun tdh ->
            let td = md.GetTypeDefinition tdh

            if qualifiedOf td = declaringType then
                td.GetMethods()
                |> Seq.tryFind (fun mdh -> md.GetString(md.GetMethodDefinition(mdh).Name) = methodName)
            else
                None
        )

    match methodHandle with
    | None -> failwithf "peMethodReturnElementType: no method '%s' on type '%s'" methodName declaringType
    | Some mdh ->
        let mutable r = md.GetBlobReader((md.GetMethodDefinition mdh).Signature)
        r.ReadSignatureHeader() |> ignore // calling convention (HASTHIS etc.)
        r.ReadCompressedInteger() |> ignore // parameter count
        r.ReadByte() // return type's ELEMENT_TYPE_* tag

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
