/// SHA-256 over source-derived metadata only: assembly identity, referenced names +
/// signatures, each `TypeDef`'s fields/methods/properties/IL bodies. The Module row is
/// excluded: its MVID is a fresh `Guid.NewGuid()` per compile, so raw PE bytes never match.
module XParsec.FSharp.Codegen.Clr.Tests.ClrStructuralDigest

open System
open System.Security.Cryptography
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

let private feedInt (h: IncrementalHash) (n: int) = h.AppendData(BitConverter.GetBytes n)

/// Length-prefixed, so `"ab" + "c"` cannot hash the same as `"a" + "bc"`.
let private feedBytes (h: IncrementalHash) (b: byte[]) =
    feedInt h b.Length
    h.AppendData b

let private feedString (h: IncrementalHash) (s: string) =
    feedBytes h (Text.Encoding.UTF8.GetBytes s)

let private fold (h: IncrementalHash) (pe: PEReader) : unit =
    let md = pe.GetMetadataReader()

    if md.IsAssembly then
        let a = md.GetAssemblyDefinition()
        feedString h (md.GetString a.Name)
        feedString h (a.Version.ToString())

    // Every table is walked in row order, which is emission order.
    for th in md.TypeReferences do
        let tr = md.GetTypeReference th
        feedString h (md.GetString tr.Namespace)
        feedString h (md.GetString tr.Name)

    for mh in md.MemberReferences do
        let mr = md.GetMemberReference mh
        feedString h (md.GetString mr.Name)
        feedBytes h (md.GetBlobBytes mr.Signature)

    for ah in md.AssemblyReferences do
        let ar = md.GetAssemblyReference ah
        feedString h (md.GetString ar.Name)
        feedString h (ar.Version.ToString())

    for tdh in md.TypeDefinitions do
        let td = md.GetTypeDefinition tdh
        feedString h (md.GetString td.Namespace)
        feedString h (md.GetString td.Name)
        feedInt h (int td.Attributes)

        for fh in td.GetFields() do
            let fd = md.GetFieldDefinition fh
            feedString h (md.GetString fd.Name)
            feedInt h (int fd.Attributes)
            feedBytes h (md.GetBlobBytes fd.Signature)

        for mh in td.GetMethods() do
            let mdef = md.GetMethodDefinition mh
            feedString h (md.GetString mdef.Name)
            feedInt h (int mdef.Attributes)
            feedInt h (int mdef.ImplAttributes)
            feedBytes h (md.GetBlobBytes mdef.Signature)

            // `ldstr` operand tokens fold string content in via the `#US` heap.
            // RVA 0 = abstract / extern, so there is no body.
            let rva = mdef.RelativeVirtualAddress

            if rva <> 0 then
                let body = pe.GetMethodBody rva
                feedInt h body.MaxStack
                feedBytes h (body.GetILBytes())
                let ls = body.LocalSignature

                if not ls.IsNil then
                    feedBytes h (md.GetBlobBytes (md.GetStandaloneSignature ls).Signature)

        // Each accessor is folded by NAME, so a `MethodSemantics` row rebound to a
        // different method moves the digest.
        for ph in td.GetProperties() do
            let pd = md.GetPropertyDefinition ph
            feedString h (md.GetString pd.Name)
            feedInt h (int pd.Attributes)
            feedBytes h (md.GetBlobBytes pd.Signature)

            let accessors = pd.GetAccessors()

            for a in [ accessors.Getter; accessors.Setter ] do
                feedString
                    h
                    (if a.IsNil then
                         ""
                     else
                         md.GetString((md.GetMethodDefinition a).Name))

/// The lowercase-hex SHA-256 of the structural fold over `bytes`.
let ofBytes (bytes: byte[]) : string =
    use pe = openPe bytes
    use h = IncrementalHash.CreateHash HashAlgorithmName.SHA256
    fold h pe
    (Convert.ToHexString(h.GetHashAndReset())).ToLowerInvariant()
