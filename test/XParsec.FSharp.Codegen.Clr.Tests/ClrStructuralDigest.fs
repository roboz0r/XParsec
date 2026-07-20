/// A DETERMINISTIC structural digest of an emitted CLR assembly, read back with
/// `MetadataReader`. It exists because the raw PE is NOT byte-deterministic:
/// `Metadata.fs:64` mints a fresh module MVID via `Guid.NewGuid()` on every compile
/// (and the PE debug/content-id is time/GUID-derived), so `hash(toBytes …)` changes
/// run-to-run for identical source. Hashing raw bytes would make a byte-identity gate
/// perpetually red.
///
/// So this folds ONLY source-derived, position-deterministic metadata — the assembly
/// identity, referenced type/member/assembly names + signatures, and for every
/// `TypeDef` its fields (name/flags/signature) and methods (name/flags/signature and
/// the IL method body: opcodes + operand tokens, max-stack, local-var signature). It
/// never reads the Module row or the `#GUID` heap, so the MVID (and EncId/EncBaseId)
/// are excluded by construction. Metadata tokens carried as IL operands are dense row
/// indices assigned by emission order, which IS deterministic — the only non-determinism
/// 0.1 must tolerate is the MVID, and that is the one thing left out.
///
/// Matches the standing preference to assert on emitted IL via `MetadataReader`
/// (rows/flags/order), not reflection.
module XParsec.FSharp.Codegen.Clr.Tests.ClrStructuralDigest

open System
open System.Security.Cryptography
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

let private feedInt (h: IncrementalHash) (n: int) = h.AppendData(BitConverter.GetBytes n)

/// Length-prefixed, so concatenation is unambiguous (two adjacent fields can never
/// alias a single longer one).
let private feedBytes (h: IncrementalHash) (b: byte[]) =
    feedInt h b.Length
    h.AppendData b

let private feedString (h: IncrementalHash) (s: string) =
    feedBytes h (Text.Encoding.UTF8.GetBytes s)

let private fold (h: IncrementalHash) (pe: PEReader) : unit =
    let md = pe.GetMetadataReader()

    // Assembly identity — deterministic (fixed name + version); never the Module MVID.
    if md.IsAssembly then
        let a = md.GetAssemblyDefinition()
        feedString h (md.GetString a.Name)
        feedString h (a.Version.ToString())

    // Referenced names + signatures (in row order = emission order).
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

    // The defining rows: every TypeDef with its fields + methods (+ method bodies), all
    // walked in table order — the observable shape of the assembly.
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

            // The IL body: raw opcodes + operand tokens (ldstr tokens fold string
            // content in via the `#US` heap), max-stack, and the local-var signature.
            // RVA 0 = abstract / extern, no body.
            let rva = mdef.RelativeVirtualAddress

            if rva <> 0 then
                let body = pe.GetMethodBody rva
                feedInt h body.MaxStack
                feedBytes h (body.GetILBytes())
                let ls = body.LocalSignature

                if not ls.IsNil then
                    feedBytes h (md.GetBlobBytes (md.GetStandaloneSignature ls).Signature)

/// The lowercase-hex SHA-256 of the structural fold over `bytes`.
let ofBytes (bytes: byte[]) : string =
    use pe = openPe bytes
    use h = IncrementalHash.CreateHash HashAlgorithmName.SHA256
    fold h pe
    (Convert.ToHexString(h.GetHashAndReset())).ToLowerInvariant()
