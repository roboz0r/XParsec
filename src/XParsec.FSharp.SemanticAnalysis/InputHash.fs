namespace XParsec.FSharp.SemanticAnalysis

open System.Text

/// The hash of some content; this type never computes it. Lowercase hex, so the value
/// doubles as a filesystem-safe path segment needing no escaping.
[<Struct>]
type InputHash =
    {
        Hex: string
    }

    override this.ToString() = this.Hex

module InputHash =

    let ofHex (hex: string) : InputHash = { Hex = hex.ToLowerInvariant() }

    let ofBytes (bytes: byte[]) : InputHash =
        let sb = StringBuilder(bytes.Length * 2)

        for b in bytes do
            sb.Append(b.ToString("x2")) |> ignore

        { Hex = sb.ToString() }
