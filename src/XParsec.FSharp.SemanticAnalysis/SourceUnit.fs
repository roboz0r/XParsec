namespace XParsec.FSharp.SemanticAnalysis

/// One compilation unit: the implementation compiled into the assembly, and the signature file
/// that publishes it. Where there is no signature file the implementation publishes the surface
/// it infers. The parameters are the halves at whatever stage the unit has reached.
type SourceUnit<'signature, 'implementation> =
    {
        Signature: 'signature voption
        Implementation: 'implementation
    }
