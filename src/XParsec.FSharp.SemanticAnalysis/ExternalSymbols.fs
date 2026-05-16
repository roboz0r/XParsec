namespace XParsec.FSharp.SemanticAnalysis

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — see
// [[project_inline_il_target_specific]] for why we don't model it here.

/// Type-name lookup is a separate concern (handled by named-type resolution
/// in the type checker, not by this interface).
type ExternalSymbol =
    { Name: string
      /// For polymorphic symbols (real FSharp.Core (+), List.map, …) this
      /// will eventually be a scheme with bound type vars + SRTP / IWSAM
      /// constraints. Mono concrete TyFun for now.
      Type: SemType }

/// Each target supplies its own provider implementation.
type IExternalSymbolProvider =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption

module ExternalSymbols =

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone }

/// TODO: replace with FSharp.Core.dll-derived equivalents when .NET
/// integration comes online; the interface above stays the same.
module MockBuiltins =

    let tyInt: SemType = TyConst "int"
    let tyBool: SemType = TyConst "bool"

    let private tyBinOp (ty: SemType) : SemType = TyFun(ty, TyFun(ty, ty))

    let private builtins =
        [
            "op_Addition", tyBinOp tyInt
            "op_Subtraction", tyBinOp tyInt
            "op_Multiply", tyBinOp tyInt
            // `true` / `false` parse as identifiers, not constants.
            "true", tyBool
            "false", tyBool
        ]
        |> List.map (fun (n, t) -> n, { Name = n; Type = t })
        |> Map.ofList

    let provider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup(name) =
                match Map.tryFind name builtins with
                | Some s -> ValueSome s
                | None -> ValueNone }
