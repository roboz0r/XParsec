namespace XParsec.FSharp.SemanticAnalysis

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — see
// [[project_inline_il_target_specific]] for why we don't model it here.

/// Type-name lookup is a separate concern (handled by named-type resolution
/// in the type checker, not by this interface).
type ExternalSymbol =
    {
        Name: string
        /// For polymorphic symbols (real FSharp.Core (+), List.map, …) this
        /// will eventually be a scheme with bound type vars + SRTP / IWSAM
        /// constraints. Mono concrete TyFun for now.
        Type: SemType
    }

/// Each target supplies its own provider implementation.
type IExternalSymbolProvider =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption

module ExternalSymbols =

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone
        }

/// TODO: replace with FSharp.Core.dll-derived equivalents when .NET
/// integration comes online; the interface above stays the same.
module MockBuiltins =

    let tyInt: SemType = TyConst "int"
    let tyInt64: SemType = TyConst "int64"
    let tyByte: SemType = TyConst "byte"
    let tyFloat: SemType = TyConst "float"
    let tyBool: SemType = TyConst "bool"
    let tyUnit: SemType = TyConst "unit"
    let tyString: SemType = TyConst "string"

    let private tyBinOp (a: SemType) (b: SemType) (r: SemType) : SemType = TyFun(a, TyFun(b, r))

    let private tyUnaryOp (ty: SemType) : SemType = TyFun(ty, ty)

    let private builtins =
        let intInfix = tyBinOp tyInt tyInt tyInt
        let intCmp = tyBinOp tyInt tyInt tyBool
        let boolInfix = tyBinOp tyBool tyBool tyBool

        [
            "op_Addition", intInfix
            "op_Subtraction", intInfix
            "op_Multiply", intInfix
            "op_Division", intInfix
            "op_Modulus", intInfix
            "op_UnaryNegation", tyUnaryOp tyInt
            // Comparison ops are monomorphic int-only for the tiny subset.
            "op_LessThan", intCmp
            "op_GreaterThan", intCmp
            "op_LessThanOrEqual", intCmp
            "op_GreaterThanOrEqual", intCmp
            "op_Equality", intCmp
            "op_Inequality", intCmp
            "op_BooleanAnd", boolInfix
            "op_BooleanOr", boolInfix
        ]
        |> List.map (fun (n, t) -> n, { Name = n; Type = t })
        |> Map.ofList

    let provider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup(name) =
                match Map.tryFind name builtins with
                | Some s -> ValueSome s
                | None -> ValueNone
        }
