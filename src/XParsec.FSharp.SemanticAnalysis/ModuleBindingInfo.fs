namespace XParsec.FSharp.SemanticAnalysis

/// The exportable identity of a module-level `let`, with the attributes it declares.
/// `InModule m` names the declaring module as its source writes it; the static class it
/// compiles to may carry the `…Module` suffix (`Vesper.Collections.ListModule::fold`).
type ModuleBindingInfo =
    {
        Container: ModuleContainer
        /// The short name the binding's source writes, which is what a use site writes.
        Name: string
        /// The name the binding emits under, `ValueNone` where that is `Name`.
        /// `[<CompiledName>]` is what makes the two differ.
        CompiledName: CompiledName voption
        /// The binding's attributes, resolved and constant-folded, in written order.
        Attributes: TAttributes
    }

    member this.BindingKey: BindingKey = SymbolKeyOps.bindingKeyOf this.Container this.Name

    member this.Key: SymbolKey = SymbolKey.Binding this.BindingKey

    /// The short name the binding emits under.
    member this.EmittedName: string = CompiledName.Emitted(this.CompiledName, this.Name)

    /// The named module this binding is declared in, or `ValueNone` for a top-level `let`.
    member this.DeclaringModule: ModuleKey voption =
        match this.Container with
        | ModuleContainer.InModule m -> ValueSome m
        | ModuleContainer.InNamespace _ -> ValueNone
