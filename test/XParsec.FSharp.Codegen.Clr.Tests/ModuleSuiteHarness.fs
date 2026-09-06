module XParsec.FSharp.Codegen.Clr.Tests.ModuleSuiteHarness

// Reflection over a built Vesper package: values come from a union's emitted static case
// factories, members are invoked by name, and results are read back as BCL types.
// Combinators taking a `Vesper.Fun` cannot be minted by reflection, so each suite runs
// those as driver programs.

open System
open System.Reflection
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// The `open` line every collection-module driver program starts with.
let prelude = "open Vesper.Collections\n"

/// The built assembly of `package` (cached). Every type reflected from one `Lazy<Assembly>`
/// shares an identity across `Invoke`s.
let packageAssembly (package: string) : Lazy<Assembly> = lazy (fst (buildPackage package).Value)

/// The generic type `typeName` from `asm`, closed over `typeArgs`.
let closedType (asm: Lazy<Assembly>) (typeName: string) (typeArgs: Type[]) : Lazy<Type> =
    lazy (asm.Value.GetType(typeName, true).MakeGenericType typeArgs)

/// Invoke the static member `name` of `ty` (a union's case factory) with `args`.
let caseFactory (ty: Lazy<Type>) (name: string) (args: obj[]) : obj =
    ty.Value.GetMethod(name).Invoke(null, args)

/// Invoke the nullary instance member `name` of `ty` (`get_IsSome`, `get_Tag`,
/// `Get_<Case>_<i>`) on `target`.
let instanceGet (ty: Lazy<Type>) (name: string) (target: obj) : obj =
    ty.Value.GetMethod(name).Invoke(target, [||])

/// Invoke the static `name` of the module class `moduleName` in `asm`, instantiated at
/// `typeArgs` when it is generic.
let callModule (asm: Lazy<Assembly>) (moduleName: string) (name: string) (typeArgs: Type[]) (args: obj[]) : obj =
    let m = asm.Value.GetType(moduleName, true).GetMethod(name)

    let m =
        if m.IsGenericMethodDefinition then
            m.MakeGenericMethod typeArgs
        else
            m

    m.Invoke(null, args)

let asBool (o: obj) : bool = o :?> bool
let asInt (o: obj) : int = o :?> int
