module XParsec.FSharp.Codegen.Clr.Tests.ReflectionHarness

// Reflection over an emitted assembly, plus the source-fixture helpers the
// data-type suites share.

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

/// Source lines joined by `\n`, with no trailing newline.
let lines (xs: string list) : string = String.concat "\n" xs

/// The error-severity diagnostics of an analysed file.
let errors (tast: TastFile) : Diagnostic list = tast.Diagnostics |> Diagnostic.errors

/// Public instance members declared on the type itself, inherited members excluded.
let declaredInstance: BindingFlags =
    BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

let publicStatic: BindingFlags = BindingFlags.Public ||| BindingFlags.Static

/// The type's own `Equals(object)` override, or null.
let equalsObj (ty: Type) : MethodInfo =
    ty.GetMethod("Equals", declaredInstance, null, [| typeof<obj> |], null)

/// The type's own `Equals(Self)`, or null.
let typedEquals (ty: Type) : MethodInfo =
    ty.GetMethod("Equals", declaredInstance, null, [| ty |], null)

/// The type's own `GetHashCode()` override, or null.
let getHash (ty: Type) : MethodInfo =
    ty.GetMethod("GetHashCode", declaredInstance, null, [||], null)

/// The type's own `CompareTo(object)`, or null.
let compareToObj (ty: Type) : MethodInfo =
    ty.GetMethod("CompareTo", declaredInstance, null, [| typeof<obj> |], null)

/// The type's own `CompareTo(Self)`, or null.
let typedCompareTo (ty: Type) : MethodInfo =
    ty.GetMethod("CompareTo", declaredInstance, null, [| ty |], null)

let implementsIEquatable (ty: Type) : bool =
    (typedefof<IEquatable<_>>.MakeGenericType ty).IsAssignableFrom ty

/// Whether `ty` implements the generic `IComparable<Self>`.
let implementsIComparable (ty: Type) : bool =
    (typedefof<IComparable<_>>.MakeGenericType ty).IsAssignableFrom ty

/// The public static factory `name` on `ty`, which is how a union case constructs.
let factory (ty: Type) (name: string) : MethodInfo = ty.GetMethod(name, publicStatic)

/// The simple names of every interface `ty` implements, `IEquatable`1` for a generic one.
let interfaceNames (ty: Type) : Set<string> =
    ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray

/// Asserts that `ty` implements an interface with the simple name `ifaceName`.
let expectInterface (ty: Type) (ifaceName: string) : unit =
    let names = interfaceNames ty

    Expect.isTrue
        (names.Contains ifaceName)
        (sprintf "%s reflects as implementing %s (interfaces: %A)" ty.Name ifaceName (Set.toList names))

/// `type IRank` with one `Rank : unit -> int` slot, for a record or union to implement.
let iRankDecl: string =
    lines [ "type IRank ="; "    abstract member Rank : unit -> int" ]

/// The static method `name` on the emitted `Program` class. Fails naming the methods
/// that were emitted when `name` is absent.
let programFunction (name: string) (bytes: byte[]) : MethodInfo =
    let methods = programClassMethods bytes

    match methods |> Array.tryFind (fun m -> m.Name = name) with
    | Some m -> m
    | None ->
        failtestf
            "no top-level function `%s` on the Program class; emitted: %A"
            name
            (methods |> Array.map (fun m -> m.Name))

/// Compiles `source` under `assemblyName` and invokes its top-level `int -> int`
/// function `fnName` with `arg`.
let invokeIntFn (assemblyName: string) (fnName: string) (source: string) (arg: int) : int =
    let fn =
        programFunction fnName (Codegen.toBytes (compileSource assemblyName source))

    fn.Invoke(null, [| box arg |]) :?> int

/// A compile-once fixture: the artifact of `src` under `assemblyName`, and its loaded
/// type `typeName`, each produced on first use and shared by every test that reads it.
type SharedType =
    {
        Artifact: Lazy<ClrArtifact>
        Type: Lazy<Type>
    }

let sharedType (assemblyName: string) (typeName: string) (src: string) : SharedType =
    let artifact = lazy (compileSource assemblyName src)

    {
        Artifact = artifact
        Type =
            lazy
                (let ty = (loadAssembly (Codegen.toBytes artifact.Value)).GetType typeName
                 Expect.isNotNull ty (sprintf "the assembly %s contains the type %s" assemblyName typeName)
                 ty)
    }
