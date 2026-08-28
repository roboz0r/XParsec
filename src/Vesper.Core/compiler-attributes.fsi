namespace Vesper

/// <summary>Specifies the application elements on which it is valid to apply an attribute.</summary>
type AttributeTargets =
    | Assembly = 1
    | Module = 2
    | Class = 4
    | Struct = 8
    | Enum = 16
    | Constructor = 32
    | Method = 64
    | Property = 128
    | Field = 256
    | Event = 512
    | Interface = 1024
    | Parameter = 2048
    | Delegate = 4096
    | ReturnValue = 8192
    | GenericParameter = 16384
    | All = 32767

/// <summary>Specifies the usage of another attribute class.</summary>
type AttributeUsageAttribute =
    inherit Attribute

    /// <summary>Initializes a new instance of the attribute with the specified list of
    /// <c>AttributeTargets</c>.</summary>
    new: validOn: AttributeTargets -> AttributeUsageAttribute

    /// <summary>Gets the set of values identifying which application elements the indicated
    /// attribute can be applied to.</summary>
    member ValidOn: AttributeTargets

    /// <summary>Gets or sets a value indicating whether more than one instance of the
    /// indicated attribute can be specified for a single application element.</summary>
    member AllowMultiple: bool with get, set

    /// <summary>Gets or sets a value indicating whether the indicated attribute is inherited
    /// by derived classes and overriding members.</summary>
    member Inherited: bool with get, set

// FSharp.Core/prim-types.fsi:99
/// <summary>Adding this attribute to class definition makes it sealed, which means it may not
/// be extended or implemented.</summary>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type SealedAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute.</summary>
    /// <returns>The created attribute.</returns>
    new: unit -> SealedAttribute

    /// <summary>Creates an instance of the attribute</summary>
    ///
    /// <param name="value">Indicates whether the class is sealed.</param>
    ///
    /// <returns>SealedAttribute</returns>
    new: value: bool -> SealedAttribute

    /// <summary>The value of the attribute, indicating whether the type is sealed or not.</summary>
    member Value: bool

/// <summary>Splices the argument unevaluated at its one use site — evaluated at most
/// once, on demand. This is how <c>&amp;&amp;</c> / <c>||</c> short-circuit without being
/// special-cased. Using it twice, or under a lambda or loop, fails compilation.</summary>
[<AttributeUsage(AttributeTargets.Parameter, AllowMultiple = false)>]
[<Sealed>]
type CallAtMostOnceAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> CallAtMostOnceAttribute

/// <summary>Adding this attribute to a record or union type confirms the automatic
/// generation of overrides for 'Equals' and 'GetHashCode' for the type.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type StructuralEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> StructuralEqualityAttribute

/// <summary>Adding this attribute to a record, union or struct type confirms the automatic
/// generation of an 'IComparable' implementation for the type.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type StructuralComparisonAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> StructuralComparisonAttribute

/// <summary>Adding this attribute to a type disables the automatic generation of
/// equality / hashing overrides; the type uses reference equality.</summary>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
[<Sealed>]
type ReferenceEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> ReferenceEqualityAttribute

/// <summary>Adding this attribute to a type indicates equality is an abnormal
/// operation: the type does not satisfy the 'equality' constraint, so generic
/// equality cannot be instantiated at it.</summary>
[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Interface
                 ||| AttributeTargets.Delegate
                 ||| AttributeTargets.Struct
                 ||| AttributeTargets.Enum,
                 AllowMultiple = false)>]
[<Sealed>]
type NoEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> NoEqualityAttribute

/// <summary>Adding this attribute to a type indicates it has a user-defined
/// implementation of equality.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type CustomEqualityAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> CustomEqualityAttribute

/// <summary>Adding this attribute to a type indicates it has a user-defined
/// implementation of comparison.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type CustomComparisonAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> CustomComparisonAttribute

/// <summary>Adding this attribute to a type indicates comparison is an abnormal
/// operation: the type does not satisfy the 'comparison' constraint, so generic
/// comparison cannot be instantiated at it.</summary>
[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Interface
                 ||| AttributeTargets.Delegate
                 ||| AttributeTargets.Struct
                 ||| AttributeTargets.Enum,
                 AllowMultiple = false)>]
[<Sealed>]
type NoComparisonAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> NoComparisonAttribute

/// <summary>Adding this attribute to a class or interface states that <c>null</c> inhabits
/// the type: it satisfies <c>when 'T : null</c>, and a <c>null</c> literal may be written at
/// it. Every other type reaches <c>null</c> through the union <c>T | null</c> instead.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Interface, AllowMultiple = false)>]
[<Sealed>]
type AllowNullLiteralAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> AllowNullLiteralAttribute

/// <summary>Declares that a module-level value IS a global of the target runtime (JS
/// <c>undefined</c>): no definition is emitted for it, and a reference emits the bare name
/// from any file with no import. The body must be one zero-operand template.</summary>
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
[<Sealed>]
type GlobalAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> GlobalAttribute

/// <summary>Declares that a module-level value's implementation is the export
/// <c>selector</c> of the committed runtime asset <c>path</c> of the declaring package.
/// <c>path</c> is <c>"./"</c> plus a manifest <c>[core] runtime</c> entry; <c>selector</c>
/// must equal the binding's emitted name; the body must be <c>jsNative</c>.</summary>
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field,
                 AllowMultiple = false)>]
[<Sealed>]
type ImportAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: selector: string * path: string -> ImportAttribute

// FSharp.Core/prim-types.fsi:205
/// <summary>Adding this attribute to a type causes it to be represented using a CLI struct.</summary>
[<AttributeUsage(AttributeTargets.Class
                 ||| AttributeTargets.Struct
                 ||| AttributeTargets.ReturnValue
                 ||| AttributeTargets.Parameter,
                 AllowMultiple = false)>]
[<Sealed>]
type StructAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    /// <returns>StructAttribute</returns>
    new: unit -> StructAttribute

// FSharp.Core/prim-types.fsi:904
/// <summary>This attribute is used to indicate that references to the elements of a module, record or union
/// type require explicit qualified access.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct ||| AttributeTargets.Enum, AllowMultiple = false)>]
[<Sealed>]
type RequireQualifiedAccessAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    /// <returns>RequireQualifiedAccessAttribute</returns>
    new: unit -> RequireQualifiedAccessAttribute

// FSharp.Core/prim-types.fsi:926
/// <summary>Indicates a construct is automatically opened when brought into scope through
/// an assembly reference or then opening of the containing namespace or module.</summary>
///
/// <remarks>When applied to an assembly, this attribute must be given a string
/// argument, and this indicates a valid module or namespace in that assembly. Source
/// code files compiled with a reference to this assembly are processed in an environment
/// where the given path is automatically opened.
///
/// When applied to a type or module within an assembly, then the attribute must not be given any arguments, and
/// the type or module is implicitly opened when its enclosing namespace or module is opened.
/// </remarks>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct ||| AttributeTargets.Assembly, AllowMultiple = true)>]
[<Sealed>]
type AutoOpenAttribute =
    inherit Attribute

    /// <summary>Creates an attribute used to mark a module as 'automatically opened' when the enclosing namespace is opened</summary>
    /// <returns>AutoOpenAttribute</returns>
    new: unit -> AutoOpenAttribute

    /// <summary>Creates an attribute used to mark a namespace or module path to be 'automatically opened' when an assembly is referenced</summary>
    ///
    /// <param name="path">The namespace or module to be automatically opened when an assembly is referenced
    /// or an enclosing module opened.</param>
    ///
    /// <returns>AutoOpenAttribute</returns>
    new: path: string -> AutoOpenAttribute

    /// <summary>Indicates the namespace or module to be automatically opened when an assembly is referenced
    /// or an enclosing module opened.</summary>
    member Path: string

// FSharp.Core/prim-types.fsi:591
/// <summary>Adding this attribute to a value or function definition in an F# module changes the name used
/// for the value in compiled CLI code.</summary>
[<AttributeUsage(AttributeTargets.Method
                 ||| AttributeTargets.Class
                 ||| AttributeTargets.Field
                 ||| AttributeTargets.Interface
                 ||| AttributeTargets.Struct
                 ||| AttributeTargets.Delegate
                 ||| AttributeTargets.Enum
                 ||| AttributeTargets.Property,
                 AllowMultiple = false)>]
[<Sealed>]
type CompiledNameAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    ///
    /// <param name="compiledName">The name to use in compiled code.</param>
    ///
    /// <returns>CompiledNameAttribute</returns>
    new: compiledName: string -> CompiledNameAttribute

    /// <summary>The name of the value as it appears in compiled code</summary>
    member CompiledName: string

// FSharp.Core/prim-types.fsi:74
/// <summary>Indicates one or more adjustments to the compiled representation of an F# type or member.</summary>
type CompilationRepresentationFlags =

    /// <summary>No special compilation representation.</summary>
    | None = 0

    /// <summary>Compile an instance member as 'static' .</summary>
    | Static = 1

    /// <summary>Compile a member as 'instance' even if <c>null</c> is used as a representation for this type.</summary>
    | Instance = 2

    /// <summary>append 'Module' to the end of a module whose name clashes with a type name in the same namespace.</summary>
    | ModuleSuffix = 4

    /// <summary>Permit the use of <c>null</c> as a representation for nullary discriminators in a discriminated union.</summary>
    | UseNullAsTrueValue = 8

    /// <summary>Compile a property as a CLI event.</summary>
    | Event = 16

// FSharp.Core/prim-types.fsi:744
/// <summary>This attribute is used to adjust the runtime representation for a type.
/// For example, it may be used to note that the <c>null</c> representation
/// may be used for a type. This affects how some constructs are compiled.
/// </summary>
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
[<Sealed>]
type CompilationRepresentationAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    ///
    /// <param name="flags">Indicates adjustments to the compiled representation of the type or member.</param>
    ///
    /// <returns>CompilationRepresentationAttribute</returns>
    new: flags: CompilationRepresentationFlags -> CompilationRepresentationAttribute

    /// <summary>Indicates one or more adjustments to the compiled representation of an F# type or member</summary>
    member Flags: CompilationRepresentationFlags

// FSharp.Core/prim-types.fsi:288
/// <summary>Adding this attribute to a value causes it to be compiled as a CLI constant literal.</summary>
[<AttributeUsage(AttributeTargets.Field, AllowMultiple = false)>]
[<Sealed>]
type LiteralAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    /// <returns>LiteralAttribute</returns>
    new: unit -> LiteralAttribute

// FSharp.Core/prim-types.fsi:218
/// <summary>Adding this attribute to a type causes it to be interpreted as a unit of measure.
/// This may only be used under very limited conditions.</summary>
[<AttributeUsage(AttributeTargets.GenericParameter ||| AttributeTargets.Class, AllowMultiple = false)>]
[<Sealed>]
type MeasureAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    /// <returns>MeasureAttribute</returns>
    new: unit -> MeasureAttribute

// FSharp.Core/prim-types.fsi:846
/// <summary>Indicates that a message should be emitted when F# source code uses this construct.</summary>
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
[<Sealed>]
type CompilerMessageAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute.</summary>
    new: message: string * messageNumber: int -> CompilerMessageAttribute

    /// <summary>Indicates the warning message to be emitted when F# source code uses this construct</summary>
    member Message: string

    /// <summary>Indicates the number associated with the message.</summary>
    member MessageNumber: int

    /// <summary>Indicates if the message should indicate a compiler error. Error numbers less than
    /// 10000 are considered reserved for use by the F# compiler and libraries.</summary>
    member IsError: bool with get, set

    /// <summary>Indicates if the construct should always be hidden in an editing environment.</summary>
    member IsHidden: bool with get, set

// FSharp.Core/prim-types.fsi:170
/// <summary>This attribute is used to indicate a generic container type satisfies the F# 'equality'
/// constraint only if a generic argument also satisfies this constraint.</summary>
///
/// <remarks> For example, adding
/// this attribute to parameter 'T on a type definition C&lt;'T&gt; means that a type C&lt;X&gt; only supports
/// equality if the type X also supports equality and all other conditions for C&lt;X&gt; to support
/// equality are also met. The type C&lt;'T&gt; can still be used with other type arguments, but a type such
/// as C&lt;(int -> int)&gt; will not support equality because the type (int -> int) is an F# function type
/// and does not support equality.
///
/// This attribute will be ignored if it is used on the generic parameters of functions or methods.
/// </remarks>
[<AttributeUsage(AttributeTargets.GenericParameter, AllowMultiple = false)>]
[<Sealed>]
type EqualityConditionalOnAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    /// <returns>EqualityConditionalOnAttribute</returns>
    new: unit -> EqualityConditionalOnAttribute

// FSharp.Core/prim-types.fsi:578
/// <summary>Adding this attribute to a non-function value with generic parameters indicates that
/// uses of the construct can give rise to generic code through type inference. </summary>
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
[<Sealed>]
type GeneralizableValueAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    /// <returns>GeneralizableValueAttribute</returns>
    new: unit -> GeneralizableValueAttribute

// FSharp.Core/prim-types.fsi:770
/// <summary>This attribute is used to tag values that are part of an experimental library
/// feature.</summary>
[<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
[<Sealed>]
type ExperimentalAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    ///
    /// <param name="message">The warning message to be emitted when code uses this construct.</param>
    ///
    /// <returns>ExperimentalAttribute</returns>
    new: message: string -> ExperimentalAttribute

    /// <summary>Indicates the warning message to be emitted when F# source code uses this construct</summary>
    member Message: string

// FSharp.Core/prim-types.fsi:329
/// <summary>Adding this attribute to a discriminated union with value false
/// turns off the generation of standard helper member tester, constructor
/// and accessor members for the generated CLI class for that type.</summary>
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
[<Sealed>]
type DefaultAugmentationAttribute =
    inherit Attribute

    /// <summary>The value of the attribute, indicating whether the type has a default augmentation or not</summary>
    member Value: bool

    /// <summary>Creates an instance of the attribute</summary>
    ///
    /// <param name="value">Indicates whether to generate helper members on the CLI class representing a discriminated
    /// union.</param>
    ///
    /// <returns>DefaultAugmentationAttribute</returns>
    new: value: bool -> DefaultAugmentationAttribute
