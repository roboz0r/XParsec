namespace XParsec.FSharp.SemanticAnalysis

/// The `SymbolKey` ↔ compiled-name algebra. A key holds arity as an INT; the `` `N ``
/// suffix and `+` nesting are CLR NAME-axis spellings, confined to this module. Parsing
/// mints only `InNamespace` / `InType`, because no compiled name says "module".
[<RequireQualifiedAccess>]
module SymbolKeyOps =

    // --- The CLR metadata-name string rules ------------------------------------------

    /// Strip a trailing `` `N `` arity suffix from a COMPILED name (`` List`1 `` ⇒ `List`).
    /// Name axis only: a `TypeKey`'s `Name` never carries a suffix to strip.
    let bareName (name: string) : string =
        let tick = name.IndexOf '`'
        if tick < 0 then name else name.Substring(0, tick)

    /// An F#-BACKTICK-ESCAPED identifier (`` ``[]`` ``), which can carry no `` `N `` suffix
    /// and so is held at `TyparArity = 0`.
    let private isEscapedName (name: string) = name.Contains '`'

    /// Render `(name, arity)` as the CLR metadata spelling (`List` + 1 ⇒ `` List`1 ``);
    /// unchanged at arity ≤ 0 or for an escaped name. Never build a key's `Name` with it.
    let arityName (name: string) (arity: int) : string =
        if arity > 0 && not (isEscapedName name) then
            sprintf "%s`%d" name arity
        else
            name

    /// The inverse of `arityName` on ONE metadata name segment: a trailing `` `N `` splits
    /// into `(bare name, N)`, otherwise `(segment, 0)`.
    let private parseArity (segment: string) : struct (string * int) =
        let tick = segment.LastIndexOf '`'

        if tick <= 0 || tick = segment.Length - 1 || segment.[tick - 1] = '`' then
            struct (segment, 0)
        else
            let mutable n = 0
            let mutable ok = true

            for i in tick + 1 .. segment.Length - 1 do
                let c = segment.[i]

                if c >= '0' && c <= '9' then
                    n <- n * 10 + int c - int '0'
                else
                    ok <- false

            if ok then
                struct (segment.Substring(0, tick), n)
            else
                struct (segment, 0)

    /// Split a fully-qualified compiled name at its LAST `.`: `Vesper.Option` ⇒
    /// `("Vesper", "Option")`; no `.` ⇒ `("", name)`.
    let private splitLastDot (compiled: string) : string * string =
        let i = compiled.LastIndexOf '.'

        if i < 0 then
            "", compiled
        else
            compiled.Substring(0, i), compiled.Substring(i + 1)

    /// The last `.`-separated segment of a compiled name, arity suffix stripped
    /// (`` Vesper.Choice`2 `` ⇒ `Choice`). A name with no `.` comes back bare.
    let shortName (compiled: string) : string = bareName (snd (splitLastDot compiled))

    // --- Namespaces ------------------------------------------------------------------

    /// Segment a dotted namespace string; `""` ⇒ the EMPTY path, the global namespace.
    let nsPath (dotted: string) : EqArray<string> =
        if System.String.IsNullOrEmpty dotted then
            EqArray.empty
        else
            EqArray.ofArray (dotted.Split '.')

    let namespaceKey (dottedNs: string) : NamespaceKey = { Path = nsPath dottedNs }

    // --- `TypeKey` ↔ metadata-name renderer / parser ----------------------------------

    /// The CLR metadata spelling of ONE segment of a type key: its own name plus its own
    /// `` `N `` (`` List`1 ``), the name a `TypeDef` / `TypeRef` row carries.
    let typeSegmentName (t: TypeKey) : string = arityName t.Name t.TyparArity

    /// The parse half of `typeSegmentName`, for a producer meeting the name segment by segment.
    let typeKeyOfSegment (container: TypeContainer) (metaName: string) : TypeKey =
        let struct (bare, arity) = parseArity metaName

        {
            Container = container
            Name = bare
            TyparArity = arity
        }

    /// The `+`-joined chain of a module's COMPILED CLASS names, WITHOUT the namespace
    /// (`A+B` for `module A` ⊃ `module B`), because a module compiles to a static class.
    let rec private moduleNestedName (m: ModuleKey) : string =
        match m.Container with
        | ModuleContainer.InNamespace _ -> m.Name
        | ModuleContainer.InModule parent -> moduleNestedName parent + "+" + m.Name

    /// The `+`-joined nested chain WITHOUT the namespace (`` List`1+Enumerator ``). EACH
    /// segment renders its OWN arity, so a generic nested in a generic spells both
    /// (`` Outer`1+Inner`1 ``); a module-held type renders `+` too (`N.MModule+T`).
    let rec typeNestedName (t: TypeKey) : string =
        let self = typeSegmentName t

        match t.Container with
        | TypeContainer.InNamespace _ -> self
        | TypeContainer.InModule m -> moduleNestedName m + "+" + self
        | TypeContainer.InType outer -> typeNestedName outer + "+" + self

    let typeNs (t: TypeKey) : string = t.Namespace.Dotted

    /// The full metadata/reflection name of a type (`` Ns.Outer`2+Inner `` for a nested
    /// one): the string handed to `asm.GetType`, and what provider stores are keyed by.
    let typeMetaName (t: TypeKey) : string =
        let ns = typeNs t
        let simple = typeNestedName t
        if ns = "" then simple else ns + "." + simple

    /// `name` may carry a `+`-mangled nested chain AND `` `N `` suffixes. Each segment's
    /// suffix PARSES into that segment's `TyparArity`, round-tripping via `typeMetaName`.
    let typeKeyOf (dottedNs: string) (name: string) : TypeKey =
        let ns = TypeContainer.InNamespace(namespaceKey dottedNs)

        if name.IndexOf '+' < 0 then
            typeKeyOfSegment ns name
        else
            let parts = name.Split '+'
            let mutable k = typeKeyOfSegment ns parts.[0]

            for i in 1 .. parts.Length - 1 do
                k <- typeKeyOfSegment (TypeContainer.InType k) parts.[i]

            k

    /// Resolve a WRITTEN dotted type name against an `exact` index keyed by `typeMetaName`,
    /// reaching the one spelling that is not that rendering: `Test.A.M.T`, keyed `Test.A.M+T`.
    let tryDottedInModule
        (exact: string -> 'T voption)
        (moduleContainer: string -> TypeContainer voption)
        (probe: string)
        : 'T voption =
        match exact probe with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            let dot = probe.LastIndexOf '.'

            if dot <= 0 || dot = probe.Length - 1 then
                ValueNone
            else
                match moduleContainer (probe.Substring(0, dot)) with
                | ValueSome container -> exact (typeMetaName (typeKeyOfSegment container (probe.Substring(dot + 1))))
                | ValueNone -> ValueNone

    /// A BARE source name plus its arity as an INT. An ESCAPED name is forced to arity 0, so
    /// `type ``[]``<'T>` keys equal to the `` ``[]`` `` a name-axis producer meets.
    let typeKeyOfContainer (container: TypeContainer) (name: string) (arity: int) : TypeKey =
        {
            Container = container
            Name = name
            TyparArity = if isEscapedName name then 0 else arity
        }

    /// `typeKeyOfContainer` for a type declared directly in a namespace.
    let typeKeyOfArity (dottedNs: string) (name: string) (arity: int) : TypeKey =
        typeKeyOfContainer (TypeContainer.InNamespace(namespaceKey dottedNs)) name arity

    let rec private spelledArity (t: TypeKey) : bool =
        t.TyparArity > 0
        || isEscapedName t.Name
        || (
            match t.Container with
            | TypeContainer.InType outer -> spelledArity outer
            | _ -> false
        )

    /// Supply an arity the compiled NAME did not spell, to the INNERMOST segment. Declines
    /// when any segment spelled one: in `` List`1+Enumerator `` the typar belongs to `List`.
    let private withArity (arity: int) (t: TypeKey) : TypeKey =
        if arity > 0 && not (spelledArity t) then
            { t with TyparArity = arity }
        else
            t

    // --- Modules ---------------------------------------------------------------------

    /// The full dotted name of a module (`Vesper.Collections`): namespace path plus the
    /// module chain, and the name of the CLR type it compiles to.
    let rec moduleFullName (m: ModuleKey) : string =
        match m.Container with
        | ModuleContainer.InNamespace ns ->
            let d = ns.Dotted
            if d = "" then m.Name else d + "." + m.Name
        | ModuleContainer.InModule parent -> moduleFullName parent + "." + m.Name

    /// Containment is a `ModuleContainer` chain, never a dotted string: only the producer knows which
    /// segments are namespace and which are module.
    let moduleKeyOf (container: ModuleContainer) (name: string) : ModuleKey = { Container = container; Name = name }

    let inNamespace (dottedNs: string) : ModuleContainer =
        ModuleContainer.InNamespace(namespaceKey dottedNs)

    /// `namespace Vesper` + `module Collections`, named SEPARATELY, so no dotted string to cut.
    let moduleInNamespace (dottedNs: string) (name: string) : ModuleKey = moduleKeyOf (inNamespace dottedNs) name

    // --- Smart constructors -----------------------------------------------------------

    let typeKey (ns: string) (name: string) : SymbolKey = SymbolKey.Type(typeKeyOf ns name)

    /// `SymbolKey.Type` from `(dotted ns, BARE name, arity)`, so no suffix is parsed.
    let typeKeyArity (ns: string) (name: string) (arity: int) : SymbolKey =
        SymbolKey.Type(typeKeyOfArity ns name arity)

    let containerFullName (h: ModuleContainer) : string =
        match h with
        | ModuleContainer.InNamespace ns -> ns.Dotted
        | ModuleContainer.InModule m -> moduleFullName m

    let bindingKeyOf (decl: ModuleContainer) (name: string) : BindingKey = { Decl = decl; Name = name }

    let valueKey (decl: ModuleContainer) (name: string) : SymbolKey =
        SymbolKey.Binding(bindingKeyOf decl name)

    /// `(dotted ns, module, name)` named separately, for a value in a namespace-level module.
    let moduleValueKey (dottedNs: string) (declModule: string) (name: string) : SymbolKey =
        valueKey (ModuleContainer.InModule(moduleInNamespace dottedNs declModule)) name

    let memberKeyOf
        (decl: TypeKey)
        (name: string)
        (argSig: EqArray<FrozenType>)
        (methodTyparArity: int)
        (kind: MemberKind)
        : MemberKey =
        {
            Decl = decl
            Name = name
            ArgSig = argSig
            MethodTyparArity = methodTyparArity
            Kind = kind
        }

    /// `memberKeyOf` widened to `SymbolKey`, for the IR positions that carry the wide key.
    let memberKey
        (decl: TypeKey)
        (name: string)
        (argSig: EqArray<FrozenType>)
        (methodTyparArity: int)
        (kind: MemberKind)
        : SymbolKey =
        SymbolKey.Member(memberKeyOf decl name argSig methodTyparArity kind)

    /// Narrow a wide `SymbolKey` to a `MemberKey`; `what` names the site in the failure.
    let asMemberKey (what: string) (k: SymbolKey) : MemberKey =
        match k with
        | SymbolKey.Member m -> m
        | other -> failwithf "%s: expected a MemberKey, got %A" what other

    let declTypeKeyOf (what: string) (k: SymbolKey) : TypeKey = (asMemberKey what k).Decl

    /// How many value parameters a member position's key DECLARES: its `ArgSig` width.
    let memberArity (what: string) (k: SymbolKey) : int = (asMemberKey what k).ArgSig.Length

    /// A tupled member's ONE argument opened to the `arity` positions it DECLARES. Declared
    /// width, not surface shape: an `(int * int)` parameter stays one position.
    let openTupledArg (asTuple: 'a -> 'a list voption) (arity: int) (arg: 'a) : 'a list voption =
        match arity with
        | 0 -> ValueSome []
        | 1 -> ValueSome [ arg ]
        | n ->
            match asTuple arg with
            | ValueSome elems when List.length elems = n -> ValueSome elems
            | _ -> ValueNone

    // --- Generic `SymbolKey` projection ----------------------------------------------

    /// The key's `name` with containment dropped: the PLAIN SOURCE name, never
    /// arity-suffixed. To COMPARE against a well-known intrinsic, match the key instead.
    let intrinsicName (k: SymbolKey) : string =
        match k with
        | SymbolKey.Type t -> t.Name
        | SymbolKey.Binding b -> b.Name
        | SymbolKey.Member m -> m.Name

    /// The key's name for HUMAN DISPLAY: containment and arity dropped, so `Point<'a,'b>` and
    /// `Point<'a,'b,'c>` display alike.
    let simpleName (k: SymbolKey) : DisplayName = DisplayName(intrinsicName k)

    /// `simpleName` for a caller already holding the narrow `TypeKey`.
    let typeSimpleName (t: TypeKey) : DisplayName = DisplayName t.Name

    /// The fully-qualified compiled name for an EXTERNAL nominal lookup.
    let qualifiedName (k: SymbolKey) : string =
        match k with
        | SymbolKey.Type t -> typeMetaName t
        | SymbolKey.Binding b ->
            match containerFullName b.Decl with
            | "" -> b.Name
            | h -> h + "." + b.Name
        | SymbolKey.Member m -> m.Name

    /// The last `.` segment is the simple name, the prefix the namespace.
    let qualifiedTypeKeyOf (compiled: string) (arity: int) : TypeKey =
        let ns, simple = splitLastDot compiled
        withArity arity (typeKeyOf ns simple)

    /// Passing arity 0 for an already-suffixed generic name is lossless: the suffix PARSES
    /// into `TyparArity`, giving the same key as the bare name plus the count.
    let qualifiedTypeKey (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(qualifiedTypeKeyOf compiled arity)

    /// The namespace comes from `compiled` itself whenever `compiled` is qualified; `origin`
    /// supplies it only for a BARE name.
    let externalTypeKeyOf (origin: SymbolOrigin) (compiled: string) (arity: int) : TypeKey =
        if compiled.IndexOf '.' >= 0 then
            qualifiedTypeKeyOf compiled arity
        else
            withArity arity (typeKeyOf origin.Namespace.Dotted compiled)

    let externalTypeKey (origin: SymbolOrigin) (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(externalTypeKeyOf origin compiled arity)

    /// The contract-sourced canon key for a published intrinsic: the key of its COMPILED name,
    /// spelled arity-suffixed (`` Vesper.Collections.seq`1 ``) so `` `1 `` parses into arity.
    let intrinsicCanonKey (compiled: string) : TypeKey = qualifiedTypeKeyOf compiled 0
