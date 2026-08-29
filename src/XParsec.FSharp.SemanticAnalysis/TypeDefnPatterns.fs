namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

/// Helpers for projecting `TypeDefn` shapes. The parser emits `TypeDefn.Anon` for the
/// bare `type C(…) = member …` form with no `class`/`end`; every projection here treats
/// it as `TypeDefn.Class`.
module TypeDefnPatterns =

    [<NoEquality; NoComparison>]
    type ClassLikeDecl<'T> =
        {
            TypeName: TypeName<'T>
            PrimaryConstr: PrimaryConstrArgs<'T> voption
            AsDefn: AsDefn<'T> voption
            Body: ObjectModelBody<'T>
        }

    /// The `Class`, `Anon` and `Struct` shapes only; `ValueNone` for every other one.
    let tryClassLikeDecl (td: TypeDefn<'T>) : ClassLikeDecl<'T> voption =
        match td with
        | TypeDefn.Class(typeName = tn; primaryConstr = pc; asDefn = asD; body = body)
        | TypeDefn.Anon(typeName = tn; primaryConstr = pc; asDefn = asD; body = body)
        | TypeDefn.Struct(typeName = tn; primaryConstr = pc; asDefn = asD; body = body) ->
            ValueSome
                {
                    TypeName = tn
                    PrimaryConstr = pc
                    AsDefn = asD
                    Body = body
                }
        | _ -> ValueNone

    /// `true` for the explicit `type X = struct … end` shape: a value type even without
    /// a `[<Struct>]` attribute, which instead lands as `Class`/`Anon`.
    let isStructShape (td: TypeDefn<'T>) : bool =
        match td with
        | TypeDefn.Struct _ -> true
        | _ -> false

    /// `true` for the explicit `interface … end` shape, or the all-abstract object-model
    /// form (`type IFoo = abstract member …`). An `inherit` clause is neutral, as fsc
    /// kinds it: `type I2 = inherit I1  abstract M: …` is an interface.
    let isInterfaceShape (td: TypeDefn<'T>) : bool =
        match td with
        | TypeDefn.Interface _ -> true
        | _ ->
            match tryClassLikeDecl td with
            | ValueNone -> false
            | ValueSome d ->
                d.PrimaryConstr.IsNone
                && d.Body.classPreamble.IsEmpty
                && not d.Body.elements.IsEmpty
                && d.Body.elements
                   |> Seq.forall (fun el ->
                       match el with
                       | TypeDefnElement.Member(MemberDefn.Member(defn = MethodOrPropDefn.AbstractSignature _)) -> true
                       | _ -> false
                   )

    /// The signature-side twin of `isInterfaceShape`: an all-abstract bodied signature type
    /// (`type IFormatSink = abstract member …`) parses as `Anon` / `Class`, so the kind is
    /// read off the members. An `inherit` clause is neutral, as fsc kinds it.
    let bodyIsInterface (elems: TypeElementsSignature<'T>) : bool =
        let mutable hasAbstract = false
        let mutable hasConcrete = false

        for e in elems do
            match e with
            | TypeSignatureElement.Abstract _ -> hasAbstract <- true
            | TypeSignatureElement.Member _
            | TypeSignatureElement.StaticMember _
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Value _
            | TypeSignatureElement.Override _
            | TypeSignatureElement.Default _ -> hasConcrete <- true
            | TypeSignatureElement.Interface _
            | TypeSignatureElement.Inherit _ -> ()

        hasAbstract && not hasConcrete

    /// A single-ident `Union` or `Record` name with its `with`-block elements: the
    /// channel that carries an `interface … with` / augmentation member.
    let tryNonClassMemberHostDecl (td: TypeDefn<'T>) : struct (TypeName<'T> * TypeDefnElements<'T> voption) voption =
        let extElems (ext: TypeExtensionElements<'T> voption) =
            match ext with
            | ValueSome(TypeExtensionElements(elements = elems)) -> ValueSome elems
            | ValueNone -> ValueNone

        match td with
        | TypeDefn.Union(typeName = (TypeName(ident = nameLi) as tn); extensions = ext)
        | TypeDefn.Record(typeName = (TypeName(ident = nameLi) as tn); extensions = ext) when nameLi.Idents.Length = 1 ->
            ValueSome(struct (tn, extElems ext))
        // An inline intrinsic-abbrev augmented with `with member …`
        // (`type X = (# … #) with member …`) hosts its members on the same path.
        | TypeDefn.Abbrev(typeName = (TypeName(ident = nameLi) as tn); extensions = ext & ValueSome _) when
            nameLi.Idents.Length = 1
            ->
            ValueSome(struct (tn, extElems ext))
        | _ -> ValueNone

    /// The `body` shared by `Class | Anon | Struct | Interface`; `ValueNone` otherwise.
    let tryObjectModelBody (td: TypeDefn<'T>) : ObjectModelBody<'T> voption =
        match td with
        | TypeDefn.Class(body = b)
        | TypeDefn.Anon(body = b)
        | TypeDefn.Struct(body = b)
        | TypeDefn.Interface(body = b) -> ValueSome b
        | _ -> ValueNone
