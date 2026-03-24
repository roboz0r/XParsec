module InlineIL =
    [<CompiledName("Length1")>]
    let length1 (array: 'T[,]) =  (# "ldlen.multi 2 0" array : int #)  
    
    [<CompiledName("Set")>]
    let set (array: 'T[,]) (index1:int) (index2:int) (value:'T) =  
        (# "stelem.multi 2 !0" type ('T) array index1 index2 value #)  
        
    let inline arrayZeroCreate (n:int) = (# "newarr !0" type ('T) n : 'T array #)
    let inline freshConsNoTail h = h :: (# "ldnull" : 'T list #)

    type ``[]``<'T> = (# "!0[]" #)
    type ``[,]``<'T> = (# "!0[0 ...,0 ...]" #)

    [<AttributeUsage(AttributeTargets.Method, AllowMultiple=false)>]
    [<Sealed>]
    type CompilationArgumentCountsAttribute(counts:int array) =
        inherit Attribute()
        member _.Counts = 
           let unboxPrim(x:obj) = (# "unbox.any !0" type ('T) x : 'T #)
           (unboxPrim(counts.Clone()) : IEnumerable<int>)
           
    [<NoDynamicInvocation>]
    [<CompiledName("StackAllocate")>]
    let inline stackalloc (count: int) : nativeptr<'T> = (# "localloc" (count * sizeof<'T>) : nativeptr<'T> #)

    let inline typeof<'T> =
        let tok = (# "ldtoken !0" type('T) : System.RuntimeTypeHandle #)
        Type.GetTypeFromHandle(tok)