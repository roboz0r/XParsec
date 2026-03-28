module TestUseDiscard

module A = 
    let g () = 
        use _ = new CompilationGlobalsScope()
