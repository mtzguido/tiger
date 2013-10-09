signature semantics = 
sig
    exception SemanFail
    val semantics : ast.exp -> unit
end
