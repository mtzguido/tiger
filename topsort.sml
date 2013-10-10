structure topsort :> topsort =
struct 
    open hash
    exception Ciclo

    val loop = ref NONE

    datatype ''a result = OK of ''a list
                        | CICLE of ''a list

    fun elem x [] = false
      | elem x (e::es) = x = e orelse elem x es
    
    fun checkDups [] = false
      | checkDups (e::es) = if elem e es then true else checkDups es

    fun topSort' deps [] = []
      | topSort' deps verts =
        let fun leaf v visited = 
                let val _ = if elem v visited
                            then let fun takeUntil e [] = []
                                       | takeUntil e (f::t) =
                                          if e = f then [e] else f::(takeUntil e t)
                                     val cicle = takeUntil v visited
                                  in ( loop := SOME cicle ; raise Ciclo )
                                 end
                            else ()
                    val preds = List.filter (fn (l,h) => l = v) deps
                in case preds of
                     [] => v
                     | _ => leaf (#2 (hd preds)) (v::visited)
                end
            val min_elem = leaf (hd verts) []
        in min_elem :: ( topSort' 
                          (List.filter (fn (l,h) => h <> min_elem) deps)
                          (List.filter (fn x => x <> min_elem) verts)
                       )
        end

    fun topSort deps vs = ( loop := NONE ;
                            OK (topSort' deps vs) handle Ciclo => CICLE (valOf (!loop)) )
end
