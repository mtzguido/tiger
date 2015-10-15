structure color :> color =
struct
    open graph

    datatype coloring =
        OK of (node -> int)
      | FAILED of node

    (* Maybe check that the graph is actually symmetrical? *)
    fun deg n = length (adj n)

    fun index x [] = raise Fail "not elem"
      | index x (h::t) = if x = h then 0 else 1 + index x t

    fun color k graph =
        let val g = copy graph
            val ns = nodes g
         in OK (fn n => index (id n) (map id ns)) end
end
