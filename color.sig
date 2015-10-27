signature color =
sig
    datatype coloring =
        OK of (graph.node -> int)
      | FAILED of graph.node

    val color : int -> (graph.node -> int option) ->
                graph.graph -> (graph.node * graph.node) list ->
                coloring
end
