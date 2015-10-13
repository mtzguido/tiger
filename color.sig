signature color =
sig
    datatype coloring =
        OK of (graph.node -> int)
      | FAILED of graph.node

    val color : int -> graph.graph -> coloring
end
