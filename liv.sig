signature liv =
sig
    datatype igraph =
        IGRAPH of { graph : graph.graph,
                    tnode : temp.temp -> graph.node,
                    ntemp : graph.node -> temp.temp,
                    moves: (graph.node * graph.node) list
                  }

    (* interference graph, live in and live out sets *)
    val liveness : flow.flowgraph ->
                    (graph.node -> temp.temp set.set * temp.temp set.set) * igraph
end
