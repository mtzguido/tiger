signature liv =
sig
    (* live in and live out sets *)
    val liveness : flow.flowgraph ->
                    (graph.node -> temp.temp set.set * temp.temp set.set)
end
