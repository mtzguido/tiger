signature graph =
sig
    type graph
    type node

    exception GraphEdge
    exception GraphFail of string

    val nodes : graph -> node list
    val succ : node -> node list
    val pred : node -> node list
    val adj  : node -> node list
    val eq : node -> node -> bool

    val newGraph : unit -> graph
    val newNode : graph -> node
    
    val mk_edge : node -> node -> unit
    val rm_edge : node -> node -> unit

    val mk_edge_sym : node -> node -> unit

    val nodename : node -> string

    val printGraph : graph -> unit
end
