signature graph =
sig
    type graph
    eqtype node

    exception GraphEdge
    exception GraphFail of string

    val nodes : graph -> node set.set
    val succ : node -> node set.set
    val pred : node -> node set.set
    val adj  : node -> node set.set

    val related : node -> node -> bool

    val newGraph : unit -> graph
    val newNode : graph -> node

    
    val mk_edge : node -> node -> unit
    val rm_edge : node -> node -> unit

    val rm_node : node -> unit
    val rm_node_id : graph -> int -> unit

    val id : node -> int

    val mk_edge_sym : node -> node -> unit

    val nodename : node -> string

    val printGraph : graph -> unit

    val copy : graph -> graph

    val outdeg : node -> int
    val indeg : node -> int

    val revdfs : graph -> node list
end
