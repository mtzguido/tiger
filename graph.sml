structure graph :> graph =
struct
    datatype node = N of {
        id: int,
        g: graph
    }
    and graph = G of {
        nodes: node list,
        edges: (node * node) list
    } ref

    fun unG (G g) = g
    fun ngraph (N n) = #g n

    fun nodes (G g) = #nodes (!g)
    fun edges (G g) = #edges (!g)

    fun nedges n = edges (ngraph n)

    fun fst (a,b) = a
    fun snd (a,b) = b

    fun fsteq x (a,b) = x = a
    fun sndeq x (a,b) = x = b
    fun anyeq x (a,b) = x = a orelse x = b

    fun elem x [] = false
      | elem x (h::t) = x = h orelse elem x t

    fun add_uniq x l = if elem x l then l else x :: l

    fun uniq l = rev (foldl (fn (e,a) => add_uniq e a) [] l)

    fun succ n = List.map snd (List.filter (fsteq n) (nedges n))
    fun pred n = List.map fst (List.filter (sndeq n) (nedges n))
    fun adj  n = uniq (succ n @ pred n)

    fun eq n1 n2 = n1 = n2

    fun newGraph () = G (ref { nodes = [], edges = [] })

    fun newNode (G g) =
        let val nodes = #nodes (!g)
            val edges = #edges (!g)
            val id = 1 + length (#nodes (!g))
            val n = N { id = id, g = (G g)}
         in g := { nodes = add_uniq n nodes, edges = edges};
            n end

    fun newEdge (G g) n1 n2 =
        let val nodes = #nodes (!g)
            val edges = #edges (!g)
         in g := { nodes = nodes, edges = add_uniq (n1,n2) edges} end

    fun delEdge (G g) n1 n2 =
        let val nodes = #nodes (!g)
            val edges = #edges (!g)
            val edges' = List.filter (fn (a,b) => a <> n1 orelse b <> n2) edges
         in g := { nodes = nodes, edges = edges'} end

    exception GraphEdge
    exception GraphFail of string

    fun mk_edge n1 n2 =
        let val G g1 = ngraph n1
            val G g2 = ngraph n2
         in if g1 <> g2
            then raise GraphFail "mk_edge en grafos distintos"
            else newEdge (G g1) n1 n2
        end

    fun mk_edge_sym n1 n2 =
        (mk_edge n1 n2; mk_edge n2 n1)

    fun rm_edge n1 n2 =
        let val G g1 = ngraph n1
            val G g2 = ngraph n2
         in if g1 <> g2
            then raise GraphFail "mk_edge en grafos distintos"
            else delEdge (G g1) n1 n2
        end

    fun rm_node n =
        let val G g = ngraph n
            val nodes' = List.filter (fn x => x <> n) (#nodes (!g))
            val edges' = List.filter (not o anyeq n)  (#edges (!g))
         in g := { nodes = nodes', edges = edges' } end

    fun id (N n) = #id n

    fun nodename (N n) = "n" ^ (makestring (#id n))

    fun printGraph g =
        let val ns = nodes g
            fun p1 n = (print ((nodename n) ^ ": ");
                       List.app (fn n => print ((nodename n) ^ ", ")) (succ n);
                       print "\n")
         in List.app p1 ns end

    fun copy g =
        let val r = ref { nodes = [], edges = [] }
            val graph = G r
            val nodes' = map (fn (N {id, g}) => N {id=id, g=graph}) (nodes g)
         in r := { nodes = nodes', edges = edges g } ;
            graph end
end
