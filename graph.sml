structure graph :> graph =
struct
    open hash set

    datatype node = N of {
        id: int,
        g: graph
    }
    and graph = G of {
        nodes: node set,
        counter: int,
        next: (int, node set) Tabla,
        prev: (int, node set) Tabla
    } ref

    fun unG (G g) = g
    fun ngraph (N n) = #g n

    fun ncomp (N m, N n) = Int.compare (#id m, #id n)
    fun id (N n) = #id n

    fun nodes (G g) = #nodes (!g)

    fun elem x [] = false
      | elem x (h::t) = x = h orelse elem x t

    fun add_uniq x l = if elem x l then l else x :: l
    fun uniq l = rev (foldl (fn (e,a) => add_uniq e a) [] l)

    fun succ n =
        let val G g = ngraph n
         in case tabFind (#next (!g)) (id n) of
                SOME v => v
              | NONE => emptySet ncomp
        end

    fun pred n =
        let val G g = ngraph n
         in case tabFind (#prev (!g)) (id n) of
                SOME v => v
              | NONE => emptySet ncomp
        end

    fun adj  n = union (succ n) (pred n)

    fun newGraph () = G (ref {
        nodes = emptySet ncomp,
        next = tabNew (),
        prev = tabNew (),
        counter = 0
    })

    fun newNode (G g) =
        let val nodes   = #nodes (!g)
            val next    = #next (!g)
            val prev    = #prev (!g)
            val counter = #counter (!g)
            val id = 1 + counter
            val n = N { id = id, g = (G g)}
         in g := { nodes = insert n nodes,
                   next = next, prev = prev,
                   counter = counter + 1 };
            n end

    fun newEdge (G g) m n =
        let val nodes   = #nodes (!g)
            val next    = #next (!g)
            val prev    = #prev (!g)
            val counter = #counter (!g)
            val msucc   = succ m
            val npred   = pred n
         in tabReplace next (id m, insert n msucc);
            tabReplace prev (id n, insert m npred)
        end

    fun delEdge (G g) m n =
        let val nodes   = #nodes (!g)
            val next    = #next (!g)
            val prev    = #prev (!g)
            val counter = #counter (!g)
            val msucc   = succ m
            val npred   = pred n
         in tabReplace next (id m, delete n msucc);
            tabReplace prev (id n, delete m npred)
        end

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
            val nodes   = #nodes (!g)
            val next    = #next (!g)
            val prev    = #prev (!g)
            val counter = #counter (!g)
            val next'   = tabMap next (fn (k, v) => delete n v)
            val prev'   = tabMap prev (fn (k, v) => delete n v)
         in g := { nodes = delete n nodes,
                   next = next', prev = prev',
                   counter = counter }
        end

    fun id (N n) = #id n

    fun rm_node_id (G g) Id =
        let val n = case find (fn n => (id n) = Id) (#nodes (!g)) of
                      NONE => raise Fail "rm of missing id"
                    | SOME x => x
         in rm_node n end

    fun nodename (N n) = "n" ^ (makestring (#id n))

    fun printGraph g =
        let val ns = nodes g
            fun p1 n = (print ((nodename n) ^ ": ");
                       List.app (fn n => print ((nodename n) ^ ", ")) (tolist (succ n));
                       print "\n")
         in List.app p1 (tolist ns) end

    fun copy (G g) =
        let val nodes   = #nodes (!g)
            val next    = #next (!g)
            val prev    = #prev (!g)
            val counter = #counter (!g)
            val r       = ref { nodes = nodes, prev = prev, next = next, counter = 0 }
            val graph = G r

            fun map_node (N {id, g}) = N {id=id, g=graph}

            (* this sucks *)
            fun fixup (k, v) = fromlist ncomp (map map_node (tolist v))

            val nodes'  = fromlist ncomp (map map_node (tolist nodes))
            val next'   = tabMap next fixup
            val prev'   = tabMap prev fixup

         in r := { nodes = nodes', next = next', prev = prev', counter = counter };
            graph end

    fun outdeg n = size (succ n)
    fun  indeg n = size (pred n)

    fun revdfs g =
        let val visited : (node, bool) Tabla = tabNew ()
            val trace = ref []
            fun mark n = trace := n::(!trace)

            fun visit node = (
                case tabFind visited node of
                    NONE => (
                        tabReplace visited (node, true);
                        mark node;
                        set.app visit (succ node)
                    )
                  | SOME _ => ()
            )

         in set.app visit (nodes g);
            !trace
        end
end
