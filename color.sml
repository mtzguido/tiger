structure color :> color =
struct
    open graph common

    exception Retry of node

    datatype coloring =
        OK of (node -> int)
      | FAILED of node

    (* Maybe check that the graph is actually symmetrical? *)
    fun deg n = length (adj n)

    fun empty_coloring n = ~1
    fun oplus f id c = fn id' => if id = id' then c else f id'

    fun interval m n = if m > n then [] else m::(interval (m+1) n)

    fun fixup k cf g n =
        let val colors = interval 1 k
            val Nn = adj n
            val used = map (fn n => cf (id n)) Nn
            val possible = ldiff colors used
         in case possible of
              (h::t) => oplus cf (id n) h
            | [] => raise Retry n
        end

    fun try_color pre k g =
        let val ns = List.filter (fn n => pre n = NONE) (nodes g)
         in case ns of
               [] => empty_coloring
             | _ => let val easy = List.filter (fn n => deg n < k) ns
                        val g' = copy g
                        val rm = case easy of
                                     (h::_) => h
                                   | [] => hd (ns)

                        val _ = rm_node_id g' (id rm)
                        val c' = try_color pre k g'
                      in fixup k c' g rm end
         end

    fun color k precolor graph =
        OK ((try_color precolor k graph) o id)
        handle Retry n => let val Id = id n
                              val n' = List.filter (fn n => id n = Id) (nodes graph)
                           in FAILED (hd n') end
end
