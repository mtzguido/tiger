structure color :> color =
struct
    open graph common set

    exception Retry of node

    datatype coloring =
        OK of (node -> int)
      | FAILED of node

    fun empty_coloring n = ~1
    fun oplus f id c = fn id' => if id = id' then c else f id'

    fun interval m n = if m > n then [] else m::(interval (m+1) n)

    fun fixup k cf g n =
        let val colors = interval 1 k
            val Nn = adj n
            val used = map (fn n => cf (id n)) (tolist Nn)
            val possible = ldiff colors used
         in case possible of
              (h::t) => oplus cf (id n) h
            | [] => raise Retry n
        end

    fun try_color pre k g =
        let val ns = List.filter (fn n => pre (id n) = NONE) (tolist (nodes g))
         in case ns of
               [] => valOf o pre
             | _ => let val easy = List.filter (fn n => outdeg n < k) ns
                        val g' = copy g
                        val rm = case easy of
                                     (h::_) => h
                                   | [] => hd (ns)

                        val _ = rm_node_id g' (id rm)
                        val c' = try_color pre k g'
                      in fixup k c' g rm end
         end

    fun color k precolor graph =
        let fun pre Id =
            let val rnode = hd (List.filter (fn n => id n = Id) (tolist (nodes graph)))
             in precolor rnode end
         in OK ((try_color pre k graph) o id)
                handle Retry n =>
                    let val Id = id n
                        val n' = List.filter (fn n => id n = Id) (tolist (nodes graph))
                     in FAILED (hd n')
                    end
        end
end
