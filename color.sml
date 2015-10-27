structure color :> color =
struct
    open graph common set

    exception Retry of node

    datatype coloring =
        OK of node -> int
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

    fun possible_spills pre k g =
        let val n = set.find (fn n => pre (id n) = NONE)
                    (nodes g)
         in case n of
               NONE => valOf o pre
             | SOME n =>
                 let val g' = copy g
                     val _ = rm_node_id g' (id n)
                     val c' = simplify pre k g'
                  in fixup k c' g n end
         end

    and coallesce pre k g =
        possible_spills pre k g

    and simplify pre k g =
        let val n = set.find (fn n => pre (id n) = NONE andalso outdeg n < k)
                    (nodes g)
         in case n of
               NONE => coallesce pre k g
             | SOME n =>
                 let val g' = copy g
                     val _ = rm_node_id g' (id n)
                     val c' = simplify pre k g'
                  in fixup k c' g n end
         end

    fun color k precolor graph moves =
        let fun pre Id =
            let val rnode = valOf (set.find (fn n => id n = Id) (nodes graph))
             in precolor rnode end
         in OK ((simplify pre k graph) o id)
                handle Retry n =>
                    let val Id = id n
                        val n' = List.filter (fn n => id n = Id) (tolist (nodes graph))
                     in FAILED (hd n')
                    end
        end
end
