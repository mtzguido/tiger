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


    fun color k precolor graph moves =

        let fun pre Id =
            let val rnode = valOf (set.find (fn n => id n = Id) (nodes graph))
             in precolor rnode end

        (* We work with IDs *)
        val moves = map (fn (a,b) => (id a, id b)) moves

        (* Set of move related IDs *)
        val mr_set = fromlist Int.compare (map (#1) moves @ map (#2) moves)

        fun significant n = outdeg n >= k

        fun fixup cf g n =
            let val colors = interval 1 k
                val Nn = adj n
                val used = map (fn n => cf (id n)) (tolist Nn)
                val possible = ldiff colors used
             in case possible of
                  (h::t) => oplus cf (id n) h
                | [] => raise Retry n
            end

        fun precolored n = pre (id n) <> NONE

        fun move_related n =
            member (id n) mr_set

        fun possible_spills g =
            pop_and_fixup (not o precolored) g (fn _ => valOf o pre)

        and pop_and_fixup pred g next =
            let val n = set.find pred (nodes g)
             in case n of
                   NONE => next g
                 | SOME n => remove_and_simplify g n
             end

        and remove_and_simplify g n =
            let val g' = copy g
                val _ = rm_node_id g' (id n)
                val c' = simplify g'
             in fixup c' g n
            end

        and try_briggs g =
            false

        and coallesce g =
            case try_briggs g of
                true => simplify g
              | false => possible_spills g

        and simplify g =
            let fun pred n = not (precolored n) andalso
                             not (significant n) andalso
                             not (move_related n)
            in pop_and_fixup pred g coallesce end

         in let val id_color = simplify graph
             in OK (fn n => id_color (id n)) end
            handle Retry n =>
                let val Id = id n
                    val mn' = set.find (fn n => id n = Id) (nodes graph)
                 in case mn' of
                        SOME n' => FAILED n'
                      | NONE => raise Fail "color: internal error"
                end
        end
end
