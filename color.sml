structure color :> color =
struct
    open graph common set

    exception Retry of node list list

    datatype coloring =
        OK of node -> int
      | FAILED of node list list

    fun oplus f id c = fn id' => if id = id' then c else f id'

    fun interval m n = if m > n then [] else m::(interval (m+1) n)

    fun color k precolor graph moves =

        let fun pre Id =
            let val rnode = valOf (set.find (fn n => id n = Id) (nodes graph))
             in precolor rnode end

        (* Set of move related IDs, changes during run *)
        val mr_set = ref (fromlist graph.ncomp (map (#1) moves @ map (#2) moves))

        fun significant n = outdeg n >= k

        fun fixup cf g n =
            let val colors = interval 1 k
                val Nn = adj n
                val used = map (fn n => cf (id n)) (tolist Nn)
                val possible = ldiff colors used
             in case possible of
                  (h::t) => oplus cf (id n) h
                | [] => raise Retry [[n]]
            end

        (* FIXME: this handling sucks *)
        fun precolored n =
            (case pre (id n) of
                NONE => false
              | _ => true)
            handle Unmapped => false

        fun move_related n =
            member n (!mr_set)

        fun remove_move_related n =
            mr_set := delete n (!mr_set)

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

        and coalesce_fixup color mnc g m n =
            let val color = oplus color (id m) mnc
                val color = oplus color (id n) mnc
             in color end

        and do_coalesce g m n =
            let val g' = copy g

                (* map to new graph *)
                val m = case set.find (fn v => id v = id m) (nodes g') of
                            SOME x => x
                          | NONE => raise Fail "color: internal error 1"

                val n = case set.find (fn v => id v = id n) (nodes g') of
                            SOME x => x
                          | NONE => raise Fail "color: internal error 2"

                val neighs_ = union (adj m) (adj n)
                val neighs = delete m (delete n neighs_)

                val _ = rm_node m
                val _ = rm_node n

                val mn = newNode g'
                val _ = set.app (fn n => mk_edge_sym mn n) neighs
             in (mn, g') end

        and briggs_coalescible g m n =
            let val neighs_ = union (adj m) (adj n)
                val neighs = delete m (delete n neighs_)
                val s_neighs = List.filter significant (tolist neighs)
             in length s_neighs < k
            end

        and try_briggs g =
            let fun valid_pair (m, n) = member m (nodes g) andalso
                                          member n (nodes g) andalso
                                          not (related m n) andalso
                                          briggs_coalescible g m n

             in case List.find valid_pair moves of
                    NONE => NONE
                  | SOME (m, n) =>
                        let val (mn, g') = do_coalesce g m n
                            val c = simplify g'
                                    (*
                                     * If it fails, we need to translate
                                     * the node we just created to
                                     * the original graph
                                     *)
                                    handle (Retry [ns]) =>
                                        let fun M v = if id v = id mn
                                                      then [m, n]
                                                      else [v]
                                            val ns' = List.concat (map M ns)
                                         in raise Retry [ns'] end

                         in SOME (coalesce_fixup c (c (id mn)) g m n) end
            end

        and freeze g =
            let val nodes = tolist (nodes g)
                val nodes = List.filter move_related nodes
                val nodes = List.filter (not o significant) nodes
             in case nodes of
                    [] => possible_spills g
                  | n::_ => (remove_move_related n;
                             simplify g)
            end

        and coalesce g =
            case try_briggs g of
                SOME g => g
              | NONE   => freeze g

        and simplify g =
            let fun pred n = not (precolored n) andalso
                             not (significant n) andalso
                             not (move_related n)
            in pop_and_fixup pred g coalesce end

         in let val id_color = simplify graph
             in OK (fn n => id_color (id n)) end
            handle Retry nss =>
                let fun mapN n = valOf (set.find (fn v => id v = id n) (nodes graph))
                 in FAILED (map (map mapN) nss) end
        end
end
