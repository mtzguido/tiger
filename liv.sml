structure liv :> liv =
struct
    open set graph flow common temp frame

    datatype igraph =
        IGRAPH of { graph : graph.graph,
                    tnode : temp.temp -> graph.node,
                    ntemp : graph.node -> temp.temp,
                    moves: (graph.node * graph.node) list
                  }

    exception Unmapped

    fun all e = fn _ => e
    fun oplus m v e = fn x => if x = v then e else m x

    fun rep 0 f b = b
      | rep n f b = rep (n-1) f (f b)

    fun fixpoint graph use def =
        let fun proc1 n (inS, outS, p) =
            let val inSn' = union (fromlist tcomp (use n)) (diff (outS n) (fromlist tcomp (def n)))
                val outSn' = set.foldl (fn (n2, s) => union s (inS n2)) (emptySet tcomp) (succ n)
                val inS'  = oplus inS  n inSn'
                val outS' = oplus outS n outSn'
                val p' = p orelse (size inSn'  > size (inS n))
                           orelse (size outSn' > size (outS n))
             in (inS', outS', p') end

             fun lap (inS, outS) =
                set.foldl (fn (n,s) => proc1 n s) (inS, outS, false) (nodes graph)

             fun rep (inS, outS) =
                let val (inS', outS', p) = lap (inS, outS)
                 in if p
                    then rep (inS', outS')
                    else     (inS, outS)
                end
             val (inS, outS) = rep (all (emptySet tcomp), all (emptySet tcomp))
          in fn n => (inS n, outS n) end

    fun interf_calc flow liv_fun =
        let val FGRAPH {control, use, def, ismove} = flow
            val init = IGRAPH {graph = newGraph (),
                               tnode = fn _ => raise Unmapped,
                               ntemp = fn _ => raise Unmapped,
                               moves = []}

            fun temp1 node = (def node) @ (use node)
            fun temps nodes = List.concat (List.map temp1 nodes)

            fun add_node interf temp =
                let val IGRAPH {graph, tnode, ntemp, moves} = interf
                 in (case tnode temp of _ => interf)
                    handle Unmapped => let val n = newNode graph
                                        val tnode' = oplus tnode temp n
                                        val ntemp' = oplus ntemp n temp
                                       in IGRAPH {graph=graph, tnode=tnode',
                                                  ntemp=ntemp', moves=moves} end
                end

            fun interfere ((p, q), s) =
                let val IGRAPH {tnode,...} = s
                 in if p = q orelse (isreal p andalso not (elem p gpregs))
                             orelse (isreal q andalso not (elem q gpregs))
                    then ()
                    else mk_edge_sym (tnode p) (tnode q);
                    s
                end

            fun interf_proc_node interf node =
                let val IGRAPH {graph, tnode, ntemp, moves} = interf
                    val defN = map tnode (def node)
                    val defT = def node
                    val (_, lo') = liv_fun node
                    val interf_set = if ismove node
                                         then case use node of
                                                  [h] => diff lo' (singleton tcomp h)
                                                | _ => raise Fail "non-singleton use in move??"
                                         else lo'
                    val interf_list = map tnode (tolist interf_set)
                    val interf_regs = tolist interf_set
                    val interf = List.foldl interfere interf (cartesian defT interf_regs)
                in if ismove node
                   then let val src = (case use node of
                                          [h] => tnode h
                                        | _ => raise Fail "non-singleton use in move??")
                            val dst = (case defN of
                                          [h] => h
                                        | _ => raise Fail "non-singleton def in move??")
                        in IGRAPH {graph=graph, tnode=tnode,
                                   ntemp=ntemp, moves=(src,dst)::moves}
                        end
                   else interf
                end

            val itf = init
            val itf = List.foldl (fn (n, s) => add_node s n) itf gpregs
            val itf = List.foldl interfere itf (cartesian gpregs gpregs)
            val itf = List.foldl (fn (n, s) => add_node s n) itf (temps (tolist (nodes control)))
            val itf = foldl (fn (n, s) => interf_proc_node s n) itf (nodes control)
        in itf end

    fun liveness flow =
        let val FGRAPH {control, use, def, ismove} = flow
            val inS = all emptySet
            val outS = all emptySet
            val liv_fun = fixpoint control use def
            val interf = interf_calc flow liv_fun
         in (liv_fun, interf) end
end
