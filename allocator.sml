open flow flowcalc liv graph common set temp color

fun run asm =
    let val flow = flowcalc asm

        val texts = map (asm.print temp.toString) asm
        
        val _ = print "unallocated asm text:\n"
        val _ = List.app (fn t => print (t ^ "\n")) texts
        val _ = print "\n"
        
        val (liv, interf) = liveness flow
        val FGRAPH cfg = flow
        val IGRAPH itf = interf
        val _ = printGraph (#control cfg)
        fun p_liv_1 n =
            let val (inS, outS) = liv n
             in "liveness for " ^ nodename n ^ ":\n" ^
                "IN: " ^ list_decor (map temp.toString (tolist inS)) ^ "\n" ^
                "OUT: " ^ list_decor (map temp.toString (tolist outS)) ^ "\n"
             end
        
        fun p_interf_1 n =
            let val ntemp = #ntemp itf
            in "interferences for " ^ toString (ntemp n) ^ ":\n" ^
               list_decor (map (temp.toString o ntemp) (succ n)) ^ "\n"
            end
        
        fun print_move (l,r) = "(" ^ toString (#ntemp itf l) ^ ", " ^ toString (#ntemp itf r) ^ ")"
        
        val _ = List.app (print o p_liv_1) (nodes (#control cfg))
        val _ = List.app (print o p_interf_1) (nodes (#graph itf))
        val _ = print ("Moves: " ^ list_decor (map print_move (#moves itf)) ^ "\n")
        val C = case color (length frame.gpregs) (#graph itf) of OK c => c | _ => raise Fail "color failed"

        val _ = List.app (fn n => print (toString (#ntemp itf n) ^ ": " ^ makestring (C n) ^ "\n")) (nodes (#graph itf))

        fun C_inv c =
            let val p = List.filter (fn (_,c') => c = c') (map (fn r => (r, C (#tnode itf r))) frame.gpregs)
             in case p of
                [(r,_)] => r
              | [] => raise Fail "no coloring?"
              | _ => raise Fail "multipli coloring?"
            end
        
        fun allocation r =
            if isreal r
            then r
            else C_inv (C (#tnode itf r))
    in allocation end
