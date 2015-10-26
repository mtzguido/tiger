structure allocator :> allocator =
struct

open flow flowcalc liv graph common set temp color
open codegen ir frame

fun spill1 reg acc i = case i of
    asm.LABEL _ => [i]
  | asm.OPER {asm, dst, src, jump} =>
        if elem reg dst orelse elem reg src
        then let val t = newtemp ()
                 val pre  = if elem reg src
                            then codegen (Move (Temp t, simpleVar acc FP))
                            else []
                 val ii   = asm.OPER { asm = asm, jump = jump,
                                       dst = common.replace reg t dst,
                                       src = common.replace reg t src }
                 val post = if elem reg dst
                            then codegen (Move (simpleVar acc FP, Temp t))
                            else []
             in pre @ (ii :: post) end
        else [i]
  | asm.MOVE {asm, dst, src} =>
    let val t = newtemp ()
     in if reg = src
        then codegen (Move (Temp t, simpleVar acc FP))
             @ [asm.MOVE {asm=asm, dst=dst, src=t}]
        else if reg = dst
        then [asm.MOVE {asm=asm, dst=t, src=src}]
             @ codegen (Move (simpleVar acc FP, Temp t))
        else [i]
    end

fun spill reg acc asm =
    List.concat (map (spill1 reg acc) asm)

fun is_trivial_move (asm.MOVE {asm, dst, src}) = dst = src
  | is_trivial_move _ = false

fun remove_trivial_moves asm = List.filter (not o is_trivial_move) asm

fun do_allocate C frame interf asm =
    let val IGRAPH {graph=itf, tnode=tnode, ntemp=ntemp, moves=moves} = interf
        val Ct = C o tnode

        fun mapped_to r = List.filter (fn t => not (isreal (ntemp t)) andalso C t = Ct r) (tolist (nodes itf))

        fun p_alloc_1 r =
            let val m = mapped_to r
            in if length m = 0 then ()
               else print (toString r ^ " <- " ^ list_decor (map (toString o ntemp) m) ^ "\n")
            end

        val _ = print "Register allocation used:\n"
        val _ = List.app p_alloc_1 frame.gpregs

        fun C_inv c =
            let val p = List.filter (fn (_,c') => c = c') (map (fn r => (r, C (tnode r))) frame.gpregs)
             in case p of
                [(r,_)] => r
              | [] => raise Fail ("no coloring for color " ^ printInt c ^ "?")
              | _ => raise Fail "multiple coloring?"
            end

        fun allocation r =
            if isreal r
            then r
            else C_inv (C (tnode r))

        val _ = if map allocation frame.gpregs <> frame.gpregs
                then raise Fail "allocation isn't ID on real gp-regs???"
                else ()

        val asm = asm.replace_alloc allocation asm
        val asm = remove_trivial_moves asm
    in asm end

fun allocate_regs frame interf asm =
    let val IGRAPH {graph=itf, tnode=tnode, ntemp=ntemp, moves=moves} = interf
        val _ = print "Trying to color....\n"

        (* This is pretty crappy *)
        fun precolor n = case (index_safe (ntemp n) gpregs) handle _ => NONE of
                            NONE => NONE
                          | SOME v => SOME (v + 1)

     in case color (length frame.gpregs) precolor itf of
              OK c => let val _ = print "Coloring OK!\n"
                       in do_allocate c frame interf asm end
            | FAILED n => let val reg = ntemp n
                              val _ = print ("Spilled node! : " ^ toString reg ^ "\n")
                              val _ = if isreal reg
                                          then raise Fail "spilled real node??\n"
                                          else ()
                              val acc = frame.frameAllocLocal frame true
                              val asm = spill reg acc asm

                              val texts = map (asm.print temp.toString) asm
                              val _ = print "Spilled assembly text:\n"
                              val _ = List.app (fn t => print (t ^ "\n")) texts
                              val _ = print "\n\n"

                           in run frame asm end
    end

and run frame asm =
    let val texts = map (asm.print temp.toString) asm

        val _ = print "Unallocated asm text:\n"
        val _ = List.app (fn t => print (t ^ "\n")) texts
        val _ = print "\n\n"

        (* build a CFG *)
        val flow = flowcalc asm
        val _ = print "CFG built\n"

        (* Do liveness analysis, and build the interference graph *)
        val (liv, interf) = liveness flow
        val _ = print "Interference graph built\n"

        val FGRAPH {control=cfg,...} = flow
        val IGRAPH {graph=itf, tnode=tnode, ntemp=ntemp, moves=moves} = interf

        (* Print *very* verbose debugging info *)
        fun p_liv_1 n =
            let val (inS, outS) = liv n
             in "liveness for " ^ nodename n ^ ":\n" ^
                "IN: " ^ list_decor (map temp.toString (tolist inS)) ^ "\n" ^
                "OUT: " ^ list_decor (map temp.toString (tolist outS)) ^ "\n"
             end

        fun p_interf_1 n = "interferences for " ^ toString (ntemp n) ^ ":\n" ^
                           list_decor (map (temp.toString o ntemp) (tolist (succ n))) ^ "\n"

        fun print_move (l,r) = "(" ^ toString (ntemp l) ^ ", " ^ toString (ntemp r) ^ ")"

        val _ = if !verbose
                    then List.app (print o p_liv_1) (tolist (nodes cfg))
                    else ()

        val _ = if !verbose
                    then List.app (print o p_interf_1) (tolist (nodes itf))
                    else ()

        val _ = if !verbose
                    then print ("Moves: " ^ list_decor (map print_move moves) ^ "\n")
                    else ()
        (* /Print verbose debug info *)

   in allocate_regs frame interf asm end
end
