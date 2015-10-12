structure translate :> translate =
struct
    open ir canon codegen flowcalc flow graph liv common set

    datatype Level = Outermost
                   | Frame of { frame : frame.Frame,
                                parent : Level,
                                sl : frame.Access,
                                uniq : unit ref
                              }

    type Access = Level * frame.Access

    val outermost = Outermost

    fun newLevel {parent, name, formals} =
        let val ff = frame.mkFrame {name = name, formals = true::formals}
        in Frame { frame = ff,
                   parent =  parent,
                   sl = hd (frame.frameFormals ff),
                   uniq = ref ()
                 }
        end

    fun allocLocal (Frame ff) e = (Frame ff, frame.frameAllocLocal (#frame ff) e)
    |   allocLocal Outermost _ = raise Fail "wrong allocLocal"


    fun unFrame (Frame x) = x
      | unFrame _ = raise Fail "unFrame unimplemented"
    val FP = frame.FP
    val RV = frame.RV

    (*
     * Returns an expression for the var represented by acc
     * in frame _#frame ff_ from frame _l2_, traversing static links
     * as needed.
     *)
    fun simpleVar' (Frame tt, acc) (Frame cc) fp=
        if #uniq tt = #uniq cc
        then frame.simpleVar acc fp
        else simpleVar' (Frame tt, acc) (#parent cc) (frame.simpleVar (#sl cc) fp)
      | simpleVar' _ _ _ =
        raise Fail "wrong simpleVar"

    fun simpleVar acc cf = simpleVar' acc cf FP

    fun indexVar base i =
        Ex (Mem (Binop (Plus, base, Binop (Mul, i, Const frame.wordSize))))

    fun formals (Frame ff) =
            map (fn a => (Frame ff, a)) (tl (frame.frameFormals (#frame ff)))
      | formals _ = raise Fail "formals unimplemented"

    (* There are esentially three cases for a call:
     * - Callee is nested directly in our frame
     * - Callee is at our same level (same block of decls, or recursion)
     * - Callee is N levels above us
     *
     * In each case, we find the frame pointer for the callee frame
     * using simpleVar, which already handles fetching a variable
     * from another frame. The variable we need is (#sl f) in frame
     * (Frame f), so make an access from that and use it.
     *)

    fun trCall true _ _ fname args =
        Call (Name fname, args)
      | trCall false (Frame f) (Frame us) fname args =
        let val sl = simpleVar (Frame f, #sl f) (Frame us)
        in Call (Name fname, sl :: args) end
      | trCall _ _ _ _ _ =
          raise Fail "trCall unimplemented"

    fun funcDecl (Frame f) b =
        let val b = frame.wrapFun1 b (#frame f)
            val b = canon b
            val (blocks, done_label) = bblocks b
            val trace = traceSched blocks
            fun p_stmts ss = concat (List.map (fn s => "  " ^ (irToString (Nx s) ^ "\n")) ss)
            val _ = print ("Trace: \n" ^ p_stmts trace)
            val asm = List.concat (map codegen trace)
            val asm = frame.wrapFun2 (#frame f) asm
            val texts = map (asm.print temp.toString) asm
            val _ = map (fn s => print (s ^ "\n")) texts
            val flow = flowcalc asm
            val (liv, interf) = liveness flow
            val FGRAPH cfg = flow
            val _ = printGraph (#control cfg)
            fun p_liv_1 n =
                let val (inS, outS) = liv n
                 in "liveness for " ^ nodename n ^ ":\n" ^
                    "IN: " ^ list_decor (map temp.toString (tolist inS)) ^ "\n" ^
                    "OUT: " ^ list_decor (map temp.toString (tolist outS)) ^ "\n"
                 end

            val _ = List.app (print o p_liv_1) (nodes (#control cfg))
         in () end
      | funcDecl _ _ =
        raise Fail "funcDecl unimplemented"

    fun stringExp s = frame.addString s
end
