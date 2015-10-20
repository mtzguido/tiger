structure translate :> translate =
struct
    open ir canon codegen flowcalc flow graph liv common set temp
    open color ofile

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

    fun get_fp' (Frame tt) (Frame us) fp =
        if #uniq tt = #uniq us
        then fp
        else get_fp' (Frame tt) (#parent us) (frame.simpleVar (#sl us) fp)
      | get_fp' Outermost Outermost _ =
        raise Fail "wrong get_fp (1) "
      | get_fp' _ Outermost _ =
        raise Fail "wrong get_fp (2) "
      | get_fp' Outermost _ _ =
        raise Fail "wrong get_fp (3) "

    fun get_fp to us = get_fp' to us FP


    (*
     * Returns an expression for the var represented by acc
     * in frame _#frame ff_ from frame _l2_, traversing static links
     * as needed.
     *)
    fun simpleVar' (Frame tt, acc) (Frame cc) fp=
        frame.simpleVar acc (get_fp (Frame tt) (Frame cc))
      | simpleVar' (Outermost, _) Outermost _ =
        raise Fail "wrong simpleVar (1) "
      | simpleVar' _ Outermost _ =
        raise Fail "wrong simpleVar (2) "
      | simpleVar' (Outermost,_) _ _ =
        raise Fail "wrong simpleVar (3) "

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
        let val sl = get_fp' (#parent f) (Frame us) FP
        in Call (Name fname, sl :: args) end
      | trCall _ _ _ _ _ =
          raise Fail "trCall unimplemented"

    fun funcDecl (Frame f) b =
        let val _ = print ("Generated IR for " ^  frame.frameName (#frame f) ^ ":\n")
            val text = irToString b
            val _ = print (indent text)
            val _ = print "\n\n"

            (*
             * call wrapFun1, which saves callee-saved
             * registers into fresh temporaries and
             * restores them at exit.
             *)
            val b = frame.wrapFun1 b (#frame f)

            (* canonize the IR tree *)
            val b = canon b

            (* split into basic blocks *)
            val (blocks, done_label) = bblocks b

            (* linearize the basic blocks into a trace *)
            val trace = traceSched blocks

            (* print the trace *)
            fun p_stmts s = print (indent (irToString (Nx s)) ^ "\n")
            val _ = print "Trace: \n"
            val _ = List.app p_stmts trace
            val _ = print "\n"

            (* generate real machine code, with unbounded regs *)
            val asm = List.concat (map codegen trace)

            (*
             * Call wrapFun2 which adds a dummy instruction
             * to keep that callee save regs live. This is needed
             * so they're not overwritten when allocating real
             * registers
             *)
            val asm = frame.wrapFun2 (#frame f) asm

            val allocation = allocator.run asm

            val _ = if map allocation frame.gpregs <> frame.gpregs
                    then raise Fail "allocation isn't ID on real gp-regs???"
                    else ()

            val asm = asm.replace_alloc allocation asm
            val {prologue,body=asm,epilogue} = frame.wrapFun3 done_label (#frame f) asm

            val texts = map (asm.print temp.toString) asm
            val _ = out prologue
            val _ = map (fn s => out (s ^ "\n")) texts
            val _ = out epilogue
         in () end
      | funcDecl _ _ =
        raise Fail "funcDecl unimplemented"

    fun stringExp s = frame.addString s
end
