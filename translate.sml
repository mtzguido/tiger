structure translate :> translate =
struct
    open ir

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

    fun formals (Frame ff) =
            map (fn a => (Frame ff, a)) (frame.frameFormals (#frame ff))
      | formals _ = raise Fail "formals unimplemented"

    fun wrapFun body (Frame ff) = frame.wrapFun1 body (#frame ff)
      | wrapFun _ _ = raise Fail "wrapFun unimplemented"

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
        frame.funcDecl (#frame f) b
      | funcDecl _ _ =
        raise Fail "funcDecl unimplemented"

    fun stringExp s = frame.addString s
end
