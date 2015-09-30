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

    (*
     * Returns an expression for the var represented by acc
     * in frame _#frame ff_ from frame _l2_, traversing static links
     * as needed.
     *)
    fun simpleVar (Frame ff, acc) l2 =
        frame.simpleVar acc (* FIXME: Take static links into account *)
      | simpleVar (Outermost, _) _ =
        raise Fail "wrong simpleVar"

    val RV = frame.RV

    fun addString s = frame.addString s

    fun formals (Frame ff) =
            map (fn a => (Frame ff, a)) (frame.frameFormals (#frame ff))
      | formals _ = raise Fail "formals unimplemented"

    fun wrapFun body (Frame ff) = frame.wrapFun1 body (#frame ff)
      | wrapFun _ _ = raise Fail "wrapFun unimplemented"
end
