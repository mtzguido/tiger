structure translate :> translate =
struct
    open ir

    datatype Level =
        Outermost | Frame of frame.Frame * Level

    type Access = Level * frame.Access

    val outermost = Outermost

    fun newLevel {parent, name, formals} =
        Frame (frame.mkFrame {name=name, formals=formals}, parent)

    fun allocLocal (Frame (f, p)) e = (Frame (f, p), frame.frameAllocLocal f e)
    |   allocLocal Outermost _ = raise Fail "wrong allocLocal"

    fun simpleVar (Frame f, acc) l2 =
        frame.simpleVar acc (* FIXME: Take static links into account *)
      | simpleVar (Outermost, _) _ =
        raise Fail "wrong simpleVar"

end
