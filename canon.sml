structure canon :> canon =
struct
    open ir

    type stmlist = IRstm list

    fun canon_stm s =
    case s of
        Seq (l,r) => (canon_stm l)@(canon_stm r)
      | Jump (e, l) =>
            let val (ep, er) = canon_expr e
             in ep @ [Jump (er, l)] end
      | Exp e =>
            let val (ep, _) = canon_expr e in ep end
      | CJump (bop, l, r, t, f) =>
            let val (lp, le) = canon_expr l
                val (rp, re) = canon_expr r
             in lp @ rp @ [CJump (bop, le, re, t, f)] end
      | Move (l, r) =>
            let val (lp, le) = canon_expr l
                val (rp, re) = canon_expr r
             in lp @ rp @ [Move (le, re)] end
      | Label l => [Label l]
      | Skip => []

    and canon_expr e =
    case e of
        Const i => ([], Const i)
      | Name n  => ([], Name n)
      | Temp t  => ([], Temp t)
      (* TODO: check for conmuting expressions to save the temp *)
      | Binop (bop, l, r) =>
            let val (lp, le) = canon_expr l
                val (rp, re) = canon_expr r
                val t = Temp (temp.newtemp ())
             in (lp @ [Move (t, le)] @ rp, Binop (bop, t, re)) end
      | Mem l =>
            let val (lp, le) = canon_expr l
             in (lp, Mem le) end
      | Call (f, args) =>
            let val (fp, fe) = canon_expr f
                val (ap, ae) = ListPair.unzip (List.map canon_expr args)
                val t = temp.newtemp ()
             in (fp @ List.concat ap @ [Move (Temp t, Call (fe, ae))], Temp t) end
      | Eseq (s, e) =>
            let val (ep, ee) = canon_expr e
             in (canon_stm s @ ep, ee) end
      | Anot (l, e) =>
            let val (ep, ee) = canon_expr e
             in (ep, Anot (l, ee)) end

    fun canon s =
        canon_stm s

    fun bblocks l =
        [l]
end
