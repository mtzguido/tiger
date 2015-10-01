structure canon :> canon =
struct
    open ir

    type stmlist = IRstm list

    fun conmute1 (Exp _) _ = raise Fail "exp on left?"
      | conmute1 _ (Const _) = true
      | conmute1 _ (Name _) = true
      | conmute1 s (Anot (_, e)) = conmute1 s e
      | conmute1 _ _ = false

    fun conmute ss e = List.all (fn s => conmute1 s e) ss

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
      | Binop (bop, l, r) =>
            let val (lp, le) = canon_expr l
                val (rp, re) = canon_expr r
             in if conmute rp le
                then (lp @ rp, Binop (bop, le, re))
                else let val t = Temp (temp.newtemp ())
                      in (lp @ [Move (t, le)] @ rp, Binop (bop, t, re))
                      end
            end
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

    fun bblocks [] = []
      | bblocks ((Label l)::ss) =
        bblocks_in [Label l] ss
      | bblocks ss =
        let val l = temp.newlabel ()
         in bblocks_in [Label l] ss end
    and bblocks_in b ((Label k)::ss) =
        (b @ [Jump (Name k, [k])]) :: bblocks_in [Label k] ss
      | bblocks_in b ((Jump (e,labs))::ss) =
        (b @ [Jump (e, labs)]) :: bblocks ss
      | bblocks_in b ((CJump (bop,l,r,t,f))::ss) =
        (b @ [CJump (bop,l,r,t,f)]) :: bblocks ss
      | bblocks_in b (s::ss) =
        bblocks_in (b@[s]) ss
      | bblocks_in b [] =
        let val don = temp.newlabel ()
         in [b @ [Jump (Name don, [don])]] end

end
