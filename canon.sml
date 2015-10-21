structure canon :> canon =
struct
    open ir common

    type stmlist = IRstm list

    fun conmute1 (Exp _) _ = raise Fail "exp on left?"
      | conmute1 Skip _ = true
      | conmute1 _ (Const _) = true
      | conmute1 _ (Name _) = true
      | conmute1 s (Anot (_, e)) = conmute1 s e
      | conmute1 _ _ = false

    fun conmute (Seq (l, r)) e = conmute l e andalso conmute r e
      | conmute s e = conmute1 s e

    fun fold_binop bop = case bop of
        Plus => curry op+
      | Minus => curry op-
      | Mul => curry op*
      | Div => (fn a => fn b => a div b)
      | _ => raise Fail "can't fold op"

    fun canon_stm s =
    case s of
        Seq (l,r) => Seq (canon_stm l, canon_stm r)
      | Jump (e, l) =>
            let val (ep, er) = canon_expr e
             in Seq (ep, Jump (er, l)) end
      | Exp e =>
            let val (ep, _) = canon_expr e in ep end
      | CJump (bop, l, r, t, f) =>
            let val (lp, le) = canon_expr l
                val (rp, re) = canon_expr r
             in Seq (lp, Seq (rp, CJump (bop, le, re, t, f))) end
      | Move (l, r) =>
            let val (lp, le) = canon_expr l
                val (rp, re) = canon_expr r
             in Seq (lp, Seq (rp, Move (le, re))) end
      | Label l => Label l
      | Skip => Skip

    and canon_expr e =
    case e of
        Const i => (Skip, Const i)
      | Name n  => (Skip, Name n)
      | Temp t  => (Skip, Temp t)
      | Binop (bop, Const l, Const r) =>
          (Skip, Const (fold_binop bop l r))
      | Binop (bop, l, r) =>
            let val (lp, le) = canon_expr l
                val (rp, re) = canon_expr r
             in if conmute rp le
                then (Seq (lp, rp), Binop (bop, le, re))
                else let val t = Temp (temp.newtemp ())
                      in (Seq (Seq (lp, (Move (t, le))), rp), Binop (bop, t, re))
                      end
            end
      | Mem l =>
            let val (lp, le) = canon_expr l
             in (lp, Mem le) end
      | Call (e, f, args) =>
            let val (fp, fe) = canon_expr f
                val (ap, ae) = ListPair.unzip (List.map canon_expr args)
                val t = temp.newtemp ()
             in (Seq (fp, Seq (SEQ ap, Move (Temp t, Call (e, fe, ae)))), Temp t) end
      | Eseq (s, e) =>
            let val (ep, ee) = canon_expr e
             in (Seq (canon_stm s, ep), ee) end
      | Anot (l, e) =>
            let val (ep, ee) = canon_expr e
             in (ep, Anot (l, ee)) end

    fun flatten (Seq (l, r)) a =
            flatten l (flatten r a)
      | flatten Skip a =
            a
      | flatten x a =
            x :: a

    fun canon s =
        flatten (canon_stm s) []

    fun bblocks ss =
        let val d = temp.newlabel ()
        in (bblocks_out ss d, d) end

    and bblocks_out [] d = []
      | bblocks_out ((Label l)::ss) d =
        bblocks_in [Label l] ss d
      | bblocks_out ss d =
        let val l = temp.newlabel ()
         in bblocks_in [Label l] ss d end

    and bblocks_in b ((Label k)::ss) d =
        (b @ [Jump (Name k, [k])]) :: bblocks_in [Label k] ss d
      | bblocks_in b ((Jump (e,labs))::ss) d =
        (b @ [Jump (e, labs)]) :: bblocks_out ss d
      | bblocks_in b ((CJump (bop,l,r,t,f))::ss) d =
        (b @ [CJump (bop,l,r,t,f)]) :: bblocks_out ss d
      | bblocks_in b (s::ss) d =
        bblocks_in (b@[s]) ss d
      | bblocks_in b [] d =
         [b @ [Jump (Name d, [d])]]

    fun diff [] e = []
      | diff (h::t) e = if h = e then t else h :: (diff t e)

    (* FIXME: improve this, and maybe remove dead code at this point *)
    fun oneTrace [] cands =
        oneTrace (hd cands) (tl cands)
      | oneTrace trace cands =
        let val lab = case List.last trace of
                          Jump (Name l, _) => l
                        | CJump (_, _, _, _, l) => l
                        | _ => raise Fail "wat?"
            fun is_lab bs = hd bs = Label lab
            val next = List.find is_lab cands
            fun add b1 ((Label l)::bs) =
                if List.last b1 = Jump (Name l, [l])
                then init b1 @ ((Label l) :: bs)
                else b1 @ ((Label l) :: bs)
              | add _ _ = raise Fail "add of empty block?"
         in case next of
                NONE => (trace, cands)
              | SOME b => oneTrace (add trace b) (diff cands b)
         end

    fun traceRepeat trace cands =
        case oneTrace [] cands of
            (t, []) => trace @ t
          | (t, cands') => traceRepeat (trace @ t) cands'

    fun traceSched bs = traceRepeat [] bs

end
