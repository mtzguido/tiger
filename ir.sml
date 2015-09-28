structure ir :> ir =
struct
    datatype IRexp =
          Const of int
        | Name of temp.label
        | Temp of temp.temp
        | Binop of binop * IRexp * IRexp
        | Mem of IRexp
        | Call of IRexp * IRexp list
        | Eseq of IRstm * IRexp
    and IRstm =
          Move of IRexp * IRexp
        | Exp of IRexp
        | Jump of IRexp * temp.label list
        | CJump of relop * IRexp * IRexp * temp.label * temp.label
        | Seq of IRstm * IRstm
        | Label of temp.label
        | Skip
    and binop = Plus | Minus | Mul | Div
              | And | Or | LShift | RShift
              | ARShift | Xor
    and relop = Eq | Ne | Lt | Gt | Le | Ge
              | Ult | Ule | Ugt | Uge

    fun sseq (Skip, s) = s
      | sseq (s, Skip) = s
      | sseq (l, r) = Seq (l, r)

    val SEQ = common.foldl1 (fn (i,a) => sseq (a,i))

    datatype IR = Ex of IRexp
                | Nx of IRstm
                | Cx of temp.label * temp.label -> IRstm

    fun p_relop Eq  = "Eq"
      | p_relop Ne  = "Ne"
      | p_relop Lt  = "Lt"
      | p_relop Gt  = "Gt"
      | p_relop Le  = "Le"
      | p_relop Ge  = "Ge"
      | p_relop Ult = "Ult"
      | p_relop Ule = "Ule"
      | p_relop Ugt = "Ugt"
      | p_relop Uge = "Uge"
    and p_binop Plus    = "Plus"
      | p_binop Minus   = "Minus"
      | p_binop Mul     = "Mul"
      | p_binop Div     = "Div"
      | p_binop And     = "And"
      | p_binop Or      = "Or"
      | p_binop LShift  = "LShift"
      | p_binop RShift  = "RShift"
      | p_binop ARShift = "ARShift"
      | p_binop Xor     = "Xor"
    and p_expr (Const n) = "Const " ^ makestring n
      | p_expr (Name n) = "Name " ^ n
      | p_expr (Temp t) = temp.toString t
      | p_expr (Binop (binop,l,r)) = "Binop (" ^ p_binop binop ^ ", " ^ p_expr l ^ ", " ^
                                     p_expr r ^ ")"
      | p_expr (Mem e) = "Mem " ^ p_expr e
      | p_expr (Call (n, a)) = "Call (" ^ p_expr n ^ ", " ^ p_exprlist a ^ ")"
      | p_expr (Eseq (s, e)) = "Eseq (" ^ p_stmt s ^ ", " ^ p_expr e ^ ")"
    and p_exprlist [] = "[]"
      | p_exprlist [e] = "[" ^ p_expr e ^ "]"
      | p_exprlist (h::t) = "[" ^ p_expr h ^ (foldl (fn (e, a) => a ^ "," ^ p_expr e) "" t) ^ "]"
    and p_stmt (Move (l,e)) = "Move (" ^ p_expr l ^ ", " ^ p_expr e ^")"
      | p_stmt (Exp e) = "Exp " ^ p_expr e
      | p_stmt (Jump (e,_)) = "Jump " ^ p_expr e
      | p_stmt (CJump (relop,l,r,t,f)) = "CJump (" ^ p_relop relop ^ ", " ^ p_expr l
                                    ^ ", " ^ p_expr r ^ ", " ^ t ^ ", " ^ f ^ ")"
      | p_stmt (Seq (l,r)) = p_stmt l ^ "; " ^ p_stmt r
      | p_stmt (Label l) = "Label " ^ l
      | p_stmt Skip = "Skip"
    and p_cond _ = "cond"
    and irToString (Ex e) = p_expr e
      | irToString (Nx n) = p_stmt n
      | irToString (Cx c) = p_cond c

    fun unEx (Ex e) = e
      | unEx (Nx s) = Eseq (s, Const 0)
      | unEx (Cx t) =
        let val r = temp.newtemp ()
            val s = temp.newlabel ()
            val f = temp.newlabel ()
            val prep = SEQ [Move (Temp r, Const 0),
                            t (s, f),
                            Label s,
                            Move (Temp r, Const 1),
                            Label f]
         in Eseq (prep, Temp r) end

    fun unNx (Nx s) = s
      | unNx (Ex e) = Exp e
      | unNx (Cx t) = let val l = temp.newlabel () in SEQ [t (l, l), Label l] end

    fun unCx (Cx t) = t
      | unCx (Ex e) = (fn (t, f) => CJump (Ne, e, Const 0, t, f))
      | unCx (Nx n) = raise Fail ("unCx of Nx??: " ^ irToString (Nx n))
end
