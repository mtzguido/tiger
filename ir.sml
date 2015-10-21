structure ir :> ir =
struct
    datatype IRexp =
          Const of int
        | Name of temp.label
        | Temp of temp.temp
        | Binop of binop * IRexp * IRexp
        | Mem of IRexp
        | Call of bool * IRexp * IRexp list
        | Eseq of IRstm * IRexp
        | Anot of string * IRexp
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

    val SEQ = foldl (fn (i,a) => sseq (a,i)) Skip

    datatype IR = Ex of IRexp
                | Nx of IRstm
                | Cx of temp.label * temp.label -> IRstm

    fun paren s = "(" ^ s ^ ")"

    fun p_relop Eq  = "=="
      | p_relop Ne  = "!="
      | p_relop Lt  = "<"
      | p_relop Gt  = ">"
      | p_relop Le  = "<="
      | p_relop Ge  = ">="
      | p_relop Ult = "U<"
      | p_relop Ule = "U<="
      | p_relop Ugt = "U>"
      | p_relop Uge = "U>="
    and p_binop Plus    = "+"
      | p_binop Minus   = "-"
      | p_binop Mul     = "*"
      | p_binop Div     = "/"
      | p_binop And     = "&"
      | p_binop Or      = "|"
      | p_binop LShift  = "<<"
      | p_binop RShift  = ">>"
      | p_binop ARShift = ">>>"
      | p_binop Xor     = "^"
    and p_expr (Const n) = makestring n
      | p_expr (Name n) = "Name " ^ n
      | p_expr (Temp t) = temp.toString t
      | p_expr (Binop (binop,l,r)) = paren (p_expr l ^ " " ^ p_binop binop ^ " " ^ p_expr r)
      | p_expr (Mem e) = "MEM[" ^ p_expr e ^ "]"
      | p_expr (Call (_, n, a)) = p_expr n ^ paren (p_exprlist a)
      | p_expr (Eseq (s, e)) = "Eseq (" ^ p_stmt s ^ ", " ^ p_expr e ^ ")"
      | p_expr (Anot (s, e)) = "Anot: <"^s^"> " ^ p_expr e
    and p_exprlist [] = "[]"
      | p_exprlist [e] = "[" ^ p_expr e ^ "]"
      | p_exprlist (h::t) = "[" ^ p_expr h ^ (foldl (fn (e, a) => a ^ "," ^ p_expr e) "" t) ^ "]"
    and p_comp (relop, l, r) = p_expr l ^ " " ^ p_relop relop ^ " " ^ p_expr r
    and p_stmt (Move (l,e)) = p_expr l ^ " <- " ^ p_expr e
      | p_stmt (Exp e) = "Exp " ^ p_expr e
      | p_stmt (Jump (e,_)) = "Jump " ^ p_expr e
      | p_stmt (CJump (relop,l,r,t,f)) = "CJump " ^ p_comp (relop, l, r) ^ " ? " ^ t ^ " : " ^ f
      | p_stmt (Seq (l,r)) = p_stmt l ^ "\n" ^ p_stmt r
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
      | unCx (Ex (Const 0)) = (fn (_,f) => Jump (Name f,[f]))
      | unCx (Ex (Const _)) = (fn (t,_) => Jump (Name t,[t]))
      | unCx (Ex e) = (fn (t, f) => CJump (Ne, e, Const 0, t, f))
      | unCx (Nx n) = raise Fail ("unCx of Nx??: " ^ irToString (Nx n))
end
