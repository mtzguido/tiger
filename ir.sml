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

    val SEQ = common.foldl1 Seq

    datatype IR = Ex of IRexp
                | Nx of IRstm
                | Cx of temp.label * temp.label -> IRstm

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
      | unCx (Nx _) = raise Fail "unCx of Nx??"

    fun irToString _ = "IR"
end
