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

    datatype IR = Ex of IRexp
                | Nx of IRstm
                | Cx of temp.label * temp.label -> IRstm
    
    fun unEx _ = raise Fail "aa"
    fun unNx _ = raise Fail "bb"
    fun unCx _ = raise Fail "cc"
    
    fun irToString _ = "IR"
end
