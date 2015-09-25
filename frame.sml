(* Frame module for amd64 architecture *)

structure frame :> frame =
struct
    open ir temp

    val wordSize = 8 (* 64bit *)

    datatype Access = InFrame of int
                    | InReg of temp.temp

    type Frame = {
        name: string,
        formals: Access list,
        localoffset: int ref
    }

    fun mkFrame {name, formals} =
        let fun add_one (esc, (off, l)) =
                    if esc then (off - wordSize, (InFrame off)::l)
                           else (off, (InReg (temp.newtemp ()))::l)
            val (_, args_access) = foldl add_one (0, []) formals
        in
            { name = name, formals = args_access, localoffset = ref 0 }
        end

    fun frameName (fr:Frame) = #name fr
    fun frameFormals (fr:Frame) = #formals fr
    fun frameAllocLocal (fr:Frame) e =
        if e
        then let val off = #localoffset fr
             in InFrame (!off) before off := !off - wordSize
             end
        else InReg (temp.newtemp ())

    val FP = Temp (newtemp ())
    val RV = Temp (newtemp ())

    fun simpleVar (InReg t) = Temp t
      | simpleVar (InFrame i) =
              Mem (Binop (Plus, (Const i), FP))
end
