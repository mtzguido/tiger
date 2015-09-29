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

    datatype reg = rax | rbx | rcx | rdx | rsi | rdi | rbp | rsp
                 | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15

    fun regToString rax = "rax"
      | regToString rbx = "rbx"
      | regToString rcx = "rcx"
      | regToString rdx = "rdx"
      | regToString rsi = "rsi"
      | regToString rdi = "rdi"
      | regToString rbp = "rbp"
      | regToString rsp = "rsp"
      | regToString r8  = "r8"
      | regToString r9  = "r9"
      | regToString r10 = "r10"
      | regToString r11 = "r11"
      | regToString r12 = "r12"
      | regToString r13 = "r13"
      | regToString r14 = "r14"
      | regToString r15 = "r15"

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

    (* FIXME: these need to be the real registers *)
    val FP = Temp (newtemp ())
    val RV = Temp (newtemp ())

    fun simpleVar (InReg t) = Temp t
      | simpleVar (InFrame i) =
              Mem (Binop (Plus, (Const i), FP))

    fun addString s =
        let val l = newlabel ()
         in print ("We should be generating code for string: <" ^ s ^ ">\n");
            Name l
        end
end
