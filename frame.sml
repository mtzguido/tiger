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

    val rax = real "rax"
    val rbx = real "rbx"
    val rcx = real "rcx"
    val rdx = real "rdx"
    val rsi = real "rsi"
    val rdi = real "rdi"
    val rbp = real "rbp"
    val rsp = real "rsp"
    val r8  = real "r8"
    val r9  = real "r9"
    val r10 = real "r10"
    val r11 = real "r11"
    val r12 = real "r12"
    val r13 = real "r13"
    val r14 = real "r14"
    val r15 = real "r15"

    val arg_regs = [rdi, rsi, rdx, rcx, r8, r9]
    val callee_save_regs = [rbx, r12, r13, r14, r15]
    val special_regs = [rsp, rbp]
    val caller_saved = [rax, r10, r11]

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

    val FP = Temp rbp
    val RV = Temp rax

    fun simpleVar (InReg t) = Temp t
      | simpleVar (InFrame i) =
              Mem (Binop (Plus, (Const i), FP))

    fun addString s =
        let val l = newlabel ()
         in print ("We should be generating code for string: <" ^ s ^ ">\n");
            Name l
        end

    fun wrapFun1 body (frame:Frame) =
        let val body = Nx (Move (RV, unEx body))
            val args = #formals frame
            fun assign_arg (acc, reg) =
                Move (simpleVar acc, reg)

            val assign_args = ListPair.map assign_arg (args, map Temp arg_regs)
        in Nx (SEQ (assign_args @ [unNx body])) end
end
