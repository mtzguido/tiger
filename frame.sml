(* Frame module for amd64 architecture *)

structure frame :> frame =
struct
    open ir temp canon codegen flowcalc flow graph ofile

    val wordSize = 8

    datatype Access = InFrame of int
                    | InReg of temp.temp

    type Frame = {
        name: string,
        formals: Access list,
        localoffset: int ref
    }

    val rax = real "%rax"
    val rbx = real "%rbx"
    val rcx = real "%rcx"
    val rdx = real "%rdx"
    val rsi = real "%rsi"
    val rdi = real "%rdi"
    val rbp = real "%rbp"
    val rsp = real "%rsp"
    val r8  = real "%r8"
    val r9  = real "%r9"
    val r10 = real "%r10"
    val r11 = real "%r11"
    val r12 = real "%r12"
    val r13 = real "%r13"
    val r14 = real "%r14"
    val r15 = real "%r15"

    val arg_regs = [rdi, rsi, rdx, rcx, r8, r9]
    val callee_save_regs = [rbx, r12, r13, r14, r15]
    val special_regs = [rsp, rbp]
    val caller_saved = [rax, r10, r11]

    val gpregs = arg_regs @ callee_save_regs @ caller_saved

    (* FIXME: coallesce this duplication in here and frameAllocLocal *)
    fun mkFrame {name, formals} =
        let fun add_one (esc, (off, l)) =
                    if esc then (off - wordSize, (InFrame off)::l)
                           else (off, (InReg (temp.newtemp ()))::l)
            val (off, args_access) = foldl add_one (~wordSize, []) formals
        in
            { name = name, formals = rev args_access, localoffset = ref off }
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

    fun simpleVar (InReg t) fp = Temp t
      | simpleVar (InFrame i) fp =
              Mem (Binop (Plus, Const i, fp))

    fun addString s =
        let val l = strlabel ()
            val asm_lab = l ^ ":\n"
            val asm_len = "\t.long " ^ makestring (String.size s) ^ "\n"
            val asm_str = "\t.ascii \"" ^ String.toCString s ^ "\"\n"
         in out (asm_lab ^ asm_len ^ asm_str ^ "\n");
            Name l
        end

    fun wrapFun1 body (frame:Frame) =
        let val body = Nx (Move (RV, unEx body))
            val args = #formals frame
            fun assign_arg (acc, reg) =
                Move (simpleVar acc FP, reg)

            val label = Label (#name frame)

            fun save_reg r =
                let val t = newtemp ()
                 in (t, Move (Temp t, Temp r)) end

            fun restore_one (r, t) =
                Move (Temp r, Temp t)

            val assign_args = ListPair.map assign_arg (args, map Temp arg_regs)
            val (save_temps, do_save_regs) =
                    ListPair.unzip (List.map save_reg callee_save_regs)

            val do_restore_regs = List.map restore_one (ListPair.zip (callee_save_regs, save_temps))

        in SEQ (label :: assign_args @ do_save_regs @ [unNx body] @ do_restore_regs) end

    fun wrapFun2 (frame:Frame) body =
        body @ [asm.OPER { asm = "", src = rsp :: callee_save_regs,
                           dst = [], jump = []}]
end
