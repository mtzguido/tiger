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

    fun frameName (fr:Frame) = #name fr
    fun frameFormals (fr:Frame) = #formals fr
    fun frameAllocLocal (fr:Frame) e =
        if e
        then let val off = #localoffset fr
             in (off := !off - wordSize ; InFrame (!off))
             end
        else InReg (temp.newtemp ())

    fun mkFrame {name, formals} =
        let val f = { name = name, formals = [], localoffset = ref 0 }
            val faccs = List.map (frameAllocLocal f) formals
         in {name = #name f, formals = faccs, localoffset = #localoffset f} end

    val FP = Temp rbp
    val RV = Temp rax

    fun simpleVar (InReg t) fp = Temp t
      | simpleVar (InFrame i) fp =
              Mem (Binop (Plus, Const i, fp))

    fun addString s =
        let val l = strlabel ()
            val asm_lab = l ^ ":\n"
            val asm_len = "\t.quad " ^ printInt (String.size s) ^ "\n"
            val asm_str = "\t.ascii \"" ^ String.toCString s ^ "\"\n"
         in out (asm_lab ^ asm_len ^ asm_str ^ "\n");
            Name l
        end

    fun wrapFun1 body (frame:Frame) =
        let val body = Nx (Move (RV, unEx body))
            val label = Label (#name frame)

            val args = #formals frame
            fun assign_arg (acc, reg) =
                Move (simpleVar acc FP, reg)

            val (by_reg, by_stack) = splitAt (length arg_regs) args

            fun save_reg r =
                let val t = newtemp ()
                 in (t, Move (Temp t, Temp r)) end

            fun restore_one (r, t) =
                Move (Temp r, Temp t)

            (* Skip saved FP and return address *)
            val idx = ref 16

            val assign_regs = ListPair.map assign_arg (by_reg, map Temp arg_regs)

            fun assign_stack_arg r =
                Move (simpleVar r FP, Mem (Binop (Plus, Const (!idx), FP)))
                before idx := (!idx) + 8

            val assign_stack = map assign_stack_arg by_stack

            val (save_temps, do_save_regs) =
                    ListPair.unzip (List.map save_reg callee_save_regs)

            val do_restore_regs = List.map restore_one (ListPair.zip (callee_save_regs, save_temps))

        in SEQ (label :: assign_regs @ assign_stack @ do_save_regs @ [unNx body] @ do_restore_regs) end

    fun wrapFun2 (frame:Frame) body =
        body @ [asm.OPER { asm = "", src = rsp :: callee_save_regs,
                           dst = [], jump = []}]

    fun literal s = asm.OPER {asm = s, dst = [], src = [], jump = [] }

    (* This assumes "h" is the entry label for the function *)
    fun wrapFun3 lab (frame:Frame) (h::body) =
        let val prologue = ".global " ^ #name frame ^ "\n"
            val framesize = !(#localoffset frame)
            val intro = [
                literal "pushq %rbp",
                literal "movq %rsp, %rbp",
                literal ("addq $" ^ printInt framesize ^ ", %rsp")
            ]
            val exit = [
                asm.LABEL { asm = lab^":", lab = lab },
                literal "movq %rbp, %rsp",
                literal "popq %rbp",
                literal "ret"
            ]

         in {prologue = prologue, body = h :: intro @ body @ exit,
             epilogue = "" } end
      | wrapFun3 _ _ [] = raise Fail "empty fun on wrapFun3?"


fun spill1 frame reg acc i = case i of
    asm.LABEL _ => [i]
  | asm.OPER {asm, dst, src, jump} =>
        if elem reg dst orelse elem reg src
        then let val t = newtemp ()
                 val pre  = if elem reg src
                            then codegen (Move (Temp t, simpleVar acc FP))
                            else []
                 val ii   = asm.OPER { asm = asm, jump = jump,
                                       dst = common.replace reg t dst,
                                       src = common.replace reg t src }
                 val post = if elem reg dst
                            then codegen (Move (simpleVar acc FP, Temp t))
                            else []
             in pre @ (ii :: post) end
        else [i]
  | asm.MOVE {asm, dst, src} =>
    let val t = newtemp ()
     in if reg = src
        then codegen (Move (Temp t, simpleVar acc FP))
             @ [asm.MOVE {asm=asm, dst=dst, src=t}]
        else if reg = dst
        then [asm.MOVE {asm=asm, dst=t, src=src}]
             @ codegen (Move (simpleVar acc FP, Temp t))
        else [i]
    end

fun spill frame reg asm =
    let val acc = frameAllocLocal frame true
     in List.concat (map (spill1 frame reg acc) asm) end

end
