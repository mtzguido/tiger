structure codegen :> codegen =
struct
    open asm ir temp

    (* COPIED FROM x86 FRAME, FIX THIS DUPLICATION! *)
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
    (* /COPIED *)

    val O = ref [] : instr list ref

    fun emit i = O := i::(!O)

    fun gen_binop binop l r =
    let val text = case binop of
        Plus => "addq"
      | Minus => "subq"
      | _ => "otro op"
        val t = newtemp ()
        val ll = gen_e l
        val rr = gen_e r
    in emit (MOVE { asm = "movq 's0, 'd0", src = ll, dst = t });
       emit (OPER { asm = text ^ " 's0, 'd0", src = [rr, t], dst = [t], jump = []});
       t end

    and gen_e e =
    case e of
        Const i =>
            let val t = newtemp ()
              in emit (OPER { asm = "movq $"^(makestring i)^", 'd0",
                              dst = [t], src = [], jump = [] }); t end
      | Temp t => t
      | Name l =>
        let val t = newtemp ()
        in emit (OPER { asm = "movq " ^ l ^ " 'd0", dst = [t], src = [],
                        jump = [] }); t end

      | Binop (binop, l, r) =>
          gen_binop binop l r
      | Mem e =>
            let val ee = gen_e e
                val t = newtemp ()
             in emit (OPER { asm = "movq ('s0), 'd0", src = [ee], dst = [t],
                                                     jump = [] });
                t end
      | Call _ => raise Fail "Out-of-place Call in gen_e"
      | Eseq _ => raise Fail "Eseq on codegen?"
      | Anot (_,e) => gen_e e

    fun gen_call f args =
        let val args' = List.map gen_e args
            val aregs = List.take (arg_regs, length args)
         in
             List.app (fn (a,r) => emit (MOVE { asm = "movq 's0, 'd0", dst = r, src = a}))
                    (ListPair.zip (args', aregs));
             emit (OPER { asm = "call " ^ f, src = [], dst = caller_saved,
                         jump = []}) end

    fun gen_cjump relop l r tl fl =
    let val text = case relop of
        Eq => "je"
      | Ne => "jne"
      | Gt => "jgt"
      | Ge => "jge"
      | Lt => "jlt"
      | Le => "jle"
      | _ => "jteladebo"
    in
    emit (OPER { asm = "cmpq 's0 's1", src = [l, r], dst = [], jump = [] }) ;
    emit (OPER { asm = text ^ " " ^ tl, src = [], dst = [],
                    jump = [tl, fl]}) end

    fun gen_s s =
    case s of
        Exp (Call (Name f, args)) =>
            gen_call f args

      | Exp _ =>
            raise Fail "Exp in codegen?"

      | Move (l, Call (Name f, args)) =>
            let val lt = gen_e l in
                gen_call f args;
                emit (MOVE { asm = "movq 's0, 'd0", src = rax, dst = lt})
            end

      | Move (l, r) =>
             emit (MOVE { asm = "movq 's0, 'd0",
                          dst = gen_e l,
                          src = gen_e r})

      | Jump (_, labs) => (* ?? *)
            emit (OPER { asm = "jmp "^(hd labs),
                         dst = [], src = [],
                         jump = labs })

      | CJump (relop, l, r, tl, fl) =>
            gen_cjump relop (gen_e l) (gen_e r) tl fl

      | Seq _ =>
            raise Fail "Seq on codegen?"

      | Label l =>
            emit (LABEL { asm = l^":", lab = l })

      | Skip =>
            raise Fail "Skip on codegen?"

    fun codegen s =
        let val _ = O := []
            val _ = gen_s s
         in rev (!O) end
end