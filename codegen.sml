structure codegen :> codegen =
struct
    open asm ir temp common

    (* COPIED FROM x86 FRAME, FIX THIS DUPLICATION! *)
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
    (* /COPIED *)

    val O = ref [] : instr list ref

    fun emit i = O := i::(!O)

    fun gen_binop_simpl binop l r =
    let val text = case binop of
        Plus => "addq"
      | Minus => "subq"
      | _ => raise Fail "Internal error on gen_binop_simpl"
        val t = newtemp ()
        val ll = gen_e l
        val rr = gen_e r
    in emit (MOVE { asm = "movq 's0, 'd0", src = ll, dst = t });
       emit (OPER { asm = text ^ " 's0, 'd0", src = [rr, t], dst = [t], jump = []});
       t end

    and gen_binop_rdx_rax binop l r =
    let val (cqto, text) = case binop of
        Mul => (false, "imulq")
      | Div => (true,  "idivq")
      | _ => raise Fail "Internal error on gen_binop_rdx_rax"
        val ll = gen_e l
        val rr = gen_e r
    in emit (MOVE { asm = "movq 's0, 'd0", src = ll, dst = rax });
       if cqto
       then emit ( OPER { asm = "cqto", src = [], dst = [rdx], jump = []})
       else ();
       emit (OPER { asm = text ^ " 's0", src = [rr, rax], dst = [rax, rdx], jump = []});
       rax end

    and gen_binop Plus l r =
        gen_binop_simpl Plus l r

      | gen_binop Minus l r =
        gen_binop_simpl Minus l r

      | gen_binop Mul l r =
        gen_binop_rdx_rax Mul l r

      | gen_binop Div l r =
        gen_binop_rdx_rax Div l r

      | gen_binop binop _ _ =
          raise Fail ("unimplemented op: " ^ p_binop binop)

    and gen_e e =
    case e of
        Const i =>
            let val t = newtemp ()
              in emit (OPER { asm = "movq $"^(printInt i)^", 'd0",
                              dst = [t], src = [], jump = [] }); t end
      | Temp t => t
      | Name l =>
        let val t = newtemp ()
        in emit (OPER { asm = "leaq " ^ l ^ ", 'd0", dst = [t], src = [],
                        jump = [] }); t end

      | Binop (binop, l, r) =>
          gen_binop binop l r

      | Mem (Binop (Plus, Const i, Temp b)) =>
            let val t = newtemp ()
             in emit (OPER { asm = "movq " ^(printInt i)^"('s0), 'd0",
                             src = [b], dst = [t], jump = [] });
                t end

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
         in
             List.app (fn (a,r) => emit (MOVE { asm = "movq 's0, 'd0", dst = r, src = a}))
                    (ListPair.zip (args', arg_regs));
             emit (OPER { asm = "xorq %rax, %rax",
                              src = [], dst = [rax], jump = []});
             emit (OPER { asm = "call " ^ f,
                              src = [], dst = arg_regs @ caller_saved,
                              jump = []}) end

    fun gen_cjump relop l r tl fl =
    let val text = case relop of
        Eq  => "je"
      | Ne  => "jne"
      | Gt  => "jg"
      | Ge  => "jge"
      | Lt  => "jl"
      | Le  => "jle"
      | Ult => "jb"
      | Ule => "jbe"
      | Ugt => "ja"
      | Uge => "jae"
    in
    (* GAS syntax, ops are reversed *)
    emit (OPER { asm = "cmpq 's0, 's1", src = [r, l], dst = [], jump = [] }) ;
    emit (OPER { asm = text ^ " " ^ tl, src = [], dst = [],
                    jump = [tl, fl]}) end

    fun gen_s s =
    case s of
        Exp (Call (Name f, args)) =>
            gen_call f args

      | Exp _ =>
            raise Fail "Exp in codegen?"

      | Move (Mem (Binop (Plus, Const i, Temp t)), r) =>
          emit (OPER { asm = "movq 's1, " ^ printInt i ^ "('s0)",
                              dst = [],
                              src = [t, gen_e r],
                              jump = []})

      | Move (Mem e, r) =>
          emit (OPER { asm = "movq 's1, ('s0)",
                              dst = [],
                              src = [gen_e e, gen_e r],
                              jump = [] })

      | Move (l, Call (Name f, args)) =>
            let val lt = gen_e l in
                gen_call f args;
                emit (MOVE { asm = "movq 's0, 'd0", src = rax, dst = lt})
            end

      | Move (l, Const i) =>
          emit (OPER { asm = "movq $"^printInt i^", 'd0",
                       dst = [gen_e l], src = [], jump = []})

      | Move (l, Binop (Plus, Const i, Temp r)) =>
          if l = Temp r
          then let val lt = gen_e l
                in emit (OPER { asm = "addq $"^printInt i^", 'd0",
                                dst = [lt], src = [lt], jump = []})
               end
          else emit (OPER { asm = "leaq "^printInt i^"('s0), 'd0",
                            dst = [gen_e l], src = [r], jump = []})

      | Move (l, r) =>
          emit (MOVE { asm = "movq 's0, 'd0",
                              dst = gen_e l,
                              src = gen_e r})

      | Jump (_, labs) => (* FIXME: this ok? *)
            emit (OPER { asm = "jmp "^(hd labs),
                         dst = [], src = [], jump = labs })

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
