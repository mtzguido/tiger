signature frame =
sig
    type Frame
    type Access

    val wordSize : int

    val mkFrame : {name: temp.label, formals: bool list} -> Frame

    val frameName : Frame -> temp.label
    val frameFormals : Frame -> Access list
    val frameAllocLocal : Frame -> bool -> Access

    val simpleVar : Access -> ir.IRexp -> ir.IRexp

    val FP : ir.IRexp
    val RV : ir.IRexp

    val addString : string -> ir.IRexp

    val gpregs : temp.temp list
    val special_regs : temp.temp list

    val wrapFun1 : ir.IR -> Frame -> ir.IRstm
    val wrapFun2 : Frame -> asm.instr list -> asm.instr list
    val wrapFun3 : temp.label -> Frame -> asm.instr list ->
                        { prologue: string,
                          body: asm.instr list,
                          epilogue: string }

    val spill : Frame -> temp.temp list -> asm.instr list -> asm.instr list
end
