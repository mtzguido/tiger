signature frame =
sig
    type Frame
    type Access
    type reg

    val mkFrame : {name: temp.label, formals: bool list}
                    -> Frame
    val frameName : Frame -> temp.label
    val frameFormals : Frame -> Access list
    val frameAllocLocal : Frame -> bool -> Access

    val simpleVar : Access -> ir.IRexp

    val FP : ir.IRexp
    val RV : ir.IRexp
    val regToString : reg -> string

    val addString : string -> ir.IRexp

    val wrapFun1 : ir.IR -> Frame -> ir.IR
end
