signature frame =
sig
    type Frame
    type Access

    val mkFrame : {name: temp.label, formals: bool list}
                    -> Frame
    val frameName : Frame -> temp.label
    val frameFormals : Frame -> Access list
    val frameAllocLocal : Frame -> bool -> Access

    val simpleVar : Access -> ir.IRexp

    val FP : ir.IRexp
    val RV : ir.IRexp

    val addString : string -> ir.IRexp
end
