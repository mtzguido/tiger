signature frame =
sig
    type Frame
    type Access

    val mkFrame : {name: temp.label, formals: bool list}
                    -> Frame
    val frameName : Frame -> temp.label
    val frameFormals : Frame -> Access list
    val frameAllocLocal : Frame -> bool -> Access
end
