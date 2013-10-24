signature frame =
sig
    type Frame
    type Access

    val mkFrame : {name: string, formals: bool list}
                    -> Frame
    val frameName : Frame -> string
    val frameFormals : Frame -> Access list
    val frameAllocLocal : Frame -> bool -> Access
end
