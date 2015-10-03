structure asm =
struct
    type temp = temp.temp
    type label = temp.label

    datatype instr = OPER of { asm: string,
                               dst : temp list,
                               src : temp list,
                               jump : label list }
                   | LABEL of { asm: string,
                                lab: label }
                   | MOVE of { asm: string,
                               dst: temp,
                               src: temp }
end
