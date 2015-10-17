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

    fun replace [] _ _ _ = []
      | replace (#"'"::s::i::t) regprint dst src =
        let val l = case s of
                        #"d" => dst
                      | #"s" => src
                      | _ => raise Fail "wat"
            val idx = valOf (Int.fromString (str i))
         in (explode (regprint (List.nth (l, idx)))) @
            (replace t regprint dst src)
         end
      | replace (h::t) regprint dst src =
        h :: (replace t regprint dst src)

    fun print regstring (LABEL {asm, lab}) = lab ^ ":"
      | print regstring (OPER {asm, dst, src, jump}) =
        "   " ^ implode (replace (explode asm) regstring dst src)
      | print regstring (MOVE {asm, dst, src}) =
        "   " ^ implode (replace (explode asm) regstring [dst] [src])

    fun replace_alloc f is = map (replace_alloc1 f) is
    and replace_alloc1 f i =
        case i of
            LABEL _ => i
          | OPER {asm,dst,src,jump} => OPER {asm=asm, dst=map f dst, src=map f src, jump=jump}
          | MOVE {asm,dst,src} => MOVE {asm=asm, dst=f dst, src=f src}

end
