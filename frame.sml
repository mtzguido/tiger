structure frame :> frame=
struct

    val wordSize = 4 (* 32bit *)

    datatype Access = InMem of int
                    | InReg of temp.temp
    type Frame = { name: string,
                   formals: Access list,
                   localoffset: int ref }

    fun mkFrame {name, formals} = 
        let fun add_one (esc, (off,l)) = 
                    if esc then (off+wordSize, (InMem off)::l)
                           else (off, (InReg (temp.newtemp ()))::l)
             val (_, args_access) = foldl add_one (0,[]) formals
        in
            {name=name, formals=args_access, localoffset=ref 0}
        end

    fun frameName (fr:Frame) = #name fr
    fun frameFormals (fr:Frame) = #formals fr
    fun frameAllocLocal (fr:Frame) e =
        if e
        then InMem (!(#localoffset fr)) before (#localoffset fr) := !(#localoffset fr) - wordSize
        else InReg (temp.newtemp ())

end
