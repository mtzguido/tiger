structure liv :> liv =
struct
    open set graph flow

    fun all e = fn _ => e
    fun oplus m v e = fn x => if x = v then e else m x
    fun oplus_n m v e = fn x => if eq x v then e else m x

    fun rep 0 f b = b
      | rep n f b = rep (n-1) f (f b)

    fun fixpoint graph use def =
        let fun proc1 n (inS, outS, p) =
            let val inSn' = union (fromlist (use n)) (diff (outS n) (fromlist (def n)))
                val outSn' = foldl (fn (n2, s) => union s (inS n2)) emptySet (succ n)
                val inS'  = oplus_n inS  n inSn'
                val outS' = oplus_n outS n outSn'
                val p' = p
                            orelse (size inSn'  > size (inS n))
                            orelse (size outSn' > size (outS n))
             in (inS', outS', p') end
             fun lap (inS, outS) =
                foldl (fn (n,s) => proc1 n s) (inS, outS, false) (nodes graph)
             fun rep (inS, outS) =
                let val (inS', outS', p) = lap (inS, outS)
                 in if p
                    then rep (inS', outS')
                    else (inS, outS)
                end
             val (inS, outS) = rep (all emptySet, all emptySet)
          in fn n => (inS n, outS n) end

    fun liveness (FGRAPH {control, def, use, ismove}) =
        let val inS = all emptySet
            val outS = all emptySet
         in fixpoint control use def end
end
