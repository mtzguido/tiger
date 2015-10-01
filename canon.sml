structure canon :> canon =
struct
    open ir

    type stmlist = IRstm list

    fun canon e =
        [e]

    fun bblocks l =
        [l]
end
