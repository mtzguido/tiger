structure ir :> ir =
struct
	datatype IR = Const of int
                | Wrap of IR
                | Wrap2 of IR * IR

    fun irToString SCAF = "SCAF"
end
