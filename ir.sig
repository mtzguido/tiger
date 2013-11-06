signature ir =
sig
	datatype IR = Const of int
                | Wrap of IR
                | Wrap2 of IR * IR

	val irToString : IR -> string
end
