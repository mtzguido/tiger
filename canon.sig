signature canon =
sig
    type stmlist = ir.IRstm list

    val canon : ir.IRstm -> stmlist

    (* the label is the one to transfer control to after
       executing the function *)
    val bblocks : stmlist -> stmlist list * temp.label

    val traceSched : stmlist list -> stmlist
end
