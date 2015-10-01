signature canon =
sig
    type stmlist = ir.IRstm list

    val canon : ir.IRstm -> stmlist
    val bblocks : stmlist -> stmlist list
end
