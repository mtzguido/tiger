open BasicIO common

val ofile = ref (open_out "/dev/null")

fun out s = (
    if !verbose then print ("OUTPUT: " ^ s) else () ;
    output (!ofile, s);
    flush_out (!ofile)
 )
