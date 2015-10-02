signature temp =
sig
    eqtype temp
    type label = string

    val newtemp : unit -> temp
    val real : string -> temp

    val mklabel : string * int -> label
    val newlabel : unit -> label

    val toString : temp -> string
end
