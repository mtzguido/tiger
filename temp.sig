signature temp =
sig
    type temp
    type label = string

    val newtemp : unit -> temp
    val mklabel : string * int -> label
    val newlabel : unit -> label

    val toString : temp -> string
end
