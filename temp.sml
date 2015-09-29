structure temp :> temp =
struct
    datatype temp = Temp of int
                  | Real of string
    type label = string

    val curr = ref 100
    fun newtemp () = Temp (!curr) before curr := !curr + 1

    val last_fun_label = ref 0
    fun mklabel (name, lineno) =
        (name^"_"^(makestring lineno)^"_"^(makestring (!last_fun_label)))
        before last_fun_label := !last_fun_label + 1

    val nn = ref 0
    fun newlabel () = ("STR_"^(makestring (!nn))) before nn := (!nn) + 1

    fun toString (Temp t) = "Temp " ^ makestring t
      | toString (Real s) = "Real " ^ s

    fun real s = Real s
end
