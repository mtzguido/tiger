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
    fun newlabel () = (".L"^(makestring (!nn))) before nn := (!nn) + 1

    val ns = ref 0
    fun strlabel () = (".STR"^(makestring (!ns))) before ns := (!ns) + 1

    fun toString (Temp t) = "t_" ^ makestring t
      | toString (Real s) = s

    fun real s = Real s

    fun isreal (Real _) = true
      | isreal _ = false

    fun tcomp (Real s1, Real s2) = String.compare (s1, s2)
      | tcomp (Temp i,  Temp j)  = Int.compare (i, j)
      | tcomp (Temp _, Real _)   = LESS
      | tcomp (Real _, Temp _)   = GREATER
end
