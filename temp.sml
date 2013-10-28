structure temp :> temp = 
struct
    type temp = string

    val curr = ref 0

    fun newtemp () = ("t"^(makestring (!curr))) before curr := !curr + 1
end
