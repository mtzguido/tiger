signature topsort =
sig
    datatype ''a result = OK of ''a list
                        | CICLE of ''a list

    val topSort : (string * string) list -> string list -> string result
end
