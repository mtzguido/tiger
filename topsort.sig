signature topsort =
sig
    datatype ''a result = OK of ''a list
                        | CICLE of ''a list

    val topSort : (''a * ''a) list -> ''a list -> ''a result
end
