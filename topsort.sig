signature topsort =
sig
    datatype ''a result = OK of ''a list
                        | CICLE of ''a

    val topSort : (''a * ''a) list -> ''a list -> ''a result
end
