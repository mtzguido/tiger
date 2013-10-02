signature topsort =
sig
    exception Ciclo
    val topSort : (''a * ''a) list -> ''a list -> ''a list
end
