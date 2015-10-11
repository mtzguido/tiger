signature set =
sig
    type 'a set

    val emptySet : ''a set
    val emptySet' : ('a -> 'a -> bool) -> 'a set
    val insert : 'a -> 'a set -> 'a set
    val delete : 'a -> 'a set -> 'a set
    val member : 'a -> 'a set -> bool
    val tolist : 'a set -> 'a list

    val singleton : ''a -> ''a set
    val fromlist : ''a list -> ''a set

    val union : 'a set -> 'a set -> 'a set
    val diff  : 'a set -> 'a set -> 'a set

    val size : 'a set -> int
end
